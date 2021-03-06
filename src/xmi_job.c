/*
Copyright (C) 2017 Tom Schoonjans and Laszlo Vincze

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <config.h>
#include "xmi_job.h"
#include "xmi_type_builtins.h"
#include "xmi_aux.h"
#include "xmi_xml.h"
#include "xmi_gobject.h"
#include <string.h>
#include <gio/gio.h>

#define XMI_OBJECT_REF(obj) \
	xmi_object_ref(obj, G_STRLOC)

#define XMI_OBJECT_UNREF(obj) \
	xmi_object_unref(obj, G_STRLOC)

//#define XMI_OBJECT_REF(obj) g_object_ref(obj)
//#define XMI_OBJECT_UNREF(obj) g_object_unref(obj)

enum {
	STDOUT_EVENT,
	STDERR_EVENT,
	FINISHED_EVENT,
	SPECIAL_EVENT,
	PROGRESS_EVENT,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

enum {
	PROP_JOB_0,
	PROP_JOB_EXECUTABLE,
	PROP_JOB_XMSI_FILE,
	PROP_JOB_OPTIONS,
	PROP_JOB_SPE_CONV,
	PROP_JOB_CSV_CONV,
	PROP_JOB_HTML_CONV,
	PROP_JOB_EXTRA_OPTIONS,
	N_JOB_PROPERTIES
};

static GParamSpec *job_props[N_JOB_PROPERTIES] = {NULL, };

G_LOCK_DEFINE_STATIC(current_job);
static XmiMsimJob *current_job = NULL;

#ifdef G_OS_WIN32
#include "xmi_detector.h"
#include "xmi_solid_angle.h"
#include <windows.h>
#include <winbase.h>
#include <io.h>
typedef LONG (NTAPI *pNtSuspendProcess )(IN HANDLE ProcessHandle );
typedef LONG (NTAPI *pNtResumeProcess )(IN HANDLE ProcessHandle );
static pNtSuspendProcess NtSuspendProcess = NULL;
static pNtResumeProcess NtResumeProcess = NULL;
#include "xmi_win32_inputstream.h"
#else
#include <errno.h>
#include <sys/wait.h>
#include <gio/gunixinputstream.h>
#endif

struct _XmiMsimJob {
	GObject parent_instance;
	GPid gpid;
	int pid;
	gboolean running;
	gboolean paused;
	gboolean finished;
	gboolean successful;
	gboolean do_not_emit;
	gboolean killed;
	gboolean send_all_stdout_events;
	GDataInputStream *stdout_input_stream;
	GDataInputStream *stderr_input_stream;
	GCancellable *cancellable;
	guint child_watch_id;
	guint stdout_watch_id;
	guint stderr_watch_id;
	gchar *executable;
	gchar *xmsi_file;
	xmi_main_options *options;
	gchar *spe_conv;
	gchar *csv_conv;
	gchar *html_conv;
	gchar **extra_options;
	gchar *xmso_file;
	GPtrArray *argv;
};

struct _XmiMsimJobClass {
	GObjectClass parent_class;
};

G_DEFINE_TYPE(
	XmiMsimJob,
	xmi_msim_job,
	G_TYPE_OBJECT)

/**
 * xmi_msim_job_kill:
 * @job: the #XmiMsimJob instance.
 * @error: return location for a GError, or NULL
 *
 * Kills the job, if currently running. Basically the same as #xmi_msim_job_stop(), but #XmiMsimJob::finished-event will not be emitted.
 *
 * Returns: %TRUE if the job was killed, %FALSE otherwise
 */
gboolean xmi_msim_job_kill(XmiMsimJob *job, GError **error) {
	if (job == NULL) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "job cannot be NULL");
		return FALSE;
	}
	job->do_not_emit = TRUE;

	if (xmi_msim_job_is_running(job) == TRUE) {
		g_source_remove(job->child_watch_id);
		g_source_remove(job->stdout_watch_id);
		g_source_remove(job->stderr_watch_id);
	}

	return xmi_msim_job_stop(job, error);
}

/**
 * xmi_msim_job_stop:
 * @job: the #XmiMsimJob instance.
 * @error: return location for a GError, or NULL
 *
 * Stops the job, if currently running. #XmiMsimJob::finished-event will be emitted.
 *
 * Returns: %TRUE if the job was stopped, %FALSE otherwise
 */
gboolean xmi_msim_job_stop(XmiMsimJob *job, GError **error) {
	// difference UNIX <-> Windows
	// UNIX -> send sigkill signal
	// Windows -> TerminateProcess
	
	if (job == NULL) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "job cannot be NULL");
		return FALSE;
	}

	// first check if still running
	if (xmi_msim_job_is_running(job) == FALSE) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_UNAVAILABLE, "job is not running");
		return FALSE;	
	}

	if (job->killed) {
		return TRUE;
	}
	job->killed = TRUE;
	g_debug("Killing job with pid %d", job->pid);
#ifdef G_OS_UNIX
	if (kill(job->gpid, SIGTERM) == -1) {
		g_set_error(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_PROCESS, "Process %d could not be terminated with the SIGTERM signal: %s", job->pid, strerror(errno));
		return FALSE;
	}
	// if paused, resume with SIGCONT, otherwise nothing will happen on Linux... Seems to work fine without on macOS though...
	if (job->paused) {
		if (kill(job->gpid, SIGCONT) == -1) {
			g_set_error(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_PROCESS, "Process %d could not be resumed with the SIGCONT signal, after sending SIGTERM: %s", job->pid, strerror(errno));
			return FALSE;
		}
	}
#elif defined(G_OS_WIN32)
	if (TerminateProcess((HANDLE) job->gpid, (UINT) 1) == 0) {
		if (error != NULL) {
			gchar *error_msg = g_win32_error_message(GetLastError());
			g_set_error(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_PROCESS, "Process %d could not be terminated with the TerminateProcess call: %s", job->pid, error_msg);
			g_free(error_msg);
		}
		return FALSE;	
	}
#else
	// unsupported platform??
	g_set_error(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_PROCESS, "Process %d could not be terminated on this unknown platform", job->pid);
	return FALSE;
#endif
	// a proper kill will result in the child_watcher being called, leading to a FINISHED_EVENT getting emitted, unless do_not_emit is set...
	return TRUE;
}

/**
 * xmi_msim_job_suspend:
 * @job: the #XmiMsimJob instance.
 * @error: return location for a GError, or NULL
 *
 * Suspends the job, if currently running.
 *
 * Returns: %TRUE if the job was suspended, %FALSE otherwise
 */
gboolean xmi_msim_job_suspend(XmiMsimJob *job, GError **error) {
	if (job == NULL) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "job cannot be NULL");
		return FALSE;
	}

	// check if we can suspend
	if (xmi_msim_job_is_suspend_available() == FALSE) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_UNAVAILABLE, "suspend is not supported on this platform");
		return FALSE;	
	}

	// check if still running
	if (xmi_msim_job_is_running(job) == FALSE) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_UNAVAILABLE, "job is not running");
		return FALSE;	
	}
	// then confirm it isn't paused already...
	if (xmi_msim_job_is_suspended(job) == TRUE) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_UNAVAILABLE, "job is already suspended");
		return FALSE;	
	}

	g_debug("Suspending job with pid %d", job->pid);
#ifdef G_OS_UNIX
	if (kill((pid_t) job->gpid, SIGSTOP) != 0) {
		g_set_error(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_PROCESS, "Process %d could not be suspended with the SIGSTOP signal: %s", job->pid, strerror(errno));
		return FALSE;	
	
	}
#elif defined(G_OS_WIN32)
	if (NtSuspendProcess((HANDLE) job->gpid) != 0) {
		if (error != NULL) {
			gchar *error_msg = g_win32_error_message(GetLastError());
			g_set_error(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_PROCESS, "Process %d could not be suspended with the NtSuspendProcess call: %s", job->pid, error_msg);
			g_free(error_msg);
		}
		return FALSE;	
	}
#endif
	job->paused = TRUE;

	return TRUE;
}

/**
 * xmi_msim_job_resume:
 * @job: the #XmiMsimJob instance.
 * @error: return location for a GError, or NULL
 *
 * Resumes the job, if currently suspended.
 *
 * Returns: %TRUE if the job was resumed, %FALSE otherwise
 */
gboolean xmi_msim_job_resume(XmiMsimJob *job, GError **error) {
	if (job == NULL) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "job cannot be NULL");
		return FALSE;
	}

	// check if we can resume 
	if (xmi_msim_job_is_suspend_available() == FALSE) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_UNAVAILABLE, "resume is not supported on this platform");
		return FALSE;	
	}

	// check if still running
	if (xmi_msim_job_is_running(job) == FALSE) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_UNAVAILABLE, "job is not running");
		return FALSE;	
	}

	// then confirm it isn't paused already...
	if (xmi_msim_job_is_suspended(job) == FALSE) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_UNAVAILABLE, "job is not suspended");
		return FALSE;	
	}

	g_debug("Resuming job with pid %d", job->pid);
#ifdef G_OS_UNIX
	if (kill((pid_t) job->gpid, SIGCONT) != 0) {
		g_set_error(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_PROCESS, "Process %d could not be resumed with the SIGcont signal: %s", job->pid, strerror(errno));
		return FALSE;	
	
	}
#elif defined(G_OS_WIN32)
	if (NtResumeProcess((HANDLE) job->gpid) != 0) {
		if (error != NULL) {
			gchar *error_msg = g_win32_error_message(GetLastError());
			g_set_error(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_PROCESS, "Process %d could not be resumed with the NtResumeProcess call: %s", job->pid, error_msg);
			g_free(error_msg);
		}
		return FALSE;	
	
	}
#endif
	job->paused = FALSE;

	return TRUE;
}

static void xmi_msim_job_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_job_parent_class)->dispose(gobject);
}

static void xmi_msim_job_finalize(GObject *gobject) {
	g_debug("Entering xmi_msim_job_finalize");
	XmiMsimJob *job = XMI_MSIM_JOB(gobject);
	
	xmi_msim_job_kill(job, NULL);

	g_ptr_array_free(job->argv, TRUE);
	g_free(job->executable);
	g_free(job->xmsi_file);
	xmi_main_options_free(job->options);
	g_free(job->spe_conv);
	g_free(job->csv_conv);
	g_free(job->html_conv);
	g_strfreev(job->extra_options);

	g_free(job->xmso_file);
	g_object_unref(job->cancellable);

	if (job->stdout_input_stream) {
		GInputStream *base_stream = g_object_ref(g_filter_input_stream_get_base_stream(G_FILTER_INPUT_STREAM(job->stdout_input_stream)));
		g_debug("stdout base stream ref_count: %d", G_OBJECT(base_stream)->ref_count);
		g_debug("stdout base stream closed: %d", g_input_stream_is_closed(base_stream));
		if (!g_input_stream_is_closed(base_stream)) {
			g_input_stream_close(base_stream, NULL, NULL);
		}
		g_debug("stdout stream ref_count: %d", G_OBJECT(job->stdout_input_stream)->ref_count);
		g_object_unref(job->stdout_input_stream);
		g_object_unref(base_stream);
	}

	if (job->stderr_input_stream) {
		GInputStream *base_stream = g_object_ref(g_filter_input_stream_get_base_stream(G_FILTER_INPUT_STREAM(job->stderr_input_stream)));
		g_debug("stderr base stream ref_count: %d", G_OBJECT(base_stream)->ref_count);
		g_debug("stderr base stream closed: %d", g_input_stream_is_closed(base_stream));
		if (!g_input_stream_is_closed(base_stream)) {
			g_input_stream_close(base_stream, NULL, NULL);
		}
		g_debug("stderr stream ref_count: %d", G_OBJECT(job->stderr_input_stream)->ref_count);
		g_object_unref(job->stderr_input_stream);
		g_object_unref(base_stream);
	}

	G_OBJECT_CLASS(xmi_msim_job_parent_class)->finalize(gobject);
}

static void init_globals() {
#ifdef G_OS_WIN32
	if (NtSuspendProcess) {
		return;
	}
	HMODULE ntdll = LoadLibrary( "ntdll.dll" );
	if (ntdll) {
		NtSuspendProcess = (pNtSuspendProcess)GetProcAddress(ntdll, "NtSuspendProcess" );
		NtResumeProcess = (pNtResumeProcess)GetProcAddress(ntdll, "NtResumeProcess" );
		FreeLibrary(ntdll);
	}
#endif
}

static void xmi_msim_job_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

	XmiMsimJob *job = XMI_MSIM_JOB(object);

	if (job->running || job->finished) {
		g_critical("cannot set properties after job has started");
		return;
	}

	switch (prop_id) {
		case PROP_JOB_EXECUTABLE:
			g_free(job->executable);
 			job->executable = g_value_dup_string(value);
			break;
		case PROP_JOB_XMSI_FILE:
			g_free(job->xmsi_file);
 			job->xmsi_file = g_value_dup_string(value);
			break;
		case PROP_JOB_OPTIONS:
			xmi_main_options_free(job->options);
 			job->options = g_value_dup_boxed(value);
			break;
		case PROP_JOB_SPE_CONV:
			g_free(job->spe_conv);
 			job->spe_conv = g_value_dup_string(value);
			break;
		case PROP_JOB_CSV_CONV:
			g_free(job->csv_conv);
 			job->csv_conv = g_value_dup_string(value);
			break;
		case PROP_JOB_HTML_CONV:
			g_free(job->html_conv);
 			job->html_conv = g_value_dup_string(value);
			break;
    		case PROP_JOB_EXTRA_OPTIONS:
			g_strfreev(job->extra_options);
 			job->extra_options = g_value_dup_boxed(value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
	}
}

static void xmi_msim_job_get_property(GObject *object, guint prop_id, GValue *value,  GParamSpec *pspec) {

	XmiMsimJob *job = XMI_MSIM_JOB(object);

	switch (prop_id) {
		case PROP_JOB_EXECUTABLE:
			if (job->executable)
				g_value_set_string(value, job->executable);
			else
				g_value_copy(g_param_spec_get_default_value(pspec), value);
			break;
		case PROP_JOB_XMSI_FILE:
			g_value_set_string(value, job->xmsi_file);
			break;
		case PROP_JOB_OPTIONS:
			g_value_set_boxed(value, job->xmsi_file);
			break;
		case PROP_JOB_SPE_CONV:
			g_value_set_string(value, job->spe_conv);
			break;
		case PROP_JOB_CSV_CONV:
			g_value_set_string(value, job->csv_conv);
			break;
		case PROP_JOB_HTML_CONV:
			g_value_set_string(value, job->html_conv);
			break;
		case PROP_JOB_EXTRA_OPTIONS:
			g_value_set_boxed(value, job->extra_options);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
	}
}

static void xmi_msim_job_class_init(XmiMsimJobClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_job_dispose;
	object_class->finalize = xmi_msim_job_finalize;
	object_class->set_property = xmi_msim_job_set_property;
	object_class->get_property = xmi_msim_job_get_property;
	
	/**
	 * XmiMsimJob::stdout-event:
	 * @job: The #XmiMsimJob object emitting the signal
	 * @message: The %stdout line produced by the %xmimsim executable
	 *
	 * Emitted whenever a regular %stdout message was produced by the %xmimsim executable.
	 * Special messages can also be emitted, if #xmi_msim_job_send_all_stdout_events() is set to %TRUE
	 */
	signals[STDOUT_EVENT] = g_signal_new(
		"stdout-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		NULL,
		G_TYPE_NONE,
		1,
		G_TYPE_STRING // gchar*
	);

	/**
	 * XmiMsimJob::stderr-event:
	 * @job: The #XmiMsimJob object emitting the signal
	 * @message: The %stderr line produced by the %xmimsim executable
	 *
	 * Emitted whenever a %stderr message was produced by the %xmimsim executable.
	 */
	signals[STDERR_EVENT] = g_signal_new(
		"stderr-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		NULL,
		G_TYPE_NONE,
		1,
		G_TYPE_STRING // gchar*
	);

	/**
	 * XmiMsimJob::finished-event:
	 * @job: The #XmiMsimJob object emitting the signal
	 * @status: The exit status of the finished job. %TRUE if successful, %FALSE otherwise
	 * @message: An appropriate message marking that the job is now finished.
	 *
	 * Emitted when the #XmiMsimJob has finished.
	 */
	signals[FINISHED_EVENT] = g_signal_new(
		"finished-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		NULL,
		G_TYPE_NONE,
		2,
		G_TYPE_BOOLEAN, // gboolean 
		G_TYPE_STRING // gchar*
	);

	/**
	 * XmiMsimJob::special-event:
	 * @job: The #XmiMsimJob object emitting the signal
	 * @event_type: The type of special event that occurred.
	 * @message: A message containing a description of the event.
	 *
	 * Emitted whenever a 'special' event has occurred.
	 */
	signals[SPECIAL_EVENT] = g_signal_new(
		"special-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		NULL,
		G_TYPE_NONE,
		2,
		XMI_MSIM_TYPE_JOB_SPECIAL_EVENT, // XmiMsimJobSpecialEvent
		G_TYPE_STRING // gchar*
	);

	/**
	 * XmiMsimJob::progress-event:
	 * @job: The #XmiMsimJob object emitting the signal
	 * @event_type: The type of special event that occurred.
	 * @fraction: the progress fraction that has been reached when this event was emitted
	 *
	 * Emitted whenever a 'special' event has occurred that corresponds to a change in progress.
	 */
	signals[PROGRESS_EVENT] = g_signal_new(
		"progress-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		NULL,
		G_TYPE_NONE,
		2,
		XMI_MSIM_TYPE_JOB_SPECIAL_EVENT, // XmiMsimJobSpecialEvent
		G_TYPE_DOUBLE // double (between 0 and 1)
	);

	job_props[PROP_JOB_EXECUTABLE] = g_param_spec_string(
		"executable",
		"XMI-MSIM main executable",
		"Full path to the XMI-MSIM main executable",
		xmi_get_xmimsim_path(),
    		G_PARAM_READWRITE
	);

	job_props[PROP_JOB_XMSI_FILE] = g_param_spec_string(
		"xmsi-file",
		"XMI-MSIM input-file",
		"XMI-MSIM input-file",
		NULL,
    		G_PARAM_READWRITE
	);

	job_props[PROP_JOB_OPTIONS] = g_param_spec_boxed(
		"options",
		"XMI-MSIM options",
		"XMI-MSIM options",
		XMI_MSIM_TYPE_MAIN_OPTIONS,
    		G_PARAM_READWRITE
	);

	job_props[PROP_JOB_SPE_CONV] = g_param_spec_string(
		"spe-conv",
		"SPE outputfile prefix",
		"SPE outputfile prefix",
		NULL,
    		G_PARAM_READWRITE
	);

	job_props[PROP_JOB_CSV_CONV] = g_param_spec_string(
		"csv-conv",
		"CSV outputfile",
		"CSV outputfile",
		NULL,
    		G_PARAM_READWRITE
	);

	job_props[PROP_JOB_HTML_CONV] = g_param_spec_string(
		"html-conv",
		"HTML outputfile",
		"HTML outputfile",
		NULL,
    		G_PARAM_READWRITE
	);

	job_props[PROP_JOB_EXTRA_OPTIONS] = g_param_spec_boxed(
		"extra-options",
		"XMI-MSIM extra options",
		"Extra options XMI-MSIM main executable",
		G_TYPE_STRV,
    		G_PARAM_READWRITE
	);

	g_object_class_install_properties(object_class, N_JOB_PROPERTIES, job_props);
	

	init_globals();
}

static void xmi_msim_job_init(XmiMsimJob *self) {
	self->cancellable = g_cancellable_new();
}

/**
 * xmi_msim_job_new: (constructor)
 * @executable: (nullable): the full path to the %xmimsim executable. If %NULL, the default executable will be used.
 * @xmsi_file: (not nullable): the full path to the xmsi input-file
 * @options: (not nullable): an #XmiMsimMainOptions boxed struct containing the options for the job
 * @spe_conv: (nullable): prefix to the SPE files
 * @csv_conv: (nullable): full path to a CSV file
 * @html_conv: (nullable): full path to an HTML file
 * @extra_options: (nullable) (array zero-terminated=1) (element-type utf8): %NULL terminated array of additional options to pass to the executable
 *
 * Instantiate a new #XmiMsimJob job object. Use the available methods to start/stop/suspend/resume as well as its signals to follow progress, messages and termination.
 *
 * Returns: (transfer full): the #XmiMsimJob object, or %NULL if an error occurred.
 */
XmiMsimJob* xmi_msim_job_new(
	const gchar *executable,
	const gchar *xmsi_file,
	xmi_main_options *options,
	const gchar *spe_conv,
	const gchar *csv_conv,
	const gchar *html_conv,
	gchar **extra_options
	) {

	g_return_val_if_fail(xmsi_file != NULL, NULL);
	g_return_val_if_fail(options != NULL, NULL);

	return g_object_new(
		XMI_MSIM_TYPE_JOB,
		"executable", executable,
		"xmsi-file", xmsi_file,
		"options", options,
		"spe-conv", spe_conv,
		"csv-conv", csv_conv,
		"html-conv", html_conv,
		"extra-options", extra_options,
		NULL
		);
}

static void xmimsim_child_watcher_cb(GPid gpid, gint status, XmiMsimJob *job) {
	// called when the child dies, either by natural causes or if it is killed
	// either way, this function must finish with a signal being emitted
	g_cancellable_cancel(job->cancellable);
	job->killed = TRUE; // to ensure that the IO watcher gets deactivated
	char *buffer;
	gboolean success;
	int pid = job->pid;
	gchar *xmi_msim_executable = g_ptr_array_index(job->argv, 0);

	g_spawn_close_pid(job->gpid);

	// free up current_job
	//G_LOCK(current_job);
	current_job = NULL;
	//G_UNLOCK(current_job);

	//windows <-> unix issues here
	//unix allows to obtain more info about the way the process was terminated, windows will just have the exit code (status)
	//conditional compilation here
#ifdef G_OS_UNIX
	if (WIFEXITED(status)) {
		if (WEXITSTATUS(status) == 0) { /* child was terminated due to a internal call to exit */
			buffer = g_strdup_printf("%s with process id %d exited normally without errors\n", xmi_msim_executable, pid);
			success = 1;
		}
		else {
			buffer = g_strdup_printf("%s with process id %i exited with an error (code: %i)\n", xmi_msim_executable, pid, WEXITSTATUS(status));
			success = 0;
		}
	}
	else if (WIFSIGNALED(status)) { /* child was terminated due to an external signal */
		buffer = g_strdup_printf( "%s with process id %i was terminated by signal %i\n", xmi_msim_executable, pid, WTERMSIG(status));
		success = 0;
	}
	else {
		buffer = g_strdup_printf( "%s with process id %i was terminated in some special way\n", xmi_msim_executable, pid);
		success = 0;
	}

#elif defined(G_OS_WIN32)
	if (status == 0) {
		buffer = g_strdup_printf("%s with process id %i exited normally without errors\n", xmi_msim_executable, pid);
		success = 1;
	}
	else {
		buffer = g_strdup_printf("%s with process id %i exited with an error (code: %i)\n", xmi_msim_executable, pid, status);
		success = 0;
	}
#endif

	job->running = FALSE;
	job->paused = FALSE;
	job->finished = TRUE;
	job->successful = success;
	if (!job->do_not_emit) /* necessary to avoid blunt kills from emitting useless signals  */
		g_signal_emit(job, signals[FINISHED_EVENT], 0, success, buffer);
	g_free(buffer);
}

#define XMI_STRNCMP(string1, string2) strncmp(string1, string2, strlen(string2))

#define SOLID_ANGLE_GRID_ALREADY_PRESENT "Solid angle grid already present in "
#define SOLID_ANGLE_GRID_REDUNDANT "Operating in brute-force mode: solid angle grid is redundant"
#define SOLID_ANGLE_GRID_PRECALCULATING "Precalculating solid angle grid"
#define SOLID_ANGLE_GRID_FINISHED "Solid angle calculation finished"

#define SIMULATING_INTERACTIONS "Simulating interactions"
#define SIMULATING_FINISHED "Interactions simulation finished"

#define ESCAPE_RATIOS_ALREADY_PRESENT "Escape peak ratios already present in "
#define ESCAPE_RATIOS_REDUNDANT "No escape peaks requested: escape peak calculation is redundant"
#define ESCAPE_RATIOS_PRECALCULATING "Precalculating escape peak ratios"
#define ESCAPE_RATIOS_FINISHED "Escape peak ratios calculation finished"

static gboolean process_stdout_channel_string(XmiMsimJob *job, gchar *string) {
	int percentage;
	char *buffer;

	//solid angles
	if(XMI_STRNCMP(string, SOLID_ANGLE_GRID_ALREADY_PRESENT) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE, "Solid angle grid loaded from file");
		return TRUE;
	}
	else if (XMI_STRNCMP(string, SOLID_ANGLE_GRID_REDUNDANT) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE, "Solid angle grid redundant");
		return TRUE;
	}
	else if(XMI_STRNCMP(string, SOLID_ANGLE_GRID_PRECALCULATING) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE, 0.0);
		return TRUE;
	}
	else if(sscanf(string, "Solid angle calculation at %i", &percentage) == 1) {
		buffer = g_strdup_printf("Solid angle grid: %i %%",percentage);
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE, ((double) percentage)/100.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE, buffer);
		g_free(buffer);
		return FALSE;
	}
	else if(XMI_STRNCMP(string, SOLID_ANGLE_GRID_FINISHED) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE, "Solid angle grid calculated");
		return TRUE;
	}
	//interactions
	else if(sscanf(string, "Simulating interactions at %i", &percentage) == 1) {
		buffer = g_strdup_printf("Simulating interactions: %i %%",percentage);
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SIMULATION, ((double) percentage)/100.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SIMULATION, buffer);
		g_free(buffer);
		return FALSE;
	}
	else if(XMI_STRNCMP(string, SIMULATING_INTERACTIONS) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SIMULATION, 0.0);
		return TRUE;
	}
	else if(XMI_STRNCMP(string, SIMULATING_FINISHED) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SIMULATION, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_SIMULATION, "Interactions simulated");
		return TRUE;
	}
	//escape ratios
	else if(XMI_STRNCMP(string, ESCAPE_RATIOS_ALREADY_PRESENT) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, "Escape peak ratios loaded from file");
		return TRUE;
	}
	else if(XMI_STRNCMP(string, ESCAPE_RATIOS_REDUNDANT) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, "Escape peaks redundant");
		return TRUE;
	}
	else if(XMI_STRNCMP(string, ESCAPE_RATIOS_PRECALCULATING) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, 0.0);
		return TRUE;
	}
	else if(sscanf(string, "Escape peak ratios calculation at %i", &percentage) == 1) {
		buffer = g_strdup_printf("Escape peak ratios: %i %%", percentage);
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, ((double) percentage)/100.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, buffer);
		g_free(buffer);
		return FALSE;
	}
	else if(XMI_STRNCMP(string, ESCAPE_RATIOS_FINISHED) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, "Escape peak ratios calculated");
		return TRUE;
	}

	return TRUE;
}

static void xmimsim_io_stream_callback(GObject *source_object, GAsyncResult *res, gpointer user_data) {
	GDataInputStream *stream = G_DATA_INPUT_STREAM(source_object);
	XmiMsimJob *job = XMI_MSIM_JOB(user_data);
	GError *error = NULL;
	gchar *xmi_msim_executable = g_ptr_array_index(job->argv, 0);
	int pid = job->pid;

	gchar *buffer = g_data_input_stream_read_line_finish_utf8(stream, res, NULL, &error);
	if (buffer == NULL) {

		if (error && !g_error_matches(error, G_IO_ERROR, G_IO_ERROR_CANCELLED)) {
			buffer = g_strdup_printf("%s with process id %i had an I/O error: %s\n", xmi_msim_executable, pid, error->message);
			g_signal_emit(job, signals[STDERR_EVENT], 0, buffer);
			g_free(buffer);
		}

		GInputStream *base_stream = g_filter_input_stream_get_base_stream(G_FILTER_INPUT_STREAM(stream));
		if (!g_input_stream_is_closed(base_stream)) {
			g_input_stream_close(base_stream, NULL, NULL);
		}
		g_object_unref(job);
	}
	else {
		g_data_input_stream_read_line_async(stream, G_PRIORITY_DEFAULT, NULL, xmimsim_io_stream_callback, job);
		if (stream == job->stdout_input_stream) {
			// if stdout -> parse and send out specific event signal if necessary
			if (process_stdout_channel_string(job, buffer) || job->send_all_stdout_events) {
				g_signal_emit(job, signals[STDOUT_EVENT], 0, buffer);
			}
		}
		else if (stream == job->stderr_input_stream) {
			g_signal_emit(job, signals[STDERR_EVENT], 0, buffer);
		}
		else {
			g_critical("Unknown source detected!");
		}
		g_free(buffer);
	}
}

/**
 * xmi_msim_job_start:
 * @job: the #XmiMsimJob instance.
 * @error: return location for a GError, or NULL
 *
 * Starts the job
 *
 * Returns: %TRUE if the job was succesfully started, %FALSE otherwise
 */
gboolean xmi_msim_job_start(XmiMsimJob *job, GError **error) {
	if (job == NULL) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "job cannot be NULL");
		return FALSE;
	}

	gboolean spawn_rv;

	// check if there is no other process running!
	// this could be the same job...
	G_LOCK(current_job);
	if (current_job != NULL) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_ANOTHER_JOB_RUNNING, "another job is already running");
		G_UNLOCK(current_job);
		return FALSE;
	}

	if (job->finished) {
		// do not allow old finished jobs to be restarted!
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_UNAVAILABLE, "this job has finished already");
		G_UNLOCK(current_job);
		return FALSE;
	}

	if (!job->xmsi_file) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "xmsi_file cannot be NULL");
		G_UNLOCK(current_job);
		return FALSE;
	}

	if (!job->options) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "options cannot be NULL");
		G_UNLOCK(current_job);
		return FALSE;
	}

	// prepare input
	job->argv = g_ptr_array_new_full(10, g_free);
	gchar *executable = job->executable != NULL ? g_strdup(job->executable) : g_value_dup_string(g_param_spec_get_default_value(job_props[PROP_JOB_EXECUTABLE]));
	g_ptr_array_add(job->argv, executable);

	if (job->options->use_M_lines) {
		g_ptr_array_add(job->argv, g_strdup("--enable-M-lines"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-M-lines"));
	}

	if (job->options->use_cascade_radiative) {
		g_ptr_array_add(job->argv, g_strdup("--enable-radiative-cascade"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-radiative-cascade"));
	}

	if (job->options->use_cascade_auger) {
		g_ptr_array_add(job->argv, g_strdup("--enable-auger-cascade"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-auger-cascade"));
	}

	if (job->options->use_variance_reduction) {
		g_ptr_array_add(job->argv, g_strdup("--enable-variance-reduction"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-variance-reduction"));
	}

	if (job->options->use_sum_peaks) {
		g_ptr_array_add(job->argv, g_strdup("--enable-pile-up"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-pile-up"));
	}

	if (job->options->use_poisson) {
		g_ptr_array_add(job->argv, g_strdup("--enable-poisson"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-poisson"));
	}
	
	if (job->options->use_escape_peaks) {
		g_ptr_array_add(job->argv, g_strdup("--enable-escape-peaks"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-escape-peaks"));
	}

	if (job->options->use_advanced_compton) {
		g_ptr_array_add(job->argv, g_strdup("--enable-advanced-compton"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-advanced-compton"));
	}

	if (job->options->use_default_seeds) {
		g_ptr_array_add(job->argv, g_strdup("--enable-default-seeds"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-default-seeds"));
	}

	if (job->options->extra_verbose) {
		g_ptr_array_add(job->argv, g_strdup("--very-verbose"));
	}
	else { /* always verbose!!! */
		g_ptr_array_add(job->argv, g_strdup("--verbose"));
	}

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H) || defined(HAVE_METAL)
	if (job->options->use_gpu) {
		g_ptr_array_add(job->argv, g_strdup("--enable-gpu"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-gpu"));
	}
#endif

	gchar *temp = g_strdup(job->options->custom_detector_response);
	if (temp && strlen(g_strstrip(temp))) {
		g_ptr_array_add(job->argv, g_strdup_printf("--custom-detector-response=%s", temp));
	}
	g_free(temp);

	temp = g_strdup(job->spe_conv);
	if (temp && strlen(g_strstrip(temp))) {
		g_ptr_array_add(job->argv, g_strdup_printf("--spe-file=%s", temp));
	}
	g_free(temp);

	temp = g_strdup(job->csv_conv);
	if (temp && strlen(g_strstrip(temp))) {
		g_ptr_array_add(job->argv, g_strdup_printf("--csv-file=%s", temp));
	}
	g_free(temp);

	temp = g_strdup(job->html_conv);
	if (temp && strlen(g_strstrip(temp))) {
		g_ptr_array_add(job->argv, g_strdup_printf("--htm-file=%s", temp));
	}
	g_free(temp);

#ifdef G_OS_WIN32
	//set solid angles and escape ratios files ourself!
	char *xmimsim_hdf5_solid_angles = NULL;
	char *xmimsim_hdf5_escape_ratios = NULL;

	if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_HDF5, "Could not determine solid angles HDF5 file");
		G_UNLOCK(current_job);
		return FALSE;
	}
	g_ptr_array_add(job->argv, g_strdup_printf("--with-solid-angles-data=%s", xmimsim_hdf5_solid_angles));
	g_free(xmimsim_hdf5_solid_angles);

	if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_HDF5, "Could not determine escape ratios HDF5 file");
		G_UNLOCK(current_job);
		return FALSE;
	}
	g_ptr_array_add(job->argv, g_strdup_printf("--with-escape-ratios-data=%s", xmimsim_hdf5_escape_ratios));
	g_free(xmimsim_hdf5_escape_ratios);
#endif

	// this is a hack to let us use batch extra options to set the number of threads :-)
	if (job->options->omp_num_threads != xmi_omp_get_max_threads()) {
		g_ptr_array_add(job->argv, g_strdup_printf("--set-threads=%d", job->options->omp_num_threads));
	}

	// extra options
	if (job->extra_options) {
		int i;
		for (i = 0 ; job->extra_options[i] != NULL ; i++) {
			g_ptr_array_add(job->argv, g_strdup(job->extra_options[i]));
		}
	}

	g_ptr_array_add(job->argv, g_strdup(job->xmsi_file));
	g_ptr_array_add(job->argv, NULL);

	gchar *wd = g_path_get_dirname(job->xmsi_file);

	// read xmsi_file to determine the output-file!
	xmi_input *input = NULL;
	if ((input = xmi_input_read_from_xml_file(job->xmsi_file, error)) == NULL) {
		G_UNLOCK(current_job);
		return FALSE;
	}

	job->xmso_file = g_strdup(input->general->outputfile);
	xmi_input_free(input);

	gint out_fh;
	gint err_fh;

	spawn_rv = g_spawn_async_with_pipes(wd, (gchar **) job->argv->pdata, NULL, G_SPAWN_DO_NOT_REAP_CHILD, NULL, NULL, &job->gpid, NULL, &out_fh, &err_fh, error);

	g_free(wd);

	if (spawn_rv == FALSE) {
		G_UNLOCK(current_job);
		return FALSE;
	}

#ifdef G_OS_WIN32
	job->pid = (gint) GetProcessId((HANDLE) job->gpid);
#else
	job->pid = (gint) job->gpid;
#endif

	g_debug("Starting job with pid %d", job->pid);

#ifdef G_OS_WIN32
	GInputStream *stdout_input_base_stream = xmi_msim_win32_input_stream_new_from_fd(out_fh, TRUE);
	GInputStream *stderr_input_base_stream = xmi_msim_win32_input_stream_new_from_fd(err_fh, TRUE);
#else
	GInputStream *stdout_input_base_stream = g_unix_input_stream_new(out_fh, TRUE);
	GInputStream *stderr_input_base_stream = g_unix_input_stream_new(err_fh, TRUE);
#endif

	job->child_watch_id = g_child_watch_add_full(G_PRIORITY_DEFAULT_IDLE, job->gpid, (GChildWatchFunc) xmimsim_child_watcher_cb, g_object_ref(job), g_object_unref);

	job->stderr_input_stream = g_data_input_stream_new(stderr_input_base_stream);
	g_object_unref(stderr_input_base_stream);
#ifdef G_OS_WIN32
	g_data_input_stream_set_newline_type(G_DATA_INPUT_STREAM(job->stderr_input_stream), G_DATA_STREAM_NEWLINE_TYPE_ANY);
#endif
	g_data_input_stream_read_line_async(job->stderr_input_stream, G_PRIORITY_DEFAULT, NULL, xmimsim_io_stream_callback, g_object_ref(job));

	job->stdout_input_stream = g_data_input_stream_new(stdout_input_base_stream);
	g_object_unref(stdout_input_base_stream);
#ifdef G_OS_WIN32
	g_data_input_stream_set_newline_type(G_DATA_INPUT_STREAM(job->stdout_input_stream), G_DATA_STREAM_NEWLINE_TYPE_ANY);
#endif
	g_data_input_stream_read_line_async(job->stdout_input_stream, G_PRIORITY_DEFAULT, NULL, xmimsim_io_stream_callback, g_object_ref(job));

	job->running = TRUE;
	current_job = job;
	G_UNLOCK(current_job);

	return TRUE;
}

/**
 * xmi_msim_job_get_pid:
 * @job: the #XmiMsimJob instance.
 * @pid: (out): the job PID.
 * @error: return location for a GError, or NULL
 *
 * Gets the PID of the job. The job must have started for this to work.
 *
 * Returns: %TRUE if the PID was available, %FALSE otherwise
 */
gboolean xmi_msim_job_get_pid(XmiMsimJob *job, gint *pid, GError **error) {
	if (job == NULL) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "job cannot be NULL");
		return FALSE;
	}

	if (pid == NULL) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "pid cannot be NULL");
		return FALSE;
	}

	if (job->pid == 0) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_UNAVAILABLE, "job has not been successfully started yet");
		return FALSE;
	}
	*pid = (gint) job->pid;
	return TRUE;
}

/**
 * xmi_msim_job_is_running:
 * @job: the #XmiMsimJob instance.
 *
 * Returns whether or not the job is currently running. Suspended jobs will return %TRUE!
 *
 * Returns: %TRUE if the job is currently running, %FALSE otherwise
 */
gboolean xmi_msim_job_is_running(XmiMsimJob *job) {
	g_return_val_if_fail(XMI_MSIM_IS_JOB(job), FALSE);

	gboolean rv;
	rv = job->running;
	return rv;
}

/**
 * xmi_msim_job_is_suspended:
 * @job: the #XmiMsimJob instance.
 *
 * Returns: %TRUE if the job is currently suspended, %FALSE otherwise
 */
gboolean xmi_msim_job_is_suspended(XmiMsimJob *job) {
	g_return_val_if_fail(XMI_MSIM_IS_JOB(job), FALSE);

	gboolean rv;
	rv = job->paused;
	return rv;
}

/**
 * xmi_msim_job_has_finished:
 * @job: the #XmiMsimJob instance.
 *
 * Returns: %TRUE if the job has finished (successfully or not...), %FALSE otherwise
 */
gboolean xmi_msim_job_has_finished(XmiMsimJob *job) {
	g_return_val_if_fail(XMI_MSIM_IS_JOB(job), FALSE);

	gboolean rv;
	rv = job->finished;
	return rv;
}

/**
 * xmi_msim_job_was_successful:
 * @job: the #XmiMsimJob instance.
 *
 * Returns: %TRUE if the job has finished and was successful, %FALSE otherwise
 */
gboolean xmi_msim_job_was_successful(XmiMsimJob *job) {
	g_return_val_if_fail(XMI_MSIM_IS_JOB(job), FALSE);

	gboolean rv;
	rv = job->successful;
	return rv;
}

/**
 * xmi_msim_job_send_all_stdout_events:
 * @job: the #XmiMsimJob instance.
 * @setting: if %TRUE, all stdout events will be emitted via the #XmiMsimJob::stdout-event signal.
 *
 */
void xmi_msim_job_send_all_stdout_events(XmiMsimJob *job, gboolean setting) {
	g_return_if_fail(XMI_MSIM_IS_JOB(job));

	job->send_all_stdout_events = setting;
}

/**
 * xmi_msim_job_get_commands:
 * @job: the #XmiMsimJob instance.
 *
 * Returns: the complete command that is associated with this #XmiMsimJob instance. Free after usage with #g_free.
 */
gchar* xmi_msim_job_get_command(XmiMsimJob *job) {
	g_return_val_if_fail(XMI_MSIM_IS_JOB(job), NULL);

	if (job->argv)
		return g_strjoinv(" ", (gchar **) job->argv->pdata);
	return NULL;
}

GQuark xmi_msim_job_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-gui-job-error-quark");
}

/**
 * xmi_msim_job_kill_all:
 *
 * Kills whatever job is currently running.
 */
void xmi_msim_job_kill_all(void ) {
	// no further processing will be triggered, just a clean kill
	G_LOCK(current_job);
	if (current_job != NULL) {
		xmi_msim_job_kill(current_job, NULL);
	}
	current_job = NULL;
	G_UNLOCK(current_job);
}

/**
 * xmi_msim_job_is_suspend_available:
 *
 * Returns: %TRUE if jobs can be suspended, %FALSE otherwise.
 */
gboolean xmi_msim_job_is_suspend_available(void) {
	init_globals();
#ifdef G_OS_WIN32
	if (!NtSuspendProcess) {
		return FALSE;
	}
#endif
	return TRUE;
}

/**
 * xmi_msim_job_get_input_file:
 * @job: the #XmiMsimJob instance.
 *
 * Returns: the name of the input-file that will be used to run this #XmiMsim.Job
 */
gchar* xmi_msim_job_get_input_file(XmiMsimJob *job) {
	g_return_val_if_fail(XMI_MSIM_IS_JOB(job), NULL);

	return g_strdup(job->xmsi_file);
}

gchar* xmi_msim_job_get_output_file(XmiMsimJob *job) {
	g_return_val_if_fail(XMI_MSIM_IS_JOB(job), NULL);

	return g_strdup(job->xmso_file);
}
