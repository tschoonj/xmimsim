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
#include <string.h>

#define XMI_OBJECT_REF(obj) \
	xmi_object_ref(obj, G_STRLOC)

#define XMI_OBJECT_UNREF(obj) \
	xmi_object_unref(obj, G_STRLOC)

enum {
	STDOUT_EVENT,
	STDERR_EVENT,
	FINISHED_EVENT,
	SPECIAL_EVENT,
	PROGRESS_EVENT,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

G_LOCK_DEFINE_STATIC(current_job);
static XmiMsimJob *current_job = NULL;

#ifdef G_OS_WIN32
#include "xmi_detector.h"
#include "xmi_solid_angle.h"
#include <windows.h>
#include <winbase.h>
typedef LONG (NTAPI *pNtSuspendProcess )(IN HANDLE ProcessHandle );
typedef LONG (NTAPI *pNtResumeProcess )(IN HANDLE ProcessHandle );
static pNtSuspendProcess NtSuspendProcess = NULL;
static pNtResumeProcess NtResumeProcess = NULL;
#else
#include <errno.h>
#include <sys/wait.h>
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
	GPtrArray *argv;
	gchar *wd;
	GIOChannel *stdout_channel;
	GIOChannel *stderr_channel;
	guint child_watch_id;
	guint stdout_watch_id;
	guint stderr_watch_id;
	gchar *xmsi_file;
	gchar *xmso_file;
};

struct _XmiMsimJobClass {
	GObjectClass parent_class;
};

G_DEFINE_TYPE(XmiMsimJob, xmi_msim_job, G_TYPE_OBJECT)

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

	g_spawn_close_pid(job->gpid);

	g_ptr_array_free(job->argv, TRUE);

	g_free(job->wd);

	g_free(job->xmsi_file);

	g_free(job->xmso_file);

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

static void xmi_msim_job_class_init(XmiMsimJobClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_job_dispose;
	object_class->finalize = xmi_msim_job_finalize;
	
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

	init_globals();
}

static void xmi_msim_job_init(XmiMsimJob *self) {
}

/**
 * xmi_msim_job_new: (constructor)
 * @xmi_msim_executable: (not nullable): the full path to the %xmimsim executable
 * @xmsi_file: (not nullable): the full path to the xmsi input-file
 * @options: (not nullable): an #XmiMsimMainOptions boxed struct containing the options for the job
 * @spe_conv: (nullable): prefix to the SPE files
 * @csv_conv: (nullable): full path to a CSV file
 * @svg_conv: (nullable): full path to an SVG file
 * @html_conv: (nullable): full path to an HTML file
 * @extra_options: (nullable) (array zero-terminated=1) (element-type utf8): %NULL terminated array of additional options to pass to the executable
 * @error: return location for a GError, or NULL
 *
 * Instantiate a new #XmiMsimJob job object. Use the available methods to start/stop/suspend/resume as well as its signals to follow progress, messages and termination.
 *
 * Returns: (transfer full): the #XmiMsimJob object, or %NULL if an error occurred.
 */
XmiMsimJob* xmi_msim_job_new(
	const gchar *xmi_msim_executable,
	const gchar *xmsi_file,
	xmi_main_options *options,
	const gchar *spe_conv,
	const gchar *csv_conv,
	const gchar *svg_conv,
	const gchar *html_conv,
	gchar **extra_options,
	GError **error
	) {

	if (!xmsi_file) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "xmsi_file cannot be NULL");
		return NULL;
	}
	if (!xmi_msim_executable) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "xmi_msim_executable cannot be NULL");
		return NULL;
	}
	if (!options) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_INVALID_INPUT, "options cannot be NULL");
		return NULL;
	}

	XmiMsimJob *job = XMI_MSIM_JOB(g_object_new(XMI_MSIM_TYPE_JOB, NULL));

	// construct argv ptr array
	job->argv = g_ptr_array_new_full(10, g_free);
	g_ptr_array_add(job->argv, g_strdup(xmi_msim_executable));

	if (options->use_M_lines) {
		g_ptr_array_add(job->argv, g_strdup("--enable-M-lines"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-M-lines"));
	}

	if (options->use_cascade_radiative) {
		g_ptr_array_add(job->argv, g_strdup("--enable-radiative-cascade"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-radiative-cascade"));
	}

	if (options->use_cascade_auger) {
		g_ptr_array_add(job->argv, g_strdup("--enable-auger-cascade"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-auger-cascade"));
	}

	if (options->use_variance_reduction) {
		g_ptr_array_add(job->argv, g_strdup("--enable-variance-reduction"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-variance-reduction"));
	}

	if (options->use_sum_peaks) {
		g_ptr_array_add(job->argv, g_strdup("--enable-pile-up"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-pile-up"));
	}

	if (options->use_poisson) {
		g_ptr_array_add(job->argv, g_strdup("--enable-poisson"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-poisson"));
	}
	
	if (options->use_escape_peaks) {
		g_ptr_array_add(job->argv, g_strdup("--enable-escape-peaks"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-escape-peaks"));
	}

	if (options->use_advanced_compton) {
		g_ptr_array_add(job->argv, g_strdup("--enable-advanced-compton"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-advanced-compton"));
	}

	if (options->use_default_seeds) {
		g_ptr_array_add(job->argv, g_strdup("--enable-default-seeds"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-default-seeds"));
	}

	if (options->extra_verbose) {
		g_ptr_array_add(job->argv, g_strdup("--very-verbose"));
	}
	else { /* always verbose!!! */
		g_ptr_array_add(job->argv, g_strdup("--verbose"));
	}

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	if (options->use_opencl) {
		g_ptr_array_add(job->argv, g_strdup("--enable-opencl"));
	}
	else {
		g_ptr_array_add(job->argv, g_strdup("--disable-opencl"));
	}
#endif

	gchar *temp = g_strdup(options->custom_detector_response);
	if (temp && strlen(g_strstrip(temp))) {
		g_ptr_array_add(job->argv, g_strdup_printf("--custom-detector-response=%s", temp));
	}
	g_free(temp);

	temp = g_strdup(spe_conv);
	if (temp && strlen(g_strstrip(temp))) {
		g_ptr_array_add(job->argv, g_strdup_printf("--spe-file=%s", temp));
	}
	g_free(temp);

	temp = g_strdup(csv_conv);
	if (temp && strlen(g_strstrip(temp))) {
		g_ptr_array_add(job->argv, g_strdup_printf("--csv-file=%s", temp));
	}
	g_free(temp);

	temp = g_strdup(svg_conv);
	if (temp && strlen(g_strstrip(temp))) {
		g_ptr_array_add(job->argv, g_strdup_printf("--svg-file=%s", temp));
	}
	g_free(temp);

	temp = g_strdup(html_conv);
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
		XMI_OBJECT_UNREF(job);
		return NULL;
	}
	g_ptr_array_add(job->argv, g_strdup_printf("--with-solid-angles-data=%s", xmimsim_hdf5_solid_angles));
	g_free(xmimsim_hdf5_solid_angles);

	if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0) {
		g_set_error_literal(error, XMI_MSIM_JOB_ERROR, XMI_MSIM_JOB_ERROR_HDF5, "Could not determine escape ratios HDF5 file");
		XMI_OBJECT_UNREF(job);
		return NULL;
	}
	g_ptr_array_add(job->argv, g_strdup_printf("--with-escape-ratios-data=%s", xmimsim_hdf5_escape_ratios));
	g_free(xmimsim_hdf5_escape_ratios);
#endif

	g_ptr_array_add(job->argv, g_strdup_printf("--set-threads=%d", options->omp_num_threads));

	// extra options
	if (extra_options) {
		int i;
		for (i = 0 ; extra_options[i] != NULL ; i++) {
			g_ptr_array_add(job->argv, g_strdup(extra_options[i]));
		}
	}

	g_ptr_array_add(job->argv, g_strdup(xmsi_file));
	g_ptr_array_add(job->argv, NULL);

	job->wd = g_path_get_dirname(xmsi_file);

	job->xmsi_file = g_strdup(xmsi_file);

	// read xmsi_file to determine the output-file!
	xmi_input *input = NULL;
	g_return_val_if_fail((input = xmi_input_read_from_xml_file(xmsi_file, error)) != NULL, NULL);
	job->xmso_file = g_strdup(input->general->outputfile);
	xmi_input_free(input);

	return job;
}

static void xmimsim_child_watcher_cb(GPid gpid, gint status, XmiMsimJob *job) {
	// called when the child dies, either by natural causes or if it is killed
	// either way, this function must finish with a signal being emitted
	job->killed = TRUE; // to ensure that the IO watcher gets deactivated
	char *buffer;
	gboolean success;
	int pid = job->pid;
	gchar *xmi_msim_executable = g_ptr_array_index(job->argv, 0);

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

	g_spawn_close_pid(gpid);
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

static gboolean xmimsim_io_watcher(GIOChannel *source, GIOCondition condition, XmiMsimJob *job) {
	// this trick appears to work well for avoiding segfaults due to text being processed when the job has been killed
	if (job->killed) {
		return FALSE;
	}

	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	gchar *buffer = NULL;
	int pid = job->pid;
	gchar *xmi_msim_executable = g_ptr_array_index(job->argv, 0);
	gboolean rv = TRUE;

	if (condition & (G_IO_IN|G_IO_PRI)) {
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);
		if (pipe_status == G_IO_STATUS_ERROR) {
			buffer = g_strdup_printf("%s with process id %i had an I/O error: %s\n", xmi_msim_executable, pid, pipe_error->message);
			g_error_free(pipe_error);
			g_signal_emit(job, signals[STDERR_EVENT], 0, buffer);
			rv = FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			pipe_string = g_strchomp(pipe_string);
			if (source == job->stdout_channel) {
				// if stdout -> parse and send out specific event signal if necessary
				if (process_stdout_channel_string(job, pipe_string) || job->send_all_stdout_events)
					g_signal_emit(job, signals[STDOUT_EVENT], 0, pipe_string);
			}
			else if (source == job->stderr_channel) {
				g_signal_emit(job, signals[STDERR_EVENT], 0, pipe_string);
			}
			else {
				g_critical("Unknown source detected!");
			}
			g_free(pipe_string);
		}
		else
			rv = FALSE;

	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		rv = FALSE;
	}

	g_free(buffer);

	return rv;
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
	
	gint out_fh;
	gint err_fh;

	spawn_rv = g_spawn_async_with_pipes(job->wd, (gchar **) job->argv->pdata, NULL, G_SPAWN_DO_NOT_REAP_CHILD, NULL, NULL, &job->gpid, NULL, &out_fh, &err_fh, error);
	
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
	job->stderr_channel = g_io_channel_win32_new_fd(err_fh);
	job->stdout_channel = g_io_channel_win32_new_fd(out_fh);
#else
	job->stderr_channel = g_io_channel_unix_new(err_fh);
	job->stdout_channel = g_io_channel_unix_new(out_fh);
#endif

	job->child_watch_id = g_child_watch_add_full(G_PRIORITY_HIGH_IDLE, job->gpid, (GChildWatchFunc) xmimsim_child_watcher_cb, g_object_ref(job), g_object_unref);

	const gchar *encoding = NULL;
	g_get_charset(&encoding);

	g_io_channel_set_encoding(job->stdout_channel, encoding, NULL);
	g_io_channel_set_close_on_unref(job->stdout_channel, TRUE);
	job->stdout_watch_id = g_io_add_watch_full(job->stdout_channel, G_PRIORITY_DEFAULT, G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL, (GIOFunc) xmimsim_io_watcher, g_object_ref(job), g_object_unref);
	g_io_channel_unref(job->stdout_channel);

	g_io_channel_set_encoding(job->stderr_channel, encoding, NULL);
	g_io_channel_set_close_on_unref(job->stderr_channel,TRUE);
	job->stderr_watch_id = g_io_add_watch_full(job->stderr_channel, G_PRIORITY_DEFAULT, G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL, (GIOFunc) xmimsim_io_watcher, g_object_ref(job), g_object_unref);
	g_io_channel_unref(job->stderr_channel);

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
