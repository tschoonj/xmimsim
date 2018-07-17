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
#include "xmimsim-gui-job.h"
#include "xmimsim-gui-marshal.h"
#include "xmi_aux.h"
#include <string.h>

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
static XmiMsimGuiJob *current_job = NULL;

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

struct _XmiMsimGuiJob {
	GObject parent_instance;
	GPid gpid;
	int pid;
	GMutex job_mutex;
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
};

struct _XmiMsimGuiJobClass {
	GObjectClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiJob, xmi_msim_gui_job, G_TYPE_OBJECT)

gboolean xmi_msim_gui_job_kill(XmiMsimGuiJob *job, GError **error) {
	job->do_not_emit = TRUE;
	return xmi_msim_gui_job_stop(job, error);
}

gboolean xmi_msim_gui_job_stop(XmiMsimGuiJob *job, GError **error) {
	// difference UNIX <-> Windows
	// UNIX -> send sigkill signal
	// Windows -> TerminateProcess
	
	// first check if still running
	if (xmi_msim_gui_job_is_running(job) == FALSE) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE, "job is not running");
		return FALSE;	
	}

	g_mutex_lock(&job->job_mutex);
	if (job->killed) {
		g_mutex_unlock(&job->job_mutex);
		return TRUE;
	}
	job->killed = TRUE;
	g_debug("Killing job with pid %d", job->pid);
#ifdef G_OS_UNIX
	if (kill(job->gpid, SIGTERM) == -1) {
		g_set_error(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_PROCESS, "Process %d could not be terminated with the SIGTERM signal: %s", job->pid, strerror(errno));
		g_mutex_unlock(&job->job_mutex);
		return FALSE;
	}
	// if paused, resume with SIGCONT, otherwise nothing will happen on Linux... Seems to work fine without on macOS though...
	if (job->paused) {
		if (kill(job->gpid, SIGCONT) == -1) {
			g_set_error(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_PROCESS, "Process %d could not be resumed with the SIGCONT signal, after sending SIGTERM: %s", job->pid, strerror(errno));
			g_mutex_unlock(&job->job_mutex);
			return FALSE;
		}
	}
#elif defined(G_OS_WIN32)
	if (TerminateProcess((HANDLE) job->gpid, (UINT) 1) == 0) {
		if (error != NULL) {
			gchar *error_msg = g_win32_error_message(GetLastError());
			g_set_error(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_PROCESS, "Process %d could not be terminated with the TerminateProcess call: %s", job->pid, error_msg);
			g_free(error_msg);
		}
		g_mutex_unlock(&job->job_mutex);
		return FALSE;	
	}
#else
	// unsupported platform??
	g_set_error(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_PROCESS, "Process %d could not be terminated on this unknown platform", job->pid);
	g_mutex_unlock(&job->job_mutex);
	return FALSE;
#endif
	// a proper kill will result in the child_watcher being called, leading to a FINISHED_EVENT getting emitted, unless do_not_emit is set...
	g_mutex_unlock(&job->job_mutex);
	return TRUE;
}

gboolean xmi_msim_gui_job_suspend(XmiMsimGuiJob *job, GError **error) {
	// check if we can suspend
	if (xmi_msim_gui_job_is_suspend_available() == FALSE) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE, "suspend is not supported on this platform");
		return FALSE;	
	}

	// check if still running
	if (xmi_msim_gui_job_is_running(job) == FALSE) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE, "job is not running");
		return FALSE;	
	}
	// then confirm it isn't paused already...
	if (xmi_msim_gui_job_is_suspended(job) == TRUE) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE, "job is already suspended");
		return FALSE;	
	}

	g_mutex_lock(&job->job_mutex);
	g_debug("Suspending job with pid %d", job->pid);
#ifdef G_OS_UNIX
	if (kill((pid_t) job->gpid, SIGSTOP) != 0) {
		g_set_error(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_PROCESS, "Process %d could not be suspended with the SIGSTOP signal: %s", job->pid, strerror(errno));
		g_mutex_unlock(&job->job_mutex);
		return FALSE;	
	
	}
#elif defined(G_OS_WIN32)
	if (NtSuspendProcess((HANDLE) job->gpid) != 0) {
		if (error != NULL) {
			gchar *error_msg = g_win32_error_message(GetLastError());
			g_set_error(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_PROCESS, "Process %d could not be suspended with the NtSuspendProcess call: %s", job->pid, error_msg);
			g_free(error_msg);
		}
		g_mutex_unlock(&job->job_mutex);
		return FALSE;	
	}
#endif
	job->paused = TRUE;
	g_mutex_unlock(&job->job_mutex);

	return TRUE;
}

gboolean xmi_msim_gui_job_resume(XmiMsimGuiJob *job, GError **error) {
	// check if we can resume 
	if (xmi_msim_gui_job_is_suspend_available() == FALSE) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE, "resume is not supported on this platform");
		return FALSE;	
	}

	// check if still running
	if (xmi_msim_gui_job_is_running(job) == FALSE) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE, "job is not running");
		return FALSE;	
	}
	// then confirm it isn't paused already...
	if (xmi_msim_gui_job_is_suspended(job) == FALSE) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE, "job is not suspended");
		return FALSE;	
	}

	g_mutex_lock(&job->job_mutex);
	g_debug("Resuming job with pid %d", job->pid);
#ifdef G_OS_UNIX
	if (kill((pid_t) job->gpid, SIGCONT) != 0) {
		g_set_error(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_PROCESS, "Process %d could not be resumed with the SIGcont signal: %s", job->pid, strerror(errno));
		g_mutex_unlock(&job->job_mutex);
		return FALSE;	
	
	}
#elif defined(G_OS_WIN32)
	if (NtResumeProcess((HANDLE) job->gpid) != 0) {
		if (error != NULL) {
			gchar *error_msg = g_win32_error_message(GetLastError());
			g_set_error(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_PROCESS, "Process %d could not be resumed with the NtResumeProcess call: %s", job->pid, error_msg);
			g_free(error_msg);
		}
		g_mutex_unlock(&job->job_mutex);
		return FALSE;	
	
	}
#endif
	job->paused = FALSE;
	g_mutex_unlock(&job->job_mutex);

	return TRUE;
}

static void xmi_msim_gui_job_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_job_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_job_finalize(GObject *gobject) {
	g_debug("Entering xmi_msim_gui_job_finalize");
	XmiMsimGuiJob *job = XMI_MSIM_GUI_JOB(gobject);
	
	if (xmi_msim_gui_job_is_running(job) == TRUE) {
		g_source_remove(job->child_watch_id);
	}

	xmi_msim_gui_job_kill(job, NULL);

	g_spawn_close_pid(job->gpid);

	g_mutex_clear(&job->job_mutex);

	g_ptr_array_free(job->argv, TRUE);

	g_free(job->wd);

	G_OBJECT_CLASS(xmi_msim_gui_job_parent_class)->finalize(gobject);
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

static void xmi_msim_gui_job_class_init(XmiMsimGuiJobClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_job_dispose;
	object_class->finalize = xmi_msim_gui_job_finalize;
	
	signals[STDOUT_EVENT] = g_signal_new(
		"stdout-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__STRING,
		G_TYPE_NONE,
		1,
		G_TYPE_STRING // gchar*
	);

	signals[STDERR_EVENT] = g_signal_new(
		"stderr-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__STRING,
		G_TYPE_NONE,
		1,
		G_TYPE_STRING // gchar*
	);

	signals[FINISHED_EVENT] = g_signal_new(
		"finished-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__BOOLEAN_STRING,
		G_TYPE_NONE,
		2,
		G_TYPE_BOOLEAN, // gboolean 
		G_TYPE_STRING // gchar*
	);

	signals[SPECIAL_EVENT] = g_signal_new(
		"special-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__INT_STRING,
		G_TYPE_NONE,
		2,
		G_TYPE_INT, // XmiMsimGuiJobSpecialEvent
		G_TYPE_STRING // gchar*
	);

	signals[PROGRESS_EVENT] = g_signal_new(
		"progress-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__INT_DOUBLE,
		G_TYPE_NONE,
		2,
		G_TYPE_INT, // XmiMsimGuiJobSpecialEvent
		G_TYPE_DOUBLE // guint (between 0 and 100)
	);

	init_globals();
}

static void xmi_msim_gui_job_init(XmiMsimGuiJob *self) {
	g_mutex_init(&self->job_mutex);
}

XmiMsimGuiJob* xmi_msim_gui_job_new(
	const gchar *xmi_msim_executable,
	const gchar *xmsifile,
	struct xmi_main_options *options,
	const gchar *spe_conv,
	const gchar *csv_conv,
	const gchar *svg_conv,
	const gchar *html_conv,
	int nthreads,
	const gchar **extra_options,
	GError **error
	) {

	if (!xmsifile) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_INVALID_INPUT, "xmsifile cannot be NULL");
		return NULL;
	}
	if (!xmi_msim_executable) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_INVALID_INPUT, "xmi_msim_executable cannot be NULL");
		return NULL;
	}
	if (!options) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_INVALID_INPUT, "xmi_msim_executable cannot be NULL");
		return NULL;
	}
	if (nthreads <= 0) {
		nthreads = xmi_omp_get_max_threads();
	}

	XmiMsimGuiJob *job = XMI_MSIM_GUI_JOB(g_object_new(XMI_MSIM_GUI_TYPE_JOB, NULL));

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
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_HDF5, "Could not determine solid angles HDF5 file");
		g_object_unref(job);
		return NULL;
	}
	g_ptr_array_add(job->argv, g_strdup_printf("--with-solid-angles-data=%s", xmimsim_hdf5_solid_angles));
	g_free(xmimsim_hdf5_solid_angles);

	if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_HDF5, "Could not determine escape ratios HDF5 file");
		g_object_unref(job);
		return NULL;
	}
	g_ptr_array_add(job->argv, g_strdup_printf("--with-escape-ratios-data=%s", xmimsim_hdf5_escape_ratios));
	g_free(xmimsim_hdf5_escape_ratios);
#endif

	g_ptr_array_add(job->argv, g_strdup_printf("--set-threads=%d", nthreads));

	// extra options
	if (extra_options) {
		int i;
		for (i = 0 ; extra_options[i] != NULL ; i++) {
			g_ptr_array_add(job->argv, g_strdup(extra_options[i]));
		}
	}

	g_ptr_array_add(job->argv, g_strdup(xmsifile));
	g_ptr_array_add(job->argv, NULL);

	job->wd = g_path_get_dirname(xmsifile);

	return job;
}

static void xmimsim_child_watcher_cb(GPid gpid, gint status, XmiMsimGuiJob *job) {
	// called when the child dies, either by natural causes or if it is killed
	// either way, this function must finish with a signal being emitted
	job->killed = TRUE; // to ensure that the IO watcher gets deactivated
	char *buffer;
	gboolean success;
	int pid = job->pid;
	gchar *xmi_msim_executable = g_ptr_array_index(job->argv, 0);

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
	g_mutex_lock(&job->job_mutex);
	job->running = FALSE;
	job->paused = FALSE;
	job->finished = TRUE;
	job->successful = success;
	if (!job->do_not_emit) /* necessary to avoid blunt kills from emitting useless signals  */
		g_signal_emit(job, signals[FINISHED_EVENT], 0, success, buffer);
	g_free(buffer);
	g_mutex_unlock(&job->job_mutex);
	G_LOCK(current_job);
	current_job = NULL;
	G_UNLOCK(current_job);
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

static gboolean process_stdout_channel_string(XmiMsimGuiJob *job, gchar *string) {
	int percentage;
	char *buffer;

	//solid angles
	if(XMI_STRNCMP(string, SOLID_ANGLE_GRID_ALREADY_PRESENT) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE, "Solid angle grid loaded from file");
		return TRUE;
	}
	else if (XMI_STRNCMP(string, SOLID_ANGLE_GRID_REDUNDANT) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE, "Solid angle grid redundant");
		return TRUE;
	}
	else if(XMI_STRNCMP(string, SOLID_ANGLE_GRID_PRECALCULATING) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE, 0.0);
		return TRUE;
	}
	else if(sscanf(string, "Solid angle calculation at %i", &percentage) == 1) {
		buffer = g_strdup_printf("Solid angle grid: %i %%",percentage);
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE, ((double) percentage)/100.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE, buffer);
		g_free(buffer);
		return FALSE;
	}
	else if(XMI_STRNCMP(string, SOLID_ANGLE_GRID_FINISHED) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE, "Solid angle grid calculated");
		return TRUE;
	}
	//interactions
	else if(sscanf(string, "Simulating interactions at %i", &percentage) == 1) {
		buffer = g_strdup_printf("Simulating interactions: %i %%",percentage);
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SIMULATION, ((double) percentage)/100.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SIMULATION, buffer);
		g_free(buffer);
		return FALSE;
	}
	else if(XMI_STRNCMP(string, SIMULATING_INTERACTIONS) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SIMULATION, 0.0);
		return TRUE;
	}
	else if(XMI_STRNCMP(string, SIMULATING_FINISHED) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SIMULATION, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SIMULATION, "Interactions simulated");
		return TRUE;
	}
	//escape ratios
	else if(XMI_STRNCMP(string, ESCAPE_RATIOS_ALREADY_PRESENT) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, "Escape peak ratios loaded from file");
		return TRUE;
	}
	else if(XMI_STRNCMP(string, ESCAPE_RATIOS_REDUNDANT) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, "Escape peaks redundant");
		return TRUE;
	}
	else if(XMI_STRNCMP(string, ESCAPE_RATIOS_PRECALCULATING) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, 0.0);
		return TRUE;
	}
	else if(sscanf(string, "Escape peak ratios calculation at %i", &percentage) == 1) {
		buffer = g_strdup_printf("Escape peak ratios: %i %%", percentage);
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, ((double) percentage)/100.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, buffer);
		g_free(buffer);
		return FALSE;
	}
	else if(XMI_STRNCMP(string, ESCAPE_RATIOS_FINISHED) == 0) {
		g_signal_emit(job, signals[PROGRESS_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, 1.0);
		g_signal_emit(job, signals[SPECIAL_EVENT], 0, XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, "Escape peak ratios calculated");
		return TRUE;
	}

	return TRUE;
}

static gboolean xmimsim_io_watcher(GIOChannel *source, GIOCondition condition, XmiMsimGuiJob *job) {
	// this trick appears to work well for avoiding segfaults due to text being processed when the job has been killed
	if (job->killed)
		return FALSE;

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

gboolean xmi_msim_gui_job_start(XmiMsimGuiJob *job, GError **error) {

	gboolean spawn_rv;

	// check if there is no other process running!
	// this could be the same job...
	G_LOCK(current_job);
	if (current_job != NULL) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_ANOTHER_JOB_RUNNING, "another job is already running");
		G_UNLOCK(current_job);
		return FALSE;
	}

	g_mutex_lock(&job->job_mutex);
	if (job->finished) {
		// do not allow old finished jobs to be restarted!
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE, "this job has finished already");
		g_mutex_unlock(&job->job_mutex);
		G_UNLOCK(current_job);
		return FALSE;
	}
	
	gint out_fh;
	gint err_fh;

	spawn_rv = g_spawn_async_with_pipes(job->wd, (gchar **) job->argv->pdata, NULL, G_SPAWN_DO_NOT_REAP_CHILD, NULL, NULL, &job->gpid, NULL, &out_fh, &err_fh, error);
	
	if (spawn_rv == FALSE) {
		G_UNLOCK(current_job);
		g_mutex_unlock(&job->job_mutex);
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

	job->child_watch_id = g_child_watch_add(job->gpid, (GChildWatchFunc) xmimsim_child_watcher_cb, job);

	const gchar *encoding = NULL;
	g_get_charset(&encoding);

	g_io_channel_set_encoding(job->stdout_channel, encoding, NULL);
	g_io_channel_set_close_on_unref(job->stdout_channel, TRUE);
	g_io_add_watch(job->stdout_channel, G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL, (GIOFunc) xmimsim_io_watcher, job);
	g_io_channel_unref(job->stdout_channel);

	g_io_channel_set_encoding(job->stderr_channel, encoding, NULL);
	g_io_channel_set_close_on_unref(job->stderr_channel,TRUE);
	g_io_add_watch(job->stderr_channel, G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL, (GIOFunc) xmimsim_io_watcher, job);
	g_io_channel_unref(job->stderr_channel);

	job->running = TRUE;
	current_job = job;
	g_mutex_unlock(&job->job_mutex);
	G_UNLOCK(current_job);

	return TRUE;
}

gboolean xmi_msim_gui_job_get_pid(XmiMsimGuiJob *job, gint *pid, GError **error) {
	g_mutex_lock(&job->job_mutex);
	if (job->pid == 0) {
		g_set_error_literal(error, XMI_MSIM_GUI_JOB_ERROR, XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE, "job has not been successfully started yet");
		g_mutex_unlock(&job->job_mutex);
		return FALSE;
	}
	*pid = (gint) job->pid;
	g_mutex_unlock(&job->job_mutex);
	return TRUE;
}

gboolean xmi_msim_gui_job_is_running(XmiMsimGuiJob *job) {
	gboolean rv;
	g_mutex_lock(&job->job_mutex);
	rv = job->running;
	g_mutex_unlock(&job->job_mutex);
	return rv;
}

gboolean xmi_msim_gui_job_is_suspended(XmiMsimGuiJob *job) {
	gboolean rv;
	g_mutex_lock(&job->job_mutex);
	rv = job->paused;
	g_mutex_unlock(&job->job_mutex);
	return rv;
}

gboolean xmi_msim_gui_job_has_finished(XmiMsimGuiJob *job) {
	gboolean rv;
	g_mutex_lock(&job->job_mutex);
	rv = job->finished;
	g_mutex_unlock(&job->job_mutex);
	return rv;
}

gboolean xmi_msim_gui_job_was_successful(XmiMsimGuiJob *job) {
	gboolean rv;
	g_mutex_lock(&job->job_mutex);
	rv = job->successful;
	g_mutex_unlock(&job->job_mutex);
	return rv;
}

void xmi_msim_gui_job_send_all_stdout_events(XmiMsimGuiJob *job, gboolean setting) {
	job->send_all_stdout_events = setting;
}

gchar* xmi_msim_gui_job_get_command(XmiMsimGuiJob *job) {
	if (job->argv)
		return g_strjoinv(" ", (gchar **) job->argv->pdata);
	return NULL;
}

GQuark xmi_msim_gui_job_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-gui-job-error-quark");
}

void xmi_msim_gui_job_kill_all(void ) {
	// no further processing will be triggered, just a clean kill
	G_LOCK(current_job);
	if (current_job != NULL) {
		xmi_msim_gui_job_kill(current_job, NULL);
	}
	current_job = NULL;
	G_UNLOCK(current_job);
}

gboolean xmi_msim_gui_job_is_suspend_available(void) {
	init_globals();
#ifdef G_OS_WIN32
	if (!NtSuspendProcess) {
		return FALSE;
	}
#endif
	return TRUE;
}
