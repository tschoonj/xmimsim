/*
Copyright (C) 2018-2019 Tom Schoonjans and Laszlo Vincze

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
#include "xmi_batch.h"
#include "xmi_aux.h"
#include "xmi_xml.h"
#include "xmi_gobject.h"
#include <gio/gio.h>
#include <glib/gstdio.h>

//#define XMI_OBJECT_REF(obj) \
//	xmi_object_ref(obj, G_STRLOC)

//#define XMI_OBJECT_UNREF(obj) \
//	xmi_object_unref(obj, G_STRLOC)

#define XMI_OBJECT_REF(obj) g_object_ref(obj)
#define XMI_OBJECT_UNREF(obj) g_object_unref(obj)

struct _XmiMsimBatchAbstractPrivate {
	XmiMsimJob *active_job;
	gboolean running;
	gboolean paused;
	gboolean finished;
	gboolean successful;
	gboolean do_not_emit;
	gboolean killed;
	gboolean send_all_stdout_events;
	gchar *executable;
	gchar **extra_options;
	gchar *last_finished_message;
	GMainLoop *main_loop;
	gboolean valid_object;
};

G_DEFINE_ABSTRACT_TYPE_WITH_PRIVATE(XmiMsimBatchAbstract, xmi_msim_batch_abstract, G_TYPE_OBJECT)

static XmiMsimJob* xmi_msim_batch_abstract_real_get_job(XmiMsimBatchAbstract *batch, guint job_index, GError **error);

static guint xmi_msim_batch_abstract_real_get_number_of_jobs(XmiMsimBatchAbstract *batch);

static void xmi_msim_batch_abstract_dispose(GObject *object);

static void xmi_msim_batch_abstract_finalize(GObject *object);

static void xmi_msim_batch_abstract_set_property(GObject          *object,
                                                 guint             prop_id,
                                                 const GValue     *value,
                                                 GParamSpec       *pspec);

static void xmi_msim_batch_abstract_get_property(GObject          *object,
                                                 guint             prop_id,
                                                 GValue     *value,
                                                 GParamSpec       *pspec);

enum {
	STDOUT_EVENT,
	STDERR_EVENT,
	FINISHED_EVENT,
	//SPECIAL_EVENT,
	PROGRESS_EVENT,
	ACTIVE_JOB_CHANGED,
	LAST_SIGNAL
};

enum {
	PROP_ABSTRACT_0,
	PROP_ABSTRACT_RUNNING,
	PROP_ABSTRACT_EXECUTABLE,
	PROP_ABSTRACT_EXTRA_OPTIONS,
	N_ABSTRACT_PROPERTIES
};

static guint signals[LAST_SIGNAL];
static GParamSpec *abstract_props[N_ABSTRACT_PROPERTIES] = {NULL, };

static void xmi_msim_batch_abstract_class_init(XmiMsimBatchAbstractClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_batch_abstract_dispose;
	object_class->finalize = xmi_msim_batch_abstract_finalize;
	object_class->set_property = xmi_msim_batch_abstract_set_property;
	object_class->get_property = xmi_msim_batch_abstract_get_property;

	klass->get_job = xmi_msim_batch_abstract_real_get_job;
	klass->get_number_of_jobs = xmi_msim_batch_abstract_real_get_number_of_jobs;
	
	/**
	 * XmiMsimBatchAbstract::stdout-event:
	 * @batch: The #XmiMsimBatch object emitting the signal
	 * @message: The %stdout line produced by the %xmimsim executable
	 *
	 * Emitted whenever a regular %stdout message was produced by the %xmimsim executable.
	 * Special messages can also be emitted, if #xmi_msim_batch_abstract_send_all_stdout_events() is set to %TRUE
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
	 * XmiMsimBatchAbstract::stderr-event:
	 * @batch: The #XmiMsimBatchAbstract object emitting the signal
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
	 * XmiMsimBatchAbstract::finished-event:
	 * @batch: The #XmiMsimBatchAbstract object emitting the signal
	 * @status: The exit status of the finished job. %TRUE if successful, %FALSE otherwise
	 * @message: An appropriate message marking that the job is now finished.
	 *
	 * Emitted when the #XmiMsimBatchAbstract has finished.
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

	/*
	 * XmiMsimJob::special-event:
	 * @job: The #XmiMsimJob object emitting the signal
	 * @event_type: The type of special event that occurred.
	 * @message: A message containing a description of the event.
	 *
	 * Emitted whenever a 'special' event has occurred.
	 */
	/*
	signals[SPECIAL_EVENT] = g_signal_new(
		"special-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_VOID__ENUM_STRING,
		G_TYPE_NONE,
		2,
		XMI_MSIM_TYPE_JOB_SPECIAL_EVENT, // XmiMsimJobSpecialEvent
		G_TYPE_STRING // gchar*
	);*/

	/**
	 * XmiMsimBatchAbstract::progress-event:
	 * @batch: The #XmiMsimBatchAbstract object emitting the signal
	 * @fraction: the progress fraction that has been reached when this event was emitted
	 *
	 * Emitted whenever a job has been completed.
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
		1,
		G_TYPE_DOUBLE // double (between 0 and 1)
	);

	/**
	 * XmiMsimBatchAbstract::active-job-changed:
	 * @batch: The #XmiMsimBatchAbstract object emitting the signal
	 * @job: the new active job or NULL
	 *
	 * Emitted whenever the active job has changed
	 */
	signals[ACTIVE_JOB_CHANGED] = g_signal_new(
		"active-job-changed",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_FIRST,
		0, // no default handler
		NULL,
		NULL,
		NULL,
		G_TYPE_NONE,
		1,
		XMI_MSIM_TYPE_JOB
	);

	abstract_props[PROP_ABSTRACT_RUNNING] = g_param_spec_boolean(
		"running",
		"Batch running",
		"Batch running",
		FALSE,
    		G_PARAM_READABLE
	);

	abstract_props[PROP_ABSTRACT_EXECUTABLE] = g_param_spec_string(
		"executable",
		"XMI-MSIM main executable",
		"Full path to the XMI-MSIM main executable",
		xmi_get_xmimsim_path(),
    		G_PARAM_READWRITE
	);

	abstract_props[PROP_ABSTRACT_EXTRA_OPTIONS] = g_param_spec_boxed(
		"extra-options",
		"XMI-MSIM extra options",
		"Extra options XMI-MSIM main executable",
		G_TYPE_STRV,
    		G_PARAM_READWRITE
	);

	g_object_class_install_properties(object_class, N_ABSTRACT_PROPERTIES, abstract_props);
}

static void xmi_msim_batch_abstract_init(XmiMsimBatchAbstract *self) {
	self->priv = xmi_msim_batch_abstract_get_instance_private(self);
	self->priv->send_all_stdout_events = TRUE;
}

static void xmi_msim_batch_abstract_dispose(GObject *object) {
	XmiMsimBatchAbstract *batch = XMI_MSIM_BATCH_ABSTRACT(object);

	G_OBJECT_CLASS(xmi_msim_batch_abstract_parent_class)->dispose(object);
}

static void xmi_msim_batch_abstract_finalize(GObject *object) {
	g_debug("Calling xmi_msim_batch_abstract_finalize");

	XmiMsimBatchAbstract *batch = XMI_MSIM_BATCH_ABSTRACT(object);

	if (batch->priv->active_job)
		g_clear_object(&batch->priv->active_job);

	g_free(batch->priv->executable);
	g_strfreev(batch->priv->extra_options);
	g_free(batch->priv->last_finished_message);
	if (batch->priv->main_loop)
		g_main_loop_unref(batch->priv->main_loop);

	G_OBJECT_CLASS(xmi_msim_batch_abstract_parent_class)->finalize(object);
}

static XmiMsimJob* xmi_msim_batch_abstract_real_get_job(XmiMsimBatchAbstract *batch, guint job_index, GError **error) {
	g_warning("XmiMsimBatchAbstract::get_job not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(batch)));
	g_set_error(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_METHOD_UNDEFINED, "XmiMsimBatchAbstract::get_job not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(batch)));

	return NULL;
}

static guint xmi_msim_batch_abstract_real_get_number_of_jobs(XmiMsimBatchAbstract *batch) {
	g_warning("XmiMsimBatchAbstract::get_number_of_jobs not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(batch)));

	return 0;
}

static void xmi_msim_batch_abstract_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

	XmiMsimBatchAbstract *batch = XMI_MSIM_BATCH_ABSTRACT(object);

	if (batch->priv->running || batch->priv->finished) {
		g_critical("cannot set properties after batch has started");
		return;
	}

	switch (prop_id) {
		case PROP_ABSTRACT_EXECUTABLE:
			g_free(batch->priv->executable);
 			batch->priv->executable = g_value_dup_string(value);
			break;
    		case PROP_ABSTRACT_EXTRA_OPTIONS:
			g_strfreev(batch->priv->extra_options);
 			batch->priv->extra_options = g_value_dup_boxed(value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
	}
}

static void xmi_msim_batch_abstract_get_property(GObject *object, guint prop_id, GValue *value,  GParamSpec *pspec) {

	XmiMsimBatchAbstract *batch = XMI_MSIM_BATCH_ABSTRACT(object);

	switch (prop_id) {
		case PROP_ABSTRACT_RUNNING:
			g_value_set_boolean(value, batch->priv->running);
			break;
		case PROP_ABSTRACT_EXECUTABLE:
			if (batch->priv->executable)
				g_value_set_string(value, batch->priv->executable);
			else
				g_value_copy(g_param_spec_get_default_value(pspec), value);
			break;
		case PROP_ABSTRACT_EXTRA_OPTIONS:
			g_value_set_boxed(value, batch->priv->extra_options);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
	}
}

static void batch_finished_cb(XmiMsimBatchAbstract *batch, GAsyncResult *result, gpointer data) {
	gchar *message = g_task_propagate_pointer(G_TASK(result), NULL);
	if (!batch->priv->do_not_emit) /* necessary to avoid blunt kills from emitting useless signals  */
		g_signal_emit(batch, signals[FINISHED_EVENT], 0, batch->priv->successful, message);
	g_free(message);
}

static void batch_process_stdout_event(XmiMsimBatchAbstract *batch, gchar *buffer, XmiMsimJob *job) {
	gchar *buffer_copy = g_strdup(buffer);
	g_signal_emit(batch, signals[STDOUT_EVENT], 0, buffer_copy);
	g_free(buffer_copy);
}

static void batch_process_stderr_event(XmiMsimBatchAbstract *batch, gchar *buffer, XmiMsimJob *job) {
	gchar *buffer_copy = g_strdup(buffer);
	g_signal_emit(batch, signals[STDERR_EVENT], 0, buffer_copy);
	g_free(buffer_copy);
}

static void batch_process_finished_event(XmiMsimJob *job, gboolean success, gchar *message, XmiMsimBatchAbstract *batch) {
	g_free(batch->priv->last_finished_message);
	batch->priv->last_finished_message = g_strdup(message);
	g_main_loop_quit(batch->priv->main_loop);
}

static void batch_thread(GTask *task, XmiMsimBatchAbstract *batch, gpointer task_data, GCancellable *cancellable) {
	const guint n_jobs = XMI_MSIM_BATCH_ABSTRACT_GET_CLASS(batch)->get_number_of_jobs(batch);

	if (n_jobs == 0) {
		batch->priv->finished = TRUE;
		g_task_return_pointer(task, g_strdup("Batch could not be started as number of jobs is 0"), g_free);
		return;
	}

	GMainContext *main_context = g_main_context_new();
	g_main_context_push_thread_default(main_context);

	guint job_i;

	batch->priv->running = TRUE;
	g_object_notify_by_pspec(G_OBJECT(batch), abstract_props[PROP_ABSTRACT_RUNNING]);

	for (job_i = 0 ; job_i < n_jobs ; job_i++) {
		GError *error = NULL;
		XmiMsimJob *job = XMI_MSIM_BATCH_ABSTRACT_GET_CLASS(batch)->get_job(batch, job_i, &error);
		if (error != NULL) {
			batch->priv->running = FALSE;
			g_object_notify_by_pspec(G_OBJECT(batch), abstract_props[PROP_ABSTRACT_RUNNING]);
			batch->priv->finished = TRUE;
			g_task_return_pointer(task, g_strdup(error->message), g_free);
			g_clear_error(&error);
			g_main_context_pop_thread_default(main_context);
			g_main_context_unref(main_context);
			return;
		}
		xmi_msim_job_send_all_stdout_events(job, batch->priv->send_all_stdout_events);

		
		if (batch->priv->main_loop)
			g_main_loop_unref(batch->priv->main_loop);
		batch->priv->main_loop = g_main_loop_new(main_context, FALSE);

		// hook up all signals
		gulong sig1 = g_signal_connect_swapped(job, "stdout-event", G_CALLBACK(batch_process_stdout_event), batch);
		gulong sig2 = g_signal_connect_swapped(job, "stderr-event", G_CALLBACK(batch_process_stderr_event), batch);
		gulong sig3 = g_signal_connect(job, "finished-event", G_CALLBACK(batch_process_finished_event), batch);
		
		// start job
		xmi_msim_job_start(job, &error);

		if (error != NULL) {
			g_signal_handler_disconnect(job, sig1);
			g_signal_handler_disconnect(job, sig2);
			g_signal_handler_disconnect(job, sig3);
			batch->priv->running = FALSE;
			g_object_notify_by_pspec(G_OBJECT(batch), abstract_props[PROP_ABSTRACT_RUNNING]);
			batch->priv->finished = TRUE;
			XMI_OBJECT_UNREF(job);
			g_task_return_pointer(task, g_strdup(error->message), g_free);
			g_clear_error(&error);
			g_main_context_pop_thread_default(main_context);
			g_main_context_unref(main_context);
			return;
		}

		// make this the active job -> mutex??
		batch->priv->active_job = XMI_OBJECT_REF(job);
		g_signal_emit(batch, signals[ACTIVE_JOB_CHANGED], 0, batch->priv->active_job);
		XMI_OBJECT_UNREF(job);

		// start blocking
		g_main_loop_run(batch->priv->main_loop);

		// handler_disconnect signal handlers
		g_signal_handler_disconnect(job, sig1);
		g_signal_handler_disconnect(job, sig2);
		g_signal_handler_disconnect(job, sig3);

		// check job status!
		if (!xmi_msim_job_was_successful(job)) {
			batch->priv->running = FALSE;
			g_object_notify_by_pspec(G_OBJECT(batch), abstract_props[PROP_ABSTRACT_RUNNING]);
			batch->priv->paused = FALSE;
			batch->priv->finished = TRUE;
			g_clear_object(&batch->priv->active_job);
			g_signal_emit(batch, signals[ACTIVE_JOB_CHANGED], 0, NULL);

			if (batch->priv->last_finished_message)
				g_task_return_pointer(task, g_strdup_printf("Job finished with error %s", batch->priv->last_finished_message), g_free);
			else
				g_task_return_pointer(task, g_strdup("Job finished with an unknown error"), g_free);
			g_main_context_pop_thread_default(main_context);
			g_main_context_unref(main_context);
			return;
		}
		double progress = (double) (job_i + 1)/ n_jobs;
		g_signal_emit(batch, signals[PROGRESS_EVENT], 0, progress);
		g_clear_object(&batch->priv->active_job);
	}

	// we are done!
	g_signal_emit(batch, signals[ACTIVE_JOB_CHANGED], 0, NULL);
	batch->priv->running = FALSE;
	g_object_notify_by_pspec(G_OBJECT(batch), abstract_props[PROP_ABSTRACT_RUNNING]);
	batch->priv->finished = TRUE;
	batch->priv->successful = TRUE;
	g_task_return_pointer(task, g_strdup("Batch has been terminated successfully"), g_free);
	g_main_context_pop_thread_default(main_context);
	g_main_context_unref(main_context);
}

gboolean xmi_msim_batch_abstract_start(XmiMsimBatchAbstract *batch, GError **error) {
	if (!XMI_MSIM_IS_BATCH_ABSTRACT(batch)) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch must be an instance of XmiMsimBatchAbstract");
		return FALSE;
	}

	if (!batch->priv->valid_object) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch was not instantiated with valid input");
		return FALSE;
	}


	if (batch->priv->finished) {
		// do not allow old finished batch jobs to be restarted!
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_UNAVAILABLE, "this batch has finished already");
		return FALSE;
	}

	GTask *task = g_task_new(batch, NULL, (GAsyncReadyCallback) batch_finished_cb, NULL);
	g_task_run_in_thread(task, (GTaskThreadFunc) batch_thread);
	XMI_OBJECT_UNREF(task);

	return TRUE;
}

gboolean xmi_msim_batch_abstract_stop(XmiMsimBatchAbstract *batch, GError **error) {
	if (!XMI_MSIM_IS_BATCH_ABSTRACT(batch)) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch must be an instance of XmiMsimBatchAbstract");
		return FALSE;
	}

	if (!batch->priv->valid_object) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch was not instantiated with valid input");
		return FALSE;
	}

	if (!batch->priv->running) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_UNAVAILABLE, "batch is not running");
		return FALSE;	
	
	}
	if (batch->priv->killed) {
		return TRUE;
	}
	XmiMsimJob *active_job = XMI_OBJECT_REF(batch->priv->active_job);
	batch->priv->killed = TRUE;
	gboolean rv = xmi_msim_job_stop(active_job, error);
	XMI_OBJECT_UNREF(active_job);
	return rv;
}

gboolean xmi_msim_batch_abstract_is_valid_object(XmiMsimBatchAbstract *batch) {
	g_return_val_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch), FALSE);

	return batch->priv->valid_object;
}

gboolean xmi_msim_batch_abstract_kill(XmiMsimBatchAbstract *batch, GError **error) {
	if (!XMI_MSIM_IS_BATCH_ABSTRACT(batch)) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch must be an instance of XmiMsimBatchAbstract");
		return FALSE;
	}

	if (!batch->priv->valid_object) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch was not instantiated with valid input");
		return FALSE;
	}

	batch->priv->do_not_emit = TRUE;
	return xmi_msim_batch_abstract_stop(batch, error);
}

gboolean xmi_msim_batch_abstract_suspend(XmiMsimBatchAbstract *batch, GError **error) {
	if (!XMI_MSIM_IS_BATCH_ABSTRACT(batch)) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch must be an instance of XmiMsimBatchAbstract");
		return FALSE;
	}

	if (!batch->priv->valid_object) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch was not instantiated with valid input");
		return FALSE;
	}

	if (!batch->priv->running) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_UNAVAILABLE, "batch is not running");
		return FALSE;	
	
	}
	if (batch->priv->paused) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_UNAVAILABLE, "batch is already suspended");
		return FALSE;	
	
	}
	XmiMsimJob *active_job = XMI_OBJECT_REF(batch->priv->active_job);
	batch->priv->paused = xmi_msim_job_suspend(active_job, error);
	XMI_OBJECT_UNREF(active_job);
	return batch->priv->paused;
}

gboolean xmi_msim_batch_abstract_resume(XmiMsimBatchAbstract *batch, GError **error) {
	if (!XMI_MSIM_IS_BATCH_ABSTRACT(batch)) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch must be an instance of XmiMsimBatchAbstract");
		return FALSE;
	}

	if (!batch->priv->valid_object) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch was not instantiated with valid input");
		return FALSE;
	}

	if (!batch->priv->running) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_UNAVAILABLE, "batch is not running");
		return FALSE;	
	
	}
	if (!batch->priv->paused) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_UNAVAILABLE, "batch is not suspended");
		return FALSE;	
	
	}
	XmiMsimJob *active_job = XMI_OBJECT_REF(batch->priv->active_job);
	batch->priv->paused = !xmi_msim_job_resume(active_job, error);
	XMI_OBJECT_UNREF(active_job);
	return !batch->priv->paused;
}

gboolean xmi_msim_batch_abstract_is_running(XmiMsimBatchAbstract *batch) {
	g_return_val_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch), FALSE);
	g_return_val_if_fail(batch->priv->valid_object, FALSE);

	gboolean rv = batch->priv->running;
	return rv;
}
	
gboolean xmi_msim_batch_abstract_is_suspended(XmiMsimBatchAbstract *batch) {
	g_return_val_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch), FALSE);
	g_return_val_if_fail(batch->priv->valid_object, FALSE);

	gboolean rv = batch->priv->paused;
	return rv;
}

gboolean xmi_msim_batch_abstract_has_finished(XmiMsimBatchAbstract *batch) {
	g_return_val_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch), FALSE);
	g_return_val_if_fail(batch->priv->valid_object, FALSE);

	gboolean rv = batch->priv->finished;
	return rv;
}

gboolean xmi_msim_batch_abstract_was_successful(XmiMsimBatchAbstract *batch) {
	g_return_val_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch), FALSE);
	g_return_val_if_fail(batch->priv->valid_object, FALSE);

	gboolean rv = batch->priv->successful;
	return rv;
}

void xmi_msim_batch_abstract_send_all_stdout_events(XmiMsimBatchAbstract *batch, gboolean setting) {
	g_return_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch));
	g_return_if_fail(batch->priv->valid_object);
	g_return_if_fail(!batch->priv->running && !batch->priv->finished);

	batch->priv->send_all_stdout_events = setting;
}

void xmi_msim_batch_abstract_set_executable(XmiMsimBatchAbstract *batch, const gchar *executable) {
	g_return_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch));
	g_return_if_fail(batch->priv->valid_object);

	g_object_set(batch, "executable", executable, NULL);
}

/**
 * xmi_msim_batch_abstract_set_extra_options:
 * @batch: an instance of XmiMsim.BatchAbstract
 * @extra_options: (nullable) (array zero-terminated=1) (element-type utf8): %NULL terminated array of additional options to pass to the executable
 */
void xmi_msim_batch_abstract_set_extra_options(XmiMsimBatchAbstract *batch, gchar **extra_options) {
	g_return_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch));
	g_return_if_fail(batch->priv->valid_object);

	g_object_set(batch, "extra-options", extra_options, NULL);
}

XmiMsimJob* xmi_msim_batch_abstract_get_active_job(XmiMsimBatchAbstract *batch) {
	g_return_val_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch), NULL);

	if (batch->priv->active_job)
		return g_object_ref(batch->priv->active_job);

	return NULL;	
}

guint xmi_msim_batch_abstract_get_number_of_jobs(XmiMsimBatchAbstract *batch) {
	g_return_val_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch), 0);

	return XMI_MSIM_BATCH_ABSTRACT_GET_CLASS(batch)->get_number_of_jobs(batch);
}

GQuark xmi_msim_batch_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-batch-error-quark");
}

struct _XmiMsimBatchMulti {
	XmiMsimBatchAbstract parent_instance;
	GPtrArray *xmsi_files;
	GPtrArray *options;
};

struct _XmiMsimBatchMultiClass {
	XmiMsimBatchAbstractClass parent_class;
};

G_DEFINE_TYPE(XmiMsimBatchMulti, xmi_msim_batch_multi, XMI_MSIM_TYPE_BATCH_ABSTRACT)

static XmiMsimJob* xmi_msim_batch_multi_real_get_job(XmiMsimBatchAbstract *batch, guint job_index, GError **error);

static guint xmi_msim_batch_multi_real_get_number_of_jobs(XmiMsimBatchAbstract *batch);

static void xmi_msim_batch_multi_dispose(GObject *object);

static void xmi_msim_batch_multi_finalize(GObject *object);

static void xmi_msim_batch_multi_constructed(GObject *object);

static void xmi_msim_batch_multi_set_property(GObject          *object,
                                              guint             prop_id,
                                              const GValue     *value,
                                              GParamSpec       *pspec);

static void xmi_msim_batch_multi_get_property(GObject          *object,
                                              guint             prop_id,
                                              GValue     *value,
                                              GParamSpec       *pspec);

enum {
	PROP_MULTI_0,
	PROP_MULTI_XMSI_FILES,
	PROP_MULTI_OPTIONS,
	N_MULTI_PROPERTIES
};

static GParamSpec *multi_props[N_MULTI_PROPERTIES] = {NULL, };

static void xmi_msim_batch_multi_class_init(XmiMsimBatchMultiClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);
	XmiMsimBatchAbstractClass *abstract_class = XMI_MSIM_BATCH_ABSTRACT_CLASS(klass);

	object_class->dispose = xmi_msim_batch_multi_dispose;
	object_class->finalize = xmi_msim_batch_multi_finalize;
	object_class->constructed = xmi_msim_batch_multi_constructed;
	object_class->set_property = xmi_msim_batch_multi_set_property;
	object_class->get_property = xmi_msim_batch_multi_get_property;

	abstract_class->get_job = xmi_msim_batch_multi_real_get_job;
	abstract_class->get_number_of_jobs = xmi_msim_batch_multi_real_get_number_of_jobs;
	

	multi_props[PROP_MULTI_XMSI_FILES] = g_param_spec_boxed(
		"xmsi-files",
		"XMSI input-files",
		"XMSI input-files",
		G_TYPE_PTR_ARRAY,
    		G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY
	);

	multi_props[PROP_MULTI_OPTIONS] = g_param_spec_boxed(
		"options",
		"Job Main Options",
		"Job Main Options",
		G_TYPE_PTR_ARRAY,
    		G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY
	);

	g_object_class_install_properties(object_class, N_MULTI_PROPERTIES, multi_props);
}

static void xmi_msim_batch_multi_init(XmiMsimBatchMulti *self) {
}

static void xmi_msim_batch_multi_dispose(GObject *object) {
	XmiMsimBatchMulti *batch = XMI_MSIM_BATCH_MULTI(object);

	G_OBJECT_CLASS(xmi_msim_batch_multi_parent_class)->dispose(object);
}

static void xmi_msim_batch_multi_finalize(GObject *object) {
	g_debug("Calling xmi_msim_batch_multi_finalize");

	XmiMsimBatchMulti *batch = XMI_MSIM_BATCH_MULTI(object);

	if (batch->xmsi_files)
		g_ptr_array_unref(batch->xmsi_files);
	if (batch->options)
		g_ptr_array_unref(batch->options);

	G_OBJECT_CLASS(xmi_msim_batch_multi_parent_class)->finalize(object);
}

static void xmi_msim_batch_multi_constructed(GObject *object) {
	XmiMsimBatchMulti *batch = XMI_MSIM_BATCH_MULTI(object);
	XmiMsimBatchAbstract *abstract = XMI_MSIM_BATCH_ABSTRACT(object);

	g_return_if_fail(batch->xmsi_files != NULL && batch->xmsi_files->len >= 1);
	g_return_if_fail(batch->options != NULL && (batch->options->len == 1 || batch->options->len == batch->xmsi_files->len));

	abstract->priv->valid_object = TRUE;

	G_OBJECT_CLASS(xmi_msim_batch_abstract_parent_class)->constructed(object);
}

static XmiMsimJob* xmi_msim_batch_multi_real_get_job(XmiMsimBatchAbstract *batch, guint job_index, GError **error) {
	if (!XMI_MSIM_IS_BATCH_MULTI(batch)) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch must be an instance of XmiMsimBatchMulti");
		return NULL;
	}

	XmiMsimBatchMulti *multi = XMI_MSIM_BATCH_MULTI(batch);
	if (job_index >= multi->xmsi_files->len) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "job_index out of range");
		return NULL;
	}

	gchar *xmsi_file = g_ptr_array_index(multi->xmsi_files, job_index);
	xmi_main_options *options = multi->options->len == 1 ? g_ptr_array_index(multi->options, 0) : g_ptr_array_index(multi->options, job_index);

	gchar *executable = batch->priv->executable != NULL ? g_strdup(batch->priv->executable) : g_value_dup_string(g_param_spec_get_default_value(abstract_props[PROP_ABSTRACT_EXECUTABLE]));
	gchar **extra_options = batch->priv->extra_options != NULL ? g_strdupv(batch->priv->extra_options) : NULL;

	XmiMsimJob *job = xmi_msim_job_new(executable, xmsi_file, options, NULL, NULL, NULL, NULL, extra_options);
	g_free(executable);
	g_strfreev(extra_options);
	
	return job;
}

static guint xmi_msim_batch_multi_real_get_number_of_jobs(XmiMsimBatchAbstract *batch) {
	g_return_val_if_fail(XMI_MSIM_IS_BATCH_MULTI(batch), 0);

	return XMI_MSIM_BATCH_MULTI(batch)->xmsi_files->len;
}

static void xmi_msim_batch_multi_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

	XmiMsimBatchMulti *batch = XMI_MSIM_BATCH_MULTI(object);

	switch (prop_id) {
		case PROP_MULTI_XMSI_FILES:
			if (batch->xmsi_files)
				g_ptr_array_unref(batch->xmsi_files);
			batch->xmsi_files = g_value_dup_boxed(value);
      			break;
		case PROP_MULTI_OPTIONS:
			if (batch->options)
				g_ptr_array_unref(batch->options);
			batch->options = g_value_dup_boxed(value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
  	}
}

static void xmi_msim_batch_multi_get_property(GObject *object, guint prop_id, GValue *value,  GParamSpec *pspec) {

  XmiMsimBatchMulti *batch = XMI_MSIM_BATCH_MULTI(object);

  switch (prop_id) {
    case PROP_MULTI_XMSI_FILES:
      g_value_set_boxed(value, batch->xmsi_files);
      break;
    case PROP_MULTI_OPTIONS:
      g_value_set_boxed(value, batch->options);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
  }
}

/**
 * xmi_msim_batch_multi_new: (constructor)
 * @xmsi_files: (array) (element-type filename): NULL terminated array of filenames
 * @options: (array) (element-type XmiMsim.MainOptions): options to pass to the simulations. Must contain either a single MainOptions struct, or as many as xmsi_files is long.
 *
 * Returns: (transfer full): a freshly allocated XmiMsim.BatchAbstract instance.
 */
XmiMsimBatchAbstract* xmi_msim_batch_multi_new(GPtrArray* xmsi_files, GPtrArray* options) {
	g_return_val_if_fail(xmsi_files != NULL && xmsi_files->len >= 1, NULL);
	g_return_val_if_fail(options != NULL && (options->len == 1 || options->len == xmsi_files->len), NULL);
	return g_object_new(XMI_MSIM_TYPE_BATCH_MULTI, "xmsi-files", xmsi_files, "options", options, NULL);
}

struct _XmiMsimBatchSingle {
	XmiMsimBatchAbstract parent_instance;
	GPtrArray *xmsi_data;
	GPtrArray *single_data;
	xmi_main_options *options;
	xmi_archive *archive;
	guint njobs;
	gchar *tmpdir;
};

struct _XmiMsimBatchSingleClass {
	XmiMsimBatchAbstractClass parent_class;
};

G_DEFINE_TYPE(XmiMsimBatchSingle, xmi_msim_batch_single, XMI_MSIM_TYPE_BATCH_ABSTRACT)

static XmiMsimJob* xmi_msim_batch_single_real_get_job(XmiMsimBatchAbstract *batch, guint job_index, GError **error);

static guint xmi_msim_batch_single_real_get_number_of_jobs(XmiMsimBatchAbstract *batch);

static void xmi_msim_batch_single_dispose(GObject *object);

static void xmi_msim_batch_single_finalize(GObject *object);

static void xmi_msim_batch_single_constructed(GObject *object);

static void xmi_msim_batch_single_set_property(GObject          *object,
                                               guint             prop_id,
                                               const GValue     *value,
                                               GParamSpec       *pspec);

static void xmi_msim_batch_single_get_property(GObject          *object,
                                               guint             prop_id,
                                               GValue     *value,
                                               GParamSpec       *pspec);

enum {
	PROP_SINGLE_0,
	PROP_SINGLE_XMSI_DATA,
	PROP_SINGLE_SINGLE_DATA,
	PROP_SINGLE_OPTIONS,
	PROP_SINGLE_ARCHIVE,
	N_SINGLE_PROPERTIES
};

static GParamSpec *single_props[N_SINGLE_PROPERTIES] = {NULL, };

static void single_active_job_finished_event_handler_cb(XmiMsimBatchSingle *batch, gboolean results, gchar *message, XmiMsimJob *active_job) {

	if (!results)
		return;

	// read in the XMSO file and add it to the archive struct
	gchar *xmso_file = xmi_msim_job_get_output_file(active_job);
	gchar *xmsi_file = xmi_msim_job_get_input_file(active_job);
	GError *error = NULL;
	xmi_output *output = xmi_output_read_from_xml_file(xmso_file, &error);
	if (output == NULL) {
		g_critical("Could not read XMSO file %s -> %s", xmso_file, error->message);
		return;
	}
	// add to archive...
	g_ptr_array_add(batch->archive->output, output);

	g_debug("Adding %s to archive", xmso_file);

	// cleanup
	unlink(xmso_file);
	g_free(xmso_file);
	unlink(xmsi_file);
	g_free(xmsi_file);
}

static void single_active_job_changed_handler_cb(XmiMsimBatchSingle *batch, XmiMsimJob *active_job, gpointer data) {
	g_debug("Entering single_active_job_changed_handler_cb");

	// this is called when is the job is created... not when it finished!
	if (!active_job)
		return;

	g_signal_connect_swapped(active_job, "finished-event", G_CALLBACK(single_active_job_finished_event_handler_cb), batch);
}

static void xmi_msim_batch_single_class_init(XmiMsimBatchSingleClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);
	XmiMsimBatchAbstractClass *abstract_class = XMI_MSIM_BATCH_ABSTRACT_CLASS(klass);

	object_class->dispose = xmi_msim_batch_single_dispose;
	object_class->finalize = xmi_msim_batch_single_finalize;
	object_class->constructed = xmi_msim_batch_single_constructed;
	object_class->set_property = xmi_msim_batch_single_set_property;
	object_class->get_property = xmi_msim_batch_single_get_property;

	abstract_class->get_job = xmi_msim_batch_single_real_get_job;
	abstract_class->get_number_of_jobs = xmi_msim_batch_single_real_get_number_of_jobs;
	

	single_props[PROP_SINGLE_XMSI_DATA] = g_param_spec_boxed(
		"xmsi-data",
		"XMSI input array",
		"XMSI input array",
		G_TYPE_PTR_ARRAY,
    		G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY
	);

	single_props[PROP_SINGLE_SINGLE_DATA] = g_param_spec_boxed(
		"single-data",
		"XmiMsimBatchSingleData array",
		"Array of xmi_batch_single_data instances",
		G_TYPE_PTR_ARRAY,
    		G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY
	);

	single_props[PROP_SINGLE_OPTIONS] = g_param_spec_boxed(
		"options",
		"Job Main Options",
		"Job Main Options",
		XMI_MSIM_TYPE_MAIN_OPTIONS,
    		G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY
	);

	single_props[PROP_SINGLE_ARCHIVE] = g_param_spec_boxed(
		"archive",
		"archive",
		"archive",
		XMI_MSIM_TYPE_ARCHIVE,
    		G_PARAM_READABLE
	);

	g_object_class_install_properties(object_class, N_SINGLE_PROPERTIES, single_props);

	// ensure that whenever a job is finished, we read in the XMSO file, add it to the archive, and afterwards delete both XMSI and XMSO files.
	g_signal_override_class_closure(
		signals[ACTIVE_JOB_CHANGED],
		XMI_MSIM_TYPE_BATCH_SINGLE,
		g_cclosure_new(G_CALLBACK(single_active_job_changed_handler_cb), NULL, NULL));
}

/**
 * xmi_msim_batch_single_new: (constructor)
 * @xmsi_data: (array) (element-type XmiMsim.Input): an an array containing XmiMsim.Input instances. The length of this array has to match with the information in single_data.
 * @single_data: (array) (element-type XmiMsim.BatchSingleData): an array containing xmi_msim_batch_single_data instances, with the information that was used to generate xmsi_data.
 * @options: the options that will be passed to the XmiMsim.Job instances.
 *
 * Returns: (transfer full): a freshly allocated XmiMsim.BatchAbstract instance.
 */
XmiMsimBatchAbstract* xmi_msim_batch_single_new(GPtrArray *xmsi_data, GPtrArray *single_data, xmi_main_options *options) {
	g_return_val_if_fail(xmsi_data != NULL && xmsi_data->len > 0, NULL);
	g_return_val_if_fail(single_data != NULL && single_data->len > 0, NULL);
	g_return_val_if_fail(options != NULL, NULL);
	return g_object_new(
		XMI_MSIM_TYPE_BATCH_SINGLE,
		"xmsi-data", xmsi_data,
		"single-data", single_data,
		"options", options,
		NULL);
}

static void xmi_msim_batch_single_constructed(GObject *object) {
	XmiMsimBatchSingle *batch = XMI_MSIM_BATCH_SINGLE(object);
	XmiMsimBatchAbstract *abstract = XMI_MSIM_BATCH_ABSTRACT(object);

	g_return_if_fail(batch->xmsi_data != NULL && batch->xmsi_data->len > 0);
	g_return_if_fail(batch->single_data != NULL && batch->single_data->len > 0);
	g_return_if_fail(batch->options != NULL);

	GArray *dims = g_array_sized_new(FALSE, FALSE, sizeof(int), batch->single_data->len);
	unsigned int i;
	int dims_prod = 1;
	for (i = 0 ; i < batch->single_data->len ; i++) {
		xmi_batch_single_data *data = g_ptr_array_index(batch->single_data, i);
		int dim = data->nsteps + 1;
		g_array_append_val(dims, dim);
		dims_prod *= dim;
	}

	g_return_if_fail(dims_prod == batch->xmsi_data->len);

	batch->archive = g_malloc0(sizeof(xmi_archive));
	batch->archive->single_data = g_ptr_array_ref(batch->single_data);
	batch->archive->dims = dims;
	batch->archive->ref_count = 1;
	batch->archive->output = g_ptr_array_new_full(batch->xmsi_data->len, (GDestroyNotify) xmi_output_free);
	batch->njobs = dims_prod;
	batch->tmpdir = g_dir_make_tmp(NULL, NULL);


	abstract->priv->valid_object = TRUE;

	G_OBJECT_CLASS(xmi_msim_batch_abstract_parent_class)->constructed(object);
}

static void xmi_msim_batch_single_init(XmiMsimBatchSingle *self) {
}

static void xmi_msim_batch_single_dispose(GObject *object) {
	XmiMsimBatchSingle *batch = XMI_MSIM_BATCH_SINGLE(object);

	G_OBJECT_CLASS(xmi_msim_batch_single_parent_class)->dispose(object);
}

static void xmi_msim_batch_single_finalize(GObject *object) {
	g_debug("Calling xmi_msim_batch_single_finalize");

	XmiMsimBatchSingle *batch = XMI_MSIM_BATCH_SINGLE(object);

	if (batch->xmsi_data)
		g_ptr_array_unref(batch->xmsi_data);

	if (batch->options)
		xmi_main_options_free(batch->options);

	if (batch->single_data)
		g_ptr_array_unref(batch->single_data);

	if (batch->archive)
		xmi_archive_unref(batch->archive);

	GDir *dir = g_dir_open(batch->tmpdir, 0, NULL);
	const gchar *file = NULL;

	while ((file = g_dir_read_name(dir)) != NULL) {
		gchar *full_file = g_build_filename(batch->tmpdir, file, NULL);
		g_remove(full_file);
		g_free(full_file);
	}
	g_dir_close(dir);

	g_rmdir(batch->tmpdir);
	g_free(batch->tmpdir);

	G_OBJECT_CLASS(xmi_msim_batch_single_parent_class)->finalize(object);
}

static void xmi_msim_batch_single_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

	XmiMsimBatchSingle *batch = XMI_MSIM_BATCH_SINGLE(object);

	switch (prop_id) {
		case PROP_SINGLE_XMSI_DATA:
			if (batch->xmsi_data)
				g_ptr_array_unref(batch->xmsi_data);
			batch->xmsi_data = g_value_dup_boxed(value);
      			break;
		case PROP_SINGLE_SINGLE_DATA:
			if (batch->single_data)
				g_ptr_array_unref(batch->single_data);
			batch->single_data = g_value_dup_boxed(value);
			break;
		case PROP_SINGLE_OPTIONS:
			if (batch->options)
				xmi_main_options_free(batch->options);
			batch->options = g_value_dup_boxed(value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
  	}
}

static void xmi_msim_batch_single_get_property(GObject *object, guint prop_id, GValue *value,  GParamSpec *pspec) {

	XmiMsimBatchSingle *batch = XMI_MSIM_BATCH_SINGLE(object);

	switch (prop_id) {
		case PROP_SINGLE_XMSI_DATA:
			g_value_set_boxed(value, batch->xmsi_data);
			break;
		case PROP_SINGLE_SINGLE_DATA:
			g_value_set_boxed(value, batch->single_data);
			break;
		case PROP_SINGLE_OPTIONS:
			g_value_set_boxed(value, batch->options);
			break;
		case PROP_SINGLE_ARCHIVE:
			g_value_set_boxed(value, batch->archive);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
	}
}

static XmiMsimJob* xmi_msim_batch_single_real_get_job(XmiMsimBatchAbstract *batch, guint job_index, GError **error) {
	if (!XMI_MSIM_IS_BATCH_SINGLE(batch)) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch must be an instance of XmiMsimBatchSingle");
		return NULL;
	}
	XmiMsimBatchSingle *self = XMI_MSIM_BATCH_SINGLE(batch);
	if (job_index >= self->xmsi_data->len) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "job_index out of range");
		return NULL;
	}

	xmi_input *input = g_ptr_array_index(self->xmsi_data, job_index);
	// construct input output filenames, and write the input-file to disk!
	gchar *filename_xmsi = g_strdup_printf("tmp_%u.xmsi", job_index);
	gchar *filename_xmsi_full = g_build_filename(self->tmpdir, filename_xmsi, NULL);
	g_free(filename_xmsi);

	gchar *filename_xmso = g_strdup_printf("tmp_%u.xmso", job_index);
	gchar *filename_xmso_full = g_build_filename(self->tmpdir, filename_xmso, NULL);
	g_free(filename_xmso);

	if (input->general->outputfile)
		g_free(input->general->outputfile);
	input->general->outputfile = filename_xmso_full;

	g_debug("Writing to %s", filename_xmsi_full);
	gboolean write_rv = xmi_input_write_to_xml_file(input, filename_xmsi_full, error);
	g_return_val_if_fail(write_rv == TRUE, NULL);


	gchar *executable = batch->priv->executable != NULL ? g_strdup(batch->priv->executable) : xmi_get_xmimsim_path();
	gchar **extra_options = batch->priv->extra_options != NULL ? g_strdupv(batch->priv->extra_options) : NULL;

	XmiMsimJob *job = xmi_msim_job_new(executable, filename_xmsi_full, self->options, NULL, NULL, NULL, NULL, extra_options);
	g_free(executable);
	g_strfreev(extra_options);
	g_free(filename_xmsi_full);
	
	return job;
}

static guint xmi_msim_batch_single_real_get_number_of_jobs(XmiMsimBatchAbstract *batch) {
	XmiMsimBatchSingle *self = XMI_MSIM_BATCH_SINGLE(batch);
	return self->njobs;
}

/**
 * xmi_msim_batch_single_write_archive:
 * @batch: an instance of XmiMsim.BatchSingle, must have finished successfully
 * @xmsa_file: the name of the file that will be written with the contents of the archive
 *
 * Returns: %TRUE if the file was successfully written, %FALSE otherwise
 */
gboolean xmi_msim_batch_single_write_archive(XmiMsimBatchSingle *batch, const char *xmsa_file, GError **error) {
	if (!XMI_MSIM_IS_BATCH_SINGLE(batch)) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch must be an instance of XmiMsimBatchSingle");
		return FALSE;
	}

	XmiMsimBatchAbstract *abstract = XMI_MSIM_BATCH_ABSTRACT(batch);

	if (!abstract->priv->valid_object) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch was not instantiated with valid input");
		return FALSE;
	}

	if (!abstract->priv->successful) {
		g_set_error_literal(error, XMI_MSIM_BATCH_ERROR, XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "batch did not finish successfully");
		return FALSE;
	}

	return xmi_archive_write_to_xml_file(batch->archive, xmsa_file, error);
}

