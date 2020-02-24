/*
Copyright (C) 2019 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-batch-controls-box.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-utils.h"
#include "xmi_aux.h"

#ifdef HAVE_GOOGLE_ANALYTICS
  #include "xmi_google_analytics.h"
#endif

#include <math.h>

struct _XmiMsimGuiBatchControlsBox {
	GtkBox parent_instance;
	XmiMsimBatchAbstract *batch_data;
	GtkWidget *playButton;
	GtkWidget *stopButton;
	GtkWidget *pauseButton;
	GtkWidget *nthreadsW;
	GtkWidget *progressbarW;
	GtkWidget *controlsLogW;
	GtkTextBuffer *controlsLogB;
	GtkWidget *saveButton;
	GtkWidget *controlsLogFileW;
	GtkWidget *verboseW;
	GTimer *timer;
	GFileOutputStream *logFile;
	XmiMsimJob *job;
	GMainLoop *main_loop;
};

struct _XmiMsimGuiBatchControlsBoxClass {
	GtkBoxClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiBatchControlsBox, xmi_msim_gui_batch_controls_box, GTK_TYPE_BOX)

static void xmi_msim_gui_batch_controls_box_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_batch_controls_box_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_batch_controls_box_finalize(GObject *gobject) {
	XmiMsimGuiBatchControlsBox *self = XMI_MSIM_GUI_BATCH_CONTROLS_BOX(gobject);

	if (self->batch_data)
		g_object_unref(self->batch_data);

	if (self->main_loop)
		g_main_loop_unref(self->main_loop);

	G_OBJECT_CLASS(xmi_msim_gui_batch_controls_box_parent_class)->finalize(gobject);
}

enum {
	PROP_0,
	PROP_BATCH_DATA,
	N_PROPERTIES
};

static GParamSpec *props[N_PROPERTIES] = {NULL, };

static void xmi_msim_gui_batch_controls_box_set_property(GObject          *object,
                                                guint             prop_id,
                                                const GValue     *value,
                                                GParamSpec       *pspec);

static void xmi_msim_gui_batch_controls_box_get_property(GObject          *object,
                                                guint             prop_id,
                                                GValue     *value,
                                                GParamSpec       *pspec);

static void xmi_msim_gui_batch_controls_box_class_init(XmiMsimGuiBatchControlsBoxClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_batch_controls_box_dispose;
	object_class->finalize = xmi_msim_gui_batch_controls_box_finalize;
	object_class->set_property = xmi_msim_gui_batch_controls_box_set_property;
	object_class->get_property = xmi_msim_gui_batch_controls_box_get_property;

	props[PROP_BATCH_DATA] = g_param_spec_object(
		"batch-data",
		"batch-data",
		"batch-data",
		XMI_MSIM_TYPE_BATCH_ABSTRACT,
    		G_PARAM_READWRITE
	);

	g_object_class_install_properties(object_class, N_PROPERTIES, props);
}

struct update_gui_data {
	XmiMsimBatchAbstract *batch;
	gchar *string;
	XmiMsimGuiBatchControlsBox *self;
	gboolean result;
	gdouble fraction;
	XmiMsimJob *active_job;
};

static void update_gui_data_free(gpointer _data) {
	struct update_gui_data *data = _data;
	if (data->batch)
		g_object_unref(data->batch);
	if (data->self)
		g_object_unref(data->self);
	g_free(data->string);
	if (data->active_job)
		g_object_unref(data->active_job);
	g_free(data);
}

static void choose_logfile(GtkButton *saveButton, XmiMsimGuiBatchControlsBox *self) {
	XmiMsimGuiFileChooserDialog *dialog;
	gchar *filename = NULL;

	dialog = xmi_msim_gui_file_chooser_dialog_new(
		"Select a filename for the logfile",
		GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(self))),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		"_Save",
		"_Cancel");
	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog), TRUE);
	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		gtk_entry_set_text(GTK_ENTRY(self->controlsLogFileW), filename);
		g_free (filename);
	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);
}

static gboolean finished_event_idle_cb(gpointer _data) {
	struct update_gui_data *data = _data;

	gchar *buffer = g_strdup_printf("%s\n", data->string);

	if (data->result) {
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(
			data->self->controlsLogW,
			data->self->timer,
			data->self->controlsLogB,
			buffer,
			-1,
			gtk_text_tag_table_lookup(
				gtk_text_buffer_get_tag_table(data->self->controlsLogB),
				"success"),
			NULL);
	}
	else {
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(
			data->self->controlsLogW,
			data->self->timer,
			data->self->controlsLogB,
			buffer,
			-1,
			gtk_text_tag_table_lookup(
				gtk_text_buffer_get_tag_table(data->self->controlsLogB),
				"error"),
			NULL);
	}

	if (data->self->logFile) {
		g_output_stream_write(G_OUTPUT_STREAM(data->self->logFile), buffer, strlen(buffer), NULL, NULL);
	}

	g_free(buffer);

	g_main_loop_quit(data->self->main_loop);

	return G_SOURCE_REMOVE;
}

static void finished_event_cb(XmiMsimBatchAbstract *batch, gboolean result, const gchar *buffer, XmiMsimGuiBatchControlsBox *self) {
	struct update_gui_data *data = g_malloc0(sizeof(struct update_gui_data));
	data->batch = g_object_ref(batch);
	data->string = g_strdup(buffer);
	data->self = g_object_ref(self);
	data->result = result;

	gdk_threads_add_idle_full(G_PRIORITY_DEFAULT_IDLE, finished_event_idle_cb, data, update_gui_data_free);
}

static gboolean progress_event_idle_cb(gpointer _data) {
	struct update_gui_data *data = _data;

	int file_index = round(data->fraction * xmi_msim_batch_abstract_get_number_of_jobs(data->batch));
	gchar *pbartext = g_strdup_printf("File %d/%d completed", file_index, xmi_msim_batch_abstract_get_number_of_jobs(data->batch));
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(data->self->progressbarW), pbartext);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(data->self->progressbarW), data->fraction);
	g_free(pbartext);

	return G_SOURCE_REMOVE;
}

static void progress_event_cb(XmiMsimBatchAbstract *batch, gdouble fraction, XmiMsimGuiBatchControlsBox *self) {
	struct update_gui_data *data = g_malloc0(sizeof(struct update_gui_data));
	data->batch = g_object_ref(batch);
	data->self = g_object_ref(self);
	data->fraction = fraction;

	gdk_threads_add_idle_full(G_PRIORITY_DEFAULT_IDLE, progress_event_idle_cb, data, update_gui_data_free);
}

static gboolean active_job_changed_idle_cb(gpointer _data) {
	struct update_gui_data *data = _data;

	int pid;
	xmi_msim_job_get_pid(data->active_job, &pid, NULL); // let's assume this won't fail...
	gchar *xmimsim_executable = NULL;
	g_object_get(data->batch, "executable", &xmimsim_executable, NULL);
	gchar *buffer = g_strdup_printf("%s was started with process id %d\n", xmimsim_executable, pid);
	g_free(xmimsim_executable);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(
		data->self->controlsLogW,
		data->self->timer,
		data->self->controlsLogB,
		buffer,
		-1,
		NULL);

	if (data->self->logFile) {
		g_output_stream_write(G_OUTPUT_STREAM(data->self->logFile), buffer, strlen(buffer), NULL, NULL);
	}

	g_free(buffer);

	return G_SOURCE_REMOVE;
}

static void active_job_changed_cb(XmiMsimBatchAbstract *batch, XmiMsimJob *active_job, XmiMsimGuiBatchControlsBox *self) {

	if (active_job == NULL)
		return;

	struct update_gui_data *data = g_malloc0(sizeof(struct update_gui_data));
	data->batch = g_object_ref(batch);
	data->self = g_object_ref(self);
	data->active_job = g_object_ref(active_job);

	gdk_threads_add_idle_full(G_PRIORITY_DEFAULT_IDLE, active_job_changed_idle_cb, data, update_gui_data_free);
}

static gboolean stdout_event_idle_cb(gpointer _data) {
	struct update_gui_data *data = _data;

	gchar *buffer = g_strdup_printf("%s\n", data->string);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(data->self->controlsLogW, data->self->timer, data->self->controlsLogB, buffer, -1, NULL);
	if (data->self->logFile) {
		g_output_stream_write(G_OUTPUT_STREAM(data->self->logFile), buffer, strlen(buffer), NULL, NULL);
	}
	g_free(buffer);

	return G_SOURCE_REMOVE;
}

static void stdout_event_cb(XmiMsimBatchAbstract *batch, const gchar *string, XmiMsimGuiBatchControlsBox *self) {
	struct update_gui_data *data = g_malloc0(sizeof(struct update_gui_data));
	data->batch = g_object_ref(batch);
	data->string = g_strdup(string);
	data->self = g_object_ref(self);

	gdk_threads_add_idle_full(G_PRIORITY_DEFAULT_IDLE, stdout_event_idle_cb, data, update_gui_data_free);

}

static gboolean stderr_event_idle_cb(gpointer _data) {
	struct update_gui_data *data = _data;

	gchar *buffer = g_strdup_printf("%s\n", data->string);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(
		data->self->controlsLogW,
		data->self->timer,
		data->self->controlsLogB,
		buffer,
		-1,
		gtk_text_tag_table_lookup(
			gtk_text_buffer_get_tag_table(data->self->controlsLogB),
			"error"),
		NULL);

	if (data->self->logFile) {
		g_output_stream_write(G_OUTPUT_STREAM(data->self->logFile), buffer, strlen(buffer), NULL, NULL);
	}
	g_free(buffer);

	return G_SOURCE_REMOVE;
}

static void stderr_event_cb(XmiMsimBatchAbstract *batch, const gchar *string, XmiMsimGuiBatchControlsBox *self) {
	struct update_gui_data *data = g_malloc0(sizeof(struct update_gui_data));
	data->batch = g_object_ref(batch);
	data->string = g_strdup(string);
	data->self = g_object_ref(self);

	gdk_threads_add_idle_full(G_PRIORITY_DEFAULT_IDLE, stderr_event_idle_cb, data, update_gui_data_free);
}

static void play_button_clicked(GtkButton *button, XmiMsimGuiBatchControlsBox *self) {


	//first deal with the pause case
	if (self->pauseButton && self->batch_data && xmi_msim_batch_abstract_is_suspended(self->batch_data)) {
		gint pid;
		gtk_widget_set_sensitive(self->playButton, FALSE);
		gboolean resume_rv;
		char *buffer;
		GError *error = NULL;
		g_timer_continue(self->timer);

		XmiMsimJob *active_job = xmi_msim_batch_abstract_get_active_job(self->batch_data);

		xmi_msim_job_get_pid(active_job, &pid, NULL); // let's assume this won't fail...

		resume_rv = xmi_msim_batch_abstract_resume(self->batch_data, &error);
		g_object_unref(active_job);

		if (resume_rv) {
			buffer = g_strdup_printf( "Process %d was successfully resumed\n", pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "pause-continue-stopped"), NULL);
		}
		else {
			buffer = g_strdup_printf( "Process %d could not be resumed\n", pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "error" ),NULL);
		}
		if (self->logFile) {
			g_output_stream_write(G_OUTPUT_STREAM(self->logFile), buffer, strlen(buffer), NULL, NULL);
		}
		gtk_widget_set_sensitive(self->pauseButton, TRUE);
		g_free(buffer);
		return;
	}

	//start_job
	gtk_widget_set_sensitive(self->playButton, FALSE);
	gtk_widget_set_sensitive(self->saveButton, FALSE);
	gtk_widget_set_sensitive(self->controlsLogFileW, FALSE);
	gtk_widget_set_sensitive(self->verboseW, FALSE);
	if (self->nthreadsW != NULL)
		gtk_widget_set_sensitive(self->nthreadsW, FALSE);

	GtkTextIter start, end;

	gchar *pbartext = g_strdup_printf("File 0/%d completed", xmi_msim_batch_abstract_get_number_of_jobs(self->batch_data));
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(self->progressbarW), pbartext);
	g_free(pbartext);

	self->timer = g_timer_new();

	//open logFile if necessary
	gchar *logFileName = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(self->controlsLogFileW))));
	if (strlen(logFileName) > 0) {
		GFile *g_file = g_file_new_for_path(logFileName);
		GError *error = NULL;
		if ((self->logFile = g_file_replace(g_file, NULL, FALSE, G_FILE_CREATE_REPLACE_DESTINATION, NULL, &error)) == NULL) {
			g_object_unref(g_file);
			//could not write to logfile
			GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(self))),
				GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Error writing to log file %s: %s",
				logFileName,
				error->message
                	);
			g_error_free(error);
     			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
	}


	// connect all signals
	g_signal_connect(self->batch_data, "stdout-event", G_CALLBACK(stdout_event_cb), self);
	g_signal_connect(self->batch_data, "stderr-event", G_CALLBACK(stderr_event_cb), self);
	g_signal_connect(self->batch_data, "finished-event", G_CALLBACK(finished_event_cb), self);
	g_signal_connect(self->batch_data, "progress-event", G_CALLBACK(progress_event_cb), self);
	g_signal_connect(self->batch_data, "active-job-changed", G_CALLBACK(active_job_changed_cb), self);



	// before launching set verbosity and number of threads
	GPtrArray *extra_options = g_ptr_array_new_with_free_func(g_free);
	if (self->nthreadsW)
		g_ptr_array_add(extra_options, g_strdup_printf("--set-threads=%d", (int) gtk_range_get_value(GTK_RANGE(self->nthreadsW))));
	else
		g_ptr_array_add(extra_options, g_strdup("--set-threads=1"));

	if (gtk_combo_box_get_active(GTK_COMBO_BOX(self->verboseW)) == 1) {
		g_ptr_array_add(extra_options, g_strdup("--very-verbose"));
	}

	g_ptr_array_add(extra_options, NULL);
	
	xmi_msim_batch_abstract_set_extra_options(self->batch_data, (gchar **) extra_options->pdata);
	xmi_msim_batch_abstract_send_all_stdout_events(self->batch_data, TRUE);
	g_ptr_array_free(extra_options, TRUE);


#ifdef HAVE_GOOGLE_ANALYTICS
	XmiMsimGoogleAnalyticsTracker *tracker = xmi_msim_google_analytics_tracker_get_global();

	if (XMI_MSIM_IS_BATCH_MULTI(self->batch_data)) {
		GPtrArray *options = NULL;

		g_object_get(self->batch_data, "options", &options, NULL);

		if (options && options->len > 1) {
			xmi_msim_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "BATCH-SIMULATION-START", "MULTIPLE-FILES-MULTIPLE-OPTIONS", NULL);
		}
		else if (options && options->len == 1) {
			xmi_msim_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "BATCH-SIMULATION-START", "MULTIPLE-FILES-SINGLE-OPTION", NULL);
		}
		else {
			g_warning("Could not get multi batch options");
		}
		if (options) {
			g_ptr_array_unref(options);
		}
	}
	else if (XMI_MSIM_IS_BATCH_SINGLE(self->batch_data)) {
		GPtrArray *single_data = NULL;
		g_object_get(self->batch_data, "single-data", &single_data, NULL);

		if (single_data) {
			GString *payload = g_string_new("SINGLE-FILE");

			guint i;

			for (i = 0 ; i < single_data->len ; i++) {
				xmi_batch_single_data *data = g_ptr_array_index(single_data, i);
				g_string_append_printf(payload, "-%s", data->xpath);
			}

			g_ptr_array_unref(single_data);

			xmi_msim_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "BATCH-SIMULATION-START", payload->str, NULL);
			g_string_free(payload, TRUE);
		}
		else {
			g_warning("Could not get single batch single data");
		}
	}
#endif

	GError *error = NULL;
	if (!xmi_msim_batch_abstract_start(self->batch_data, &error)) {
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, error->message, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "error"), NULL);
		if (self->logFile) {
			gchar *buffer = g_strdup_printf( "%s\n", error->message);
			g_output_stream_write(G_OUTPUT_STREAM(self->logFile), buffer, strlen(buffer), NULL, NULL);
			g_free(buffer);
		}
		g_error_free(error);
		if (self->logFile)
			g_object_unref(self->logFile);
		// may need to emit a finished signal here to ensure the summary page can be reached later on
		return;
	}

	if (self->pauseButton)
		gtk_widget_set_sensitive(self->pauseButton, TRUE);

	gtk_widget_set_sensitive(self->stopButton, TRUE);

	self->main_loop = g_main_loop_new(NULL, FALSE);
	g_main_loop_run(self->main_loop);
	
	gtk_widget_set_sensitive(self->stopButton, FALSE);
	if (self->pauseButton)
		gtk_widget_set_sensitive(self->pauseButton, FALSE);
	
	if (self->logFile)
		g_object_unref(self->logFile);
	self->logFile = NULL;

	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(self->progressbarW), 1.0);
	
	if (xmi_msim_batch_abstract_was_successful(self->batch_data)) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(self->progressbarW), "Simulations completed");
	}
	else {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(self->progressbarW), "Simulations failed");
	}
	
	while(gtk_events_pending())
		gtk_main_iteration();
}

static void stop_button_clicked(GtkButton *button, XmiMsimGuiBatchControlsBox *self) {
	GError *error = NULL;

	gtk_widget_set_sensitive(self->pauseButton,FALSE);
	gtk_widget_set_sensitive(self->stopButton,FALSE);

	g_timer_stop(self->timer);

	if (xmi_msim_batch_abstract_stop(self->batch_data, &error)) {
		XmiMsimJob *active_job = xmi_msim_batch_abstract_get_active_job(self->batch_data);

		gint pid = 0;
		xmi_msim_job_get_pid(active_job, &pid, NULL);

		gchar *buffer = g_strdup_printf( "Process %d was successfully terminated before completion\n", pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "pause-continue-stopped"), NULL);
		if (self->logFile)
			g_output_stream_write(G_OUTPUT_STREAM(self->logFile), buffer, strlen(buffer), NULL, NULL);
		g_free(buffer);
		g_object_unref(active_job);
	}
	else {
		XmiMsimJob *active_job = xmi_msim_batch_abstract_get_active_job(self->batch_data);

		gint pid = 0;
		xmi_msim_job_get_pid(active_job, &pid, NULL);

		gchar *buffer = g_strdup_printf( "Process %d could not be terminated. %s\n", pid, error->message);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "error"), NULL);
		if (self->logFile)
			g_output_stream_write(G_OUTPUT_STREAM(self->logFile), buffer, strlen(buffer), NULL, NULL);
		g_free(buffer);
		g_object_unref(active_job);
	}
}

static void pause_button_clicked(GtkButton *button, XmiMsimGuiBatchControlsBox *self) {
	GError *error = NULL;

	gtk_widget_set_sensitive(self->pauseButton,FALSE);
	gtk_widget_set_sensitive(self->stopButton,FALSE);

	g_timer_stop(self->timer);

	if (xmi_msim_batch_abstract_suspend(self->batch_data, &error)) {
		XmiMsimJob *active_job = xmi_msim_batch_abstract_get_active_job(self->batch_data);

		gint pid = 0;
		xmi_msim_job_get_pid(active_job, &pid, NULL);

		gchar *buffer = g_strdup_printf( "Process %d was successfully suspended. Press the Play button to continue or Stop to kill the process\n", pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "pause-continue-stopped"), NULL);
		if (self->logFile)
			g_output_stream_write(G_OUTPUT_STREAM(self->logFile), buffer, strlen(buffer), NULL, NULL);
		gtk_widget_set_sensitive(self->stopButton, TRUE);
		gtk_widget_set_sensitive(self->playButton, TRUE);
		g_free(buffer);
		g_object_unref(active_job);
	}
	else {
		XmiMsimJob *active_job = xmi_msim_batch_abstract_get_active_job(self->batch_data);

		gint pid = 0;
		xmi_msim_job_get_pid(active_job, &pid, NULL);

		gchar *buffer = g_strdup_printf( "Process %d could not be suspended. %s\n", pid, error->message);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "error"), NULL);
		if (self->logFile)
			g_output_stream_write(G_OUTPUT_STREAM(self->logFile), buffer, strlen(buffer), NULL, NULL);
		gtk_widget_set_sensitive(self->stopButton, TRUE);
		g_free(buffer);
		g_object_unref(active_job);
	}
}

static void xmi_msim_gui_batch_controls_box_init(XmiMsimGuiBatchControlsBox *self) {

	g_object_set(self, "orientation", GTK_ORIENTATION_VERTICAL, NULL);

	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);

	self->playButton = gtk_button_new_from_icon_name("media-playback-start", GTK_ICON_SIZE_DIALOG);
	gtk_box_pack_start(GTK_BOX(hbox), self->playButton, FALSE, FALSE, 2);

	if (xmi_msim_job_is_suspend_available()) {
		self->pauseButton = gtk_button_new_from_icon_name("media-playback-pause", GTK_ICON_SIZE_DIALOG);
		gtk_box_pack_start(GTK_BOX(hbox), self->pauseButton, FALSE, FALSE, 2);
		gtk_widget_set_sensitive(self->pauseButton, FALSE);
	}

	self->stopButton = gtk_button_new_from_icon_name("media-playback-stop", GTK_ICON_SIZE_DIALOG);
	gtk_box_pack_start(GTK_BOX(hbox), self->stopButton, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(self->stopButton, FALSE);

	GtkWidget *nthreadsW = NULL;
	if (xmi_omp_get_max_threads() > 1) {
		GtkWidget *cpuLabel = gtk_label_new("CPUs");
		gtk_box_pack_start(GTK_BOX(hbox), cpuLabel, FALSE, FALSE, 2);
		GtkAdjustment *nthreadsA = GTK_ADJUSTMENT(gtk_adjustment_new((gdouble) xmi_omp_get_max_threads(), 1.0, (gdouble) xmi_omp_get_max_threads(), 1.0, 1.0, 0.0));
		nthreadsW = gtk_scale_new(GTK_ORIENTATION_HORIZONTAL, nthreadsA);
		gtk_scale_set_digits(GTK_SCALE(nthreadsW), 0);
		gtk_scale_set_value_pos(GTK_SCALE(nthreadsW), GTK_POS_TOP);
		gtk_widget_set_size_request(nthreadsW, 30, -1);
		gtk_box_pack_start(GTK_BOX(hbox), nthreadsW, TRUE, TRUE, 2);
	}
	self->nthreadsW = nthreadsW;

	//add progressbar
	GtkWidget *progressbarW = gtk_progress_bar_new();
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbarW), "Start simulation");
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbarW), 0.0);
	gtk_orientable_set_orientation(GTK_ORIENTABLE(progressbarW), GTK_ORIENTATION_HORIZONTAL);
	gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(progressbarW), TRUE);
	GtkWidget *pvbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 1);
	gtk_box_set_homogeneous(GTK_BOX(pvbox), TRUE);
	gtk_box_pack_start(GTK_BOX(pvbox), progressbarW, FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(hbox), pvbox, TRUE, TRUE, 2);
	gtk_widget_set_size_request(progressbarW, -1, 30);
	self->progressbarW = progressbarW;

	gtk_widget_show_all(hbox);
	gtk_box_pack_start(GTK_BOX(self), hbox, FALSE, FALSE, 0);

	//output log
	GtkWidget *controlsLogW = gtk_text_view_new();
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(controlsLogW), GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(controlsLogW), 3);
	GtkTextBuffer *controlsLogB = gtk_text_view_get_buffer(GTK_TEXT_VIEW(controlsLogW));
	gtk_container_set_border_width(GTK_CONTAINER(controlsLogW), 2);
	gtk_text_view_set_editable(GTK_TEXT_VIEW(controlsLogW), FALSE);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(controlsLogW), FALSE);
	gtk_text_buffer_create_tag(controlsLogB, "error", "foreground", "red", NULL);
	gtk_text_buffer_create_tag(controlsLogB, "success", "foreground", "green", NULL);
	gtk_text_buffer_create_tag(controlsLogB, "pause-continue-stopped", "foreground", "orange", NULL);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), controlsLogW);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 2);
	gtk_widget_show_all(scrolled_window);
	gtk_box_pack_start(GTK_BOX(self), scrolled_window, TRUE, TRUE, 3);
	self->controlsLogW = controlsLogW;
	self->controlsLogB = controlsLogB;

	//bottom widget
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	gtk_container_set_border_width(GTK_CONTAINER(hbox), 2);
	GtkWidget *saveButton = gtk_button_new_with_mnemonic("_Save As");
	self->saveButton = saveButton;
	gtk_box_pack_start(GTK_BOX(hbox), saveButton, FALSE, FALSE, 2);
	GtkWidget *controlsLogFileW = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(controlsLogFileW), FALSE);
	gtk_box_pack_start(GTK_BOX(hbox), controlsLogFileW, TRUE, TRUE, 2);
	self->controlsLogFileW = controlsLogFileW;

	g_signal_connect(G_OBJECT(saveButton), "clicked", G_CALLBACK(choose_logfile), self);

	GtkWidget *verboseW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(verboseW), "Verbose");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(verboseW), "Very verbose");
	gtk_combo_box_set_active(GTK_COMBO_BOX(verboseW), 0);
	self->verboseW = verboseW;
	gtk_box_pack_start(GTK_BOX(hbox), verboseW, FALSE, FALSE, 2);

	gtk_widget_show_all(hbox);
	gtk_box_pack_start(GTK_BOX(self), hbox, FALSE, FALSE, 0);

	g_signal_connect(G_OBJECT(self->playButton), "clicked", G_CALLBACK(play_button_clicked), self);
	g_signal_connect(G_OBJECT(self->stopButton), "clicked", G_CALLBACK(stop_button_clicked), self);
	if (self->pauseButton)
		g_signal_connect(G_OBJECT(self->pauseButton), "clicked", G_CALLBACK(pause_button_clicked), self);
}

static void xmi_msim_gui_batch_controls_box_set_property(GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec) {
  XmiMsimGuiBatchControlsBox *self = XMI_MSIM_GUI_BATCH_CONTROLS_BOX(object);

  switch (prop_id) {
    case PROP_BATCH_DATA:
      if (self->batch_data)
        g_object_unref(self->batch_data);
      self->batch_data = g_value_dup_object(value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static void xmi_msim_gui_batch_controls_box_get_property(GObject *object, guint prop_id, GValue *value, GParamSpec *pspec) {
  XmiMsimGuiBatchControlsBox *self = XMI_MSIM_GUI_BATCH_CONTROLS_BOX(object);

  switch (prop_id) {
    case PROP_BATCH_DATA:
      g_value_set_object(value, self->batch_data);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

GtkWidget* xmi_msim_gui_batch_controls_box_new(void) {
	return g_object_new(XMI_MSIM_GUI_TYPE_BATCH_CONTROLS_BOX, NULL);
}

void xmi_msim_gui_batch_controls_box_set_batch_data(XmiMsimGuiBatchControlsBox *self, XmiMsimBatchAbstract *batch_data) {
	g_return_if_fail(XMI_MSIM_GUI_IS_BATCH_CONTROLS_BOX(self));
	g_return_if_fail(XMI_MSIM_IS_BATCH_ABSTRACT(batch_data) || batch_data == NULL);

	g_object_set(self, "batch-data", batch_data, NULL);
}
