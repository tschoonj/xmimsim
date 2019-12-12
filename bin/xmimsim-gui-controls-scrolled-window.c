/*
Copyright (C) 2018 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-controls-scrolled-window.h"
#include "xmi_job.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-private.h"
#include "xmimsim-gui-options-box.h"
#include "xmimsim-gui-application-window.h"
#include "xmi_aux.h"

#ifdef HAVE_GOOGLE_ANALYTICS
  #include "xmi_google_analytics.h"
#endif

enum {
	FINISHED_EVENT,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

struct _XmiMsimGuiControlsScrolledWindowClass {
	GtkScrolledWindowClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiControlsScrolledWindow, xmi_msim_gui_controls_scrolled_window, GTK_TYPE_SCROLLED_WINDOW)

static void xmi_msim_gui_controls_scrolled_window_finalize(GObject *gobject) {
	g_debug("Entering xmi_msim_gui_controls_scrolled_window_finalize");
	XmiMsimGuiControlsScrolledWindow *self = XMI_MSIM_GUI_CONTROLS_SCROLLED_WINDOW(gobject);

	if (self->timer)
		g_timer_destroy(self->timer);
	g_free(self->input_file);
	g_free(self->output_file);

	G_OBJECT_CLASS(xmi_msim_gui_controls_scrolled_window_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_controls_scrolled_window_dispose(GObject *gobject) {
	g_debug("Entering xmi_msim_gui_controls_scrolled_window_dispose");
	XmiMsimGuiControlsScrolledWindow *self = XMI_MSIM_GUI_CONTROLS_SCROLLED_WINDOW(gobject);

	g_clear_object(&self->job);
	g_clear_object(&self->manager);

	G_OBJECT_CLASS(xmi_msim_gui_controls_scrolled_window_parent_class)->dispose(gobject);
}

static gboolean executable_file_filter(const GtkFileFilterInfo *filter_info, gpointer data) {
	return g_file_test(filter_info->filename, G_FILE_TEST_IS_EXECUTABLE);
}

static void select_executable_cb(GtkWidget *button, XmiMsimGuiControlsScrolledWindow *self) {
	XmiMsimGuiFileChooserDialog *dialog;
	GtkFileFilter *filter;
	gchar *filename;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_custom(filter, GTK_FILE_FILTER_FILENAME, executable_file_filter, NULL, NULL);
	gtk_file_filter_set_name(filter,"Executables");
	dialog = xmi_msim_gui_file_chooser_dialog_new ("Open simulation executable",
		GTK_WINDOW(gtk_widget_get_toplevel(button)),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		"_Open",
		"_Cancel"
		);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gtk_entry_set_text(GTK_ENTRY(self->executableW),filename);
		g_free(filename);
	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);
}

static void select_extra_output_cb(GtkWidget *button, XmiMsimGuiControlsScrolledWindow *self) {
	XmiMsimGuiFileChooserDialog *dialog;
	GtkFileFilter *filter = NULL;
	gchar *filename = NULL;
	gchar *title = NULL;
	gchar *extension = NULL;
	GtkWidget *entry = NULL;

	if (button == self->spe_convB) {
		title = g_strdup("Select the prefix of the SPE files");
		entry = self->spe_convW;
	}
	else if (button == self->csv_convB) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.csv");
		gtk_file_filter_set_name(filter, "CSV files");
		title = g_strdup("Select the name of the CSV file");
		entry = self->csv_convW;
		extension = g_strdup(".csv");
	}
	else if (button == self->html_convB) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.html");
		gtk_file_filter_set_name(filter, "Hypertext Markup Language");
		title = g_strdup("Select the name of the HTML report file");
		entry = self->html_convW;
		extension = g_strdup(".html");
	}
	else {
		g_warning("select_extra_output_cb: unknown button detected!");
		return;
	}

	dialog = xmi_msim_gui_file_chooser_dialog_new(
		title,
		GTK_WINDOW(gtk_widget_get_toplevel(button)),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		"_Save",
		"_Cancel"
	);

	if (filter)
		gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		xmi_msim_gui_utils_ensure_extension(&filename, extension);

		gtk_entry_set_text(GTK_ENTRY(entry), filename);
		g_free(filename);

	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);
}

static void xmi_msim_gui_controls_scrolled_window_class_init(XmiMsimGuiControlsScrolledWindowClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_controls_scrolled_window_dispose;
	object_class->finalize = xmi_msim_gui_controls_scrolled_window_finalize;

	/**
	 * XmiMsimGuiControlsScrolledWindow::finished-event:
	 * @window: The #XmiMsimGuiControlsScrolledWindow object emitting the signal
	 * @status: The exit status of the finished job. %TRUE if successful, %FALSE otherwise
	 * @xmso_file: The file that should be opened if successful, or %NULL otherwise
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
		G_TYPE_FILE // GFile
	);
}

static void reset_controls(XmiMsimGuiControlsScrolledWindow *self) {

	//set progressbars to 0 %
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(self->progressbar_solidW), "Solid angle grid: 0 %");
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(self->progressbar_mainW), "Simulating interactions: 0 %");
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(self->progressbar_escapeW), "Escape peak ratios: 0 %");

	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(self->progressbar_solidW), 0.0);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(self->progressbar_mainW), 0.0);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(self->progressbar_escapeW), 0.0);

	//icons
	gtk_widget_show(self->image_solid_stopW);
	gtk_widget_hide(self->image_solid_spinnerW);
	gtk_widget_hide(self->image_solid_yesW);
	gtk_widget_hide(self->image_solid_noW);
	gtk_widget_show(self->image_main_stopW);
	gtk_widget_hide(self->image_main_spinnerW);
	gtk_widget_hide(self->image_main_yesW);
	gtk_widget_hide(self->image_main_noW);
	gtk_widget_show(self->image_escape_stopW);
	gtk_widget_hide(self->image_escape_spinnerW);
	gtk_widget_hide(self->image_escape_yesW);
	gtk_widget_hide(self->image_escape_noW);

	//clear textbuffer
	GtkTextIter start, end;
	gtk_text_buffer_get_bounds(self->controlsLogB, &start, &end);
	gtk_text_buffer_delete(self->controlsLogB, &start, &end);
}

static void error_spinners(XmiMsimGuiControlsScrolledWindow *self) {
	if (gtk_widget_get_visible(self->image_solid_spinnerW)) {
		gtk_spinner_stop(GTK_SPINNER(self->image_solid_spinnerW));
		gtk_widget_hide(self->image_solid_spinnerW);
		gtk_widget_show(self->image_solid_noW);
	}
	else if (gtk_widget_get_visible(self->image_main_spinnerW)) {
		gtk_spinner_stop(GTK_SPINNER(self->image_main_spinnerW));
		gtk_widget_hide(self->image_main_spinnerW);
		gtk_widget_show(self->image_main_noW);
	}
	else if (gtk_widget_get_visible(self->image_escape_spinnerW)) {
		gtk_spinner_stop(GTK_SPINNER(self->image_escape_spinnerW));
		gtk_widget_hide(self->image_escape_spinnerW);
		gtk_widget_show(self->image_escape_noW);
	}

}
static void job_stderr_cb(XmiMsimJob *job, const gchar *string, XmiMsimGuiControlsScrolledWindow *self) {
	gchar *buffer = g_strdup_printf("%s\n", string);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "error" ), NULL);
	g_free(buffer);
}

static void job_stdout_cb(XmiMsimJob *job, const gchar *string, XmiMsimGuiControlsScrolledWindow *self) {
	gchar *buffer = g_strdup_printf("%s\n", string);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, NULL);
	g_free(buffer);
}

static void job_finished_cb(XmiMsimJob *job, gboolean result, const gchar *string, XmiMsimGuiControlsScrolledWindow *self) {
	gchar *buffer = g_strdup_printf("%s\n", string);
	if (result) {
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "success"), NULL);
	}
	else {
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "error"), NULL);
	}
	gtk_widget_set_sensitive(self->stopButton, FALSE);
	if (self->pauseButton)
		gtk_widget_set_sensitive(self->pauseButton, FALSE);

	gtk_widget_set_sensitive(self->playButton, TRUE);
	//make sensitive again
	gtk_widget_set_sensitive(self->executableW, TRUE);
	gtk_widget_set_sensitive(self->executableB, TRUE);
	gtk_widget_set_sensitive(self->options_boxW, TRUE);
	gtk_widget_set_sensitive(self->spe_convW, TRUE);
	gtk_widget_set_sensitive(self->csv_convW, TRUE);
	gtk_widget_set_sensitive(self->html_convW,TRUE);
	gtk_widget_set_sensitive(self->spe_convB,TRUE);
	gtk_widget_set_sensitive(self->csv_convB,TRUE);
	gtk_widget_set_sensitive(self->html_convB,TRUE);
	if (self->nthreadsW != NULL)
		gtk_widget_set_sensitive(self->nthreadsW,TRUE);

#if !defined(G_OS_WIN32) || GLIB_CHECK_VERSION(2, 58, 0)
	// send notification
	GValue xpv = G_VALUE_INIT;
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NOTIFICATIONS, &xpv) == 0) {
		g_warning("Could not get notification preferences!");
		g_value_set_boolean(&xpv, FALSE);
	}

	if (g_value_get_boolean(&xpv)) {
		GNotification *notification;
		if (result == FALSE) {
			notification = g_notification_new("Simulation failed");
			if (notification)
				g_notification_set_body(notification, "Check error messages");
		}
		else {
			notification = g_notification_new("Simulation succeeded");
			if (notification) {
				gchar *my_basename = g_path_get_basename(self->output_file);
				gchar *information = g_strdup_printf("%s is now showing in the results window", my_basename);
				g_notification_set_body(notification, information);
				g_free(my_basename);
				g_free(information);
			}
		}
		if (notification) {
			g_notification_set_default_action (notification, "app.focus-window");
			g_application_send_notification(g_application_get_default(), NULL, notification);
			g_object_unref(notification);
		}
	}
	g_value_unset(&xpv);
#endif

	if (result == FALSE) {
		//if something is spinning, make it stop and make it red
		error_spinners(self);
		g_signal_emit(self, signals[FINISHED_EVENT], 0, result, NULL);
		return;
	}

	// read the spectrum in
	GFile *file = g_file_new_for_path(self->output_file);
	g_signal_emit(self, signals[FINISHED_EVENT], 0, result, file);
	g_object_unref(file);
}

static void job_special_cb(XmiMsimJob *job, XmiMsimJobSpecialEvent event, const gchar *message, XmiMsimGuiControlsScrolledWindow *self) {
	if (event == XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(self->progressbar_solidW), message);
	}
	else if (event == XMI_MSIM_JOB_SPECIAL_EVENT_SIMULATION) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(self->progressbar_mainW), message);
	}
	else if (event == XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(self->progressbar_escapeW), message);
	}
}

static void job_progress_cb(XmiMsimJob *job, XmiMsimJobSpecialEvent event, double progress, XmiMsimGuiControlsScrolledWindow *self) {
	if (event == XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE) {
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(self->progressbar_solidW), progress);
		if (progress == 0.0) {
			gtk_widget_hide(self->image_solid_stopW);
			gtk_widget_show(self->image_solid_spinnerW);
			gtk_spinner_start(GTK_SPINNER(self->image_solid_spinnerW));
		}
		else if (progress == 1.0) {
			gtk_spinner_stop(GTK_SPINNER(self->image_solid_spinnerW));
			gtk_widget_hide(self->image_solid_spinnerW);
			gtk_widget_hide(self->image_solid_stopW);
			gtk_widget_show(self->image_solid_yesW);
		}
	}
	else if (event == XMI_MSIM_JOB_SPECIAL_EVENT_SIMULATION) {
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(self->progressbar_mainW), progress);
		if (progress == 0.0) {
			gtk_widget_hide(self->image_main_stopW);
			gtk_widget_show(self->image_main_spinnerW);
			gtk_spinner_start(GTK_SPINNER(self->image_main_spinnerW));
		}
		else if (progress == 1.0) {
			gtk_spinner_stop(GTK_SPINNER(self->image_main_spinnerW));
			gtk_widget_hide(self->image_main_spinnerW);
			gtk_widget_hide(self->image_main_stopW);
			gtk_widget_show(self->image_main_yesW);
		}
	}
	else if (event == XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS) {
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(self->progressbar_escapeW), progress);
		if (progress == 0.0) {
			gtk_widget_hide(self->image_escape_stopW);
			gtk_widget_show(self->image_escape_spinnerW);
			gtk_spinner_start(GTK_SPINNER(self->image_escape_spinnerW));
		}
		else if (progress == 1.0) {
			gtk_spinner_stop(GTK_SPINNER(self->image_escape_spinnerW));
			gtk_widget_hide(self->image_escape_spinnerW);
			gtk_widget_hide(self->image_escape_stopW);
			gtk_widget_show(self->image_escape_yesW);
		}
	}
}

static void pause_button_clicked_cb(GtkWidget *button, XmiMsimGuiControlsScrolledWindow *self) {
	g_timer_stop(self->timer);

	gtk_widget_set_sensitive(self->pauseButton, FALSE);
	gtk_widget_set_sensitive(self->stopButton, FALSE);

	gint pid;
	xmi_msim_job_get_pid(self->job, &pid, NULL); // let's assume this won't fail...
	GError *error = NULL;
	gboolean clean_suspend = xmi_msim_job_suspend(self->job, &error);

	if (clean_suspend) {
		gchar *buffer = g_strdup_printf( "Process %d was successfully paused. Press the Play button to continue or Stop to kill the process\n", pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "pause-continue-stopped" ), NULL);
		g_free(buffer);
		gtk_widget_set_sensitive(self->stopButton, TRUE);
		gtk_widget_set_sensitive(self->playButton, TRUE);
		if (gtk_widget_get_visible(self->image_solid_spinnerW)) {
			gtk_spinner_stop(GTK_SPINNER(self->image_solid_spinnerW));
		}
		else if (gtk_widget_get_visible(self->image_main_spinnerW)) {
			gtk_spinner_stop(GTK_SPINNER(self->image_main_spinnerW));
		}
		else if (gtk_widget_get_visible(self->image_escape_spinnerW)) {
			gtk_spinner_stop(GTK_SPINNER(self->image_escape_spinnerW));
		}
	}
	else {
		g_timer_continue(self->timer);
		gchar *buffer = g_strdup_printf( "Process %d could not be paused: %s\n", pid, error->message);
		g_error_free(error);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "error" ), NULL);
		g_free(buffer);
		gtk_widget_set_sensitive(self->pauseButton, TRUE);
		gtk_widget_set_sensitive(self->stopButton, TRUE);
	}
}

static void stop_button_clicked_cb(GtkWidget *button, XmiMsimGuiControlsScrolledWindow *self) {
	gtk_widget_set_sensitive(self->stopButton, FALSE);
	if (self->pauseButton)
		gtk_widget_set_sensitive(self->pauseButton, FALSE);

	gint pid;
	xmi_msim_job_get_pid(self->job, &pid, NULL); // let's assume this won't fail...
	GError *error = NULL;
	gboolean clean_kill = xmi_msim_job_stop(self->job, &error);

	if (clean_kill) {
		gchar *buffer = g_strdup_printf( "Process %d was successfully terminated before completion\n", pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "pause-continue-stopped" ), NULL);
		g_free(buffer);
	}
	else {
		gchar *buffer = g_strdup_printf( "Process %d could not be stopped: %s\n", pid, error->message);
		g_error_free(error);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "error" ), NULL);
		g_free(buffer);
	}

	error_spinners(self);
}
static void play_button_clicked_cb(GtkWidget *button, XmiMsimGuiControlsScrolledWindow *self) {

	//first deal with the pause case
	if (self->pauseButton && self->job && xmi_msim_job_is_suspended(self->job)) {
		gint pid;
		gtk_widget_set_sensitive(self->playButton, FALSE);
		gboolean resume_rv;
		char *buffer;
		GError *error = NULL;
		g_timer_continue(self->timer);
		gboolean spinning;

		xmi_msim_job_get_pid(self->job, &pid, NULL); // let's assume this won't fail...

		resume_rv = xmi_msim_job_resume(self->job, &error);

		if (resume_rv) {
			buffer = g_strdup_printf( "Process %d was successfully resumed\n", pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer,-1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "pause-continue-stopped"), NULL);
			gtk_widget_set_sensitive(self->pauseButton, TRUE);
			if (gtk_widget_get_visible(self->image_solid_spinnerW)) {
				g_object_get(self->image_solid_spinnerW, "active", &spinning, NULL);
				if (spinning == FALSE) {
					gtk_spinner_start(GTK_SPINNER(self->image_solid_spinnerW));
				}
			}
			else if (gtk_widget_get_visible(self->image_main_spinnerW)) {
				g_object_get(self->image_main_spinnerW, "active", &spinning, NULL);
				if (spinning == FALSE) {
					gtk_spinner_start(GTK_SPINNER(self->image_main_spinnerW));
				}
			}
			else if (gtk_widget_get_visible(self->image_escape_spinnerW)) {
				g_object_get(self->image_escape_spinnerW, "active", &spinning, NULL);
				if (spinning == FALSE) {
					gtk_spinner_start(GTK_SPINNER(self->image_escape_spinnerW));
				}
			}
		}
		else {
			buffer = g_strdup_printf( "Process %d could not be resumed\n", pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer,-1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "error"), NULL);
			// this probably needs some better error handling...
			gtk_widget_set_sensitive(self->playButton, TRUE);
		}
		g_free(buffer);
		return;
	}

	// check undo manager -> simple for now -> more options could be provided if necessary...
	g_free(self->input_file);
	self->input_file = g_strdup(xmi_msim_gui_undo_manager_get_filename(self->manager));
	if (self->input_file == NULL) {
		GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(self))),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Cannot start simulation. The input parameters page is incomplete or has not been saved."
	       	);
	     	gtk_dialog_run(GTK_DIALOG(dialog));
	     	gtk_widget_destroy(dialog);
	     	return;
	}

	g_free(self->output_file);
	self->output_file = g_strdup(xmi_msim_gui_undo_manager_get_output_filename(self->manager));
	// if saved -> start job	

#ifdef HAVE_GOOGLE_ANALYTICS
	XmiMsimGoogleAnalyticsTracker *tracker = xmi_msim_google_analytics_tracker_get_global();
	xmi_msim_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "SIMULATION-START", NULL, NULL);
#endif

	//freeze gui except for pause and stop buttons
	gtk_widget_set_sensitive(self->playButton, FALSE);
	gtk_widget_set_sensitive(self->executableW, FALSE);
	gtk_widget_set_sensitive(self->executableB, FALSE);
	gtk_widget_set_sensitive(self->options_boxW, FALSE);
	gtk_widget_set_sensitive(self->spe_convW, FALSE);
	gtk_widget_set_sensitive(self->csv_convW, FALSE);
	gtk_widget_set_sensitive(self->html_convW, FALSE);
	gtk_widget_set_sensitive(self->spe_convB, FALSE);
	gtk_widget_set_sensitive(self->csv_convB, FALSE);
	gtk_widget_set_sensitive(self->html_convB, FALSE);
	if (self->nthreadsW != NULL)
		gtk_widget_set_sensitive(self->nthreadsW, FALSE);

	reset_controls(self);

	if (self->timer == NULL)
		self->timer = g_timer_new();
	else
		g_timer_start(self->timer);

	xmi_main_options *options = xmi_msim_gui_options_box_get_options(XMI_MSIM_GUI_OPTIONS_BOX(self->options_boxW)); 

	GError *error = NULL;
	gchar *buffer;

	if (self->job)
		g_clear_object(&self->job);

	options->omp_num_threads = (int) gtk_range_get_value(GTK_RANGE(self->nthreadsW));

	self->job = xmi_msim_job_new(
		gtk_entry_get_text(GTK_ENTRY(self->executableW)),
		self->input_file,
		options,
		gtk_entry_get_text(GTK_ENTRY(self->spe_convW)),
		gtk_entry_get_text(GTK_ENTRY(self->csv_convW)),
		gtk_entry_get_text(GTK_ENTRY(self->html_convW)),
		NULL
		);

	xmi_main_options_free(options);

	// hook up to 5 signals and start
	g_signal_connect(G_OBJECT(self->job), "stdout-event", G_CALLBACK(job_stdout_cb), self);
	g_signal_connect(G_OBJECT(self->job), "stderr-event", G_CALLBACK(job_stderr_cb), self);
	g_signal_connect(G_OBJECT(self->job), "finished-event", G_CALLBACK(job_finished_cb), self);
	g_signal_connect(G_OBJECT(self->job), "special-event", G_CALLBACK(job_special_cb), self);
	g_signal_connect(G_OBJECT(self->job), "progress-event", G_CALLBACK(job_progress_cb), self);

	if (!xmi_msim_job_start(self->job, &error)) {
		buffer = g_strdup_printf("Could not start new job: %s\n", error->message);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(self->controlsLogB), "error" ), NULL);
		g_free(buffer);
		g_error_free(error);
		gtk_widget_set_sensitive(self->playButton, TRUE);
		return;
	}

	gchar *command = xmi_msim_job_get_command(self->job);
	gint pid;
	xmi_msim_job_get_pid(self->job, &pid, NULL); // let's assume this won't fail...
	buffer = g_strdup_printf("%s was started with process id %d\n", command, pid);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(self->controlsLogW, self->timer, self->controlsLogB, buffer, -1, NULL);
	g_free(command);
	g_free(buffer);

	if (self->pauseButton)
		gtk_widget_set_sensitive(self->pauseButton, TRUE);
	gtk_widget_set_sensitive(self->stopButton, TRUE);

}

static void xmi_msim_gui_controls_scrolled_window_init(XmiMsimGuiControlsScrolledWindow *self) {
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);

	GtkWidget *superframe;
	GtkWidget *vbox_notebook;
	GtkWidget *frame;
	GtkWidget *hbox_text_label, *label;
	gchar *xmimsim_executable;
	GtkWidget *button;
	GtkWidget *buttonbox;
	GtkWidget *progressbox;
	GtkWidget *hbox_small;
	GtkWidget *hbox_controls;
	GtkWidget *cpuLabel;
	GtkWidget *cpuBox;

	superframe = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);

	self->playButton = gtk_button_new_from_icon_name("media-playback-start", GTK_ICON_SIZE_DIALOG);
	g_signal_connect(G_OBJECT(self->playButton), "clicked", G_CALLBACK(play_button_clicked_cb), self);

	self->stopButton = gtk_button_new_from_icon_name("media-playback-stop", GTK_ICON_SIZE_DIALOG);
	g_signal_connect(G_OBJECT(self->stopButton), "clicked", G_CALLBACK(stop_button_clicked_cb), self);

	if (xmi_msim_job_is_suspend_available()) {
		self->pauseButton = gtk_button_new_from_icon_name("media-playback-pause", GTK_ICON_SIZE_DIALOG);
		g_signal_connect(G_OBJECT(self->pauseButton), "clicked", G_CALLBACK(pause_button_clicked_cb), self);
	}
	else {
		self->pauseButton = NULL;
	}

	buttonbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(buttonbox), self->playButton, FALSE, FALSE, 3);
	if (self->pauseButton)
		gtk_box_pack_start(GTK_BOX(buttonbox), self->pauseButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), self->stopButton, FALSE, FALSE, 3);

	hbox_controls = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(hbox_controls), buttonbox, FALSE, FALSE, 5);

	if (xmi_omp_get_max_threads() > 1) {
		cpuBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 1);
		cpuLabel = gtk_label_new("CPUs");
		GtkAdjustment *nthreadsA = gtk_adjustment_new((gdouble) xmi_omp_get_max_threads(), 1.0, (gdouble) xmi_omp_get_max_threads(), 1.0,1.0,0.0);
		self->nthreadsW = gtk_scale_new(GTK_ORIENTATION_VERTICAL, nthreadsA);
		gtk_scale_set_digits(GTK_SCALE(self->nthreadsW), 0);
		gtk_range_set_inverted(GTK_RANGE(self->nthreadsW), TRUE);
		gtk_scale_set_value_pos(GTK_SCALE(self->nthreadsW), GTK_POS_RIGHT);
		gtk_box_pack_start(GTK_BOX(cpuBox), cpuLabel, FALSE, FALSE, 2);
		gtk_box_pack_start(GTK_BOX(cpuBox), self->nthreadsW, TRUE, TRUE, 2);
		gtk_box_pack_start(GTK_BOX(hbox_controls), cpuBox, FALSE, FALSE, 10);
	}
	else {
		self->nthreadsW = NULL;
	}

	progressbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(progressbox), TRUE);

	self->image_solid_stopW = gtk_image_new_from_icon_name("media-playback-stop", GTK_ICON_SIZE_MENU);
	gtk_widget_set_halign(self->image_solid_stopW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_solid_stopW, GTK_ALIGN_CENTER);
	self->image_solid_spinnerW = gtk_spinner_new();
	gtk_widget_set_halign(self->image_solid_spinnerW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_solid_spinnerW, GTK_ALIGN_CENTER);
	self->image_solid_yesW = gtk_image_new_from_resource("/com/github/tschoonj/xmimsim/gui/icons/gtk-yes.png");
	gtk_widget_set_halign(self->image_solid_yesW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_solid_yesW, GTK_ALIGN_CENTER);
	self->image_solid_noW = gtk_image_new_from_resource("/com/github/tschoonj/xmimsim/gui/icons/gtk-no.png");
	gtk_widget_set_halign(self->image_solid_noW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_solid_noW, GTK_ALIGN_CENTER);
	self->progressbar_solidW = gtk_progress_bar_new();
	gtk_orientable_set_orientation(GTK_ORIENTABLE(self->progressbar_solidW), GTK_ORIENTATION_HORIZONTAL);
	gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(self->progressbar_solidW), TRUE);
	hbox_small = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 1);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_solid_stopW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_solid_spinnerW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_solid_yesW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_solid_noW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->progressbar_solidW, TRUE, TRUE, 1);
	gtk_box_pack_start(GTK_BOX(progressbox), hbox_small, FALSE, FALSE, 1);

	self->image_main_stopW = gtk_image_new_from_icon_name("media-playback-stop", GTK_ICON_SIZE_MENU);
	gtk_widget_set_halign(self->image_main_stopW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_main_stopW, GTK_ALIGN_CENTER);
	self->image_main_spinnerW = gtk_spinner_new();
	gtk_widget_set_halign(self->image_main_spinnerW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_main_spinnerW, GTK_ALIGN_CENTER);
	self->image_main_yesW = gtk_image_new_from_resource("/com/github/tschoonj/xmimsim/gui/icons/gtk-yes.png");
	gtk_widget_set_halign(self->image_main_yesW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_main_yesW, GTK_ALIGN_CENTER);
	self->image_main_noW = gtk_image_new_from_resource("/com/github/tschoonj/xmimsim/gui/icons/gtk-no.png");
	gtk_widget_set_halign(self->image_main_noW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_main_noW, GTK_ALIGN_CENTER);
	self->progressbar_mainW = gtk_progress_bar_new();
	gtk_orientable_set_orientation(GTK_ORIENTABLE(self->progressbar_mainW), GTK_ORIENTATION_HORIZONTAL);
	gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(self->progressbar_mainW), TRUE);
	hbox_small = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 1);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_main_stopW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_main_spinnerW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_main_yesW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_main_noW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->progressbar_mainW, TRUE, TRUE, 1);
	gtk_box_pack_start(GTK_BOX(progressbox), hbox_small, FALSE, FALSE, 1);

	self->image_escape_stopW = gtk_image_new_from_icon_name("media-playback-stop", GTK_ICON_SIZE_MENU);
	gtk_widget_set_halign(self->image_escape_stopW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_escape_stopW, GTK_ALIGN_CENTER);
	self->image_escape_spinnerW = gtk_spinner_new();
	gtk_widget_set_halign(self->image_escape_spinnerW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_escape_spinnerW, GTK_ALIGN_CENTER);
	self->image_escape_yesW = gtk_image_new_from_resource("/com/github/tschoonj/xmimsim/gui/icons/gtk-yes.png");
	gtk_widget_set_halign(self->image_escape_yesW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_escape_yesW, GTK_ALIGN_CENTER);
	self->image_escape_noW = gtk_image_new_from_resource("/com/github/tschoonj/xmimsim/gui/icons/gtk-no.png");
	gtk_widget_set_halign(self->image_escape_noW, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(self->image_escape_noW, GTK_ALIGN_CENTER);
	self->progressbar_escapeW = gtk_progress_bar_new();
	gtk_orientable_set_orientation(GTK_ORIENTABLE(self->progressbar_escapeW), GTK_ORIENTATION_HORIZONTAL);
	gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(self->progressbar_escapeW), TRUE);
	hbox_small = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 1);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_escape_stopW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_escape_spinnerW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_escape_yesW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->image_escape_noW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_small), self->progressbar_escapeW, TRUE, TRUE, 1);
	gtk_box_pack_start(GTK_BOX(progressbox), hbox_small, FALSE, FALSE, 1);

	gtk_box_pack_start(GTK_BOX(hbox_controls), progressbox, TRUE, TRUE, 3);

	//textbuffer
	self->controlsLogW = gtk_text_view_new();
	gtk_widget_set_size_request(self->controlsLogW, 350, -1);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(self->controlsLogW), GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(self->controlsLogW), 3);
	self->controlsLogB = gtk_text_view_get_buffer(GTK_TEXT_VIEW(self->controlsLogW));
	gtk_container_set_border_width(GTK_CONTAINER(self->controlsLogW), 2);
	gtk_text_view_set_editable(GTK_TEXT_VIEW(self->controlsLogW), FALSE);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(self->controlsLogW), FALSE);
	gtk_text_buffer_create_tag(self->controlsLogB, "error", "foreground", "red", NULL);
	gtk_text_buffer_create_tag(self->controlsLogB, "success", "foreground", "green", NULL);
	gtk_text_buffer_create_tag(self->controlsLogB, "pause-continue-stopped", "foreground", "orange", NULL);

	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), self->controlsLogW);
	GtkWidget *controls_frameW = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(controls_frameW), scrolled_window);
	gtk_box_pack_start(GTK_BOX(hbox_controls), controls_frameW, TRUE, TRUE, 3);

	gtk_container_set_border_width(GTK_CONTAINER(hbox_controls), 10);
	gtk_box_pack_start(GTK_BOX(superframe),hbox_controls, FALSE, FALSE, 2);
	frame = gtk_frame_new("");
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.5, 0.5);
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Executable</span>");

	vbox_notebook = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook), 10);

	//Executable
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Executable");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->executableB = gtk_button_new_with_mnemonic("_Open");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->executableB, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(self->executableB), "clicked", G_CALLBACK(select_executable_cb), self);
	xmimsim_executable = xmi_get_xmimsim_path();
	self->executableW = gtk_entry_new();
	if (xmimsim_executable == NULL) {
		//bad...
		gtk_entry_set_text(GTK_ENTRY(self->executableW), "xmimsim");
	}
	else {
		gtk_entry_set_text(GTK_ENTRY(self->executableW), xmimsim_executable);
		g_free(xmimsim_executable);
	}
	gtk_editable_set_editable(GTK_EDITABLE(self->executableW), FALSE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->executableW, TRUE, TRUE, 0);
	gtk_container_add(GTK_CONTAINER(frame), vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe), frame, FALSE, FALSE, 2);

	//options
	frame = gtk_frame_new("");
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.5, 0.5);
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Options</span>");

	self->options_boxW = xmi_msim_gui_options_box_new();
	gtk_container_add(GTK_CONTAINER(frame), self->options_boxW);

	gtk_box_pack_start(GTK_BOX(superframe), frame, FALSE, FALSE, 2);

	//Exports
	frame = gtk_frame_new("");
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.5, 0.5);
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Export results</span>");

	GtkWidget *table_notebook = gtk_grid_new();
	gtk_grid_set_row_spacing(GTK_GRID(table_notebook), 3);
	gtk_grid_set_column_spacing(GTK_GRID(table_notebook), 3);
	gtk_container_set_border_width(GTK_CONTAINER(table_notebook), 10);

	//SPE file
	label = gtk_label_new("SPE file prefix");
	gtk_widget_set_halign(label, GTK_ALIGN_END);
	const gchar spe_tooltip[] = "Setting the prefix will result in the generation of SPE type files containing the spectral data. Compatible with PyMca and AXIL. One file is generated per interaction order.";
	gtk_widget_set_tooltip_text(GTK_WIDGET(label), spe_tooltip);
	button = gtk_button_new_with_label("Save");
	gtk_widget_set_tooltip_text(GTK_WIDGET(button), spe_tooltip);
	self->spe_convB = button;
	self->spe_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(self->spe_convW), spe_tooltip);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(select_extra_output_cb), self);
	gtk_editable_set_editable(GTK_EDITABLE(self->spe_convW), TRUE);
	gtk_widget_set_hexpand(self->spe_convW, TRUE);
	gtk_widget_set_halign(self->spe_convW, GTK_ALIGN_FILL);
	gtk_grid_attach(GTK_GRID(table_notebook), label, 0, 0, 1, 1);
	gtk_grid_attach(GTK_GRID(table_notebook), self->spe_convW, 1, 0, 1, 1);
	gtk_grid_attach(GTK_GRID(table_notebook), self->spe_convB, 2, 0, 1, 1);

	//CSV files
	label = gtk_label_new("Comma Separated Values (CSV) file");
	gtk_widget_set_halign(label, GTK_ALIGN_END);
	const gchar csv_tooltip[] = "Export the spectra as Comma Separated Values files. Readable by Microsoft Excel and other programs.";
	gtk_widget_set_tooltip_text(GTK_WIDGET(label), csv_tooltip);
	button = gtk_button_new_with_label("Save");
	gtk_widget_set_tooltip_text(GTK_WIDGET(button), csv_tooltip);
	self->csv_convB = button;
	self->csv_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(self->csv_convW), csv_tooltip);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(select_extra_output_cb), self);
	gtk_editable_set_editable(GTK_EDITABLE(self->csv_convW), TRUE);
	gtk_widget_set_hexpand(self->csv_convW, TRUE);
	gtk_widget_set_halign(self->csv_convW, GTK_ALIGN_FILL);
	gtk_grid_attach(GTK_GRID(table_notebook), label, 0, 1, 1, 1);
	gtk_grid_attach(GTK_GRID(table_notebook), self->csv_convW, 1, 1, 1, 1);
	gtk_grid_attach(GTK_GRID(table_notebook), self->csv_convB, 2, 1, 1, 1);

	//html files
	label = gtk_label_new("Report HTML file");
	gtk_widget_set_halign(label, GTK_ALIGN_END);
	const gchar html_tooltip[] = "Produces an interactive HTML file containing an overview of all the results produced by the simulation: spectra and tables of all the individual XRF lines. Readable with recent versions of Firefox, Chrome and Safari.";
	gtk_widget_set_tooltip_text(GTK_WIDGET(label), html_tooltip);
	button = gtk_button_new_with_label("Save");
	gtk_widget_set_tooltip_text(GTK_WIDGET(button), html_tooltip);
	self->html_convB = button;
	self->html_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(self->html_convW), html_tooltip);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(select_extra_output_cb), self);
	gtk_editable_set_editable(GTK_EDITABLE(self->html_convW), TRUE);
	gtk_widget_set_hexpand(self->html_convW, TRUE);
	gtk_widget_set_halign(self->html_convW, GTK_ALIGN_FILL);
	gtk_grid_attach(GTK_GRID(table_notebook), label, 0, 2, 1, 1);
	gtk_grid_attach(GTK_GRID(table_notebook), self->html_convW, 1, 2, 1, 1);
	gtk_grid_attach(GTK_GRID(table_notebook), self->html_convB, 2, 2, 1, 1);

	gtk_container_add(GTK_CONTAINER(frame), table_notebook);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE, 2);
	gtk_container_add(GTK_CONTAINER(self), superframe);

	gtk_widget_show_all(superframe);
	reset_controls(self);	
	gtk_widget_set_sensitive(self->playButton, TRUE);
	if (self->pauseButton)
		gtk_widget_set_sensitive(self->pauseButton, FALSE);
	gtk_widget_set_sensitive(self->stopButton, FALSE);
}

GtkWidget* xmi_msim_gui_controls_scrolled_window_new(XmiMsimGuiUndoManager *manager) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager), NULL);

	XmiMsimGuiControlsScrolledWindow *scrolled_window = XMI_MSIM_GUI_CONTROLS_SCROLLED_WINDOW(g_object_new(XMI_MSIM_GUI_TYPE_CONTROLS_SCROLLED_WINDOW, NULL));
	scrolled_window->manager = g_object_ref(manager);

	return GTK_WIDGET(scrolled_window);
}
