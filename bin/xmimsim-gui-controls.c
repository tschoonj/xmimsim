/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-controls.h"
#include "xmimsim-gui-results.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-notifications.h"
#include "xmimsim-gui-options-box.h"
#include "xmi_aux.h"
#include "xmi_xml.h"
#include "xmi_detector.h"
#include "xmi_data_structs.h"
#include <glib.h>
#include <string.h>
#include <gmodule.h>
#ifdef G_OS_UNIX
  #include <sys/types.h>
  #include <sys/wait.h>
  #include <signal.h>
  #define real_xmimsim_pid ((int) xmimsim_pid)
#elif defined(G_OS_WIN32)
  #include <windows.h>
  #include <winbase.h>
  #include "xmi_detector.h"
  #include "xmi_solid_angle.h"
  typedef LONG (NTAPI *pNtSuspendProcess )(IN HANDLE ProcessHandle );
  typedef LONG (NTAPI *pNtResumeProcess )(IN HANDLE ProcessHandle );
  static pNtSuspendProcess NtSuspendProcess = NULL;
  static pNtResumeProcess NtResumeProcess = NULL;
  #define real_xmimsim_pid ((int) GetProcessId((HANDLE) xmimsim_pid))
#endif

#ifdef HAVE_GOOGLE_ANALYTICS
  #include "xmimsim-gui-google-analytics.h"
#endif


#ifdef MAC_INTEGRATION
#include <gtkosxapplication.h>
#include <xmi_resources_mac.h>
#endif
struct window_entry {
	GtkWidget *window;
	GtkWidget *entry;
};


static GtkWidget *executableW;
static GtkWidget *executableB;
static GtkWidget *options_boxW;
static GtkWidget *spe_convW;
static GtkWidget *spe_convB;
static GtkWidget *csv_convW;
static GtkWidget *csv_convB;
static GtkWidget *svg_convW;
static GtkWidget *svg_convB;
static GtkWidget *html_convW;
static GtkWidget *html_convB;

static GtkWidget *playButton;
static GtkWidget *pauseButton = NULL;
static GtkWidget *stopButton;

static GtkWidget *controlsLogW;
static GtkTextBuffer *controlsLogB;

static GtkWidget *progressbar_solidW;
static GtkWidget *progressbar_mainW;
static GtkWidget *progressbar_escapeW;

static GtkWidget *image_solidW;
static GtkWidget *image_mainW;
static GtkWidget *image_escapeW;

static GtkWidget *nthreadsW;
static GTimer *timer = NULL;

GPid xmimsim_pid = GPID_INACTIVE;

static gboolean xmimsim_paused = FALSE;

void reset_controls(void);

void error_spinners(void) {
	//check spinners
	if (GTK_IS_SPINNER(gtk_bin_get_child(GTK_BIN(image_solidW)))) {
		gtk_spinner_stop(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_solidW))));
		gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_solidW)));
		gtk_container_add(GTK_CONTAINER(image_solidW),gtk_image_new_from_stock(GTK_STOCK_NO,GTK_ICON_SIZE_MENU));
		gtk_widget_show_all(image_solidW);
	}
	if (GTK_IS_SPINNER(gtk_bin_get_child(GTK_BIN(image_mainW)))) {
		gtk_spinner_stop(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_mainW))));
		gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_mainW)));
		gtk_container_add(GTK_CONTAINER(image_mainW),gtk_image_new_from_stock(GTK_STOCK_NO,GTK_ICON_SIZE_MENU));
		gtk_widget_show_all(image_mainW);
	}
	if (GTK_IS_SPINNER(gtk_bin_get_child(GTK_BIN(image_escapeW)))) {
		gtk_spinner_stop(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_escapeW))));
		gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_escapeW)));
		gtk_container_add(GTK_CONTAINER(image_escapeW),gtk_image_new_from_stock(GTK_STOCK_NO,GTK_ICON_SIZE_MENU));
		gtk_widget_show_all(image_escapeW);
	}

}
static gboolean executable_file_filter(const GtkFileFilterInfo *filter_info, gpointer data) {
	return g_file_test(filter_info->filename,G_FILE_TEST_IS_EXECUTABLE);
}


static int process_xmimsim_stdout_string(gchar *string) {
	int percentage;
	char *buffer;

	//solid angles
	if(strncmp(string,"Solid angle grid already present in ",strlen("Solid angle grid already present in ")) == 0) {
		gtk_image_set_from_stock(GTK_IMAGE(gtk_bin_get_child(GTK_BIN(image_solidW))),GTK_STOCK_YES, GTK_ICON_SIZE_MENU);
		gtk_widget_show_all(image_solidW);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_solidW),"Solid angle grid loaded from file");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_solidW),1.0);
		while(gtk_events_pending())
		    gtk_main_iteration();
		return 1;
	}
	else if(strncmp(string, "Operating in brute-force mode: solid angle grid is redundant", strlen("Operating in brute-force mode: solid angle grid is redundant")) == 0) {
		gtk_image_set_from_stock(GTK_IMAGE(gtk_bin_get_child(GTK_BIN(image_solidW))),GTK_STOCK_YES, GTK_ICON_SIZE_MENU);
		gtk_widget_show_all(image_solidW);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_solidW),"Solid angle grid redundant");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_solidW),1.0);
		while(gtk_events_pending())
		    gtk_main_iteration();
		return 1;
	}
	else if(strncmp(string, "No escape peaks requested: escape peak calculation is redundant", strlen("No escape peaks requested: escape peak calculation is redundant")) == 0) {
		gtk_image_set_from_stock(GTK_IMAGE(gtk_bin_get_child(GTK_BIN(image_escapeW))),GTK_STOCK_YES, GTK_ICON_SIZE_MENU);
		gtk_widget_show_all(image_escapeW);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_escapeW),"Escape peaks redundant");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_escapeW),1.0);
		while(gtk_events_pending())
		    gtk_main_iteration();
		return 1;
	}
	else if(strncmp(string,"Precalculating solid angle grid",strlen("Precalculating solid angle grid")) == 0) {
		gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_solidW)));
		gtk_container_add(GTK_CONTAINER(image_solidW),gtk_spinner_new());
		gtk_widget_show_all(image_solidW);
		gtk_spinner_start(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_solidW))));
		return 1;
	}
	else if(sscanf(string,"Solid angle calculation at %i",&percentage) == 1) {
		buffer = g_strdup_printf("Solid angle grid: %i %%",percentage);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_solidW),buffer);
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_solidW),((double) percentage)/100.0);
		while(gtk_events_pending())
		    gtk_main_iteration();
		return 0;
	}
	else if(strncmp(string,"Solid angle calculation finished",strlen("Solid angle calculation finished")) == 0) {
		gtk_spinner_stop(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_solidW))));
		gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_solidW)));
		gtk_container_add(GTK_CONTAINER(image_solidW),gtk_image_new_from_stock(GTK_STOCK_YES,GTK_ICON_SIZE_MENU));
		gtk_widget_show_all(image_solidW);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_solidW),"Solid angle grid calculated");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_solidW),1.0);
		while(gtk_events_pending())
		    gtk_main_iteration();
		return 1;
	}
	//interactions
	else if(sscanf(string,"Simulating interactions at %i",&percentage) == 1) {
		buffer = g_strdup_printf("Simulating interactions: %i %%",percentage);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_mainW),buffer);
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_mainW),((double) percentage)/100.0);
		while(gtk_events_pending())
		    gtk_main_iteration();
		return 0;
	}
	else if(strncmp(string,"Simulating interactions",strlen("Simulating interactions")) == 0) {
		gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_mainW)));
		gtk_container_add(GTK_CONTAINER(image_mainW),gtk_spinner_new());
		gtk_widget_show_all(image_mainW);
		gtk_spinner_start(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_mainW))));
		return 1;
	}
	else if(strncmp(string,"Interactions simulation finished",strlen("Interactions simulation finished")) == 0) {
		gtk_spinner_stop(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_mainW))));
		gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_mainW)));
		gtk_container_add(GTK_CONTAINER(image_mainW),gtk_image_new_from_stock(GTK_STOCK_YES,GTK_ICON_SIZE_MENU));
		gtk_widget_show_all(image_mainW);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_mainW),"Interactions simulated");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_mainW),1.0);
		while(gtk_events_pending())
		    gtk_main_iteration();
		return 1;
	}
	//escape ratios
	else if(strncmp(string,"Escape peak ratios already present in ",strlen("Escape peak ratios already present in ")) == 0) {
		gtk_image_set_from_stock(GTK_IMAGE(gtk_bin_get_child(GTK_BIN(image_escapeW))),GTK_STOCK_YES, GTK_ICON_SIZE_MENU);
		gtk_widget_show_all(image_escapeW);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_escapeW),"Escape peak ratios loaded from file");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_escapeW),1.0);
		while(gtk_events_pending())
		    gtk_main_iteration();
		return 1;
	}
	else if(strncmp(string,"Precalculating escape peak ratios",strlen("Precalculating escape peak ratios")) == 0) {
		gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_escapeW)));
		gtk_container_add(GTK_CONTAINER(image_escapeW),gtk_spinner_new());
		gtk_widget_show_all(image_escapeW);
		gtk_spinner_start(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_escapeW))));
		return 1;
	}
	else if(sscanf(string,"Escape peak ratios calculation at %i",&percentage) == 1) {
		buffer = g_strdup_printf("Escape peak ratios: %i %%",percentage);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_escapeW),buffer);
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_escapeW),((double) percentage)/100.0);
		while(gtk_events_pending())
		    gtk_main_iteration();
		return 0;
	}
	else if(strncmp(string,"Escape peak ratios calculation finished",strlen("Escape peak ratios calculation finished")) == 0) {
		gtk_spinner_stop(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_escapeW))));
		gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_escapeW)));
		gtk_container_add(GTK_CONTAINER(image_escapeW),gtk_image_new_from_stock(GTK_STOCK_YES,GTK_ICON_SIZE_MENU));
		gtk_widget_show_all(image_escapeW);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_escapeW),"Escape peak ratios calculated");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_escapeW),1.0);
		while(gtk_events_pending())
		    gtk_main_iteration();
		return 1;
	}



	return 1;
}





static gboolean xmimsim_stdout_watcher(GIOChannel *source, GIOCondition condition, gpointer data) {
	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	char *buffer;

	if (condition & (G_IO_IN|G_IO_PRI)) {
		/*while (gtk_events_pending ())
		        gtk_main_iteration ();*/
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);
		if (pipe_status == G_IO_STATUS_ERROR) {
			buffer = g_strdup_printf("%s with process id %i had an I/O error: %s\n",(char *) data, real_xmimsim_pid,pipe_error->message);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			g_error_free(pipe_error);
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			//lot of message handling should come here...
			//for now just print them in the box
			if (process_xmimsim_stdout_string(pipe_string))
				xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, pipe_string,-1,NULL);
			g_free(pipe_string);
		}
		else
			return FALSE;

	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		//buffer = g_strdup_printf("%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		//xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}


static gboolean xmimsim_stderr_watcher(GIOChannel *source, GIOCondition condition, gpointer data) {
	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	char *buffer;

	if (condition & (G_IO_IN|G_IO_PRI)) {
		/*while (gtk_events_pending ())
		        gtk_main_iteration ();*/
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);
		if (pipe_status == G_IO_STATUS_ERROR) {
			buffer = g_strdup_printf("%s with process id %i had an I/O error: %s\n",(char *) data, real_xmimsim_pid,pipe_error->message);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			g_error_free(pipe_error);
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, pipe_string,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			g_free(pipe_string);
		}
		else
			return FALSE;

	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		//buffer = g_strdup_printf("%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		//xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}

struct child_data {
	char *outputfile;
	gchar *executable;
	GtkWidget *window;
};

static void xmimsim_child_watcher_cb(GPid pid, gint status, struct child_data *cd) {
	char *buffer;
	int success;


	gchar *data = cd->executable;


	gtk_widget_set_sensitive(stopButton,FALSE);
	if (pauseButton)
		gtk_widget_set_sensitive(pauseButton,FALSE);

	//windows <-> unix issues here
	//unix allows to obtain more info about the way the process was terminated, windows will just have the exit code (status)
	//conditional compilation here
#ifdef G_OS_UNIX
	if (WIFEXITED(status)) {
		if (WEXITSTATUS(status) == 0) { /* child was terminated due to a call to exit */
			buffer = g_strdup_printf("%s with process id %i exited normally without errors\n", data, real_xmimsim_pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"success" ),NULL);
			success = 1;
		}
		else {
			buffer = g_strdup_printf("%s with process id %i exited with an error (code: %i)\n",data, real_xmimsim_pid, WEXITSTATUS(status));
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			success = 0;
		}
	}
	else if (WIFSIGNALED(status)) { /* child was terminated due to a signal */
		buffer = g_strdup_printf( "%s with process id %i was terminated by signal %i\n",data, real_xmimsim_pid, WTERMSIG(status));
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		success = 0;
	}
	else {
		buffer = g_strdup_printf( "%s with process id %i was terminated in some special way\n",data, real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		success = 0;
	}

#elif defined(G_OS_WIN32)
	if (status == 0) {
		buffer = g_strdup_printf("%s with process id %i exited normally without errors\n", data, real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"success" ),NULL);
		success = 1;
	}
	else {
		buffer = g_strdup_printf("%s with process id %i exited with an error (code: %i)\n",data, real_xmimsim_pid, status);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		success = 0;
	}
#endif

	g_spawn_close_pid(xmimsim_pid);
	xmimsim_pid = GPID_INACTIVE;

	gtk_widget_set_sensitive(playButton,TRUE);
	//make sensitive again
	gtk_widget_set_sensitive(executableW,TRUE);
	gtk_widget_set_sensitive(executableB,TRUE);
	gtk_widget_set_sensitive(options_boxW,TRUE);
	gtk_widget_set_sensitive(spe_convW,TRUE);
	gtk_widget_set_sensitive(csv_convW,TRUE);
	gtk_widget_set_sensitive(svg_convW,TRUE);
	gtk_widget_set_sensitive(html_convW,TRUE);
	gtk_widget_set_sensitive(spe_convB,TRUE);
	gtk_widget_set_sensitive(csv_convB,TRUE);
	gtk_widget_set_sensitive(svg_convB,TRUE);
	gtk_widget_set_sensitive(html_convB,TRUE);
	if (nthreadsW != NULL)
		gtk_widget_set_sensitive(nthreadsW,TRUE);

	//g_timer_stop(timer);
	//g_timer_destroy(timer);

	if (success == 0) {
		xmimsim_notifications_deliver("Simulation failed","Check error messages");
		//if something is spinning, make it stop and make it red
		error_spinners();

		return;
	}
	else {
		gchar *my_basename = g_path_get_basename(cd->outputfile);
		gchar *information = g_strdup_printf("%s is now showing in the results window", my_basename);
		xmimsim_notifications_deliver("Simulation succeeded",information);
		g_free(my_basename);
		g_free(information);
	}


	//if successful, read the spectrum in
	gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),results_page);
	if(plot_spectra_from_file(cd->outputfile) == 1) {
		gchar *temp_base = g_path_get_basename(cd->outputfile);
		update_xmimsim_title_xmso(temp_base, cd->window, cd->outputfile);
		g_free(temp_base);
	}
	else {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(cd->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
		        GTK_MESSAGE_ERROR,
		        GTK_BUTTONS_CLOSE,
		        "Could not read file %s",cd->outputfile
	                );
	     	gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);

	}


#ifdef MAC_INTEGRATION
	gtkosx_application_attention_request((GtkosxApplication *) g_object_new(GTKOSX_TYPE_APPLICATION, NULL), CRITICAL_REQUEST);
#endif
	g_free(cd->outputfile);
	g_free(cd);

	return;
}

void start_job(struct undo_single *xmimsim_struct, GtkWidget *window) {
	GPtrArray *argv;
	gchar *wd;
	gchar *tmp_string;
	gboolean spawn_rv;
	gint out_fh, err_fh;
	GError *spawn_error = NULL;
	char *buffer;
	const gchar *encoding = NULL;
	struct child_data *cd;

#ifdef HAVE_GOOGLE_ANALYTICS
	const XmiMsimGuiGoogleAnalyticsTracker *tracker = xmi_msim_gui_google_analytics_tracker_get_global();
	xmi_msim_gui_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "SIMULATION-START", NULL, NULL);

#endif

	//freeze gui except for pause and stop buttons
	gtk_widget_set_sensitive(playButton,FALSE);
	gtk_widget_set_sensitive(executableW,FALSE);
	gtk_widget_set_sensitive(executableB,FALSE);
	gtk_widget_set_sensitive(options_boxW,FALSE);
	gtk_widget_set_sensitive(spe_convW,FALSE);
	gtk_widget_set_sensitive(csv_convW,FALSE);
	gtk_widget_set_sensitive(svg_convW,FALSE);
	gtk_widget_set_sensitive(html_convW,FALSE);
	gtk_widget_set_sensitive(spe_convB,FALSE);
	gtk_widget_set_sensitive(csv_convB,FALSE);
	gtk_widget_set_sensitive(svg_convB,FALSE);
	gtk_widget_set_sensitive(html_convB,FALSE);
	if (nthreadsW != NULL)
		gtk_widget_set_sensitive(nthreadsW,FALSE);

	reset_controls();
	if (timer == NULL)
		timer = g_timer_new();
	else
		g_timer_start(timer);

	struct xmi_main_options *options = xmi_msim_gui_options_box_get_options(XMI_MSIM_GUI_OPTIONS_BOX(options_boxW)); 

	argv = g_ptr_array_new_full(10, g_free);
	g_ptr_array_add(argv, g_strdup(gtk_entry_get_text(GTK_ENTRY(executableW))));

	if (options->use_M_lines) {
		g_ptr_array_add(argv, g_strdup("--enable-M-lines"));
	}
	else {
		g_ptr_array_add(argv, g_strdup("--disable-M-lines"));
	}

	if (options->use_cascade_radiative) {
		g_ptr_array_add(argv, g_strdup("--enable-radiative-cascade"));
	}
	else {
		g_ptr_array_add(argv, g_strdup("--disable-radiative-cascade"));
	}

	if (options->use_cascade_auger) {
		g_ptr_array_add(argv, g_strdup("--enable-auger-cascade"));
	}
	else {
		g_ptr_array_add(argv, g_strdup("--disable-auger-cascade"));
	}

	if (options->use_variance_reduction) {
		g_ptr_array_add(argv, g_strdup("--enable-variance-reduction"));
	}
	else {
		g_ptr_array_add(argv, g_strdup("--disable-variance-reduction"));
	}

	if (options->use_sum_peaks) {
		g_ptr_array_add(argv, g_strdup("--enable-pile-up"));
	}
	else {
		g_ptr_array_add(argv, g_strdup("--disable-pile-up"));
	}

	if (options->use_poisson) {
		g_ptr_array_add(argv, g_strdup("--enable-poisson"));
	}
	else {
		g_ptr_array_add(argv, g_strdup("--disable-poisson"));
	}
	
	if (options->use_escape_peaks) {
		g_ptr_array_add(argv, g_strdup("--enable-escape-peaks"));
	}
	else {
		g_ptr_array_add(argv, g_strdup("--disable-escape-peaks"));
	}

	if (options->use_advanced_compton) {
		g_ptr_array_add(argv, g_strdup("--enable-advanced-compton"));
	}
	else {
		g_ptr_array_add(argv, g_strdup("--disable-advanced-compton"));
	}

	if (options->use_default_seeds) {
		g_ptr_array_add(argv, g_strdup("--enable-default-seeds"));
	}
	else {
		g_ptr_array_add(argv, g_strdup("--disable-default-seeds"));
	}

	g_ptr_array_add(argv, g_strdup("--verbose"));

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	if (options->use_opencl) {
		g_ptr_array_add(argv, g_strdup("--enable-opencl"));
	}
	else {
		g_ptr_array_add(argv, g_strdup("--disable-opencl"));
	}
#endif

	if (options->custom_detector_response) {
		g_ptr_array_add(argv, g_strdup_printf("--custom-detector-response=%s", options->custom_detector_response));
	}

	tmp_string = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(spe_convW))));
	if (strlen(tmp_string) > 0) {
		g_ptr_array_add(argv, g_strdup_printf("--spe-file=%s", tmp_string));
	}
	g_free(tmp_string);

	tmp_string = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(csv_convW))));
	if (strlen(tmp_string) > 0) {
		g_ptr_array_add(argv, g_strdup_printf("--csv-file=%s", tmp_string));
	}
	g_free(tmp_string);

	tmp_string = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(svg_convW))));
	if (strlen(tmp_string) > 0) {
		g_ptr_array_add(argv, g_strdup_printf("--svg-file=%s", tmp_string));
	}
	g_free(tmp_string);

	tmp_string = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(html_convW))));
	if (strlen(tmp_string) > 0) {
		g_ptr_array_add(argv, g_strdup_printf("--htm-file=%s", tmp_string));
	}
	g_free(tmp_string);

#ifdef G_OS_WIN32
	//set solid angles and escape ratios files ourself!
	char *xmimsim_hdf5_solid_angles = NULL;
	char *xmimsim_hdf5_escape_ratios = NULL;

	if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0) {
		buffer = g_strdup_printf("Could not determine solid angles HDF5 file\n");
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		gtk_widget_set_sensitive(playButton,TRUE);
		return;
	}
	g_ptr_array_add(argv, g_strdup_printf("--with-solid-angles-data=%s", xmimsim_hdf5_solid_angles));

	if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0) {
		buffer = g_strdup_printf("Could not determine escape ratios HDF5 file\n");
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		gtk_widget_set_sensitive(playButton,TRUE);
		return;
	}
	g_ptr_array_add(argv, g_strdup_printf("--with-escape-ratios-data=%s", xmimsim_hdf5_escape_ratios));
#endif



	//number of threads
	if (nthreadsW != NULL) {
		g_ptr_array_add(argv, g_strdup_printf("--set-threads=%d",(int) gtk_range_get_value(GTK_RANGE(nthreadsW))));
	}

	g_ptr_array_add(argv, g_strdup(xmimsim_struct->filename));
	g_ptr_array_add(argv, NULL);

	g_free(options->custom_detector_response);
	g_free(options);

	wd = g_path_get_dirname(xmimsim_struct->xi->general->outputfile);

	//execute command
	spawn_rv = g_spawn_async_with_pipes(wd, (gchar **) argv->pdata, NULL, G_SPAWN_DO_NOT_REAP_CHILD, NULL, NULL,
		&xmimsim_pid, NULL, &out_fh, &err_fh, &spawn_error);


	if (spawn_rv == FALSE) {
		//couldn't spawn
		//print messag_ to textbox in red...
		buffer = g_strdup_printf("%s\n",spawn_error->message);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		g_error_free(spawn_error);
		gtk_widget_set_sensitive(playButton,TRUE);
		xmimsim_pid = (GPid) -1;
		return;
	}

	gchar *command = g_strjoinv(" ", (gchar **) argv->pdata);
	buffer = g_strdup_printf("%s was started with process id %i\n", command, real_xmimsim_pid);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,NULL);
	g_free(wd);
	g_free(command);

	xmimsim_paused=FALSE;
	if (pauseButton)
		gtk_widget_set_sensitive(pauseButton,TRUE);
	gtk_widget_set_sensitive(stopButton,TRUE);

	cd = (struct child_data *) g_malloc(sizeof(struct child_data));
	cd->outputfile = g_strdup(xmimsim_struct->xi->general->outputfile);
	cd->executable = g_strdup(g_ptr_array_index(argv, 0));
	cd->window = window;

	GIOChannel *xmimsim_stdout;
	GIOChannel *xmimsim_stderr;

#ifdef G_OS_WIN32
	xmimsim_stderr= g_io_channel_win32_new_fd(err_fh);
	xmimsim_stdout = g_io_channel_win32_new_fd(out_fh);
#else
	xmimsim_stderr= g_io_channel_unix_new(err_fh);
	xmimsim_stdout = g_io_channel_unix_new(out_fh);
#endif

	g_child_watch_add(xmimsim_pid,(GChildWatchFunc) xmimsim_child_watcher_cb, (gpointer) cd);


	g_get_charset(&encoding);

	g_io_channel_set_encoding(xmimsim_stdout, encoding, NULL);
	g_io_channel_set_close_on_unref(xmimsim_stdout,TRUE);
	g_io_add_watch(xmimsim_stdout, (GIOCondition) (G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL), xmimsim_stdout_watcher, g_strdup(g_ptr_array_index(argv, 0)));
	g_io_channel_unref(xmimsim_stdout);

	g_io_channel_set_encoding(xmimsim_stderr, encoding, NULL);
	g_io_channel_set_close_on_unref(xmimsim_stderr,TRUE);
	g_io_add_watch(xmimsim_stderr, (GIOCondition) (G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL), xmimsim_stderr_watcher, g_strdup(g_ptr_array_index(argv, 0)));
	g_io_channel_unref(xmimsim_stderr);

	g_ptr_array_unref(argv);

}

static void pause_button_clicked_cb(GtkWidget *widget, gpointer data) {
	//UNIX only

	int kill_rv;
	char *buffer;
	gboolean spinning;

	g_timer_stop(timer);

	gtk_widget_set_sensitive(pauseButton,FALSE);
	gtk_widget_set_sensitive(stopButton,FALSE);
#ifdef G_OS_UNIX
	kill_rv = kill((pid_t) xmimsim_pid, SIGSTOP);
#elif defined(G_OS_WIN32)
	kill_rv = (int) NtSuspendProcess((HANDLE) xmimsim_pid);
#endif
	if (kill_rv == 0) {
		buffer = g_strdup_printf( "Process %i was successfully paused. Press the Play button to continue or Stop to kill the process\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
		xmimsim_paused=TRUE;
		gtk_widget_set_sensitive(stopButton,TRUE);
		gtk_widget_set_sensitive(playButton,TRUE);
		if (GTK_IS_SPINNER(gtk_bin_get_child(GTK_BIN(image_solidW)))) {
			g_object_get(gtk_bin_get_child(GTK_BIN(image_solidW)),"active",&spinning,NULL);
			if (spinning == TRUE) {
				gtk_spinner_stop(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_solidW))));
			}
		}
		if (GTK_IS_SPINNER(gtk_bin_get_child(GTK_BIN(image_mainW)))) {
			g_object_get(gtk_bin_get_child(GTK_BIN(image_mainW)),"active",&spinning,NULL);
			if (spinning == TRUE) {
				gtk_spinner_stop(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_mainW))));
			}
		}
		if (GTK_IS_SPINNER(gtk_bin_get_child(GTK_BIN(image_escapeW)))) {
			g_object_get(gtk_bin_get_child(GTK_BIN(image_escapeW)),"active",&spinning,NULL);
			if (spinning == TRUE) {
				gtk_spinner_stop(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_escapeW))));
			}
		}
	}
	else {
		g_timer_continue(timer);
		buffer = g_strdup_printf( "Process %i could not be paused.\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		xmimsim_paused=FALSE;
		gtk_widget_set_sensitive(pauseButton,TRUE);
		gtk_widget_set_sensitive(stopButton,TRUE);
	}


}

static void stop_button_clicked_cb(GtkWidget *widget, gpointer data) {
	//difference UNIX <-> Windows
	//UNIX -> send sigkill signal
	//Windows -> TerminateProcess
	char *buffer;

	gtk_widget_set_sensitive(stopButton,FALSE);
	if (pauseButton)
		gtk_widget_set_sensitive(pauseButton,FALSE);
	//g_io_channel_shutdown(xmimsim_stdout, FALSE, NULL);
	//g_io_channel_shutdown(xmimsim_stderr, FALSE, NULL);
	//g_io_channel_unref(xmimsim_stdout);
	//g_io_channel_unref(xmimsim_stderr);

	//set buttons back in order
	xmimsim_paused = FALSE;
#ifdef G_OS_UNIX
	int kill_rv;

	kill_rv = kill((pid_t) xmimsim_pid, SIGTERM);
#if !GLIB_CHECK_VERSION (2, 35, 0)
	//starting with 2.36.0 (and some unstable versions before),
	//waitpid is called from within the main loop
	//causing all kinds of trouble if I would call wait here
	//wait(NULL);
	waitpid(xmimsim_pid, NULL, WNOHANG);
#endif
	if (kill_rv == 0) {
		buffer = g_strdup_printf( "Process %i was successfully terminated before completion\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		buffer = g_strdup_printf( "Process %i could not be terminated with the SIGTERM signal\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
	}

#elif defined(G_OS_WIN32)
	BOOL terminate_rv;

	terminate_rv = TerminateProcess((HANDLE) xmimsim_pid, (UINT) 1);

	if (terminate_rv == TRUE) {
		buffer = g_strdup_printf( "Process %i was successfully terminated\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		buffer = g_strdup_printf( "Process %i could not be terminated with the TerminateProcess call\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
	}
#endif

	error_spinners();
}

static void play_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct undo_single *check_rv;
	int check_status;
	GtkWidget *dialog = NULL;
	GtkWidget *content;
	gint dialog_rv;
	GtkWidget *label;
	GtkTextIter iterb, itere;
	char *filename;

	if (pauseButton && xmimsim_paused == TRUE) {
		gtk_widget_set_sensitive(playButton,FALSE);
		//send SIGCONT
		int kill_rv;
		char *buffer;
		gboolean spinning;

		g_timer_continue(timer);

#ifdef G_OS_UNIX
		kill_rv = kill((pid_t) xmimsim_pid, SIGCONT);
#elif defined(G_OS_WIN32)
		kill_rv = (int) NtResumeProcess((HANDLE) xmimsim_pid);
#endif
		if (kill_rv == 0) {
			buffer = g_strdup_printf( "Process %i was successfully resumed\n", real_xmimsim_pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
			gtk_widget_set_sensitive(pauseButton,TRUE);
			xmimsim_paused = FALSE;
			if (GTK_IS_SPINNER(gtk_bin_get_child(GTK_BIN(image_solidW)))) {
				g_object_get(gtk_bin_get_child(GTK_BIN(image_solidW)),"active",&spinning,NULL);
				if (spinning == FALSE) {
					gtk_spinner_start(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_solidW))));
				}
			}
			if (GTK_IS_SPINNER(gtk_bin_get_child(GTK_BIN(image_mainW)))) {
				g_object_get(gtk_bin_get_child(GTK_BIN(image_mainW)),"active",&spinning,NULL);
				if (spinning == FALSE) {
					gtk_spinner_start(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_mainW))));
				}
			}
			if (GTK_IS_SPINNER(gtk_bin_get_child(GTK_BIN(image_escapeW)))) {
				g_object_get(gtk_bin_get_child(GTK_BIN(image_escapeW)),"active",&spinning,NULL);
				if (spinning == FALSE) {
					gtk_spinner_start(GTK_SPINNER(gtk_bin_get_child(GTK_BIN(image_escapeW))));
				}
			}
		}
		else {
			buffer = g_strdup_printf( "Process %i could not be resumed\n", real_xmimsim_pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(controlsLogW, timer, controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			gtk_widget_set_sensitive(playButton,TRUE);
		}
		return;
	}

	xmimsim_paused = FALSE;

	if (check_changeables() == 0) {
		dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Cannot start simulation. The input parameters page contains invalid values."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
	     	return;

	}

	check_rv = check_changes_saved(&check_status);
	if (check_status == CHECK_CHANGES_SAVED_BEFORE) {
		//saved before: give option to continue or to SAVE
		dialog = gtk_dialog_new_with_buttons("",GTK_WINDOW(data),
			GTK_DIALOG_MODAL,
			GTK_STOCK_OK,GTK_RESPONSE_OK,
			GTK_STOCK_CANCEL,GTK_RESPONSE_CANCEL,
			GTK_STOCK_SAVE,GTK_RESPONSE_SAVE,
			NULL
		);
		content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
		label = gtk_label_new("You have made changes since your last save. Continue with last saved version or save changes?");
		gtk_widget_show(label);
		gtk_box_pack_start(GTK_BOX(content),label, FALSE, FALSE, 3);

		//run dialog
		dialog_rv = gtk_dialog_run(GTK_DIALOG(dialog));
		switch (dialog_rv) {
			case GTK_RESPONSE_CANCEL:
			case GTK_RESPONSE_DELETE_EVENT:
				//user was scared off by the message: nothing happens
				gtk_widget_destroy(dialog);
				return;
			case GTK_RESPONSE_SAVE:
				if (check_changeables() == 0 || xmi_validate_input(current->xi) != 0 )  {
					gtk_widget_destroy(dialog);
					dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
					GTK_DIALOG_DESTROY_WITH_PARENT,
		        		GTK_MESSAGE_ERROR,
		        		GTK_BUTTONS_CLOSE,
		        		"Could not write to file: model is incomplete/invalid"
	                		);
	     				gtk_dialog_run (GTK_DIALOG (dialog));
	     				gtk_widget_destroy (dialog);
	     				return;
				}
				//get text from comments...
				gtk_text_buffer_get_bounds(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW)),&iterb, &itere);
				if (gtk_text_iter_equal (&iterb, &itere) == TRUE) {
					g_free(current->xi->general->comments);
					current->xi->general->comments = g_strdup("");
				}
				else {
					g_free(current->xi->general->comments);
					current->xi->general->comments = g_strdup(gtk_text_buffer_get_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW)),&iterb, &itere, FALSE));
				}
				//update file
				if (xmi_write_input_xml(check_rv->filename, current->xi) == 1) {
					filename = g_strdup(last_saved->filename);
					g_free(last_saved->filename);
					xmi_free_input(last_saved->xi);
					g_free(last_saved);
					last_saved = (struct undo_single *) g_malloc(sizeof(struct undo_single));
					xmi_copy_input(current->xi, &(last_saved->xi));
					last_saved->filename = g_strdup(filename);
					g_free(filename);
					gtk_widget_destroy (dialog);
					gtk_widget_set_sensitive(saveW,FALSE);
					gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
					start_job(last_saved, (GtkWidget *) data);
					break;
				}
				else {
					gtk_widget_destroy (dialog);
					dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
						GTK_DIALOG_DESTROY_WITH_PARENT,
		        			GTK_MESSAGE_ERROR,
		        			GTK_BUTTONS_CLOSE,
		        			"Could not write to file %s: model is incomplete/invalid",check_rv->filename
	                		);
	     				gtk_dialog_run (GTK_DIALOG (dialog));
	     				gtk_widget_destroy (dialog);
					//g_signal_handler_block(G_OBJECT(notebook), notebookG);
					gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),input_page);
					//g_signal_handler_unblock(G_OBJECT(notebook), notebookG);
					//send user back to input page
					return;
				}

			case GTK_RESPONSE_OK:
				//proceed without updating the file (which is probably stupid)
				//launch simulation...
				gtk_widget_destroy(dialog);
				start_job(check_rv, (GtkWidget *) data);
				break;



		}
	}
	else if (check_status == CHECK_CHANGES_JUST_SAVED) {
		//current version corresponds to saved version
		//launch simulation...
		start_job(check_rv, (GtkWidget *) data);
	}
	else if (check_status == CHECK_CHANGES_NEVER_SAVED ||
		check_status == CHECK_CHANGES_NEW) {
		//hmmm... send user back to first page with error message
		dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	        	GTK_MESSAGE_ERROR,
	        	GTK_BUTTONS_CLOSE,
	        	"Input parameters need to be saved before the simulation can be launched."
                );
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		return;
	}

	return;
}


static void select_executable_cb(GtkButton *button, gpointer data) {
	XmiMsimGuiFileChooserDialog *dialog;
	GtkFileFilter *filter;
	gchar *filename;


	filter = gtk_file_filter_new();
	gtk_file_filter_add_custom(filter, GTK_FILE_FILTER_FILENAME, executable_file_filter, NULL, NULL);
	gtk_file_filter_set_name(filter,"Executables");
	dialog = xmi_msim_gui_file_chooser_dialog_new ("Open simulation executable",
		GTK_WINDOW((GtkWidget *) data),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_OPEN,
		GTK_STOCK_CANCEL
		);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gtk_entry_set_text(GTK_ENTRY(executableW),filename);
		g_free(filename);
	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);

}

static void select_extra_output_cb(GtkButton *button, gpointer data) {
	XmiMsimGuiFileChooserDialog *dialog;
	GtkFileFilter *filter=NULL;
	gchar *filename;
	struct window_entry *we = (struct window_entry *) data;
	gchar *title;

	if (we->entry == spe_convW) {
		//gtk_file_filter_add_pattern(filter,"*.spe");
		//gtk_file_filter_set_name(filter,"SPE spectral data files");
		title = g_strdup("Select the prefix of the SPE files");
	}
	else if (we->entry == svg_convW) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.svg");
		gtk_file_filter_set_name(filter,"Scalable Vector Graphics");
		title = g_strdup("Select the name of the SVG file");
	}
	else if (we->entry == csv_convW) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.csv");
		gtk_file_filter_set_name(filter,"CSV files");
		title = g_strdup("Select the name of the CSV file");
	}
	else if (we->entry == html_convW) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.html");
		gtk_file_filter_set_name(filter,"Hypertext Markup Language");
		title = g_strdup("Select the name of the HTML report file");
	}

	dialog = xmi_msim_gui_file_chooser_dialog_new(
		title,
		GTK_WINDOW(we->window),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_SAVE,
		GTK_STOCK_CANCEL
	);
	if (filter)
		gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog),filter);

	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		if (we->entry == svg_convW) {
			xmi_msim_gui_utils_ensure_extension(&filename, ".svg");
		}
		else if (we->entry == csv_convW) {
			xmi_msim_gui_utils_ensure_extension(&filename, ".csv");
		}
		else if (we->entry == html_convW) {
			xmi_msim_gui_utils_ensure_extension(&filename, ".html");
		}

		gtk_entry_set_text(GTK_ENTRY(we->entry),filename);
		g_free(filename);

	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);


}

GtkWidget *init_simulation_controls(GtkWidget *window) {

	GtkWidget *superframe;
	GtkWidget *vbox_notebook;
	GtkWidget *frame;
	GtkWidget *hbox_text_label,*label;
	gchar *xmimsim_executable;
	GtkWidget *scrolled_window;
	GtkWidget *button;
	struct window_entry *we;

	GtkWidget *buttonbox;
	GtkWidget *progressbox;
	GtkWidget *hbox_small;
	GtkWidget *hbox_controls;
	GtkWidget *cpuLabel;
	GtkWidget *cpuBox;



	superframe = gtk_vbox_new(FALSE,2);



	//playButton = gtk_button_new_from_stock(GTK_STOCK_MEDIA_PLAY);
	playButton = gtk_button_new();
	g_signal_connect(G_OBJECT(playButton), "clicked",G_CALLBACK(play_button_clicked_cb), window);
	gtk_container_add(GTK_CONTAINER(playButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PLAY,GTK_ICON_SIZE_DIALOG));
	stopButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(stopButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_DIALOG));
	g_signal_connect(G_OBJECT(stopButton), "clicked",G_CALLBACK(stop_button_clicked_cb), window);
#ifdef G_OS_WIN32
	HMODULE ntdll = LoadLibrary( "ntdll.dll" );
	if (ntdll) {
		NtSuspendProcess = (pNtSuspendProcess)GetProcAddress(ntdll, "NtSuspendProcess" );
		NtResumeProcess = (pNtResumeProcess)GetProcAddress(ntdll, "NtResumeProcess" );
		FreeLibrary(ntdll);
		if (NtSuspendProcess != NULL && NtResumeProcess != NULL) {
#endif
			pauseButton = gtk_button_new();
			gtk_container_add(GTK_CONTAINER(pauseButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PAUSE,GTK_ICON_SIZE_DIALOG));
			g_signal_connect(G_OBJECT(pauseButton), "clicked",G_CALLBACK(pause_button_clicked_cb), window);
#ifdef G_OS_WIN32
		}
	}
#endif
	buttonbox = gtk_vbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(buttonbox), playButton, FALSE,FALSE,3);
	if (pauseButton)
		gtk_box_pack_start(GTK_BOX(buttonbox), pauseButton, FALSE,FALSE,3);
	gtk_box_pack_start(GTK_BOX(buttonbox), stopButton, FALSE,FALSE,3);
	if (pauseButton)
		gtk_widget_set_sensitive(pauseButton,FALSE);
	gtk_widget_set_sensitive(stopButton,FALSE);
	hbox_controls = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(hbox_controls), buttonbox, FALSE, FALSE, 5);

	if (xmi_omp_get_max_threads() > 1) {
		cpuBox = gtk_vbox_new(FALSE,1);
		cpuLabel = gtk_label_new("CPUs");
		GtkAdjustment *nthreadsA = GTK_ADJUSTMENT(gtk_adjustment_new((gdouble) xmi_omp_get_max_threads(), 1.0, (gdouble) xmi_omp_get_max_threads(), 1.0,1.0,0.0));
		nthreadsW = gtk_vscale_new(nthreadsA);
		gtk_scale_set_digits(GTK_SCALE(nthreadsW), 0);
		gtk_range_set_inverted(GTK_RANGE(nthreadsW),TRUE);
		gtk_scale_set_value_pos(GTK_SCALE(nthreadsW),GTK_POS_RIGHT);
		//g_signal_connect(G_OBJECT(nthreadsW), "format-value", G_CALLBACK(format_nthreads_cb), NULL);
		gtk_box_pack_start(GTK_BOX(cpuBox), cpuLabel,FALSE,FALSE,2);
		gtk_box_pack_start(GTK_BOX(cpuBox), nthreadsW, TRUE, TRUE,2);
		gtk_box_pack_start(GTK_BOX(hbox_controls), cpuBox, FALSE, FALSE, 10);
	}
	else
		nthreadsW = NULL;

	progressbox = gtk_vbox_new(TRUE,5);

	image_solidW = gtk_alignment_new(0.5,0.5,0,0);
	gtk_container_add(GTK_CONTAINER(image_solidW),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU));
	progressbar_solidW = gtk_progress_bar_new();
	gtk_widget_set_size_request(progressbar_solidW,-1,30);
	gtk_orientable_set_orientation(GTK_ORIENTABLE(progressbar_solidW), GTK_ORIENTATION_HORIZONTAL);
  gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(progressbar_solidW), TRUE);
	hbox_small = gtk_hbox_new(FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),image_solidW,FALSE,FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),progressbar_solidW,TRUE,TRUE,1);
	gtk_box_pack_start(GTK_BOX(progressbox),hbox_small,FALSE,FALSE,1);

	image_mainW = gtk_alignment_new(0.5,0.5,0,0);
	gtk_container_add(GTK_CONTAINER(image_mainW),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU));
	progressbar_mainW = gtk_progress_bar_new();
	gtk_widget_set_size_request(progressbar_mainW,-1,30);
	gtk_orientable_set_orientation(GTK_ORIENTABLE(progressbar_mainW), GTK_ORIENTATION_HORIZONTAL);
  gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(progressbar_mainW), TRUE);
	hbox_small = gtk_hbox_new(FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),image_mainW,FALSE,FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),progressbar_mainW,TRUE,TRUE,1);
	gtk_box_pack_start(GTK_BOX(progressbox),hbox_small,FALSE,FALSE,1);

	image_escapeW = gtk_alignment_new(0.5,0.5,0,0);
	gtk_container_add(GTK_CONTAINER(image_escapeW),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU));
	progressbar_escapeW = gtk_progress_bar_new();
	gtk_widget_set_size_request(progressbar_escapeW,-1,30);
	gtk_orientable_set_orientation(GTK_ORIENTABLE(progressbar_escapeW), GTK_ORIENTATION_HORIZONTAL);
  gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(progressbar_escapeW), TRUE);
	hbox_small = gtk_hbox_new(FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),image_escapeW,FALSE,FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),progressbar_escapeW,TRUE,TRUE,1);
	gtk_box_pack_start(GTK_BOX(progressbox),hbox_small,FALSE,FALSE,1);


	gtk_box_pack_start(GTK_BOX(hbox_controls), progressbox, TRUE, TRUE, 3);

	//OPENMP number of threads!!!!!

	//textbuffer
	controlsLogW = gtk_text_view_new();
	gtk_widget_set_size_request(controlsLogW,350,-1);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(controlsLogW),GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(controlsLogW),3);
	controlsLogB = gtk_text_view_get_buffer(GTK_TEXT_VIEW(controlsLogW));
	gtk_container_set_border_width(GTK_CONTAINER(controlsLogW),2);
	gtk_text_view_set_editable(GTK_TEXT_VIEW(controlsLogW),FALSE);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(controlsLogW),FALSE);
	gtk_text_buffer_create_tag(controlsLogB, "error","foreground","red",NULL);
	gtk_text_buffer_create_tag(controlsLogB, "success","foreground","green",NULL);
	gtk_text_buffer_create_tag(controlsLogB, "pause-continue-stopped","foreground","orange",NULL);
	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), controlsLogW);
	GtkWidget *controls_frameW = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(controls_frameW), scrolled_window);
	gtk_box_pack_start(GTK_BOX(hbox_controls), controls_frameW, TRUE, TRUE, 3);

	gtk_container_set_border_width(GTK_CONTAINER(hbox_controls),10);
	gtk_box_pack_start(GTK_BOX(superframe),hbox_controls, FALSE, FALSE,2);
	frame = gtk_frame_new("Executable");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Executable</span>");


	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);

	//Executable
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Executable");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	executableB = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),executableB,FALSE,FALSE,0);
	g_signal_connect(G_OBJECT(executableB),"clicked",G_CALLBACK(select_executable_cb), (gpointer) window);
#ifdef MAC_INTEGRATION
	if (xmi_resources_mac_query(XMI_RESOURCES_MAC_XMIMSIM_EXEC, &xmimsim_executable) == 0) {
		xmimsim_executable = NULL;
	}
#else
	xmimsim_executable = g_find_program_in_path("xmimsim");
#endif
	executableW = gtk_entry_new();
	if (xmimsim_executable == NULL) {
		//bad...
		gtk_entry_set_text(GTK_ENTRY(executableW),"xmimsim");
	}
	else {
		gtk_entry_set_text(GTK_ENTRY(executableW),xmimsim_executable);
	}
	gtk_editable_set_editable(GTK_EDITABLE(executableW), FALSE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),executableW, TRUE, TRUE,0);
	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);


	//options
	union xmimsim_prefs_val xpv;
	frame = gtk_frame_new("Options");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Options</span>");

	options_boxW = xmi_msim_gui_options_box_new(window);
	gtk_container_add(GTK_CONTAINER(frame), options_boxW);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);

	//Exports
	frame = gtk_frame_new("Export results");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Export results</span>");


	GtkWidget *table_notebook = gtk_table_new(4, 3, FALSE);
	gtk_table_set_row_spacings(GTK_TABLE(table_notebook), 3);
	gtk_table_set_col_spacings(GTK_TABLE(table_notebook), 3);
	gtk_container_set_border_width(GTK_CONTAINER(table_notebook),10);
	//hdf5 file??? too advanced
	//SPE file
	label = gtk_label_new("SPE file prefix");
	gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Setting the prefix will result in the generation of SPE type files containing the spectral data. Compatible with PyMca and AXIL. One file is generated per interaction order.");
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Setting the prefix will result in the generation of SPE type files containing the spectral data. Compatible with PyMca and AXIL. One file is generated per interaction order.");
	spe_convB = button;
	spe_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(spe_convW),"Setting the prefix will result in the generation of SPE type files containing the spectral data. Compatible with PyMca and AXIL. One file is generated per interaction order.");
	we = (struct window_entry *) g_malloc(sizeof(struct window_entry));
	we->entry = spe_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(spe_convW), TRUE);
	gtk_table_attach(GTK_TABLE(table_notebook), label, 0, 1, 0, 1, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);
	gtk_table_attach(GTK_TABLE(table_notebook), spe_convW, 1, 2, 0, 1, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);
	gtk_table_attach(GTK_TABLE(table_notebook), spe_convB, 2, 3, 0, 1, (GtkAttachOptions) 0, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);

	//SVG files
	label = gtk_label_new("Scalable Vector Graphics (SVG) file");
	gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Export the spectra as Scalable Vector Graphics.");
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Export the spectra as Scalable Vector Graphics.");
	svg_convB = button;
	svg_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(svg_convW),"Export the spectra as Scalable Vector Graphics.");
	we = (struct window_entry *) g_malloc(sizeof(struct window_entry));
	we->entry = svg_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(svg_convW), TRUE);
	gtk_table_attach(GTK_TABLE(table_notebook), label    , 0, 1, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);
	gtk_table_attach(GTK_TABLE(table_notebook), svg_convW, 1, 2, 1, 2, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);
	gtk_table_attach(GTK_TABLE(table_notebook), svg_convB, 2, 3, 1, 2, (GtkAttachOptions) 0, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);

	//CSV files
	label = gtk_label_new("Comma Separated Values (CSV) file");
	gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Export the spectra as Comma Separated Values files. Readable by Microsoft Excel and other programs.");
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Export the spectra as Comma Separated Values files. Readable by Microsoft Excel and other programs.");
	csv_convB = button;
	csv_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(csv_convW),"Export the spectra as Comma Separated Values files. Readable by Microsoft Excel and other programs.");
	we = (struct window_entry *) g_malloc(sizeof(struct window_entry));
	we->entry = csv_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(csv_convW), TRUE);
	gtk_table_attach(GTK_TABLE(table_notebook), label    , 0, 1, 2, 3, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);
	gtk_table_attach(GTK_TABLE(table_notebook), csv_convW, 1, 2, 2, 3, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);
	gtk_table_attach(GTK_TABLE(table_notebook), csv_convB, 2, 3, 2, 3, (GtkAttachOptions) 0, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);

	//html files
	label = gtk_label_new("Report HTML file");
	gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Produces an interactive HTML file containing an overview of all the results produced by the simulation: spectra and tables of all the individual XRF lines. Readable with recent versions of Firefox, Chrome and Safari.");
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Produces an interactive HTML file containing an overview of all the results produced by the simulation: spectra and tables of all the individual XRF lines. Readable with recent versions of Firefox, Chrome and Safari.");
	html_convB = button;
	html_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(html_convW),"Produces an interactive HTML file containing an overview of all the results produced by the simulation: spectra and tables of all the individual XRF lines. Readable with recent versions of Firefox, Chrome and Safari.");
	we = (struct window_entry *) g_malloc(sizeof(struct window_entry));
	we->entry = html_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(html_convW), TRUE);
	gtk_table_attach(GTK_TABLE(table_notebook), label     , 0, 1, 3, 4, GTK_FILL, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);
	gtk_table_attach(GTK_TABLE(table_notebook), html_convW, 1, 2, 3, 4, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);
	gtk_table_attach(GTK_TABLE(table_notebook), html_convB, 2, 3, 3, 4, (GtkAttachOptions) 0, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK), 0, 0);




	gtk_container_add(GTK_CONTAINER(frame), table_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);





	//finalize widget
	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), superframe);



	reset_controls();

	return scrolled_window;
}

void reset_controls(void) {
	GtkTextIter start, end;

	//set progressbars to 0 %
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_solidW),"Solid angle grid: 0 %");
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_mainW),"Simulating interactions: 0 %");
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_escapeW),"Escape peak ratios: 0 %");

	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_solidW),0.0);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_mainW),0.0);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_escapeW),0.0);

	//icons
	gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_solidW)));
	gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_mainW)));
	gtk_widget_destroy(gtk_bin_get_child(GTK_BIN(image_escapeW)));
	gtk_container_add(GTK_CONTAINER(image_solidW),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU));
	gtk_container_add(GTK_CONTAINER(image_mainW),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU));
	gtk_container_add(GTK_CONTAINER(image_escapeW),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU));
	gtk_widget_show_all(image_solidW);
	gtk_widget_show_all(image_mainW);
	gtk_widget_show_all(image_escapeW);


	//clear textbuffer
	gtk_text_buffer_get_start_iter (controlsLogB,&start);
	gtk_text_buffer_get_end_iter (controlsLogB,&end);
	gtk_text_buffer_delete (controlsLogB,&start,&end);

	xmimsim_paused = FALSE;
}
