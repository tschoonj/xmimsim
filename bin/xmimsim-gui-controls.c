/*
Copyright (C) 2010-2012 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-controls.h"
#include "xmimsim-gui-results.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-notifications.h"
#include "xmi_aux.h"
#include "xmi_xml.h"
#include "xmi_detector.h"
#include "xmi_data_structs.h"
#include <glib.h>
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




#ifdef MAC_INTEGRATION
#include <gtkosxapplication.h>
#include <xmi_resources_mac.h>
#endif
struct window_entry {
	GtkWidget *window;
	GtkWidget *entry;
};


GtkWidget *executableW;
GtkWidget *executableB;
GtkWidget *MlinesW;
GtkWidget *rad_cascadeW;
GtkWidget *nonrad_cascadeW;
GtkWidget *variance_reductionW;
GtkWidget *pile_upW;
GtkWidget *poissonW;
GtkWidget *escape_peaksW;
GtkWidget *advanced_comptonW;
GtkWidget *default_seedsW;
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
GtkWidget *openclW;
#endif
GtkWidget *custom_detector_responseE;
GtkWidget *custom_detector_responseB;
GtkWidget *custom_detector_responseC;
GtkWidget *spe_convW;
GtkWidget *spe_convB;
GtkWidget *spe_uconvW;
GtkWidget *csv_convW;
GtkWidget *csv_convB;
GtkWidget *csv_uconvW;
GtkWidget *svg_convW;
GtkWidget *svg_convB;
GtkWidget *svg_uconvW;
GtkWidget *html_convW;
GtkWidget *html_convB;
GtkWidget *html_uconvW;

GtkWidget *playButton;
GtkWidget *pauseButton = NULL;
GtkWidget *stopButton;

GtkWidget *controlsLogW;
GtkTextBuffer *controlsLogB;

GtkWidget *progressbar_solidW;
GtkWidget *progressbar_mainW;
GtkWidget *progressbar_escapeW;

GtkWidget *image_solidW;
GtkWidget *image_mainW;
GtkWidget *image_escapeW;

GtkWidget *nthreadsW;
GTimer *timer = NULL;

GIOChannel *xmimsim_stdout;
GIOChannel *xmimsim_stderr;



GPid xmimsim_pid = GPID_INACTIVE;

static gboolean xmimsim_paused = FALSE;

void my_gtk_text_buffer_insert_at_cursor_with_tags(GtkTextBuffer *buffer, const gchar *text, gint len, GtkTextTag *first_tag, ...);
void reset_controls(void);


static void custom_detector_response_toggled_cb(GtkToggleButton *button, gpointer data) {
	if (gtk_toggle_button_get_active(button) == TRUE) {
		gtk_widget_set_sensitive(custom_detector_responseE, TRUE);
		gtk_widget_set_sensitive(custom_detector_responseB, TRUE);
	}
	else {
		gtk_widget_set_sensitive(custom_detector_responseE, FALSE);
		gtk_widget_set_sensitive(custom_detector_responseB, FALSE);
	}
}




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
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			g_error_free(pipe_error);
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			//lot of message handling should come here...
			//for now just print them in the box
			if (process_xmimsim_stdout_string(pipe_string))
				my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, pipe_string,-1,NULL);
			g_free(pipe_string);
		}
		else
			return FALSE;

	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		//buffer = g_strdup_printf("%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		//my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
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
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			g_error_free(pipe_error);
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, pipe_string,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			g_free(pipe_string);
		}
		else
			return FALSE;

	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		//buffer = g_strdup_printf("%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		//my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}

struct child_data {
	char *outputfile;
	gchar **argv;
	GtkWidget *window;
};

static void xmimsim_child_watcher_cb(GPid pid, gint status, struct child_data *cd) {
	char *buffer;
	int success;


	gchar *data = cd->argv[0];


	fprintf(stdout,"xmimsim_child_watcher_cb called with status: %i\n",status);
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
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"success" ),NULL);
			success = 1;
		}
		else {
			buffer = g_strdup_printf("%s with process id %i exited with an error (code: %i)\n",data, real_xmimsim_pid, WEXITSTATUS(status));
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			success = 0;
		}
	}
	else if (WIFSIGNALED(status)) { /* child was terminated due to a signal */
		buffer = g_strdup_printf( "%s with process id %i was terminated by signal %i\n",data, real_xmimsim_pid, WTERMSIG(status));
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		success = 0;
	}
	else {
		buffer = g_strdup_printf( "%s with process id %i was terminated in some special way\n",data, real_xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		success = 0;
	}

#elif defined(G_OS_WIN32)
	if (status == 0) {
		buffer = g_strdup_printf("%s with process id %i exited normally without errors\n", data, real_xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"success" ),NULL);
		success = 1;
	}
	else {
		buffer = g_strdup_printf("%s with process id %i exited with an error (code: %i)\n",data, real_xmimsim_pid, status);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		success = 0;
	}
#endif

	g_spawn_close_pid(xmimsim_pid);
	xmimsim_pid = GPID_INACTIVE;

	//g_strfreev(argv);

	gtk_widget_set_sensitive(playButton,TRUE);
	//make sensitive again
	gtk_widget_set_sensitive(executableW,TRUE);
	gtk_widget_set_sensitive(executableB,TRUE);
	gtk_widget_set_sensitive(MlinesW,TRUE);
	gtk_widget_set_sensitive(rad_cascadeW,TRUE);
	gtk_widget_set_sensitive(nonrad_cascadeW,TRUE);
	gtk_widget_set_sensitive(variance_reductionW,TRUE);
	gtk_widget_set_sensitive(pile_upW,TRUE);
	gtk_widget_set_sensitive(poissonW,TRUE);
	gtk_widget_set_sensitive(escape_peaksW,TRUE);
	gtk_widget_set_sensitive(advanced_comptonW,TRUE);
	gtk_widget_set_sensitive(default_seedsW,TRUE);
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	gtk_widget_set_sensitive(openclW,TRUE);
#endif
	gtk_widget_set_sensitive(custom_detector_responseE, TRUE);
	gtk_widget_set_sensitive(custom_detector_responseB, TRUE);
	gtk_widget_set_sensitive(custom_detector_responseC, TRUE);

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


void my_gtk_text_buffer_insert_at_cursor_with_tags(GtkTextBuffer *buffer, const gchar *text, gint len, GtkTextTag *first_tag, ...) {
	GtkTextIter iter, start;
	va_list args;
	GtkTextTag *tag;
	GtkTextMark *insert_mark;
	gint start_offset;

	g_return_if_fail(GTK_IS_TEXT_BUFFER(buffer));
	g_return_if_fail(text != NULL);

	glong time_elapsed = (glong) g_timer_elapsed(timer,NULL);
	glong hours = time_elapsed / 3600;
	time_elapsed = time_elapsed % 3600;
	glong minutes = time_elapsed / 60;
	glong seconds = time_elapsed % 60;


	gchar *to_print = g_strdup_printf("%02i:%02i:%02i %s",(int) hours, (int) minutes, (int) seconds,text);

	gtk_text_buffer_get_end_iter(buffer, &iter);

	start_offset = gtk_text_iter_get_offset(&iter);
	gtk_text_buffer_insert(buffer, &iter, to_print,len);

	g_free(to_print);

	if (first_tag == NULL) {
		gtk_text_buffer_get_end_iter(buffer, &iter);
		insert_mark = gtk_text_buffer_get_insert(buffer);
		gtk_text_buffer_place_cursor(buffer,&iter);
        	gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (controlsLogW),
	        insert_mark, 0.0, FALSE, 0, 1.0);
		return;
	}

	gtk_text_buffer_get_iter_at_offset (buffer, &start, start_offset);

	va_start(args, first_tag);
	tag = first_tag;
	while (tag) {
		gtk_text_buffer_apply_tag(buffer, tag, &start, &iter);
		tag = va_arg(args, GtkTextTag*);
	}
	va_end(args);

	gtk_text_buffer_get_end_iter(buffer, &iter);
	insert_mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_place_cursor(buffer,&iter);
       	gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (controlsLogW),
	        insert_mark, 0.0, FALSE, 0, 1.0);



	return;
}






void start_job(struct undo_single *xmimsim_struct, GtkWidget *window) {
	gchar **argv;
	gchar *wd;
	gchar *tmp_string;
	gint arg_counter;
	gboolean spawn_rv;
	gint out_fh, err_fh;
	GError *spawn_error = NULL;
	char *buffer;
	const gchar *encoding = NULL;
	struct child_data *cd;

	fprintf(stdout,"Entering start_job\n");



	//freeze gui except for pause and stop buttons
	gtk_widget_set_sensitive(playButton,FALSE);
	gtk_widget_set_sensitive(executableW,FALSE);
	gtk_widget_set_sensitive(executableB,FALSE);
	gtk_widget_set_sensitive(MlinesW,FALSE);
	gtk_widget_set_sensitive(rad_cascadeW,FALSE);
	gtk_widget_set_sensitive(nonrad_cascadeW,FALSE);
	gtk_widget_set_sensitive(variance_reductionW,FALSE);
	gtk_widget_set_sensitive(pile_upW,FALSE);
	gtk_widget_set_sensitive(poissonW,FALSE);
	gtk_widget_set_sensitive(escape_peaksW,FALSE);
	gtk_widget_set_sensitive(advanced_comptonW,FALSE);
	gtk_widget_set_sensitive(default_seedsW,FALSE);
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	gtk_widget_set_sensitive(openclW,FALSE);
#endif
	gtk_widget_set_sensitive(custom_detector_responseE, FALSE);
	gtk_widget_set_sensitive(custom_detector_responseB, FALSE);
	gtk_widget_set_sensitive(custom_detector_responseC, FALSE);
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

	arg_counter = 0;
	argv = (gchar **) g_malloc(sizeof(gchar *)*++arg_counter);
	argv[arg_counter-1] = g_strdup(gtk_entry_get_text(GTK_ENTRY(executableW)));

	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(MlinesW)) == TRUE) {
		argv[arg_counter-1] = g_strdup("--enable-M-lines");
	}
	else
		argv[arg_counter-1] = g_strdup("--disable-M-lines");

	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(rad_cascadeW)) == TRUE) {
		argv[arg_counter-1] = g_strdup("--enable-radiative-cascade");
	}
	else
		argv[arg_counter-1] = g_strdup("--disable-radiative-cascade");

	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(nonrad_cascadeW)) == TRUE) {
		argv[arg_counter-1] = g_strdup("--enable-auger-cascade");
	}
	else
		argv[arg_counter-1] = g_strdup("--disable-auger-cascade");

	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(variance_reductionW)) == TRUE) {
		argv[arg_counter-1] = g_strdup("--enable-variance-reduction");
	}
	else
		argv[arg_counter-1] = g_strdup("--disable-variance-reduction");

	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pile_upW)) == TRUE) {
		argv[arg_counter-1] = g_strdup("--enable-pile-up");
	}
	else
		argv[arg_counter-1] = g_strdup("--disable-pile-up");

	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(poissonW)) == TRUE) {
		argv[arg_counter-1] = g_strdup("--enable-poisson");
	}
	else
		argv[arg_counter-1] = g_strdup("--disable-poisson");

	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(escape_peaksW)) == TRUE) {
		argv[arg_counter-1] = g_strdup("--enable-escape-peaks");
	}
	else
		argv[arg_counter-1] = g_strdup("--disable-escape-peaks");

	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(advanced_comptonW)) == TRUE) {
		argv[arg_counter-1] = g_strdup("--enable-advanced-compton");
	}
	else
		argv[arg_counter-1] = g_strdup("--disable-advanced-compton");

	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(default_seedsW)) == TRUE) {
		argv[arg_counter-1] = g_strdup("--enable-default-seeds");
	}
	else
		argv[arg_counter-1] = g_strdup("--disable-default-seeds");



	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	argv[arg_counter-1] = g_strdup("--verbose");

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(openclW)) == TRUE) {
		argv[arg_counter-1] = g_strdup("--enable-opencl");
	}
	else
		argv[arg_counter-1] = g_strdup("--disable-opencl");
#endif

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(custom_detector_responseC)) == TRUE &&
		strlen(gtk_entry_get_text(GTK_ENTRY(custom_detector_responseE))) > 0) {
		argv = (gchar **) g_realloc(argv, sizeof(gchar *)*++arg_counter);
		argv[arg_counter-1] = g_strdup_printf("--custom-detector-response=%s", gtk_entry_get_text(GTK_ENTRY(custom_detector_responseE)));
	}

	tmp_string = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(spe_convW))));
	if (strlen(tmp_string) > 0) {
		argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
		argv[arg_counter] = g_strdup_printf("--spe-file=%s",tmp_string);
		arg_counter++;
	}
	g_free(tmp_string);

	tmp_string = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(csv_convW))));
	if (strlen(tmp_string) > 0) {
		argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
		argv[arg_counter] = g_strdup_printf("--csv-file=%s",tmp_string);
		arg_counter++;
	}
	g_free(tmp_string);

	tmp_string = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(svg_convW))));
	if (strlen(tmp_string) > 0) {
		argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
		argv[arg_counter] = g_strdup_printf("--svg-file=%s",tmp_string);
		arg_counter++;
	}
	g_free(tmp_string);

	tmp_string = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(html_convW))));
	if (strlen(tmp_string) > 0) {
		argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
		argv[arg_counter] = g_strdup_printf("--htm-file=%s",tmp_string);
		arg_counter++;
	}
	g_free(tmp_string);

#ifdef G_OS_WIN32
	//set solid angles and escape ratios files ourself!
	char *xmimsim_hdf5_solid_angles = NULL;
	char *xmimsim_hdf5_escape_ratios = NULL;

	if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0) {
		buffer = g_strdup_printf("Could not determine solid angles HDF5 file\n");
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		gtk_widget_set_sensitive(playButton,TRUE);
		return;
	}
	argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
	argv[arg_counter] = g_strdup_printf("--with-solid-angles-data=%s",xmimsim_hdf5_solid_angles);
	arg_counter++;

	if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0) {
		buffer = g_strdup_printf("Could not determine escape ratios HDF5 file\n");
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		gtk_widget_set_sensitive(playButton,TRUE);
		return;
	}
	argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
	argv[arg_counter] = g_strdup_printf("--with-escape-ratios-data=%s",xmimsim_hdf5_escape_ratios);
	arg_counter++;
#endif



	//number of threads
	if (nthreadsW != NULL) {
		argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
		argv[arg_counter++] = g_strdup_printf("--set-threads=%i",(int) gtk_range_get_value(GTK_RANGE(nthreadsW)));
	}

	argv[arg_counter++] = g_strdup(xmimsim_struct->filename);
	argv[arg_counter] = NULL;

	wd = g_path_get_dirname(xmimsim_struct->xi->general->outputfile);
	//execute command
	spawn_rv = g_spawn_async_with_pipes(wd, argv, NULL, G_SPAWN_DO_NOT_REAP_CHILD, NULL, NULL,
		&xmimsim_pid, NULL, &out_fh, &err_fh, &spawn_error);

	// print fds for debugging purposes
	fprintf(stdout, "out_fh: %d\n", out_fh);
	fprintf(stdout, "err_fh: %d\n", err_fh);


	if (spawn_rv == FALSE) {
		//couldn't spawn
		//print messag_ to textbox in red...
		buffer = g_strdup_printf("%s\n",spawn_error->message);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		g_error_free(spawn_error);
		gtk_widget_set_sensitive(playButton,TRUE);
		xmimsim_pid = (GPid) -1;
		return;
	}
	gchar *command = g_strjoinv(" ", argv);
	buffer = g_strdup_printf("%s was started with process id %i\n", command, real_xmimsim_pid);
	my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,NULL);
	g_free(wd);
	g_free(command);

	xmimsim_paused=FALSE;
	if (pauseButton)
		gtk_widget_set_sensitive(pauseButton,TRUE);
	gtk_widget_set_sensitive(stopButton,TRUE);

	cd = (struct child_data *) g_malloc(sizeof(struct child_data));
	cd->outputfile = g_strdup(xmimsim_struct->xi->general->outputfile);
	cd->argv = argv;
	cd->window = window;

#ifdef G_OS_WIN32
	fprintf(stdout, "out_fh before channel: %d\n", out_fh);
	fprintf(stdout, "err_fh before channel: %d\n", err_fh);
	xmimsim_stderr= g_io_channel_win32_new_fd(err_fh);
	xmimsim_stdout = g_io_channel_win32_new_fd(out_fh);
#else
	xmimsim_stderr= g_io_channel_unix_new(err_fh);
	xmimsim_stdout = g_io_channel_unix_new(out_fh);
#endif

	g_child_watch_add(xmimsim_pid,(GChildWatchFunc) xmimsim_child_watcher_cb, (gpointer) cd);


	g_get_charset(&encoding);

	g_io_channel_set_encoding(xmimsim_stdout, encoding, NULL);
	//g_io_channel_set_flags(xmimsim_stdout,G_IO_FLAG_NONBLOCK,NULL);
	g_io_channel_set_close_on_unref(xmimsim_stdout,TRUE);
	g_io_add_watch(xmimsim_stdout, (GIOCondition) (G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL), xmimsim_stdout_watcher, argv[0]);
	g_io_channel_unref(xmimsim_stdout);

	g_io_channel_set_encoding(xmimsim_stderr, encoding, NULL);
	g_io_channel_set_close_on_unref(xmimsim_stderr,TRUE);
	g_io_add_watch(xmimsim_stderr, (GIOCondition) (G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL), xmimsim_stderr_watcher, argv[0]);
	g_io_channel_unref(xmimsim_stderr);

	//while (gtk_events_pending ())
	//        gtk_main_iteration ();


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
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
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
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
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

	fprintf(stdout,"stop_button_clicked_cb entered\n");
	kill_rv = kill((pid_t) xmimsim_pid, SIGTERM);
#if !GLIB_CHECK_VERSION (2, 35, 0)
	//starting with 2.36.0 (and some unstable versions before),
	//waitpid is called from within the main loop
	//causing all kinds of trouble if I would call wait here
	//wait(NULL);
	waitpid(xmimsim_pid, NULL, WNOHANG);
#endif
	fprintf(stdout,"stop_button_clicked_cb kill: %i\n",kill_rv);
	if (kill_rv == 0) {
		buffer = g_strdup_printf( "Process %i was successfully terminated before completion\n", real_xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		buffer = g_strdup_printf( "Process %i could not be terminated with the SIGTERM signal\n", real_xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
	}

#elif defined(G_OS_WIN32)
	BOOL terminate_rv;

	terminate_rv = TerminateProcess((HANDLE) xmimsim_pid, (UINT) 1);

	if (terminate_rv == TRUE) {
		buffer = g_strdup_printf( "Process %i was successfully terminated\n", real_xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		buffer = g_strdup_printf( "Process %i could not be terminated with the TerminateProcess call\n", real_xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
	}
#endif

	error_spinners();




	fprintf(stdout,"stop_button_clicked_cb exited\n");
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

	fprintf(stdout,"play_button_clicked_cb\n");

	fprintf(stdout,"paused: %i\n",xmimsim_paused);

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
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
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
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
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
	GtkWidget *dialog;
	GtkFileFilter *filter;
	gchar *filename;


	filter = gtk_file_filter_new();
	gtk_file_filter_add_custom(filter, GTK_FILE_FILTER_FILENAME, executable_file_filter, NULL, NULL);
	gtk_file_filter_set_name(filter,"Executables");
	dialog = gtk_file_chooser_dialog_new ("Open simulation executable",
		GTK_WINDOW((GtkWidget *) data),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
		NULL);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gtk_entry_set_text(GTK_ENTRY(executableW),filename);
		g_free(filename);
	}
	gtk_widget_destroy(dialog);

}

static void select_extra_output_cb(GtkButton *button, gpointer data) {
	GtkWidget *dialog;
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

	dialog = gtk_file_chooser_dialog_new(title,
		GTK_WINDOW(we->window),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL
	);
	if (filter)
		gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog),filter);

	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		if (we->entry == svg_convW) {
			xmi_msim_gui_ensure_extension(&filename, ".svg");
		}
		else if (we->entry == csv_convW) {
			xmi_msim_gui_ensure_extension(&filename, ".csv");
		}
		else if (we->entry == html_convW) {
			xmi_msim_gui_ensure_extension(&filename, ".html");
		}

		gtk_entry_set_text(GTK_ENTRY(we->entry),filename);
		g_free(filename);

	}
	gtk_widget_destroy(dialog);


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
#if GTK_MAJOR_VERSION == 3
	gtk_orientable_set_orientation(GTK_ORIENTABLE(progressbar_solidW), GTK_ORIENTATION_HORIZONTAL);
  gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(progressbar_solidW), TRUE);
#else
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(progressbar_solidW), GTK_PROGRESS_LEFT_TO_RIGHT);
#endif
	hbox_small = gtk_hbox_new(FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),image_solidW,FALSE,FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),progressbar_solidW,TRUE,TRUE,1);
	gtk_box_pack_start(GTK_BOX(progressbox),hbox_small,FALSE,FALSE,1);

	image_mainW = gtk_alignment_new(0.5,0.5,0,0);
	gtk_container_add(GTK_CONTAINER(image_mainW),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU));
	progressbar_mainW = gtk_progress_bar_new();
	gtk_widget_set_size_request(progressbar_mainW,-1,30);
#if GTK_MAJOR_VERSION == 3
	gtk_orientable_set_orientation(GTK_ORIENTABLE(progressbar_mainW), GTK_ORIENTATION_HORIZONTAL);
  gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(progressbar_mainW), TRUE);
#else
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(progressbar_mainW), GTK_PROGRESS_LEFT_TO_RIGHT);
#endif
	hbox_small = gtk_hbox_new(FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),image_mainW,FALSE,FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),progressbar_mainW,TRUE,TRUE,1);
	gtk_box_pack_start(GTK_BOX(progressbox),hbox_small,FALSE,FALSE,1);

	image_escapeW = gtk_alignment_new(0.5,0.5,0,0);
	gtk_container_add(GTK_CONTAINER(image_escapeW),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU));
	progressbar_escapeW = gtk_progress_bar_new();
	gtk_widget_set_size_request(progressbar_escapeW,-1,30);
#if GTK_MAJOR_VERSION == 3
	gtk_orientable_set_orientation(GTK_ORIENTABLE(progressbar_escapeW), GTK_ORIENTATION_HORIZONTAL);
  gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(progressbar_escapeW), TRUE);
#else
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(progressbar_escapeW), GTK_PROGRESS_LEFT_TO_RIGHT);
#endif
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
	gtk_box_pack_start(GTK_BOX(hbox_controls), scrolled_window, TRUE, TRUE, 3);


	gtk_container_set_border_width(GTK_CONTAINER(hbox_controls),10);
	gtk_box_pack_start(GTK_BOX(superframe),hbox_controls, FALSE, FALSE,2);
	frame = gtk_frame_new("Executable");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
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
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Options</span>");


	vbox_notebook = gtk_vbox_new(TRUE,0);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);
	MlinesW = gtk_check_button_new_with_label("Simulate M-lines");
	gtk_widget_set_tooltip_text(MlinesW,"Enables the simulation of M-lines. Disabling this option may lead to a significant performance increase. Should always be enabled when high atomic number elements are present in the sample.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_M_LINES, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(MlinesW),xpv.b);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),MlinesW, TRUE, FALSE, 0);

	rad_cascadeW = gtk_check_button_new_with_label("Simulate the radiative cascade effect");
	gtk_widget_set_tooltip_text(rad_cascadeW,"Enables the simulation of the radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_RAD_CASCADE, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rad_cascadeW),xpv.b);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),rad_cascadeW, TRUE, FALSE, 0);

	nonrad_cascadeW = gtk_check_button_new_with_label("Simulate the non-radiative cascade effect");
	gtk_widget_set_tooltip_text(nonrad_cascadeW,"Enables the simulation of the non-radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the non-radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NONRAD_CASCADE, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(nonrad_cascadeW),xpv.b);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),nonrad_cascadeW, TRUE, FALSE, 0);

	variance_reductionW = gtk_check_button_new_with_label("Enable variance reduction techniques");
	gtk_widget_set_tooltip_text(variance_reductionW,"Disabling this option enables the brute-force method. Should only be used in combination with a high number of simulated photons.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(variance_reductionW), xpv.b);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),variance_reductionW, TRUE, FALSE, 0);

	pile_upW = gtk_check_button_new_with_label("Enable pulse pile-up simulation");
	gtk_widget_set_tooltip_text(pile_upW,"When activated, will estimate detector electronics pulse pile-up. Determined by the pulse width parameter in Detector settings.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_PILE_UP, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pile_upW),xpv.b);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),pile_upW, TRUE, FALSE, 0);

	poissonW = gtk_check_button_new_with_label("Enable Poisson noise generation");
	gtk_widget_set_tooltip_text(poissonW,"Enabling this feature will add noise according to a Poisson distribution to the convoluted spectra");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_POISSON, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(poissonW),xpv.b);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),poissonW, TRUE, FALSE, 0);

	escape_peaksW = gtk_check_button_new_with_label("Enable escape peaks support");
	gtk_widget_set_tooltip_text(escape_peaksW,"Enabling this feature will add fluorescence and Compton escape peaks to the convoluted spectra");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_ESCAPE_PEAKS, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(escape_peaksW),xpv.b);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), escape_peaksW, TRUE, FALSE, 0);

	advanced_comptonW = gtk_check_button_new_with_label("Enable advanced Compton scattering simulation");
	gtk_widget_set_tooltip_text(advanced_comptonW, "Enabling this feature will improve the simulation of the Compton scattering, and add support for the Compton fluorescence photons. Warning: due to the added complexity, the code will slow down considerably (at least a factor of 2, and increases with higher atomic number)");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_ADVANCED_COMPTON, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(advanced_comptonW),xpv.b);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), advanced_comptonW, TRUE, FALSE, 0);

	default_seedsW = gtk_check_button_new_with_label("Enable default seeds support");
	gtk_widget_set_tooltip_text(default_seedsW, "Enabling this feature will set the seeds that will be used during the simulation to default values, instead of random ones. This is useful when exactly reproducible simulation results are required, usually for testing purposes");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_DEFAULT_SEEDS, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(default_seedsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), default_seedsW, TRUE, FALSE, 0);

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	openclW = gtk_check_button_new_with_label("Enable OpenCL");
	gtk_widget_set_tooltip_text(openclW,"Enabling OpenCL will have the simulation use the GPU in order to calculate the solid angle grids, resulting in considerably speed-up. Requires the installation of OpenCL drivers. Consult the website of the manufacturer of your videocard for more information");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_OPENCL, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(openclW),xpv.b);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),openclW, TRUE, FALSE, 0);
#endif


	GtkWidget *hbox = gtk_hbox_new(FALSE, 0);
	custom_detector_responseC = gtk_check_button_new_with_label("Custom detector response");
	gtk_widget_set_tooltip_text(custom_detector_responseC, "Loads an alternative detector response routine from a dynamically loadable module. This module must export a function called \"xmi_detector_convolute_all_custom\". More information can be found in the manual");
	g_signal_connect(G_OBJECT(custom_detector_responseC), "toggled", G_CALLBACK(custom_detector_response_toggled_cb), NULL);
	gtk_box_pack_start(GTK_BOX(hbox), custom_detector_responseC, FALSE, FALSE, 0);
	custom_detector_responseE = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(custom_detector_responseE), FALSE);
	gtk_box_pack_start(GTK_BOX(hbox), custom_detector_responseE, TRUE, TRUE, 3);
	custom_detector_responseB = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	g_signal_connect(G_OBJECT(custom_detector_responseB), "clicked", G_CALLBACK(custom_detector_response_clicked_cb), custom_detector_responseE);
	gtk_box_pack_end(GTK_BOX(hbox), custom_detector_responseB, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox, TRUE, FALSE, 3);
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_CUSTOM_DETECTOR_RESPONSE, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	if (xpv.s != NULL) {
		gtk_widget_set_sensitive(custom_detector_responseE, TRUE);
		gtk_widget_set_sensitive(custom_detector_responseB, TRUE);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(custom_detector_responseC), TRUE);
		gtk_entry_set_text(GTK_ENTRY(custom_detector_responseE), xpv.s);
	}
	else {
		gtk_widget_set_sensitive(custom_detector_responseE, FALSE);
		gtk_widget_set_sensitive(custom_detector_responseB, FALSE);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(custom_detector_responseC), FALSE);
	}
	g_free(xpv.s);




	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);

	//Exports
	frame = gtk_frame_new("Export results");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
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
