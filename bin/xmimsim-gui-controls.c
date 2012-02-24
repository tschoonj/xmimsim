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

#include "xmimsim-gui.h"
#include "xmimsim-gui-controls.h"
#include "xmi_xml.h"
#include "xmi_data_structs.h"
#include <glib.h>
#include <string.h>
#include <stdlib.h>
#ifdef G_OS_UNIX
  #include <sys/types.h>
  #include <sys/wait.h>
  #include <signal.h>
#elif defined(G_OS_WIN32)
  #include <windows.h>
#endif

struct window_entry {
	GtkWidget *window;
	GtkWidget *entry;
};


GtkWidget *executableW;
GtkWidget *MlinesW;
GtkWidget *rad_cascadeW;
GtkWidget *nonrad_cascadeW;
GtkWidget *variance_reductionW;
GtkWidget *pile_upW;
GtkWidget *spe_convW;
GtkWidget *spe_uconvW;
GtkWidget *csv_convW;
GtkWidget *csv_uconvW;
GtkWidget *svg_convW;
GtkWidget *svg_uconvW;
GtkWidget *html_convW;
GtkWidget *html_uconvW;

GtkWidget *playButton;
GtkWidget *pauseButton;
GtkWidget *stopButton;

GtkWidget *controlsLogW;
GtkTextBuffer *controlsLogB;

GtkWidget *progressbar_solidW;
GtkWidget *progressbar_mainW;
GtkWidget *progressbar_escapeW;

GtkWidget *image_solidW;
GtkWidget *image_mainW;
GtkWidget *image_escapeW;

GPid xmimsim_pid;

gboolean xmimsim_paused;

void my_gtk_text_buffer_insert_at_cursor_with_tags(GtkTextBuffer *buffer, const gchar *text, gint len, GtkTextTag *first_tag, ...);
void reset_controls(void);

static gboolean executable_file_filter(const GtkFileFilterInfo *filter_info, gpointer data) {
	return g_file_test(filter_info->filename,G_FILE_TEST_IS_EXECUTABLE);
}



static gboolean xmimsim_stdout_watcher(GIOChannel *source, GIOCondition condition, gpointer data) {
	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	char buffer[512];

	if (condition & (G_IO_IN|G_IO_PRI)) {
		/*while (gtk_events_pending ())
		        gtk_main_iteration ();*/
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);	
		if (pipe_status == G_IO_STATUS_ERROR) {
			sprintf(buffer,"%s with process id %i had an I/O error: %s\n",(char *) data, (int) xmimsim_pid,pipe_error->message);
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			g_error_free(pipe_error);
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			//lot of message handling should come here...
			//for now just print them in the box
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, pipe_string,-1,NULL);
			g_free(pipe_string);
		}
		else
			return FALSE;

	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		sprintf(buffer,"%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}


static gboolean xmimsim_stderr_watcher(GIOChannel *source, GIOCondition condition, gpointer data) {
	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	char buffer[512];

	if (condition & (G_IO_IN|G_IO_PRI)) {
		/*while (gtk_events_pending ())
		        gtk_main_iteration ();*/
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);	
		if (pipe_status == G_IO_STATUS_ERROR) {
			sprintf(buffer,"%s with process id %i had an I/O error: %s\n",(char *) data, (int) xmimsim_pid,pipe_error->message);
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
		sprintf(buffer,"%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}

static void xmimsim_child_watcher_cb(GPid pid, gint status, gpointer data) {
	char buffer[512];


	fprintf(stdout,"xmimsim_child_watcher_cb called\n");

	//windows <-> unix issues here
	//unix allows to obtain more info about the way the process was terminated, windows will just have the exit code (status)
	//conditional compilation here
#ifdef G_OS_UNIX
	if (WIFEXITED(status)) {
		if (WEXITSTATUS(status) == 0) { /* child was terminated due to a call to exit */
			sprintf(buffer,"%s with process id %i exited normally without errors\n", (char *)data, (int) xmimsim_pid);
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"success" ),NULL);
		}
		else {
			sprintf(buffer,"%s with process id %i exited with an error (code: %i)\n",(char *)data, (int) xmimsim_pid, WEXITSTATUS(status));
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		}
	}
	else if (WIFSIGNALED(status)) { /* child was terminated due to a signal */
		sprintf(buffer, "%s with process id %i was terminated by signal %i\n",(char *)data, (int) xmimsim_pid, WTERMSIG(status));
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
	}
	else {
		sprintf(buffer, "%s with process id %i was terminated in some special way\n",(char *)data, (int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
	}

#elif defined(G_OS_WIN32)
	if (status == 0) {
		sprintf(buffer,"%s with process id %i exited normally without errors\n", (char *)data, (int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"success" ),NULL);
	}
	else {
		sprintf(buffer,"%s with process id %i exited with an error (code: %i)\n",(char *)data, (int) xmimsim_pid, status);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
	}
#endif

	g_spawn_close_pid(xmimsim_pid);



	gtk_widget_set_sensitive(playButton,TRUE);
	gtk_widget_set_sensitive(pauseButton,FALSE);
	gtk_widget_set_sensitive(stopButton,FALSE);

	//read in outputfile and put it on the results page
	fprintf(stdout,"end of xmimsim_child_watcher_cb reached\n");


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

	gtk_text_buffer_get_end_iter(buffer, &iter);

	start_offset = gtk_text_iter_get_offset(&iter);
	gtk_text_buffer_insert(buffer, &iter, text,len);

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


void start_job() {
	gchar *argv[] = {"/usr/local/bin/xmimsim-gui-helper",NULL};
	gboolean spawn_rv;
	gint out_fh, err_fh;
	GError *spawn_error = NULL;
	char buffer[512];
	const gchar *encoding = NULL;
	GIOChannel *xmimsim_stdout;
	GIOChannel *xmimsim_stderr;


	fprintf(stdout,"Entering start_job\n");


	//freeze gui except for pause and stop buttons
	gtk_widget_set_sensitive(playButton,FALSE);

	reset_controls();

	//construct command


	//execute command
	spawn_rv = g_spawn_async_with_pipes(NULL, argv, NULL, G_SPAWN_DO_NOT_REAP_CHILD, NULL, NULL,
		&xmimsim_pid, NULL, &out_fh, &err_fh, &spawn_error);


	if (spawn_rv == FALSE) {
		//couldn't spawn
		//print messag_ to textbox in red...
		sprintf(buffer,"%s\n",spawn_error->message);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		g_error_free(spawn_error);
		gtk_widget_set_sensitive(playButton,TRUE);
		return;
	}
	sprintf(buffer,"%s was started with process id %i\n",argv[0],(int)xmimsim_pid);
	my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,NULL);

	xmimsim_paused=FALSE;
	gtk_widget_set_sensitive(pauseButton,TRUE);
	gtk_widget_set_sensitive(stopButton,TRUE);

	g_child_watch_add(xmimsim_pid,xmimsim_child_watcher_cb, argv[0]);
	
#ifdef G_OS_WIN32
	xmimsim_stderr= g_io_channel_win32_new_fd(err_fh);
	xmimsim_stdout = g_io_channel_win32_new_fd(out_fh);
#else
	xmimsim_stderr= g_io_channel_unix_new(err_fh);
	xmimsim_stdout = g_io_channel_unix_new(out_fh);
#endif

	g_get_charset(&encoding);

	g_io_channel_set_encoding(xmimsim_stdout, encoding, NULL);
	//g_io_channel_set_flags(xmimsim_stdout,G_IO_FLAG_NONBLOCK,NULL);
	g_io_channel_set_close_on_unref(xmimsim_stdout,TRUE);
	g_io_add_watch(xmimsim_stdout, G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL, xmimsim_stdout_watcher, argv[0]);
	g_io_channel_unref(xmimsim_stdout);

	g_io_channel_set_encoding(xmimsim_stderr, encoding, NULL);
	g_io_channel_set_close_on_unref(xmimsim_stderr,TRUE);
	g_io_add_watch(xmimsim_stderr, G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL, xmimsim_stderr_watcher, argv[0]);
	g_io_channel_unref(xmimsim_stderr);

	//while (gtk_events_pending ())
	//        gtk_main_iteration ();

	
}

#ifdef G_OS_UNIX
static void pause_button_clicked_cb(GtkWidget *widget, gpointer data) {
	//UNIX only
	
	int kill_rv;
	char buffer[512];

	gtk_widget_set_sensitive(pauseButton,FALSE);
	gtk_widget_set_sensitive(stopButton,FALSE);
	kill_rv = kill((pid_t) xmimsim_pid, SIGSTOP);
	if (kill_rv == 0) {
		sprintf(buffer, "Process %i was successfully paused. Press the Play button to continueor Stop to kill the process\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
		xmimsim_paused=TRUE;
		gtk_widget_set_sensitive(stopButton,TRUE);
		gtk_widget_set_sensitive(playButton,TRUE);
	}
	else {
		sprintf(buffer, "Process %i could not be paused.\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		xmimsim_paused=FALSE;
		gtk_widget_set_sensitive(pauseButton,TRUE);
		gtk_widget_set_sensitive(stopButton,TRUE);
	}
	

}
#endif
static void stop_button_clicked_cb(GtkWidget *widget, gpointer data) {
	//difference UNIX <-> Windows
	//UNIX -> send sigkill signal
	//Windows -> TerminateProcess
	char buffer[512];

	//g_io_channel_unref(xmimsim_stdout);
	//g_io_channel_unref(xmimsim_stderr);

	//set buttons back in order
	gtk_widget_set_sensitive(playButton,TRUE);
	gtk_widget_set_sensitive(stopButton,FALSE);
	gtk_widget_set_sensitive(pauseButton,FALSE);
	xmimsim_paused = FALSE;
#ifdef G_OS_UNIX
	int kill_rv;
	
	fprintf(stdout,"stop_button_clicked_cb entered\n");
	kill_rv = kill((pid_t) xmimsim_pid, SIGTERM);
	wait(NULL);
	fprintf(stdout,"stop_button_clicked_cb kill: %i\n",kill_rv);
	if (kill_rv == 0) {
		sprintf(buffer, "Process %i was successfully terminated before completion\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		sprintf(buffer, "Process %i could not be terminated with the SIGTERM signal\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
	}

#elif defined(G_OS_WIN32)
	BOOL terminate_rv;

	terminate_rv = TerminateProcess((HANDLE) xmimsim_pid, (UINT) 1);

	if (terminate_rv == TRUE) {
		sprintf(buffer, "Process %i was successfully terminated\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		sprintf(buffer, "Process %i could not be terminated with the TerminateProcess call\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
	}
#endif

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

	fprintf(stdout,"play_button_clicked_cb\n");

	fprintf(stdout,"paused: %i\n",xmimsim_paused);

#ifdef G_OS_UNIX
	if (xmimsim_paused == TRUE) {
		gtk_widget_set_sensitive(playButton,FALSE);
		//send SIGCONT	
		int kill_rv;
		char buffer[512];

		kill_rv = kill((pid_t) xmimsim_pid, SIGCONT);
		if (kill_rv == 0) {
			sprintf(buffer, "Process %i was successfully resumed\n",(int) xmimsim_pid);
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"pause-continue-stopped" ),NULL);
			gtk_widget_set_sensitive(pauseButton,TRUE);
			xmimsim_paused = FALSE;
		}
		else {
			sprintf(buffer, "Process %i could not be resumed\n",(int) xmimsim_pid);
			my_gtk_text_buffer_insert_at_cursor_with_tags(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
			gtk_widget_set_sensitive(playButton,TRUE);
		}
		return;
	}
#endif

	xmimsim_paused = FALSE;

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
					free(current->xi->general->comments);
					current->xi->general->comments = strdup("");
				}
				else {
					free(current->xi->general->comments);
					current->xi->general->comments = strdup(gtk_text_buffer_get_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW)),&iterb, &itere, FALSE));
				}
				//update file
				if (xmi_write_input_xml(check_rv->filename, current->xi) == 1) {

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
				start_job();
				break;



		}
	}
	else if (check_status == CHECK_CHANGES_JUST_SAVED) {
		//current version corresponds to saved version
		//launch simulation...
		start_job();
	}
	else if (check_status == CHECK_CHANGES_NEVER_SAVED) {
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
	gchar title[512];

	if (we->entry == spe_convW) {
		//gtk_file_filter_add_pattern(filter,"*.spe");
		//gtk_file_filter_set_name(filter,"SPE spectral data files");
		strcpy(title,"Select the prefix of the SPE files");
	}
	else if (we->entry == svg_convW) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.svg");
		gtk_file_filter_set_name(filter,"Scalable Vector Graphics");
		strcpy(title,"Select the name of the SVG file");
	}
	else if (we->entry == csv_convW) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.csv");
		gtk_file_filter_set_name(filter,"Comma separated value files");
		strcpy(title,"Select the name of the CSV file");
	}
	else if (we->entry == html_convW) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.html");
		gtk_file_filter_set_name(filter,"Hypertext Markup Language");
		strcpy(title,"Select the name of the HTML report file");
	}
	
	dialog = gtk_file_chooser_dialog_new(title,
		GTK_WINDOW(we->window),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL
	);
	if (filter)
		gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog),filter);


	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		if (we->entry == svg_convW) {
			if (strcmp(filename+strlen(filename)-4, ".svg") != 0) {
				filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+5));
				strcat(filename,".svg");
			}
		}
		else if (we->entry == csv_convW) {
			if (strcmp(filename+strlen(filename)-4, ".csv") != 0) {
				filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+5));
				strcat(filename,".csv");
			}
		}
		else if (we->entry == html_convW) {
			if (strcmp(filename+strlen(filename)-5, ".html") != 0) {
				filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+6));
				strcat(filename,".html");
			}
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



	superframe = gtk_vbox_new(FALSE,2);

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
	label = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	g_signal_connect(G_OBJECT(label),"clicked",G_CALLBACK(select_executable_cb), (gpointer) window);
	xmimsim_executable = g_find_program_in_path("xmimsim");
	executableW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(executableW),80);
	if (xmimsim_executable == NULL) {
		//bad...
		gtk_entry_set_text(GTK_ENTRY(executableW),"xmimsim");
	}
	else {
		gtk_entry_set_text(GTK_ENTRY(executableW),xmimsim_executable);
	}
	gtk_editable_set_editable(GTK_EDITABLE(executableW), FALSE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),executableW,FALSE,FALSE,0);
	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);


	//options
	frame = gtk_frame_new("Options");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Options</span>");


	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);
	MlinesW = gtk_check_button_new_with_label("Simulate M-lines");
	gtk_widget_set_tooltip_text(MlinesW,"Enables the simulation of M-lines. Disabling this option may lead to a significant performance increase. Should always be enabled when high atomic number elements are present in the sample.");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(MlinesW),TRUE);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),MlinesW, TRUE, FALSE, 3);

	rad_cascadeW = gtk_check_button_new_with_label("Simulate the radiative cascade effect");
	gtk_widget_set_tooltip_text(rad_cascadeW,"Enables the simulation of the radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the radiative cascade effect.");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rad_cascadeW),TRUE);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),rad_cascadeW, TRUE, FALSE, 3);

	nonrad_cascadeW = gtk_check_button_new_with_label("Simulate the non-radiative cascade effect");
	gtk_widget_set_tooltip_text(nonrad_cascadeW,"Enables the simulation of the non-radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the non-radiative cascade effect.");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(nonrad_cascadeW),TRUE);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),nonrad_cascadeW, TRUE, FALSE, 3);

	variance_reductionW = gtk_check_button_new_with_label("Enable variance reduction techniques");
	gtk_widget_set_tooltip_text(variance_reductionW,"Disabling this option enables the brute-force method. Should only be used in combination with a high number of simulated photons.");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(variance_reductionW),TRUE);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),variance_reductionW, TRUE, FALSE, 3);

	pile_upW = gtk_check_button_new_with_label("Enable pulse pile-up simulation");
	gtk_widget_set_tooltip_text(pile_upW,"When activated, will estimate detector electronics pulse pile-up. Determined by the pulse width parameter in Detector settings.");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pile_upW),FALSE);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),pile_upW, TRUE, FALSE, 3);
	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);

	//Exports
	frame = gtk_frame_new("Export results");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Export results</span>");


	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);
	//hdf5 file??? too advanced
	//SPE file
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("SPE file prefix");
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Setting the prefix will result in the generation of SPE type files containing the spectral data. Compatible with PyMca and AXIL. One file is generated per interaction order.");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Setting the prefix will result in the generation of SPE type files containing the spectral data. Compatible with PyMca and AXIL. One file is generated per interaction order.");
	gtk_box_pack_end(GTK_BOX(hbox_text_label),button,FALSE,FALSE,0);
	spe_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(spe_convW),"Setting the prefix will result in the generation of SPE type files containing the spectral data. Compatible with PyMca and AXIL. One file is generated per interaction order.");
	gtk_entry_set_width_chars(GTK_ENTRY(spe_convW),60);
	we = (struct window_entry *) malloc(sizeof(struct window_entry));
	we->entry = spe_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(spe_convW), TRUE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),spe_convW,FALSE,FALSE,0);

	//SVG files
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Scalable Vector Graphics (SVG) file");
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Export the spectra as Scalable Vector Graphics.");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Export the spectra as Scalable Vector Graphics.");
	gtk_box_pack_end(GTK_BOX(hbox_text_label),button,FALSE,FALSE,0);
	svg_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(svg_convW),"Export the spectra as Scalable Vector Graphics.");
	gtk_entry_set_width_chars(GTK_ENTRY(svg_convW),60);
	we = (struct window_entry *) malloc(sizeof(struct window_entry));
	we->entry = svg_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(svg_convW), TRUE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),svg_convW,FALSE,FALSE,0);
	
	//CSV files
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Comma Separated Values (CSV) file");
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Export the spectra as Comma Separated Values files. Readable by Microsoft Excel and other programs.");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Export the spectra as Comma Separated Values files. Readable by Microsoft Excel and other programs.");
	gtk_box_pack_end(GTK_BOX(hbox_text_label),button,FALSE,FALSE,0);
	csv_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(csv_convW),"Export the spectra as Comma Separated Values files. Readable by Microsoft Excel and other programs.");
	gtk_entry_set_width_chars(GTK_ENTRY(csv_convW),60);
	we = (struct window_entry *) malloc(sizeof(struct window_entry));
	we->entry = csv_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(csv_convW), TRUE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),csv_convW,FALSE,FALSE,0);
	
	//html files
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Report HTML file");
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Produces an interactive HTML file containing an overview of all the results produced by the simulation: spectra and tables of all the individual XRF lines. Readable with recent versions of Firefox, Chrome and Safari.");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Produces an interactive HTML file containing an overview of all the results produced by the simulation: spectra and tables of all the individual XRF lines. Readable with recent versions of Firefox, Chrome and Safari.");
	gtk_box_pack_end(GTK_BOX(hbox_text_label),button,FALSE,FALSE,0);
	html_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(html_convW),"Produces an interactive HTML file containing an overview of all the results produced by the simulation: spectra and tables of all the individual XRF lines. Readable with recent versions of Firefox, Chrome and Safari.");
	gtk_entry_set_width_chars(GTK_ENTRY(html_convW),60);
	we = (struct window_entry *) malloc(sizeof(struct window_entry));
	we->entry = html_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(html_convW), TRUE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),html_convW,FALSE,FALSE,0);
	



	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);

	//actual controls
	frame = gtk_frame_new("Simulation controls");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Simulation controls</span>");


	//playButton = gtk_button_new_from_stock(GTK_STOCK_MEDIA_PLAY);
	playButton = gtk_button_new();
	g_signal_connect(G_OBJECT(playButton), "clicked",G_CALLBACK(play_button_clicked_cb), window);
	gtk_container_add(GTK_CONTAINER(playButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PLAY,GTK_ICON_SIZE_DIALOG));
	stopButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(stopButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_DIALOG));
	g_signal_connect(G_OBJECT(stopButton), "clicked",G_CALLBACK(stop_button_clicked_cb), window);
	pauseButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(pauseButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PAUSE,GTK_ICON_SIZE_DIALOG));
#ifdef G_OS_UNIX
	g_signal_connect(G_OBJECT(pauseButton), "clicked",G_CALLBACK(pause_button_clicked_cb), window);
#endif
	buttonbox = gtk_vbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(buttonbox), playButton, FALSE,FALSE,3);
	gtk_box_pack_start(GTK_BOX(buttonbox), pauseButton, FALSE,FALSE,3);
	gtk_box_pack_start(GTK_BOX(buttonbox), stopButton, FALSE,FALSE,3);
	gtk_widget_set_sensitive(pauseButton,FALSE);
	gtk_widget_set_sensitive(stopButton,FALSE);
	hbox_controls = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(hbox_controls), buttonbox, FALSE, FALSE, 3);


	progressbox = gtk_vbox_new(TRUE,5);

	image_solidW = gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU);
	progressbar_solidW = gtk_progress_bar_new();
	gtk_widget_set_size_request(progressbar_solidW,-1,30);
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(progressbar_solidW), GTK_PROGRESS_LEFT_TO_RIGHT);
	hbox_small = gtk_hbox_new(FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),image_solidW,FALSE,FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),progressbar_solidW,TRUE,TRUE,1);
	gtk_box_pack_start(GTK_BOX(progressbox),hbox_small,FALSE,FALSE,1);

	image_mainW = gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU);
	progressbar_mainW = gtk_progress_bar_new();
	gtk_widget_set_size_request(progressbar_mainW,-1,30);
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(progressbar_mainW), GTK_PROGRESS_LEFT_TO_RIGHT);
	hbox_small = gtk_hbox_new(FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),image_mainW,FALSE,FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),progressbar_mainW,TRUE,TRUE,1);
	gtk_box_pack_start(GTK_BOX(progressbox),hbox_small,FALSE,FALSE,1);

	image_escapeW = gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU);
	progressbar_escapeW = gtk_progress_bar_new();
	gtk_widget_set_size_request(progressbar_escapeW,-1,30);
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(progressbar_escapeW), GTK_PROGRESS_LEFT_TO_RIGHT);
	hbox_small = gtk_hbox_new(FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),image_escapeW,FALSE,FALSE,1);
	gtk_box_pack_start(GTK_BOX(hbox_small),progressbar_escapeW,TRUE,TRUE,1);
	gtk_box_pack_start(GTK_BOX(progressbox),hbox_small,FALSE,FALSE,1);


	gtk_box_pack_start(GTK_BOX(hbox_controls), progressbox, TRUE, TRUE, 3);

	//OPENMP number of threads!!!!!

	//textbuffer
	controlsLogW = gtk_text_view_new();
	gtk_widget_set_size_request(controlsLogW,400,-1);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(controlsLogW),GTK_WRAP_WORD);
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
	gtk_container_add(GTK_CONTAINER(frame),hbox_controls);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);




	//finalize widget
	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), superframe);



	reset_controls();

	return scrolled_window;
}

void reset_controls(void) {
	char buffer[512];
	GtkTextIter start, end;

	//set progressbars to 0 %
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_solidW),"Solid angles: 0 %");
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_mainW),"Interactions: 0 %");
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar_escapeW),"Escape peaks: 0 %");
	
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_solidW),0.0);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_mainW),0.0);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar_escapeW),0.0);

	//icons
	gtk_image_set_from_stock(GTK_IMAGE(image_solidW), GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU);
	gtk_image_set_from_stock(GTK_IMAGE(image_mainW), GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU);
	gtk_image_set_from_stock(GTK_IMAGE(image_escapeW), GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_MENU);
	

	//clear textbuffer
	gtk_text_buffer_get_start_iter (controlsLogB,&start); 
	gtk_text_buffer_get_end_iter (controlsLogB,&end); 
	gtk_text_buffer_delete (controlsLogB,&start,&end); 

}



