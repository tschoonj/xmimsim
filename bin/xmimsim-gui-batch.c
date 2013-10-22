#include "xmimsim-gui-batch.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-controls.h"
#include "xmi_main.h"
#include <stdio.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <string.h>

struct options_widget {
	GtkWidget *Mlines_prefsW;
	GtkWidget *rad_cascade_prefsW;
	GtkWidget *nonrad_cascade_prefsW;
	GtkWidget *variance_reduction_prefsW;
	GtkWidget *pile_up_prefsW;
	GtkWidget *poisson_prefsW;
	GtkWidget *nchannels_prefsW;
	GtkWidget *superframe;
};

enum xmi_msim_batch_options{
	XMI_MSIM_BATCH_MULTIPLE_OPTIONS,
	XMI_MSIM_BATCH_ONE_OPTION
};

struct batch_window_data {
	int *rv;
	GtkWidget *batch_window;
	GtkWidget *playButton;
	GtkWidget *stopButton;
#ifdef G_OS_UNIX
	GtkWidget *pauseButton;
#endif
	GtkWidget *nthreadsW;
	GtkWidget *progressbarW;
	GtkWidget *controlsLogW;
	GtkTextBuffer *controlsLogB;
	GtkWidget *saveButton;
	GtkWidget *controlsLogFileW;
	GtkWidget *verboseW;
	GtkWidget *exitButton;
	struct xmi_main_options *options;
	GSList *filenames;
	enum xmi_msim_batch_options batch_options;
	gboolean paused;
	GTimer *timer;
	FILE *logFile;
	int i;
	gchar *argv0;
};

static void xmimsim_child_watcher_cb(GPid pid, gint status, struct batch_window_data *bwd);
static gboolean xmimsim_stdout_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd);
static gboolean xmimsim_stderr_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd);

static void my_gtk_text_buffer_insert_at_cursor_with_tags2(struct batch_window_data *bwd, const gchar *text, gint len, GtkTextTag *first_tag, ...) {
	GtkTextIter iter, start;
	va_list args;
	GtkTextTag *tag;
	GtkTextMark *insert_mark;
	gint start_offset;

	g_return_if_fail(GTK_IS_TEXT_BUFFER(bwd->controlsLogB));
	g_return_if_fail(text != NULL);

	glong time_elapsed = (glong) g_timer_elapsed(bwd->timer,NULL);
	glong hours = time_elapsed / 3600;
	time_elapsed = time_elapsed % 3600;
	glong minutes = time_elapsed / 60;
	glong seconds = time_elapsed % 60;


	gchar *to_print = g_strdup_printf("%02i:%02i:%02i %s",(int) hours, (int) minutes, (int) seconds,text);

	gtk_text_buffer_get_end_iter(bwd->controlsLogB, &iter);

	start_offset = gtk_text_iter_get_offset(&iter);
	gtk_text_buffer_insert(bwd->controlsLogB, &iter, to_print,len);

	g_free(to_print);

	if (first_tag == NULL) {
		gtk_text_buffer_get_end_iter(bwd->controlsLogB, &iter);
		insert_mark = gtk_text_buffer_get_insert(bwd->controlsLogB);
		gtk_text_buffer_place_cursor(bwd->controlsLogB,&iter);
        	gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (bwd->controlsLogW),
	        insert_mark, 0.0, FALSE, 0, 1.0);
		return;
	}

	gtk_text_buffer_get_iter_at_offset (bwd->controlsLogB, &start, start_offset);

	va_start(args, first_tag);
	tag = first_tag;
	while (tag) {
		gtk_text_buffer_apply_tag(bwd->controlsLogB, tag, &start, &iter);
		tag = va_arg(args, GtkTextTag*);
	}
	va_end(args);
	
	gtk_text_buffer_get_end_iter(bwd->controlsLogB, &iter);
	insert_mark = gtk_text_buffer_get_insert(bwd->controlsLogB);
	gtk_text_buffer_place_cursor(bwd->controlsLogB,&iter);
       	gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (bwd->controlsLogW),
	        insert_mark, 0.0, FALSE, 0, 1.0);

	

	return;
}

static void batch_start_job_recursive(struct batch_window_data *bwd) {
	gchar **argv = NULL;
	gchar *xmimsim_executable;

#ifdef MAC_INTEGRATION
	if (xmi_resources_mac_query(XMI_RESOURCES_MAC_XMIMSIM_EXEC, &xmimsim_executable) == 0) {
		xmimsim_executable = NULL;
	}	
#else
	xmimsim_executable = g_find_program_in_path("xmimsim");
#endif

	int arg_counter = 9;
	//i refers to the file being executed
	int i = bwd->i;

	gchar *pbartext = g_strdup_printf("File %i/%i",i, g_slist_length(bwd->filenames));
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), pbartext);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), (double) i/(double) g_slist_length(bwd->filenames));
	while(gtk_events_pending())
	    gtk_main_iteration();
	g_free(pbartext);
	


	int j;
	if (bwd->batch_options == XMI_MSIM_BATCH_MULTIPLE_OPTIONS)
		j = i;
	else
		j = 0;

	argv = (gchar **) g_malloc(sizeof(gchar *)*10);
	argv[0] = g_strdup(xmimsim_executable);	

	if (bwd->options[j].use_M_lines) {
		argv[1] = g_strdup("--enable-M-lines");
	}
	else
		argv[1] = g_strdup("--disable-M-lines");

	if (bwd->options[j].use_cascade_radiative) {
		argv[2] = g_strdup("--enable-radiative-cascade");
	}
	else
		argv[2] = g_strdup("--disable-radiative-cascade");

	if (bwd->options[j].use_cascade_auger) {
		argv[3] = g_strdup("--enable-auger-cascade");
	}
	else
		argv[3] = g_strdup("--disable-auger-cascade");

	if (bwd->options[j].use_variance_reduction) {
		argv[4] = g_strdup("--enable-variance-reduction");
	}
	else
		argv[4] = g_strdup("--disable-variance-reduction");

	if (bwd->options[j].use_sum_peaks) {
		argv[5] = g_strdup("--enable-pile-up");
	}
	else
		argv[5] = g_strdup("--disable-pile-up");

	if (bwd->options[j].use_poisson) {
		argv[6] = g_strdup("--enable-poisson");
	}
	else
		argv[6] = g_strdup("--disable-poisson");

	argv[7]	= g_strdup_printf("--set-channels=%i", bwd->options[j].nchannels); 

	if (gtk_combo_box_get_active(GTK_COMBO_BOX(bwd->verboseW)) == 0) {
		argv[8] = g_strdup("--verbose");
	}
	else {
		argv[8] = g_strdup("--very-verbose");
	} 
#ifdef G_OS_WIN32
	//set solid angles and escape ratios files ourself!
	char *xmimsim_hdf5_solid_angles = NULL;
	char *xmimsim_hdf5_escape_ratios = NULL;

	if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0) {
		sprintf(buffer,"Could not determine solid angles HDF5 file\n");
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		if (bwd->logFile) {
			g_fprintf(bwd->logFile,"%s",buffer);
		}
		gtk_widget_set_sensitive(playButton,TRUE);
		return;	
	}
	argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
	argv[arg_counter] = g_strdup_printf("--with-solid-angles-data=%s",xmimsim_hdf5_solid_angles);
	arg_counter++;

	if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0) {
		sprintf(buffer,"Could not determine escape ratios HDF5 file\n");
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		if (bwd->logFile) {
			g_fprintf(bwd->logFile,"%s",buffer);
		}
		gtk_widget_set_sensitive(playButton,TRUE);
		return;	
	}
	argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
	argv[arg_counter] = g_strdup_printf("--with-escape-ratios-data=%s",xmimsim_hdf5_escape_ratios);
	arg_counter++;
#endif

	//number of threads
	if (bwd->nthreadsW != NULL) {
		argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
		argv[arg_counter++] = g_strdup_printf("--set-threads=%i",(int) gtk_range_get_value(GTK_RANGE(bwd->nthreadsW)));
	}
	argv[arg_counter++] = g_strdup((char *) g_slist_nth_data(bwd->filenames,i));
	argv[arg_counter++] = NULL;

	gchar *wd = g_path_get_dirname((char *) g_slist_nth_data(bwd->filenames,i));
	gboolean spawn_rv;
	gint out_fh, err_fh;
	GError *spawn_error = NULL;
	char buffer[512];

	//spawn
	spawn_rv = g_spawn_async_with_pipes(wd, argv, NULL, G_SPAWN_DO_NOT_REAP_CHILD, NULL, NULL, &xmimsim_pid, NULL, &out_fh, &err_fh, &spawn_error);

	if (spawn_rv == FALSE) {
		//couldn't spawn
		//print messag_ to textbox in red...
		sprintf(buffer,"%s\n",spawn_error->message);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		if (bwd->logFile) {
			g_fprintf(bwd->logFile,"%s",buffer);
			fclose(bwd->logFile);
		}
		g_error_free(spawn_error);
		xmimsim_pid = (GPid) -1;
		//Error dialog
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
		GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL,
	        GTK_MESSAGE_ERROR,
	        GTK_BUTTONS_CLOSE,
	        "Batch execution failed with error message %s",buffer
                );
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		*(bwd->rv) = 0;
		gtk_widget_destroy(bwd->batch_window);

		return;
	}
	sprintf(buffer,"%s was started with process id %i\n",argv[0],(int)xmimsim_pid);
	my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,NULL);
	if (bwd->logFile) {
		g_fprintf(bwd->logFile,"%s",buffer);
	}
	g_free(wd);

	bwd->paused=FALSE;

	if (i == 0) {
#ifdef G_OS_UNIX
		gtk_widget_set_sensitive(bwd->pauseButton,TRUE);
#endif
		gtk_widget_set_sensitive(bwd->stopButton,TRUE);
	}

	GIOChannel *xmimsim_stdout;
	GIOChannel *xmimsim_stderr;
#ifdef G_OS_WIN32
	xmimsim_stderr= g_io_channel_win32_new_fd(err_fh);
	xmimsim_stdout = g_io_channel_win32_new_fd(out_fh);
#else
	xmimsim_stderr= g_io_channel_unix_new(err_fh);
	xmimsim_stdout = g_io_channel_unix_new(out_fh);
#endif
	bwd->i = i;
	bwd->argv0 = argv[0];
	g_child_watch_add(xmimsim_pid,(GChildWatchFunc) xmimsim_child_watcher_cb, (gpointer) bwd);


	const gchar *encoding = NULL;
	g_get_charset(&encoding);

	g_io_channel_set_encoding(xmimsim_stdout, encoding, NULL);
	g_io_channel_set_close_on_unref(xmimsim_stdout,TRUE);
	g_io_add_watch(xmimsim_stdout, G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL, (GIOFunc) xmimsim_stdout_watcher, (gpointer) bwd);
	g_io_channel_unref(xmimsim_stdout);

	g_io_channel_set_encoding(xmimsim_stderr, encoding, NULL);
	g_io_channel_set_close_on_unref(xmimsim_stderr,TRUE);
	g_io_add_watch(xmimsim_stderr, G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL, (GIOFunc) xmimsim_stderr_watcher, (gpointer) bwd);
	g_io_channel_unref(xmimsim_stderr);

	return;
}



static gboolean xmimsim_stdout_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd) {
	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	char buffer[512];

	if (condition & (G_IO_IN|G_IO_PRI)) {
		/*while (gtk_events_pending ())
		        gtk_main_iteration ();*/
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);	
		if (pipe_status == G_IO_STATUS_ERROR) {
			sprintf(buffer,"%s with process id %i had an I/O error: %s\n", bwd->argv0, (int) xmimsim_pid,pipe_error->message);
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			g_error_free(pipe_error);
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, pipe_string,-1,NULL);
			g_free(pipe_string);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",pipe_string);
			}
		}
		else
			return FALSE;
	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		//sprintf(buffer,"%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		//my_gtk_text_buffer_insert_at_cursor_with_tags2(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}

static gboolean xmimsim_stderr_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd) {
	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	char buffer[512];

	if (condition & (G_IO_IN|G_IO_PRI)) {
		/*while (gtk_events_pending ())
		        gtk_main_iteration ();*/
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);	
		if (pipe_status == G_IO_STATUS_ERROR) {
			sprintf(buffer,"%s with process id %i had an I/O error: %s\n", bwd->argv0, (int) xmimsim_pid,pipe_error->message);
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			g_error_free(pipe_error);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, pipe_string,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			g_free(pipe_string);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
		}
		else
			return FALSE;

	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		//sprintf(buffer,"%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		//my_gtk_text_buffer_insert_at_cursor_with_tags2(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}

static int batch_mode(GtkWidget * main_window, struct xmi_main_options *options, GSList *filenames, enum xmi_msim_batch_options);

static void batch_reset_controls(struct batch_window_data *bwd) {
	char buffer[512];
	GtkTextIter start, end;

	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Start simulation");
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 0.0);

	//clear textbuffer
	gtk_text_buffer_get_start_iter (bwd->controlsLogB,&start); 
	gtk_text_buffer_get_end_iter (bwd->controlsLogB,&end); 
	gtk_text_buffer_delete (bwd->controlsLogB,&start,&end); 

	bwd->paused = FALSE;
}


static void choose_logfile(GtkButton *saveButton, struct batch_window_data *bwd) {
	GtkWidget *dialog;
	gchar *filename;

	dialog = gtk_file_chooser_dialog_new("Select a filename for the logfile", GTK_WINDOW(bwd->batch_window), GTK_FILE_CHOOSER_ACTION_SAVE, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL);
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		gtk_entry_set_text(GTK_ENTRY(bwd->controlsLogFileW), filename);
		g_free (filename);							
	}
	gtk_widget_destroy(dialog);
	return;
}

static void play_button_clicked(GtkButton *playButton, struct batch_window_data *bwd) {
	char buffer[512];

	//first deal with the pause case
#ifdef G_OS_UNIX
	if (bwd->paused) {
		gtk_widget_set_sensitive(bwd->playButton, FALSE);
		//send SIGCONT	
		int kill_rv;
		char buffer[512];
		gboolean spinning;
		g_timer_continue(bwd->timer);

		kill_rv = kill((pid_t) xmimsim_pid, SIGCONT);
		if (kill_rv == 0) {
			sprintf(buffer, "Process %i was successfully resumed\n",(int) xmimsim_pid);
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			gtk_widget_set_sensitive(bwd->pauseButton,TRUE);
			bwd->paused = FALSE;
		}
		else {
			sprintf(buffer, "Process %i could not be resumed\n",(int) xmimsim_pid);
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			gtk_widget_set_sensitive(bwd->playButton,TRUE);
			*(bwd->rv) = 0;
			//should end batch operation!
		}
		return;
	}
#endif
	bwd->paused = FALSE;

	//start_job
	gtk_widget_set_sensitive(bwd->playButton,FALSE);
	gtk_widget_set_sensitive(bwd->saveButton, FALSE);
	gtk_widget_set_sensitive(bwd->controlsLogFileW, FALSE);
	gtk_widget_set_sensitive(bwd->verboseW, FALSE);
	if (bwd->nthreadsW != NULL)
		gtk_widget_set_sensitive(bwd->nthreadsW,FALSE);
	
	batch_reset_controls(bwd);
	bwd->timer = g_timer_new();


	//open logFile if necessary
	gchar *logFileName = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(bwd->controlsLogFileW))));
	if (strlen(logFileName) > 0) {
		if ((bwd->logFile = fopen(logFileName, "w")) == NULL) {
			//could not write to logfile
			return;
		}
	}

	bwd->i = 0;

	batch_start_job_recursive(bwd);


	return;
}

static void xmimsim_child_watcher_cb(GPid pid, gint status, struct batch_window_data *bwd) {
	char buffer[512];
	int success;


	gchar *data = bwd->argv0;


	fprintf(stdout,"xmimsim_child_watcher_cb called with status: %i\n",status);
	gtk_widget_set_sensitive(bwd->stopButton,FALSE);
#ifdef G_OS_UNIX
	gtk_widget_set_sensitive(bwd->pauseButton,FALSE);
#endif

	//windows <-> unix issues here
	//unix allows to obtain more info about the way the process was terminated, windows will just have the exit code (status)
	//conditional compilation here
#ifdef G_OS_UNIX
	if (WIFEXITED(status)) {
		if (WEXITSTATUS(status) == 0) { /* child was terminated due to a call to exit */
			sprintf(buffer,"%s with process id %i exited normally without errors\n", data, (int) xmimsim_pid);
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"success" ),NULL);
			success = 1;
		}
		else {
			sprintf(buffer,"%s with process id %i exited with an error (code: %i)\n",data, (int) xmimsim_pid, WEXITSTATUS(status));
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			success = 0;
		}
	}
	else if (WIFSIGNALED(status)) { /* child was terminated due to a signal */
		sprintf(buffer, "%s with process id %i was terminated by signal %i\n",data, (int) xmimsim_pid, WTERMSIG(status));
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		success = 0;
	}
	else {
		sprintf(buffer, "%s with process id %i was terminated in some special way\n",data, (int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		success = 0;
	}

#elif defined(G_OS_WIN32)
	if (status == 0) {
		sprintf(buffer,"%s with process id %i exited normally without errors\n", data, (int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"success" ),NULL);
		success = 1;
	}
	else {
		sprintf(buffer,"%s with process id %i exited with an error (code: %i)\n",data, (int) xmimsim_pid, status);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		success = 0;
	}
#endif

	if (bwd->logFile) {
		g_fprintf(bwd->logFile,"%s",buffer);
	}
	g_spawn_close_pid(xmimsim_pid);
	xmimsim_pid = GPID_INACTIVE;

	bwd->i++;

	if (success == 0) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Simulations completed");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 1.0);
		while(gtk_events_pending())
			gtk_main_iteration();
		if (bwd->logFile)
			fclose(bwd->logFile);
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
		GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL,
	        GTK_MESSAGE_ERROR,
	        GTK_BUTTONS_CLOSE,
	        "Batch execution failed with error message %s",buffer
	        );
		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		*(bwd->rv) = 0;
		gtk_widget_destroy(bwd->batch_window);
	}
	/*
	else if (success == 0) {
		//simulation failed but moving on to the next one
		bwd->success[bwd->i-1] = 0;
		batch_start_job_recursive(bwd);
	}
	*/
	else if (bwd->i == g_slist_length(bwd->filenames)) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Simulations completed");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 1.0);
		while(gtk_events_pending())
			gtk_main_iteration();
		if (bwd->logFile)
			fclose(bwd->logFile);
		if (bwd->logFile)
			fclose(bwd->logFile);
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
		GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL,
	        GTK_MESSAGE_INFO,
	        GTK_BUTTONS_CLOSE,
	        "Batch execution succeeded");
		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		*(bwd->rv) = 1;
		gtk_widget_destroy(bwd->batch_window);
	}
	else {
		//start the next job
		batch_start_job_recursive(bwd);
	}
		


	return;
}
static void stop_button_clicked(GtkButton *stopButton, struct batch_window_data *bwd) {
	char buffer[512];
	gboolean spinning;

	gtk_widget_set_sensitive(bwd->stopButton,FALSE);
#ifdef G_OS_UNIX
	gtk_widget_set_sensitive(bwd->pauseButton,FALSE);
#endif
	//set buttons back in order
	bwd->paused = FALSE;
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
		sprintf(buffer, "Process %i was successfully terminated before completion\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		sprintf(buffer, "Process %i could not be terminated with the SIGTERM signal\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
	}

#elif defined(G_OS_WIN32)
	BOOL terminate_rv;

	terminate_rv = TerminateProcess((HANDLE) xmimsim_pid, (UINT) 1);

	if (terminate_rv == TRUE) {
		sprintf(buffer, "Process %i was successfully terminated\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		sprintf(buffer, "Process %i could not be terminated with the TerminateProcess call\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
	}
#endif
	if (bwd->logFile)
		g_fprintf(bwd->logFile,"%s",buffer);

	return;
}

#ifdef G_OS_UNIX
static void pause_button_clicked(GtkButton *pauseButton, struct batch_window_data *bwd) {
	//UNIX only
	
	int kill_rv;
	char buffer[512];
	gboolean spinning;

	g_timer_stop(bwd->timer);

	gtk_widget_set_sensitive(bwd->pauseButton,FALSE);
	gtk_widget_set_sensitive(bwd->stopButton,FALSE);
	kill_rv = kill((pid_t) xmimsim_pid, SIGSTOP);
	if (kill_rv == 0) {
		sprintf(buffer, "Process %i was successfully paused. Press the Play button to continue or Stop to kill the process\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
		bwd->paused=TRUE;
		if (bwd->logFile)
			g_fprintf(bwd->logFile, "%s", buffer);
		gtk_widget_set_sensitive(bwd->stopButton,TRUE);
		gtk_widget_set_sensitive(bwd->playButton,TRUE);
	}

}
#endif

static struct options_widget *create_options_frame(GtkWidget *main_window) {
	union xmimsim_prefs_val xpv;
	struct options_widget *rv = g_malloc(sizeof(struct options_widget));

	rv->superframe = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(rv->superframe),10);

	rv->Mlines_prefsW = gtk_check_button_new_with_label("Simulate M-lines");
	gtk_widget_set_tooltip_text(rv->Mlines_prefsW,"Enables the simulation of M-lines. Disabling this option may lead to a significant performance increase. Should always be enabled when high atomic number elements are present in the sample.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_M_LINES, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->Mlines_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->Mlines_prefsW, FALSE, FALSE, 3);

	rv->rad_cascade_prefsW = gtk_check_button_new_with_label("Simulate the radiative cascade effect");
	gtk_widget_set_tooltip_text(rv->rad_cascade_prefsW,"Enables the simulation of the radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_RAD_CASCADE, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->rad_cascade_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->rad_cascade_prefsW, FALSE, FALSE, 3);

	rv->nonrad_cascade_prefsW = gtk_check_button_new_with_label("Simulate the non-radiative cascade effect");
	gtk_widget_set_tooltip_text(rv->nonrad_cascade_prefsW,"Enables the simulation of the non-radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the non-radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NONRAD_CASCADE, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->nonrad_cascade_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->nonrad_cascade_prefsW, FALSE, FALSE, 3);

	rv->variance_reduction_prefsW = gtk_check_button_new_with_label("Enable variance reduction techniques");
	gtk_widget_set_tooltip_text(rv->variance_reduction_prefsW,"Disabling this option enables the brute-force method. Should only be used in combination with a high number of simulated photons.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->variance_reduction_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->variance_reduction_prefsW, FALSE, FALSE, 3);

	rv->pile_up_prefsW = gtk_check_button_new_with_label("Enable pulse pile-up simulation");
	gtk_widget_set_tooltip_text(rv->pile_up_prefsW,"When activated, will estimate detector electronics pulse pile-up. Determined by the pulse width parameter in Detector settings.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_PILE_UP, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->pile_up_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->pile_up_prefsW, FALSE, FALSE, 3);


	rv->poisson_prefsW = gtk_check_button_new_with_label("Enable Poisson noise generation");
	gtk_widget_set_tooltip_text(rv->poisson_prefsW,"Enabling this feature will add noise according to a Poisson distribution the convoluted spectra");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_POISSON, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->poisson_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->poisson_prefsW, FALSE, FALSE, 3);

	GtkAdjustment *spinner_adj = GTK_ADJUSTMENT(gtk_adjustment_new(2048.0, 10.0, 100000.0, 1.0, 10.0, 0.0));
	rv->nchannels_prefsW = gtk_spin_button_new(spinner_adj, 1, 0);
	gtk_editable_set_editable(GTK_EDITABLE(rv->nchannels_prefsW), TRUE);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(rv->nchannels_prefsW), GTK_UPDATE_IF_VALID);
	gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(rv->nchannels_prefsW), TRUE);
	gtk_entry_set_max_length(GTK_ENTRY(rv->nchannels_prefsW), 7);
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NCHANNELS, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(rv->nchannels_prefsW), (gdouble) xpv.i);
	GtkWidget *hbox = gtk_hbox_new(FALSE, 5);
	GtkWidget *label = gtk_label_new("Number of spectrum channels");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), rv->nchannels_prefsW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(rv->superframe), hbox, FALSE, FALSE, 3);

	return rv;
}

struct wizard_close_data {
	struct xmi_main_options *options;
	struct options_widget **ows;
	int *rv;
};

static void wizard_cancel(GtkAssistant *wizard, struct wizard_close_data *wcd) {
	*(wcd->rv) = 0;
	gtk_widget_destroy(GTK_WIDGET(wizard));
	return;
}

static void wizard_close(GtkAssistant *wizard, struct wizard_close_data *wcd) {
	//collect options from all pages
	int i;

	for (i = 0 ; i < gtk_assistant_get_n_pages(wizard)-2 ; i++) {
		g_fprintf(stdout, "Processing file %i\n",i);
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->Mlines_prefsW)))
			wcd->options[i].use_M_lines = 1;
		else
			wcd->options[i].use_M_lines = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->rad_cascade_prefsW)))
			wcd->options[i].use_cascade_radiative = 1;
		else
			wcd->options[i].use_cascade_radiative = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->nonrad_cascade_prefsW)))
			wcd->options[i].use_cascade_auger = 1;
		else
			wcd->options[i].use_cascade_auger = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->variance_reduction_prefsW)))
			wcd->options[i].use_variance_reduction = 1;
		else
			wcd->options[i].use_variance_reduction = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->pile_up_prefsW)))
			wcd->options[i].use_sum_peaks = 1;
		else
			wcd->options[i].use_sum_peaks = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->poisson_prefsW)))
			wcd->options[i].use_poisson = 1;
		else
			wcd->options[i].use_poisson = 0;

		wcd->options[i].nchannels = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(wcd->ows[i]->nchannels_prefsW));
			
	}
	*(wcd->rv) = 1;
	gtk_widget_destroy(GTK_WIDGET(wizard));
	return;
}

static gboolean wizard_delete_event(GtkWidget *wizard, GdkEvent *event, struct wizard_close_data *wcd) {
	*(wcd->rv) = 0;
	return FALSE;
}

static void batch_window_exit(GtkButton *button, struct batch_window_data *bwd) {
	//should preserve the current rv
	//*(bwd->rv) = 1;
	if (kill_current_job() == 0)
		gtk_widget_destroy(bwd->batch_window);	
	return;
}

static gboolean batch_window_delete_event(GtkWidget *batch_window, GdkEvent *event, struct batch_window_data *bwd) {
	if (kill_current_job() == 0)
		return FALSE;	

	//*(bwd->rv) = 0;
	return TRUE;
}

static int specific_options(GtkWidget *main_window, struct xmi_main_options *options, GSList *filenames) {
	GtkWidget *wizard = gtk_assistant_new();
	gtk_window_set_transient_for(GTK_WINDOW(wizard), GTK_WINDOW(main_window));
	gtk_window_set_modal(GTK_WINDOW(wizard), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(wizard), TRUE);
	gtk_window_set_position (GTK_WINDOW(wizard), GTK_WIN_POS_CENTER);

	gtk_window_set_title(GTK_WINDOW(wizard), "Set simulation options per file");


	//add intro page
	GtkWidget *introLabel = gtk_label_new("Use this wizard to set the simulation options per XMI-MSIM input-file");	
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), introLabel);
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), introLabel, TRUE);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), introLabel, GTK_ASSISTANT_PAGE_INTRO);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), introLabel, "Introduction");

	int i;
	struct options_widget **ows = g_malloc(sizeof(struct options_widget*)*g_slist_length(filenames));
	for (i = 0 ; i < g_slist_length(filenames) ; i++) {
		ows[i] = create_options_frame(main_window);
		gtk_assistant_append_page(GTK_ASSISTANT(wizard), ows[i]->superframe);
		gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), ows[i]->superframe, GTK_ASSISTANT_PAGE_CONTENT);
		gchar *filename = g_strdup_printf("File %i: %s",i+1, g_path_get_basename((char *) g_slist_nth_data(filenames,i)));
		gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), ows[i]->superframe, filename);
		g_free(filename);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), ows[i]->superframe, TRUE);
	}

	//add confirmation page
	GtkWidget *confirmationLabel = gtk_label_new("Confirm the options selected on the previous pages and continue with the simulation?");
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), confirmationLabel);
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), confirmationLabel, TRUE);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), confirmationLabel, GTK_ASSISTANT_PAGE_CONFIRM);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), confirmationLabel, "Confirmation");

	//signal handlers
	struct wizard_close_data *wcd = g_malloc(sizeof(struct wizard_close_data));
	wcd->options = options;
	wcd->ows = ows;
	int rv;
	wcd->rv = &rv;
	g_signal_connect(G_OBJECT(wizard), "cancel", G_CALLBACK(wizard_cancel), (gpointer) wcd);
	g_signal_connect(G_OBJECT(wizard), "close", G_CALLBACK(wizard_close), (gpointer) wcd);
	g_signal_connect(G_OBJECT(wizard), "delete-event", G_CALLBACK(wizard_delete_event), (gpointer) wcd);
	g_signal_connect(G_OBJECT(wizard), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	gtk_widget_show_all(wizard);
	gtk_main();
	return rv;
}



static int general_options(GtkWidget *main_window, struct xmi_main_options *options) {
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Set the options for the simulations batch", GTK_WINDOW(main_window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
	GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	struct options_widget *ow = create_options_frame(main_window);
	gtk_container_add(GTK_CONTAINER(content_area), ow->superframe);
	gtk_widget_show_all(ow->superframe);
	int dialog_rv = gtk_dialog_run(GTK_DIALOG(dialog));

	if (dialog_rv == GTK_RESPONSE_ACCEPT) {
		//accepted -> read options from dialog
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->Mlines_prefsW)))
			options->use_M_lines = 1;
		else
			options->use_M_lines = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->rad_cascade_prefsW)))
			options->use_cascade_radiative = 1;
		else
			options->use_cascade_radiative = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->nonrad_cascade_prefsW)))
			options->use_cascade_auger = 1;
		else
			options->use_cascade_auger = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->variance_reduction_prefsW)))
			options->use_variance_reduction = 1;
		else
			options->use_variance_reduction = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->pile_up_prefsW)))
			options->use_sum_peaks = 1;
		else
			options->use_sum_peaks = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->poisson_prefsW)))
			options->use_poisson = 1;
		else
			options->use_poisson = 0;

		options->nchannels = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(ow->nchannels_prefsW));
	}
	else {
		gtk_widget_destroy(dialog);
		return 0;
	}
	gtk_widget_destroy(dialog);
	return 1;
}  


void batchmode_button_clicked_cb(GtkWidget *button, GtkWidget *window) {

	g_fprintf(stdout,"Entering batchnode_button_clicked_cb...\n"); 
	//open dialog
	GtkWidget *dialog = gtk_file_chooser_dialog_new("Select one or more files", GTK_WINDOW(window), GTK_FILE_CHOOSER_ACTION_OPEN, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, NULL);
	gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(dialog), TRUE);
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	GtkWidget *label = gtk_label_new("If one file is selected then a batch of files will be created based on this file with one variable parameter. Selecting multiple files will result in all these files being executed.");
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(dialog), label);	
	GtkFileFilter *filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsi");
	gtk_file_filter_set_name(filter,"XMI-MSIM inputfiles");
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) != GTK_RESPONSE_ACCEPT) {
		//nothing was selected
   		gtk_widget_destroy (dialog);
		return;
	}

	//extract all selected filenames
	GSList *filenames = gtk_file_chooser_get_filenames(GTK_FILE_CHOOSER(dialog));
	int i;
	for (i = 0 ; i < g_slist_length(filenames) ; i++) {
		g_fprintf(stdout,"filename: %s\n", (char *) g_slist_nth_data(filenames,i));
	}

   	gtk_widget_destroy (dialog);
	struct xmi_main_options *options;
	if (g_slist_length(filenames) > 1) {
		//more than one file selected
		//1) ask if the options will apply to all or if individual job options will be used
		dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, "Set options for each input-file separately?");
		int response = gtk_dialog_run(GTK_DIALOG(dialog));
		//2) produce dialog asking for options
		if (response == GTK_RESPONSE_YES) {
   			gtk_widget_destroy (dialog);
			//file specific options
			g_fprintf(stdout, "yes clicked\n");
			options = malloc(sizeof(struct xmi_main_options)*g_slist_length(filenames));
			int rv = specific_options(window, options, filenames); 
			if (rv == 1) {
				//wizard completed 
				g_fprintf(stdout,"wizard completed\n");
			}
			else if (rv == 0) {
				//wizard aborted 
				g_fprintf(stdout,"wizard aborted\n");
				return;
			}
		}
		else if (response == GTK_RESPONSE_NO) {
			//options apply to all
   			gtk_widget_destroy (dialog);
			g_fprintf(stdout, "no clicked\n");
			options = malloc(sizeof(struct xmi_main_options));
			int rv = general_options(window, options);
			if (rv == 0) {
				return;
			}
			//options are set
		}
		else {
   			gtk_widget_destroy (dialog);
			return;
		}
		//3) launch execution window
		int exec_rv = batch_mode(window, options, filenames, response == GTK_RESPONSE_YES ? XMI_MSIM_BATCH_MULTIPLE_OPTIONS : XMI_MSIM_BATCH_ONE_OPTION);
		//4) display message with result
		g_fprintf(stdout,"exec_rv: %i\n", exec_rv);
	}
	else {
		//one file selected
		//options apply to all
		g_fprintf(stdout, "no clicked\n");
		options = malloc(sizeof(struct xmi_main_options));
		int rv = general_options(window, options);
		if (rv == 0) {
			return;
		}
	}



	return;
}

static int batch_mode(GtkWidget *main_window, struct xmi_main_options *options, GSList *filenames, enum xmi_msim_batch_options batch_options) {
	int rv = 0;
	GtkWidget *batch_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_transient_for(GTK_WINDOW(batch_window), GTK_WINDOW(main_window));
	gtk_window_set_modal(GTK_WINDOW(batch_window), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(batch_window), TRUE);
	gtk_window_set_position (GTK_WINDOW(batch_window), GTK_WIN_POS_CENTER);
	gtk_window_set_title(GTK_WINDOW(batch_window), "Batch simulation controls");
	gtk_window_set_default_size(GTK_WINDOW(batch_window),500,500);

	struct batch_window_data *bwd = g_malloc(sizeof(struct batch_window_data));
	bwd->batch_window = batch_window;

	GtkWidget *main_vbox = gtk_vbox_new(FALSE,0);
	gtk_container_add(GTK_CONTAINER(batch_window), main_vbox);
	gtk_container_set_border_width(GTK_CONTAINER(batch_window),3);
	GtkWidget *hbox = gtk_hbox_new(FALSE, 2);

	GtkWidget *playButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(playButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PLAY,GTK_ICON_SIZE_DIALOG));
	bwd->playButton = playButton;
	gtk_box_pack_start(GTK_BOX(hbox), playButton, FALSE, FALSE, 2);

#ifdef G_OS_UNIX
	GtkWidget *pauseButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(pauseButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PAUSE,GTK_ICON_SIZE_DIALOG));
	bwd->pauseButton = pauseButton;
	gtk_box_pack_start(GTK_BOX(hbox), pauseButton, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(pauseButton, FALSE);
#endif

	GtkWidget *stopButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(stopButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_DIALOG));
	bwd->stopButton = stopButton;
	gtk_box_pack_start(GTK_BOX(hbox), stopButton, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(stopButton, FALSE);

	GtkWidget *nthreadsW = NULL;
	if (omp_get_max_threads() > 1) {
		GtkWidget *cpuLabel = gtk_label_new("CPUs");
		gtk_box_pack_start(GTK_BOX(hbox), cpuLabel, FALSE, FALSE, 2);
		GtkObject *nthreadsA = gtk_adjustment_new((gdouble) omp_get_max_threads(), 1.0, (gdouble) omp_get_max_threads(), 1.0,1.0,0.0);
		nthreadsW = gtk_hscale_new(GTK_ADJUSTMENT(nthreadsA));	
		gtk_scale_set_digits(GTK_SCALE(nthreadsW), 0);
		gtk_scale_set_value_pos(GTK_SCALE(nthreadsW),GTK_POS_TOP);
		gtk_widget_set_size_request(nthreadsW,30,-1);
		gtk_box_pack_start(GTK_BOX(hbox), nthreadsW, TRUE, TRUE, 2);
	}
	bwd->nthreadsW = nthreadsW;

	//add progressbar
	GtkWidget *progressbarW = gtk_progress_bar_new();
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(progressbarW), GTK_PROGRESS_LEFT_TO_RIGHT);
	//gtk_widget_set_size_request(progressbarW,-1,10);
	GtkWidget *pvbox = gtk_vbox_new(TRUE,1);
	gtk_box_pack_start(GTK_BOX(pvbox), progressbarW, FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(hbox), pvbox, TRUE, TRUE, 2);
	gtk_widget_set_size_request(progressbarW,-1,30);
	bwd->progressbarW = progressbarW;
	
	gtk_box_pack_start(GTK_BOX(main_vbox), hbox, FALSE, FALSE, 0);

	//output log
	GtkWidget *controlsLogW = gtk_text_view_new();
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(controlsLogW),GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(controlsLogW),3);
	GtkTextBuffer *controlsLogB = gtk_text_view_get_buffer(GTK_TEXT_VIEW(controlsLogW));
	gtk_container_set_border_width(GTK_CONTAINER(controlsLogW),2);
	gtk_text_view_set_editable(GTK_TEXT_VIEW(controlsLogW),FALSE);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(controlsLogW),FALSE);
	gtk_text_buffer_create_tag(controlsLogB, "error","foreground","red",NULL);
	gtk_text_buffer_create_tag(controlsLogB, "success","foreground","green",NULL);
	gtk_text_buffer_create_tag(controlsLogB, "pause-continue-stopped","foreground","orange",NULL);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), controlsLogW);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 2);
	gtk_box_pack_start(GTK_BOX(main_vbox), scrolled_window, TRUE, TRUE, 3);
	bwd->controlsLogW = controlsLogW;
	bwd->controlsLogB = controlsLogB;

	//bottom widget
	hbox = gtk_hbox_new(FALSE, 2);
	gtk_container_set_border_width(GTK_CONTAINER(hbox), 2);
	GtkWidget *saveButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	bwd->saveButton = saveButton;
	gtk_box_pack_start(GTK_BOX(hbox), saveButton, FALSE, FALSE, 2);
	GtkWidget *controlsLogFileW = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(controlsLogFileW), FALSE);
	gtk_box_pack_start(GTK_BOX(hbox), controlsLogFileW, TRUE, TRUE, 2);
	bwd->controlsLogFileW = controlsLogFileW;

	g_signal_connect(G_OBJECT(saveButton), "clicked", G_CALLBACK(choose_logfile), (gpointer) bwd);

#if GTK_CHECK_VERSION(2,24,0)
	GtkWidget *verboseW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(verboseW),"Verbose");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(verboseW),"Very verbose");
#else
	GtkWidget *verboseW = gtk_combo_box_new_text();
	gtk_combo_box_append_text(GTK_COMBO_BOX(verboseW),"Verbose");
	gtk_combo_box_append_text(GTK_COMBO_BOX(verboseW),"Very verbose");
#endif
	gtk_combo_box_set_active(GTK_COMBO_BOX(verboseW),0);
	bwd->verboseW = verboseW;
	gtk_box_pack_start(GTK_BOX(hbox), verboseW, FALSE, FALSE, 2);
	GtkWidget *exitButton = gtk_button_new_from_stock(GTK_STOCK_QUIT);
	bwd->exitButton = exitButton;
	gtk_box_pack_end(GTK_BOX(hbox), exitButton, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(main_vbox), hbox, FALSE, FALSE, 0);

	bwd->options = options;
	bwd->filenames = filenames;
	bwd->batch_options = batch_options;
	bwd->rv = &rv;


	g_signal_connect(G_OBJECT(batch_window), "delete-event", G_CALLBACK(batch_window_delete_event), (gpointer) bwd);
	g_signal_connect(G_OBJECT(batch_window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(exitButton), "clicked", G_CALLBACK(batch_window_exit), (gpointer) bwd);
	g_signal_connect(G_OBJECT(playButton), "clicked", G_CALLBACK(play_button_clicked), (gpointer) bwd);
	g_signal_connect(G_OBJECT(stopButton), "clicked", G_CALLBACK(stop_button_clicked), (gpointer) bwd);
#ifdef G_OS_UNIX
	g_signal_connect(G_OBJECT(pauseButton), "clicked", G_CALLBACK(pause_button_clicked), (gpointer) bwd);
#endif
	bwd->paused = FALSE;
	bwd->logFile = NULL;
	gtk_widget_show_all(batch_window);
	
	gtk_main();

	return rv;
}
