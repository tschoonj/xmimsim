/*
Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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
#include "xmi_aux.h"
#include "xmimsim-gui-layer.h"
#include "xmimsim-gui-energies.h"
#include "xmimsim-gui-controls.h"
#include "xmimsim-gui-results.h"
#include <string.h>
#include <stdio.h>
#include "xmi_xml.h"
#include "xmi_data_structs.h"
#include <stdlib.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <xraylib.h>
#include <gdk/gdkkeysyms.h>
#include <locale.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#ifdef G_OS_UNIX
#include <signal.h>
#elif defined(G_OS_WIN32)
#include <windows.h>
#include "xmi_registry_win.h"
#endif

#ifdef MAC_INTEGRATION
#import <Foundation/Foundation.h>
#include <gtkosxapplication.h>
#include <gdk/gdkquartz.h>
#include "xmi_resources_mac.h"
#define PRIMARY_ACCEL_KEY GDK_META_MASK
#else
#define PRIMARY_ACCEL_KEY GDK_CONTROL_MASK
#endif


#define UNLIKELY_FILENAME "Kabouter Wesley rules!"


/*
 *
 * Widgets
 *
 */


//general
static GtkWidget *outputfileW;
static GtkWidget *n_photons_intervalW;
static GtkWidget *n_photons_lineW;
static GtkWidget *n_interactions_trajectoryW;
GtkWidget *commentsW;
static GtkWidget *undoW;
static GtkWidget *redoW;
static GtkWidget *newW;
static GtkWidget *openW;
GtkWidget *saveW;
GtkWidget *save_asW;
#ifndef MAC_INTEGRATION
static GtkWidget *quitW;
#endif
static GtkToolItem *newT;
static GtkToolItem *openT;
GtkToolItem *saveasT;
GtkToolItem *saveT;
static GtkToolItem *undoT;
static GtkToolItem *redoT;


//composition 
struct layerWidget *layerW;

static GtkWidget *compositionW;
static GtkListStore *compositionL;
static GtkWidget *exc_compositionW;
static GtkListStore *exc_compositionL;
static GtkWidget *det_compositionW;
static GtkListStore *det_compositionL;
static GtkWidget *crystal_compositionW;
static GtkListStore *crystal_compositionL;

//geometry
static GtkWidget *d_sample_sourceW;
static GtkWidget *n_sample_orientation_xW;
static GtkWidget *n_sample_orientation_yW;
static GtkWidget *n_sample_orientation_zW;
static GtkWidget *p_detector_window_xW;
static GtkWidget *p_detector_window_yW;
static GtkWidget *p_detector_window_zW;
static GtkWidget *n_detector_orientation_xW;
static GtkWidget *n_detector_orientation_yW;
static GtkWidget *n_detector_orientation_zW;
static GtkWidget *area_detectorW;
static GtkWidget *collimator_heightW;
static GtkWidget *collimator_diameterW;
static GtkWidget *d_source_slitW;
static GtkWidget *slit_size_xW;
static GtkWidget *slit_size_yW;

//detector
static GtkWidget *detector_typeW;
static GtkWidget *detector_gainW;
static GtkWidget *detector_live_timeW;
static GtkWidget *detector_pulse_widthW;
static GtkWidget *detector_zeroW;
static GtkWidget *detector_fanoW;
static GtkWidget *detector_noiseW;
static GtkWidget *detector_max_convolution_energyW;

GtkWidget *notebook;

/*
 *
 * gulongs
 *
 */

//general
static gulong outputfileG;
static gulong n_photons_intervalG;
static gulong n_photons_lineG;
static gulong n_interactions_trajectoryG;
static gulong comments_beginG;
static gulong comments_endG;
static gulong commentsG;
//geometry
static gulong d_sample_sourceG;
static gulong n_sample_orientation_xG;
static gulong n_sample_orientation_yG;
static gulong n_sample_orientation_zG;
static gulong p_detector_window_xG;
static gulong p_detector_window_yG;
static gulong p_detector_window_zG;
static gulong n_detector_orientation_xG;
static gulong n_detector_orientation_yG;
static gulong n_detector_orientation_zG;
static gulong area_detectorG;
static gulong collimator_heightG;
static gulong collimator_diameterG;
static gulong d_source_slitG;
static gulong slit_size_xG;
static gulong slit_size_yG;

//detector
static gulong detector_typeG;
static gulong detector_gainG;
static gulong detector_live_timeG;
static gulong detector_pulse_widthG;
static gulong detector_zeroG;
static gulong detector_fanoG;
static gulong detector_noiseG;
static gulong detector_max_convolution_energyG;

//notebook
gulong notebookG;



/*
 *
 *	values that check validity of entry widgets
 *
 */

//general
//static int outputfileC;
static int n_photons_intervalC;
static int n_photons_lineC;
static int n_interactions_trajectoryC;
//geometry
static int d_sample_sourceC;
static int n_sample_orientation_xC;
static int n_sample_orientation_yC;
static int n_sample_orientation_zC;
static int p_detector_window_xC;
static int p_detector_window_yC;
static int p_detector_window_zC;
static int n_detector_orientation_xC;
static int n_detector_orientation_yC;
static int n_detector_orientation_zC;
static int area_detectorC;
static int collimator_heightC;
static int collimator_diameterC;
static int d_source_slitC;
static int slit_size_xC;
static int slit_size_yC;

//detector
static int detector_typeC;
static int detector_gainC;
static int detector_live_timeC;
static int detector_pulse_widthC;
static int detector_zeroC;
static int detector_fanoC;
static int detector_noiseC;
static int detector_max_convolution_energyC;





/*
 *
 *  xmi_composition structs
 *
 */

struct xmi_composition *compositionS;
struct xmi_composition *exc_compositionS;
struct xmi_composition *det_compositionS;
struct xmi_composition *crystal_compositionS;
struct xmi_layer *layer;





/*
 *
 * undo buffer
 *
 */


static struct undo_single *redo_buffer;
struct undo_single *current;
static struct undo_single *last;
struct undo_single *last_saved;
static char *opened_file_name;




/*
 *
 * Gregex patterns
 *
 */

GRegex *pos_int;


/*
 *
 * Gdk colors
 *
 */

GdkColor white = {.red = (guint16) 65535, .green = (guint16) 65535, .blue = (guint16) 65535};
GdkColor red = {.red = (guint16) 65535, .green = (guint16) 1000, .blue = (guint16) 1000};


/*
 *
 * notebook pages
 *
 *
 */

gint input_page;
gint control_page;
gint results_page;
gint current_page;


enum {
	N_ELEMENTS_COLUMN,
	ELEMENTS_COLUMN,
	DENSITY_COLUMN,
	THICKNESS_COLUMN,
	REFERENCE_COLUMN,
	NCOLUMNS_MATRIX
};

struct matrix_data {
	GtkWidget *addButton;
	GtkWidget *editButton;
	GtkWidget *deleteButton;
	GtkWidget *topButton;
	GtkWidget *upButton;
	GtkWidget *downButton;
	GtkWidget *bottomButton;
};

char *xmimsim_title_xmsi = NULL;
char *xmimsim_title_xmso = NULL;
char *xmimsim_filename_xmsi = NULL;
char *xmimsim_filename_xmso = NULL;



void reset_undo_buffer(struct xmi_input *xi_new, char *filename);
void change_all_values(struct xmi_input *);
void load_from_file_cb(GtkWidget *, gpointer);
void saveas_cb(GtkWidget *widget, gpointer data);
gboolean saveas_function(GtkWidget *widget, gpointer data);
void save_cb(GtkWidget *widget, gpointer data);
#ifdef MAC_INTEGRATION
void quit_program_cb(GtkOSXApplication *app, gpointer data);
gboolean quit_blocker_mac_cb(GtkOSXApplication *app, gpointer data);
#else
void quit_program_cb(GtkWidget *widget, gpointer data);
#endif
void new_cb(GtkWidget *widget, gpointer data);

#ifdef G_OS_UNIX
void signal_handler(int sig) {
	if (sig == SIGINT) {
		fprintf(stdout,"SIGINT caught\n");
	}
	else if (sig == SIGTERM){
		fprintf(stdout,"SIGTERM caught\n");
	}
	else if (sig == SIGSEGV){
		fprintf(stdout,"SIGSEGV caught\n");
	}
	else if (sig == SIGHUP){
		fprintf(stdout,"SIGHUP caught\n");
	}

	if (xmimsim_pid != GPID_INACTIVE) {
		int kill_rv;
	
		fprintf(stdout,"killing %i UNIX style\n", (int) xmimsim_pid);
		kill_rv = kill((pid_t) xmimsim_pid, SIGTERM);
		wait(NULL);
		if (kill_rv == 0) {
			fprintf(stdout, "Process %i was successfully terminated before completion\n",(int) xmimsim_pid);
		}
		else {
			fprintf(stdout, "Process %i could not be terminated with the SIGTERM signal\n",(int) xmimsim_pid);
		}
	}
	_exit(0);
}

#elif defined(G_OS_WIN32)

#endif

void adjust_save_buttons(void) {

	int status;

	check_changes_saved(&status);


	if(check_changeables() == 1 && xmi_validate_input(current->xi) == 0) {
		if (status == CHECK_CHANGES_SAVED_BEFORE || status == CHECK_CHANGES_NEVER_SAVED) {
			gtk_widget_set_sensitive(saveW,TRUE);
		 	gtk_widget_set_sensitive(GTK_WIDGET(saveT),TRUE);
		}
		else {
			gtk_widget_set_sensitive(saveW,FALSE);
		 	gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
		}
		gtk_widget_set_sensitive(save_asW,TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveasT),TRUE);
	}
	else {
		gtk_widget_set_sensitive(saveW,FALSE);
		gtk_widget_set_sensitive(save_asW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveasT),FALSE);
	}

}






gboolean process_pre_file_operation (GtkWidget *window) {

	struct undo_single *check_rv;
	int check_status;
	GtkWidget *dialog = NULL;
	GtkWidget *content;
	gint dialog_rv;
	GtkWidget *label;
	GtkTextIter iterb, itere;

	//check if last changes have been saved, because they will be lost otherwise!
	check_rv = check_changes_saved(&check_status);

	if (check_status == CHECK_CHANGES_NEW) {
		return TRUE;
	}
	else if (check_status == CHECK_CHANGES_SAVED_BEFORE) {
		dialog = gtk_dialog_new_with_buttons("",GTK_WINDOW(window),
			GTK_DIALOG_MODAL,
			GTK_STOCK_SAVE_AS, GTK_RESPONSE_SAVEAS,
			GTK_STOCK_SAVE, GTK_RESPONSE_SAVE,
			GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			GTK_STOCK_NO, GTK_RESPONSE_NOSAVE,
			NULL
		);	
		content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
		label = gtk_label_new("You have made changes since your last save. This is your last chance to save them or they will be lost otherwise!");
		gtk_widget_show(label);
		gtk_box_pack_start(GTK_BOX(content),label, FALSE, FALSE, 3);

	}
	else if (check_status == CHECK_CHANGES_NEVER_SAVED) {
		dialog = gtk_dialog_new_with_buttons("",GTK_WINDOW(window),
			GTK_DIALOG_MODAL,
			GTK_STOCK_SAVE_AS, GTK_RESPONSE_SAVEAS,
			GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			GTK_STOCK_NO, GTK_RESPONSE_NOSAVE,
			NULL
		);	
		content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
		label = gtk_label_new("You have never saved the introduced information. This is your last chance to save it or it will be lost otherwise!");
		gtk_widget_show(label);
		gtk_box_pack_start(GTK_BOX(content),label, FALSE, FALSE, 3);
	}

	if (dialog != NULL) {
		dialog_rv = gtk_dialog_run(GTK_DIALOG(dialog));
		if (dialog_rv == GTK_RESPONSE_CANCEL || dialog_rv == GTK_RESPONSE_DELETE_EVENT) {
			fprintf(stdout,"Cancel button clicked\n");
			gtk_widget_destroy(dialog);
			return FALSE;
		}
		else if (dialog_rv == GTK_RESPONSE_NOSAVE) {
			//do nothing
		}
		else if (dialog_rv == GTK_RESPONSE_SAVEAS) {
			//run saveas dialog
			//incomplete -> case is not handled when user clicks cancel in saveas_cb...
			if(saveas_function(dialog, (gpointer) dialog) == FALSE) {
				gtk_widget_destroy(dialog);
				return FALSE;
			}
		}
		else if (dialog_rv == GTK_RESPONSE_SAVE) {
			//update file
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
			if (xmi_write_input_xml(check_rv->filename, current->xi) == 1) {

			}
			else {
				gtk_widget_destroy (dialog);
				dialog = gtk_message_dialog_new (GTK_WINDOW(window),
					GTK_DIALOG_DESTROY_WITH_PARENT,
		        		GTK_MESSAGE_ERROR,
		        		GTK_BUTTONS_CLOSE,
		        		"Could not write to file %s: not writeable?",check_rv->filename
	                	);
	     			gtk_dialog_run (GTK_DIALOG (dialog));
	     			gtk_widget_destroy (dialog);
				return FALSE;
			}

		}


		gtk_widget_destroy(dialog);
	}

	return TRUE;
}

void update_xmimsim_title_xmsi(char *new_title, GtkWidget *my_window, char *filename) {
	g_free(xmimsim_title_xmsi);
	xmimsim_title_xmsi = g_strdup_printf(XMIMSIM_TITLE_PREFIX "%s",new_title);

#ifdef MAC_INTEGRATION
	if (filename != NULL) {
		g_free(xmimsim_filename_xmsi);
		xmimsim_filename_xmsi = g_strdup(filename);
	}
	else {
		g_free(xmimsim_filename_xmsi);
		xmimsim_filename_xmsi = NULL;
	}
#endif


	if (gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)) == input_page ||
	  gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)) == control_page) {
		gtk_window_set_title(GTK_WINDOW(my_window),xmimsim_title_xmsi);
#ifdef MAC_INTEGRATION
		NSWindow *qwindow = gdk_quartz_window_get_nswindow(gtk_widget_get_window(my_window));
		if (filename != NULL) {
			gchar *uri = g_filename_to_uri(filename,NULL, NULL);
			NSURL *nsurl = [NSURL URLWithString:[NSString stringWithUTF8String:uri]];
			[qwindow setRepresentedURL:nsurl];
			g_free(uri);
		}
		else {
			[qwindow setRepresentedURL:nil];
		}
#endif
	}
	return;
}

void update_xmimsim_title_xmso(char *new_title, GtkWidget *my_window, char *filename) {
	g_free(xmimsim_title_xmso);
	xmimsim_title_xmso = g_strdup_printf(XMIMSIM_TITLE_PREFIX "%s",new_title);

#ifdef MAC_INTEGRATION
	if (filename != NULL) {
		g_free(xmimsim_filename_xmso);
		xmimsim_filename_xmso = g_strdup(filename);
	}
	else {
		g_free(xmimsim_filename_xmso);
		xmimsim_filename_xmso = NULL;
	}
#endif

	if (gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)) == results_page) {
		gtk_window_set_title(GTK_WINDOW(my_window),xmimsim_title_xmso);
#ifdef MAC_INTEGRATION
		NSWindow *qwindow = gdk_quartz_window_get_nswindow(gtk_widget_get_window(my_window));
		if (filename != NULL) {
			gchar *uri = g_filename_to_uri(filename,NULL, NULL);
			NSURL *nsurl = [NSURL URLWithString:[NSString stringWithUTF8String:uri]];
			[qwindow setRepresentedURL:nsurl];
			g_free(uri);
		}
		else {
			[qwindow setRepresentedURL:nil];
		}
#endif
	}
	return;
}


static void notebook_page_changed_cb(GtkNotebook *notebook, gpointer pageptr, guint page, gpointer data) {
	GtkWidget *my_window = (GtkWidget *) data;


	if (page == results_page) {
		gtk_window_set_title(GTK_WINDOW(my_window),xmimsim_title_xmso);
#ifdef MAC_INTEGRATION
		NSWindow *qwindow = gdk_quartz_window_get_nswindow(gtk_widget_get_window(my_window));
		if (xmimsim_filename_xmso != NULL) {
			gchar *uri = g_filename_to_uri(xmimsim_filename_xmso,NULL, NULL);
			NSURL *nsurl = [NSURL URLWithString:[NSString stringWithUTF8String:uri]];
			[qwindow setRepresentedURL:nsurl];
			g_free(uri);
		}
		else {
			[qwindow setRepresentedURL:nil];
		}
#endif
		
	}
	else if (page == control_page || page == input_page) {
		gtk_window_set_title(GTK_WINDOW(my_window),xmimsim_title_xmsi);
#ifdef MAC_INTEGRATION
		NSWindow *qwindow = gdk_quartz_window_get_nswindow(gtk_widget_get_window(my_window));
		if (xmimsim_filename_xmsi!= NULL) {
			gchar *uri = g_filename_to_uri(xmimsim_filename_xmsi,NULL, NULL);
			NSURL *nsurl = [NSURL URLWithString:[NSString stringWithUTF8String:uri]];
			[qwindow setRepresentedURL:nsurl];
			g_free(uri);
		}
		else {
			[qwindow setRepresentedURL:nil];
		}
#endif
	
	
	}




	current_page = (gint) page;

	return;
}

void my_gtk_cell_renderer_set_alignment (GtkCellRenderer *cell, gfloat xalign, gfloat yalign) {
	g_return_if_fail (GTK_IS_CELL_RENDERER (cell));
	g_return_if_fail (xalign >= 0.0 && xalign <= 1.0);
	g_return_if_fail (yalign >= 0.0 && yalign <= 1.0);

	if ((xalign != cell->xalign) || (yalign != cell->yalign)) {
		g_object_freeze_notify (G_OBJECT (cell));
	        if (xalign != cell->xalign) {
			cell->xalign = xalign;
			g_object_notify (G_OBJECT (cell), "xalign");
		}

		if (yalign != cell->yalign) {
			cell->yalign = yalign;
			g_object_notify (G_OBJECT (cell), "yalign");
		}

		g_object_thaw_notify (G_OBJECT (cell));
	}
}

void my_gtk_cell_renderer_toggle_set_activatable (GtkCellRendererToggle *toggle, gboolean setting) {
	g_return_if_fail (GTK_IS_CELL_RENDERER_TOGGLE (toggle));

	if (toggle->activatable != setting) {
		toggle->activatable = setting ? TRUE : FALSE;
		g_object_notify (G_OBJECT (toggle), "activatable");
	}
}

struct undo_single *check_changes_saved(int *status) {
	struct undo_single *temp = current;
	GtkTextIter iterb, itere;

	gchar *commentsText;
	int commentsEqual;


	
	//get text from comments...
	gtk_text_buffer_get_bounds(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW)),&iterb, &itere);

	if (gtk_text_iter_equal (&iterb, &itere) == TRUE) {
		commentsText = strdup("");
	}
	else {
		commentsText = strdup(gtk_text_buffer_get_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW)),&iterb, &itere, FALSE));
	}

	commentsEqual = strcmp(commentsText,current->xi->general->comments);

	if (last_saved == NULL && redo_buffer == current && 
		strcmp(redo_buffer->filename,UNLIKELY_FILENAME) == 0) {
		*status = CHECK_CHANGES_NEW;
		temp = NULL;
	}
	else if (last_saved == NULL) {
		*status = CHECK_CHANGES_NEVER_SAVED;
		temp = NULL;
	}
	else if (last_saved == current && commentsEqual == 0) {
		//found filename different from unlikely one... and it's in the current variable
		//meaning it was just saved!
		*status = CHECK_CHANGES_JUST_SAVED;
		temp = current;
#if DEBUG == 1
		fprintf(stdout,"last saved == current\n");
#endif
	}
	else if (xmi_compare_input(current->xi,last_saved->xi) == 0 && commentsEqual == 0) {
		//it was saved before, but there are no changes compared to before
		*status = CHECK_CHANGES_JUST_SAVED;
		temp = last_saved;
#if DEBUG == 1
		fprintf(stdout,"last saved and current are equal\n");
#endif
	}
	else {
		//it was saved before, and now the status has changed
		*status = CHECK_CHANGES_SAVED_BEFORE;
		temp = last_saved;
	}

	g_free(commentsText);

	return temp;
}




static void select_outputfile_cb(GtkButton *button, gpointer data) {
	GtkWidget *dialog;
	GtkFileFilter *filter;
	char *filename;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmso");
	gtk_file_filter_set_name(filter,"XMI-MSIM outputfiles");

	dialog = gtk_file_chooser_dialog_new("Select the outputfile for the simulation",
		GTK_WINDOW((GtkWidget *) data),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL
	);

	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		//check extension
		if (strcmp(filename+strlen(filename)-5, ".xmso") != 0) {
			filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+6));
			strcat(filename,".xmso");
		}

		gtk_entry_set_text(GTK_ENTRY(outputfileW), filename);
		update_undo_buffer(OUTPUTFILE,outputfileW);

		g_free (filename);							
	}

	gtk_widget_destroy (dialog);

	return;
}



static void layer_widget_hide_cb(GtkWidget *window, gpointer data) {
	struct layerWidget *lw = (struct layerWidget *) data;	
	struct xmi_composition *composition;
	GtkTreeIter iter;
	GtkListStore *store;
	char *elementString;
	int i,j;
	int updateKind;

#if DEBUG == 1
	fprintf(stdout,"layerWidget is hiding\n");
#endif

	if (lw->matrixKind == COMPOSITION) {
		composition = compositionS;
		store = compositionL;
		if (lw->AddOrEdit == LW_ADD) 
			updateKind = COMPOSITION_ADD;
		else if (lw->AddOrEdit == LW_EDIT) 
			updateKind = COMPOSITION_EDIT;
	}
	else if (lw->matrixKind == EXC_COMPOSITION) {
		composition = exc_compositionS;
		store = exc_compositionL;
		if (lw->AddOrEdit == LW_ADD) 
			updateKind = EXC_COMPOSITION_ADD;
		else if (lw->AddOrEdit == LW_EDIT) 
			updateKind = EXC_COMPOSITION_EDIT;
	}
	else if (lw->matrixKind == DET_COMPOSITION) {
		composition = det_compositionS;
		store = det_compositionL;
		if (lw->AddOrEdit == LW_ADD) 
			updateKind = DET_COMPOSITION_ADD;
		else if (lw->AddOrEdit == LW_EDIT) 
			updateKind = DET_COMPOSITION_EDIT;
	}
	else if (lw->matrixKind == CRYSTAL_COMPOSITION) {
		composition = crystal_compositionS;
		store = crystal_compositionL;
		if (lw->AddOrEdit == LW_ADD) 
			updateKind = CRYSTAL_COMPOSITION_ADD;
		else if (lw->AddOrEdit == LW_EDIT) 
			updateKind = CRYSTAL_COMPOSITION_EDIT;
	}

	if (*(lw->my_layer) != NULL) {
		//OK button was clicked
		//in case of editing and changing nothing.. undo should not be triggered
		//requires xmi_compare_layer... too lazy for now
		if (lw->AddOrEdit == LW_ADD) {
			//adding layer
			composition->layers = (struct xmi_layer*) realloc(composition->layers, sizeof(struct xmi_layer)*(++composition->n_layers));
			xmi_copy_layer2(layer,composition->layers+composition->n_layers-1);

			if (composition->n_layers == 1)
				composition->reference_layer = 1;

			gtk_list_store_append(store, &iter);
			i = composition->n_layers-1;
			elementString = (char *) malloc(sizeof(char)* (composition->layers[i].n_elements*5));
			elementString[0] = '\0';
			for (j = 0 ; j < composition->layers[i].n_elements ; j++) {
				strcat(elementString,AtomicNumberToSymbol(composition->layers[i].Z[j]));
				if (j != composition->layers[i].n_elements-1) {
					strcat(elementString,", ");
				}
			}
			if (lw->matrixKind == COMPOSITION) {
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,composition->layers[i].density,
					THICKNESS_COLUMN,composition->layers[i].thickness,
					REFERENCE_COLUMN, composition->n_layers == 1 ? TRUE : FALSE,
					-1
					);

			}
			else {
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,composition->layers[i].density,
					THICKNESS_COLUMN,composition->layers[i].thickness,
					-1
					);
			}
	
			free(elementString);
	

		}
		else if (lw->AddOrEdit == LW_EDIT) {
			//editing layer
			i = lw->layerNumber;
			free(composition->layers[i].Z);
			free(composition->layers[i].weight);
			xmi_copy_layer2(layer,composition->layers+lw->layerNumber);

			elementString = (char *) malloc(sizeof(char)* (composition->layers[i].n_elements*5));
			elementString[0] = '\0';
			for (j = 0 ; j < composition->layers[i].n_elements ; j++) {
				strcat(elementString,AtomicNumberToSymbol(composition->layers[i].Z[j]));
				if (j != composition->layers[i].n_elements-1) {
					strcat(elementString,", ");
				}
			}
			if (lw->matrixKind == COMPOSITION) {
				gtk_list_store_set(store, &(lw->iter),
					N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,composition->layers[i].density,
					THICKNESS_COLUMN,composition->layers[i].thickness,
					REFERENCE_COLUMN,(i+1 == composition->reference_layer) ? TRUE : FALSE ,
					-1
					);
			}
			else {
				gtk_list_store_set(store, &(lw->iter),
					N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,composition->layers[i].density,
					THICKNESS_COLUMN,composition->layers[i].thickness,
					-1
					);
			}
	
			free(elementString);
		}
		update_undo_buffer(updateKind, (GtkWidget *) store);	


	}  	
	else
		return;


	return;

}





static void layer_selection_changed_cb (GtkTreeSelection *selection, gpointer data) {
	GtkTreeIter iter,temp_iter;
	GtkTreeModel *model;
	int n_elements;
	gchar *elements;
	double density,thickness;
	gboolean valid;
	int index,nindices;
	struct matrix_data *md = (struct matrix_data *) data;


	if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
#if DEBUG == 1
		gtk_tree_model_get(model, &iter, N_ELEMENTS_COLUMN, &n_elements, ELEMENTS_COLUMN, &elements, DENSITY_COLUMN, &density, THICKNESS_COLUMN, &thickness, -1);
		fprintf(stdout,"n_elements: %i elements: %s density: %lf thickness: %lf\n",n_elements, elements, density, thickness);	
		g_free(elements);
#endif
		valid = gtk_tree_model_get_iter_first(model, &temp_iter);
		index = 0;
		nindices = 0;
		while(valid) {
			if (gtk_tree_selection_iter_is_selected(selection, &temp_iter)) {
#if DEBUG == 1
				fprintf(stdout,"Index: %i\n",nindices);
#endif
				index = nindices;
			}
			nindices++;
			valid = gtk_tree_model_iter_next(model, &temp_iter);
		}

		if (nindices == 1) {
			gtk_widget_set_sensitive(md->downButton,FALSE);
			gtk_widget_set_sensitive(md->bottomButton,FALSE);
			gtk_widget_set_sensitive(md->upButton,FALSE);
			gtk_widget_set_sensitive(md->topButton,FALSE);
		}
		else {
			if (index == 0) {
				gtk_widget_set_sensitive(md->downButton,TRUE);
				gtk_widget_set_sensitive(md->bottomButton,TRUE);
				gtk_widget_set_sensitive(md->upButton,FALSE);
				gtk_widget_set_sensitive(md->topButton,FALSE);
			}
			else if(index == nindices-1) {
				gtk_widget_set_sensitive(md->downButton,FALSE);
				gtk_widget_set_sensitive(md->bottomButton,FALSE);
				gtk_widget_set_sensitive(md->upButton,TRUE);
				gtk_widget_set_sensitive(md->topButton,TRUE);
			}
			else {
				gtk_widget_set_sensitive(md->downButton,TRUE);
				gtk_widget_set_sensitive(md->bottomButton,TRUE);
				gtk_widget_set_sensitive(md->upButton,TRUE);
				gtk_widget_set_sensitive(md->topButton,TRUE);
			}
		}
		gtk_widget_set_sensitive(md->deleteButton,TRUE);
		gtk_widget_set_sensitive(md->editButton,TRUE);
		gtk_widget_set_sensitive(md->addButton,TRUE);

	}
	else {
		gtk_widget_set_sensitive(md->downButton,FALSE);
		gtk_widget_set_sensitive(md->bottomButton,FALSE);
		gtk_widget_set_sensitive(md->upButton,FALSE);
		gtk_widget_set_sensitive(md->topButton,FALSE);
		gtk_widget_set_sensitive(md->deleteButton,FALSE);
		gtk_widget_set_sensitive(md->editButton,FALSE);
		gtk_widget_set_sensitive(md->addButton,TRUE);
	}


	return;
} 

struct matrix_reorder {
	int kind;
	GtkTreeSelection *select;
	GtkListStore *store;
	GtkWidget *addButton;
	GtkWidget *editButton;
	GtkWidget *deleteButton;
	GtkWidget *topButton;
	GtkWidget *upButton;
	GtkWidget *downButton;
	GtkWidget *bottomButton;
};


void layer_reordering_cb(GtkTreeModel *tree_model, GtkTreePath  *path,  GtkTreeIter  *iter, gpointer new_order, gpointer user_data) {
	struct matrix_reorder *mr = (struct matrix_reorder *) user_data;	
	GtkTreeModel *model;
	GtkTreeIter iter2,temp_iter;
	int index, nindices;
	gboolean valid;

#if DEBUG == 1
	fprintf(stdout,"Layer reordering detected\n");
#endif

	//after pushing the move buttons... their sensivity needs to be checked
	if (gtk_tree_selection_get_selected(mr->select, &model, &iter2)) {
		valid = gtk_tree_model_get_iter_first(model, &temp_iter);
		index = 0;
		nindices = 0;
		while(valid) {
			if (gtk_tree_selection_iter_is_selected(mr->select, &temp_iter)) {
#if DEBUG == 1
				fprintf(stdout,"Index: %i\n",nindices);
#endif
				index = nindices;
			}
			nindices++;
			valid = gtk_tree_model_iter_next(model, &temp_iter);
		}

		if (index == 0) {
			gtk_widget_set_sensitive(mr->downButton,TRUE);
			gtk_widget_set_sensitive(mr->bottomButton,TRUE);
			gtk_widget_set_sensitive(mr->upButton,FALSE);
			gtk_widget_set_sensitive(mr->topButton,FALSE);
		}
		else if(index == nindices-1) {
			gtk_widget_set_sensitive(mr->downButton,FALSE);
			gtk_widget_set_sensitive(mr->bottomButton,FALSE);
			gtk_widget_set_sensitive(mr->upButton,TRUE);
			gtk_widget_set_sensitive(mr->topButton,TRUE);
		}
		else {
			gtk_widget_set_sensitive(mr->downButton,TRUE);
			gtk_widget_set_sensitive(mr->bottomButton,TRUE);
			gtk_widget_set_sensitive(mr->upButton,TRUE);
			gtk_widget_set_sensitive(mr->topButton,TRUE);
		}
		gtk_widget_set_sensitive(mr->deleteButton,TRUE);
		gtk_widget_set_sensitive(mr->editButton,TRUE);
		
	}
}

enum {
	BUTTON_UP,
	BUTTON_DOWN,
	BUTTON_TOP,
	BUTTON_BOTTOM,
	BUTTON_DELETE,
	BUTTON_EDIT,
	BUTTON_ADD,
};

struct matrix_button {
	int buttonKind;
	int matrixKind;
	GtkTreeSelection *select;
	GtkListStore *store;
};


static void layers_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct matrix_button *mb= (struct matrix_button *) data;
	GtkTreeIter iter,temp_iter;
	GtkTreeModel *model;
	gboolean valid;
	int index,nindices;
	int n_elements;
	gchar *elements;
	double density,thickness;
	gint *indices;
	int i;
	struct xmi_layer temp;
	struct xmi_composition *composition;

#if DEBUG == 1
	if (mb->buttonKind == BUTTON_TOP)
		fprintf(stdout,"Top button clicked\n" );
	else if (mb->buttonKind == BUTTON_DOWN)
		fprintf(stdout,"Down button clicked\n" );
	else if (mb->buttonKind == BUTTON_BOTTOM)
		fprintf(stdout,"Bottom button clicked\n" );
	else if (mb->buttonKind == BUTTON_UP)
		fprintf(stdout,"Up button clicked\n" );
	else if (mb->buttonKind == BUTTON_DELETE)
		fprintf(stdout,"Delete button clicked\n" );
#endif
	if (mb->matrixKind == COMPOSITION)
		composition = compositionS;
	else if (mb->matrixKind == EXC_COMPOSITION)
		composition = exc_compositionS;
	else if (mb->matrixKind == DET_COMPOSITION)
		composition = det_compositionS;
	else if (mb->matrixKind == CRYSTAL_COMPOSITION)
		composition = crystal_compositionS;

	if (mb->buttonKind == BUTTON_ADD) {
		//add line... testing only for now...
#if DEBUG == 1
		fprintf(stdout,"window pointer before showing it: %p\n",layerW->window);
#endif
		layer = NULL;
		layerW->AddOrEdit = LW_ADD;
		layerW->matrixKind = mb->matrixKind;
		gtk_widget_show_all(layerW->window);
#if DEBUG == 1
		fprintf(stdout,"After widget show command\n" );
#endif
		return;
	}

	//the olde switcharooooo
	if (gtk_tree_selection_get_selected(mb->select, &model, &iter)) {
		valid = gtk_tree_model_get_iter_first(model, &temp_iter);
		index = 0;
		nindices = 0;
		while(valid) {
			if (gtk_tree_selection_iter_is_selected(mb->select, &temp_iter)) {
#if DEBUG == 1
				fprintf(stdout,"Index: %i\n",nindices);
#endif
				index = nindices;
			}
			nindices++;
			valid = gtk_tree_model_iter_next(model, &temp_iter);
		}
		indices = (gint *) malloc(sizeof(gint)*nindices);
		for (i = 0 ; i < nindices ; i++) 
			indices[i] = i;

		if (mb->buttonKind == BUTTON_TOP) {
			temp = composition->layers[index]; 
			indices[0] = index;
			for (i = 1 ; i < index+1 ; i++) {
				indices[i] = i-1;

			}
			for (i = index ; i > 0 ; i--) {
				composition->layers[i] = composition->layers[i-1];
			}
			composition->layers[0] = temp;

			if (mb->matrixKind == COMPOSITION) {
				if (composition->reference_layer == index+1) {
					//reference_layer is moved...
					composition->reference_layer = 1;
				}
				else if (composition->reference_layer < index+1)
					composition->reference_layer++;
			}
			gtk_list_store_reorder(mb->store,indices);	
			if (mb->matrixKind == COMPOSITION)
				update_undo_buffer(COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == EXC_COMPOSITION)
				update_undo_buffer(EXC_COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == DET_COMPOSITION)
				update_undo_buffer(DET_COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == CRYSTAL_COMPOSITION)
				update_undo_buffer(CRYSTAL_COMPOSITION_ORDER, (GtkWidget*) mb->store);
		}
		else if (mb->buttonKind == BUTTON_BOTTOM) {
			temp = composition->layers[index]; 
			indices[nindices-1] = index;
			for (i = nindices-2 ; i >= index ; i--)
				indices[i] = i+1;
			for (i = index ; i < nindices-1 ; i++)
				composition->layers[i] = composition->layers[i+1];
			composition->layers[nindices-1] = temp;

			if (mb->matrixKind == COMPOSITION) {
				if (composition->reference_layer == index+1) {
					//reference_layer is moved...
					composition->reference_layer = nindices;
				}
				else if (composition->reference_layer > index-1)
					composition->reference_layer--;
			}	
			gtk_list_store_reorder(mb->store,indices);	
			if (mb->matrixKind == COMPOSITION)
				update_undo_buffer(COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == EXC_COMPOSITION)
				update_undo_buffer(EXC_COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == DET_COMPOSITION)
				update_undo_buffer(DET_COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == CRYSTAL_COMPOSITION)
				update_undo_buffer(CRYSTAL_COMPOSITION_ORDER, (GtkWidget*) mb->store);
		}
		else if (mb->buttonKind == BUTTON_UP) {
			temp = composition->layers[index]; 
			composition->layers[index] = composition->layers[index-1];
			composition->layers[index-1] = temp;
			indices[index-1] = index;
			indices[index] = index-1;

			if (mb->matrixKind == COMPOSITION) {
				if (composition->reference_layer == index+1) {
					//reference_layer is moved...
					composition->reference_layer = index;
				}
				else if (composition->reference_layer == index)
					composition->reference_layer++;
			}
			gtk_list_store_reorder(mb->store,indices);	
			if (mb->matrixKind == COMPOSITION)
				update_undo_buffer(COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == EXC_COMPOSITION)
				update_undo_buffer(EXC_COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == DET_COMPOSITION)
				update_undo_buffer(DET_COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == CRYSTAL_COMPOSITION)
				update_undo_buffer(CRYSTAL_COMPOSITION_ORDER, (GtkWidget*) mb->store);
		}
		else if (mb->buttonKind == BUTTON_DOWN) {
			temp = composition->layers[index]; 
			composition->layers[index] = composition->layers[index+1];
			composition->layers[index+1] = temp;
			indices[index+1] = index;
			indices[index] = index+1;
			if (mb->matrixKind == COMPOSITION) {
				if (composition->reference_layer == index+1) {
					//reference_layer is moved...
					composition->reference_layer = index+2;
				}
				else if (composition->reference_layer == index+2)
					composition->reference_layer--;
			}
			gtk_list_store_reorder(mb->store,indices);	
			if (mb->matrixKind == COMPOSITION)
				update_undo_buffer(COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == EXC_COMPOSITION)
				update_undo_buffer(EXC_COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == DET_COMPOSITION)
				update_undo_buffer(DET_COMPOSITION_ORDER, (GtkWidget*) mb->store);
			else if (mb->matrixKind == CRYSTAL_COMPOSITION)
				update_undo_buffer(CRYSTAL_COMPOSITION_ORDER, (GtkWidget*) mb->store);
		}
		else if (mb->buttonKind == BUTTON_DELETE) {
			//delete the selected line
			//watch out for reference layer... for now... leave it to the user
			//update composition
			xmi_free_layer(composition->layers+index);
			for (i = index ;  i < nindices ; i++)
				composition->layers[i] = composition->layers[i+1];
			composition->layers = (struct xmi_layer*) realloc(composition->layers, sizeof(struct xmi_layer)*(nindices-1));
			composition->n_layers--;
			gtk_list_store_remove(mb->store,&iter);
			if (mb->matrixKind == COMPOSITION)
				update_undo_buffer(COMPOSITION_DELETE, (GtkWidget*) mb->store);
			else if (mb->matrixKind == EXC_COMPOSITION)
				update_undo_buffer(EXC_COMPOSITION_DELETE, (GtkWidget*) mb->store);
			else if (mb->matrixKind == DET_COMPOSITION)
				update_undo_buffer(DET_COMPOSITION_DELETE, (GtkWidget*) mb->store);
			else if (mb->matrixKind == CRYSTAL_COMPOSITION)
				update_undo_buffer(CRYSTAL_COMPOSITION_DELETE, (GtkWidget*) mb->store);
		
		}
		else if (mb->buttonKind == BUTTON_EDIT) {
			//add line... testing only for now...
#if DEBUG == 1
			fprintf(stdout,"window pointer before showing it: %p\n",layerW->window);
#endif
			//should work with a copy instead of the real thing
		//	layer = composition->layers+index;	
			xmi_copy_layer(composition->layers+index,&layer);
			layerW->AddOrEdit = LW_EDIT;
			layerW->matrixKind = mb->matrixKind;
			layerW->layerNumber = index;
			layerW->iter = iter;
			gtk_widget_show_all(layerW->window);
		}


		else {
			fprintf(stdout,"Unknown button clicked\n");
			exit(1);
		}

	}


	return;
}

struct reference_toggle {
	GtkListStore *store;
	GtkTreeModel *model;
};

static void reference_layer_toggled_cb(GtkCellRendererToggle *renderer, gchar *path, gpointer data) {
	struct reference_toggle *rt = (struct reference_toggle *) data;
	GtkTreeIter iter,iter2;
	GValue reference = {0};
	int i;
	gboolean valid;
	GtkTreePath *tpath, *tpath2;
	int new_reference;


#if DEBUG == 1
	fprintf(stdout,"reference_layer_toggled: path: %s\n",path);
#endif

	tpath=gtk_tree_path_new_from_string(path);
	
	//convert path to iter
	if (gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(rt->store), &iter, path) == FALSE) {
		fprintf(stdout,"Error in reference_layer_toggled_cb\n");
		exit(1);
	}

	valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(rt->store), &iter2);

	while (valid) {
		tpath2=gtk_tree_model_get_path(GTK_TREE_MODEL(rt->store), &iter2);
		g_value_init(&reference, G_TYPE_BOOLEAN);
		g_value_set_boolean(&reference, (gtk_tree_path_compare(tpath,tpath2) == 0)?TRUE:FALSE);
		gtk_list_store_set_value(rt->store, &iter2, REFERENCE_COLUMN, &reference);
		g_value_unset(&reference);
		gtk_tree_path_free(tpath2);	
		valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(rt->store), &iter2);
	}
	gtk_tree_path_free(tpath);	

	//update compositionS if necessary
	new_reference = 1+ (int) g_ascii_strtoll(path, NULL, 10);
	if (new_reference != compositionS->reference_layer) {
		compositionS->reference_layer = new_reference;
		//update undo buffer
		update_undo_buffer(COMPOSITION_REFERENCE, (GtkWidget *) rt->store);
	}
}

void matrix_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer user_data) {
	struct matrix_button *mb = (struct matrix_button *) user_data;
	gint *indices;
	gint depth;
	struct xmi_composition *composition;
	GtkTreeIter iter;


#if DEBUG == 1
	fprintf(stdout,"row activation detected\n");
#endif
	//indices = gtk_tree_path_get_indices_with_depth(path,&depth);
	indices = gtk_tree_path_get_indices(path);
	depth = gtk_tree_path_get_depth(path);


#if DEBUG == 1
	fprintf(stdout,"depth: %i\n",depth);
	fprintf(stdout,"indices: %i\n",indices[0]);
#endif
	if (mb->matrixKind == COMPOSITION)
		composition = compositionS;
	else if (mb->matrixKind == EXC_COMPOSITION)
		composition = exc_compositionS;
	else if (mb->matrixKind == DET_COMPOSITION)
		composition = det_compositionS;
	else if (mb->matrixKind == CRYSTAL_COMPOSITION)
		composition = crystal_compositionS;


	gtk_tree_model_get_iter(GTK_TREE_MODEL(mb->store), &iter, path);
	xmi_copy_layer(composition->layers+indices[0],&layer);
	layerW->AddOrEdit = LW_EDIT;
	layerW->matrixKind = mb->matrixKind;
	layerW->layerNumber = indices[0];
	layerW->iter = iter;
	gtk_widget_show_all(layerW->window);

	return;
}

static void layer_print_double(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gdouble value;
	gchar *double_text;

	gtk_tree_model_get(tree_model,iter, GPOINTER_TO_INT(data), &value,-1);

	double_text = g_strdup_printf("%lg",value);
	g_object_set(G_OBJECT(renderer), "text", double_text, NULL);

	g_free(double_text);

	return;

}



GtkWidget *initialize_matrix(struct xmi_composition *composition, int kind) {
	GtkListStore *store;
	GtkTreeIter iter;
	GtkWidget *tree;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	char *elementString;
	GtkTreeSelection *select;
	GtkWidget *mainbox, *mainbox2;
	GtkWidget *buttonbox;
	GtkWidget *addButton;
	GtkWidget *editButton;
	GtkWidget *deleteButton;
	GtkWidget *topButton;
	GtkWidget *upButton;
	GtkWidget *downButton;
	GtkWidget *bottomButton;
	GtkRequisition size;
	struct matrix_data *md;
	struct matrix_button *mb;
	struct reference_toggle *rt;
	struct matrix_reorder *mr;
	GtkWidget *scrolledWindow;

	int i,j;

	size.width = 350;
	size.height = 80;
	

	mainbox = gtk_hbox_new(FALSE, 5);
	mainbox2 = gtk_vbox_new(FALSE, 5);


	if (kind == COMPOSITION)
		store = gtk_list_store_new(NCOLUMNS_MATRIX, G_TYPE_INT, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_BOOLEAN);
	else
		store = gtk_list_store_new(NCOLUMNS_MATRIX-1, G_TYPE_INT, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE);



	for (i=0 ; i < composition->n_layers ; i++) {
		gtk_list_store_append(store, &iter);
		elementString = (char *) malloc(sizeof(char)* (composition->layers[i].n_elements*5));
		elementString[0] = '\0';
		for (j = 0 ; j < composition->layers[i].n_elements ; j++) {
			strcat(elementString,AtomicNumberToSymbol(composition->layers[i].Z[j]));
			if (j != composition->layers[i].n_elements-1) {
				strcat(elementString,", ");
			}
		}
		if (kind == COMPOSITION) {
			gtk_list_store_set(store, &iter,
				N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
				ELEMENTS_COLUMN,elementString,
				DENSITY_COLUMN,composition->layers[i].density,
				THICKNESS_COLUMN,composition->layers[i].thickness,
				REFERENCE_COLUMN,(i+1 == composition->reference_layer) ? TRUE : FALSE,
				-1
				);
		}
		else {
			gtk_list_store_set(store, &iter,
				N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
				ELEMENTS_COLUMN,elementString,
				DENSITY_COLUMN,composition->layers[i].density,
				THICKNESS_COLUMN,composition->layers[i].thickness,
				-1
				);
		}
	
		free(elementString);
	}

	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Number of elements", renderer,"text",N_ELEMENTS_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Elements", renderer,"text",ELEMENTS_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//need to come up with a way to get pango markup here...
	//column = gtk_tree_view_column_new_with_attributes("Density (g/cm3)", renderer,"text",DENSITY_COLUMN,NULL);
	//gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Density (g/cm3)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, layer_print_double, GINT_TO_POINTER(DENSITY_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	g_object_set(G_OBJECT(renderer), "xalign", 0.5, NULL);
	//g_object_set(G_OBJECT(renderer), "yalign", 0.5, NULL);
	//column = gtk_tree_view_column_new_with_attributes("Thickness (cm)", renderer,"text",THICKNESS_COLUMN,NULL);
	//gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Thickness (cm)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, layer_print_double, GINT_TO_POINTER(THICKNESS_COLUMN),NULL);

	
	if (kind == COMPOSITION) {
		rt = (struct reference_toggle *) malloc(sizeof(struct reference_toggle));
		rt->store = store;
		rt->model = (GtkTreeModel *) tree;
		renderer = gtk_cell_renderer_toggle_new();
		my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
		gtk_cell_renderer_toggle_set_radio(GTK_CELL_RENDERER_TOGGLE(renderer), TRUE);
		my_gtk_cell_renderer_toggle_set_activatable(GTK_CELL_RENDERER_TOGGLE(renderer), TRUE);
		g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(reference_layer_toggled_cb), rt);
		column = gtk_tree_view_column_new_with_attributes("Reference layer?", renderer,"active",REFERENCE_COLUMN,NULL);
		gtk_tree_view_column_set_resizable(column,TRUE);
		gtk_tree_view_column_set_alignment(column, 0.5);
		gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	}
	scrolledWindow = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	//gtk_widget_size_request(scrolledWindow,&size);
	gtk_widget_set_size_request(scrolledWindow, 550,100);
	gtk_container_add(GTK_CONTAINER(scrolledWindow), tree);
	gtk_box_pack_start(GTK_BOX(mainbox),scrolledWindow, FALSE, FALSE,3 );
	
	select = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));

	buttonbox = gtk_vbox_new(FALSE, 5);
	topButton = gtk_button_new_from_stock(GTK_STOCK_GOTO_TOP);
	upButton = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
	downButton = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
	bottomButton = gtk_button_new_from_stock(GTK_STOCK_GOTO_BOTTOM);
	gtk_box_pack_start(GTK_BOX(buttonbox), topButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), upButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), downButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), bottomButton, FALSE, FALSE, 3);
	//set insensitive at start
	gtk_widget_set_sensitive(topButton, FALSE);
	gtk_widget_set_sensitive(upButton, FALSE);
	gtk_widget_set_sensitive(downButton, FALSE);
	gtk_widget_set_sensitive(bottomButton, FALSE);
	//signals -> let's create some nice memory leaks..
	//TOP button
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_TOP;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(topButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	//BOTTOM button
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_BOTTOM;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(bottomButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	//UP button
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_UP;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(upButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	//DOWN button
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_DOWN;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(downButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);


	



	gtk_box_pack_start(GTK_BOX(mainbox), buttonbox, FALSE, FALSE, 2);


	buttonbox = gtk_vbox_new(FALSE, 5);

	addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
	editButton = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	deleteButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);

	gtk_box_pack_start(GTK_BOX(buttonbox), addButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), editButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), deleteButton, TRUE, FALSE, 3);

	gtk_box_pack_start(GTK_BOX(mainbox), buttonbox, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainbox2), mainbox, FALSE, FALSE, 2);

	gtk_widget_set_sensitive(editButton, FALSE);
	gtk_widget_set_sensitive(deleteButton, FALSE);
	//DELETE
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_DELETE;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(deleteButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);

	//ADD
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_ADD;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(addButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);

	//EDIT
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_EDIT;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(editButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	g_signal_connect(G_OBJECT(tree), "row-activated", G_CALLBACK(matrix_row_activated_cb), (gpointer) mb);





	md = (struct matrix_data*) malloc(sizeof(struct matrix_data));
	md->topButton = topButton;
	md->upButton = upButton;
	md->downButton = downButton;
	md->bottomButton = bottomButton;
	md->editButton = editButton;
	md->addButton = addButton;
	md->deleteButton = deleteButton;

	gtk_tree_selection_set_mode(select,GTK_SELECTION_SINGLE);
	g_signal_connect(G_OBJECT(select), "changed",
			G_CALLBACK(layer_selection_changed_cb),
			(gpointer) md
		);

	mr = (struct matrix_reorder *) malloc(sizeof(struct matrix_reorder));
	mr->kind=kind;
	mr->select=select;
	mr->store=store;
	mr->topButton = topButton;
	mr->upButton = upButton;
	mr->downButton = downButton;
	mr->bottomButton = bottomButton;
	mr->editButton = editButton;
	mr->addButton = addButton;
	mr->deleteButton = deleteButton;
	g_signal_connect(G_OBJECT(store), "rows-reordered", G_CALLBACK(layer_reordering_cb), (gpointer) mr);	
	if (kind == COMPOSITION) {
		compositionW = tree;
		xmi_copy_composition(composition,&compositionS);	
		compositionL = store;
	}
	else if (kind == EXC_COMPOSITION) {
		exc_compositionW = tree;
		xmi_copy_composition(composition,&exc_compositionS);	
		exc_compositionL = store;
	}
	else if (kind == DET_COMPOSITION) {
		det_compositionW = tree;
		xmi_copy_composition(composition,&det_compositionS);	
		det_compositionL = store;
	}
	else if (kind == CRYSTAL_COMPOSITION) {
		crystal_compositionW = tree;
		xmi_copy_composition(composition,&crystal_compositionS);	
		crystal_compositionL = store;
	}
	return mainbox2;
}






static void undo_menu_click(GtkWidget *widget, gpointer data) {
	char buffer[512];
	GtkListStore *store;
	GtkTreeIter iter;
	char *elementString;
	int i,j;


	//restore previous state
	//current-- && copy changes
#if DEBUG == 1
	fprintf(stdout,"Undo button clicked\n");
#endif


	switch (current->kind) {
		case OUTPUTFILE:
			gtk_entry_set_text(GTK_ENTRY((current)->widget),(current-1)->xi->general->outputfile);
			break;
		case N_PHOTONS_INTERVAL:
			g_sprintf(buffer,"%li",(current-1)->xi->general->n_photons_interval);
#if DEBUG == 1
			fprintf(stdout,"n_photons_interval undo to %s\n",buffer);
#endif
			g_signal_handler_block(G_OBJECT(current->widget), n_photons_intervalG);
			gtk_entry_set_text(GTK_ENTRY(current->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(current->widget), n_photons_intervalG);
			break;
		case N_PHOTONS_LINE:
			g_sprintf(buffer,"%li",(current-1)->xi->general->n_photons_line);
			g_signal_handler_block(G_OBJECT(current->widget), n_photons_lineG);
			gtk_entry_set_text(GTK_ENTRY(current->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(current->widget), n_photons_lineG);
			break;
		case N_INTERACTIONS_TRAJECTORY:
			g_sprintf(buffer,"%i",(current-1)->xi->general->n_interactions_trajectory);
			g_signal_handler_block(G_OBJECT(current->widget), n_interactions_trajectoryG);
			gtk_entry_set_text(GTK_ENTRY(current->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(current->widget), n_interactions_trajectoryG);
			break;
		case COMPOSITION_ORDER:
		case COMPOSITION_REFERENCE:
		case COMPOSITION_DELETE:
		case COMPOSITION_ADD:
		case COMPOSITION_EDIT:
			//clear list and repopulate
			store = (GtkListStore *) (current)->widget;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current-1)->xi->composition->n_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = (char *) malloc(sizeof(char)* ((current-1)->xi->composition->layers[i].n_elements*5));
				elementString[0] = '\0';
				for (j = 0 ; j < (current-1)->xi->composition->layers[i].n_elements ; j++) {
					strcat(elementString,AtomicNumberToSymbol((current-1)->xi->composition->layers[i].Z[j]));
					if (j != (current-1)->xi->composition->layers[i].n_elements-1) {
						strcat(elementString,", ");
					}
				}
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current-1)->xi->composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current-1)->xi->composition->layers[i].density,
					THICKNESS_COLUMN,(current-1)->xi->composition->layers[i].thickness,
					REFERENCE_COLUMN,(i+1 == (current-1)->xi->composition->reference_layer) ? TRUE : FALSE,
					-1
					);
				free(elementString);
			}
			xmi_free_composition(compositionS);
			xmi_copy_composition((current-1)->xi->composition, &compositionS);
			break;
		case D_SAMPLE_SOURCE:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->d_sample_source);
			g_signal_handler_block(G_OBJECT((current)->widget), d_sample_sourceG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), d_sample_sourceG);
			break;
		case N_SAMPLE_ORIENTATION_X:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->n_sample_orientation[0]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_sample_orientation_xG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_sample_orientation_xG);
			break;
		case N_SAMPLE_ORIENTATION_Y:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->n_sample_orientation[1]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_sample_orientation_yG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_sample_orientation_yG);
			break;
		case N_SAMPLE_ORIENTATION_Z:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->n_sample_orientation[2]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_sample_orientation_zG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_sample_orientation_zG);
			break;
		case P_DETECTOR_WINDOW_X:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->p_detector_window[0]);
			g_signal_handler_block(G_OBJECT((current)->widget), p_detector_window_xG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), p_detector_window_xG);
			break;
		case P_DETECTOR_WINDOW_Y:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->p_detector_window[1]);
			g_signal_handler_block(G_OBJECT((current)->widget), p_detector_window_yG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), p_detector_window_yG);
			break;
		case P_DETECTOR_WINDOW_Z:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->p_detector_window[2]);
			g_signal_handler_block(G_OBJECT((current)->widget), p_detector_window_zG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), p_detector_window_zG);
			break;
		case N_DETECTOR_ORIENTATION_X:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->n_detector_orientation[0]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_detector_orientation_xG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_detector_orientation_xG);
			break;
		case N_DETECTOR_ORIENTATION_Y:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->n_detector_orientation[1]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_detector_orientation_yG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_detector_orientation_yG);
			break;
		case N_DETECTOR_ORIENTATION_Z:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->n_detector_orientation[2]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_detector_orientation_zG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_detector_orientation_zG);
			break;
		case AREA_DETECTOR:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->area_detector);
			g_signal_handler_block(G_OBJECT((current)->widget), area_detectorG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), area_detectorG);
			break;
		case COLLIMATOR_HEIGHT:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->collimator_height);
			g_signal_handler_block(G_OBJECT((current)->widget), collimator_heightG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), collimator_heightG);
			break;
		case COLLIMATOR_DIAMETER:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->collimator_diameter);
			g_signal_handler_block(G_OBJECT((current)->widget), collimator_diameterG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), collimator_diameterG);
			break;
		case D_SOURCE_SLIT:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->d_source_slit);
			g_signal_handler_block(G_OBJECT((current)->widget), d_source_slitG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), d_source_slitG);
			break;
		case SLIT_SIZE_X:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->slit_size_x);
			g_signal_handler_block(G_OBJECT((current)->widget), slit_size_xG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), slit_size_xG);
			break;
		case SLIT_SIZE_Y:
			g_sprintf(buffer,"%lg",(current-1)->xi->geometry->slit_size_y);
			g_signal_handler_block(G_OBJECT((current)->widget), slit_size_yG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), slit_size_yG);
			break;
		case DISCRETE_ENERGY_ADD:
		case DISCRETE_ENERGY_EDIT:
		case DISCRETE_ENERGY_DELETE:
			gtk_list_store_clear(discWidget->store);
			for (i = 0 ; i < (current-1)->xi->excitation->n_discrete ; i++) {
				gtk_list_store_append(discWidget->store, &iter);
				gtk_list_store_set(discWidget->store, &iter,
					ENERGY_COLUMN, (current-1)->xi->excitation->discrete[i].energy,
					HOR_INTENSITY_COLUMN, (current-1)->xi->excitation->discrete[i].horizontal_intensity,
					VER_INTENSITY_COLUMN, (current-1)->xi->excitation->discrete[i].vertical_intensity,
					SIGMA_X_COLUMN, (current-1)->xi->excitation->discrete[i].sigma_x,
					SIGMA_XP_COLUMN,(current-1)->xi->excitation->discrete[i].sigma_xp,
					SIGMA_Y_COLUMN,(current-1)->xi->excitation->discrete[i].sigma_y,
					SIGMA_YP_COLUMN,(current-1)->xi->excitation->discrete[i].sigma_yp,
					-1);
			}
			break;
		case CONTINUOUS_ENERGY_ADD:
		case CONTINUOUS_ENERGY_EDIT:
		case CONTINUOUS_ENERGY_DELETE:
			gtk_list_store_clear(contWidget->store);
			for (i = 0 ; i < (current-1)->xi->excitation->n_continuous ; i++) {
				gtk_list_store_append(contWidget->store, &iter);
				gtk_list_store_set(contWidget->store, &iter,
					ENERGY_COLUMN, (current-1)->xi->excitation->continuous[i].energy,
					HOR_INTENSITY_COLUMN, (current-1)->xi->excitation->continuous[i].horizontal_intensity,
					VER_INTENSITY_COLUMN, (current-1)->xi->excitation->continuous[i].vertical_intensity,
					SIGMA_X_COLUMN, (current-1)->xi->excitation->continuous[i].sigma_x,
					SIGMA_XP_COLUMN,(current-1)->xi->excitation->continuous[i].sigma_xp,
					SIGMA_Y_COLUMN,(current-1)->xi->excitation->continuous[i].sigma_y,
					SIGMA_YP_COLUMN,(current-1)->xi->excitation->continuous[i].sigma_yp,
					-1);
			}
			break;
		case EXC_COMPOSITION_ORDER:
		case EXC_COMPOSITION_DELETE:
		case EXC_COMPOSITION_ADD:
		case EXC_COMPOSITION_EDIT:
			//clear list and repopulate
			store = exc_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current-1)->xi->absorbers->n_exc_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = (char *) malloc(sizeof(char)* ((current-1)->xi->absorbers->exc_layers[i].n_elements*5));
				elementString[0] = '\0';
				for (j = 0 ; j < (current-1)->xi->absorbers->exc_layers[i].n_elements ; j++) {
					strcat(elementString,AtomicNumberToSymbol((current-1)->xi->absorbers->exc_layers[i].Z[j]));
					if (j != (current-1)->xi->absorbers->exc_layers[i].n_elements-1) {
						strcat(elementString,", ");
					}
				}
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current-1)->xi->absorbers->exc_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current-1)->xi->absorbers->exc_layers[i].density,
					THICKNESS_COLUMN,(current-1)->xi->absorbers->exc_layers[i].thickness,
					-1
					);
				free(elementString);
			}
#if DEBUG == 1

#endif
			xmi_free_composition(exc_compositionS);
			xmi_copy_abs_or_crystal2composition((current-1)->xi->absorbers->exc_layers,(current-1)->xi->absorbers->n_exc_layers , &exc_compositionS);
			break;
		case DET_COMPOSITION_ORDER:
		case DET_COMPOSITION_DELETE:
		case DET_COMPOSITION_ADD:
		case DET_COMPOSITION_EDIT:
			//clear list and repopulate
			store = det_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current-1)->xi->absorbers->n_det_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = (char *) malloc(sizeof(char)* ((current-1)->xi->absorbers->det_layers[i].n_elements*5));
				elementString[0] = '\0';
				for (j = 0 ; j < (current-1)->xi->absorbers->det_layers[i].n_elements ; j++) {
					strcat(elementString,AtomicNumberToSymbol((current-1)->xi->absorbers->det_layers[i].Z[j]));
					if (j != (current-1)->xi->absorbers->det_layers[i].n_elements-1) {
						strcat(elementString,", ");
					}
				}
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current-1)->xi->absorbers->det_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current-1)->xi->absorbers->det_layers[i].density,
					THICKNESS_COLUMN,(current-1)->xi->absorbers->det_layers[i].thickness,
					-1
					);
				free(elementString);
			}
			xmi_free_composition(det_compositionS);
			xmi_copy_abs_or_crystal2composition((current-1)->xi->absorbers->det_layers,(current-1)->xi->absorbers->n_det_layers , &det_compositionS);
			break;
		case CRYSTAL_COMPOSITION_ORDER:
		case CRYSTAL_COMPOSITION_DELETE:
		case CRYSTAL_COMPOSITION_ADD:
		case CRYSTAL_COMPOSITION_EDIT:
			//clear list and repopulate
			store = crystal_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current-1)->xi->detector->n_crystal_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = (char *) malloc(sizeof(char)* ((current-1)->xi->detector->crystal_layers[i].n_elements*5));
				elementString[0] = '\0';
				for (j = 0 ; j < (current-1)->xi->detector->crystal_layers[i].n_elements ; j++) {
					strcat(elementString,AtomicNumberToSymbol((current-1)->xi->detector->crystal_layers[i].Z[j]));
					if (j != (current-1)->xi->detector->crystal_layers[i].n_elements-1) {
						strcat(elementString,", ");
					}
				}
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current-1)->xi->detector->crystal_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current-1)->xi->detector->crystal_layers[i].density,
					THICKNESS_COLUMN,(current-1)->xi->detector->crystal_layers[i].thickness,
					-1
					);
				free(elementString);
			}
#if DEBUG == 1

#endif
			xmi_free_composition(crystal_compositionS);
			xmi_copy_abs_or_crystal2composition((current-1)->xi->detector->crystal_layers,(current-1)->xi->detector->n_crystal_layers , &crystal_compositionS);
			break;
		case DETECTOR_TYPE:
			//g_sprintf(buffer,"%lg",(current-1)->xi->detector->detector_type);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_typeG);
			//gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			gtk_combo_box_set_active(GTK_COMBO_BOX((current)->widget),(current-1)->xi->detector->detector_type);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_typeG);
			break;
		case DETECTOR_GAIN:
			g_sprintf(buffer,"%lg",(current-1)->xi->detector->gain);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_gainG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_gainG);
			break;
		case DETECTOR_LIVE_TIME:
			g_sprintf(buffer,"%lg",(current-1)->xi->detector->live_time);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_live_timeG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_live_timeG);
			break;
		case DETECTOR_PULSE_WIDTH:
			g_sprintf(buffer,"%lg",(current-1)->xi->detector->pulse_width);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_pulse_widthG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_pulse_widthG);
			break;
		case DETECTOR_ZERO:
			g_sprintf(buffer,"%lg",(current-1)->xi->detector->zero);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_zeroG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_zeroG);
			break;
		case DETECTOR_NOISE:
			g_sprintf(buffer,"%lg",(current-1)->xi->detector->noise);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_noiseG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_noiseG);
			break;
		case DETECTOR_FANO:
			g_sprintf(buffer,"%lg",(current-1)->xi->detector->fano);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_fanoG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_fanoG);
			break;
		case DETECTOR_MAX_CONVOLUTION_ENERGY:
			g_sprintf(buffer,"%lg",(current-1)->xi->detector->max_convolution_energy);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_max_convolution_energyG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_max_convolution_energyG);
			break;
	}
	if (current-1 != redo_buffer) {
		g_sprintf(buffer,"Undo: %s",(current-1)->message);
		gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);		
		gtk_tool_item_set_tooltip_text(undoT,buffer);
	}
	else {
		g_sprintf(buffer,"Undo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);		
		gtk_tool_item_set_tooltip_text(undoT,buffer);
		gtk_widget_set_sensitive(undoW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
	}
	//update redo
	g_sprintf(buffer,"Redo: %s",current->message);
	gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);		
	gtk_tool_item_set_tooltip_text(redoT,buffer);
	gtk_widget_set_sensitive(redoW,TRUE);
	gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);

	current--;

#if DEBUG == 1
		fprintf(stdout,"After undo click\n");
		fprintf(stdout,"current: %p\n",current);
		fprintf(stdout,"last: %p\n",last);
#endif

	adjust_save_buttons();

}

static void about_click(GtkWidget *widget, gpointer data) {
	fprintf(stdout,"About clicked\n");

	static const gchar * const authors[] = {
		"Tom Schoonjans <Tom.Schoonjans@me.com>",
		"Laszlo Vincze <Laszlo.Vincze@UGent.be>",
		"Vicente Armando Sol\303\251 <sole@esrf.fr>",
		"Philip Brondeel <Philip.Brondeel@UGent.be>",
		"Manuel Sanchez del Rio <srio@esrf.fr>",
		"Claudio Ferrero <ferrero@esrf.fr>",
		NULL
	};

	static const gchar * const artists[] = {
		"Jan Garrevoet <Jan.Garrevoet@UGent.be>",
		NULL
	};

	static const gchar copyright[] = "Copyright \xc2\xa9 2010-2012 Tom Schoonjans, Philip Brondeel and Laszlo Vincze";

	static const gchar comments[] = "XMI-MSIM is a tool for the Monte Carlo simulation of ED-XRF spectrometers";

	GdkPixbuf *logo = NULL;
	gchar *logo_file = NULL;

#if defined(MAC_INTEGRATION)
	xmi_resources_mac_query(XMI_RESOURCES_MAC_LOGO,&logo_file);
#elif defined(G_OS_WIN32)
	xmi_registry_win_query(XMI_REGISTRY_WIN_LOGO,&logo_file);
#else
	logo_file = g_strdup(XMIMSIM_ICON_DEFAULT);
#endif

	GError *error = NULL;
	if (logo_file) {
		logo = gdk_pixbuf_new_from_file_at_scale(logo_file, 100,-1,TRUE,&error);
		if (error != NULL) {
			fprintf(stderr,"Could not load logo in %s: %s\n",logo_file,error->message);
		}

		g_free(logo_file);
	}

	gtk_show_about_dialog(GTK_WINDOW(data),
		"program-name", "XMI-MSIM",
		"authors", authors,
		"comments", comments,
		"copyright", copyright,
		"license","This program comes with ABSOLUTELY NO WARRANTY. It is made available under the terms and conditions specified by version 3 of the GNU General Public License. For details, visit http://www.gnu.org/licenses/gpl.html", 
		"logo", logo,
		"artists", artists,
		"version", VERSION,
		"website", "http://github.com/tschoonj/xmimsim",
		"website-label", "github.com/tschoonj/xmimsim",
		"wrap-license",TRUE,
		NULL
	);
	if (logo)
		g_object_unref(logo);

}

static void redo_menu_click(GtkWidget *widget, gpointer data) {
	char buffer[512];
	GtkListStore *store;
	GtkTreeIter iter;
	char *elementString;
	int i,j;

#if DEBUG == 1
	fprintf(stdout,"Redo button clicked: current kind: %i\n",current->kind);
#endif
	switch ((current+1)->kind) {
		case OUTPUTFILE:
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),(current+1)->xi->general->outputfile);
			break;
		case N_PHOTONS_INTERVAL:
			g_sprintf(buffer,"%li",(current+1)->xi->general->n_photons_interval);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_photons_intervalG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_photons_intervalG);
			break;
		case N_PHOTONS_LINE:
			g_sprintf(buffer,"%li",(current+1)->xi->general->n_photons_line);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_photons_lineG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_photons_lineG);
			break;
		case N_INTERACTIONS_TRAJECTORY:
			g_sprintf(buffer,"%i",(current+1)->xi->general->n_interactions_trajectory);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_interactions_trajectoryG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_interactions_trajectoryG);
			break;
		case COMPOSITION_ORDER:
		case COMPOSITION_REFERENCE:
		case COMPOSITION_DELETE:
		case COMPOSITION_ADD:
		case COMPOSITION_EDIT:
			//clear list and repopulate
			store = (GtkListStore *) (current+1)->widget;
			gtk_list_store_clear(store);
			for (i=0 ; i < (current+1)->xi->composition->n_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = (char *) malloc(sizeof(char)* ((current+1)->xi->composition->layers[i].n_elements*5));
				elementString[0] = '\0';
				for (j = 0 ; j < (current+1)->xi->composition->layers[i].n_elements ; j++) {
					strcat(elementString,AtomicNumberToSymbol((current+1)->xi->composition->layers[i].Z[j]));
					if (j != (current+1)->xi->composition->layers[i].n_elements-1) {
						strcat(elementString,", ");
					}
				}
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current+1)->xi->composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current+1)->xi->composition->layers[i].density,
					THICKNESS_COLUMN,(current+1)->xi->composition->layers[i].thickness,
					REFERENCE_COLUMN,(i+1 == (current+1)->xi->composition->reference_layer) ? TRUE : FALSE,
					-1
					);
				free(elementString);
			}
			xmi_free_composition(compositionS);
			xmi_copy_composition((current+1)->xi->composition, &compositionS);
			break;
		case D_SAMPLE_SOURCE:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->d_sample_source);
			g_signal_handler_block(G_OBJECT((current+1)->widget), d_sample_sourceG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), d_sample_sourceG);
			break;
		case N_SAMPLE_ORIENTATION_X:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->n_sample_orientation[0]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_sample_orientation_xG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_sample_orientation_xG);
			break;
		case N_SAMPLE_ORIENTATION_Y:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->n_sample_orientation[1]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_sample_orientation_yG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_sample_orientation_yG);
			break;
		case N_SAMPLE_ORIENTATION_Z:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->n_sample_orientation[2]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_sample_orientation_zG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_sample_orientation_zG);
			break;
		case P_DETECTOR_WINDOW_X:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->p_detector_window[0]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), p_detector_window_xG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), p_detector_window_xG);
			break;
		case P_DETECTOR_WINDOW_Y:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->p_detector_window[1]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), p_detector_window_yG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), p_detector_window_yG);
			break;
		case P_DETECTOR_WINDOW_Z:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->p_detector_window[2]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), p_detector_window_zG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), p_detector_window_zG);
			break;
		case N_DETECTOR_ORIENTATION_X:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->n_detector_orientation[0]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_detector_orientation_xG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_detector_orientation_xG);
			break;
		case N_DETECTOR_ORIENTATION_Y:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->n_detector_orientation[1]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_detector_orientation_yG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_detector_orientation_yG);
			break;
		case N_DETECTOR_ORIENTATION_Z:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->n_detector_orientation[2]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_detector_orientation_zG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_detector_orientation_zG);
			break;
		case AREA_DETECTOR:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->area_detector);
			g_signal_handler_block(G_OBJECT((current+1)->widget), area_detectorG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), area_detectorG);
			break;
		case COLLIMATOR_HEIGHT:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->collimator_height);
			g_signal_handler_block(G_OBJECT((current+1)->widget), collimator_heightG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), collimator_heightG);
			break;
		case COLLIMATOR_DIAMETER:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->collimator_diameter);
			g_signal_handler_block(G_OBJECT((current+1)->widget), collimator_diameterG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), collimator_diameterG);
			break;
		case D_SOURCE_SLIT:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->d_source_slit);
			g_signal_handler_block(G_OBJECT((current+1)->widget), d_source_slitG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), d_source_slitG);
			break;
		case SLIT_SIZE_X:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->slit_size_x);
			g_signal_handler_block(G_OBJECT((current+1)->widget), slit_size_xG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), slit_size_xG);
			break;
		case SLIT_SIZE_Y:
			g_sprintf(buffer,"%lg",(current+1)->xi->geometry->slit_size_y);
			g_signal_handler_block(G_OBJECT((current+1)->widget), slit_size_yG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), slit_size_yG);
			break;
		case DISCRETE_ENERGY_ADD:
		case DISCRETE_ENERGY_EDIT:
		case DISCRETE_ENERGY_DELETE:
			gtk_list_store_clear(discWidget->store);
			for (i = 0 ; i < (current+1)->xi->excitation->n_discrete ; i++) {
				gtk_list_store_append(discWidget->store, &iter);
				gtk_list_store_set(discWidget->store, &iter,
					ENERGY_COLUMN, (current+1)->xi->excitation->discrete[i].energy,
					HOR_INTENSITY_COLUMN, (current+1)->xi->excitation->discrete[i].horizontal_intensity,
					VER_INTENSITY_COLUMN, (current+1)->xi->excitation->discrete[i].vertical_intensity,
					SIGMA_X_COLUMN, (current+1)->xi->excitation->discrete[i].sigma_x,
					SIGMA_XP_COLUMN,(current+1)->xi->excitation->discrete[i].sigma_xp,
					SIGMA_Y_COLUMN,(current+1)->xi->excitation->discrete[i].sigma_y,
					SIGMA_YP_COLUMN,(current+1)->xi->excitation->discrete[i].sigma_yp,
					-1);
			}
			break;
		case CONTINUOUS_ENERGY_ADD:
		case CONTINUOUS_ENERGY_EDIT:
		case CONTINUOUS_ENERGY_DELETE:
			gtk_list_store_clear(contWidget->store);
			for (i = 0 ; i < (current+1)->xi->excitation->n_continuous ; i++) {
				gtk_list_store_append(contWidget->store, &iter);
				gtk_list_store_set(contWidget->store, &iter,
					ENERGY_COLUMN, (current+1)->xi->excitation->continuous[i].energy,
					HOR_INTENSITY_COLUMN, (current+1)->xi->excitation->continuous[i].horizontal_intensity,
					VER_INTENSITY_COLUMN, (current+1)->xi->excitation->continuous[i].vertical_intensity,
					SIGMA_X_COLUMN, (current+1)->xi->excitation->continuous[i].sigma_x,
					SIGMA_XP_COLUMN,(current+1)->xi->excitation->continuous[i].sigma_xp,
					SIGMA_Y_COLUMN,(current+1)->xi->excitation->continuous[i].sigma_y,
					SIGMA_YP_COLUMN,(current+1)->xi->excitation->continuous[i].sigma_yp,
					-1);
			}
			break;
		case EXC_COMPOSITION_ORDER:
		case EXC_COMPOSITION_DELETE:
		case EXC_COMPOSITION_ADD:
		case EXC_COMPOSITION_EDIT:
			//clear list and repopulate
			store = exc_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current+1)->xi->absorbers->n_exc_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = (char *) malloc(sizeof(char)* ((current+1)->xi->absorbers->exc_layers[i].n_elements*5));
				elementString[0] = '\0';
				for (j = 0 ; j < (current+1)->xi->absorbers->exc_layers[i].n_elements ; j++) {
					strcat(elementString,AtomicNumberToSymbol((current+1)->xi->absorbers->exc_layers[i].Z[j]));
					if (j != (current+1)->xi->absorbers->exc_layers[i].n_elements-1) {
						strcat(elementString,", ");
					}
				}
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current+1)->xi->absorbers->exc_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current+1)->xi->absorbers->exc_layers[i].density,
					THICKNESS_COLUMN,(current+1)->xi->absorbers->exc_layers[i].thickness,
					-1
					);
				free(elementString);
			}
			xmi_free_composition(exc_compositionS);
			xmi_copy_abs_or_crystal2composition((current+1)->xi->absorbers->exc_layers,(current+1)->xi->absorbers->n_exc_layers , &exc_compositionS);
			break;
		case DET_COMPOSITION_ORDER:
		case DET_COMPOSITION_DELETE:
		case DET_COMPOSITION_ADD:
		case DET_COMPOSITION_EDIT:
			//clear list and repopulate
			store = det_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current+1)->xi->absorbers->n_det_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = (char *) malloc(sizeof(char)* ((current+1)->xi->absorbers->det_layers[i].n_elements*5));
				elementString[0] = '\0';
				for (j = 0 ; j < (current+1)->xi->absorbers->det_layers[i].n_elements ; j++) {
					strcat(elementString,AtomicNumberToSymbol((current+1)->xi->absorbers->det_layers[i].Z[j]));
					if (j != (current+1)->xi->absorbers->det_layers[i].n_elements-1) {
						strcat(elementString,", ");
					}
				}
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current+1)->xi->absorbers->det_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current+1)->xi->absorbers->det_layers[i].density,
					THICKNESS_COLUMN,(current+1)->xi->absorbers->det_layers[i].thickness,
					-1
					);
				free(elementString);
			}
			xmi_free_composition(det_compositionS);
			xmi_copy_abs_or_crystal2composition((current+1)->xi->absorbers->det_layers,(current+1)->xi->absorbers->n_det_layers , &det_compositionS);
			break;
		case CRYSTAL_COMPOSITION_ORDER:
		case CRYSTAL_COMPOSITION_DELETE:
		case CRYSTAL_COMPOSITION_ADD:
		case CRYSTAL_COMPOSITION_EDIT:
			//clear list and repopulate
			store = crystal_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current+1)->xi->detector->n_crystal_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = (char *) malloc(sizeof(char)* ((current+1)->xi->detector->crystal_layers[i].n_elements*5));
				elementString[0] = '\0';
				for (j = 0 ; j < (current+1)->xi->detector->crystal_layers[i].n_elements ; j++) {
					strcat(elementString,AtomicNumberToSymbol((current+1)->xi->detector->crystal_layers[i].Z[j]));
					if (j != (current+1)->xi->detector->crystal_layers[i].n_elements-1) {
						strcat(elementString,", ");
					}
				}
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current+1)->xi->detector->crystal_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current+1)->xi->detector->crystal_layers[i].density,
					THICKNESS_COLUMN,(current+1)->xi->detector->crystal_layers[i].thickness,
					-1
					);
				free(elementString);
			}
			xmi_free_composition(crystal_compositionS);
			xmi_copy_abs_or_crystal2composition((current+1)->xi->detector->crystal_layers,(current+1)->xi->detector->n_crystal_layers , &crystal_compositionS);
			break;
		case DETECTOR_TYPE:
			//g_sprintf(buffer,"%lg",(current-1)->xi->detector->detector_type);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_typeG);
			//gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			gtk_combo_box_set_active(GTK_COMBO_BOX((current+1)->widget),(current+1)->xi->detector->detector_type);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_typeG);
			break;
		case DETECTOR_GAIN:
			g_sprintf(buffer,"%lg",(current+1)->xi->detector->gain);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_gainG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_gainG);
			break;
		case DETECTOR_LIVE_TIME:
			g_sprintf(buffer,"%lg",(current+1)->xi->detector->live_time);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_live_timeG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_live_timeG);
			break;
		case DETECTOR_PULSE_WIDTH:
			g_sprintf(buffer,"%lg",(current+1)->xi->detector->pulse_width);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_pulse_widthG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_pulse_widthG);
			break;
		case DETECTOR_ZERO:
			g_sprintf(buffer,"%lg",(current+1)->xi->detector->zero);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_zeroG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_zeroG);
			break;
		case DETECTOR_NOISE:
			g_sprintf(buffer,"%lg",(current+1)->xi->detector->noise);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_noiseG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_noiseG);
			break;
		case DETECTOR_FANO:
			g_sprintf(buffer,"%lg",(current+1)->xi->detector->fano);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_fanoG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_fanoG);
			break;
		case DETECTOR_MAX_CONVOLUTION_ENERGY:
			g_sprintf(buffer,"%lg",(current+1)->xi->detector->max_convolution_energy);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_max_convolution_energyG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_max_convolution_energyG);
			break;
			

	}

	
	g_sprintf(buffer,"Undo: %s",(current+1)->message);
	gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);		
	gtk_tool_item_set_tooltip_text(undoT,buffer);
	gtk_widget_set_sensitive(undoW,TRUE);
	gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);



	current++;
	if (current == last) {
		g_sprintf(buffer,"Redo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);		
		gtk_tool_item_set_tooltip_text(redoT,buffer);
		gtk_widget_set_sensitive(redoW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	}
	else {
		g_sprintf(buffer,"Redo: %s",(current+1)->message);
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);		
		gtk_tool_item_set_tooltip_text(redoT,buffer);
	}

	adjust_save_buttons();
}

static void file_menu_click(GtkWidget *widget, gpointer data) {

#if DEBUG == 1
	g_print("%s\n",(char *) data);
#endif

	if (strcmp((char *)data, "quit") == 0)
		gtk_main_quit();


}

static void detector_type_changed(GtkComboBox *widget, gpointer data) {

	//should always work out
	update_undo_buffer(GPOINTER_TO_INT(data), GTK_WIDGET(widget));

	return;
}


struct val_changed {
	int kind;
	int *check;
};


static gboolean pos_int_changed_current_check(int kind, long new_value) {
	gboolean rv;

#define POS_INT_CHANGED_CURRENT_CHECK(my_kind,my_current) case my_kind: \
		if (new_value == current->xi->my_current) {\
			rv = FALSE;\
		}\
		else {\
			rv = TRUE;\
		}\
		break;	

	switch (kind) {	
		POS_INT_CHANGED_CURRENT_CHECK(N_PHOTONS_INTERVAL,general->n_photons_interval)
		POS_INT_CHANGED_CURRENT_CHECK(N_PHOTONS_LINE,general->n_photons_line)
		POS_INT_CHANGED_CURRENT_CHECK(N_INTERACTIONS_TRAJECTORY,general->n_interactions_trajectory)

		default:
			g_print("Unknown kind detected in pos_int_changed_current_check. Aborting\n");
			exit(1);
	}

	return rv;
}

static gboolean double_changed_current_check(int kind, double new_value) {
	gboolean rv;

#define DOUBLE_CHANGED_CURRENT_CHECK(my_kind,my_current) case my_kind: \
		if (new_value == current->xi->my_current) {\
			rv = FALSE;\
		}\
		else {\
			rv = TRUE;\
		}\
		break;	


	switch (kind) {	
		DOUBLE_CHANGED_CURRENT_CHECK(D_SAMPLE_SOURCE,geometry->d_sample_source)
		DOUBLE_CHANGED_CURRENT_CHECK(AREA_DETECTOR,geometry->area_detector)
		DOUBLE_CHANGED_CURRENT_CHECK(D_SOURCE_SLIT,geometry->d_source_slit)
		DOUBLE_CHANGED_CURRENT_CHECK(SLIT_SIZE_X,geometry->slit_size_x)
		DOUBLE_CHANGED_CURRENT_CHECK(SLIT_SIZE_Y,geometry->slit_size_y)
		DOUBLE_CHANGED_CURRENT_CHECK(DETECTOR_GAIN,detector->gain)
		DOUBLE_CHANGED_CURRENT_CHECK(DETECTOR_LIVE_TIME,detector->live_time)
		DOUBLE_CHANGED_CURRENT_CHECK(DETECTOR_FANO,detector->fano)
		DOUBLE_CHANGED_CURRENT_CHECK(DETECTOR_NOISE,detector->noise)
		DOUBLE_CHANGED_CURRENT_CHECK(DETECTOR_MAX_CONVOLUTION_ENERGY,detector->max_convolution_energy)
		DOUBLE_CHANGED_CURRENT_CHECK(DETECTOR_PULSE_WIDTH,detector->pulse_width)
		DOUBLE_CHANGED_CURRENT_CHECK(COLLIMATOR_HEIGHT,geometry->collimator_height)
		DOUBLE_CHANGED_CURRENT_CHECK(COLLIMATOR_DIAMETER,geometry->collimator_diameter)
		DOUBLE_CHANGED_CURRENT_CHECK(N_SAMPLE_ORIENTATION_X,geometry->n_sample_orientation[0])
		DOUBLE_CHANGED_CURRENT_CHECK(N_SAMPLE_ORIENTATION_Y,geometry->n_sample_orientation[1])
		DOUBLE_CHANGED_CURRENT_CHECK(N_SAMPLE_ORIENTATION_Z,geometry->n_sample_orientation[2])
		DOUBLE_CHANGED_CURRENT_CHECK(P_DETECTOR_WINDOW_X,geometry->p_detector_window[0])
		DOUBLE_CHANGED_CURRENT_CHECK(P_DETECTOR_WINDOW_Y,geometry->p_detector_window[1])
		DOUBLE_CHANGED_CURRENT_CHECK(P_DETECTOR_WINDOW_Z,geometry->p_detector_window[2])
		DOUBLE_CHANGED_CURRENT_CHECK(N_DETECTOR_ORIENTATION_X,geometry->n_detector_orientation[0])
		DOUBLE_CHANGED_CURRENT_CHECK(N_DETECTOR_ORIENTATION_Y,geometry->n_detector_orientation[1])
		DOUBLE_CHANGED_CURRENT_CHECK(N_DETECTOR_ORIENTATION_Z,geometry->n_detector_orientation[2])
		DOUBLE_CHANGED_CURRENT_CHECK(DETECTOR_ZERO,detector->zero)

		default:
			g_print("Unknown kind detected in double_changed_current_check. Aborting\n");
			exit(1);
	}

	return rv;
}

static void double_changed(GtkWidget *widget, gpointer data) {
	struct val_changed *vc = (struct val_changed *) data;

	int kind = vc->kind;
	int *check = vc->check;
	char buffer[512];
	char *textPtr,*endPtr,*lastPtr;

	double value;

	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(widget));
	value=strtod(textPtr, &endPtr);

	lastPtr = textPtr + strlen(textPtr);

	switch (kind) {
		//strict positive
		case D_SAMPLE_SOURCE:
		case AREA_DETECTOR:
		case D_SOURCE_SLIT:
		case SLIT_SIZE_X:
		case SLIT_SIZE_Y:
		case DETECTOR_GAIN:
		case DETECTOR_LIVE_TIME:
		case DETECTOR_FANO:
		case DETECTOR_NOISE:
		case DETECTOR_MAX_CONVOLUTION_ENERGY:
			if (lastPtr == endPtr && value > 0.0) {
				//ok
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&white);
				*check = 1;
				if (double_changed_current_check(kind,value))
					update_undo_buffer(kind, widget);
				else
					adjust_save_buttons();
				if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(redoW)), "Redo") != 0) {
					gtk_widget_set_sensitive(redoW,TRUE);
					gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);
				}
				if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(undoW)), "Undo") != 0) {
					gtk_widget_set_sensitive(undoW,TRUE);
					gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
				}
				return;
			}
			else {
				//bad value
				*check = 0;
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
				gtk_widget_set_sensitive(redoW,FALSE);
				gtk_widget_set_sensitive(undoW,FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
			}
			break;
		//positive
		case DETECTOR_PULSE_WIDTH:
		case COLLIMATOR_HEIGHT:
		case COLLIMATOR_DIAMETER:
			if (lastPtr == endPtr && value >= 0.0) {
				//ok
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&white);
				*check = 1;
				if (double_changed_current_check(kind,value))
					update_undo_buffer(kind, widget);
				else
					adjust_save_buttons();
				if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(redoW)), "Redo") != 0) {
					gtk_widget_set_sensitive(redoW,TRUE);
					gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);
				}
				if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(undoW)), "Undo") != 0) {
					gtk_widget_set_sensitive(undoW,TRUE);
					gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
				}
				return;
			}
			else {
				//bad value
				*check = 0;
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
				gtk_widget_set_sensitive(redoW,FALSE);
				gtk_widget_set_sensitive(undoW,FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
			}
			break;
		
		//no restrictions
		case N_SAMPLE_ORIENTATION_X:
		case N_SAMPLE_ORIENTATION_Y:
		case N_SAMPLE_ORIENTATION_Z:
		case P_DETECTOR_WINDOW_X:
		case P_DETECTOR_WINDOW_Y:
		case P_DETECTOR_WINDOW_Z:
		case N_DETECTOR_ORIENTATION_X:
		case N_DETECTOR_ORIENTATION_Y:
		case N_DETECTOR_ORIENTATION_Z:
		case DETECTOR_ZERO:
			if (lastPtr == endPtr) {
				//ok
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&white);
				*check = 1;
				if (double_changed_current_check(kind,value))
					update_undo_buffer(kind, widget);
				else
					adjust_save_buttons();
				if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(redoW)), "Redo") != 0) {
					gtk_widget_set_sensitive(redoW,TRUE);
					gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);
				}
				if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(undoW)), "Undo") != 0) {
					gtk_widget_set_sensitive(undoW,TRUE);
					gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
				}
				return;
			}
			else {
				//bad value
				*check = 0;
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
				gtk_widget_set_sensitive(redoW,FALSE);
				gtk_widget_set_sensitive(undoW,FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
			}
			break;
		default:
			g_print("Unknown kind detected in double_changed. Aborting\n");
			exit(1);
	}

	gtk_widget_set_sensitive(saveW,FALSE);
	gtk_widget_set_sensitive(save_asW,FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(saveasT),FALSE);

	return;
}

static void comments_changed(GtkWidget *widget, gpointer data) {
	gtk_widget_set_sensitive(saveW,TRUE);
	gtk_widget_set_sensitive(save_asW,TRUE);
	gtk_widget_set_sensitive(GTK_WIDGET(saveT),TRUE);
	gtk_widget_set_sensitive(GTK_WIDGET(saveasT),TRUE);
	
	return;
}


static void pos_int_changed(GtkWidget *widget, gpointer data) {
	struct val_changed *vc = (struct val_changed *) data;

	int kind = vc->kind;
	int *check = vc->check;
	char buffer[512];
	char *textPtr;
	long value;


#if DEBUG == 1
	g_print("Widget: %i\n",kind);
#endif

	switch (kind) {
		case N_PHOTONS_INTERVAL:
		case N_PHOTONS_LINE:
		case N_INTERACTIONS_TRAJECTORY:
			textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(widget));
			if (g_regex_match(pos_int,textPtr,0,NULL) == TRUE ){
				//ok
				value = strtol(textPtr, NULL, 10);
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&white);
				*check = 1;
				if (pos_int_changed_current_check(kind,value))
					update_undo_buffer(kind, widget);
				else
					adjust_save_buttons();
				if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(redoW)), "Redo") != 0) {
					gtk_widget_set_sensitive(redoW,TRUE);
					gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);
				}
				if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(undoW)), "Undo") != 0) {
					gtk_widget_set_sensitive(undoW,TRUE);
					gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
				}
					
				return;
			}
			else {
				//bad value
				*check = 0;
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
				gtk_widget_set_sensitive(redoW,FALSE);
				gtk_widget_set_sensitive(undoW,FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
			}
			break;
		default:
			g_print("Unknown kind detected. Aborting\n");
			exit(1);
	}

	gtk_widget_set_sensitive(saveW,FALSE);
	gtk_widget_set_sensitive(save_asW,FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(saveasT),FALSE);

}



static gboolean delete_event(GtkWidget *widget, GdkEvent *event, gpointer data) {
	//should check if the user maybe would like to save his stuff...
	
#ifdef MAC_INTEGRATION
	quit_program_cb((GtkOSXApplication*) data, widget);
#else
	quit_program_cb(widget, widget);
#endif
	return TRUE;

}

static gboolean dialog_helper_cb(gpointer data) {
	GtkWidget *dialog = (GtkWidget *) data;

	gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_destroy (dialog);
	return FALSE;
}



#ifdef MAC_INTEGRATION

struct osx_load_data {
	GtkWidget *window;
	gchar *path;
};

static gboolean load_from_file_osx_helper_cb(gpointer data) {
	GtkWidget *dialog = NULL;
	struct xmi_input *xi;
	gchar *title;
	struct osx_load_data *old = (struct osx_load_data *) data;
	gchar *filename = old->path;
	NSWindow *qwindow = gdk_quartz_window_get_nswindow(gtk_widget_get_window(old->window));

	fprintf(stdout,"load_from_file_osx_helper_cb called: %s\n",filename);

	//bring window to front if necessary
	if ([NSApp isHidden] == YES)
		[NSApp unhide: nil];
	else if ([qwindow isMiniaturized])
		[qwindow deminiaturize:nil];
	


	if (process_pre_file_operation(old->window) == FALSE)
		return FALSE;

	//check for filetype
	if (strcmp(filename+strlen(filename)-5,".xmsi") == 0) {
		//XMSI file
		gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),input_page);
		if (xmi_read_input_xml(filename, &xi) == 1) {
			//success reading it in...
			change_all_values(xi);
			//reset redo_buffer
			reset_undo_buffer(xi, filename);	
			title = g_path_get_basename(filename);
			update_xmimsim_title_xmsi(title,old->window,filename);
			g_free(title);
			return FALSE;			
		}
		else {
			dialog = gtk_message_dialog_new (GTK_WINDOW(old->window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
		       		GTK_MESSAGE_ERROR,
		       		GTK_BUTTONS_CLOSE,
		       		"Could not read file %s: model is incomplete/invalid",filename
	                	);
	     		gtk_dialog_run (GTK_DIALOG (dialog));
			gtk_widget_destroy(dialog);
		}
	}
	else if (strcmp(filename+strlen(filename)-5,".xmso") == 0) {
		//XMSO file
		gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),results_page);
		if (plot_spectra_from_file(filename) == 1) {
			gchar *temp_base = g_path_get_basename(filename);
			update_xmimsim_title_xmso(temp_base, old->window, filename);
			g_free(temp_base);
			return FALSE;			
		}
		else {
			dialog = gtk_message_dialog_new (GTK_WINDOW(old->window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
		       		GTK_MESSAGE_ERROR,
		       		GTK_BUTTONS_CLOSE,
		       		"Could not read file %s",filename
	               	);
	     		gtk_dialog_run (GTK_DIALOG (dialog));
			gtk_widget_destroy (dialog);
		}
	}
	return FALSE;
}


static gboolean load_from_file_osx_cb(GtkOSXApplication *app, gchar *path, gpointer data) {
	struct osx_load_data *old = (struct osx_load_data *) malloc(sizeof(struct osx_load_data));

	old->window = (GtkWidget *) data;
	old->path = g_strdup(path);

	g_idle_add(load_from_file_osx_helper_cb, (gpointer) old);

	g_fprintf(stdout,"OSX File open event: %s\n",path);


	return TRUE;
}
#endif
void reset_undo_buffer(struct xmi_input *xi_new, char *filename) {
	struct undo_single *iter;
	char buffer[10];

#if DEBUG == 1
	fprintf(stdout,"resetting undo_buffer\n");
#endif


	for (iter = redo_buffer ; iter <= current ; iter++) {
		free(iter->filename);
		xmi_free_input(iter->xi);
	}
	free(redo_buffer);
	redo_buffer = (struct undo_single *) malloc(sizeof(struct undo_single));
	current = redo_buffer;
	last = redo_buffer;
	redo_buffer->filename = strdup(filename);
	redo_buffer->xi = xi_new;
	if (filename != NULL) {
		if (last_saved != NULL) {
			free(last_saved->filename);
			xmi_free_input(last_saved->xi);
			free(last_saved);
		}
		last_saved = (struct undo_single *) malloc(sizeof(struct undo_single));
		xmi_copy_input(xi_new, &(last_saved->xi));
		last_saved->filename = strdup(filename);
		gtk_widget_set_sensitive(save_asW,TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveasT),TRUE);
	}
	//clear undo/redo messages
	gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	gtk_widget_set_sensitive(undoW,FALSE);
	gtk_widget_set_sensitive(redoW,FALSE);
	g_sprintf(buffer,"Undo");
	gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);
	gtk_tool_item_set_tooltip_text(undoT,buffer);
	g_sprintf(buffer,"Redo");
	gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);
	gtk_tool_item_set_tooltip_text(redoT,buffer);

	return;
}



void update_undo_buffer(int kind, GtkWidget *widget) {
	char buffer[512];
	ptrdiff_t last_diff, current_diff;
	struct undo_single *tempPtr;
	int i, status;

	//two cases... 
	//current == last (NO REDO(s) used)
	//last > current
	
	last_diff = last - redo_buffer;
	current_diff = current - redo_buffer;
	
	if (last > current) {
#if DEBUG == 1
		fprintf(stdout,"last > current\n");
#endif
		for (tempPtr = current+1 ; tempPtr == last ; tempPtr++) {
			xmi_free_input(tempPtr->xi);
		}
		//disable redo
		g_sprintf(buffer,"Redo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);
		gtk_widget_set_sensitive(redoW,FALSE);
		gtk_tool_item_set_tooltip_text(redoT,buffer);
		gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	}

	redo_buffer = (struct undo_single *) realloc(redo_buffer,(current-redo_buffer+2)*sizeof(struct undo_single));
#if DEBUG == 1
	fprintf(stdout,"After redo_buffer realloc\n");
#endif
	last = redo_buffer + last_diff;
	current = redo_buffer + current_diff;
	last = current+1;
	last->filename=strdup(UNLIKELY_FILENAME);
	xmi_copy_input(current->xi, &(last->xi));
	switch (kind) {
		case OUTPUTFILE:
			strcpy(last->message,"selection of outputfile");
			free(last->xi->general->outputfile);
			last->xi->general->outputfile=strdup(gtk_entry_get_text(GTK_ENTRY(widget)));
			last->kind = kind;
			last->widget = widget;
			break;
		case N_PHOTONS_INTERVAL:
			strcpy(last->message,"change of number of photons per interval");
			last->xi->general->n_photons_interval = strtol((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL,10);	
			last->kind = kind;
			last->widget = widget;
			break;
		case N_PHOTONS_LINE:
			strcpy(last->message,"change of number of photons per line");
			last->xi->general->n_photons_line = strtol((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL,10);	
			last->kind = kind;
			last->widget = widget;
			break;
		case N_INTERACTIONS_TRAJECTORY:
			strcpy(last->message,"change of number of interactions per trajectory");
			last->xi->general->n_interactions_trajectory = strtol((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL,10);	
			last->kind = kind;
			last->widget = widget;
			break;
		case COMPOSITION_REFERENCE:
			strcpy(last->message, "change of reference layer");
			last->xi->composition->reference_layer = compositionS->reference_layer;
			last->kind = kind;
			last->widget = widget;
			break;
		case COMPOSITION_ORDER:
			strcpy(last->message,"change of composition ordering");
			xmi_free_composition(last->xi->composition);
			xmi_copy_composition(compositionS, &(last->xi->composition));
			last->kind = kind;
			last->widget = widget;
#if DEBUG == 1
			fprintf(stdout,"store pointer during update: %p\n",last->widget);
#endif
			break;
		case COMPOSITION_DELETE:
			strcpy(last->message,"removal of layer");
			xmi_free_composition(last->xi->composition);
			xmi_copy_composition(compositionS, &(last->xi->composition));
			last->kind = kind;
			last->widget = widget;
			break;
		case COMPOSITION_ADD:
			strcpy(last->message,"addition of layer");
			xmi_free_composition(last->xi->composition);
			xmi_copy_composition(compositionS, &(last->xi->composition));
			last->kind = kind;
			last->widget = widget;
			break;
		case COMPOSITION_EDIT:
			strcpy(last->message,"editing of layer");
			xmi_free_composition(last->xi->composition);
			xmi_copy_composition(compositionS, &(last->xi->composition));
			last->kind = kind;
			last->widget = widget;
			break;
		case D_SAMPLE_SOURCE:
			strcpy(last->message,"change of sample-source distance");
			last->xi->geometry->d_sample_source = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case N_SAMPLE_ORIENTATION_X:
			strcpy(last->message,"change of sample orientation vector x");
			last->xi->geometry->n_sample_orientation[0] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case N_SAMPLE_ORIENTATION_Y:
			strcpy(last->message,"change of sample orientation vector y");
			last->xi->geometry->n_sample_orientation[1] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case N_SAMPLE_ORIENTATION_Z:
			strcpy(last->message,"change of sample orientation vector z");
			last->xi->geometry->n_sample_orientation[2] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case P_DETECTOR_WINDOW_X:
			strcpy(last->message,"change of detector window position x");
			last->xi->geometry->p_detector_window[0] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case P_DETECTOR_WINDOW_Y:
			strcpy(last->message,"change of detector window position y");
			last->xi->geometry->p_detector_window[1] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case P_DETECTOR_WINDOW_Z:
			strcpy(last->message,"change of detector window position z");
			last->xi->geometry->p_detector_window[2] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case N_DETECTOR_ORIENTATION_X:
			strcpy(last->message,"change of detector orientation x");
			last->xi->geometry->n_detector_orientation[0] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case N_DETECTOR_ORIENTATION_Y:
			strcpy(last->message,"change of detector orientation y");
			last->xi->geometry->n_detector_orientation[1] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case N_DETECTOR_ORIENTATION_Z:
			strcpy(last->message,"change of detector orientation z");
			last->xi->geometry->n_detector_orientation[2] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case AREA_DETECTOR:
			strcpy(last->message,"change of active detector area");
			last->xi->geometry->area_detector = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case COLLIMATOR_HEIGHT:
			strcpy(last->message,"change of collimator height");
			last->xi->geometry->collimator_height = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case COLLIMATOR_DIAMETER:
			strcpy(last->message,"change of collimator opening diameter");
			last->xi->geometry->collimator_diameter = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case D_SOURCE_SLIT:
			strcpy(last->message,"change of source-slits distance");
			last->xi->geometry->d_source_slit = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case SLIT_SIZE_X:
			strcpy(last->message,"change of slit size x");
			last->xi->geometry->slit_size_x = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case SLIT_SIZE_Y:
			strcpy(last->message,"change of slit size y");
			last->xi->geometry->slit_size_y = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case DISCRETE_ENERGY_ADD:
			strcpy(last->message,"addition of discrete energy");
			last->kind = kind;
			//realloc discrete energies
			last->xi->excitation->discrete = (struct xmi_energy*) realloc(last->xi->excitation->discrete,sizeof(struct xmi_energy)*++last->xi->excitation->n_discrete);
			last->xi->excitation->discrete[last->xi->excitation->n_discrete-1] = *energy;
			break;
		case DISCRETE_ENERGY_EDIT:
			strcpy(last->message,"editing of discrete energy");
			last->kind = kind;
			last->xi->excitation->discrete[current_index] = *energy;
			break;
		case DISCRETE_ENERGY_DELETE:
			strcpy(last->message,"deletion of discrete energy");
			last->kind = kind;
			for (i = current_index ; i < current_nindices ; i++) {
				last->xi->excitation->discrete[i] = last->xi->excitation->discrete[i+1];	
			}	
			last->xi->excitation->discrete = (struct xmi_energy *) realloc(last->xi->excitation->discrete, sizeof(struct xmi_energy)*last->xi->excitation->n_discrete--);
			break;
		case CONTINUOUS_ENERGY_ADD:
			strcpy(last->message,"addition of continuous energy");
			last->kind = kind;
			//realloc continuous energies
			last->xi->excitation->continuous = (struct xmi_energy*) realloc(last->xi->excitation->continuous,sizeof(struct xmi_energy)*++last->xi->excitation->n_continuous);
			last->xi->excitation->continuous[last->xi->excitation->n_continuous-1] = *energy;
			break;
		case CONTINUOUS_ENERGY_EDIT:
			strcpy(last->message,"editing of continuous energy");
			last->kind = kind;
			last->xi->excitation->continuous[current_index] = *energy;
			break;
		case CONTINUOUS_ENERGY_DELETE:
			strcpy(last->message,"deletion of continuous energy");
			last->kind = kind;
			for (i = current_index ; i < current_nindices ; i++) {
				last->xi->excitation->continuous[i] = last->xi->excitation->continuous[i+1];	
			}	
			last->xi->excitation->continuous = (struct xmi_energy *) realloc(last->xi->excitation->continuous, sizeof(struct xmi_energy)*last->xi->excitation->n_continuous--);
			break;
		case EXC_COMPOSITION_ORDER:
			strcpy(last->message,"change of excitation absorber ordering");
			if (last->xi->absorbers->n_exc_layers > 0) {
				for (i = 0 ; i < last->xi->absorbers->n_exc_layers ; i++)
					xmi_free_layer(last->xi->absorbers->exc_layers+i);
				free(last->xi->absorbers->exc_layers);	
			}
			xmi_copy_composition2abs_or_crystal(exc_compositionS, &(last->xi->absorbers->exc_layers),&(last->xi->absorbers->n_exc_layers));
			last->kind = kind;
			last->widget = widget;
#if DEBUG == 1
			fprintf(stdout,"store pointer during update: %p\n",last->widget);
#endif
			break;
		case EXC_COMPOSITION_DELETE:
			strcpy(last->message,"removal of excitation absorber layer");
			if (last->xi->absorbers->n_exc_layers > 0) {
				for (i = 0 ; i < last->xi->absorbers->n_exc_layers ; i++)
					xmi_free_layer(last->xi->absorbers->exc_layers+i);
				free(last->xi->absorbers->exc_layers);	
			}
			xmi_copy_composition2abs_or_crystal(exc_compositionS, &(last->xi->absorbers->exc_layers),&(last->xi->absorbers->n_exc_layers));
			last->kind = kind;
			last->widget = widget;
			break;
		case EXC_COMPOSITION_ADD:
			strcpy(last->message,"addition of excitation absorber layer");
			if (last->xi->absorbers->n_exc_layers > 0) {
				for (i = 0 ; i < last->xi->absorbers->n_exc_layers ; i++)
					xmi_free_layer(last->xi->absorbers->exc_layers+i);
				free(last->xi->absorbers->exc_layers);	
			}
			xmi_copy_composition2abs_or_crystal(exc_compositionS, &(last->xi->absorbers->exc_layers),&(last->xi->absorbers->n_exc_layers));
			last->kind = kind;
			last->widget = widget;
			break;
		case EXC_COMPOSITION_EDIT:
			strcpy(last->message,"editing of excitation absorber layer");
			if (last->xi->absorbers->n_exc_layers > 0) {
				for (i = 0 ; i < last->xi->absorbers->n_exc_layers ; i++)
					xmi_free_layer(last->xi->absorbers->exc_layers+i);
				free(last->xi->absorbers->exc_layers);	
			}
			xmi_copy_composition2abs_or_crystal(exc_compositionS, &(last->xi->absorbers->exc_layers),&(last->xi->absorbers->n_exc_layers));
			last->kind = kind;
			last->widget = widget;
			break;
		case DET_COMPOSITION_ORDER:
			strcpy(last->message,"change of detector absorber ordering");
			if (last->xi->absorbers->n_det_layers > 0) {
				for (i = 0 ; i < last->xi->absorbers->n_det_layers ; i++)
					xmi_free_layer(last->xi->absorbers->det_layers+i);
				free(last->xi->absorbers->det_layers);	
			}
			xmi_copy_composition2abs_or_crystal(det_compositionS, &(last->xi->absorbers->det_layers),&(last->xi->absorbers->n_det_layers));
			last->kind = kind;
			last->widget = widget;
#if DEBUG == 1
			fprintf(stdout,"store pointer during update: %p\n",last->widget);
#endif
			break;
		case DET_COMPOSITION_DELETE:
			strcpy(last->message,"removal of detector absorber layer");
			if (last->xi->absorbers->n_det_layers > 0) {
				for (i = 0 ; i < last->xi->absorbers->n_det_layers ; i++)
					xmi_free_layer(last->xi->absorbers->det_layers+i);
				free(last->xi->absorbers->det_layers);	
			}
			xmi_copy_composition2abs_or_crystal(det_compositionS, &(last->xi->absorbers->det_layers),&(last->xi->absorbers->n_det_layers));
			last->kind = kind;
			last->widget = widget;
			break;
		case DET_COMPOSITION_ADD:
			strcpy(last->message,"addition of detector absorber layer");
			if (last->xi->absorbers->n_det_layers > 0) {
				for (i = 0 ; i < last->xi->absorbers->n_det_layers ; i++)
					xmi_free_layer(last->xi->absorbers->det_layers+i);
				free(last->xi->absorbers->det_layers);	
			}
			xmi_copy_composition2abs_or_crystal(det_compositionS, &(last->xi->absorbers->det_layers),&(last->xi->absorbers->n_det_layers));
			last->kind = kind;
			last->widget = widget;
			break;
		case DET_COMPOSITION_EDIT:
			strcpy(last->message,"editing of detector absorber layer");
			if (last->xi->absorbers->n_det_layers > 0) {
				for (i = 0 ; i < last->xi->absorbers->n_det_layers ; i++)
					xmi_free_layer(last->xi->absorbers->det_layers+i);
				free(last->xi->absorbers->det_layers);	
			}
			xmi_copy_composition2abs_or_crystal(det_compositionS, &(last->xi->absorbers->det_layers),&(last->xi->absorbers->n_det_layers));
			last->kind = kind;
			last->widget = widget;
			break;
		case CRYSTAL_COMPOSITION_ORDER:
			strcpy(last->message,"change of crystal absorber ordering");
			if (last->xi->detector->n_crystal_layers > 0) {
				for (i = 0 ; i < last->xi->detector->n_crystal_layers ; i++)
					xmi_free_layer(last->xi->detector->crystal_layers+i);
				free(last->xi->detector->crystal_layers);	
			}
			xmi_copy_composition2abs_or_crystal(crystal_compositionS, &(last->xi->detector->crystal_layers),&(last->xi->detector->n_crystal_layers));
			last->kind = kind;
			last->widget = widget;
#if DEBUG == 1
			fprintf(stdout,"store pointer during update: %p\n",last->widget);
#endif
			break;
		case CRYSTAL_COMPOSITION_DELETE:
			strcpy(last->message,"removal of crystal absorber layer");
			if (last->xi->detector->n_crystal_layers > 0) {
				for (i = 0 ; i < last->xi->detector->n_crystal_layers ; i++)
					xmi_free_layer(last->xi->detector->crystal_layers+i);
				free(last->xi->detector->crystal_layers);	
			}
			xmi_copy_composition2abs_or_crystal(crystal_compositionS, &(last->xi->detector->crystal_layers),&(last->xi->detector->n_crystal_layers));
			last->kind = kind;
			last->widget = widget;
			break;
		case CRYSTAL_COMPOSITION_ADD:
			strcpy(last->message,"addition of crystal absorber layer");
			if (last->xi->detector->n_crystal_layers > 0) {
				for (i = 0 ; i < last->xi->detector->n_crystal_layers ; i++)
					xmi_free_layer(last->xi->detector->crystal_layers+i);
				free(last->xi->detector->crystal_layers);	
			}
			xmi_copy_composition2abs_or_crystal(crystal_compositionS, &(last->xi->detector->crystal_layers),&(last->xi->detector->n_crystal_layers));
			last->kind = kind;
			last->widget = widget;
			break;
		case CRYSTAL_COMPOSITION_EDIT:
			strcpy(last->message,"editing of crystal absorber layer");
			if (last->xi->detector->n_crystal_layers > 0) {
				for (i = 0 ; i < last->xi->detector->n_crystal_layers ; i++)
					xmi_free_layer(last->xi->detector->crystal_layers+i);
				free(last->xi->detector->crystal_layers);	
			}
			xmi_copy_composition2abs_or_crystal(crystal_compositionS, &(last->xi->detector->crystal_layers),&(last->xi->detector->n_crystal_layers));
			last->kind = kind;
			last->widget = widget;
			break;
		case DETECTOR_TYPE:
			strcpy(last->message,"change of detector type");
			last->xi->detector->detector_type = gtk_combo_box_get_active(GTK_COMBO_BOX(widget));
			last->kind = kind;
			last->widget = widget;
			break;
		case DETECTOR_GAIN: 
			strcpy(last->message,"change of detector gain");
			last->xi->detector->gain = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case DETECTOR_LIVE_TIME: 
			strcpy(last->message,"change of live time");
			last->xi->detector->live_time = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case DETECTOR_PULSE_WIDTH: 
			strcpy(last->message,"change of detector pulse width");
			last->xi->detector->pulse_width= strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case DETECTOR_ZERO: 
			strcpy(last->message,"change of detector zero");
			last->xi->detector->zero = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case DETECTOR_NOISE: 
			strcpy(last->message,"change of detector noise");
			last->xi->detector->noise = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case DETECTOR_FANO: 
			strcpy(last->message,"change of detector Fano factor");
			last->xi->detector->fano = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
		case DETECTOR_MAX_CONVOLUTION_ENERGY: 
			strcpy(last->message,"change of maximum convolution energy");
			last->xi->detector->max_convolution_energy = strtod((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL);	
			last->kind = kind;
			last->widget = widget;
			break;
	} 
	current = last;
	g_sprintf(buffer,"Undo: %s",last->message);
	gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);
	gtk_widget_set_sensitive(undoW,TRUE);
	gtk_tool_item_set_tooltip_text(undoT,buffer);
	gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);


/*
	if (opened_file == current || (opened_file != NULL && xmi_compare_input(current->xi,opened_file->xi) == 0)) {
		//new file was opened or current settings are identical to those of opened file
		gtk_widget_set_sensitive(saveW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
	}
	else {
		gtk_widget_set_sensitive(saveW,TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveT),TRUE);
	} 
*/	

	adjust_save_buttons();	
}

void comments_begin(GtkTextBuffer *textbuffer, gpointer user_data){
#if DEBUG == 1
	fprintf(stdout,"comments started\n");
#endif
	return;
}

void comments_end(GtkTextBuffer *textbuffer, gpointer user_data){
#if DEBUG == 1
	fprintf(stdout,"comments ended\n");
#endif
	return;
}




XMI_MAIN
	GtkWidget *window;
	GtkWidget *Main_vbox;

	GtkWidget *menubar;
	GtkWidget *filemenu;
	GtkWidget *file;
	GtkWidget *editmenu;
	GtkWidget *edit;
	GtkWidget *frame;
	GtkWidget *superframe;
	GtkWidget *label;
	GtkWidget *vbox_notebook;
	GtkWidget *hbox_text_label;
	GtkWidget *text;
	GtkWidget *toolbar;
	GtkWidget *scrolled_window;
	GtkWidget *tempW;
	char buffer[512];
	struct xmi_composition *temp_composition;
	struct val_changed *vc;
	struct xmi_input *xi_temp;
	char *title;
	GtkTextBuffer *commentsBuffer;
	GtkTextIter tib, tie;
	gint main_height=900;
	gint main_width=900;
	gint main_temp;
	GtkWidget *resultsPageW, *controlsPageW;
	GtkAccelGroup *accel_group = NULL;
	GtkWidget *aboutW;
#ifdef MAC_INTEGRATION
	GtkOSXApplication *theApp;
#endif

/*
 *
 *
 *
 *  Initialize
 *
 *
 *
 */

	//signal handlers
#ifdef G_OS_UNIX
	struct sigaction action;
	action.sa_handler = signal_handler;
	sigemptyset(&action.sa_mask);
	action.sa_flags = 0;

	sigaction(SIGINT, &action, NULL);
	sigaction(SIGTERM, &action, NULL);
	sigaction(SIGSEGV, &action, NULL);
	sigaction(SIGHUP, &action, NULL);
	
#elif defined(G_OS_WIN32)

#endif

	//needs to be checked on Windows... especially XP!
#if defined(G_OS_WIN32)
	setlocale(LC_ALL,"English_United States");
	//g_setenv("LANG","en_US",TRUE);
#else
	g_setenv("LANG","en_US",TRUE);
#endif
	gtk_disable_setlocale();
	setbuf(stdout,NULL);
	//let's use the default C locale
	//g_type_init
	g_type_init();


	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		return 1;
	}


	//initialize undo system
	redo_buffer = (struct undo_single *) malloc(sizeof(struct undo_single ));		
	current = redo_buffer;
	last = current;
	last_saved = NULL;



#if DEBUG == 1
	fprintf(stdout,"Initial undo buffer pointers\n");
	fprintf(stdout,"redo_buffer: %p\n",redo_buffer);
	fprintf(stdout,"current: %p\n",current);
	fprintf(stdout,"last: %p\n",last);
#endif



	current->xi = xmi_init_empty_input();	
	current->filename = strdup(UNLIKELY_FILENAME);

	n_photons_intervalC = 1;
	n_photons_lineC = 1;
	n_interactions_trajectoryC = 1;
	d_sample_sourceC = 1;
	n_sample_orientation_xC = 1;
	n_sample_orientation_yC = 1;
	n_sample_orientation_zC = 1;
	p_detector_window_xC = 1;
	p_detector_window_yC = 1;
	p_detector_window_zC = 1;
	n_detector_orientation_xC = 1;
	n_detector_orientation_yC = 1;
	n_detector_orientation_zC = 1;
	area_detectorC = 1;
	collimator_heightC = 1;
	collimator_diameterC = 1;
	d_source_slitC = 1;
	slit_size_xC = 1;
	slit_size_yC = 1;
	detector_typeC = 1;
	detector_gainC = 1;
	detector_pulse_widthC = 1;
	detector_live_timeC = 1;
	detector_zeroC = 1;
	detector_fanoC = 1;
	detector_noiseC = 1;
	detector_max_convolution_energyC = 1;

	//initialize regex patterns
	/*
	if (regcomp(&pos_int,"^[1-9][0-9]*$" , REG_EXTENDED | REG_NOSUB) != 0) {
		fprintf(stderr,"Error compiling regex pattern pos_int\n");
		return 1;
	}
	*/
	pos_int = g_regex_new("^[1-9][0-9]*$", G_REGEX_EXTENDED,0, NULL);





	gtk_init(&argc, &argv);

#ifdef MAC_INTEGRATION
	theApp = g_object_new(GTK_TYPE_OSX_APPLICATION,NULL);
	gtk_osxapplication_set_use_quartz_accelerators(theApp, TRUE);
#endif

	g_set_application_name("XMI-MSIM");

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	accel_group = gtk_accel_group_new();
	gtk_window_add_accel_group(GTK_WINDOW(window), accel_group);
	//size must depend on available height (and width too I guess)
	main_temp = 0.90* (double) gdk_screen_get_height(gdk_screen_get_default());
	if (main_temp <= main_height)
		main_height = main_temp;
	
	main_temp = 0.95* (double) gdk_screen_get_width(gdk_screen_get_default());
	if (main_temp <= main_width)
		main_width = main_temp;
	


	gtk_window_set_default_size(GTK_WINDOW(window),main_width,main_height);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);

	Main_vbox = gtk_vbox_new(FALSE,0);
	gtk_container_add(GTK_CONTAINER(window),Main_vbox);

	menubar = gtk_menu_bar_new();
	filemenu = gtk_menu_new();

	file = gtk_menu_item_new_with_label("File");
	//new = gtk_menu_item_new_with_label("New");
	newW = gtk_image_menu_item_new_from_stock(GTK_STOCK_NEW,accel_group);
	openW = gtk_image_menu_item_new_from_stock(GTK_STOCK_OPEN,accel_group);
	saveW = gtk_image_menu_item_new_from_stock(GTK_STOCK_SAVE,accel_group);
	save_asW = gtk_image_menu_item_new_from_stock(GTK_STOCK_SAVE_AS,accel_group);
#ifndef MAC_INTEGRATION
	quitW = gtk_image_menu_item_new_from_stock(GTK_STOCK_QUIT,accel_group);
#endif
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(file),filemenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),newW);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),openW);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),saveW);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),save_asW);
#ifndef MAC_INTEGRATION
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),quitW);
#endif
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),file);
#ifndef MAC_INTEGRATION
	g_signal_connect(G_OBJECT(quitW),"activate",G_CALLBACK(quit_program_cb),(gpointer) window);
#else
	g_signal_connect(theApp, "NSApplicationBlockTermination",G_CALLBACK(quit_blocker_mac_cb),(gpointer)window);
	g_signal_connect(theApp, "NSApplicationWillTerminate",G_CALLBACK(quit_program_cb),(gpointer)window);
	g_signal_connect(theApp, "NSApplicationOpenFile", G_CALLBACK(load_from_file_osx_cb),(gpointer) window);
#endif
	g_signal_connect(G_OBJECT(openW),"activate",G_CALLBACK(load_from_file_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(saveW),"activate",G_CALLBACK(save_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(save_asW),"activate",G_CALLBACK(saveas_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(newW),"activate",G_CALLBACK(new_cb),(gpointer) window);
	editmenu = gtk_menu_new();
	edit = gtk_menu_item_new_with_label("Edit");
	undoW = gtk_image_menu_item_new_from_stock(GTK_STOCK_UNDO,accel_group);
	g_signal_connect(G_OBJECT(undoW),"activate",G_CALLBACK(undo_menu_click),NULL);
	redoW = gtk_image_menu_item_new_from_stock(GTK_STOCK_REDO,accel_group);
	g_signal_connect(G_OBJECT(redoW),"activate",G_CALLBACK(redo_menu_click),NULL);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(edit),editmenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),undoW);
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),redoW);
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),edit);
	//both should be greyed out in the beginning
	gtk_widget_set_sensitive(undoW,FALSE);
	gtk_widget_set_sensitive(redoW,FALSE);
	gtk_widget_set_sensitive(saveW,FALSE);
	gtk_widget_set_sensitive(save_asW,FALSE);

	//add accelerators
	gtk_widget_add_accelerator(newW, "activate", accel_group, GDK_n, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(openW, "activate", accel_group, GDK_o, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(saveW, "activate", accel_group, GDK_s, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(save_asW, "activate", accel_group, GDK_s, PRIMARY_ACCEL_KEY | GDK_SHIFT_MASK, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(undoW, "activate", accel_group, GDK_z, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(redoW, "activate", accel_group, GDK_z, PRIMARY_ACCEL_KEY | GDK_SHIFT_MASK, GTK_ACCEL_VISIBLE);

#ifdef MAC_INTEGRATION
	GtkWidget *help = gtk_menu_item_new_with_label("Help");
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),help);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(help), gtk_menu_new());
	gtk_box_pack_start(GTK_BOX(Main_vbox), menubar, FALSE, FALSE, 0);
	gtk_widget_show_all(menubar);
	gtk_widget_hide(menubar);
	gtk_osxapplication_set_menu_bar(theApp, GTK_MENU_SHELL(menubar));
	aboutW = gtk_image_menu_item_new_from_stock(GTK_STOCK_ABOUT, NULL);
	g_signal_connect(G_OBJECT(aboutW),"activate",G_CALLBACK(about_click),window);
	gtk_osxapplication_insert_app_menu_item(theApp, aboutW, 0);
	gtk_osxapplication_insert_app_menu_item(theApp, g_object_ref(gtk_separator_menu_item_new()), 1);
	gtk_osxapplication_set_help_menu(theApp, GTK_MENU_ITEM(help));
	gtk_osxapplication_set_window_menu(theApp, NULL);
#else
	GtkWidget *helpmenu, *help;
	helpmenu = gtk_menu_new();
	help = gtk_menu_item_new_with_label("Help");
	aboutW = gtk_image_menu_item_new_from_stock(GTK_STOCK_ABOUT,NULL);
	g_signal_connect(G_OBJECT(aboutW),"activate",G_CALLBACK(about_click),window);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(help),helpmenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),aboutW);
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),help);
	

	gtk_widget_add_accelerator(quitW, "activate", accel_group, GDK_q, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_box_pack_start(GTK_BOX(Main_vbox), menubar, FALSE, FALSE, 3);
	gtk_widget_show_all(menubar);
#endif

	//toolbar
	toolbar = gtk_toolbar_new();
	newT = gtk_tool_button_new_from_stock(GTK_STOCK_NEW);
	openT = gtk_tool_button_new_from_stock(GTK_STOCK_OPEN);
	saveasT = gtk_tool_button_new_from_stock(GTK_STOCK_SAVE_AS);
	saveT = gtk_tool_button_new_from_stock(GTK_STOCK_SAVE);
	undoT = gtk_tool_button_new_from_stock(GTK_STOCK_UNDO);
	redoT = gtk_tool_button_new_from_stock(GTK_STOCK_REDO);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), newT,(gint) 0);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), openT,(gint) 1);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), saveasT,(gint) 2);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), saveT,(gint) 3);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), undoT,(gint) 4);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), redoT,(gint) 5);
	gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(saveasT),FALSE);
	g_signal_connect(G_OBJECT(undoT),"clicked",G_CALLBACK(undo_menu_click),NULL);
	gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	g_signal_connect(G_OBJECT(redoT),"clicked",G_CALLBACK(redo_menu_click),NULL);
	g_signal_connect(G_OBJECT(openT),"clicked",G_CALLBACK(load_from_file_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(saveasT),"clicked",G_CALLBACK(saveas_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(saveT),"clicked",G_CALLBACK(save_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(newT),"clicked",G_CALLBACK(new_cb),(gpointer) window);

	gtk_box_pack_start(GTK_BOX(Main_vbox), toolbar, FALSE, FALSE, 3);
	gtk_widget_show_all(toolbar);


	//g_signal_connect_swapped(G_OBJECT(window), "destroy", G_CALLBACK(quit_program_cb),(gpointer) window);
#ifdef MAC_INTEGRATION
	g_signal_connect(window,"delete-event",G_CALLBACK(delete_event),theApp);
#else
	g_signal_connect(window,"delete-event",G_CALLBACK(delete_event),window);
#endif

	//notebook
	notebook = gtk_notebook_new();
	notebookG = g_signal_connect(G_OBJECT(notebook), "switch-page",G_CALLBACK(notebook_page_changed_cb),window);
	g_signal_handler_block(G_OBJECT(notebook), notebookG);
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);
	gtk_widget_show(notebook);

	frame = gtk_frame_new("General");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">General</span>");

	//Append general
	superframe = gtk_vbox_new(FALSE,5);
	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);
	//gtk_container_add(GTK_CONTAINER(notebook),vbox_notebook);
	//Outputfile
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Outputfile");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	label = gtk_button_new_from_stock(GTK_STOCK_SAVE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	g_signal_connect(G_OBJECT(label),"clicked",G_CALLBACK(select_outputfile_cb), (gpointer) window);
	text = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(text),80);
	outputfileW = text;
	gtk_entry_set_text(GTK_ENTRY(text),current->xi->general->outputfile);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	//n_photons_interval
	hbox_text_label = gtk_hbox_new(FALSE,5);
	//gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of photons per interval");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	n_photons_intervalW = text;
	g_sprintf(buffer,"%li",current->xi->general->n_photons_interval);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = N_PHOTONS_INTERVAL;
	vc->check = &n_photons_intervalC;
	n_photons_intervalG = g_signal_connect(G_OBJECT(text),"changed",G_CALLBACK(pos_int_changed), (gpointer) vc);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	//n_photons_line
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of photons per discrete line");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	n_photons_lineW = text;
	g_sprintf(buffer,"%li",current->xi->general->n_photons_line);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = N_PHOTONS_LINE;
	vc->check = &n_photons_lineC;
	n_photons_lineG = g_signal_connect(G_OBJECT(text),"changed",G_CALLBACK(pos_int_changed), (gpointer) vc);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	//n_interactions_trajectory
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of interactions per trajectory");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	n_interactions_trajectoryW = text;
	g_sprintf(buffer,"%i",current->xi->general->n_interactions_trajectory);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = N_INTERACTIONS_TRAJECTORY;
	vc->check = &n_interactions_trajectoryC;
	n_interactions_trajectoryG = g_signal_connect(G_OBJECT(text),"changed",G_CALLBACK(pos_int_changed), (gpointer) vc);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);

	//comments
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Comments");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	commentsW = gtk_text_view_new();	
	gtk_container_set_border_width(GTK_CONTAINER(commentsW),2);

	commentsBuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (commentsW));
	//comments_beginG = g_signal_connect(G_OBJECT(commentsBuffer), "begin-user-action", G_CALLBACK(comments_begin), NULL);
	//comments_endG = g_signal_connect(G_OBJECT(commentsBuffer), "end-user-action", G_CALLBACK(comments_end), NULL);
	//gtk_text_buffer_get_bounds(commentsBuffer,&tib,&tie)
	commentsG = g_signal_connect(G_OBJECT(commentsBuffer), "changed", G_CALLBACK(comments_changed), NULL);
	gtk_text_buffer_set_text(commentsBuffer,current->xi->general->comments,-1);
	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), commentsW);
	gtk_widget_set_size_request(scrolled_window,700,100);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),scrolled_window,FALSE,FALSE,0);

	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);




	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), superframe);


	label = gtk_label_new("Input parameters");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Input parameters</span>");
	input_page = gtk_notebook_append_page(GTK_NOTEBOOK(notebook), scrolled_window, label);
	current_page = (gint) input_page;
	gtk_box_pack_start(GTK_BOX(Main_vbox), notebook, TRUE, TRUE, 3);
	gtk_widget_show_all(notebook);

	//composition
	tempW = initialize_matrix(current->xi->composition, COMPOSITION); 

	//initialize layer widget
	layerW = initialize_layer_widget(&layer);
	g_signal_connect(G_OBJECT(layerW->window),"hide",G_CALLBACK(layer_widget_hide_cb), (gpointer) layerW);


	frame = gtk_frame_new("Composition");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Composition</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_container_add(GTK_CONTAINER(frame),tempW);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);


	//geometry
	//d_sample_source
	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Sample-source distance (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	d_sample_sourceW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->d_sample_source);
	gtk_entry_set_text(GTK_ENTRY(d_sample_sourceW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = D_SAMPLE_SOURCE;
	vc->check = &d_sample_sourceC;
	d_sample_sourceG = g_signal_connect(G_OBJECT(d_sample_sourceW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), d_sample_sourceW, FALSE, FALSE, 0);
	//n_sample_orientation
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Sample orientation vector");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_sample_orientation_zW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->n_sample_orientation[2]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_zW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_sample_orientation_zW),10);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = N_SAMPLE_ORIENTATION_Z;
	vc->check = &n_sample_orientation_zC;
	n_sample_orientation_zG = g_signal_connect(G_OBJECT(n_sample_orientation_zW),"changed",G_CALLBACK(double_changed), (gpointer) vc);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_sample_orientation_zW, FALSE, FALSE, 0);
	label = gtk_label_new("z:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_sample_orientation_yW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->n_sample_orientation[1]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_yW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_sample_orientation_yW),10);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = N_SAMPLE_ORIENTATION_Y;
	vc->check = &n_sample_orientation_yC;
	n_sample_orientation_yG = g_signal_connect(G_OBJECT(n_sample_orientation_yW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_sample_orientation_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_sample_orientation_xW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->n_sample_orientation[0]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_xW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_sample_orientation_xW),10);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = N_SAMPLE_ORIENTATION_X;
	vc->check = &n_sample_orientation_xC;
	n_sample_orientation_xG = g_signal_connect(G_OBJECT(n_sample_orientation_xW),"changed",G_CALLBACK(double_changed),(gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_sample_orientation_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	//p_detector_window
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector window position (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	p_detector_window_zW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->p_detector_window[2]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_zW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(p_detector_window_zW),10);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = P_DETECTOR_WINDOW_Z;
	vc->check = &p_detector_window_zC;
	p_detector_window_zG = g_signal_connect(G_OBJECT(p_detector_window_zW),"changed",G_CALLBACK(double_changed), vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), p_detector_window_zW, FALSE, FALSE, 0);
	label = gtk_label_new("z:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	p_detector_window_yW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->p_detector_window[1]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_yW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(p_detector_window_yW),10);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = P_DETECTOR_WINDOW_Y;
	vc->check = &p_detector_window_yC;
	p_detector_window_yG = g_signal_connect(G_OBJECT(p_detector_window_yW),"changed",G_CALLBACK(double_changed), (gpointer) vc);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), p_detector_window_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	p_detector_window_xW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->p_detector_window[0]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_xW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(p_detector_window_xW),10);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = P_DETECTOR_WINDOW_X;
	vc->check = &p_detector_window_xC;
	p_detector_window_xG = g_signal_connect(G_OBJECT(p_detector_window_xW),"changed",G_CALLBACK(double_changed), (gpointer) vc);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), p_detector_window_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	//n_detector_orientation
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector window normal vector");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_detector_orientation_zW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->n_detector_orientation[2]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_zW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_detector_orientation_zW),10);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = N_DETECTOR_ORIENTATION_Z;
	vc->check = &n_detector_orientation_zC ;
	n_detector_orientation_zG = g_signal_connect(G_OBJECT(n_detector_orientation_zW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_detector_orientation_zW, FALSE, FALSE, 0);
	label = gtk_label_new("z:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_detector_orientation_yW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->n_detector_orientation[1]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_yW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_detector_orientation_yW),10);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = N_DETECTOR_ORIENTATION_Y;
	vc->check = &n_detector_orientation_yC ;
	n_detector_orientation_yG = g_signal_connect(G_OBJECT(n_detector_orientation_yW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_detector_orientation_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_detector_orientation_xW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->n_detector_orientation[0]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_xW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_detector_orientation_xW),10);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = N_DETECTOR_ORIENTATION_X;
	vc->check = &n_detector_orientation_xC ;
	n_detector_orientation_xG = g_signal_connect(G_OBJECT(n_detector_orientation_xW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_detector_orientation_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	//area detector
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label),"Active detector area (cm<sup>2</sup>)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	area_detectorW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->area_detector);
	gtk_entry_set_text(GTK_ENTRY(area_detectorW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = AREA_DETECTOR;
	vc->check = &area_detectorC;
	area_detectorG = g_signal_connect(G_OBJECT(area_detectorW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), area_detectorW, FALSE, FALSE, 0);

	//collimator_height
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Collimator height (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	collimator_heightW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->collimator_height);
	gtk_entry_set_text(GTK_ENTRY(collimator_heightW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = COLLIMATOR_HEIGHT;
	vc->check = &collimator_heightC ;
	collimator_heightG = g_signal_connect(G_OBJECT(collimator_heightW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), collimator_heightW, FALSE, FALSE, 0);

	//collimator_diameter
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Collimator diameter (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	collimator_diameterW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->collimator_diameter);
	gtk_entry_set_text(GTK_ENTRY(collimator_diameterW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = COLLIMATOR_DIAMETER;
	vc->check = &collimator_diameterC ;
	collimator_diameterG = g_signal_connect(G_OBJECT(collimator_diameterW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), collimator_diameterW, FALSE, FALSE, 0);

	//d_source_slit
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Source-slits distance (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	d_source_slitW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->d_source_slit);
	gtk_entry_set_text(GTK_ENTRY(d_source_slitW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = D_SOURCE_SLIT;
	vc->check = &d_source_slitC ;
	d_source_slitG = g_signal_connect(G_OBJECT(d_source_slitW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), d_source_slitW, FALSE, FALSE, 0);

	//slit sizes
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Slits size (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	slit_size_yW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->slit_size_y);
	gtk_entry_set_text(GTK_ENTRY(slit_size_yW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = SLIT_SIZE_Y;
	vc->check = &slit_size_yC;
	slit_size_yG = g_signal_connect(G_OBJECT(slit_size_yW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), slit_size_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	slit_size_xW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->geometry->slit_size_x);
	gtk_entry_set_text(GTK_ENTRY(slit_size_xW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = SLIT_SIZE_X;
	vc->check = &slit_size_xC;
	slit_size_xG = g_signal_connect(G_OBJECT(slit_size_xW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), slit_size_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);


	frame = gtk_frame_new("Geometry");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Geometry</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);
	
	//energies	

	frame = gtk_frame_new("Energies");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Excitation</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_container_add(GTK_CONTAINER(frame),initialize_energies(current->xi->excitation));
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);


	//absorbers
	//convert to composition struct
	xmi_copy_abs_or_crystal2composition(current->xi->absorbers->exc_layers, current->xi->absorbers->n_exc_layers   ,&temp_composition)	;
	tempW = initialize_matrix(temp_composition  , EXC_COMPOSITION); 


	frame = gtk_frame_new("Beam absorbers");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Beam absorbers</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_container_add(GTK_CONTAINER(frame),tempW);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);


	xmi_free_composition(temp_composition);
	xmi_copy_abs_or_crystal2composition(current->xi->absorbers->det_layers, current->xi->absorbers->n_det_layers   ,&temp_composition);	
	tempW = initialize_matrix(temp_composition  , DET_COMPOSITION); 


	frame = gtk_frame_new("Detection absorbers");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Detection absorbers</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_container_add(GTK_CONTAINER(frame),tempW);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);


	xmi_free_composition(temp_composition);

	//detector
	//detector type
	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector type");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_typeW = gtk_combo_box_new_text();
	gtk_combo_box_append_text(GTK_COMBO_BOX(detector_typeW),"Si(Li)");
	gtk_combo_box_append_text(GTK_COMBO_BOX(detector_typeW),"Ge");
	gtk_combo_box_append_text(GTK_COMBO_BOX(detector_typeW),"SDD");
	gtk_combo_box_set_active(GTK_COMBO_BOX(detector_typeW),current->xi->detector->detector_type);
	detector_typeG = g_signal_connect(G_OBJECT(detector_typeW),"changed",G_CALLBACK(detector_type_changed), GINT_TO_POINTER(DETECTOR_TYPE)  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_typeW, FALSE, FALSE, 0);
	
	//live time
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Live time (s)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_live_timeW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->detector->live_time);
	gtk_entry_set_text(GTK_ENTRY(detector_live_timeW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_LIVE_TIME;
	vc->check = &detector_live_timeC;
	detector_live_timeG = g_signal_connect(G_OBJECT(detector_live_timeW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_live_timeW, FALSE, FALSE, 0);
	
	//gain
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector gain (keV/channel)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_gainW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->detector->gain);
	gtk_entry_set_text(GTK_ENTRY(detector_gainW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_GAIN;
	vc->check = &detector_gainC;
	detector_gainG = g_signal_connect(G_OBJECT(detector_gainW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_gainW, FALSE, FALSE, 0);
	
	//zero
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector zero (keV)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_zeroW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->detector->zero);
	gtk_entry_set_text(GTK_ENTRY(detector_zeroW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_ZERO;
	vc->check = &detector_zeroC;
	detector_zeroG = g_signal_connect(G_OBJECT(detector_zeroW),"changed",G_CALLBACK(double_changed), (gpointer) vc);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_zeroW, FALSE, FALSE, 0);

	//fano
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector Fano factor");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_fanoW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->detector->fano);
	gtk_entry_set_text(GTK_ENTRY(detector_fanoW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_FANO;
	vc->check = &detector_fanoC;
	detector_fanoG = g_signal_connect(G_OBJECT(detector_fanoW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_fanoW, FALSE, FALSE, 0);

	//noise
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector electronic noise (keV)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_noiseW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->detector->noise);
	gtk_entry_set_text(GTK_ENTRY(detector_noiseW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_NOISE;
	vc->check = &detector_noiseC;
	detector_noiseG = g_signal_connect(G_OBJECT(detector_noiseW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_noiseW, FALSE, FALSE, 0);

	//pulse_width
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Pulse width (s)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_pulse_widthW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->detector->pulse_width);
	gtk_entry_set_text(GTK_ENTRY(detector_pulse_widthW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_PULSE_WIDTH;
	vc->check = &detector_pulse_widthC;
	detector_pulse_widthG = g_signal_connect(G_OBJECT(detector_pulse_widthW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_pulse_widthW, FALSE, FALSE, 0);
	
	//max_convolution_energy
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Max convolution energy (keV)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_max_convolution_energyW = gtk_entry_new();
	g_sprintf(buffer,"%lg",current->xi->detector->max_convolution_energy);
	gtk_entry_set_text(GTK_ENTRY(detector_max_convolution_energyW),buffer);
	vc = (struct val_changed *) malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_MAX_CONVOLUTION_ENERGY;
	vc->check = &detector_max_convolution_energyC;
	detector_max_convolution_energyG = g_signal_connect(G_OBJECT(detector_max_convolution_energyW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_max_convolution_energyW, FALSE, FALSE, 0);

	//crystal
	xmi_copy_abs_or_crystal2composition(current->xi->detector->crystal_layers, current->xi->detector->n_crystal_layers   ,&temp_composition);	
	tempW = initialize_matrix(temp_composition  , CRYSTAL_COMPOSITION); 
	
	gtk_box_pack_start(GTK_BOX(vbox_notebook), tempW, TRUE, FALSE, 3);
	xmi_free_composition(temp_composition);

	frame = gtk_frame_new("Detector settings");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Detector settings</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);
	gtk_widget_show_all(superframe);



	//second notebook page: Simulation controls
	label = gtk_label_new("Simulation controls");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Simulation controls</span>");
	controlsPageW = init_simulation_controls(window);
	control_page = gtk_notebook_append_page(GTK_NOTEBOOK(notebook), controlsPageW, label);
	gtk_widget_show_all(controlsPageW);

	//third notebook page: Results
	label = gtk_label_new("Results");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Results</span>");
	resultsPageW = init_results(window);
	results_page = gtk_notebook_append_page(GTK_NOTEBOOK(notebook), resultsPageW, label);
	gtk_widget_show_all(resultsPageW);
	//gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), results_page);
	//fprintf(stdout,"going to input_page\n");
	//gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), input_page);
	
	gtk_widget_show(Main_vbox);
	gtk_widget_show(window);
	g_signal_handler_unblock(G_OBJECT(notebook), notebookG);


	gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),"Redo");		
	gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),"Undo");		


#ifdef MAC_INTEGRATION
	gtk_osxapplication_ready(theApp);
#endif

	gchar *filename = g_strdup(argv[1]);
	struct xmi_input *xi;
	GtkWidget *dialog;
	if (argc == 2) {
		if (strcmp(filename+strlen(filename)-5,".xmsi") == 0) {
			update_xmimsim_title_xmso("No simulation data available", window, NULL);
			//XMSI file
			gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),input_page);
			if (xmi_read_input_xml(filename, &xi) == 1) {
				//success reading it in...
				change_all_values(xi);
				//reset redo_buffer
				reset_undo_buffer(xi, filename);	
				title = g_path_get_basename(filename);
				update_xmimsim_title_xmsi(title, window,filename);
				g_free(title);
			}
			else {
				update_xmimsim_title_xmsi("New file", window, NULL);
				dialog = gtk_message_dialog_new (GTK_WINDOW(window),
					GTK_DIALOG_DESTROY_WITH_PARENT,
			       		GTK_MESSAGE_ERROR,
			       		GTK_BUTTONS_CLOSE,
			       		"Could not read file %s: model is incomplete/invalid",filename
	        	        	);
	     			//gtk_dialog_run (GTK_DIALOG (dialog));
				//gtk_widget_destroy(dialog);
				g_idle_add(dialog_helper_cb,(gpointer) dialog);
			}
		}
		else if (strcmp(filename+strlen(filename)-5,".xmso") == 0) {
			update_xmimsim_title_xmsi("New file", window, NULL);
			//XMSO file
			gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),results_page);
			if (plot_spectra_from_file(filename) == 1) {
				gchar *temp_base = g_path_get_basename(filename);
				update_xmimsim_title_xmso(temp_base, window, filename);
				g_free(temp_base);
			}
			else {
				update_xmimsim_title_xmso("No simulation data available", window, NULL);
				dialog = gtk_message_dialog_new (GTK_WINDOW(window),
					GTK_DIALOG_DESTROY_WITH_PARENT,
			       		GTK_MESSAGE_ERROR,
			       		GTK_BUTTONS_CLOSE,
			       		"Could not read file %s",filename
	               		);
	     			//gtk_dialog_run (GTK_DIALOG (dialog));
				//gtk_widget_destroy (dialog);
				g_idle_add(dialog_helper_cb,(gpointer) dialog);
			}
		}
		else {
			update_xmimsim_title_xmsi("New file", window, NULL);
			update_xmimsim_title_xmso("No simulation data available", window, NULL);
			dialog = gtk_message_dialog_new (GTK_WINDOW(window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
			      	GTK_BUTTONS_CLOSE,
			       	"Could not read file %s\nExtenstion must be either xmsi or xmso.",filename
	               	);
			g_idle_add(dialog_helper_cb,(gpointer) dialog);
		}
	}
	else { 
		update_xmimsim_title_xmsi("New file", window, NULL);
		update_xmimsim_title_xmso("No simulation data available", window, NULL);
	}





	gtk_main();

#ifdef MAC_INTEGRATION
	g_object_unref(theApp);
#endif



	return 0;
}

void change_all_values(struct xmi_input *new_input) {
	char buffer[512], *elementString;
	int i,j;
	GtkTreeIter iter;
	GtkTextBuffer *commentsBuffer;

	//checkeable values
	n_photons_intervalC = 1;
	n_photons_lineC = 1;
	n_interactions_trajectoryC = 1;
	d_sample_sourceC = 1;
	n_sample_orientation_xC = 1;
	n_sample_orientation_yC = 1;
	n_sample_orientation_zC = 1;
	p_detector_window_xC = 1;
	p_detector_window_yC = 1;
	p_detector_window_zC = 1;
	n_detector_orientation_xC = 1;
	n_detector_orientation_yC = 1;
	n_detector_orientation_zC = 1;
	area_detectorC = 1;
	collimator_heightC = 1;
	collimator_diameterC = 1;
	d_source_slitC = 1;
	slit_size_xC = 1;
	slit_size_yC = 1;
	detector_typeC = 1;
	detector_gainC = 1;
	detector_live_timeC = 1;
	detector_pulse_widthC = 1;
	detector_zeroC = 1;
	detector_fanoC = 1;
	detector_noiseC = 1;
	detector_max_convolution_energyC = 1;

	//disable signal handlers where necessary
	g_signal_handler_block(G_OBJECT(n_photons_intervalW), n_photons_intervalG);
	g_signal_handler_block(G_OBJECT(n_photons_lineW), n_photons_lineG);
	g_signal_handler_block(G_OBJECT(n_interactions_trajectoryW), n_interactions_trajectoryG);
	g_signal_handler_block(G_OBJECT(d_sample_sourceW), d_sample_sourceG);
	g_signal_handler_block(G_OBJECT(n_sample_orientation_xW), n_sample_orientation_xG);
	g_signal_handler_block(G_OBJECT(n_sample_orientation_yW), n_sample_orientation_yG);
	g_signal_handler_block(G_OBJECT(n_sample_orientation_zW), n_sample_orientation_zG);
	g_signal_handler_block(G_OBJECT(p_detector_window_xW), p_detector_window_xG);
	g_signal_handler_block(G_OBJECT(p_detector_window_yW), p_detector_window_yG);
	g_signal_handler_block(G_OBJECT(p_detector_window_zW), p_detector_window_zG);
	g_signal_handler_block(G_OBJECT(n_detector_orientation_xW), n_detector_orientation_xG);
	g_signal_handler_block(G_OBJECT(n_detector_orientation_yW), n_detector_orientation_yG);
	g_signal_handler_block(G_OBJECT(n_detector_orientation_zW), n_detector_orientation_zG);
	g_signal_handler_block(G_OBJECT(area_detectorW), area_detectorG);
	g_signal_handler_block(G_OBJECT(collimator_heightW), collimator_heightG);
	g_signal_handler_block(G_OBJECT(collimator_diameterW), collimator_diameterG);
	g_signal_handler_block(G_OBJECT(d_source_slitW), d_source_slitG);
	g_signal_handler_block(G_OBJECT(slit_size_xW), slit_size_xG);
	g_signal_handler_block(G_OBJECT(slit_size_yW), slit_size_yG);
	g_signal_handler_block(G_OBJECT(detector_typeW), detector_typeG);
	g_signal_handler_block(G_OBJECT(detector_gainW), detector_gainG);
	g_signal_handler_block(G_OBJECT(detector_pulse_widthW), detector_pulse_widthG);
	g_signal_handler_block(G_OBJECT(detector_live_timeW), detector_live_timeG);
	g_signal_handler_block(G_OBJECT(detector_zeroW), detector_zeroG);
	g_signal_handler_block(G_OBJECT(detector_noiseW), detector_noiseG);
	g_signal_handler_block(G_OBJECT(detector_fanoW), detector_fanoG);
	g_signal_handler_block(G_OBJECT(detector_max_convolution_energyW), detector_max_convolution_energyG);
	g_signal_handler_block(G_OBJECT(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW))),commentsG);
	

	//general
	gtk_entry_set_text(GTK_ENTRY(outputfileW),new_input->general->outputfile);
	g_sprintf(buffer,"%li",new_input->general->n_photons_interval);
	gtk_entry_set_text(GTK_ENTRY(n_photons_intervalW),buffer);
	g_sprintf(buffer,"%li",new_input->general->n_photons_line);
	gtk_entry_set_text(GTK_ENTRY(n_photons_lineW),buffer);
	g_sprintf(buffer,"%i",new_input->general->n_interactions_trajectory);
	gtk_entry_set_text(GTK_ENTRY(n_interactions_trajectoryW),buffer);
	commentsBuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (commentsW));
	gtk_text_buffer_set_text(commentsBuffer,new_input->general->comments,-1);


	//composition
	gtk_list_store_clear(compositionL);
	for (i=0 ; i < new_input->composition->n_layers ; i++) {
		gtk_list_store_append(compositionL, &iter);
		elementString = (char *) malloc(sizeof(char)* (new_input->composition->layers[i].n_elements*5));
		elementString[0] = '\0';
		for (j = 0 ; j < new_input->composition->layers[i].n_elements ; j++) {
			strcat(elementString,AtomicNumberToSymbol(new_input->composition->layers[i].Z[j]));
			if (j != new_input->composition->layers[i].n_elements-1) {
				strcat(elementString,", ");
			}

		}
		gtk_list_store_set(compositionL, &iter,
			N_ELEMENTS_COLUMN, new_input->composition->layers[i].n_elements,
			ELEMENTS_COLUMN,elementString,
			DENSITY_COLUMN, new_input->composition->layers[i].density,
			THICKNESS_COLUMN, new_input->composition->layers[i].thickness,
			REFERENCE_COLUMN,(i+1 == new_input->composition->reference_layer) ? TRUE : FALSE,
			-1
			);
		free(elementString);
	}
	xmi_free_composition(compositionS);
	xmi_copy_composition(new_input->composition,&compositionS);	

	//geometry
	g_sprintf(buffer,"%lg",new_input->geometry->d_sample_source);
	gtk_entry_set_text(GTK_ENTRY(d_sample_sourceW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->n_sample_orientation[0]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_xW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->n_sample_orientation[1]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_yW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->n_sample_orientation[2]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_zW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->p_detector_window[0]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_xW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->p_detector_window[1]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_yW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->p_detector_window[2]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_zW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->n_detector_orientation[0]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_xW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->n_detector_orientation[1]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_yW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->n_detector_orientation[2]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_zW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->area_detector);
	gtk_entry_set_text(GTK_ENTRY(area_detectorW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->collimator_height);
	gtk_entry_set_text(GTK_ENTRY(collimator_heightW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->collimator_diameter);
	gtk_entry_set_text(GTK_ENTRY(collimator_diameterW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->d_source_slit);
	gtk_entry_set_text(GTK_ENTRY(d_source_slitW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->slit_size_x);
	gtk_entry_set_text(GTK_ENTRY(slit_size_xW),buffer);
	g_sprintf(buffer,"%lg",new_input->geometry->slit_size_y);
	gtk_entry_set_text(GTK_ENTRY(slit_size_yW),buffer);
	

	//energies
	gtk_list_store_clear(discWidget->store);
	for (i = 0 ; i < (new_input)->excitation->n_discrete ; i++) {
		gtk_list_store_append(discWidget->store, &iter);
		gtk_list_store_set(discWidget->store, &iter,
			ENERGY_COLUMN, (new_input)->excitation->discrete[i].energy,
			HOR_INTENSITY_COLUMN, (new_input)->excitation->discrete[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, (new_input)->excitation->discrete[i].vertical_intensity,
			SIGMA_X_COLUMN, (new_input)->excitation->discrete[i].sigma_x,
			SIGMA_XP_COLUMN,(new_input)->excitation->discrete[i].sigma_xp,
			SIGMA_Y_COLUMN,(new_input)->excitation->discrete[i].sigma_y,
			SIGMA_YP_COLUMN,(new_input)->excitation->discrete[i].sigma_yp,
			-1);
	}
	gtk_list_store_clear(contWidget->store);
	for (i = 0 ; i < (new_input)->excitation->n_continuous ; i++) {
		gtk_list_store_append(contWidget->store, &iter);
		gtk_list_store_set(contWidget->store, &iter,
			ENERGY_COLUMN, (new_input)->excitation->continuous[i].energy,
			HOR_INTENSITY_COLUMN, (new_input)->excitation->continuous[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, (new_input)->excitation->continuous[i].vertical_intensity,
			SIGMA_X_COLUMN, (new_input)->excitation->continuous[i].sigma_x,
			SIGMA_XP_COLUMN,(new_input)->excitation->continuous[i].sigma_xp,
			SIGMA_Y_COLUMN,(new_input)->excitation->continuous[i].sigma_y,
			SIGMA_YP_COLUMN,(new_input)->excitation->continuous[i].sigma_yp,
			-1);
	}

	//absorbers
	gtk_list_store_clear(exc_compositionL);
	for (i=0 ; i < new_input->absorbers->n_exc_layers ; i++) {
		gtk_list_store_append(exc_compositionL, &iter);
		elementString = (char *) malloc(sizeof(char)* (new_input->absorbers->exc_layers[i].n_elements*5));
		elementString[0] = '\0';
		for (j = 0 ; j < new_input->absorbers->exc_layers[i].n_elements ; j++) {
			strcat(elementString,AtomicNumberToSymbol(new_input->absorbers->exc_layers[i].Z[j]));
			if (j != new_input->absorbers->exc_layers[i].n_elements-1) {
				strcat(elementString,", ");
			}

		}
		gtk_list_store_set(exc_compositionL, &iter,
			N_ELEMENTS_COLUMN, new_input->absorbers->exc_layers[i].n_elements,
			ELEMENTS_COLUMN,elementString,
			DENSITY_COLUMN, new_input->absorbers->exc_layers[i].density,
			THICKNESS_COLUMN, new_input->absorbers->exc_layers[i].thickness,
			-1
			);
		free(elementString);
	}
	xmi_free_composition(exc_compositionS);
	xmi_copy_abs_or_crystal2composition(new_input->absorbers->exc_layers, new_input->absorbers->n_exc_layers,&exc_compositionS);	

	gtk_list_store_clear(det_compositionL);
	for (i=0 ; i < new_input->absorbers->n_det_layers ; i++) {
		gtk_list_store_append(det_compositionL, &iter);
		elementString = (char *) malloc(sizeof(char)* (new_input->absorbers->det_layers[i].n_elements*5));
		elementString[0] = '\0';
		for (j = 0 ; j < new_input->absorbers->det_layers[i].n_elements ; j++) {
			strcat(elementString,AtomicNumberToSymbol(new_input->absorbers->det_layers[i].Z[j]));
			if (j != new_input->absorbers->det_layers[i].n_elements-1) {
				strcat(elementString,", ");
			}

		}
		gtk_list_store_set(det_compositionL, &iter,
			N_ELEMENTS_COLUMN, new_input->absorbers->det_layers[i].n_elements,
			ELEMENTS_COLUMN,elementString,
			DENSITY_COLUMN, new_input->absorbers->det_layers[i].density,
			THICKNESS_COLUMN, new_input->absorbers->det_layers[i].thickness,
			-1
			);
		free(elementString);
	}
	xmi_free_composition(det_compositionS);
	xmi_copy_abs_or_crystal2composition(new_input->absorbers->det_layers, new_input->absorbers->n_det_layers,&det_compositionS);	

	//detector
	gtk_combo_box_set_active(GTK_COMBO_BOX(detector_typeW),new_input->detector->detector_type);
	
	g_sprintf(buffer,"%lg",new_input->detector->gain);
#if DEBUG == 1
	fprintf(stdout,"gain: %s\n",buffer);
#endif
	gtk_entry_set_text(GTK_ENTRY(detector_gainW),buffer);
	g_sprintf(buffer,"%lg",new_input->detector->zero);
#if DEBUG == 1
	fprintf(stdout,"zero: %s\n",buffer);
#endif
	gtk_entry_set_text(GTK_ENTRY(detector_zeroW),buffer);
	g_sprintf(buffer,"%lg",new_input->detector->fano);
	gtk_entry_set_text(GTK_ENTRY(detector_fanoW),buffer);
	g_sprintf(buffer,"%lg",new_input->detector->noise);
	gtk_entry_set_text(GTK_ENTRY(detector_noiseW),buffer);
	g_sprintf(buffer,"%lg",new_input->detector->max_convolution_energy);
	gtk_entry_set_text(GTK_ENTRY(detector_max_convolution_energyW),buffer);
	g_sprintf(buffer,"%lg",new_input->detector->live_time);
	gtk_entry_set_text(GTK_ENTRY(detector_live_timeW),buffer);
	g_sprintf(buffer,"%lg",new_input->detector->pulse_width);
	gtk_entry_set_text(GTK_ENTRY(detector_pulse_widthW),buffer);

	gtk_list_store_clear(crystal_compositionL);
	for (i=0 ; i < new_input->detector->n_crystal_layers ; i++) {
		gtk_list_store_append(crystal_compositionL, &iter);
		elementString = (char *) malloc(sizeof(char)* (new_input->detector->crystal_layers[i].n_elements*5));
		elementString[0] = '\0';
		for (j = 0 ; j < new_input->detector->crystal_layers[i].n_elements ; j++) {
			strcat(elementString,AtomicNumberToSymbol(new_input->detector->crystal_layers[i].Z[j]));
			if (j != new_input->detector->crystal_layers[i].n_elements-1) {
				strcat(elementString,", ");
			}

		}
		gtk_list_store_set(crystal_compositionL, &iter,
			N_ELEMENTS_COLUMN, new_input->detector->crystal_layers[i].n_elements,
			ELEMENTS_COLUMN,elementString,
			DENSITY_COLUMN, new_input->detector->crystal_layers[i].density,
			THICKNESS_COLUMN, new_input->detector->crystal_layers[i].thickness,
			-1
			);
		free(elementString);
	}
	xmi_free_composition(crystal_compositionS);
	xmi_copy_abs_or_crystal2composition(new_input->detector->crystal_layers, new_input->detector->n_crystal_layers,&crystal_compositionS);	


	//enable signal handlers where necessary
	g_signal_handler_unblock(G_OBJECT(n_photons_intervalW), n_photons_intervalG);
	g_signal_handler_unblock(G_OBJECT(n_photons_lineW), n_photons_lineG);
	g_signal_handler_unblock(G_OBJECT(n_interactions_trajectoryW), n_interactions_trajectoryG);
	g_signal_handler_unblock(G_OBJECT(d_sample_sourceW), d_sample_sourceG);
	g_signal_handler_unblock(G_OBJECT(n_sample_orientation_xW), n_sample_orientation_xG);
	g_signal_handler_unblock(G_OBJECT(n_sample_orientation_yW), n_sample_orientation_yG);
	g_signal_handler_unblock(G_OBJECT(n_sample_orientation_zW), n_sample_orientation_zG);
	g_signal_handler_unblock(G_OBJECT(p_detector_window_xW), p_detector_window_xG);
	g_signal_handler_unblock(G_OBJECT(p_detector_window_yW), p_detector_window_yG);
	g_signal_handler_unblock(G_OBJECT(p_detector_window_zW), p_detector_window_zG);
	g_signal_handler_unblock(G_OBJECT(n_detector_orientation_xW), n_detector_orientation_xG);
	g_signal_handler_unblock(G_OBJECT(n_detector_orientation_yW), n_detector_orientation_yG);
	g_signal_handler_unblock(G_OBJECT(n_detector_orientation_zW), n_detector_orientation_zG);
	g_signal_handler_unblock(G_OBJECT(area_detectorW), area_detectorG);
	g_signal_handler_unblock(G_OBJECT(collimator_heightW), collimator_heightG);
	g_signal_handler_unblock(G_OBJECT(collimator_diameterW), collimator_diameterG);
	g_signal_handler_unblock(G_OBJECT(d_source_slitW), d_source_slitG);
	g_signal_handler_unblock(G_OBJECT(slit_size_xW), slit_size_xG);
	g_signal_handler_unblock(G_OBJECT(slit_size_yW), slit_size_yG);
	g_signal_handler_unblock(G_OBJECT(detector_typeW), detector_typeG);
	g_signal_handler_unblock(G_OBJECT(detector_gainW), detector_gainG);
	g_signal_handler_unblock(G_OBJECT(detector_zeroW), detector_zeroG);
	g_signal_handler_unblock(G_OBJECT(detector_pulse_widthW), detector_pulse_widthG);
	g_signal_handler_unblock(G_OBJECT(detector_live_timeW), detector_live_timeG);
	g_signal_handler_unblock(G_OBJECT(detector_noiseW), detector_noiseG);
	g_signal_handler_unblock(G_OBJECT(detector_fanoW), detector_fanoG);
	g_signal_handler_unblock(G_OBJECT(detector_max_convolution_energyW), detector_max_convolution_energyG);
	g_signal_handler_unblock(G_OBJECT(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW))),commentsG);
	
	

	return;
}

void new_cb(GtkWidget *widget, gpointer data) {
	struct xmi_input *xi;
	


	//start a new inputfile, using default settings
	if (process_pre_file_operation((GtkWidget *) data) == FALSE)
		return;


	xi = xmi_init_empty_input();
	change_all_values(xi);
	reset_undo_buffer(xi,UNLIKELY_FILENAME);

	update_xmimsim_title_xmsi("New file", data, NULL);

	return;
}

#ifdef MAC_INTEGRATION
gboolean quit_blocker_mac_cb(GtkOSXApplication *app, gpointer data){

	if (process_pre_file_operation((GtkWidget *) data) == FALSE)
		return TRUE;
	return FALSE;

}

void quit_program_cb(GtkOSXApplication *app, gpointer data) {
#else
void quit_program_cb(GtkWidget *widget, gpointer data) {
	if (process_pre_file_operation((GtkWidget *) data) == FALSE)
		return;
#endif



	if (xmimsim_pid != GPID_INACTIVE) {
		//if UNIX -> send sigterm
		//if WIN32 -> call TerminateProcess 

#ifdef G_OS_UNIX
		int kill_rv;
	
		fprintf(stdout,"killing %i UNIX style\n", (int) xmimsim_pid);
		kill_rv = kill((pid_t) xmimsim_pid, SIGTERM);
		wait(NULL);
		if (kill_rv == 0) {
			fprintf(stdout, "Process %i was successfully terminated before completion\n",(int) xmimsim_pid);
		}
		else {
			fprintf(stdout, "Process %i could not be terminated with the SIGTERM signal\n",(int) xmimsim_pid);
		}
#elif defined(G_OS_WIN32)
		BOOL terminate_rv;

		terminate_rv = TerminateProcess((HANDLE) xmimsim_pid, (UINT) 1);

		if (terminate_rv == TRUE) {
			fprintf(stdout, "Process %i was successfully terminated\n",(int) xmimsim_pid);
		}
		else {
			fprintf(stdout, "Process %i could not be terminated with the TerminateProcess call\n",(int) xmimsim_pid);
		}
#endif
		g_spawn_close_pid(xmimsim_pid);
		xmimsim_pid = GPID_INACTIVE;

	}

	fprintf(stdout,"quitting\n");

	gtk_main_quit();

	return;
}

void load_from_file_cb(GtkWidget *widget, gpointer data) {
	GtkWidget *dialog = NULL;
	GtkFileFilter *filter1, *filter2;
	gchar *filename;
	struct xmi_input *xi;
	gchar *title;


	if (process_pre_file_operation((GtkWidget *) data) == FALSE)
		return;


	filter1 = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter1,"*.xmsi");
	gtk_file_filter_set_name(filter1,"XMI-MSIM inputfiles");
	filter2 = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter2,"*.xmso");
	gtk_file_filter_set_name(filter2,"XMI-MSIM outputfiles");
	dialog = gtk_file_chooser_dialog_new ("Open simulation file",
		GTK_WINDOW((GtkWidget *) data),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
		NULL);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter1);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter2);
																
	if (gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)) == input_page ||
	  gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)) == control_page) {
		gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(dialog), filter1);
	}
	else
		gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(dialog), filter2);

	  
	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		//get filetype
		if (gtk_file_chooser_get_filter(GTK_FILE_CHOOSER(dialog)) == filter1) {
			gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),input_page);
			if (xmi_read_input_xml(filename, &xi) == 1) {
				//success reading it in...
				change_all_values(xi);
				//reset redo_buffer
				reset_undo_buffer(xi, filename);	
				title = g_path_get_basename(filename);
				update_xmimsim_title_xmsi(title, data, filename);
				g_free(title);
				//update_undo_buffer(OPEN_FILE,(GtkWidget *) xi);	
			}
			else {
				gtk_widget_destroy (dialog);
				dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
					GTK_DIALOG_DESTROY_WITH_PARENT,
		        		GTK_MESSAGE_ERROR,
		        		GTK_BUTTONS_CLOSE,
		        		"Could not read file %s: model is incomplete/invalid",filename
	                	);
	     			gtk_dialog_run (GTK_DIALOG (dialog));
			}
		}
		else if (gtk_file_chooser_get_filter(GTK_FILE_CHOOSER(dialog)) == filter2) {
			
			gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),results_page);
			if (plot_spectra_from_file(filename) == 1) {
				gchar *temp_base = g_path_get_basename(filename);
				update_xmimsim_title_xmso(temp_base, data, filename);
				g_free(temp_base);
			}
			else {
				gtk_widget_destroy (dialog);
				dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
					GTK_DIALOG_DESTROY_WITH_PARENT,
		        		GTK_MESSAGE_ERROR,
		        		GTK_BUTTONS_CLOSE,
		        		"Could not read file %s",filename
	                	);
	     			gtk_dialog_run (GTK_DIALOG (dialog));
			}
		}
		g_free (filename);							
	}

	gtk_widget_destroy (dialog);
}

int check_changeables(void) {

 return n_photons_intervalC &&
 n_photons_lineC &&
 n_interactions_trajectoryC &&
 d_sample_sourceC &&
 n_sample_orientation_xC &&
 n_sample_orientation_yC &&
 n_sample_orientation_zC &&
 p_detector_window_xC &&
 p_detector_window_yC &&
 p_detector_window_zC &&
 n_detector_orientation_xC &&
 n_detector_orientation_yC &&
 n_detector_orientation_zC &&
 area_detectorC &&
 collimator_heightC &&
 collimator_diameterC &&
 d_source_slitC &&
 slit_size_xC &&
 slit_size_yC &&
 detector_typeC &&
 detector_gainC &&
 detector_live_timeC &&
 detector_pulse_widthC &&
 detector_zeroC &&
 detector_fanoC &&
 detector_noiseC &&
 detector_max_convolution_energyC;
}



void saveas_cb(GtkWidget *widget, gpointer data) {
	
	saveas_function(widget, data);

	return;
}

void save_cb(GtkWidget *widget, gpointer data) {
	int check_status;
	GtkWidget *dialog;
	struct undo_single *check_rv; 
	gchar *filename;
	GtkTextIter iterb, itere;

	check_rv = check_changes_saved(&check_status);
#if DEBUG == 1
	fprintf(stdout,"check_status: %i\n",check_status);
#endif
	//check if it was saved before... otherwise call saveas
	if (check_status == CHECK_CHANGES_SAVED_BEFORE) {
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
		if (xmi_write_input_xml(check_rv->filename, current->xi) != 1) {
			dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
				GTK_DIALOG_DESTROY_WITH_PARENT,
		       		GTK_MESSAGE_ERROR,
		       		GTK_BUTTONS_CLOSE,
		       		"Could not write to file %s: file writeable?",check_rv->filename
	               	);
	     		gtk_dialog_run (GTK_DIALOG (dialog));
			gtk_widget_destroy(dialog);
			return;

		}
		else {
			filename = strdup(last_saved->filename);
			free(last_saved->filename);
			xmi_free_input(last_saved->xi);
			free(last_saved);
			last_saved = (struct undo_single *) malloc(sizeof(struct undo_single));
			xmi_copy_input(current->xi, &(last_saved->xi));
			last_saved->filename = strdup(filename);
			free(filename);
		}
		gtk_widget_set_sensitive(saveW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
	}
	else if (check_status == CHECK_CHANGES_NEVER_SAVED ||
		check_status == CHECK_CHANGES_NEW) {
		//never saved -> call saveas
		saveas_function(widget, data);

	}
	else if (check_status == CHECK_CHANGES_JUST_SAVED) {
		//do nothing
	}



	return;


}
gboolean saveas_function(GtkWidget *widget, gpointer data) {

	GtkWidget *dialog;
	GtkFileFilter *filter;
	gchar *filename;
	gchar *title;
	GtkTextIter iterb, itere;

	if (check_changeables() == 0 || xmi_validate_input(current->xi) != 0 )  {
		dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
			GTK_DIALOG_DESTROY_WITH_PARENT,
		        GTK_MESSAGE_ERROR,
		        GTK_BUTTONS_CLOSE,
		        "Could not write to file: model is incomplete/invalid"
	                );
	     gtk_dialog_run (GTK_DIALOG (dialog));
	     gtk_widget_destroy (dialog);
	     return FALSE;
	}
	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsi");
	gtk_file_filter_set_name(filter,"XMI-MSIM inputfiles");
	dialog = gtk_file_chooser_dialog_new ("Save simulation inputfile",
		GTK_WINDOW((GtkWidget *) data),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
		NULL);
		gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
		gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
																
	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		if (strcmp(filename+strlen(filename)-5, ".xmsi") != 0) {
			filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+6));
			strcat(filename,".xmsi");
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


		if (xmi_write_input_xml(filename, current->xi) == 1) {
			gtk_widget_destroy (dialog);
			if (last_saved != NULL) {
				free(last_saved->filename);
				xmi_free_input(last_saved->xi);
				free(last_saved);
			}
			last_saved = (struct undo_single *) malloc(sizeof(struct undo_single));
			xmi_copy_input(current->xi, &(last_saved->xi));
			last_saved->filename = strdup(filename);
			title = g_path_get_basename(filename);
			update_xmimsim_title_xmsi(title, data, filename);
			g_free(title);
			g_free (filename);							
			gtk_widget_set_sensitive(saveW,FALSE);
			gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
		}
		else {
			gtk_widget_destroy (dialog);
			dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
				GTK_DIALOG_DESTROY_WITH_PARENT,
	        		GTK_MESSAGE_ERROR,
	        		GTK_BUTTONS_CLOSE,
	        		"Could not write to file %s: model is incomplete/invalid",filename
                	);
     			gtk_dialog_run (GTK_DIALOG (dialog));
     			gtk_widget_destroy (dialog);
			g_free (filename);							
			return FALSE;
		}

	}
	else 
   		gtk_widget_destroy (dialog);
		


	return TRUE;


}
