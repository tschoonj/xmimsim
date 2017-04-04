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
#include "xmi_aux.h"
#include "xmi_private.h"
#include <gdk-pixbuf/gdk-pixbuf.h>
#include "xmimsim-gui-energies.h"
#include "xmimsim-gui-controls.h"
#include "xmimsim-gui-results.h"
#include "xmimsim-gui-tools.h"
#include "xmimsim-gui-batch.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-sources-dialog.h"
#include "xmimsim-gui-layer-dialog.h"
#include "xmimsim-gui-source-module.h"
#include "xmimsim-gui-source-abstract.h"
#include <stdio.h>
#include "xmi_xml.h"
#include "xmi_data_structs.h"
#include <glib.h>
#include <glib/gprintf.h>
#include <xraylib.h>
#include <gdk/gdkkeysyms.h>
#include <locale.h>
#include <unistd.h>
#include <gdk-pixbuf/gdk-pixdata.h>
#include <math.h>
#include <string.h>

#ifdef G_OS_UNIX
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#define real_xmimsim_pid ((int) xmimsim_pid)
#elif defined(G_OS_WIN32)
#include <windows.h>
#include "xmi_registry_win.h"
#define real_xmimsim_pid ((int) GetProcessId((HANDLE) xmimsim_pid))
#endif

#ifdef MAC_INTEGRATION
#import <Foundation/Foundation.h>
#include <CoreFoundation/CFBundle.h>
#include <ApplicationServices/ApplicationServices.h>
#include <AvailabilityMacros.h>
#include <gtkosxapplication.h>
#include <gdk/gdkquartz.h>
#include "xmi_resources_mac.h"
#define PRIMARY_ACCEL_KEY GDK_META_MASK

#if !defined(MAC_OS_X_VERSION_10_12) || MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_12

@interface NSWindow(AutomaticWindowTabbing)
+ (void)setAllowsAutomaticWindowTabbing:(BOOL)allow;
@end

#endif

#else
#define PRIMARY_ACCEL_KEY GDK_CONTROL_MASK
#endif

#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
	#include "xmimsim-gui-updater.h"
#endif

#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-notifications.h"


#ifdef HAVE_CXX
#include <gtkmm/main.h>
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
static GtkWidget *cutW;
static GtkWidget *copyW;
static GtkWidget *pasteW;
static GtkWidget *newW;
static GtkWidget *openW;
static GtkWidget *importW;
static GtkWidget *preferencesW;
static GtkWidget *tube_ebelW;
static GtkWidget *batchmodeW;


GtkWidget *saveW;
GtkWidget *save_asW;
#ifndef MAC_INTEGRATION
static GtkWidget *quitW;
#else
static GtkWidget *minimizeW;
static GtkWidget *bringToFrontW;
#endif
static GtkToolItem *newT;
static GtkToolItem *openT;
GtkToolItem *saveasT;
GtkToolItem *saveT;
static GtkToolItem *undoT;
static GtkToolItem *redoT;
static GtkToolItem *preferencesT;
static GtkToolItem *tube_ebelT;
static GtkToolItem *batchmodeT;
static GtkToolItem *cutT;
static GtkToolItem *copyT;
static GtkToolItem *pasteT;

#ifdef XMIMSIM_GUI_UPDATER_H
static GtkWidget *updatesW;
#endif

//composition
static GtkListStore *compositionL;
static GtkListStore *exc_compositionL;
static GtkListStore *det_compositionL;
static GtkListStore *crystal_compositionL;
static GtkWidget *compositionW;
static GtkWidget *exc_compositionW;
static GtkWidget *det_compositionW;
static GtkWidget *crystal_compositionW;

//geometry
static GtkWidget *d_sample_sourceW;
static GtkWidget *d_sample_source_ebW;
static GtkWidget *n_sample_orientation_xW;
static GtkWidget *n_sample_orientation_yW;
static GtkWidget *n_sample_orientation_zW;
static GtkWidget *n_sample_orientation_ebW;
static GtkWidget *p_detector_window_xW;
static GtkWidget *p_detector_window_yW;
static GtkWidget *p_detector_window_zW;
static GtkWidget *p_detector_window_ebW;
static GtkWidget *n_detector_orientation_xW;
static GtkWidget *n_detector_orientation_yW;
static GtkWidget *n_detector_orientation_zW;
static GtkWidget *n_detector_orientation_ebW;
static GtkWidget *area_detectorW;
static GtkWidget *area_detector_ebW;
static GtkWidget *collimator_heightW;
static GtkWidget *collimator_height_ebW;
static GtkWidget *collimator_diameterW;
static GtkWidget *collimator_diameter_ebW;
static GtkWidget *d_source_slitW;
static GtkWidget *d_source_slit_ebW;
static GtkWidget *slit_size_xW;
static GtkWidget *slit_size_yW;
static GtkWidget *slit_size_ebW;
static GtkWidget *geometry_helpW;
static GtkWidget *cs_window;

//energy
static struct energiesWidget *contWidget;
static struct energiesWidget *discWidget;

//detector
static GtkWidget *detector_typeW;
static GtkWidget *detector_gainW;
static GtkWidget *detector_live_timeW;
static GtkWidget *detector_pulse_widthW;
static GtkWidget *detector_zeroW;
static GtkWidget *detector_fanoW;
static GtkWidget *detector_noiseW;
static GtkWidget *detector_nchannelsW;

GtkWidget *notebook;

/*
 *
 * gulongs
 *
 */

//general
static gulong n_photons_intervalG;
static gulong n_photons_lineG;
static gulong n_interactions_trajectoryG;
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
static gulong geometry_helpG;

//detector
static gulong detector_typeG;
static gulong detector_gainG;
static gulong detector_live_timeG;
static gulong detector_pulse_widthG;
static gulong detector_zeroG;
static gulong detector_fanoG;
static gulong detector_noiseG;
static gulong detector_nchannelsG;


//notebook
gulong notebookG;

static gulong d_sample_source_enter_ebG;
static gulong n_sample_orientation_enter_ebG;
static gulong p_detector_window_enter_ebG;
static gulong n_detector_orientation_enter_ebG;
static gulong area_detector_enter_ebG;
static gulong collimator_height_enter_ebG;
static gulong collimator_diameter_enter_ebG;
static gulong d_source_slit_enter_ebG;
static gulong slit_size_enter_ebG;
static gulong d_sample_source_leave_ebG;
static gulong n_sample_orientation_leave_ebG;
static gulong p_detector_window_leave_ebG;
static gulong n_detector_orientation_leave_ebG;
static gulong area_detector_leave_ebG;
static gulong collimator_height_leave_ebG;
static gulong collimator_diameter_leave_ebG;
static gulong d_source_slit_leave_ebG;
static gulong slit_size_leave_ebG;


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

GdkAtom LayerAtom;
GtkTargetEntry LayerTE = {(gchar *) "xmi-msim-layer", 0, 0};




/*
 *
 *  xmi_composition structs
 *
 */

struct xmi_composition *compositionS;
struct xmi_composition *exc_compositionS;
struct xmi_composition *det_compositionS;
struct xmi_composition *crystal_compositionS;





/*
 *
 * undo buffer
 *
 */


static struct undo_single *redo_buffer;
struct undo_single *current;
static struct undo_single *last;
struct undo_single *last_saved;
static struct undo_single *undo_error = NULL;




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

GdkColor white = {(guint32) 0, (guint16) 65535, (guint16) 65535, (guint16) 65535};
GdkColor red = {(guint32) 0, (guint16) 65535, (guint16) 1000, (guint16) 1000};
GdkColor green = {(guint32) 0, (guint16) 0, (guint16) 65535, (guint16) 0};
GdkColor chartreuse_green;


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

struct control_widget *control_array = NULL;
int control_array_elements = 0;


struct dialog_helper_xmsa_data {
	GtkWidget *window;
	gchar *filename;
};

static gboolean dialog_helper_xmsa_cb(struct dialog_helper_xmsa_data *data);


void reset_undo_buffer(struct xmi_input *xi_new, const char *filename);
static void change_all_values(struct xmi_input *);
static void change_all_values_general(struct xmi_input *new_input);
static void change_all_values_composition(struct xmi_input *new_input);
static void change_all_values_geometry(struct xmi_input *new_input);
static void change_all_values_excitation(struct xmi_input *new_input);
static void change_all_values_beamabsorbers(struct xmi_input *new_input);
static void change_all_values_detectionabsorbers(struct xmi_input *new_input);
static void change_all_values_detectorsettings(struct xmi_input *new_input);
void load_from_file_cb(GtkWidget *, gpointer);
void saveas_cb(GtkWidget *widget, gpointer data);
gboolean saveas_function(GtkWidget *widget, gpointer data);
gboolean save_function(GtkWidget *widget, gpointer data);
void save_cb(GtkWidget *widget, gpointer data);
#ifdef MAC_INTEGRATION
void quit_program_cb(GtkosxApplication *app, gpointer data);
gboolean quit_blocker_mac_cb(GtkosxApplication *app, gpointer data);
#else
void quit_program_cb(GtkWidget *widget, gpointer data);
#endif
static void new_cb(GtkWidget *widget, gpointer data);
static void switch_tab_click(GtkWidget *widget, gpointer data);
static void import_cb(GtkWidget *widget, gpointer data);
static void cut_button_clicked_cb(GtkWidget *widget, gpointer data);
static void copy_button_clicked_cb(GtkWidget *widget, gpointer data);
static void paste_button_clicked_cb(GtkWidget *widget, gpointer data);
gboolean process_pre_file_operation (GtkWidget *window);
static void geometry_help_clicked_cb(GtkWidget *window);


struct import_undo_data {
	struct xmi_input *xi;
	int undo_rv;
	char *filename;
};

struct control_widget {
	GtkWidget *widget;
	gboolean sensitivity;
};

static void get_control_widgets(GtkWidget *start_widget, struct control_widget **array, int *array_elements) {
	//We need to fetch all:
	//1) entries
	//2) buttons
	//3) textviews
	//4) treeviews
	//
	//If it's a button, keep track of its sensitivity

	struct control_widget *local_array = *array;
	int local_array_elements = *array_elements;

	if (GTK_IS_ENTRY(start_widget) ||
		GTK_IS_BUTTON(start_widget) ||
		GTK_IS_TEXT_VIEW(start_widget) ||
		GTK_IS_TREE_VIEW(start_widget) ||
		GTK_IS_COMBO_BOX(start_widget) ||
		GTK_IS_IMAGE_MENU_ITEM(start_widget) ||
		GTK_IS_TOOL_BUTTON(start_widget)) {
		local_array = (struct control_widget *) g_realloc(local_array, sizeof(struct control_widget)*++local_array_elements);
		local_array[local_array_elements-1].widget = start_widget;

	}
	else if (GTK_IS_BIN(start_widget)) {
		get_control_widgets(gtk_bin_get_child(GTK_BIN(start_widget)), &local_array, &local_array_elements);
	}
	else if (GTK_IS_CONTAINER(start_widget)) {
		GList *children = gtk_container_get_children(GTK_CONTAINER(start_widget));
		GList *l;
		for (l = children; l != NULL; l = l->next) {
			get_control_widgets((GtkWidget *) l->data, &local_array, &local_array_elements);
                }
	}

	*array = local_array;
	*array_elements = local_array_elements;
}

static void add_to_control_widgets(GtkWidget *widget) {
	//control_array = g_realloc(control_array, sizeof(struct control_widget)*++control_array_elements);
	//control_array[control_array_elements-1].widget = widget;
	get_control_widgets(widget, &control_array, &control_array_elements);

}


static void adjust_control_widgets(GtkWidget *widget, gboolean sensitivity) {

	int i;


	for (i = 0 ; i < control_array_elements ; i++) {
		if (control_array[i].widget == widget)
			continue;
		if (sensitivity == FALSE && GTK_IS_BUTTON(control_array[i].widget)) {
			control_array[i].sensitivity = gtk_widget_get_sensitive(control_array[i].widget);
			gtk_widget_set_sensitive(control_array[i].widget, FALSE);
		}
		else if (sensitivity == TRUE && GTK_IS_BUTTON(control_array[i].widget)) {
			gtk_widget_set_sensitive(control_array[i].widget, control_array[i].sensitivity);
		}
		else {
			gtk_widget_set_sensitive(control_array[i].widget, sensitivity);
		}
	}
}

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

	kill_current_job();
	_exit(0);
}

#elif defined(G_OS_WIN32)

#endif

static gchar *get_message_string(int kind) {
	gchar *message;
	switch (kind) {
		case OUTPUTFILE:
			message = g_strdup("selection of outputfile");
			break;
		case N_PHOTONS_INTERVAL:
			message = g_strdup("change of number of photons per interval");
			break;
		case N_PHOTONS_LINE:
			message = g_strdup("change of number of photons per line");
			break;
		case N_INTERACTIONS_TRAJECTORY:
			message = g_strdup("change of number of interactions per trajectory");
			break;
		case COMPOSITION_REFERENCE:
			message = g_strdup( "change of reference layer");
			break;
		case COMPOSITION_ORDER:
			message = g_strdup("change of composition ordering");
			break;
		case COMPOSITION_DELETE:
			message = g_strdup("removal of layer");
			break;
		case COMPOSITION_ADD:
			message = g_strdup("addition of layer");
			break;
		case COMPOSITION_EDIT:
			message = g_strdup("editing of layer");
			break;
		case COMPOSITION_PASTE:
			message = g_strdup("pasting of layer");
			break;
		case COMPOSITION_CUT:
			message = g_strdup("cutting of layer");
			break;
		case D_SAMPLE_SOURCE:
			message = g_strdup("change of sample-source distance");
			break;
		case N_SAMPLE_ORIENTATION_X:
			message = g_strdup("change of sample orientation vector x");
			break;
		case N_SAMPLE_ORIENTATION_Y:
			message = g_strdup("change of sample orientation vector y");
			break;
		case N_SAMPLE_ORIENTATION_Z:
			message = g_strdup("change of sample orientation vector z");
			break;
		case P_DETECTOR_WINDOW_X:
			message = g_strdup("change of detector window position x");
			break;
		case P_DETECTOR_WINDOW_Y:
			message = g_strdup("change of detector window position y");
			break;
		case P_DETECTOR_WINDOW_Z:
			message = g_strdup("change of detector window position z");
			break;
		case N_DETECTOR_ORIENTATION_X:
			message = g_strdup("change of detector orientation x");
			break;
		case N_DETECTOR_ORIENTATION_Y:
			message = g_strdup("change of detector orientation y");
			break;
		case N_DETECTOR_ORIENTATION_Z:
			message = g_strdup("change of detector orientation z");
			break;
		case AREA_DETECTOR:
			message = g_strdup("change of active detector area");
			break;
		case COLLIMATOR_HEIGHT:
			message = g_strdup("change of collimator height");
			break;
		case COLLIMATOR_DIAMETER:
			message = g_strdup("change of collimator opening diameter");
			break;
		case D_SOURCE_SLIT:
			message = g_strdup("change of source-slits distance");
			break;
		case SLIT_SIZE_X:
			message = g_strdup("change of slit size x");
			break;
		case SLIT_SIZE_Y:
			message = g_strdup("change of slit size y");
			break;
		case DISCRETE_ENERGY_ADD:
			message = g_strdup("addition of discrete energy");
			break;
		case DISCRETE_ENERGY_IMPORT_ADD:
			message = g_strdup("addition of imported discrete energies");
			break;
		case DISCRETE_ENERGY_IMPORT_REPLACE:
			message = g_strdup("replacing energies with imported discrete energies");
			break;
		case DISCRETE_ENERGY_CLEAR:
			message = g_strdup("clearing of all discrete energies");
			break;
		case DISCRETE_ENERGY_EDIT:
			message = g_strdup("editing of discrete energy");
			break;
		case DISCRETE_ENERGY_SCALE:
			message = g_strdup("scaling of discrete energies");
			break;
		case DISCRETE_ENERGY_DELETE:
			message = g_strdup("deletion of discrete energies");
			break;
		case CONTINUOUS_ENERGY_ADD:
			message = g_strdup("addition of continuous energy");
			break;
		case CONTINUOUS_ENERGY_IMPORT_ADD:
			message = g_strdup("addition of imported continuous energies");
			break;
		case CONTINUOUS_ENERGY_IMPORT_REPLACE:
			message = g_strdup("replacing energies with imported continuous energies");
			break;
		case CONTINUOUS_ENERGY_CLEAR:
			message = g_strdup("clearing of all continuous energies");
			break;
		case CONTINUOUS_ENERGY_SCALE:
			message = g_strdup("scaling of continuous energies");
			break;
		case CONTINUOUS_ENERGY_EDIT:
			message = g_strdup("editing of continuous energy");
			break;
		case CONTINUOUS_ENERGY_DELETE:
			message = g_strdup("deletion of continuous energy");
			break;
		case SOURCE_SPECTRUM_REPLACE:
			message = g_strdup("importing external spectrum");
			break;
		case SOURCE_SPECTRUM_ADD:
			message = g_strdup("adding external spectrum");
			break;
		case EXC_COMPOSITION_ORDER:
			message = g_strdup("change of excitation absorber ordering");
			break;
		case EXC_COMPOSITION_DELETE:
			message = g_strdup("removal of excitation absorber layer");
			break;
		case EXC_COMPOSITION_ADD:
			message = g_strdup("addition of excitation absorber layer");
			break;
		case EXC_COMPOSITION_PASTE:
			message = g_strdup("pasting of excitation absorber layer");
			break;
		case EXC_COMPOSITION_CUT:
			message = g_strdup("cutting of excitation absorber layer");
			break;
		case EXC_COMPOSITION_EDIT:
			message = g_strdup("editing of excitation absorber layer");
			break;
		case DET_COMPOSITION_ORDER:
			message = g_strdup("change of detector absorber ordering");
			break;
		case DET_COMPOSITION_DELETE:
			message = g_strdup("removal of detector absorber layer");
			break;
		case DET_COMPOSITION_ADD:
			message = g_strdup("addition of detector absorber layer");
			break;
		case DET_COMPOSITION_PASTE:
			message = g_strdup("pasting of detector absorber layer");
			break;
		case DET_COMPOSITION_CUT:
			message = g_strdup("cutting of detector absorber layer");
			break;
		case DET_COMPOSITION_EDIT:
			message = g_strdup("editing of detector absorber layer");
			break;
		case CRYSTAL_COMPOSITION_ORDER:
			message = g_strdup("change of crystal absorber ordering");
			break;
		case CRYSTAL_COMPOSITION_DELETE:
			message = g_strdup("removal of crystal absorber layer");
			break;
		case CRYSTAL_COMPOSITION_ADD:
			message = g_strdup("addition of crystal absorber layer");
			break;
		case CRYSTAL_COMPOSITION_PASTE:
			message = g_strdup("pasting of crystal absorber layer");
			break;
		case CRYSTAL_COMPOSITION_CUT:
			message = g_strdup("cutting of crystal absorber layer");
			break;
		case CRYSTAL_COMPOSITION_EDIT:
			message = g_strdup("editing of crystal absorber layer");
			break;
		case DETECTOR_TYPE:
			message = g_strdup("change of detector type");
			break;
		case DETECTOR_GAIN:
			message = g_strdup("change of detector gain");
			break;
		case DETECTOR_NCHANNELS:
			message = g_strdup("change of detector number of channels");
			break;
		case DETECTOR_LIVE_TIME:
			message = g_strdup("change of live time");
			break;
		case DETECTOR_PULSE_WIDTH:
			message = g_strdup("change of detector pulse width");
			break;
		case DETECTOR_ZERO:
			message = g_strdup("change of detector zero");
			break;
		case DETECTOR_NOISE:
			message = g_strdup("change of detector noise");
			break;
		case DETECTOR_FANO:
			message = g_strdup("change of detector Fano factor");
			break;
		default:
			g_fprintf(stderr, "Invalid value in get_message_string. Fatal error\n");
			exit(1);
	}
	return message;
}

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

void chooser_activated_cb(GtkRecentChooser *chooser, gpointer *data) {
	gchar *fileuri = gtk_recent_chooser_get_current_uri(chooser);
	gchar *filename = g_filename_from_uri(fileuri, NULL, NULL);
	g_free(fileuri);
	GtkWidget *dialog;
	gchar *title;
	struct xmi_input *xi;


	if (g_ascii_strcasecmp(filename+strlen(filename)-5,".xmsi") == 0) {
		if (process_pre_file_operation((GtkWidget *) data) == FALSE)
			return;
		gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),input_page);
		if (xmi_read_input_xml(filename, &xi) == 1) {
			//success reading it in...
			change_all_values(xi);
			//reset redo_buffer
			reset_undo_buffer(xi, filename);
			title = g_path_get_basename(filename);
			update_xmimsim_title_xmsi(title, (GtkWidget *) data, filename);
			g_free(title);
			adjust_save_buttons();
		}
		else {
			dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
				GTK_DIALOG_DESTROY_WITH_PARENT,
	        		GTK_MESSAGE_ERROR,
	        		GTK_BUTTONS_CLOSE,
	        		"Could not read file %s: model is incomplete/invalid",filename
	                	);
	    gtk_dialog_run (GTK_DIALOG (dialog));
			gtk_widget_destroy (dialog);
		}
	}
	else if (g_ascii_strcasecmp(filename+strlen(filename)-5,".xmso") == 0) {
		gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),results_page);
		if (plot_spectra_from_file(filename) == 1) {
			gchar *temp_base = g_path_get_basename(filename);
			update_xmimsim_title_xmso(temp_base, (GtkWidget *) data, filename);
			g_free(temp_base);
		}
		else {
			dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
				GTK_DIALOG_DESTROY_WITH_PARENT,
	        		GTK_MESSAGE_ERROR,
	        		GTK_BUTTONS_CLOSE,
	        		"Could not read file %s",filename
	               	);
			gtk_dialog_run (GTK_DIALOG (dialog));
			gtk_widget_destroy (dialog);
		}
	}
	else if (g_ascii_strcasecmp(filename+strlen(filename)-5,".xmsa") == 0) {
		struct dialog_helper_xmsa_data *my_data = (struct dialog_helper_xmsa_data *) g_malloc(sizeof(struct dialog_helper_xmsa_data));
		my_data->window = (GtkWidget *)data;
		my_data->filename = filename;
		dialog_helper_xmsa_cb(my_data);
		return;
	}
	else {
	}

	g_free(filename);
	gtk_widget_grab_focus(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook),input_page));
}

static void query_source_modules_dir(gchar *dirname) {
	GError *error = NULL;
	GDir *dir = g_dir_open(dirname, 0, &error);

	if (dir == NULL) {
		g_warning("Could not open %s: %s\n", dirname, error->message);
		return;
	}

	// dir exists, let's start creating modules out of these files and load them if possible
	const gchar *file = NULL;
	while ((file = g_dir_read_name(dir)) != NULL) {
		if (!g_str_has_suffix(file, G_MODULE_SUFFIX))
			continue;
		if (!g_str_has_prefix(file, "xmimsim-gui-source-"))
			continue;
		gchar *full_file = g_build_filename(dirname, file, NULL);
		XmiMsimGuiSourceModule *module = xmi_msim_gui_source_module_new(full_file);
		g_free(full_file);
		if (module != NULL && g_type_module_use(G_TYPE_MODULE(module)) == FALSE)
			g_type_module_unuse(G_TYPE_MODULE(module));
	}

	g_dir_close(dir);
}

static gboolean query_source_modules(void) {
	gchar *sources_dir;

	// first add the system-wide modules
	g_debug("Querying system-wide installed modules");
#ifdef G_OS_WIN32
	if (xmi_registry_win_query(XMI_REGISTRY_WIN_SOURCES, &sources_dir) == 0) {
		return FALSE;
	}
#elif defined(MAC_INTEGRATION)
	if (xmi_resources_mac_query(XMI_RESOURCES_MAC_SOURCES, &sources_dir) == 0) {
		return FALSE;
	}
#else
	sources_dir = g_strdup(XMIMSIM_SOURCES_DEFAULT);
#endif
	query_source_modules_dir(sources_dir);
	g_free(sources_dir);

#ifdef MAC_INTEGRATION
        NSAutoreleasePool *pool = [[NSAutoreleasePool alloc]init];
        NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask,TRUE);
        NSString *documentsDirectory = [paths objectAtIndex:0];
        const gchar *config_dir = [documentsDirectory cStringUsingEncoding:NSUTF8StringEncoding];
#else
        const gchar *config_dir = g_get_user_config_dir();
#endif

	//first check if the preferences file exists!
	sources_dir = g_strdup_printf("%s" G_DIR_SEPARATOR_S "XMI-MSIM" G_DIR_SEPARATOR_S "sources", config_dir);
	
	g_debug("Querying locally installed modules");
	query_source_modules_dir(sources_dir);
	g_free(sources_dir);


	// get kids
	guint ntypes;
	GType *source_types = g_type_children(XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT, &ntypes);
	guint i;
	for (i = 0 ; i < ntypes ; i++)
		g_debug("source %s found\n", g_type_name(source_types[i]));


#ifdef MAC_INTEGRATION
        [pool drain];
#endif
	return FALSE;
}

#ifdef XMIMSIM_GUI_UPDATER_H

static gboolean check_for_updates_on_init_cb(GtkWidget *window) {
	char *max_version;

	int rv;

	//do this only if it is allowed by the preferences
	union xmimsim_prefs_val prefs;
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, &prefs) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(window),
			GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR , GTK_BUTTONS_CLOSE, "A serious error occurred while checking\nthe preferences file.\nThe program will abort.");
		gtk_dialog_run(GTK_DIALOG(dialog));
	        gtk_widget_destroy(dialog);
#ifdef MAC_INTEGRATION
		GtkosxApplication *app = (GtkosxApplication *) g_object_new(GTKOSX_TYPE_APPLICATION,NULL);
		quit_program_cb(app, window);
#else
		quit_program_cb(window, window);
#endif
	}
	if (prefs.b == FALSE)
		return FALSE;


	gtk_widget_set_sensitive(updatesW,FALSE);
	char *message = NULL;
	rv = check_for_updates(&max_version, &message);
	if (rv == XMIMSIM_UPDATES_ERROR) {
		//do nothing
	}
	else if (rv == XMIMSIM_UPDATES_AVAILABLE) {
		rv = download_updates(window, max_version, message);
		if (rv == 1) {
			//exit XMI-MSIM
#ifdef MAC_INTEGRATION
			GtkosxApplication *app = (GtkosxApplication *) g_object_new(GTKOSX_TYPE_APPLICATION,NULL);
			quit_program_cb(app, window);
#else
			quit_program_cb(window, window);
#endif
		}
	}
	else if (rv == XMIMSIM_UPDATES_NONE) {
		//do nothing
	}
	gtk_widget_set_sensitive(updatesW,TRUE);




	return FALSE;
}



static void check_for_updates_on_click_cb(GtkWidget *widget, GtkWidget *window) {
	char *max_version, *message = NULL;

	int rv;

	gtk_widget_set_sensitive(updatesW,FALSE);
	rv = check_for_updates(&max_version, &message);

	if (rv == XMIMSIM_UPDATES_ERROR) {
		GtkWidget *update_dialog = gtk_message_dialog_new(GTK_WINDOW(window),
		GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR , GTK_BUTTONS_CLOSE, "An error occurred while checking for updates.\nCheck your internet connection\nor try again later.");
		gtk_dialog_run(GTK_DIALOG(update_dialog));
		gtk_widget_destroy(update_dialog);
	}
	else if (rv == XMIMSIM_UPDATES_AVAILABLE) {
		rv = download_updates(window, max_version, message);
		if (rv == 1) {
			//exit XMI-MSIM
#ifdef MAC_INTEGRATION
			GtkosxApplication *app = (GtkosxApplication *) g_object_new(GTKOSX_TYPE_APPLICATION,NULL);
			quit_program_cb(app, window);
#else
			quit_program_cb(window, window);
#endif
		}
	}
	else if (rv == XMIMSIM_UPDATES_NONE) {
		GtkWidget *update_dialog = gtk_message_dialog_new(GTK_WINDOW(window),
		GTK_DIALOG_MODAL, GTK_MESSAGE_INFO , GTK_BUTTONS_CLOSE, "No updates are available at this time.\nPlease check again later.");
		gtk_dialog_run(GTK_DIALOG(update_dialog));
		gtk_widget_destroy(update_dialog);
	}


	gtk_widget_set_sensitive(updatesW,TRUE);


	return;
}
#endif




gboolean process_pre_file_operation (GtkWidget *window) {
	int check_status;
	GtkWidget *dialog = NULL;
	GtkWidget *content;
	gint dialog_rv;
	GtkWidget *label;

	//check if last changes have been saved, because they will be lost otherwise!
	check_changes_saved(&check_status);

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
			if(save_function(dialog, (gpointer) dialog) == FALSE) {
				gtk_widget_destroy(dialog);
				return FALSE;
			}

		}


		gtk_widget_destroy(dialog);
	}

	return TRUE;
}

void update_xmimsim_title_xmsi(const char *new_title, GtkWidget *my_window, const char *filename) {
	g_free(xmimsim_title_xmsi);
	xmimsim_title_xmsi = g_strdup_printf(XMIMSIM_TITLE_PREFIX "%s",new_title);

#ifdef MAC_INTEGRATION
	if (xmimsim_filename_xmsi)
		g_free(xmimsim_filename_xmsi);

	if (filename != NULL) {
		xmimsim_filename_xmsi = g_strdup(filename);
	}
	else {
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

void update_xmimsim_title_xmso(const char *new_title, GtkWidget *my_window, const char *filename) {
	g_free(xmimsim_title_xmso);
	xmimsim_title_xmso = g_strdup_printf(XMIMSIM_TITLE_PREFIX "%s",new_title);

#ifdef MAC_INTEGRATION
	if (xmimsim_filename_xmso)
		g_free(xmimsim_filename_xmso);

	if (filename != NULL) {
		xmimsim_filename_xmso = g_strdup(filename);
	}
	else {
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


static void notebook_page_changed_cb(GtkNotebook *my_notebook, gpointer pageptr, guint page, gpointer data) {
	GtkWidget *my_window = (GtkWidget *) data;


	if ((gint) page == results_page) {
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
	else if ((gint) page == control_page || (gint) page == input_page) {
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

struct undo_single *check_changes_saved(int *status) {
	struct undo_single *temp = current;
	GtkTextIter iterb, itere;

	gchar *commentsText;
	int commentsEqual;



	//get text from comments...
	gtk_text_buffer_get_bounds(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW)),&iterb, &itere);

	if (gtk_text_iter_equal (&iterb, &itere) == TRUE) {
		commentsText = g_strdup("");
	}
	else {
		commentsText = g_strdup(gtk_text_buffer_get_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW)),&iterb, &itere, FALSE));
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
	gtk_file_filter_add_pattern(filter,"*.[xX][mM][sS][oO]");
	gtk_file_filter_set_name(filter,"XMI-MSIM outputfiles");

	dialog = gtk_file_chooser_dialog_new("Select the outputfile for the simulation",
		GTK_WINDOW((GtkWidget *) data),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL
	);

	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		//check extension
		xmi_msim_gui_utils_ensure_extension(&filename, ".xmso");

		gtk_entry_set_text(GTK_ENTRY(outputfileW), filename);
		update_undo_buffer(OUTPUTFILE, (void *) outputfileW);

		g_free (filename);
	}

	gtk_widget_destroy (dialog);

	return;
}


static void layer_selection_changed_cb (GtkTreeSelection *selection, gpointer data) {
	GtkTreeIter iter,temp_iter;
	GtkTreeModel *model;
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
	GtkTreeView *tree;
	gboolean valid;
	int index,nindices;
	gint *indices;
	int i, updateKind = -1;
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
	if (mb->matrixKind == COMPOSITION) {
		composition = compositionS;
		tree = GTK_TREE_VIEW(compositionW);
	}
	else if (mb->matrixKind == EXC_COMPOSITION) {
		composition = exc_compositionS;
		tree = GTK_TREE_VIEW(exc_compositionW);
	}
	else if (mb->matrixKind == DET_COMPOSITION) {
		composition = det_compositionS;
		tree = GTK_TREE_VIEW(det_compositionW);
	}
	else if (mb->matrixKind == CRYSTAL_COMPOSITION){
		composition = crystal_compositionS;
		tree = GTK_TREE_VIEW(crystal_compositionW);
	}

	if (mb->buttonKind == BUTTON_ADD) {

		GtkWidget *dialog = xmi_msim_gui_layer_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(widget)), XMI_MSIM_GUI_LAYER_DIALOG_ADD);

		if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
			struct xmi_layer *layer = xmi_msim_gui_layer_dialog_get_layer(XMI_MSIM_GUI_LAYER_DIALOG(dialog));
			if (mb->matrixKind == COMPOSITION) {
				updateKind = COMPOSITION_ADD;
			}
			else if (mb->matrixKind == EXC_COMPOSITION) {
				updateKind = EXC_COMPOSITION_ADD;
			}
			else if (mb->matrixKind == DET_COMPOSITION) {
				updateKind = DET_COMPOSITION_ADD;
			}
			else if (mb->matrixKind == CRYSTAL_COMPOSITION) {
				updateKind = CRYSTAL_COMPOSITION_ADD;
			}

			//adding layer
			composition->layers = (struct xmi_layer*) g_realloc(composition->layers, sizeof(struct xmi_layer)*(++composition->n_layers));
			xmi_copy_layer2(layer,composition->layers+composition->n_layers-1);

			GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(tree));

			gtk_list_store_append(store, &iter);
			i = composition->n_layers-1;

			gchar *elementString = xmi_msim_gui_utils_get_layer_element_string(&composition->layers[i]);
			if (mb->matrixKind == COMPOSITION) {
				if (composition->n_layers == 1)
					composition->reference_layer = 1;

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

			g_free(elementString);
			update_undo_buffer(updateKind, (void *) store);

			xmi_free_layer(layer);
			g_free(layer);
		}
		gtk_widget_destroy(dialog);
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
		indices = (gint *) g_malloc(sizeof(gint)*nindices);
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
				update_undo_buffer(COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == EXC_COMPOSITION)
				update_undo_buffer(EXC_COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == DET_COMPOSITION)
				update_undo_buffer(DET_COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == CRYSTAL_COMPOSITION)
				update_undo_buffer(CRYSTAL_COMPOSITION_ORDER, (void*) mb->store);
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
				update_undo_buffer(COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == EXC_COMPOSITION)
				update_undo_buffer(EXC_COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == DET_COMPOSITION)
				update_undo_buffer(DET_COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == CRYSTAL_COMPOSITION)
				update_undo_buffer(CRYSTAL_COMPOSITION_ORDER, (void*) mb->store);
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
				update_undo_buffer(COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == EXC_COMPOSITION)
				update_undo_buffer(EXC_COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == DET_COMPOSITION)
				update_undo_buffer(DET_COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == CRYSTAL_COMPOSITION)
				update_undo_buffer(CRYSTAL_COMPOSITION_ORDER, (void*) mb->store);
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
				update_undo_buffer(COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == EXC_COMPOSITION)
				update_undo_buffer(EXC_COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == DET_COMPOSITION)
				update_undo_buffer(DET_COMPOSITION_ORDER, (void*) mb->store);
			else if (mb->matrixKind == CRYSTAL_COMPOSITION)
				update_undo_buffer(CRYSTAL_COMPOSITION_ORDER, (void*) mb->store);
		}
		else if (mb->buttonKind == BUTTON_DELETE) {
			//delete the selected line
			//watch out for reference layer... for now... leave it to the user
			//update composition
			xmi_free_layer(composition->layers+index);
			if (nindices > 1) {
				for (i = index ;  i < nindices-1 ; i++)
					composition->layers[i] = composition->layers[i+1];
			}
			composition->layers = (struct xmi_layer*) g_realloc(composition->layers, sizeof(struct xmi_layer)*(nindices-1));
			composition->n_layers--;
			gtk_list_store_remove(mb->store,&iter);
			if (mb->matrixKind == COMPOSITION) {
				//reference layer may have to be updated
				if (nindices > 1) {
					if (composition->reference_layer == index+1) {
						//reference_layer was deleted -> only a problem if selected layer is the last one
						if (index == nindices -1) {
							composition->reference_layer--;
							//get iter to last element
							gchar *path_string = g_strdup_printf("%i",nindices-2);
							gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(mb->store), &iter, path_string);
							gtk_list_store_set(mb->store,&iter, REFERENCE_COLUMN, TRUE, -1);
							g_free(path_string);
						}
						else {
							gtk_list_store_set(mb->store,&iter, REFERENCE_COLUMN, TRUE, -1);
						}
					}
					else if (composition->reference_layer > index+1)
						composition->reference_layer--;
				}
				update_undo_buffer(COMPOSITION_DELETE, (void*) mb->store);
			}
			else if (mb->matrixKind == EXC_COMPOSITION)
				update_undo_buffer(EXC_COMPOSITION_DELETE, (void*) mb->store);
			else if (mb->matrixKind == DET_COMPOSITION)
				update_undo_buffer(DET_COMPOSITION_DELETE, (void*) mb->store);
			else if (mb->matrixKind == CRYSTAL_COMPOSITION)
				update_undo_buffer(CRYSTAL_COMPOSITION_DELETE, (void*) mb->store);
			gtk_widget_grab_focus(GTK_WIDGET(gtk_tree_selection_get_tree_view(mb->select)));
			if (gtk_tree_model_iter_n_children(GTK_TREE_MODEL(mb->store), NULL) == 0) {
				gtk_widget_set_sensitive(GTK_WIDGET(cutT), FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(copyT), FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(cutW), FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(copyW), FALSE);
			}

		}
		else if (mb->buttonKind == BUTTON_EDIT) {
			GtkWidget *dialog = xmi_msim_gui_layer_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(widget)), XMI_MSIM_GUI_LAYER_DIALOG_EDIT);
			xmi_msim_gui_layer_dialog_set_layer(XMI_MSIM_GUI_LAYER_DIALOG(dialog), composition->layers + index);
			if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
				struct xmi_layer *layer = xmi_msim_gui_layer_dialog_get_layer(XMI_MSIM_GUI_LAYER_DIALOG(dialog));
				if (mb->matrixKind == COMPOSITION) {
					updateKind = COMPOSITION_EDIT;
				}
				else if (mb->matrixKind == EXC_COMPOSITION) {
					updateKind = EXC_COMPOSITION_EDIT;
				}
				else if (mb->matrixKind == DET_COMPOSITION) {
					updateKind = DET_COMPOSITION_EDIT;
				}
				else if (mb->matrixKind == CRYSTAL_COMPOSITION) {
					updateKind = CRYSTAL_COMPOSITION_EDIT;
				}
				//editing layer
				g_free(composition->layers[index].Z);
				g_free(composition->layers[index].weight);
				xmi_copy_layer2(layer,composition->layers + index);

				GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(tree));

				gchar *elementString = xmi_msim_gui_utils_get_layer_element_string(composition->layers+index);
				if (mb->matrixKind == COMPOSITION) {
					gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, composition->layers[index].n_elements,
					ELEMENTS_COLUMN, elementString,
					DENSITY_COLUMN, composition->layers[index].density,
					THICKNESS_COLUMN,composition->layers[index].thickness,
					REFERENCE_COLUMN,(index + 1 == composition->reference_layer) ? TRUE : FALSE ,
					-1
					);
				}
				else {
					gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, composition->layers[index].n_elements,
					ELEMENTS_COLUMN, elementString,
					DENSITY_COLUMN, composition->layers[index].density,
					THICKNESS_COLUMN, composition->layers[index].thickness,
					-1
					);
				}

				g_free(elementString);
				update_undo_buffer(updateKind, (void*) store);

				xmi_free_layer(layer);
				g_free(layer);
			}
			gtk_widget_destroy(dialog);
		}
		else {
			fprintf(stderr,"Unknown button clicked\n");
			exit(1);
		}

	}

	//return focus to treeview!
	gtk_widget_grab_focus(GTK_WIDGET(tree));
	//make sure a layer is selected
	model = gtk_tree_view_get_model(tree);
	GtkTreeSelection *select = gtk_tree_view_get_selection(tree);
	if (gtk_tree_model_iter_n_children(model, NULL) > 0 && gtk_tree_selection_count_selected_rows(select) == 0) {
		gtk_tree_model_get_iter_first(model, &iter);
		GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(model), &iter);
		gtk_tree_selection_select_path(select, path);
		gtk_tree_path_free(path);
	}

	g_signal_emit_by_name(G_OBJECT(select), "changed");

	return;
}

static gboolean layers_backspace_key_clicked(GtkWidget *widget, GdkEventKey *event, gpointer data) {
	if (event->keyval == gdk_keyval_from_name("BackSpace")) {
		layers_button_clicked_cb(widget,data);
		return TRUE;
	}

	return FALSE;
}


struct reference_toggle {
	GtkListStore *store;
	GtkTreeModel *model;
};

static void reference_layer_toggled_cb(GtkCellRendererToggle *renderer, gchar *path, gpointer data) {
	struct reference_toggle *rt = (struct reference_toggle *) data;
	GtkTreeIter iter,iter2;
	GValue reference = {0};
	gboolean valid;
	GtkTreePath *tpath, *tpath2;
	int new_reference;


#if DEBUG == 1
	fprintf(stdout,"reference_layer_toggled: path: %s\n",path);
#endif

	tpath=gtk_tree_path_new_from_string(path);

	//convert path to iter
	if (gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(rt->store), &iter, path) == FALSE) {
		fprintf(stderr,"Error in reference_layer_toggled_cb\n");
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
	new_reference = 1 + (int) g_ascii_strtoll(path, NULL, 10);
	if (new_reference != compositionS->reference_layer) {
		compositionS->reference_layer = new_reference;
		//update undo buffer
		update_undo_buffer(COMPOSITION_REFERENCE, (void*) rt->store);
	}
}

void matrix_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer user_data) {

	layers_button_clicked_cb(GTK_WIDGET(tree_view), user_data);

	return;
}

static void layer_print_double(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gdouble value;
	gchar *double_text;

	gtk_tree_model_get(tree_model,iter, GPOINTER_TO_INT(data), &value,-1);

	g_object_set(G_OBJECT(renderer), "xalign", 0.5, NULL);
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	double_text = g_strdup_printf("%lg",value);
	g_object_set(G_OBJECT(renderer), "text", double_text, NULL);

	g_free(double_text);

	return;

}
static void layer_right_click_menu_delete_cb(GtkWidget *widget, gpointer data) {
	layers_button_clicked_cb(widget, data);
}

struct clipboard_data {
	guchar *data;
	gint length;
};

static void clipboard_clear_layer_cb(GtkClipboard *clipboard, struct clipboard_data *cd) {
	g_free(cd->data);
	g_free(cd);
	return;
}

static void clipboard_get_layer_cb(GtkClipboard *clipboard, GtkSelectionData *selection_data, guint info, struct clipboard_data *data) {
	gtk_selection_data_set(selection_data, LayerAtom, 8, data->data, data->length);
}

static void clipboard_receive_layer_cb(GtkClipboard *clipboard, GtkSelectionData *selection_data, struct matrix_button *mb) {

	const guchar *data = gtk_selection_data_get_data(selection_data);
	struct xmi_layer *clipboard_layer = (struct xmi_layer *) g_malloc(sizeof(struct xmi_layer));
	memcpy(&clipboard_layer->n_elements, data, sizeof(int));
	clipboard_layer->Z = (int *) xmi_memdup(data+sizeof(int), sizeof(int)*clipboard_layer->n_elements);
	clipboard_layer->weight = (double *) xmi_memdup(data+sizeof(int)+sizeof(int)*clipboard_layer->n_elements, sizeof(double)*clipboard_layer->n_elements);
	memcpy(&clipboard_layer->density, data+sizeof(int)+(sizeof(int)+sizeof(double))*clipboard_layer->n_elements, sizeof(double));
	memcpy(&clipboard_layer->thickness, data+sizeof(int)+(sizeof(int)+sizeof(double))*clipboard_layer->n_elements+sizeof(double), sizeof(double));

	//xmi_print_layer(stdout, clipboard_layer, 1);

	struct xmi_composition *composition = NULL;
	GtkTreeIter iter;
	GtkListStore *store = NULL;
	gchar *elementString;
	int i;
	int updateKind = -1;

	if (mb->matrixKind == COMPOSITION) {
		composition = compositionS;
		store = compositionL;
		updateKind = COMPOSITION_PASTE;
	}
	else if (mb->matrixKind == EXC_COMPOSITION) {
		composition = exc_compositionS;
		store = exc_compositionL;
		updateKind = EXC_COMPOSITION_PASTE;
	}
	else if (mb->matrixKind == DET_COMPOSITION) {
		composition = det_compositionS;
		store = det_compositionL;
		updateKind = DET_COMPOSITION_PASTE;
	}
	else if (mb->matrixKind == CRYSTAL_COMPOSITION) {
		composition = crystal_compositionS;
		store = crystal_compositionL;
		updateKind = CRYSTAL_COMPOSITION_PASTE;
	}
	//adding layer
	composition->layers = (struct xmi_layer*) g_realloc(composition->layers, sizeof(struct xmi_layer)*(++composition->n_layers));
	xmi_copy_layer2(clipboard_layer,composition->layers+composition->n_layers-1);

	gtk_list_store_append(store, &iter);
	i = composition->n_layers-1;
	elementString = xmi_msim_gui_utils_get_layer_element_string(composition->layers + i);
	if (mb->matrixKind == COMPOSITION) {
		if (composition->n_layers == 1)
			composition->reference_layer = 1;

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

	g_free(elementString);
	update_undo_buffer(updateKind, (void*) store);

	//if there is one layer now -> activeate copy/cut
	GtkTreeModel *model = gtk_tree_view_get_model(gtk_tree_selection_get_tree_view(mb->select));
	if (gtk_tree_model_iter_n_children(model, NULL) > 0 && gtk_tree_selection_count_selected_rows(mb->select) == 0) {
		gtk_tree_model_get_iter_first(model, &iter);
		GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(model), &iter);
		gtk_tree_selection_select_path(mb->select, path);
		gtk_tree_path_free(path);
		gtk_widget_set_sensitive(GTK_WIDGET(cutT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(cutW), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyW), TRUE);
	}
}

static void layer_paste_cb(GtkWidget *button, struct matrix_button *mb) {
	GtkClipboard *clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
	if (clipboard)
		gtk_clipboard_request_contents(clipboard, LayerAtom, (GtkClipboardReceivedFunc) clipboard_receive_layer_cb, mb);
	return;
}

static void layer_copy_cb(GtkWidget *button, struct matrix_button *mb) {
	GtkClipboard *clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
	if (!clipboard)
		return;

	struct clipboard_data *cd = (struct clipboard_data *) g_malloc(sizeof(struct clipboard_data));

	GtkTreeModel *model;
	GtkTreeIter iter;
	struct xmi_composition *composition = NULL;

	if (!gtk_tree_selection_get_selected(mb->select, &model, &iter)) {
		g_fprintf(stderr, "Nothing selected in layer_copy_cb->this should not occur!\n");
		return;
	}
	if (mb->matrixKind == COMPOSITION)
		composition = compositionS;
	else if (mb->matrixKind == EXC_COMPOSITION)
		composition = exc_compositionS;
	else if (mb->matrixKind == DET_COMPOSITION)
		composition = det_compositionS;
	else if (mb->matrixKind == CRYSTAL_COMPOSITION)
		composition = crystal_compositionS;

	GtkTreePath *path = gtk_tree_model_get_path(model, &iter);
	gint *my_indices = gtk_tree_path_get_indices(path);
	struct xmi_layer *clipboard_layer;
	xmi_copy_layer(composition->layers+my_indices[0], &clipboard_layer);

	//xmi_print_layer(stdout, clipboard_layer, 1);

	//create data for clipboard
	cd->length = sizeof(int)+clipboard_layer->n_elements*(sizeof(double)+sizeof(int))+2*sizeof(double);
	guchar *data = (guchar *) g_malloc(cd->length);
	memcpy(data, &clipboard_layer->n_elements, sizeof(int));
	memcpy(data+sizeof(int), clipboard_layer->Z, sizeof(int)*clipboard_layer->n_elements);
	memcpy(data+sizeof(int)+sizeof(int)*clipboard_layer->n_elements, clipboard_layer->weight, sizeof(double)*clipboard_layer->n_elements);
	memcpy(data+sizeof(int)+sizeof(int)*clipboard_layer->n_elements+sizeof(double)*clipboard_layer->n_elements, &clipboard_layer->density, sizeof(double));
	memcpy(data+sizeof(int)+sizeof(int)*clipboard_layer->n_elements+sizeof(double)*clipboard_layer->n_elements+sizeof(double), &clipboard_layer->thickness, sizeof(double));
	cd->data = data;

	gtk_tree_path_free(path);
	xmi_free_layer(clipboard_layer);
	if (gtk_clipboard_set_with_data(clipboard, &LayerTE, 1, (GtkClipboardGetFunc) clipboard_get_layer_cb, (GtkClipboardClearFunc) clipboard_clear_layer_cb, (gpointer) cd)) {
		//g_fprintf(stdout, "Clipboard set\n");
		gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}
	else {
		g_fprintf(stderr, "Could not set clipboard!!!\n");
	}

}

static void layer_cut_cb(GtkWidget *button, struct matrix_button *mb) {
	//cut is basically copying followed by deleting
	GtkTreeIter iter;
	GtkTreeModel *model;

	if (!gtk_tree_selection_get_selected(mb->select, &model, &iter)) {
		g_fprintf(stderr, "Nothing selected in layer_cut_cb->this should not occur!\n");
		return;
	}

	//call my copy routine
	layer_copy_cb(button, mb);

	struct xmi_composition *composition = NULL;

	if (mb->matrixKind == COMPOSITION)
		composition = compositionS;
	else if (mb->matrixKind == EXC_COMPOSITION)
		composition = exc_compositionS;
	else if (mb->matrixKind == DET_COMPOSITION)
		composition = det_compositionS;
	else if (mb->matrixKind == CRYSTAL_COMPOSITION)
		composition = crystal_compositionS;


	GtkTreePath *path = gtk_tree_model_get_path(model, &iter);
	gint *my_indices = gtk_tree_path_get_indices(path);
	gint index = my_indices[0];
	gint nindices = gtk_tree_model_iter_n_children(model, NULL);

	//delete the selected line
	//watch out for reference layer... for now... leave it to the user
	//update composition
	xmi_free_layer(composition->layers+index);
	int i;
	if (nindices > 1) {
		for (i = index ;  i < nindices-1 ; i++)
			composition->layers[i] = composition->layers[i+1];
	}
	composition->layers = (struct xmi_layer*) g_realloc(composition->layers, sizeof(struct xmi_layer)*(nindices-1));
	composition->n_layers--;
	gtk_list_store_remove(mb->store,&iter);
	if (mb->matrixKind == COMPOSITION) {
		//reference layer may have to be updated
		if (nindices > 1) {
			if (composition->reference_layer == index+1) {
				//reference_layer was deleted -> only a problem if selected layer is the last one
				if (index == nindices -1) {
					composition->reference_layer--;
					//get iter to last element
					gchar *path_string = g_strdup_printf("%i",nindices-2);
					gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(mb->store), &iter, path_string);
					gtk_list_store_set(mb->store,&iter, REFERENCE_COLUMN, TRUE, -1);
					g_free(path_string);
				}
				else {
					gtk_list_store_set(mb->store,&iter, REFERENCE_COLUMN, TRUE, -1);
				}
			}
			else if (composition->reference_layer > index+1)
				composition->reference_layer--;
		}
		update_undo_buffer(COMPOSITION_CUT, (void*) mb->store);
	}
	else if (mb->matrixKind == EXC_COMPOSITION)
		update_undo_buffer(EXC_COMPOSITION_CUT, (void*) mb->store);
	else if (mb->matrixKind == DET_COMPOSITION)
		update_undo_buffer(DET_COMPOSITION_CUT, (void*) mb->store);
	else if (mb->matrixKind == CRYSTAL_COMPOSITION)
		update_undo_buffer(CRYSTAL_COMPOSITION_CUT, (void*) mb->store);

	//if no layers remain -> disable copy/cut
	if (gtk_tree_model_iter_n_children(model, NULL) == 0) {
		gtk_widget_set_sensitive(GTK_WIDGET(cutT), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyT), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(cutW), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyW), FALSE);
	}

}

static void create_popup_menu(GtkWidget *tree, GdkEventButton *event, struct matrix_button *mb) {
	GtkClipboard *clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
	if (!clipboard)
		return;

	GtkWidget *menu, *menuitem;

	//sensitivity should be determined by clipboard state and whether or not a layer was activated!
	//paste works always if clipboard is filled with goodies
	gboolean cut_and_copy_and_delete = FALSE;
	if (gtk_tree_selection_count_selected_rows(mb->select) == 1) {
		cut_and_copy_and_delete = TRUE;
	}


	menu = gtk_menu_new();
	menuitem = gtk_image_menu_item_new_from_stock(GTK_STOCK_CUT, NULL);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	if (cut_and_copy_and_delete) {
		g_signal_connect(menuitem, "activate", G_CALLBACK(layer_cut_cb), mb);
	}
	else {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}
	menuitem = gtk_image_menu_item_new_from_stock(GTK_STOCK_COPY, NULL);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	if (cut_and_copy_and_delete) {
		g_signal_connect(menuitem, "activate", G_CALLBACK(layer_copy_cb), mb);
	}
	else {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}
	menuitem = gtk_image_menu_item_new_from_stock(GTK_STOCK_PASTE, NULL);
	if (gtk_clipboard_wait_is_target_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD), LayerAtom)) {
		g_signal_connect(menuitem, "activate", G_CALLBACK(layer_paste_cb), mb);
	}
	else {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	menuitem = gtk_image_menu_item_new_from_stock(GTK_STOCK_DELETE, NULL);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	if (cut_and_copy_and_delete) {
		g_signal_connect(menuitem, "activate", G_CALLBACK(layer_right_click_menu_delete_cb), (gpointer) mb);
	}
	else {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}

	gtk_widget_show_all(menu);

	gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, event != NULL ? event->button : 0, gdk_event_get_time((GdkEvent *) event));
}

static gboolean layer_right_click_cb(GtkWidget *tree, GdkEventButton *event, struct matrix_button *mb) {
	if (event->type == GDK_BUTTON_PRESS && event->button == 3) {
		//count total number of rows
		//if clicked layer is not selected -> select it
		GtkTreeSelection *selection;
		selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
		GtkTreePath *path;
		if (gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(tree), (gint) event->x, (gint) event->y, &path, NULL, NULL, NULL) &&
		    !gtk_tree_selection_path_is_selected(selection, path)) {
			gtk_tree_selection_select_path(selection, path);
			gtk_tree_path_free(path);
		}
		create_popup_menu(tree, event, mb);
		return TRUE;
	}
	return FALSE;
}

static gboolean layer_popup_menu_cb(GtkWidget *tree, struct matrix_button *mb) {
	//call menu
	create_popup_menu(tree, NULL, mb);

	return TRUE;
}

static gboolean layer_focus_in_cb(GtkTreeView *tree, GdkEvent *event, gpointer data) {
	if (!gtk_clipboard_get(GDK_SELECTION_CLIPBOARD))
		return FALSE;

	//count number of children
	GtkTreeModel *model = gtk_tree_view_get_model(tree);
	int children = gtk_tree_model_iter_n_children(model, NULL);
	//g_fprintf(stdout, "children: %i\n", children);
	if (children > 0) {
		//activate cut and copy
		gtk_widget_set_sensitive(GTK_WIDGET(cutT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(cutW), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyW), TRUE);
	}
	if (gtk_clipboard_wait_is_target_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD), LayerAtom)) {
		gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}

	return FALSE;
}

static gboolean layer_focus_out_cb(GtkTreeView *tree, GdkEvent *event, gpointer data) {
	//deactivate cut and copy
	gtk_widget_set_sensitive(GTK_WIDGET(cutT), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(copyT), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(cutW), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(copyW), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pasteT), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pasteW), FALSE);
	return FALSE;
}

GtkWidget *initialize_matrix(struct xmi_composition *composition, int kind) {
	GtkListStore *store;
	GtkTreeIter iter;
	GtkWidget *tree;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	gchar *elementString;
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
	struct matrix_data *md;
	struct matrix_button *mb;
	struct reference_toggle *rt;
	struct matrix_reorder *mr;
	GtkWidget *scrolledWindow;

	int i;

	mainbox = gtk_hbox_new(FALSE, 5);
	mainbox2 = gtk_vbox_new(FALSE, 5);


	if (kind == COMPOSITION)
		store = gtk_list_store_new(NCOLUMNS_MATRIX, G_TYPE_INT, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_BOOLEAN);
	else
		store = gtk_list_store_new(NCOLUMNS_MATRIX-1, G_TYPE_INT, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE);



	for (i=0 ; i < composition->n_layers ; i++) {
		gtk_list_store_append(store, &iter);
		elementString = xmi_msim_gui_utils_get_layer_element_string(composition->layers + i);
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

		g_free(elementString);
	}

	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Number of elements", renderer,"text",N_ELEMENTS_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,FALSE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_set_expand(column, FALSE);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Elements", renderer,"text",ELEMENTS_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,FALSE);
	gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_column_set_expand(column, TRUE);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//need to come up with a way to get pango markup here...
	//column = gtk_tree_view_column_new_with_attributes("Density (g/cm3)", renderer,"text",DENSITY_COLUMN,NULL);
	//gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
	column = gtk_tree_view_column_new();
	GtkWidget *label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label),"Density (g/cm<sup>3</sup>)");
	//gtk_misc_set_alignment(GTK_MISC(label), 0.5, 0.2);
	//gtk_label_set_text(GTK_LABEL(label), "Density(g/cm3)");
	gtk_widget_show(label);
	gtk_tree_view_column_set_widget(column, label);
	gtk_tree_view_column_set_resizable(column,FALSE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, layer_print_double, GINT_TO_POINTER(DENSITY_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	g_object_set(G_OBJECT(renderer), "xalign", 0.5, NULL);
	//g_object_set(G_OBJECT(renderer), "yalign", 0.5, NULL);
	//column = gtk_tree_view_column_new_with_attributes("Thickness (cm)", renderer,"text",THICKNESS_COLUMN,NULL);
	//gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Thickness (cm)");
	gtk_tree_view_column_set_resizable(column,FALSE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_set_cell_data_func(column, renderer, layer_print_double, GINT_TO_POINTER(THICKNESS_COLUMN),NULL);


	if (kind == COMPOSITION) {
		rt = (struct reference_toggle *) g_malloc(sizeof(struct reference_toggle));
		rt->store = store;
		rt->model = (GtkTreeModel *) tree;
		renderer = gtk_cell_renderer_toggle_new();
		gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
		gtk_cell_renderer_toggle_set_radio(GTK_CELL_RENDERER_TOGGLE(renderer), TRUE);
		gtk_cell_renderer_toggle_set_activatable(GTK_CELL_RENDERER_TOGGLE(renderer), TRUE);
		g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(reference_layer_toggled_cb), rt);
		column = gtk_tree_view_column_new_with_attributes("Reference layer?", renderer,"active",REFERENCE_COLUMN,NULL);
		gtk_tree_view_column_set_resizable(column,FALSE);
		gtk_tree_view_column_set_alignment(column, 0.5);
		gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
		gtk_tree_view_column_set_expand(column, FALSE);
	}
	scrolledWindow = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	//gtk_widget_size_request(scrolledWindow,&size);
	//gtk_widget_set_size_request(scrolledWindow, 550,100);
	gtk_container_add(GTK_CONTAINER(scrolledWindow), tree);
	gtk_box_pack_start(GTK_BOX(mainbox),scrolledWindow, TRUE, TRUE,3 );

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
	mb = (struct matrix_button *) g_malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_TOP;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(topButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	//BOTTOM button
	mb = (struct matrix_button *) g_malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_BOTTOM;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(bottomButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	//UP button
	mb = (struct matrix_button *) g_malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_UP;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(upButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	//DOWN button
	mb = (struct matrix_button *) g_malloc(sizeof(struct matrix_button));
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
	mb = (struct matrix_button *) g_malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_DELETE;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(deleteButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	g_signal_connect(G_OBJECT(tree), "key-press-event", G_CALLBACK(layers_backspace_key_clicked), (gpointer) mb);
	//right click menu -> needs delete data
	g_signal_connect(G_OBJECT(tree), "button-press-event", G_CALLBACK(layer_right_click_cb), (gpointer) mb);
	g_signal_connect(G_OBJECT(tree), "popup-menu", G_CALLBACK(layer_popup_menu_cb), (gpointer) mb);

	//ADD
	mb = (struct matrix_button *) g_malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_ADD;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(addButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);

	//EDIT
	mb = (struct matrix_button *) g_malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_EDIT;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(editButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	g_signal_connect(G_OBJECT(tree), "row-activated", G_CALLBACK(matrix_row_activated_cb), (gpointer) mb);


	g_signal_connect(G_OBJECT(tree), "focus-in-event", G_CALLBACK(layer_focus_in_cb), NULL);
	g_signal_connect(G_OBJECT(tree), "focus-out-event", G_CALLBACK(layer_focus_out_cb), NULL);


	md = (struct matrix_data*) g_malloc(sizeof(struct matrix_data));
	md->topButton = topButton;
	md->upButton = upButton;
	md->downButton = downButton;
	md->bottomButton = bottomButton;
	md->editButton = editButton;
	md->addButton = addButton;
	md->deleteButton = deleteButton;

	gtk_tree_selection_set_mode(select,GTK_SELECTION_SINGLE);
	g_signal_connect(G_OBJECT(select), "changed", G_CALLBACK(layer_selection_changed_cb), (gpointer) md);

	mr = (struct matrix_reorder *) g_malloc(sizeof(struct matrix_reorder));
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
		xmi_copy_composition(composition,&compositionS);
		compositionL = store;
		compositionW = tree;
	}
	else if (kind == EXC_COMPOSITION) {
		xmi_copy_composition(composition,&exc_compositionS);
		exc_compositionL = store;
		exc_compositionW = tree;
	}
	else if (kind == DET_COMPOSITION) {
		xmi_copy_composition(composition,&det_compositionS);
		det_compositionL = store;
		det_compositionW = tree;
	}
	else if (kind == CRYSTAL_COMPOSITION) {
		xmi_copy_composition(composition,&crystal_compositionS);
		crystal_compositionL = store;
		crystal_compositionW = tree;
	}
	return mainbox2;
}



void undo_menu_fix_after_error(void) {
	char *buffer;
	//update undo
	if (current != redo_buffer) {
		buffer = g_strdup_printf("Undo: %s",(current)->message);
		gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);
		gtk_tool_item_set_tooltip_text(undoT,buffer);
	}
	else {
		buffer = g_strdup_printf("Undo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);
		gtk_tool_item_set_tooltip_text(undoT,buffer);
		gtk_widget_set_sensitive(undoW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
	}

	//update redo
	if (current == last) {
		buffer = g_strdup_printf("Redo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);
		gtk_tool_item_set_tooltip_text(redoT,buffer);
		gtk_widget_set_sensitive(redoW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	}
	else {
		buffer = g_strdup_printf("Redo: %s", (current+1)->message);
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);
		gtk_tool_item_set_tooltip_text(redoT,buffer);
		gtk_widget_set_sensitive(redoW,TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);
	}

	gtk_widget_modify_base(GTK_WIDGET(undo_error->widget), GTK_STATE_NORMAL, NULL);
	adjust_control_widgets(GTK_WIDGET(undo_error->widget), TRUE);
	g_free(undo_error->message);
	g_free(undo_error);
	undo_error = NULL;

	adjust_save_buttons();

}

void undo_menu_click_with_error(void) {
	char *buffer;

	*(undo_error->check) = 1;

	switch (undo_error->kind) {
		case N_PHOTONS_INTERVAL:
			buffer = g_strdup_printf("%li",(current)->xi->general->n_photons_interval);
#if DEBUG == 1
			fprintf(stdout,"n_photons_interval undo to %s\n",buffer);
#endif
			g_signal_handler_block(G_OBJECT(undo_error->widget), n_photons_intervalG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), n_photons_intervalG);
			break;
		case N_PHOTONS_LINE:
			buffer = g_strdup_printf("%li",(current)->xi->general->n_photons_line);
			g_signal_handler_block(G_OBJECT(undo_error->widget), n_photons_lineG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), n_photons_lineG);
			break;
		case N_INTERACTIONS_TRAJECTORY:
			buffer = g_strdup_printf("%i",(current)->xi->general->n_interactions_trajectory);
			g_signal_handler_block(G_OBJECT(undo_error->widget), n_interactions_trajectoryG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), n_interactions_trajectoryG);
			break;
		case D_SAMPLE_SOURCE:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->d_sample_source);
			g_signal_handler_block(G_OBJECT(undo_error->widget), d_sample_sourceG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), d_sample_sourceG);
			break;
		case N_SAMPLE_ORIENTATION_X:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->n_sample_orientation[0]);
			g_signal_handler_block(G_OBJECT(undo_error->widget), n_sample_orientation_xG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), n_sample_orientation_xG);
			break;
		case N_SAMPLE_ORIENTATION_Y:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->n_sample_orientation[1]);
			g_signal_handler_block(G_OBJECT(undo_error->widget), n_sample_orientation_yG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), n_sample_orientation_yG);
			break;
		case N_SAMPLE_ORIENTATION_Z:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->n_sample_orientation[2]);
			g_signal_handler_block(G_OBJECT(undo_error->widget), n_sample_orientation_zG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), n_sample_orientation_zG);
			break;
		case P_DETECTOR_WINDOW_X:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->p_detector_window[0]);
			g_signal_handler_block(G_OBJECT(undo_error->widget), p_detector_window_xG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), p_detector_window_xG);
			break;
		case P_DETECTOR_WINDOW_Y:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->p_detector_window[1]);
			g_signal_handler_block(G_OBJECT(undo_error->widget), p_detector_window_yG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), p_detector_window_yG);
			break;
		case P_DETECTOR_WINDOW_Z:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->p_detector_window[2]);
			g_signal_handler_block(G_OBJECT(undo_error->widget), p_detector_window_zG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), p_detector_window_zG);
			break;
		case N_DETECTOR_ORIENTATION_X:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->n_detector_orientation[0]);
			g_signal_handler_block(G_OBJECT(undo_error->widget), n_detector_orientation_xG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), n_detector_orientation_xG);
			break;
		case N_DETECTOR_ORIENTATION_Y:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->n_detector_orientation[1]);
			g_signal_handler_block(G_OBJECT(undo_error->widget), n_detector_orientation_yG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), n_detector_orientation_yG);
			break;
		case N_DETECTOR_ORIENTATION_Z:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->n_detector_orientation[2]);
			g_signal_handler_block(G_OBJECT(undo_error->widget), n_detector_orientation_zG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), n_detector_orientation_zG);
			break;
		case AREA_DETECTOR:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->area_detector);
			g_signal_handler_block(G_OBJECT(undo_error->widget), area_detectorG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), area_detectorG);
			break;
		case COLLIMATOR_HEIGHT:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->collimator_height);
			g_signal_handler_block(G_OBJECT(undo_error->widget), collimator_heightG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), collimator_heightG);
			break;
		case COLLIMATOR_DIAMETER:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->collimator_diameter);
			g_signal_handler_block(G_OBJECT(undo_error->widget), collimator_diameterG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), collimator_diameterG);
			break;
		case D_SOURCE_SLIT:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->d_source_slit);
			g_signal_handler_block(G_OBJECT(undo_error->widget), d_source_slitG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), d_source_slitG);
			break;
		case SLIT_SIZE_X:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->slit_size_x);
			g_signal_handler_block(G_OBJECT(undo_error->widget), slit_size_xG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), slit_size_xG);
			break;
		case SLIT_SIZE_Y:
			buffer = g_strdup_printf("%lg",(current)->xi->geometry->slit_size_y);
			g_signal_handler_block(G_OBJECT(undo_error->widget), slit_size_yG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), slit_size_yG);
			break;
		case DETECTOR_GAIN:
			buffer = g_strdup_printf("%lg",(current)->xi->detector->gain);
			g_signal_handler_block(G_OBJECT(undo_error->widget), detector_gainG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), detector_gainG);
			break;
		case DETECTOR_NCHANNELS:
			//this is actually not necessary since it's impossible to introduce invalid values
			g_signal_handler_block(G_OBJECT(undo_error->widget), detector_nchannelsG);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(undo_error->widget), (double) (current)->xi->detector->nchannels);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), detector_nchannelsG);
			break;
		case DETECTOR_LIVE_TIME:
			buffer = g_strdup_printf("%lg",(current)->xi->detector->live_time);
			g_signal_handler_block(G_OBJECT(undo_error->widget), detector_live_timeG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), detector_live_timeG);
			break;
		case DETECTOR_PULSE_WIDTH:
			buffer = g_strdup_printf("%lg",(current)->xi->detector->pulse_width);
			g_signal_handler_block(G_OBJECT(undo_error->widget), detector_pulse_widthG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), detector_pulse_widthG);
			break;
		case DETECTOR_ZERO:
			buffer = g_strdup_printf("%lg",(current)->xi->detector->zero);
			g_signal_handler_block(G_OBJECT(undo_error->widget), detector_zeroG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), detector_zeroG);
			break;
		case DETECTOR_NOISE:
			buffer = g_strdup_printf("%lg",(current)->xi->detector->noise);
			g_signal_handler_block(G_OBJECT(undo_error->widget), detector_noiseG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), detector_noiseG);
			break;
		case DETECTOR_FANO:
			buffer = g_strdup_printf("%lg",(current)->xi->detector->fano);
			g_signal_handler_block(G_OBJECT(undo_error->widget), detector_fanoG);
			gtk_entry_set_text(GTK_ENTRY(undo_error->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(undo_error->widget), detector_fanoG);
			break;
	}

	undo_menu_fix_after_error();
}

static void undo_menu_click(GtkWidget *widget, gpointer data) {
	char *buffer;
	GtkListStore *store;
	GtkTreeIter iter;
	char *elementString;
	int i;
	int undo_rv;

	//restore previous state
	//current-- && copy changes
#if DEBUG == 1
	fprintf(stdout,"Undo button clicked\n");
#endif

	//behavior changes when we're in error restoration mode
	if (undo_error) {
		undo_menu_click_with_error();
		return;
	}



	switch (current->kind) {
		case OUTPUTFILE:
			gtk_entry_set_text(GTK_ENTRY((current)->widget),(current-1)->xi->general->outputfile);
			break;
		case N_PHOTONS_INTERVAL:
			buffer = g_strdup_printf("%li",(current-1)->xi->general->n_photons_interval);
#if DEBUG == 1
			fprintf(stdout,"n_photons_interval undo to %s\n",buffer);
#endif
			g_signal_handler_block(G_OBJECT(current->widget), n_photons_intervalG);
			gtk_entry_set_text(GTK_ENTRY(current->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(current->widget), n_photons_intervalG);
			break;
		case N_PHOTONS_LINE:
			buffer = g_strdup_printf("%li",(current-1)->xi->general->n_photons_line);
			g_signal_handler_block(G_OBJECT(current->widget), n_photons_lineG);
			gtk_entry_set_text(GTK_ENTRY(current->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(current->widget), n_photons_lineG);
			break;
		case N_INTERACTIONS_TRAJECTORY:
			buffer = g_strdup_printf("%i",(current-1)->xi->general->n_interactions_trajectory);
			g_signal_handler_block(G_OBJECT(current->widget), n_interactions_trajectoryG);
			gtk_entry_set_text(GTK_ENTRY(current->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(current->widget), n_interactions_trajectoryG);
			break;
		case COMPOSITION_ORDER:
		case COMPOSITION_REFERENCE:
		case COMPOSITION_DELETE:
		case COMPOSITION_ADD:
		case COMPOSITION_EDIT:
		case COMPOSITION_PASTE:
		case COMPOSITION_CUT:
			//clear list and repopulate
			store = (GtkListStore *) (current)->widget;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current-1)->xi->composition->n_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = xmi_msim_gui_utils_get_layer_element_string((current-1)->xi->composition->layers + i);
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current-1)->xi->composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current-1)->xi->composition->layers[i].density,
					THICKNESS_COLUMN,(current-1)->xi->composition->layers[i].thickness,
					REFERENCE_COLUMN,(i+1 == (current-1)->xi->composition->reference_layer) ? TRUE : FALSE,
					-1
					);
				g_free(elementString);
			}
			xmi_free_composition(compositionS);
			xmi_copy_composition((current-1)->xi->composition, &compositionS);
			break;
		case D_SAMPLE_SOURCE:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->d_sample_source);
			g_signal_handler_block(G_OBJECT((current)->widget), d_sample_sourceG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), d_sample_sourceG);
			break;
		case N_SAMPLE_ORIENTATION_X:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->n_sample_orientation[0]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_sample_orientation_xG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_sample_orientation_xG);
			break;
		case N_SAMPLE_ORIENTATION_Y:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->n_sample_orientation[1]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_sample_orientation_yG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_sample_orientation_yG);
			break;
		case N_SAMPLE_ORIENTATION_Z:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->n_sample_orientation[2]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_sample_orientation_zG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_sample_orientation_zG);
			break;
		case P_DETECTOR_WINDOW_X:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->p_detector_window[0]);
			g_signal_handler_block(G_OBJECT((current)->widget), p_detector_window_xG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), p_detector_window_xG);
			break;
		case P_DETECTOR_WINDOW_Y:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->p_detector_window[1]);
			g_signal_handler_block(G_OBJECT((current)->widget), p_detector_window_yG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), p_detector_window_yG);
			break;
		case P_DETECTOR_WINDOW_Z:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->p_detector_window[2]);
			g_signal_handler_block(G_OBJECT((current)->widget), p_detector_window_zG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), p_detector_window_zG);
			break;
		case N_DETECTOR_ORIENTATION_X:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->n_detector_orientation[0]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_detector_orientation_xG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_detector_orientation_xG);
			break;
		case N_DETECTOR_ORIENTATION_Y:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->n_detector_orientation[1]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_detector_orientation_yG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_detector_orientation_yG);
			break;
		case N_DETECTOR_ORIENTATION_Z:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->n_detector_orientation[2]);
			g_signal_handler_block(G_OBJECT((current)->widget), n_detector_orientation_zG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), n_detector_orientation_zG);
			break;
		case AREA_DETECTOR:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->area_detector);
			g_signal_handler_block(G_OBJECT((current)->widget), area_detectorG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), area_detectorG);
			break;
		case COLLIMATOR_HEIGHT:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->collimator_height);
			g_signal_handler_block(G_OBJECT((current)->widget), collimator_heightG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), collimator_heightG);
			break;
		case COLLIMATOR_DIAMETER:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->collimator_diameter);
			g_signal_handler_block(G_OBJECT((current)->widget), collimator_diameterG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), collimator_diameterG);
			break;
		case D_SOURCE_SLIT:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->d_source_slit);
			g_signal_handler_block(G_OBJECT((current)->widget), d_source_slitG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), d_source_slitG);
			break;
		case SLIT_SIZE_X:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->slit_size_x);
			g_signal_handler_block(G_OBJECT((current)->widget), slit_size_xG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), slit_size_xG);
			break;
		case SLIT_SIZE_Y:
			buffer = g_strdup_printf("%lg",(current-1)->xi->geometry->slit_size_y);
			g_signal_handler_block(G_OBJECT((current)->widget), slit_size_yG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), slit_size_yG);
			break;
		case DISCRETE_ENERGY_ADD:
		case DISCRETE_ENERGY_IMPORT_ADD:
		case DISCRETE_ENERGY_IMPORT_REPLACE:
		case DISCRETE_ENERGY_CLEAR:
		case DISCRETE_ENERGY_EDIT:
		case DISCRETE_ENERGY_DELETE:
		case DISCRETE_ENERGY_SCALE:
			repopulate_discrete_energies(discWidget->store, (current-1)->xi->excitation);
			break;
		case SOURCE_SPECTRUM_ADD:
		case SOURCE_SPECTRUM_REPLACE:
			repopulate_discrete_energies(discWidget->store, (current-1)->xi->excitation);
		case CONTINUOUS_ENERGY_ADD:
		case CONTINUOUS_ENERGY_IMPORT_ADD:
		case CONTINUOUS_ENERGY_IMPORT_REPLACE:
		case CONTINUOUS_ENERGY_CLEAR:
		case CONTINUOUS_ENERGY_EDIT:
		case CONTINUOUS_ENERGY_DELETE:
		case CONTINUOUS_ENERGY_SCALE:
			repopulate_continuous_energies(contWidget->store, (current-1)->xi->excitation);
			break;
		case EXC_COMPOSITION_ORDER:
		case EXC_COMPOSITION_DELETE:
		case EXC_COMPOSITION_ADD:
		case EXC_COMPOSITION_EDIT:
		case EXC_COMPOSITION_PASTE:
		case EXC_COMPOSITION_CUT:
			//clear list and repopulate
			store = exc_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current-1)->xi->absorbers->n_exc_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = xmi_msim_gui_utils_get_layer_element_string((current-1)->xi->absorbers->exc_layers + i);
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current-1)->xi->absorbers->exc_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current-1)->xi->absorbers->exc_layers[i].density,
					THICKNESS_COLUMN,(current-1)->xi->absorbers->exc_layers[i].thickness,
					-1
					);
				g_free(elementString);
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
		case DET_COMPOSITION_PASTE:
		case DET_COMPOSITION_CUT:
			//clear list and repopulate
			store = det_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current-1)->xi->absorbers->n_det_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = xmi_msim_gui_utils_get_layer_element_string((current-1)->xi->absorbers->det_layers + i);
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current-1)->xi->absorbers->det_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current-1)->xi->absorbers->det_layers[i].density,
					THICKNESS_COLUMN,(current-1)->xi->absorbers->det_layers[i].thickness,
					-1
					);
				g_free(elementString);
			}
			xmi_free_composition(det_compositionS);
			xmi_copy_abs_or_crystal2composition((current-1)->xi->absorbers->det_layers,(current-1)->xi->absorbers->n_det_layers , &det_compositionS);
			break;
		case CRYSTAL_COMPOSITION_ORDER:
		case CRYSTAL_COMPOSITION_DELETE:
		case CRYSTAL_COMPOSITION_ADD:
		case CRYSTAL_COMPOSITION_EDIT:
		case CRYSTAL_COMPOSITION_PASTE:
		case CRYSTAL_COMPOSITION_CUT:
			//clear list and repopulate
			store = crystal_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current-1)->xi->detector->n_crystal_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = xmi_msim_gui_utils_get_layer_element_string((current-1)->xi->detector->crystal_layers + i);
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current-1)->xi->detector->crystal_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current-1)->xi->detector->crystal_layers[i].density,
					THICKNESS_COLUMN,(current-1)->xi->detector->crystal_layers[i].thickness,
					-1
					);
				g_free(elementString);
			}
#if DEBUG == 1

#endif
			xmi_free_composition(crystal_compositionS);
			xmi_copy_abs_or_crystal2composition((current-1)->xi->detector->crystal_layers,(current-1)->xi->detector->n_crystal_layers , &crystal_compositionS);
			break;
		case DETECTOR_TYPE:
			//buffer = g_strdup_printf("%lg",(current-1)->xi->detector->detector_type);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_typeG);
			//gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			gtk_combo_box_set_active(GTK_COMBO_BOX((current)->widget),(current-1)->xi->detector->detector_type);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_typeG);
			break;
		case DETECTOR_GAIN:
			buffer = g_strdup_printf("%lg",(current-1)->xi->detector->gain);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_gainG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_gainG);
			break;
		case DETECTOR_NCHANNELS:
			g_signal_handler_block(G_OBJECT((current)->widget), detector_nchannelsG);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON((current)->widget), (double) (current-1)->xi->detector->nchannels);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_nchannelsG);
			break;
		case DETECTOR_LIVE_TIME:
			buffer = g_strdup_printf("%lg",(current-1)->xi->detector->live_time);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_live_timeG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_live_timeG);
			break;
		case DETECTOR_PULSE_WIDTH:
			buffer = g_strdup_printf("%lg",(current-1)->xi->detector->pulse_width);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_pulse_widthG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_pulse_widthG);
			break;
		case DETECTOR_ZERO:
			buffer = g_strdup_printf("%lg",(current-1)->xi->detector->zero);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_zeroG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_zeroG);
			break;
		case DETECTOR_NOISE:
			buffer = g_strdup_printf("%lg",(current-1)->xi->detector->noise);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_noiseG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_noiseG);
			break;
		case DETECTOR_FANO:
			buffer = g_strdup_printf("%lg",(current-1)->xi->detector->fano);
			g_signal_handler_block(G_OBJECT((current)->widget), detector_fanoG);
			gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current)->widget), detector_fanoG);
			break;
		case IMPORT_FROM_FILE:
			undo_rv = GPOINTER_TO_INT(current->widget);
			if (undo_rv & IMPORT_SELECT_COMPOSITION) {
				change_all_values_composition((current-1)->xi);
			}
			if (undo_rv & IMPORT_SELECT_GEOMETRY) {
				change_all_values_geometry((current-1)->xi);
			}
			if (undo_rv & IMPORT_SELECT_EXCITATION) {
				change_all_values_excitation((current-1)->xi);
			}
			if (undo_rv & IMPORT_SELECT_DETECTORSETTINGS) {
				change_all_values_detectorsettings((current-1)->xi);
			}
			if (undo_rv & IMPORT_SELECT_BEAMABSORBERS) {
				change_all_values_beamabsorbers((current-1)->xi);
			}
			if (undo_rv & IMPORT_SELECT_DETECTIONABSORBERS) {
				change_all_values_detectionabsorbers((current-1)->xi);
			}
			break;
	}
	if (current-1 != redo_buffer) {
		buffer = g_strdup_printf("Undo: %s",(current-1)->message);
		gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);
		gtk_tool_item_set_tooltip_text(undoT,buffer);
	}
	else {
		buffer = g_strdup_printf("Undo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);
		gtk_tool_item_set_tooltip_text(undoT,buffer);
		gtk_widget_set_sensitive(undoW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
	}
	//update redo
	buffer = g_strdup_printf("Redo: %s",current->message);
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

	//adjust cut/copy
	GtkWidget *toplevel = gtk_widget_get_toplevel(widget);
	GtkWidget *focused = gtk_window_get_focus(GTK_WINDOW(toplevel));
	if (focused == compositionW ||
	    focused == exc_compositionW ||
	    focused == det_compositionW ||
	    focused == crystal_compositionW) {
		//let's assume it will remain focused
		//so if the store is now empty -> disable cut/copy
		//if instead the store is no longer empty -> enable cut/copy and make sure something is selected
		GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(focused));
		if (gtk_tree_model_iter_n_children(model, NULL) == 0) {
			gtk_widget_set_sensitive(GTK_WIDGET(cutT), FALSE);
			gtk_widget_set_sensitive(GTK_WIDGET(copyT), FALSE);
			gtk_widget_set_sensitive(GTK_WIDGET(cutW), FALSE);
			gtk_widget_set_sensitive(GTK_WIDGET(copyW), FALSE);
		}
		else if (gtk_tree_selection_count_selected_rows(gtk_tree_view_get_selection(GTK_TREE_VIEW(focused))) == 0) {
			gtk_tree_model_get_iter_first(model, &iter);
			GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(model), &iter);
			gtk_tree_selection_select_path(gtk_tree_view_get_selection(GTK_TREE_VIEW(focused)), path);
			gtk_tree_path_free(path);
			gtk_widget_set_sensitive(GTK_WIDGET(cutT), TRUE);
			gtk_widget_set_sensitive(GTK_WIDGET(copyT), TRUE);
			gtk_widget_set_sensitive(GTK_WIDGET(cutW), TRUE);
			gtk_widget_set_sensitive(GTK_WIDGET(copyW), TRUE);
		}

	}
}

static void about_activate_link(GtkAboutDialog *about, const gchar *url, gpointer data) {
	if (strncmp(url, "https", 5) == 0) {
		xmi_msim_gui_utils_open_url(url);
	}
	else {
		xmi_open_email(url);
	}
	return;
}

static void url_click(GtkWidget *widget, const char *url) {
	xmi_msim_gui_utils_open_url(url);
}

static void email_click(GtkWidget *widget, const char *url) {
	xmi_open_email(url);
}


static void about_click(GtkWidget *widget, gpointer data) {
	static const gchar *authors[] = {
		"Tom Schoonjans <Tom.Schoonjans@me.com>",
		"Laszlo Vincze <Laszlo.Vincze@UGent.be>",
		"Vicente Armando Sol\303\251 <sole@esrf.fr>",
		"Philip Brondeel <Philip.Brondeel@UGent.be>",
		"Manuel Sanchez del Rio <srio@esrf.fr>",
		"Claudio Ferrero <ferrero@esrf.fr>",
		NULL
	};

	static const gchar *artists[] = {
		"Jan Garrevoet <Jan.Garrevoet@UGent.be>",
		NULL
	};

	static const gchar copyright[] = "Copyright \xc2\xa9 2010-2016 Tom Schoonjans, Philip Brondeel and Laszlo Vincze";

	static const gchar comments[] = "A tool for the Monte Carlo simulation of ED-XRF spectrometers\n\n\nPlease read carefully the License section and the links therein.";

	GdkPixbuf *logo = NULL;

	GtkWidget *about_dialog = gtk_about_dialog_new();
	gtk_window_set_transient_for(GTK_WINDOW(about_dialog), GTK_WINDOW(data));
	gtk_window_set_modal(GTK_WINDOW(about_dialog), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(about_dialog), TRUE);
	gtk_about_dialog_set_program_name(GTK_ABOUT_DIALOG(about_dialog), "XMI-MSIM");
	gtk_about_dialog_set_authors(GTK_ABOUT_DIALOG(about_dialog), authors);
	gtk_about_dialog_set_comments(GTK_ABOUT_DIALOG(about_dialog), comments);
	gtk_about_dialog_set_copyright(GTK_ABOUT_DIALOG(about_dialog), copyright);
	gtk_about_dialog_set_license(GTK_ABOUT_DIALOG(about_dialog), "This program comes with ABSOLUTELY NO WARRANTY. It is made available under the terms and conditions specified by version 3 of the GNU General Public License. For details, visit http://www.gnu.org/licenses/gpl.html\n\nPlease refer to our paper \"A general Monte Carlo simulation of energy-dispersive X-ray fluorescence spectrometers - Part 5. Polarized radiation, stratified samples, cascade effects, M-lines\" (http://dx.doi.org/10.1016/j.sab.2012.03.011 ) in your manuscripts when using this tool.\n\nWhen using XMI-MSIM through the PyMca quantification interface, please refer to our paper \"A general Monte Carlo simulation of energy-dispersive X-ray fluorescence spectrometers - Part 6. Quantification through iterative simulations\" (http://dx.doi.org/10.1016/j.sab.2012.12.011 ) in your manuscripts.");
	gtk_about_dialog_set_logo_icon_name(GTK_ABOUT_DIALOG(about_dialog), XMI_STOCK_LOGO);
	gtk_about_dialog_set_artists(GTK_ABOUT_DIALOG(about_dialog), artists);
	gtk_about_dialog_set_version(GTK_ABOUT_DIALOG(about_dialog), VERSION);
	gtk_about_dialog_set_website(GTK_ABOUT_DIALOG(about_dialog), "https://github.com/tschoonj/xmimsim");
	gtk_about_dialog_set_website_label(GTK_ABOUT_DIALOG(about_dialog), "https://github.com/tschoonj/xmimsim");
	gtk_about_dialog_set_wrap_license(GTK_ABOUT_DIALOG(about_dialog), TRUE);
	g_signal_connect(about_dialog, "activate-link", G_CALLBACK(about_activate_link), NULL);


	gtk_dialog_run(GTK_DIALOG(about_dialog));
	gtk_widget_destroy(about_dialog);


	if (logo)
		g_object_unref(logo);

}

static void redo_menu_click(GtkWidget *widget, gpointer data) {
	char *buffer;
	GtkListStore *store;
	GtkTreeIter iter;
	char *elementString;
	int i;
	int undo_rv;

#if DEBUG == 1
	fprintf(stdout,"Redo button clicked: current kind: %i\n",current->kind);
#endif
	switch ((current+1)->kind) {
		case OUTPUTFILE:
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),(current+1)->xi->general->outputfile);
			break;
		case N_PHOTONS_INTERVAL:
			buffer = g_strdup_printf("%li",(current+1)->xi->general->n_photons_interval);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_photons_intervalG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_photons_intervalG);
			break;
		case N_PHOTONS_LINE:
			buffer = g_strdup_printf("%li",(current+1)->xi->general->n_photons_line);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_photons_lineG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_photons_lineG);
			break;
		case N_INTERACTIONS_TRAJECTORY:
			buffer = g_strdup_printf("%i",(current+1)->xi->general->n_interactions_trajectory);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_interactions_trajectoryG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_interactions_trajectoryG);
			break;
		case COMPOSITION_ORDER:
		case COMPOSITION_REFERENCE:
		case COMPOSITION_DELETE:
		case COMPOSITION_ADD:
		case COMPOSITION_EDIT:
		case COMPOSITION_PASTE:
		case COMPOSITION_CUT:
			//clear list and repopulate
			store = (GtkListStore *) (current+1)->widget;
			gtk_list_store_clear(store);
			for (i=0 ; i < (current+1)->xi->composition->n_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = xmi_msim_gui_utils_get_layer_element_string((current+1)->xi->composition->layers + i);
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current+1)->xi->composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current+1)->xi->composition->layers[i].density,
					THICKNESS_COLUMN,(current+1)->xi->composition->layers[i].thickness,
					REFERENCE_COLUMN,(i+1 == (current+1)->xi->composition->reference_layer) ? TRUE : FALSE,
					-1
					);
				g_free(elementString);
			}
			xmi_free_composition(compositionS);
			xmi_copy_composition((current+1)->xi->composition, &compositionS);
			break;
		case D_SAMPLE_SOURCE:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->d_sample_source);
			g_signal_handler_block(G_OBJECT((current+1)->widget), d_sample_sourceG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), d_sample_sourceG);
			break;
		case N_SAMPLE_ORIENTATION_X:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->n_sample_orientation[0]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_sample_orientation_xG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_sample_orientation_xG);
			break;
		case N_SAMPLE_ORIENTATION_Y:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->n_sample_orientation[1]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_sample_orientation_yG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_sample_orientation_yG);
			break;
		case N_SAMPLE_ORIENTATION_Z:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->n_sample_orientation[2]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_sample_orientation_zG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_sample_orientation_zG);
			break;
		case P_DETECTOR_WINDOW_X:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->p_detector_window[0]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), p_detector_window_xG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), p_detector_window_xG);
			break;
		case P_DETECTOR_WINDOW_Y:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->p_detector_window[1]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), p_detector_window_yG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), p_detector_window_yG);
			break;
		case P_DETECTOR_WINDOW_Z:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->p_detector_window[2]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), p_detector_window_zG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), p_detector_window_zG);
			break;
		case N_DETECTOR_ORIENTATION_X:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->n_detector_orientation[0]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_detector_orientation_xG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_detector_orientation_xG);
			break;
		case N_DETECTOR_ORIENTATION_Y:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->n_detector_orientation[1]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_detector_orientation_yG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_detector_orientation_yG);
			break;
		case N_DETECTOR_ORIENTATION_Z:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->n_detector_orientation[2]);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_detector_orientation_zG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_detector_orientation_zG);
			break;
		case AREA_DETECTOR:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->area_detector);
			g_signal_handler_block(G_OBJECT((current+1)->widget), area_detectorG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), area_detectorG);
			break;
		case COLLIMATOR_HEIGHT:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->collimator_height);
			g_signal_handler_block(G_OBJECT((current+1)->widget), collimator_heightG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), collimator_heightG);
			break;
		case COLLIMATOR_DIAMETER:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->collimator_diameter);
			g_signal_handler_block(G_OBJECT((current+1)->widget), collimator_diameterG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), collimator_diameterG);
			break;
		case D_SOURCE_SLIT:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->d_source_slit);
			g_signal_handler_block(G_OBJECT((current+1)->widget), d_source_slitG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), d_source_slitG);
			break;
		case SLIT_SIZE_X:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->slit_size_x);
			g_signal_handler_block(G_OBJECT((current+1)->widget), slit_size_xG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), slit_size_xG);
			break;
		case SLIT_SIZE_Y:
			buffer = g_strdup_printf("%lg",(current+1)->xi->geometry->slit_size_y);
			g_signal_handler_block(G_OBJECT((current+1)->widget), slit_size_yG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), slit_size_yG);
			break;
		case DISCRETE_ENERGY_ADD:
		case DISCRETE_ENERGY_IMPORT_ADD:
		case DISCRETE_ENERGY_IMPORT_REPLACE:
		case DISCRETE_ENERGY_CLEAR:
		case DISCRETE_ENERGY_EDIT:
		case DISCRETE_ENERGY_DELETE:
		case DISCRETE_ENERGY_SCALE:
			repopulate_discrete_energies(discWidget->store, (current+1)->xi->excitation);
			break;
		case SOURCE_SPECTRUM_ADD:
		case SOURCE_SPECTRUM_REPLACE:
			repopulate_discrete_energies(discWidget->store, (current+1)->xi->excitation);
			repopulate_continuous_energies(contWidget->store, (current+1)->xi->excitation);
			break;
		case CONTINUOUS_ENERGY_ADD:
		case CONTINUOUS_ENERGY_IMPORT_ADD:
		case CONTINUOUS_ENERGY_IMPORT_REPLACE:
		case CONTINUOUS_ENERGY_CLEAR:
		case CONTINUOUS_ENERGY_EDIT:
		case CONTINUOUS_ENERGY_DELETE:
		case CONTINUOUS_ENERGY_SCALE:
			repopulate_continuous_energies(contWidget->store, (current+1)->xi->excitation);
			break;
		case EXC_COMPOSITION_ORDER:
		case EXC_COMPOSITION_DELETE:
		case EXC_COMPOSITION_ADD:
		case EXC_COMPOSITION_EDIT:
		case EXC_COMPOSITION_PASTE:
		case EXC_COMPOSITION_CUT:
			//clear list and repopulate
			store = exc_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current+1)->xi->absorbers->n_exc_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = xmi_msim_gui_utils_get_layer_element_string((current+1)->xi->absorbers->exc_layers + i);
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current+1)->xi->absorbers->exc_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current+1)->xi->absorbers->exc_layers[i].density,
					THICKNESS_COLUMN,(current+1)->xi->absorbers->exc_layers[i].thickness,
					-1
					);
				g_free(elementString);
			}
			xmi_free_composition(exc_compositionS);
			xmi_copy_abs_or_crystal2composition((current+1)->xi->absorbers->exc_layers,(current+1)->xi->absorbers->n_exc_layers , &exc_compositionS);
			break;
		case DET_COMPOSITION_ORDER:
		case DET_COMPOSITION_DELETE:
		case DET_COMPOSITION_ADD:
		case DET_COMPOSITION_EDIT:
		case DET_COMPOSITION_PASTE:
		case DET_COMPOSITION_CUT:
			//clear list and repopulate
			store = det_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current+1)->xi->absorbers->n_det_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = xmi_msim_gui_utils_get_layer_element_string((current+1)->xi->absorbers->det_layers + i);
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current+1)->xi->absorbers->det_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current+1)->xi->absorbers->det_layers[i].density,
					THICKNESS_COLUMN,(current+1)->xi->absorbers->det_layers[i].thickness,
					-1
					);
				g_free(elementString);
			}
			xmi_free_composition(det_compositionS);
			xmi_copy_abs_or_crystal2composition((current+1)->xi->absorbers->det_layers,(current+1)->xi->absorbers->n_det_layers , &det_compositionS);
			break;
		case CRYSTAL_COMPOSITION_ORDER:
		case CRYSTAL_COMPOSITION_DELETE:
		case CRYSTAL_COMPOSITION_ADD:
		case CRYSTAL_COMPOSITION_EDIT:
		case CRYSTAL_COMPOSITION_PASTE:
		case CRYSTAL_COMPOSITION_CUT:
			//clear list and repopulate
			store = crystal_compositionL;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current+1)->xi->detector->n_crystal_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = xmi_msim_gui_utils_get_layer_element_string((current+1)->xi->detector->crystal_layers + i);
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current+1)->xi->detector->crystal_layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current+1)->xi->detector->crystal_layers[i].density,
					THICKNESS_COLUMN,(current+1)->xi->detector->crystal_layers[i].thickness,
					-1
					);
				g_free(elementString);
			}
			xmi_free_composition(crystal_compositionS);
			xmi_copy_abs_or_crystal2composition((current+1)->xi->detector->crystal_layers,(current+1)->xi->detector->n_crystal_layers , &crystal_compositionS);
			break;
		case DETECTOR_TYPE:
			//buffer = g_strdup_printf("%lg",(current-1)->xi->detector->detector_type);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_typeG);
			//gtk_entry_set_text(GTK_ENTRY((current)->widget),buffer);
			gtk_combo_box_set_active(GTK_COMBO_BOX((current+1)->widget),(current+1)->xi->detector->detector_type);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_typeG);
			break;
		case DETECTOR_NCHANNELS:
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_nchannelsG);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON((current+1)->widget), (double) (current+1)->xi->detector->nchannels);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_nchannelsG);
			break;
		case DETECTOR_GAIN:
			buffer = g_strdup_printf("%lg",(current+1)->xi->detector->gain);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_gainG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_gainG);
			break;
		case DETECTOR_LIVE_TIME:
			buffer = g_strdup_printf("%lg",(current+1)->xi->detector->live_time);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_live_timeG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_live_timeG);
			break;
		case DETECTOR_PULSE_WIDTH:
			buffer = g_strdup_printf("%lg",(current+1)->xi->detector->pulse_width);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_pulse_widthG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_pulse_widthG);
			break;
		case DETECTOR_ZERO:
			buffer = g_strdup_printf("%lg",(current+1)->xi->detector->zero);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_zeroG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_zeroG);
			break;
		case DETECTOR_NOISE:
			buffer = g_strdup_printf("%lg",(current+1)->xi->detector->noise);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_noiseG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_noiseG);
			break;
		case DETECTOR_FANO:
			buffer = g_strdup_printf("%lg",(current+1)->xi->detector->fano);
			g_signal_handler_block(G_OBJECT((current+1)->widget), detector_fanoG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), detector_fanoG);
			break;
		case IMPORT_FROM_FILE:
			undo_rv = GPOINTER_TO_INT((current+1)->widget);
			if (undo_rv & IMPORT_SELECT_COMPOSITION) {
				change_all_values_composition((current+1)->xi);
			}
			if (undo_rv & IMPORT_SELECT_GEOMETRY) {
				change_all_values_geometry((current+1)->xi);
			}
			if (undo_rv & IMPORT_SELECT_EXCITATION) {
				change_all_values_excitation((current+1)->xi);
			}
			if (undo_rv & IMPORT_SELECT_DETECTORSETTINGS) {
				change_all_values_detectorsettings((current+1)->xi);
			}
			if (undo_rv & IMPORT_SELECT_BEAMABSORBERS) {
				change_all_values_beamabsorbers((current+1)->xi);
			}
			if (undo_rv & IMPORT_SELECT_DETECTIONABSORBERS) {
				change_all_values_detectionabsorbers((current+1)->xi);
			}
			break;


	}


	buffer = g_strdup_printf("Undo: %s",(current+1)->message);
	gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);
	gtk_tool_item_set_tooltip_text(undoT,buffer);
	gtk_widget_set_sensitive(undoW,TRUE);
	gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);



	current++;
	if (current == last) {
		buffer = g_strdup_printf("Redo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);
		gtk_tool_item_set_tooltip_text(redoT,buffer);
		gtk_widget_set_sensitive(redoW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	}
	else {
		buffer = g_strdup_printf("Redo: %s",(current+1)->message);
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);
		gtk_tool_item_set_tooltip_text(redoT,buffer);
	}

	adjust_save_buttons();

	//adjust cut/copy
	GtkWidget *toplevel = gtk_widget_get_toplevel(widget);
	GtkWidget *focused = gtk_window_get_focus(GTK_WINDOW(toplevel));
	if (focused == compositionW ||
	    focused == exc_compositionW ||
	    focused == det_compositionW ||
	    focused == crystal_compositionW) {
		//let's assume it will remain focused
		//so if the store is now empty -> disable cut/copy
		//if instead the store is no longer empty -> enable cut/copy and make sure something is selected
		GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(focused));
		if (gtk_tree_model_iter_n_children(model, NULL) == 0) {
			gtk_widget_set_sensitive(GTK_WIDGET(cutT), FALSE);
			gtk_widget_set_sensitive(GTK_WIDGET(copyT), FALSE);
			gtk_widget_set_sensitive(GTK_WIDGET(cutW), FALSE);
			gtk_widget_set_sensitive(GTK_WIDGET(copyW), FALSE);
		}
		else if (gtk_tree_selection_count_selected_rows(gtk_tree_view_get_selection(GTK_TREE_VIEW(focused))) == 0) {
			gtk_tree_model_get_iter_first(model, &iter);
			GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(model), &iter);
			gtk_tree_selection_select_path(gtk_tree_view_get_selection(GTK_TREE_VIEW(focused)), path);
			gtk_tree_path_free(path);
			gtk_widget_set_sensitive(GTK_WIDGET(cutT), TRUE);
			gtk_widget_set_sensitive(GTK_WIDGET(copyT), TRUE);
			gtk_widget_set_sensitive(GTK_WIDGET(cutW), TRUE);
			gtk_widget_set_sensitive(GTK_WIDGET(copyW), TRUE);
		}

	}
}

static void detector_type_changed(GtkComboBox *widget, gpointer data) {

	//should always work out
	update_undo_buffer(GPOINTER_TO_INT(data), (void*) widget);

	return;
}

static void detector_nchannels_changed(GtkSpinButton *widget, gpointer data) {
	//check if value is different
	if (gtk_spin_button_get_value_as_int(widget) == current->xi->detector->nchannels)
		return;

	//should always work out
	update_undo_buffer(GPOINTER_TO_INT(data), (void*) widget);

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
			if (lastPtr == endPtr && value > 0.0 && strlen(textPtr) != 0) {
				//ok
				if (undo_error) {
					adjust_control_widgets(widget, TRUE);
					//this means that we were in undo error mode
					//cleanup needed
					undo_menu_fix_after_error();
				}
				else {
					if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(redoW)), "Redo") != 0) {
						gtk_widget_set_sensitive(redoW,TRUE);
						gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);
					}
					if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(undoW)), "Undo") != 0) {
						gtk_widget_set_sensitive(undoW,TRUE);
						gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
					}
				}
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,NULL);
				*check = 1;
				if (double_changed_current_check(kind,value))
					update_undo_buffer(kind, (void*) widget);
				else
					adjust_save_buttons();
				return;
			}
			else {
				//bad value
				if (!undo_error) {
					adjust_control_widgets(widget, FALSE);
				}
				*check = 0;
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
				gtk_widget_set_sensitive(redoW,FALSE);
				gtk_widget_set_sensitive(undoW,TRUE);
				gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(undoT), TRUE);
				update_undo_buffer_with_error(kind, (void*) widget, check);
			}
			break;
		//positive
		case DETECTOR_PULSE_WIDTH:
		case COLLIMATOR_HEIGHT:
		case COLLIMATOR_DIAMETER:
			if (lastPtr == endPtr && value >= 0.0 && strlen(textPtr) != 0) {
				//ok
				if (undo_error) {
					adjust_control_widgets(widget, TRUE);
					//this means that we were in undo error mode
					//cleanup needed
					undo_menu_fix_after_error();
				}
				else {
					if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(redoW)), "Redo") != 0) {
						gtk_widget_set_sensitive(redoW,TRUE);
						gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);
					}
					if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(undoW)), "Undo") != 0) {
						gtk_widget_set_sensitive(undoW,TRUE);
						gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
					}
				}
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,NULL);
				*check = 1;
				if (double_changed_current_check(kind,value))
					update_undo_buffer(kind, (void*) widget);
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
				if (!undo_error) {
					adjust_control_widgets(widget, FALSE);
				}
				*check = 0;
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
				gtk_widget_set_sensitive(redoW,FALSE);
				gtk_widget_set_sensitive(undoW,TRUE);
				gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
				update_undo_buffer_with_error(kind, (void*) widget, check);
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
			if (lastPtr == endPtr && strlen(textPtr) != 0) {
				//ok
				if (undo_error) {
					adjust_control_widgets(widget, TRUE);
					//this means that we were in undo error mode
					//cleanup needed
					undo_menu_fix_after_error();
				}
				else {
					if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(redoW)), "Redo") != 0) {
						gtk_widget_set_sensitive(redoW,TRUE);
						gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);
					}
					if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(undoW)), "Undo") != 0) {
						gtk_widget_set_sensitive(undoW,TRUE);
						gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
					}
				}
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,NULL);
				*check = 1;
				if (double_changed_current_check(kind,value))
					update_undo_buffer(kind, (void*) widget);
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
				if (!undo_error) {
					adjust_control_widgets(widget, FALSE);
				}
				*check = 0;
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
				gtk_widget_set_sensitive(redoW,FALSE);
				gtk_widget_set_sensitive(undoW,TRUE);
				gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
				update_undo_buffer_with_error(kind, (void*) widget, check);
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
	adjust_save_buttons();
	return;
}


static void pos_int_changed(GtkWidget *widget, gpointer data) {
	struct val_changed *vc = (struct val_changed *) data;

	int kind = vc->kind;
	int *check = vc->check;
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
			if (g_regex_match(pos_int,textPtr,(GRegexMatchFlags) 0,NULL) == TRUE ){
				//ok
				if (undo_error) {
					adjust_control_widgets(widget, TRUE);
					//this means that we were in undo error mode
					//cleanup needed
					undo_menu_fix_after_error();
				}
				else {
					if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(redoW)), "Redo") != 0) {
						gtk_widget_set_sensitive(redoW,TRUE);
						gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);
					}
					if (strcmp(gtk_menu_item_get_label(GTK_MENU_ITEM(undoW)), "Undo") != 0) {
						gtk_widget_set_sensitive(undoW,TRUE);
						gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
					}
				}
				value = strtol(textPtr, NULL, 10);
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,NULL);
				*check = 1;
				if (pos_int_changed_current_check(kind,value))
					update_undo_buffer(kind, (void*) widget);
				else
					adjust_save_buttons();

				return;
			}
			else {
				//bad value
				*check = 0;
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
				gtk_widget_set_sensitive(redoW,FALSE);
				gtk_widget_set_sensitive(undoW,TRUE);
				gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
				gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);
				if (!undo_error) {
					adjust_control_widgets(widget, FALSE);
				}
				update_undo_buffer_with_error(kind, (void*) widget, check);
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
	if (process_pre_file_operation((GtkWidget *) data) == FALSE)
		return TRUE;
	quit_program_cb((GtkosxApplication*) data, widget);
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


static gboolean dialog_helper_xmsa_cb(struct dialog_helper_xmsa_data *data) {
	struct xmi_archive *archive;
	GtkWidget *dialog = xmi_msim_gui_utils_long_job_dialog(data->window, "<b>Reading XMSA file</b>");
	gtk_widget_show_all(dialog);
	GdkCursor* watchCursor = gdk_cursor_new(GDK_WATCH);
	gdk_window_set_cursor(gtk_widget_get_window(dialog), watchCursor);

	while(gtk_events_pending())
	    gtk_main_iteration();

	struct read_xmsa_data *rxd = (struct read_xmsa_data *) g_malloc(sizeof(struct read_xmsa_data));
	rxd->filename = data->filename;
	rxd->archive = &archive;
#if GLIB_CHECK_VERSION (2, 32, 0)
	//new API
	GThread *xmsa_thread = g_thread_new(NULL, (GThreadFunc) xmi_msim_gui_utils_read_xmsa_thread, (gpointer) rxd);
#else
	//old API
	GThread *xmsa_thread = g_thread_create((GThreadFunc) xmi_msim_gui_utils_read_xmsa_thread, (gpointer) rxd, TRUE, NULL);
#endif
	int xmsa_thread_rv = GPOINTER_TO_INT(g_thread_join(xmsa_thread));
	g_free(rxd);
	gdk_window_set_cursor(gtk_widget_get_window(dialog), NULL);
	gtk_widget_destroy(dialog);
	if (!xmsa_thread_rv) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(data->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"Could not read file %s",data->filename
	       	);
	    	gtk_dialog_run (GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
	}
	else {
		launch_archive_plot(archive, data->window);
	}
	g_free(data->filename);
	g_free(data);
	return FALSE;
}


#ifdef MAC_INTEGRATION

static void quartz_minimize(GtkMenuItem *menuitem, gpointer user_data) {
	[NSApp miniaturizeAll:nil];
}

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

	//bring window to front if necessary
	if ([NSApp isHidden] == YES)
		[NSApp unhide: nil];
	else if ([qwindow isMiniaturized])
		[qwindow deminiaturize:nil];



	//check for filetype
	if (g_ascii_strcasecmp(filename+strlen(filename)-5,".xmsi") == 0) {
		if (process_pre_file_operation(old->window) == FALSE)
			return FALSE;

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
			adjust_save_buttons();
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
	else if (g_ascii_strcasecmp(filename+strlen(filename)-5,".xmso") == 0) {
		//XMSO file
		gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),results_page);
		if (plot_spectra_from_file(filename) == 1) {
			gchar *temp_base = g_path_get_basename(filename);
			update_xmimsim_title_xmso(temp_base, old->window, filename);
			g_free(temp_base);
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
	else if (g_ascii_strcasecmp(filename+strlen(filename)-5,".xmsa") == 0) {
		struct dialog_helper_xmsa_data *my_data = (struct dialog_helper_xmsa_data *) g_malloc(sizeof(struct dialog_helper_xmsa_data));
		my_data->window = old->window;
		my_data->filename = filename;
		dialog_helper_xmsa_cb(my_data);
	}

	gtk_widget_grab_focus(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook),current_page));
	return FALSE;
}


static gboolean load_from_file_osx_cb(GtkosxApplication *app, gchar *path, gpointer data) {
	struct osx_load_data *old = (struct osx_load_data *) g_malloc(sizeof(struct osx_load_data));

	old->window = (GtkWidget *) data;
	old->path = g_strdup(path);

	g_idle_add(load_from_file_osx_helper_cb, (gpointer) old);

	return TRUE;
}
#endif
void reset_undo_buffer(struct xmi_input *xi_new, const char *filename) {
	struct undo_single *iter;

#if DEBUG == 1
	fprintf(stdout,"resetting undo_buffer\n");
#endif


	if (undo_error) {
		g_free(undo_error->message);
		g_free(undo_error);
		gtk_widget_modify_base(GTK_WIDGET(undo_error->widget), GTK_STATE_NORMAL, NULL);
		adjust_control_widgets(GTK_WIDGET(undo_error->widget), TRUE);
		undo_error = NULL;
	}

	for (iter = redo_buffer ; iter <= current ; iter++) {
		g_free(iter->filename);
		if (iter->message)
			g_free(iter->message);
		xmi_free_input(iter->xi);
	}
	g_free(redo_buffer);
	redo_buffer = (struct undo_single *) g_malloc(sizeof(struct undo_single));
	current = redo_buffer;
	last = redo_buffer;
	redo_buffer->filename = g_strdup(filename);
	redo_buffer->xi = xi_new;
	redo_buffer->message = NULL;
	if (filename != NULL && strcmp(filename,UNLIKELY_FILENAME) != 0) {
		if (last_saved != NULL) {
			g_free(last_saved->filename);
			xmi_free_input(last_saved->xi);
			g_free(last_saved);
		}
		last_saved = (struct undo_single *) g_malloc(sizeof(struct undo_single));
		xmi_copy_input(xi_new, &(last_saved->xi));
		last_saved->filename = g_strdup(filename);
	}
	else {
		last_saved = NULL;
	}
	//clear undo/redo messages
	gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	gtk_widget_set_sensitive(undoW,FALSE);
	gtk_widget_set_sensitive(redoW,FALSE);
	gtk_menu_item_set_label(GTK_MENU_ITEM(undoW), "Undo");
	gtk_tool_item_set_tooltip_text(undoT, "Undo");
	gtk_menu_item_set_label(GTK_MENU_ITEM(redoW), "Redo");
	gtk_tool_item_set_tooltip_text(redoT, "Redo");

	return;
}

void update_undo_buffer_with_error(int kind, void *data, int *check) {
	gchar *buffer;
	if (undo_error) {
		//nothing changes while other invalid values are inserted
		return;
	}

	undo_error = (struct undo_single *) g_malloc(sizeof(struct undo_single));
	undo_error->kind = kind;
	undo_error->widget = data;
	undo_error->message = get_message_string(kind);
	undo_error->check = check;

	buffer = g_strdup_printf("Undo: %s", undo_error->message);
	gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);
	gtk_tool_item_set_tooltip_text(undoT,buffer);
	g_free(buffer);
}

void update_undo_buffer(int kind, void *data) {
	char *buffer;
	ptrdiff_t last_diff, current_diff;
	struct undo_single *tempPtr;
	int i,j,n;
	struct import_undo_data *iud;

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
			g_free(tempPtr->filename);
			if (tempPtr->message)
				g_free(tempPtr->message);
			xmi_free_input(tempPtr->xi);
		}
		//disable redo
		buffer = g_strdup_printf("Redo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);
		gtk_widget_set_sensitive(redoW,FALSE);
		gtk_tool_item_set_tooltip_text(redoT,buffer);
		gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	}

	redo_buffer = (struct undo_single *) g_realloc(redo_buffer,(current-redo_buffer+2)*sizeof(struct undo_single));
#if DEBUG == 1
	fprintf(stdout,"After redo_buffer realloc\n");
#endif
	last = redo_buffer + last_diff;
	current = redo_buffer + current_diff;
	last = current+1;
	last->filename = g_strdup(UNLIKELY_FILENAME);
	xmi_copy_input(current->xi, &(last->xi));
	last->kind = kind;
	struct xmi_excitation *temp_exc;
	struct energiesUndoInfo *eui;

	if (kind != IMPORT_FROM_FILE)
		last->message = get_message_string(kind);

	switch (kind) {
		case OUTPUTFILE:
			g_free(last->xi->general->outputfile);
			last->xi->general->outputfile = g_strdup(gtk_entry_get_text(GTK_ENTRY(data)));
			last->widget = data;
			break;
		case N_PHOTONS_INTERVAL:
			last->xi->general->n_photons_interval = strtol((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL,10);
			last->widget = data;
			break;
		case N_PHOTONS_LINE:
			last->xi->general->n_photons_line = strtol((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL,10);
			last->widget = data;
			break;
		case N_INTERACTIONS_TRAJECTORY:
			last->xi->general->n_interactions_trajectory = strtol((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL,10);
			last->widget = data;
			break;
		case COMPOSITION_REFERENCE:
			last->xi->composition->reference_layer = compositionS->reference_layer;
			last->widget = data;
			break;
		case COMPOSITION_ORDER:
		case COMPOSITION_DELETE:
		case COMPOSITION_ADD:
		case COMPOSITION_EDIT:
		case COMPOSITION_PASTE:
		case COMPOSITION_CUT:
			xmi_free_composition(last->xi->composition);
			xmi_copy_composition(compositionS, &(last->xi->composition));
			last->widget = data;
			break;
		case D_SAMPLE_SOURCE:
			last->xi->geometry->d_sample_source = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case N_SAMPLE_ORIENTATION_X:
		case N_SAMPLE_ORIENTATION_Y:
		case N_SAMPLE_ORIENTATION_Z:
			last->xi->geometry->n_sample_orientation[kind-N_SAMPLE_ORIENTATION_X] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case P_DETECTOR_WINDOW_X:
		case P_DETECTOR_WINDOW_Y:
		case P_DETECTOR_WINDOW_Z:
			last->xi->geometry->p_detector_window[kind-P_DETECTOR_WINDOW_X] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case N_DETECTOR_ORIENTATION_X:
		case N_DETECTOR_ORIENTATION_Y:
		case N_DETECTOR_ORIENTATION_Z:
			last->xi->geometry->n_detector_orientation[kind-N_DETECTOR_ORIENTATION_X] = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case AREA_DETECTOR:
			last->xi->geometry->area_detector = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case COLLIMATOR_HEIGHT:
			last->xi->geometry->collimator_height = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case COLLIMATOR_DIAMETER:
			last->xi->geometry->collimator_diameter = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case D_SOURCE_SLIT:
			last->xi->geometry->d_source_slit = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case SLIT_SIZE_X:
			last->xi->geometry->slit_size_x = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case SLIT_SIZE_Y:
			last->xi->geometry->slit_size_y = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case DISCRETE_ENERGY_ADD:
			eui = (struct energiesUndoInfo *) data;
			//realloc discrete energies
			last->xi->excitation->discrete = (struct xmi_energy_discrete*) g_realloc(last->xi->excitation->discrete,sizeof(struct xmi_energy_discrete)*++last->xi->excitation->n_discrete);
			last->xi->excitation->discrete[last->xi->excitation->n_discrete-1] = *eui->energy_disc;
			//sort
			if (last->xi->excitation->n_discrete > 1)
				qsort(last->xi->excitation->discrete, last->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete);
			break;
		case SOURCE_SPECTRUM_ADD:
			temp_exc = (struct xmi_excitation*) data;
			last->xi->excitation->n_discrete += temp_exc->n_discrete;
			//realloc discrete energies
			last->xi->excitation->discrete = (struct xmi_energy_discrete*) g_realloc(last->xi->excitation->discrete,sizeof(struct xmi_energy_discrete)*last->xi->excitation->n_discrete);
			for (i = last->xi->excitation->n_discrete-temp_exc->n_discrete ; i < last->xi->excitation->n_discrete ; i++) {
				last->xi->excitation->discrete[i] = temp_exc->discrete[i-last->xi->excitation->n_discrete+temp_exc->n_discrete];
			}
			qsort(last->xi->excitation->discrete, last->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete);
			last->xi->excitation->n_continuous += temp_exc->n_continuous;
			//realloc continuous energies
			last->xi->excitation->continuous = (struct xmi_energy_continuous*) g_realloc(last->xi->excitation->continuous,sizeof(struct xmi_energy_continuous)*last->xi->excitation->n_continuous);
			for (i = last->xi->excitation->n_continuous-temp_exc->n_continuous ; i < last->xi->excitation->n_continuous ; i++) {
				last->xi->excitation->continuous[i] = temp_exc->continuous[i-last->xi->excitation->n_continuous+temp_exc->n_continuous];
			}
			qsort(last->xi->excitation->continuous, last->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous);
			g_free(temp_exc);

			break;
		case DISCRETE_ENERGY_IMPORT_ADD:
			eui = (struct energiesUndoInfo *) data;
			last->xi->excitation->n_discrete += eui->n_energy_disc;
			//realloc discrete energies
			last->xi->excitation->discrete = (struct xmi_energy_discrete*) g_realloc(last->xi->excitation->discrete,sizeof(struct xmi_energy_discrete)*last->xi->excitation->n_discrete);
			for (i = last->xi->excitation->n_discrete - eui->n_energy_disc ; i < last->xi->excitation->n_discrete ; i++) {
				last->xi->excitation->discrete[i] = eui->energy_disc[i-last->xi->excitation->n_discrete + eui->n_energy_disc];
			}
			if (last->xi->excitation->n_discrete > 1)
				qsort(last->xi->excitation->discrete, last->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete);
			break;
		case DISCRETE_ENERGY_IMPORT_REPLACE:
			eui = (struct energiesUndoInfo *) data;
			last->xi->excitation->n_discrete = eui->n_energy_disc;
			g_free(last->xi->excitation->discrete);
			last->xi->excitation->discrete = (struct xmi_energy_discrete *) xmi_memdup(eui->energy_disc, sizeof(struct xmi_energy_discrete)*last->xi->excitation->n_discrete);
			if (last->xi->excitation->n_discrete > 1)
				qsort(last->xi->excitation->discrete, last->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete);
			break;
		case DISCRETE_ENERGY_CLEAR:
			g_free(last->xi->excitation->discrete);
			last->xi->excitation->discrete = NULL;
			last->xi->excitation->n_discrete = 0;
			break;
		case DISCRETE_ENERGY_EDIT:
			eui = (struct energiesUndoInfo *) data;
			last->xi->excitation->discrete[eui->index] = *eui->energy_disc;
			if (last->xi->excitation->n_discrete > 1)
				qsort(last->xi->excitation->discrete, last->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete);
			break;
		case DISCRETE_ENERGY_SCALE:
			eui = (struct energiesUndoInfo *) data;
			for (i = 0 ; i < eui->indices->len ; i++) {
				last->xi->excitation->discrete[g_array_index(eui->indices, int, i)].horizontal_intensity *= eui->scale_value;
				last->xi->excitation->discrete[g_array_index(eui->indices, int, i)].vertical_intensity *= eui->scale_value;
			}
			break;
		case DISCRETE_ENERGY_DELETE:
			eui = (struct energiesUndoInfo *) data;
			n = eui->indices->len;
			if (last->xi->excitation->n_discrete != n) {
				for (i = last->xi->excitation->n_discrete-1 ; i >= 0 ; i--) {
					if (i == g_array_index(eui->indices, int, n-1)) {
						n--;
						//delete this energy
						for (j = i ; j < last->xi->excitation->n_discrete-1 ; j++)
							last->xi->excitation->discrete[j] = last->xi->excitation->discrete[j+1];
						last->xi->excitation->n_discrete--;
						if (n < 1)
							break;
					}
				}

				last->xi->excitation->discrete = (struct xmi_energy_discrete *) g_realloc(last->xi->excitation->discrete, sizeof(struct xmi_energy_discrete)*last->xi->excitation->n_discrete);
			}
			else {
				g_free(last->xi->excitation->discrete);
				last->xi->excitation->discrete = NULL;
				last->xi->excitation->n_discrete = 0;
			}
			break;
		case CONTINUOUS_ENERGY_ADD:
			eui = (struct energiesUndoInfo *) data;
			//realloc continuous energies
			last->xi->excitation->continuous = (struct xmi_energy_continuous*) g_realloc(last->xi->excitation->continuous,sizeof(struct xmi_energy_continuous)*++last->xi->excitation->n_continuous);
			last->xi->excitation->continuous[last->xi->excitation->n_continuous-1] = *eui->energy_cont;
			//sort
			if (last->xi->excitation->n_continuous > 1)
				qsort(last->xi->excitation->continuous, last->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous);
			break;
		case CONTINUOUS_ENERGY_IMPORT_ADD:
			eui = (struct energiesUndoInfo *) data;
			last->xi->excitation->n_continuous += eui->n_energy_cont;
			//realloc continuous energies
			last->xi->excitation->continuous = (struct xmi_energy_continuous*) g_realloc(last->xi->excitation->continuous,sizeof(struct xmi_energy_continuous)*last->xi->excitation->n_continuous);
			for (i = last->xi->excitation->n_continuous-GPOINTER_TO_INT(data) ; i < last->xi->excitation->n_continuous ; i++) {
				last->xi->excitation->continuous[i] = eui->energy_cont[i-last->xi->excitation->n_continuous + eui->n_energy_cont];
			}
			if (last->xi->excitation->n_continuous > 1)
				qsort(last->xi->excitation->continuous, last->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous);
			break;
		case CONTINUOUS_ENERGY_IMPORT_REPLACE:
			eui = (struct energiesUndoInfo *) data;
			last->xi->excitation->n_continuous = eui->n_energy_cont;
			g_free(last->xi->excitation->continuous);
			last->xi->excitation->continuous = (struct xmi_energy_continuous *) xmi_memdup(eui->energy_cont, sizeof(struct xmi_energy_continuous)*last->xi->excitation->n_continuous);
			if (last->xi->excitation->n_continuous > 1)
				qsort(last->xi->excitation->continuous, last->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous);
			break;
		case CONTINUOUS_ENERGY_CLEAR:
			g_free(last->xi->excitation->continuous);
			last->xi->excitation->continuous = NULL;
			last->xi->excitation->n_continuous = 0;
			break;
		case CONTINUOUS_ENERGY_SCALE:
			eui = (struct energiesUndoInfo *) data;
			for (i = 0 ; i < eui->indices->len ; i++) {
				last->xi->excitation->continuous[g_array_index(eui->indices, int, i)].horizontal_intensity *= eui->scale_value;
				last->xi->excitation->continuous[g_array_index(eui->indices, int, i)].vertical_intensity *= eui->scale_value;
			}
			break;
		case CONTINUOUS_ENERGY_EDIT:
			eui = (struct energiesUndoInfo *) data;
			last->xi->excitation->continuous[eui->index] = *eui->energy_cont;
			if (last->xi->excitation->n_continuous > 1)
				qsort(last->xi->excitation->continuous, last->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous);
			break;
		case CONTINUOUS_ENERGY_DELETE:
			eui = (struct energiesUndoInfo *) data;
			n = eui->indices->len;
			if (last->xi->excitation->n_continuous != n) {
				for (i = last->xi->excitation->n_continuous-1 ; i >= 0 ; i--) {
					if (i == g_array_index(eui->indices, int, n-1)) {
						n--;
						//delete this energy
						for (j = i ; j < last->xi->excitation->n_continuous-1 ; j++)
							last->xi->excitation->continuous[j] = last->xi->excitation->continuous[j+1];
						last->xi->excitation->n_continuous--;
						if (n < 1)
							break;
					}
				}

				last->xi->excitation->continuous = (struct xmi_energy_continuous *) g_realloc(last->xi->excitation->continuous, sizeof(struct xmi_energy_continuous)*last->xi->excitation->n_continuous);
			}
			else {
				g_free(last->xi->excitation->continuous);
				last->xi->excitation->continuous = NULL;
				last->xi->excitation->n_continuous = 0;
			}
			break;
		case SOURCE_SPECTRUM_REPLACE:
			if (last->xi->excitation->n_continuous > 0)
				g_free(last->xi->excitation->continuous);
			if (last->xi->excitation->n_discrete > 0)
				g_free(last->xi->excitation->discrete);
			g_free(last->xi->excitation);
			temp_exc = (struct xmi_excitation*) data;
			last->xi->excitation = temp_exc;
			break;
		case EXC_COMPOSITION_ORDER:
		case EXC_COMPOSITION_DELETE:
		case EXC_COMPOSITION_ADD:
		case EXC_COMPOSITION_PASTE:
		case EXC_COMPOSITION_CUT:
		case EXC_COMPOSITION_EDIT:
			if (last->xi->absorbers->n_exc_layers > 0) {
				for (i = 0 ; i < last->xi->absorbers->n_exc_layers ; i++)
					xmi_free_layer(last->xi->absorbers->exc_layers+i);
				g_free(last->xi->absorbers->exc_layers);
			}
			xmi_copy_composition2abs_or_crystal(exc_compositionS, &(last->xi->absorbers->exc_layers),&(last->xi->absorbers->n_exc_layers));
			last->widget = data;
			break;
		case DET_COMPOSITION_ORDER:
		case DET_COMPOSITION_DELETE:
		case DET_COMPOSITION_ADD:
		case DET_COMPOSITION_PASTE:
		case DET_COMPOSITION_CUT:
		case DET_COMPOSITION_EDIT:
			if (last->xi->absorbers->n_det_layers > 0) {
				for (i = 0 ; i < last->xi->absorbers->n_det_layers ; i++)
					xmi_free_layer(last->xi->absorbers->det_layers+i);
				g_free(last->xi->absorbers->det_layers);
			}
			xmi_copy_composition2abs_or_crystal(det_compositionS, &(last->xi->absorbers->det_layers),&(last->xi->absorbers->n_det_layers));
			last->widget = data;
			break;
		case CRYSTAL_COMPOSITION_ORDER:
		case CRYSTAL_COMPOSITION_DELETE:
		case CRYSTAL_COMPOSITION_ADD:
		case CRYSTAL_COMPOSITION_PASTE:
		case CRYSTAL_COMPOSITION_CUT:
		case CRYSTAL_COMPOSITION_EDIT:
			if (last->xi->detector->n_crystal_layers > 0) {
				for (i = 0 ; i < last->xi->detector->n_crystal_layers ; i++)
					xmi_free_layer(last->xi->detector->crystal_layers+i);
				g_free(last->xi->detector->crystal_layers);
			}
			xmi_copy_composition2abs_or_crystal(crystal_compositionS, &(last->xi->detector->crystal_layers),&(last->xi->detector->n_crystal_layers));
			last->widget = data;
			break;
		case DETECTOR_TYPE:
			last->xi->detector->detector_type = gtk_combo_box_get_active(GTK_COMBO_BOX(data));
			last->widget = data;
			break;
		case DETECTOR_NCHANNELS:
			last->xi->detector->nchannels = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(data));
			last->widget = data;
			break;
		case DETECTOR_GAIN:
			last->xi->detector->gain = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case DETECTOR_LIVE_TIME:
			last->xi->detector->live_time = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case DETECTOR_PULSE_WIDTH:
			last->xi->detector->pulse_width= strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case DETECTOR_ZERO:
			last->xi->detector->zero = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case DETECTOR_NOISE:
			last->xi->detector->noise = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case DETECTOR_FANO:
			last->xi->detector->fano = strtod((char *) gtk_entry_get_text(GTK_ENTRY(data)),NULL);
			last->widget = data;
			break;
		case IMPORT_FROM_FILE:
			iud = (struct import_undo_data *) data;
			last->message = g_strdup_printf("import from %s", g_path_get_basename(iud->filename));
			last->widget = (GtkWidget *) GINT_TO_POINTER(iud->undo_rv);
			if (iud->undo_rv & IMPORT_SELECT_COMPOSITION) {
				xmi_free_composition(last->xi->composition);
				xmi_copy_composition(iud->xi->composition, &last->xi->composition);
			}
			if (iud->undo_rv & IMPORT_SELECT_GEOMETRY) {
				xmi_free_geometry(last->xi->geometry);
				xmi_copy_geometry(iud->xi->geometry, &last->xi->geometry);
			}
			if (iud->undo_rv & IMPORT_SELECT_EXCITATION) {
				xmi_free_excitation(last->xi->excitation);
				xmi_copy_excitation(iud->xi->excitation, &last->xi->excitation);
			}
			if (iud->undo_rv & IMPORT_SELECT_DETECTORSETTINGS) {
				xmi_free_detector(last->xi->detector);
				xmi_copy_detector(iud->xi->detector, &last->xi->detector);
			}
			if (iud->undo_rv & IMPORT_SELECT_BEAMABSORBERS) {
				xmi_free_exc_absorbers(last->xi->absorbers);
				xmi_copy_exc_absorbers(iud->xi->absorbers, last->xi->absorbers);
			}
			if (iud->undo_rv & IMPORT_SELECT_DETECTIONABSORBERS) {
				xmi_free_det_absorbers(last->xi->absorbers);
				xmi_copy_det_absorbers(iud->xi->absorbers, last->xi->absorbers);
			}
			break;
		default:
			g_fprintf(stderr, "Invalid value in update_undo_buffer. Fatal error\n");
			exit(1);
	}
	current = last;
	buffer = g_strdup_printf("Undo: %s",last->message);
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

static void comments_buffer_notify_has_selection_cb(GtkTextBuffer *buffer, GParamSpec *pspec, gpointer data) {
	gboolean has_selection = gtk_text_buffer_get_has_selection(buffer);
	gtk_widget_set_sensitive(GTK_WIDGET(cutT), has_selection);
	gtk_widget_set_sensitive(GTK_WIDGET(copyT), has_selection);
	gtk_widget_set_sensitive(GTK_WIDGET(cutW), has_selection);
	gtk_widget_set_sensitive(GTK_WIDGET(copyW), has_selection);
}

static void entry_notify_cursor_position_cb(GObject *entry, GParamSpec *pspec, gpointer data) {
	//fprintf(stdout, "Entering entry_notify_cursor_position_cb\n");
	//ignore if not in focus
	if (!gtk_widget_has_focus(GTK_WIDGET(entry))) {
		return;
	}

	gint selection_bound;
	gint current_pos;

	g_object_get(entry, "selection-bound", &selection_bound, "cursor-position", &current_pos, NULL);

	if (selection_bound == current_pos) {
		//fprintf(stdout, "No selection\n");
		gtk_widget_set_sensitive(GTK_WIDGET(cutT), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyT), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(cutW), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyW), FALSE);
	}
	else {
		//fprintf(stdout, "With selection\n");
		gtk_widget_set_sensitive(GTK_WIDGET(cutT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(cutW), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyW), TRUE);
	}

}
static gboolean comments_focus_out_cb(GtkTextView *comments, GdkEvent *event, gpointer data) {
	GtkTextBuffer *commentsBuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (comments));
	GtkTextIter start;
	if (gtk_text_buffer_get_selection_bounds(commentsBuffer, &start, NULL)) {
		gtk_text_buffer_select_range(commentsBuffer, &start, &start);
	}

	gtk_widget_set_sensitive(GTK_WIDGET(pasteT), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pasteW), FALSE);

	return FALSE;
}

static gboolean comments_focus_in_cb(GtkTextView *comments, GdkEvent *event, gpointer data) {
	if (gtk_clipboard_get(GDK_SELECTION_CLIPBOARD) && gtk_clipboard_wait_is_text_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD))) {
		gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}

	return FALSE;
}

static gboolean entry_focus_out_cb(GtkEntry *entry, GdkEvent *event, gpointer data) {
	//make sure possible selections are removed
	gtk_editable_select_region(GTK_EDITABLE(entry), 0, 0);


	//deactivate cut and copy
	gtk_widget_set_sensitive(GTK_WIDGET(cutT), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(copyT), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pasteT), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(cutW), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(copyW), FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(pasteW), FALSE);


	return FALSE;
}

static gboolean entry_focus_in_cb(GtkEntry *entry, GdkEvent *event, gpointer data) {
	//fprintf(stdout, "Entering entry_focus_in_cb\n");
	//entry_notify_cursor_position_cb(G_OBJECT(entry), NULL, NULL);
	if (gtk_clipboard_get(GDK_SELECTION_CLIPBOARD) && gtk_clipboard_wait_is_text_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD))) {
		gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}
	return FALSE;
}

static void xray_sources_button_clicked_cb(GtkWidget *xray_button, GtkWidget *main_window) {
	GtkWidget *dialog = xmi_msim_gui_sources_dialog_new(GTK_WINDOW(main_window), current->xi);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		// get the excitation data from the dialog
		struct xmi_excitation *excitation_orig = xmi_msim_gui_sources_dialog_get_raw_data(XMI_MSIM_GUI_SOURCES_DIALOG(dialog));
		struct xmi_excitation *excitation;

		// a copy is necessary since the original data will get destroyed when the dialog is destroyed
		xmi_copy_excitation(excitation_orig, &excitation);

		gtk_widget_destroy(dialog);

		// unlikely to occur: produce an error message if it does...
		if (excitation == NULL) {
			GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "xmi_msim_gui_sources_dialog_get_raw_data returned NULL!!!");
			gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "This is a bug and should be reported to the developers");
			gtk_dialog_run(GTK_DIALOG(info_dialog));
			gtk_widget_destroy(info_dialog);
			return;	
		}

		// start new dialog: we need to know if the existing spectrum needs to be replaced or augmented with the new one
		GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "Add new source spectrum to current excitation spectrum or replace it completely?");
		gtk_dialog_add_buttons(GTK_DIALOG(info_dialog), GTK_STOCK_ADD, GTK_RESPONSE_OK, GTK_STOCK_REFRESH, GTK_RESPONSE_CANCEL, NULL);
		GtkWidget *button = gtk_dialog_get_widget_for_response(GTK_DIALOG(info_dialog), GTK_RESPONSE_CANCEL);
		xmi_msim_gui_utils_update_button_text(button, "Replace");
		//this may not work on all platforms -> Mac OS X
		gtk_window_set_deletable(GTK_WINDOW(info_dialog), FALSE);
		int rv;
		while ((rv = gtk_dialog_run(GTK_DIALOG(info_dialog))) == GTK_RESPONSE_DELETE_EVENT) {
		}
		gtk_widget_destroy(info_dialog);
		if (rv == GTK_RESPONSE_OK) {
			// add
			int i;
			if (current->xi->excitation->n_discrete > 0) {
				for (i = 0 ; i < current->xi->excitation->n_discrete ; i++) {
					if (bsearch(excitation->discrete+i, current->xi->excitation->discrete, current->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
						GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy lines: one or more of the new energies exist already in the list of lines.");
						gtk_dialog_run(GTK_DIALOG(error_dialog));
						gtk_widget_destroy(error_dialog);
						return;
					}
				}
			}
			if (current->xi->excitation->n_continuous > 0) {
				for (i = 0 ; i < current->xi->excitation->n_continuous ; i++) {
					if (bsearch(excitation->continuous+i, current->xi->excitation->continuous, current->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
						GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy lines: one or more of the new energies exist already in the list of lines.");
						gtk_dialog_run(GTK_DIALOG(error_dialog));
						gtk_widget_destroy(error_dialog);
						return;
					}
				}
			}
			update_undo_buffer(SOURCE_SPECTRUM_ADD, (GtkWidget *) excitation);
		} else {
			// replace
			update_undo_buffer(SOURCE_SPECTRUM_REPLACE, (GtkWidget *) excitation);
		}
		repopulate_discrete_energies(discWidget->store, (current)->xi->excitation);
		repopulate_continuous_energies(contWidget->store, (current)->xi->excitation);
	}
	else {
		gtk_widget_destroy(dialog);
	}

	adjust_save_buttons();
}

static void enable_entry_signals(GtkWidget *entry) {
	g_signal_connect(G_OBJECT(entry), "notify::cursor-position", G_CALLBACK(entry_notify_cursor_position_cb), NULL);
	g_signal_connect(G_OBJECT(entry), "notify::selection-bound", G_CALLBACK(entry_notify_cursor_position_cb), NULL);
	g_signal_connect(G_OBJECT(entry), "focus-out-event", G_CALLBACK(entry_focus_out_cb), NULL);
	g_signal_connect(G_OBJECT(entry), "focus-in-event", G_CALLBACK(entry_focus_in_cb), NULL);
}



XMI_MAIN
	GtkWidget *window;
	GtkWidget *Main_vbox;

	GtkWidget *menubar;
	GtkWidget *filemenu;
	GtkWidget *file;
	GtkWidget *editmenu;
	GtkWidget *edit;
	GtkWidget *toolsmenu;
	GtkWidget *tools;
	GtkWidget *convertW;
	GtkWidget *convertXMSIW;
	GtkWidget *convertXMSOW;
	GtkWidget *convertXMSAW;
	GtkWidget *convertmenu;
	GtkWidget *convertmenuXMSI;
	GtkWidget *convertmenuXMSO;
	GtkWidget *convertmenuXMSA;
	GtkWidget *xmso2xmsiW;
	GtkWidget *xmso2csvW;
	GtkWidget *xmso2htmlW;
	GtkWidget *xmso2svgW;
	GtkWidget *xmso2speW;
	GtkWidget *xmsi2xrmcW;
	GtkWidget *xmsa2xmsoW;

	GtkWidget *frame;
	GtkWidget *superframe;
	GtkWidget *label;
	GtkWidget *vbox_notebook;
	GtkWidget *hbox_text_label;
	GtkWidget *text;
	GtkWidget *toolbar;
	GtkWidget *scrolled_window;
	GtkWidget *tempW;
	char *buffer;
	struct xmi_composition *temp_composition;
	struct val_changed *vc;
	char *title;
	GtkTextBuffer *commentsBuffer;
	gint main_height=900;
	gint main_width=950;
	gint main_temp;
	GtkWidget *resultsPageW, *controlsPageW;
	GtkAccelGroup *accel_group = NULL;
	GtkWidget *aboutW;
	GtkWidget *userguideW;
	GtkWidget *github_rootW;
	GtkWidget *github_wikiW;
	GtkWidget *report_bugW;
	GtkWidget *request_featureW;
	GtkWidget *tab1W;
	GtkWidget *tab2W;
	GtkWidget *tab3W;
	GOptionContext *context;
	GError *error = NULL;
	static int version = 0;
	static GOptionEntry entries[] = {
		{ "version", 0, 0, G_OPTION_ARG_NONE, &version, "display version information", NULL },
		{NULL}
	};
#ifdef MAC_INTEGRATION
	GtkosxApplication *theApp;
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

	// no xraylib error messages!
	SetErrorMessages(0);

	xmi_init_hdf5();

	context = g_option_context_new ("XMSI/XMSO/XMSA file");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmimsim-gui: a graphical user interface for generating and running XMSI files and visualizing XMSO files\n");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		return 1;
	}

	if (version) {
		g_fprintf(stdout,"%s",xmi_version_string());
		return 0;
	}



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
	gchar *installation_dir = g_win32_get_package_installation_directory_of_module(NULL);
	gchar *path_to_plplot = g_build_filename(installation_dir, "Share", "plplot", NULL);
	g_setenv("PLPLOT_LIB", path_to_plplot, TRUE);
	g_free(installation_dir);
	g_free(path_to_plplot);
#else
	g_setenv("LANG","en_US",TRUE);
#endif
	gtk_disable_setlocale();
	setbuf(stdout,NULL);
	//let's use the default C locale
	//g_type_init
#if !GLIB_CHECK_VERSION (2, 35, 3)
	g_type_init();
#endif

#if !GLIB_CHECK_VERSION (2, 32, 0)
	g_thread_init(NULL);
#endif

	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		return 1;
	}


	//initialize undo system
	redo_buffer = (struct undo_single *) g_malloc(sizeof(struct undo_single ));
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
	current->filename = g_strdup(UNLIKELY_FILENAME);
	current->message = NULL;

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

	//initialize regex patterns
	/*
	if (regcomp(&pos_int,"^[1-9][0-9]*$" , REG_EXTENDED | REG_NOSUB) != 0) {
		fprintf(stderr,"Error compiling regex pattern pos_int\n");
		return 1;
	}
	*/
	pos_int = g_regex_new("^[1-9][0-9]*$", G_REGEX_EXTENDED, (GRegexMatchFlags) 0, NULL);


	gtk_init(&argc, &argv);
#ifdef HAVE_CXX
	Gtk::Main::init_gtkmm_internals();
#endif
	LayerAtom = gdk_atom_intern("xmi-msim-layer", FALSE);

	xmi_msim_gui_utils_init_colors();

#ifdef MAC_INTEGRATION
	theApp = (GtkosxApplication *) g_object_new(GTKOSX_TYPE_APPLICATION,NULL);
	gtkosx_application_set_use_quartz_accelerators(theApp, TRUE);

	if ([NSWindow respondsToSelector:@selector(setAllowsAutomaticWindowTabbing:)]) {
		[NSWindow setAllowsAutomaticWindowTabbing:NO];
	}
#endif

	g_set_application_name("XMI-MSIM");

	if (!gdk_color_parse("Chartreuse", &chartreuse_green)) {
		g_fprintf(stderr, "Chartreuse green not found, continuing with standard green instead\n");
		chartreuse_green = green;
	}

#if GTK_MAJOR_VERSION == 3
	g_type_class_unref(g_type_class_ref(GTK_TYPE_IMAGE_MENU_ITEM));
	g_object_set(gtk_settings_get_default(), "gtk-menu-images", TRUE, NULL);
#endif
	//iconfactory stuff
	//based on http://www.gtkforums.com/viewtopic.php?t=7654
	static GtkStockItem stock_items[] = {
		{(gchar *) XMI_STOCK_RADIATION_WARNING, (gchar *) "X-ray sources", (GdkModifierType) 0, 0, NULL},
		{(gchar *) XMI_STOCK_LOGO, (gchar *) "XMSI file", (GdkModifierType) 0, 0, NULL},
		{(gchar *) XMI_STOCK_LOGO_RED, (gchar *) "XMSO file", (GdkModifierType) 0, 0, NULL},
		{(gchar *) XMI_STOCK_LOGO_ARCHIVE, (gchar *) "XMSA file", (GdkModifierType) 0, 0, NULL}
	};
	gtk_stock_add_static (stock_items, G_N_ELEMENTS (stock_items));

	GtkIconFactory *factory = gtk_icon_factory_new();
	gtk_icon_factory_add_default (factory);
	GtkIconSet *iconset;
	GtkIconSource *source;

#ifdef G_OS_WIN32
	GdkPixbuf *pixbuf;
	gchar *icons_dir = NULL;
	if (xmi_registry_win_query(XMI_REGISTRY_WIN_ICONS_DIR, &icons_dir) == 0) {
		g_fprintf(stderr, "Could not open icons dir\n");
		exit(1);
	}

#define ADD_ICON(name_macro, name_pixbuf) \
	{ \
		GString *icon_file = g_string_new(icons_dir); \
		g_string_append(icon_file, name_pixbuf ".png"); \
		GError *icon_error = NULL; \
		pixbuf = gdk_pixbuf_new_from_file(icon_file->str, &icon_error); \
		if (icon_error) { \
			fprintf(stderr, "gdk_pixbuf_new_from_file error %s\n", icon_error->message); \
			exit(1); \
		} \
		iconset = gtk_icon_set_new_from_pixbuf(pixbuf);\
		g_object_unref(pixbuf); \
		gtk_icon_factory_add (factory, name_macro, iconset);\
		gtk_icon_set_unref (iconset); \
		g_string_free(icon_file, TRUE); \
	}

	ADD_ICON(XMI_STOCK_RADIATION_WARNING, "Radiation_warning_symbol");
	ADD_ICON(XMI_STOCK_LOGO, "Logo_xmi_msim");
	ADD_ICON(XMI_STOCK_LOGO_RED, "Logo_xmi_msim_red");
	ADD_ICON(XMI_STOCK_LOGO_ARCHIVE, "Logo_xmi_msim_archive");
	g_free(icons_dir);

#undef ADD_ICON
#else
#define ADD_ICON(name) source = gtk_icon_source_new (); \
	gtk_icon_source_set_icon_name (source, name); \
	iconset = gtk_icon_set_new (); \
	gtk_icon_set_add_source (iconset, source);\
	gtk_icon_source_free (source);\
	gtk_icon_factory_add (factory, name, iconset);\
	gtk_icon_set_unref (iconset)

	ADD_ICON(XMI_STOCK_RADIATION_WARNING);
	ADD_ICON(XMI_STOCK_LOGO);
	ADD_ICON(XMI_STOCK_LOGO_RED);
	ADD_ICON(XMI_STOCK_LOGO_ARCHIVE);

#undef ADD_ICON
#endif

	gtk_window_set_default_icon_name(XMI_STOCK_LOGO);
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
	GtkWidget *openrecentW = gtk_recent_chooser_menu_new();
	GtkRecentFilter *filter = gtk_recent_filter_new();
	gtk_recent_filter_add_pattern(filter, "*.xmsi");
	gtk_recent_filter_add_pattern(filter, "*.xmso");
	gtk_recent_filter_add_pattern(filter, "*.xmsa");
	//gtk_recent_filter_add_application(filter, g_get_application_name());
	gtk_recent_chooser_add_filter(GTK_RECENT_CHOOSER(openrecentW), filter);
#if defined(G_OS_WIN32) || defined(MAC_INTEGRATION)
	gtk_recent_chooser_set_show_icons(GTK_RECENT_CHOOSER(openrecentW), FALSE);
#else
	gtk_recent_chooser_set_show_icons(GTK_RECENT_CHOOSER(openrecentW), TRUE);
#endif
	gtk_recent_chooser_set_show_tips(GTK_RECENT_CHOOSER(openrecentW), TRUE);
	gtk_recent_chooser_set_sort_type(GTK_RECENT_CHOOSER(openrecentW), GTK_RECENT_SORT_MRU);
	g_signal_connect(G_OBJECT(openrecentW), "item-activated", G_CALLBACK(chooser_activated_cb), (gpointer) window);

	GtkWidget *openrecent_menuW = gtk_menu_item_new_with_label("Open Recent");
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(openrecent_menuW), openrecentW);

	saveW = gtk_image_menu_item_new_from_stock(GTK_STOCK_SAVE,accel_group);
	save_asW = gtk_image_menu_item_new_from_stock(GTK_STOCK_SAVE_AS,accel_group);
	importW= gtk_menu_item_new_with_label("Import");
#ifndef MAC_INTEGRATION
	quitW = gtk_image_menu_item_new_from_stock(GTK_STOCK_QUIT,accel_group);
#endif
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(file),filemenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),newW);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),openW);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),openrecent_menuW);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),saveW);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),save_asW);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),importW);
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
	g_signal_connect(G_OBJECT(newW),"activate",G_CALLBACK(new_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(openW),"activate",G_CALLBACK(load_from_file_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(saveW),"activate",G_CALLBACK(save_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(save_asW),"activate",G_CALLBACK(saveas_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(importW),"activate",G_CALLBACK(import_cb),(gpointer) window);
	editmenu = gtk_menu_new();
	edit = gtk_menu_item_new_with_label("Edit");
	undoW = gtk_image_menu_item_new_from_stock(GTK_STOCK_UNDO,accel_group);
	g_signal_connect(G_OBJECT(undoW),"activate",G_CALLBACK(undo_menu_click),NULL);
	redoW = gtk_image_menu_item_new_from_stock(GTK_STOCK_REDO,accel_group);
	g_signal_connect(G_OBJECT(redoW),"activate",G_CALLBACK(redo_menu_click),NULL);
	cutW = gtk_image_menu_item_new_from_stock(GTK_STOCK_CUT,accel_group);
	g_signal_connect(G_OBJECT(cutW),"activate",G_CALLBACK(cut_button_clicked_cb), (gpointer) window);
	copyW = gtk_image_menu_item_new_from_stock(GTK_STOCK_COPY,accel_group);
	g_signal_connect(G_OBJECT(copyW),"activate",G_CALLBACK(copy_button_clicked_cb), (gpointer) window);
	pasteW = gtk_image_menu_item_new_from_stock(GTK_STOCK_PASTE,accel_group);
	g_signal_connect(G_OBJECT(pasteW),"activate",G_CALLBACK(paste_button_clicked_cb), (gpointer) window);

	//Tools
	toolsmenu = gtk_menu_new();
	tools = gtk_menu_item_new_with_label("Tools");
	tube_ebelW = gtk_image_menu_item_new_from_stock(XMI_STOCK_RADIATION_WARNING,accel_group);
	g_signal_connect(G_OBJECT(tube_ebelW),"activate",G_CALLBACK(xray_sources_button_clicked_cb), (gpointer) window);
	batchmodeW = gtk_image_menu_item_new_from_stock(GTK_STOCK_DND_MULTIPLE,NULL);
	g_signal_connect(G_OBJECT(batchmodeW),"activate",G_CALLBACK(batchmode_button_clicked_cb), (gpointer) window);
	convertW = gtk_image_menu_item_new_from_stock(GTK_STOCK_CONVERT, NULL);
	gtk_menu_item_set_label(GTK_MENU_ITEM(convertW), "Convert");
	gtk_menu_item_set_label(GTK_MENU_ITEM(batchmodeW), "Batch mode");
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(tools), toolsmenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(toolsmenu), convertW);
	gtk_menu_shell_append(GTK_MENU_SHELL(toolsmenu), tube_ebelW);
	gtk_menu_shell_append(GTK_MENU_SHELL(toolsmenu), batchmodeW);

	convertmenuXMSI = gtk_menu_new();
	xmsi2xrmcW = gtk_menu_item_new_with_label("to XRMC input-files");
	gtk_menu_shell_append(GTK_MENU_SHELL(convertmenuXMSI), xmsi2xrmcW);
	g_signal_connect(G_OBJECT(xmsi2xrmcW), "activate", G_CALLBACK(xmimsim_gui_xmsi2xrmc), (gpointer) window);

	convertmenuXMSO = gtk_menu_new();
	xmso2xmsiW = gtk_menu_item_new_with_label("to XMSI");
	xmso2csvW = gtk_menu_item_new_with_label("to CSV");
	xmso2svgW = gtk_menu_item_new_with_label("to SVG");
	xmso2htmlW = gtk_menu_item_new_with_label("to HTML");
	xmso2speW = gtk_menu_item_new_with_label("to SPE");
	gtk_menu_shell_append(GTK_MENU_SHELL(convertmenuXMSO), xmso2xmsiW);
	gtk_menu_shell_append(GTK_MENU_SHELL(convertmenuXMSO), xmso2csvW);
	gtk_menu_shell_append(GTK_MENU_SHELL(convertmenuXMSO), xmso2svgW);
	gtk_menu_shell_append(GTK_MENU_SHELL(convertmenuXMSO), xmso2htmlW);
	gtk_menu_shell_append(GTK_MENU_SHELL(convertmenuXMSO), xmso2speW);
	g_signal_connect(G_OBJECT(xmso2xmsiW), "activate", G_CALLBACK(xmimsim_gui_xmso2xmsi), (gpointer) window);
	g_signal_connect(G_OBJECT(xmso2csvW), "activate", G_CALLBACK(xmimsim_gui_xmso2csv), (gpointer) window);
	g_signal_connect(G_OBJECT(xmso2svgW), "activate", G_CALLBACK(xmimsim_gui_xmso2svg), (gpointer) window);
	g_signal_connect(G_OBJECT(xmso2htmlW), "activate", G_CALLBACK(xmimsim_gui_xmso2html), (gpointer) window);
	g_signal_connect(G_OBJECT(xmso2speW), "activate", G_CALLBACK(xmimsim_gui_xmso2spe), (gpointer) window);


	convertmenuXMSA = gtk_menu_new();
	xmsa2xmsoW = gtk_menu_item_new_with_label("to XMSO");
	gtk_menu_shell_append(GTK_MENU_SHELL(convertmenuXMSA), xmsa2xmsoW);
	g_signal_connect(G_OBJECT(xmsa2xmsoW), "activate", G_CALLBACK(xmimsim_gui_xmsa2xmso), (gpointer) window);


	convertmenu = gtk_menu_new();
	convertXMSIW = gtk_image_menu_item_new_from_stock(XMI_STOCK_LOGO, NULL);
	convertXMSOW = gtk_image_menu_item_new_from_stock(XMI_STOCK_LOGO_RED, NULL);
	convertXMSAW = gtk_image_menu_item_new_from_stock(XMI_STOCK_LOGO_ARCHIVE, NULL);
	gtk_menu_shell_append(GTK_MENU_SHELL(convertmenu), convertXMSIW);
	gtk_menu_shell_append(GTK_MENU_SHELL(convertmenu), convertXMSOW);
	gtk_menu_shell_append(GTK_MENU_SHELL(convertmenu), convertXMSAW);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(convertW), convertmenu);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(convertXMSIW), convertmenuXMSI);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(convertXMSOW), convertmenuXMSO);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(convertXMSAW), convertmenuXMSA);



	preferencesW = gtk_image_menu_item_new_from_stock(GTK_STOCK_PREFERENCES,accel_group);
	g_signal_connect(G_OBJECT(preferencesW),"activate",G_CALLBACK(xmimsim_gui_launch_preferences), (gpointer) window);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(edit),editmenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),undoW);
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),redoW);
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),cutW);
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),copyW);
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),pasteW);
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),edit);
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),tools);
#ifndef MAC_INTEGRATION
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),(GtkWidget *)g_object_ref(gtk_separator_menu_item_new()));
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),preferencesW);
#endif
	//both should be greyed out in the beginning
	gtk_widget_set_sensitive(undoW,FALSE);
	gtk_widget_set_sensitive(redoW,FALSE);
	gtk_widget_set_sensitive(saveW,FALSE);
	gtk_widget_set_sensitive(save_asW,FALSE);
	gtk_widget_set_sensitive(copyW,FALSE);
	gtk_widget_set_sensitive(cutW,FALSE);
	gtk_widget_set_sensitive(pasteW,FALSE);

	//add accelerators
	gtk_widget_add_accelerator(newW, "activate", accel_group, GDK_KEY_n, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(openW, "activate", accel_group, GDK_KEY_o, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(saveW, "activate", accel_group, GDK_KEY_s, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(save_asW, "activate", accel_group, GDK_KEY_s, (GdkModifierType) (PRIMARY_ACCEL_KEY | GDK_SHIFT_MASK), GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(importW, "activate", accel_group, GDK_KEY_i, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(undoW, "activate", accel_group, GDK_KEY_z, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(redoW, "activate", accel_group, GDK_KEY_z, (GdkModifierType) (PRIMARY_ACCEL_KEY | GDK_SHIFT_MASK), GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(cutW, "activate", accel_group, GDK_KEY_x, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(copyW, "activate", accel_group, GDK_KEY_c, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(pasteW, "activate", accel_group, GDK_KEY_v, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);

	GtkWidget *helpmenu, *helpitem;
	GtkWidget *windowmenu, *windowitem;
	windowmenu = gtk_menu_new();
	windowitem = gtk_menu_item_new_with_label("Window");
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),windowitem);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(windowitem), windowmenu);
#ifdef MAC_INTEGRATION
	minimizeW = gtk_menu_item_new_with_label("Minimize");
	gtk_menu_shell_append(GTK_MENU_SHELL(windowmenu), minimizeW);
	gtk_widget_add_accelerator(minimizeW, "activate", accel_group, GDK_KEY_m, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	g_signal_connect(G_OBJECT(minimizeW), "activate", G_CALLBACK(quartz_minimize), NULL);
	gtk_menu_shell_append(GTK_MENU_SHELL(windowmenu), GTK_WIDGET(g_object_ref(gtk_separator_menu_item_new())));
#endif
	tab1W= gtk_menu_item_new_with_label("Input parameters");
	tab2W= gtk_menu_item_new_with_label("Simulation controls");
	tab3W= gtk_menu_item_new_with_label("Results");
	gtk_menu_shell_append(GTK_MENU_SHELL(windowmenu),tab1W);
	gtk_menu_shell_append(GTK_MENU_SHELL(windowmenu),tab2W);
	gtk_menu_shell_append(GTK_MENU_SHELL(windowmenu),tab3W);
	g_signal_connect(G_OBJECT(tab1W),"activate",G_CALLBACK(switch_tab_click), GINT_TO_POINTER(0));
	g_signal_connect(G_OBJECT(tab2W),"activate",G_CALLBACK(switch_tab_click), GINT_TO_POINTER(1));
	g_signal_connect(G_OBJECT(tab3W),"activate",G_CALLBACK(switch_tab_click), GINT_TO_POINTER(2));
	gtk_widget_add_accelerator(tab1W, "activate", accel_group, GDK_KEY_1, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(tab2W, "activate", accel_group, GDK_KEY_2, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_widget_add_accelerator(tab3W, "activate", accel_group, GDK_KEY_3, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);

#ifdef MAC_INTEGRATION
	helpmenu = gtk_menu_new();
	helpitem = gtk_menu_item_new_with_label("Help");
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),helpitem);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(helpitem), helpmenu);
	userguideW= gtk_menu_item_new_with_label("Visit XMI-MSIM User guide");
	github_rootW= gtk_menu_item_new_with_label("Visit XMI-MSIM Github repository");
	github_wikiW= gtk_menu_item_new_with_label("Visit XMI-MSIM Wiki");
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),userguideW);
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),github_rootW);
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),github_wikiW);
	g_signal_connect(G_OBJECT(userguideW),"activate",G_CALLBACK(url_click), (gpointer) "https://github.com/tschoonj/xmimsim/wiki/User-guide");
	g_signal_connect(G_OBJECT(github_rootW),"activate",G_CALLBACK(url_click), (gpointer) "https://github.com/tschoonj/xmimsim/");
	g_signal_connect(G_OBJECT(github_wikiW),"activate",G_CALLBACK(url_click), (gpointer) "https://github.com/tschoonj/xmimsim/wiki/Home");
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu), GTK_WIDGET(g_object_ref(gtk_separator_menu_item_new())));
	report_bugW= gtk_menu_item_new_with_label("Report a Bug");
	request_featureW= gtk_menu_item_new_with_label("Request a feature");
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),report_bugW);
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),request_featureW);
	g_signal_connect(G_OBJECT(report_bugW), "activate", G_CALLBACK(email_click), (gpointer) "Tom.Schoonjans@gmail.com?subject=XMI-MSIM%20bug%20report");
	g_signal_connect(G_OBJECT(request_featureW), "activate", G_CALLBACK(email_click), (gpointer) "Tom.Schoonjans@gmail.com?subject=XMI-MSIM%20feature%20request");

	gtk_box_pack_start(GTK_BOX(Main_vbox), menubar, FALSE, FALSE, 0);
	gtk_widget_show_all(menubar);
	gtk_widget_hide(menubar);
	gtkosx_application_set_menu_bar(theApp, GTK_MENU_SHELL(menubar));
	aboutW = gtk_image_menu_item_new_from_stock(GTK_STOCK_ABOUT, NULL);
	g_signal_connect(G_OBJECT(aboutW),"activate",G_CALLBACK(about_click),window);
	gtkosx_application_insert_app_menu_item(theApp, aboutW, 0);
  #ifdef XMIMSIM_GUI_UPDATER_H
	updatesW = gtk_menu_item_new_with_label("Check for updates...");
	g_signal_connect(G_OBJECT(updatesW),"activate",G_CALLBACK(check_for_updates_on_click_cb),window);
	gtkosx_application_insert_app_menu_item(theApp, updatesW, 1);
	gtkosx_application_insert_app_menu_item(theApp, GTK_WIDGET(g_object_ref(gtk_separator_menu_item_new())), 2);
	gtkosx_application_insert_app_menu_item(theApp, preferencesW, 3);
  #else
	gtkosx_application_insert_app_menu_item(theApp, GTK_WIDGET(g_object_ref(gtk_separator_menu_item_new())), 1);
	gtkosx_application_insert_app_menu_item(theApp, preferencesW, 2);
  #endif
	gtkosx_application_set_help_menu(theApp, GTK_MENU_ITEM(helpitem));
	gtkosx_application_set_window_menu(theApp, GTK_MENU_ITEM(windowitem));
#else
	helpmenu = gtk_menu_new();
	helpitem = gtk_menu_item_new_with_label("Help");
	aboutW = gtk_image_menu_item_new_from_stock(GTK_STOCK_ABOUT,NULL);
	g_signal_connect(G_OBJECT(aboutW),"activate",G_CALLBACK(about_click),window);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(helpitem),helpmenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),helpitem);
	userguideW= gtk_menu_item_new_with_label("Visit XMI-MSIM User guide");
	github_rootW= gtk_menu_item_new_with_label("Visit XMI-MSIM Github repository");
	github_wikiW= gtk_menu_item_new_with_label("Visit XMI-MSIM Wiki");
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),userguideW);
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),github_rootW);
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),github_wikiW);
	g_signal_connect(G_OBJECT(userguideW),"activate",G_CALLBACK(url_click),(gpointer) "https://github.com/tschoonj/xmimsim/wiki/User-guide");
	g_signal_connect(G_OBJECT(github_rootW),"activate",G_CALLBACK(url_click),(gpointer) "https://github.com/tschoonj/xmimsim/");
	g_signal_connect(G_OBJECT(github_wikiW),"activate",G_CALLBACK(url_click),(gpointer) "https://github.com/tschoonj/xmimsim/wiki/Home");
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),(GtkWidget *) g_object_ref(gtk_separator_menu_item_new()));
	report_bugW= gtk_menu_item_new_with_label("Report a Bug");
	request_featureW= gtk_menu_item_new_with_label("Request a feature");
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),report_bugW);
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),request_featureW);
	g_signal_connect(G_OBJECT(report_bugW),"activate",G_CALLBACK(email_click), (gpointer) "Tom.Schoonjans@gmail.com?subject=XMI-MSIM%20bug%20report");
	g_signal_connect(G_OBJECT(request_featureW),"activate",G_CALLBACK(email_click), (gpointer) "Tom.Schoonjans@gmail.com?subject=XMI-MSIM%20feature%20request");
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),(GtkWidget *) g_object_ref(gtk_separator_menu_item_new()));
  #ifdef XMIMSIM_GUI_UPDATER_H
	updatesW = gtk_menu_item_new_with_label("Check for updates...");
	g_signal_connect(G_OBJECT(updatesW),"activate",G_CALLBACK(check_for_updates_on_click_cb),window);
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),updatesW);
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),(GtkWidget *) g_object_ref(gtk_separator_menu_item_new()));
  #endif
	gtk_menu_shell_append(GTK_MENU_SHELL(helpmenu),aboutW);

	gtk_widget_add_accelerator(quitW, "activate", accel_group, GDK_KEY_q, PRIMARY_ACCEL_KEY, GTK_ACCEL_VISIBLE);
	gtk_box_pack_start(GTK_BOX(Main_vbox), menubar, FALSE, FALSE, 3);
	gtk_widget_show_all(menubar);
#endif

	//toolbar
	toolbar = gtk_toolbar_new();
	newT = gtk_tool_button_new_from_stock(GTK_STOCK_NEW);
	gtk_tool_item_set_tooltip_text(newT, "New XMSI file");
	openT = gtk_menu_tool_button_new_from_stock(GTK_STOCK_OPEN);
	gtk_tool_item_set_tooltip_text(openT, "Open file");
	GtkWidget *openrecentT = gtk_recent_chooser_menu_new();
	gtk_recent_chooser_add_filter(GTK_RECENT_CHOOSER(openrecentT), filter);
	gtk_recent_chooser_set_show_tips(GTK_RECENT_CHOOSER(openrecentT), TRUE);
#ifdef G_OS_WIN32
	gtk_recent_chooser_set_show_icons(GTK_RECENT_CHOOSER(openrecentT), FALSE);
#else
	gtk_recent_chooser_set_show_icons(GTK_RECENT_CHOOSER(openrecentT), TRUE);
#endif
	gtk_recent_chooser_set_sort_type(GTK_RECENT_CHOOSER(openrecentT), GTK_RECENT_SORT_MRU);
	g_signal_connect(G_OBJECT(openrecentT), "item-activated", G_CALLBACK(chooser_activated_cb), (gpointer) window);
	gtk_menu_tool_button_set_menu(GTK_MENU_TOOL_BUTTON(openT), openrecentT);

	saveasT = gtk_tool_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_tool_item_set_tooltip_text(saveasT, "Save XMSI file as");
	saveT = gtk_tool_button_new_from_stock(GTK_STOCK_SAVE);
	gtk_tool_item_set_tooltip_text(saveT, "Save XMSI file");
	undoT = gtk_tool_button_new_from_stock(GTK_STOCK_UNDO);
	redoT = gtk_tool_button_new_from_stock(GTK_STOCK_REDO);
	cutT = gtk_tool_button_new_from_stock(GTK_STOCK_CUT);
	copyT = gtk_tool_button_new_from_stock(GTK_STOCK_COPY);
	pasteT = gtk_tool_button_new_from_stock(GTK_STOCK_PASTE);
	preferencesT = gtk_tool_button_new_from_stock(GTK_STOCK_PREFERENCES);
	gtk_tool_item_set_tooltip_text(preferencesT, "Preferences");
	tube_ebelT = gtk_tool_button_new_from_stock(XMI_STOCK_RADIATION_WARNING);
	gtk_tool_item_set_tooltip_text(tube_ebelT, "X-ray sources");
	batchmodeT = gtk_tool_button_new_from_stock(GTK_STOCK_DND_MULTIPLE);
	gtk_tool_item_set_tooltip_text(batchmodeT, "Batch mode");
	gtk_tool_button_set_label(GTK_TOOL_BUTTON(batchmodeT), "Batch mode");
	gtk_widget_set_can_focus(GTK_WIDGET(newT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(openT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(saveasT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(saveT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(undoT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(redoT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(cutT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(copyT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(pasteT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(preferencesT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(tube_ebelT),FALSE);
	gtk_widget_set_can_focus(GTK_WIDGET(batchmodeT),FALSE);
	int tbcounter = 0;
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), newT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), openT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), saveasT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), saveT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), undoT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), redoT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), cutT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), copyT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), pasteT,(gint) tbcounter++);
	GtkToolItem *separatorT = gtk_separator_tool_item_new();
	gtk_separator_tool_item_set_draw(GTK_SEPARATOR_TOOL_ITEM(separatorT), FALSE);
	gtk_tool_item_set_expand(separatorT, TRUE);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), separatorT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), batchmodeT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), tube_ebelT,(gint) tbcounter++);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), preferencesT,(gint) tbcounter++);
	gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
	//let's assume nothing is in focus at startup
	gtk_widget_set_sensitive(GTK_WIDGET(cutT),FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(copyT),FALSE);
	//to be set based on current clipboard contents
	gtk_widget_set_sensitive(GTK_WIDGET(pasteT),FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
	gtk_widget_set_sensitive(GTK_WIDGET(saveasT),FALSE);
	g_signal_connect(G_OBJECT(undoT),"clicked",G_CALLBACK(undo_menu_click),NULL);
	gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	g_signal_connect(G_OBJECT(redoT),"clicked",G_CALLBACK(redo_menu_click),NULL);
	g_signal_connect(G_OBJECT(openT),"clicked",G_CALLBACK(load_from_file_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(saveasT),"clicked",G_CALLBACK(saveas_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(saveT),"clicked",G_CALLBACK(save_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(newT),"clicked",G_CALLBACK(new_cb),(gpointer) window);
	g_signal_connect(G_OBJECT(preferencesT),"clicked",G_CALLBACK(xmimsim_gui_launch_preferences), (gpointer) window);
	g_signal_connect(G_OBJECT(tube_ebelT),"clicked",G_CALLBACK(xray_sources_button_clicked_cb), (gpointer) window);
	g_signal_connect(G_OBJECT(batchmodeT),"clicked",G_CALLBACK(batchmode_button_clicked_cb), (gpointer) window);
	g_signal_connect(G_OBJECT(cutT),"clicked",G_CALLBACK(cut_button_clicked_cb), (gpointer) window);
	g_signal_connect(G_OBJECT(copyT),"clicked",G_CALLBACK(copy_button_clicked_cb), (gpointer) window);
	g_signal_connect(G_OBJECT(pasteT),"clicked",G_CALLBACK(paste_button_clicked_cb), (gpointer) window);

	gtk_box_pack_start(GTK_BOX(Main_vbox), toolbar, FALSE, FALSE, 3);
	gtk_widget_show_all(toolbar);


	g_signal_connect(window,"delete-event",G_CALLBACK(delete_event),window);

	//notebook
	notebook = gtk_notebook_new();
	notebookG = g_signal_connect(G_OBJECT(notebook), "switch-page",G_CALLBACK(notebook_page_changed_cb),window);
	g_signal_handler_block(G_OBJECT(notebook), notebookG);
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);
	gtk_widget_show(notebook);

	frame = gtk_frame_new("General");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
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
	outputfileW = text;
	gtk_entry_set_text(GTK_ENTRY(text),current->xi->general->outputfile);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,TRUE,TRUE,0);
	//n_photons_interval
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of photons per interval");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	n_photons_intervalW = text;
	buffer = g_strdup_printf("%li",current->xi->general->n_photons_interval);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = N_PHOTONS_INTERVAL;
	vc->check = &n_photons_intervalC;
	n_photons_intervalG = g_signal_connect(G_OBJECT(text),"changed",G_CALLBACK(pos_int_changed), (gpointer) vc);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	//g_signal_connect(G_OBJECT(text), "notify::cursor-position", G_CALLBACK(entry_notify_cursor_position_cb), NULL);
	//g_signal_connect(G_OBJECT(text), "focus-out-event", G_CALLBACK(entry_focus_out_cb), NULL);
	//g_signal_connect(G_OBJECT(text), "focus-in-event", G_CALLBACK(entry_focus_in_cb), NULL);
	enable_entry_signals(text);
	//n_photons_line
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of photons per discrete line");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	n_photons_lineW = text;
	buffer = g_strdup_printf("%li",current->xi->general->n_photons_line);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = N_PHOTONS_LINE;
	vc->check = &n_photons_lineC;
	n_photons_lineG = g_signal_connect(G_OBJECT(text),"changed",G_CALLBACK(pos_int_changed), (gpointer) vc);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	enable_entry_signals(text);
	//n_interactions_trajectory
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of interactions per trajectory");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	n_interactions_trajectoryW = text;
	buffer = g_strdup_printf("%i",current->xi->general->n_interactions_trajectory);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = N_INTERACTIONS_TRAJECTORY;
	vc->check = &n_interactions_trajectoryC;
	n_interactions_trajectoryG = g_signal_connect(G_OBJECT(text),"changed",G_CALLBACK(pos_int_changed), (gpointer) vc);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	enable_entry_signals(text);

	//comments
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Comments");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	commentsW = gtk_text_view_new();
	gtk_container_set_border_width(GTK_CONTAINER(commentsW),2);
	g_signal_connect(G_OBJECT(commentsW), "focus-out-event", G_CALLBACK(comments_focus_out_cb), NULL);
	g_signal_connect(G_OBJECT(commentsW), "focus-in-event", G_CALLBACK(comments_focus_in_cb), NULL);

	commentsBuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (commentsW));
	g_signal_connect(G_OBJECT(commentsBuffer), "notify::has-selection", G_CALLBACK(comments_buffer_notify_has_selection_cb), NULL);
	//comments_beginG = g_signal_connect(G_OBJECT(commentsBuffer), "begin-user-action", G_CALLBACK(comments_begin), NULL);
	//comments_endG = g_signal_connect(G_OBJECT(commentsBuffer), "end-user-action", G_CALLBACK(comments_end), NULL);
	//gtk_text_buffer_get_bounds(commentsBuffer,&tib,&tie)
	commentsG = g_signal_connect(G_OBJECT(commentsBuffer), "changed", G_CALLBACK(comments_changed), NULL);
	gtk_text_buffer_set_text(commentsBuffer,current->xi->general->comments,-1);
	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), commentsW);
	gtk_widget_set_size_request(scrolled_window,700,100);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),scrolled_window,TRUE,TRUE,0);

	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);




	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), superframe);


	label = gtk_label_new("Input parameters");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Input parameters</span>");
	input_page = gtk_notebook_append_page(GTK_NOTEBOOK(notebook), scrolled_window, label);
	gtk_container_child_set(GTK_CONTAINER(notebook), scrolled_window, "tab-expand", TRUE, "tab-fill", TRUE, NULL);
	current_page = (gint) input_page;
	gtk_box_pack_start(GTK_BOX(Main_vbox), notebook, TRUE, TRUE, 3);
	gtk_widget_show_all(notebook);
	gtk_widget_grab_focus(label);


	//composition
	tempW = initialize_matrix(current->xi->composition, COMPOSITION);
	gtk_container_set_border_width(GTK_CONTAINER(tempW), 10);

	frame = gtk_frame_new("Composition");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Composition</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_container_add(GTK_CONTAINER(frame),tempW);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);


	//geometry
	//d_sample_source
	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook), 5);
	hbox_text_label = gtk_hbox_new(FALSE,5);
	label = gtk_label_new("Sample-source distance (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	d_sample_sourceW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->d_sample_source);
	gtk_entry_set_text(GTK_ENTRY(d_sample_sourceW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = D_SAMPLE_SOURCE;
	vc->check = &d_sample_sourceC;
	d_sample_sourceG = g_signal_connect(G_OBJECT(d_sample_sourceW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label), d_sample_sourceW, FALSE, FALSE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	d_sample_source_ebW = gtk_event_box_new();
	gtk_container_add(GTK_CONTAINER(d_sample_source_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), d_sample_source_ebW, TRUE, FALSE, 3);
	enable_entry_signals(d_sample_sourceW);



	//n_sample_orientation
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	n_sample_orientation_ebW = gtk_event_box_new();
	gtk_container_add(GTK_CONTAINER(n_sample_orientation_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), n_sample_orientation_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Sample orientation vector");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_sample_orientation_zW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->n_sample_orientation[2]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_zW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_sample_orientation_zW),10);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = N_SAMPLE_ORIENTATION_Z;
	vc->check = &n_sample_orientation_zC;
	n_sample_orientation_zG = g_signal_connect(G_OBJECT(n_sample_orientation_zW),"changed",G_CALLBACK(double_changed), (gpointer) vc);
	enable_entry_signals(n_sample_orientation_zW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_sample_orientation_zW, FALSE, FALSE, 0);
	label = gtk_label_new("z:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_sample_orientation_yW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->n_sample_orientation[1]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_yW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_sample_orientation_yW),10);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = N_SAMPLE_ORIENTATION_Y;
	vc->check = &n_sample_orientation_yC;
	n_sample_orientation_yG = g_signal_connect(G_OBJECT(n_sample_orientation_yW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(n_sample_orientation_yW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_sample_orientation_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_sample_orientation_xW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->n_sample_orientation[0]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_xW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_sample_orientation_xW),10);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = N_SAMPLE_ORIENTATION_X;
	vc->check = &n_sample_orientation_xC;
	n_sample_orientation_xG = g_signal_connect(G_OBJECT(n_sample_orientation_xW),"changed",G_CALLBACK(double_changed),(gpointer) vc  );
	enable_entry_signals(n_sample_orientation_xW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_sample_orientation_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	//p_detector_window
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	p_detector_window_ebW = gtk_event_box_new();
	gtk_container_add(GTK_CONTAINER(p_detector_window_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), p_detector_window_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Detector window position (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	p_detector_window_zW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->p_detector_window[2]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_zW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(p_detector_window_zW),10);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = P_DETECTOR_WINDOW_Z;
	vc->check = &p_detector_window_zC;
	p_detector_window_zG = g_signal_connect(G_OBJECT(p_detector_window_zW),"changed",G_CALLBACK(double_changed), vc  );
	enable_entry_signals(p_detector_window_zW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), p_detector_window_zW, FALSE, FALSE, 0);
	label = gtk_label_new("z:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	p_detector_window_yW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->p_detector_window[1]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_yW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(p_detector_window_yW),10);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = P_DETECTOR_WINDOW_Y;
	vc->check = &p_detector_window_yC;
	p_detector_window_yG = g_signal_connect(G_OBJECT(p_detector_window_yW),"changed",G_CALLBACK(double_changed), (gpointer) vc);
	enable_entry_signals(p_detector_window_yW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), p_detector_window_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	p_detector_window_xW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->p_detector_window[0]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_xW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(p_detector_window_xW),10);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = P_DETECTOR_WINDOW_X;
	vc->check = &p_detector_window_xC;
	p_detector_window_xG = g_signal_connect(G_OBJECT(p_detector_window_xW),"changed",G_CALLBACK(double_changed), (gpointer) vc);
	enable_entry_signals(p_detector_window_xW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), p_detector_window_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	//n_detector_orientation
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	n_detector_orientation_ebW = gtk_event_box_new();
	gtk_container_add(GTK_CONTAINER(n_detector_orientation_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), n_detector_orientation_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Detector window normal vector");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_detector_orientation_zW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->n_detector_orientation[2]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_zW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_detector_orientation_zW),10);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = N_DETECTOR_ORIENTATION_Z;
	vc->check = &n_detector_orientation_zC ;
	n_detector_orientation_zG = g_signal_connect(G_OBJECT(n_detector_orientation_zW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(n_detector_orientation_zW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_detector_orientation_zW, FALSE, FALSE, 0);
	label = gtk_label_new("z:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_detector_orientation_yW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->n_detector_orientation[1]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_yW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_detector_orientation_yW),10);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = N_DETECTOR_ORIENTATION_Y;
	vc->check = &n_detector_orientation_yC ;
	n_detector_orientation_yG = g_signal_connect(G_OBJECT(n_detector_orientation_yW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(n_detector_orientation_yW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_detector_orientation_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	n_detector_orientation_xW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->n_detector_orientation[0]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_xW),buffer);
	gtk_entry_set_width_chars(GTK_ENTRY(n_detector_orientation_xW),10);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = N_DETECTOR_ORIENTATION_X;
	vc->check = &n_detector_orientation_xC ;
	n_detector_orientation_xG = g_signal_connect(G_OBJECT(n_detector_orientation_xW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(n_detector_orientation_xW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), n_detector_orientation_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	//area detector
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	area_detector_ebW = gtk_event_box_new();
	gtk_container_add(GTK_CONTAINER(area_detector_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), area_detector_ebW, TRUE, FALSE, 3);
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label),"Active detector area (cm<sup>2</sup>)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	area_detectorW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->area_detector);
	gtk_entry_set_text(GTK_ENTRY(area_detectorW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = AREA_DETECTOR;
	vc->check = &area_detectorC;
	area_detectorG = g_signal_connect(G_OBJECT(area_detectorW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(area_detectorW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), area_detectorW, FALSE, FALSE, 0);

	//collimator_height
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	collimator_height_ebW = gtk_event_box_new();
	gtk_container_add(GTK_CONTAINER(collimator_height_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), collimator_height_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Collimator height (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	collimator_heightW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->collimator_height);
	gtk_entry_set_text(GTK_ENTRY(collimator_heightW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = COLLIMATOR_HEIGHT;
	vc->check = &collimator_heightC ;
	collimator_heightG = g_signal_connect(G_OBJECT(collimator_heightW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(collimator_heightW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), collimator_heightW, FALSE, FALSE, 0);

	//collimator_diameter
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	collimator_diameter_ebW = gtk_event_box_new();
	gtk_container_add(GTK_CONTAINER(collimator_diameter_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), collimator_diameter_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Collimator diameter (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	collimator_diameterW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->collimator_diameter);
	gtk_entry_set_text(GTK_ENTRY(collimator_diameterW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = COLLIMATOR_DIAMETER;
	vc->check = &collimator_diameterC ;
	collimator_diameterG = g_signal_connect(G_OBJECT(collimator_diameterW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(collimator_diameterW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), collimator_diameterW, FALSE, FALSE, 0);

	//d_source_slit
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	d_source_slit_ebW = gtk_event_box_new();
	gtk_container_add(GTK_CONTAINER(d_source_slit_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), d_source_slit_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Source-slits distance (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	d_source_slitW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->d_source_slit);
	gtk_entry_set_text(GTK_ENTRY(d_source_slitW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = D_SOURCE_SLIT;
	vc->check = &d_source_slitC ;
	d_source_slitG = g_signal_connect(G_OBJECT(d_source_slitW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(d_source_slitW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), d_source_slitW, FALSE, FALSE, 0);

	//slit sizes
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	slit_size_ebW = gtk_event_box_new();
	gtk_container_add(GTK_CONTAINER(slit_size_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), slit_size_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Slits size (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	slit_size_yW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->slit_size_y);
	gtk_entry_set_text(GTK_ENTRY(slit_size_yW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = SLIT_SIZE_Y;
	vc->check = &slit_size_yC;
	slit_size_yG = g_signal_connect(G_OBJECT(slit_size_yW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(slit_size_yW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), slit_size_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	slit_size_xW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->geometry->slit_size_x);
	gtk_entry_set_text(GTK_ENTRY(slit_size_xW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = SLIT_SIZE_X;
	vc->check = &slit_size_xC;
	slit_size_xG = g_signal_connect(G_OBJECT(slit_size_xW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(slit_size_xW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), slit_size_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	geometry_helpW = gtk_toggle_button_new_with_label("Show geometry help");
	hbox_text_label = gtk_hbox_new(TRUE,5);
	gtk_box_pack_start(GTK_BOX(hbox_text_label), geometry_helpW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	geometry_helpG = g_signal_connect_swapped(G_OBJECT(geometry_helpW),"toggled",G_CALLBACK(geometry_help_clicked_cb), (gpointer) window);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(geometry_helpW), FALSE);

	frame = gtk_frame_new("Geometry");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Geometry</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);

	//energies

	frame = gtk_frame_new("Energies");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Excitation</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	gtk_container_add(GTK_CONTAINER(frame), initialize_energies(current->xi->excitation, window, &discWidget, &contWidget));
	gtk_box_pack_start(GTK_BOX(superframe), frame, FALSE, FALSE, 5);


	//absorbers
	//convert to composition struct
	xmi_copy_abs_or_crystal2composition(current->xi->absorbers->exc_layers, current->xi->absorbers->n_exc_layers   ,&temp_composition)	;
	tempW = initialize_matrix(temp_composition  , EXC_COMPOSITION);
	gtk_container_set_border_width(GTK_CONTAINER(tempW), 10);


	frame = gtk_frame_new("Beam absorbers");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Beam absorbers</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_container_add(GTK_CONTAINER(frame),tempW);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);


	xmi_free_composition(temp_composition);
	xmi_copy_abs_or_crystal2composition(current->xi->absorbers->det_layers, current->xi->absorbers->n_det_layers   ,&temp_composition);
	tempW = initialize_matrix(temp_composition  , DET_COMPOSITION);
	gtk_container_set_border_width(GTK_CONTAINER(tempW), 10);


	frame = gtk_frame_new("Detection absorbers");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
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
	detector_typeW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(detector_typeW),"Si(Li)");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(detector_typeW),"Ge");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(detector_typeW),"SDD");
	gtk_combo_box_set_active(GTK_COMBO_BOX(detector_typeW),current->xi->detector->detector_type);
	detector_typeG = g_signal_connect(G_OBJECT(detector_typeW),"changed",G_CALLBACK(detector_type_changed), GINT_TO_POINTER(DETECTOR_TYPE));
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_typeW, FALSE, FALSE, 0);

	//channels
	GtkAdjustment *spinner_adj = GTK_ADJUSTMENT(gtk_adjustment_new(2048.0, 10.0, 100000.0, 1.0, 10.0, 0.0));
	detector_nchannelsW = gtk_spin_button_new(spinner_adj, 1, 0);
	detector_nchannelsG = g_signal_connect(G_OBJECT(detector_nchannelsW), "value-changed", G_CALLBACK(detector_nchannels_changed), GINT_TO_POINTER(DETECTOR_NCHANNELS));
	gtk_editable_set_editable(GTK_EDITABLE(detector_nchannelsW), TRUE);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(detector_nchannelsW), GTK_UPDATE_IF_VALID);
	gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(detector_nchannelsW), TRUE);
	gtk_entry_set_max_length(GTK_ENTRY(detector_nchannelsW), 7);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(detector_nchannelsW), (gdouble) current->xi->detector->nchannels);
	hbox_text_label = gtk_hbox_new(FALSE, 5);
	label = gtk_label_new("Number of spectrum channels");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_nchannelsW, FALSE, FALSE, 0);
	enable_entry_signals(detector_nchannelsW);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);

	//gain
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector gain (keV/channel)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_gainW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->detector->gain);
	gtk_entry_set_text(GTK_ENTRY(detector_gainW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_GAIN;
	vc->check = &detector_gainC;
	detector_gainG = g_signal_connect(G_OBJECT(detector_gainW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(detector_gainW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_gainW, FALSE, FALSE, 0);

	//zero
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector zero (keV)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_zeroW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->detector->zero);
	gtk_entry_set_text(GTK_ENTRY(detector_zeroW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_ZERO;
	vc->check = &detector_zeroC;
	detector_zeroG = g_signal_connect(G_OBJECT(detector_zeroW),"changed",G_CALLBACK(double_changed), (gpointer) vc);
	enable_entry_signals(detector_zeroW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_zeroW, FALSE, FALSE, 0);

	//fano
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector Fano factor");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_fanoW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->detector->fano);
	gtk_entry_set_text(GTK_ENTRY(detector_fanoW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_FANO;
	vc->check = &detector_fanoC;
	detector_fanoG = g_signal_connect(G_OBJECT(detector_fanoW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(detector_fanoW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_fanoW, FALSE, FALSE, 0);

	//noise
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector electronic noise (keV)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_noiseW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->detector->noise);
	gtk_entry_set_text(GTK_ENTRY(detector_noiseW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_NOISE;
	vc->check = &detector_noiseC;
	detector_noiseG = g_signal_connect(G_OBJECT(detector_noiseW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(detector_noiseW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_noiseW, FALSE, FALSE, 0);

	//live time
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Live time (s)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_live_timeW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->detector->live_time);
	gtk_entry_set_text(GTK_ENTRY(detector_live_timeW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_LIVE_TIME;
	vc->check = &detector_live_timeC;
	detector_live_timeG = g_signal_connect(G_OBJECT(detector_live_timeW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(detector_live_timeW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_live_timeW, FALSE, FALSE, 0);

	//pulse_width
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Pulse width (s)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	detector_pulse_widthW = gtk_entry_new();
	buffer = g_strdup_printf("%lg",current->xi->detector->pulse_width);
	gtk_entry_set_text(GTK_ENTRY(detector_pulse_widthW),buffer);
	vc = (struct val_changed *) g_malloc(sizeof(struct val_changed));
	vc->kind = DETECTOR_PULSE_WIDTH;
	vc->check = &detector_pulse_widthC;
	detector_pulse_widthG = g_signal_connect(G_OBJECT(detector_pulse_widthW),"changed",G_CALLBACK(double_changed), (gpointer) vc  );
	enable_entry_signals(detector_pulse_widthW);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), detector_pulse_widthW, FALSE, FALSE, 0);

	//crystal
	xmi_copy_abs_or_crystal2composition(current->xi->detector->crystal_layers, current->xi->detector->n_crystal_layers   ,&temp_composition);
	tempW = initialize_matrix(temp_composition  , CRYSTAL_COMPOSITION);

	gtk_box_pack_start(GTK_BOX(vbox_notebook), tempW, TRUE, FALSE, 3);
	xmi_free_composition(temp_composition);

	frame = gtk_frame_new("Detector settings");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
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
	gtk_container_child_set(GTK_CONTAINER(notebook), controlsPageW, "tab-expand", TRUE, "tab-fill", TRUE, NULL);
	gtk_widget_show_all(controlsPageW);

	//third notebook page: Results
	label = gtk_label_new("Results");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Results</span>");
	resultsPageW = init_results(window);
	results_page = gtk_notebook_append_page(GTK_NOTEBOOK(notebook), resultsPageW, label);
	gtk_container_child_set(GTK_CONTAINER(notebook), resultsPageW, "tab-expand", TRUE, "tab-fill", TRUE, NULL);
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
	gtkosx_application_ready(theApp);
	//only works in Lion and newer
	NSWindow *qwindow = gdk_quartz_window_get_nswindow(gtk_widget_get_window(window));
	[qwindow setCollectionBehavior:NSWindowCollectionBehaviorFullScreenPrimary];
#endif

	struct xmi_input *xi;
	GtkWidget *dialog;
	if (argc == 2) {
		gchar *filename = g_strdup(argv[1]);
		if (g_ascii_strcasecmp(filename+strlen(filename)-5,".xmsi") == 0) {
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
			adjust_save_buttons();
		}
		else if (g_ascii_strcasecmp(filename+strlen(filename)-5,".xmso") == 0) {
			update_xmimsim_title_xmsi("New file", window, NULL);
			update_xmimsim_title_xmso("No simulation data available", window, NULL);
			//XMSO file
			gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),results_page);
			if (plot_spectra_from_file(filename) == 1) {
				gchar *temp_base = g_path_get_basename(filename);
				update_xmimsim_title_xmso(temp_base, window, filename);
				g_free(temp_base);
			}
			else {
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
		else if (g_ascii_strcasecmp(filename+strlen(filename)-5,".xmsa") == 0) {
			update_xmimsim_title_xmsi("New file", window, NULL);
			update_xmimsim_title_xmso("No simulation data available", window, NULL);
			//have to add busy readin XMSA dialog here, through custom g_idle_add callback
			struct dialog_helper_xmsa_data *data = (struct dialog_helper_xmsa_data *) g_malloc(sizeof(struct dialog_helper_xmsa_data));
			data->window = window;
			data->filename = filename;
			g_idle_add((GSourceFunc) dialog_helper_xmsa_cb, (gpointer) data);
		}
		else {
			update_xmimsim_title_xmsi("New file", window, NULL);
			update_xmimsim_title_xmso("No simulation data available", window, NULL);
			dialog = gtk_message_dialog_new (GTK_WINDOW(window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
			      	GTK_BUTTONS_CLOSE,
				"Could not read file %s\nExtension must be either xmsi, xmso or xmsa.", filename
	               	);
			g_idle_add(dialog_helper_cb,(gpointer) dialog);
		}
	}
	else {
		update_xmimsim_title_xmsi("New file", window, NULL);
		update_xmimsim_title_xmso("No simulation data available", window, NULL);
	}

	//add warning message in case we are dealing with a development version
	//get version from config.h (I tend to forget updating xmi_msim.h...
	double package_version = strtod(PACKAGE_VERSION, NULL);
	if (package_version - floor(package_version) > 0.0) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
			GTK_MESSAGE_WARNING,
			GTK_BUTTONS_CLOSE,
			"You are running a development version of XMI-MSIM."
	        );
		gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(dialog), "Please report bugs to <span><a href=\"mailto:%s\">%s</a></span>", PACKAGE_BUGREPORT, PACKAGE_BUGREPORT);
		g_idle_add(dialog_helper_cb,(gpointer) dialog);
	}


#ifdef XMIMSIM_GUI_UPDATER_H
	g_idle_add((GSourceFunc) check_for_updates_on_init_cb, window);
#endif

	// import sources
	g_idle_add((GSourceFunc) query_source_modules, NULL);

	gtk_widget_grab_focus(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook),input_page));


	xmimsim_notifications_init();
	add_to_control_widgets(superframe);
	add_to_control_widgets(GTK_WIDGET(tube_ebelT));
	add_to_control_widgets(tube_ebelW);

	gtk_main();

#ifdef MAC_INTEGRATION
	g_object_unref(theApp);
#endif

	xmimsim_notifications_close();


	return 0;
}

static void change_all_values(struct xmi_input *new_input) {
	change_all_values_general(new_input);
	change_all_values_composition(new_input);
	change_all_values_geometry(new_input);
	change_all_values_excitation(new_input);
	change_all_values_beamabsorbers(new_input);
	change_all_values_detectionabsorbers(new_input);
	change_all_values_detectorsettings(new_input);

	return;
}

static void cut_button_clicked_cb(GtkWidget *widget, gpointer data) {
	//three possibilities:
	//1) textview
	//2) entry
	//3) treeview


	GtkWidget *focused = gtk_window_get_focus(GTK_WINDOW(data));

	if (GTK_IS_TEXT_VIEW(focused)) {
		g_signal_emit_by_name(G_OBJECT(focused), "cut-clipboard", NULL);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}
	else if (GTK_IS_ENTRY(focused)) {
		g_signal_emit_by_name(G_OBJECT(focused), "cut-clipboard", NULL);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}
	else if (GTK_IS_TREE_VIEW(focused)) {
		struct matrix_button *mb = (struct matrix_button *) g_malloc(sizeof(struct matrix_button));
		mb->select= gtk_tree_view_get_selection(GTK_TREE_VIEW(focused));
		if (focused == compositionW) {
			mb->matrixKind = COMPOSITION;
			mb->store = compositionL;
		}
		else if (focused == exc_compositionW) {
			mb->matrixKind = EXC_COMPOSITION;
			mb->store = exc_compositionL;
		}
		else if (focused == det_compositionW) {
			mb->matrixKind = DET_COMPOSITION;
			mb->store = det_compositionL;
		}
		else if (focused == crystal_compositionW) {
			mb->matrixKind = CRYSTAL_COMPOSITION;
			mb->store = crystal_compositionL;
		}
		else {
			g_fprintf(stderr, "Could not match tree_view widget!\n");
			return;
		}

		layer_cut_cb(NULL, mb);
	}
	else {
		g_fprintf(stderr, "Invalid widget type detected in cut_button_clicked_cb\n!");
		return;
	}
}
static void copy_button_clicked_cb(GtkWidget *widget, gpointer data) {
	//three possibilities:
	//1) textview
	//2) entry
	//3) treeview


	GtkWidget *focused = gtk_window_get_focus(GTK_WINDOW(data));

	if (GTK_IS_TEXT_VIEW(focused)) {
		g_signal_emit_by_name(G_OBJECT(focused), "copy-clipboard", NULL);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}
	else if (GTK_IS_ENTRY(focused)) {
		g_signal_emit_by_name(G_OBJECT(focused), "copy-clipboard", NULL);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}
	else if (GTK_IS_TREE_VIEW(focused)) {
		struct matrix_button *mb = (struct matrix_button *) g_malloc(sizeof(struct matrix_button));
		mb->select= gtk_tree_view_get_selection(GTK_TREE_VIEW(focused));
		if (focused == compositionW) {
			mb->matrixKind = COMPOSITION;
		}
		else if (focused == exc_compositionW) {
			mb->matrixKind = EXC_COMPOSITION;
		}
		else if (focused == det_compositionW) {
			mb->matrixKind = DET_COMPOSITION;
		}
		else if (focused == crystal_compositionW) {
			mb->matrixKind = CRYSTAL_COMPOSITION;
		}
		else {
			g_fprintf(stderr, "Could not match tree_view widget!\n");
			return;
		}

		layer_copy_cb(NULL, mb);
	}
	else {
		g_fprintf(stderr, "Invalid widget type detected in copy_button_clicked_cb\n!");
		return;
	}

}

static void paste_button_clicked_cb(GtkWidget *widget, gpointer data) {

	//three possibilities:
	//1) textview
	//2) entry
	//3) treeview


	GtkWidget *focused = gtk_window_get_focus(GTK_WINDOW(data));
	GtkClipboard *clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);

	if (!clipboard)
		return;

	if (GTK_IS_TEXT_VIEW(focused) && gtk_clipboard_wait_is_text_available(clipboard)) {
		g_signal_emit_by_name(G_OBJECT(focused), "paste-clipboard", NULL);
	}
	else if (GTK_IS_ENTRY(focused) && gtk_clipboard_wait_is_text_available(clipboard)) {
		g_signal_emit_by_name(G_OBJECT(focused), "paste-clipboard", NULL);
	}
	else if (GTK_IS_TREE_VIEW(focused) && gtk_clipboard_wait_is_target_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD), LayerAtom)) {
		struct matrix_button *mb = (struct matrix_button *) g_malloc(sizeof(struct matrix_button));
		mb->select= gtk_tree_view_get_selection(GTK_TREE_VIEW(focused));
		if (focused == compositionW) {
			mb->matrixKind = COMPOSITION;
		}
		else if (focused == exc_compositionW) {
			mb->matrixKind = EXC_COMPOSITION;
		}
		else if (focused == det_compositionW) {
			mb->matrixKind = DET_COMPOSITION;
		}
		else if (focused == crystal_compositionW) {
			mb->matrixKind = CRYSTAL_COMPOSITION;
		}
		else {
			g_fprintf(stderr, "Could not match tree_view widget!\n");
			return;
		}

		layer_paste_cb(NULL, mb);
	}
	else {
		g_fprintf(stderr, "Invalid widget type detected in paste_button_clicked_cb\n!");
		return;
	}

}

static void import_cb(GtkWidget *widget, gpointer data) {
	//launch file selection dialog
	GtkWidget *dialog = NULL;
	GtkFileFilter *filter;
	gchar *filename;
	struct xmi_input *xi = NULL;
	struct xmi_output *xo = NULL;


	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.[xX][mM][sS][iI]");
	gtk_file_filter_add_pattern(filter,"*.[xX][mM][sS][oO]");
	gtk_file_filter_set_name(filter,"XMI-MSIM input and output files");
	dialog = gtk_file_chooser_dialog_new ("Select a XMI-MSIM file to import from",
		GTK_WINDOW((GtkWidget *) data),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
		NULL);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
		//open file based on extension
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		if (g_ascii_strcasecmp(filename+strlen(filename)-5, ".xmsi") == 0) {
			//input-file found
			if (xmi_read_input_xml(filename, &xi) == 0) {
				xi = NULL;
			}
		}
		else if (g_ascii_strcasecmp(filename+strlen(filename)-5, ".xmso") == 0) {
			//output-file found
			if (xmi_read_output_xml(filename, &xo) == 0) {
				xo = NULL;
				xi = NULL;
			}
			else {
				xi = xo->input;
			}
		}
		else {
			g_fprintf(stderr,"Unsupported filetype selected. This error should never appear\n");
			exit(1);
		}
		gtk_widget_destroy(dialog);
		if (xi == NULL) {
			dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
				GTK_DIALOG_DESTROY_WITH_PARENT,
		       		GTK_MESSAGE_ERROR,
		       		GTK_BUTTONS_CLOSE,
		       		"Could not read file %s: model is incomplete/invalid",filename
	               	);
	     		gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		//ok... spawn dialogue with components selection
		dialog = gtk_dialog_new_with_buttons("Import components", GTK_WINDOW((GtkWidget *) data), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
		gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

		GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
		GtkWidget *vbox = gtk_vbox_new(FALSE, 2);
		GtkWidget *composition_check = gtk_check_button_new_with_label("Composition");
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(composition_check), TRUE);
		gtk_box_pack_start(GTK_BOX(vbox), composition_check, FALSE, FALSE, 0);
		GtkWidget *geometry_check = gtk_check_button_new_with_label("Geometry");
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(geometry_check), TRUE);
		gtk_box_pack_start(GTK_BOX(vbox), geometry_check, FALSE, FALSE, 0);
		GtkWidget *excitation_check = gtk_check_button_new_with_label("Excitation");
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(excitation_check), TRUE);
		gtk_box_pack_start(GTK_BOX(vbox), excitation_check, FALSE, FALSE, 0);
		GtkWidget *beamabsorbers_check = gtk_check_button_new_with_label("Beam absorbers");
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(beamabsorbers_check), TRUE);
		gtk_box_pack_start(GTK_BOX(vbox), beamabsorbers_check, FALSE, FALSE, 0);
		GtkWidget *detectionabsorbers_check = gtk_check_button_new_with_label("Detection absorbers");
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(detectionabsorbers_check), TRUE);
		gtk_box_pack_start(GTK_BOX(vbox), detectionabsorbers_check, FALSE, FALSE, 0);
		GtkWidget *detectorsettings_check = gtk_check_button_new_with_label("Detection settings");
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(detectorsettings_check), TRUE);
		gtk_box_pack_start(GTK_BOX(vbox), detectorsettings_check, FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(content_area), vbox, FALSE, FALSE,0);
		gtk_container_set_border_width(GTK_CONTAINER(dialog),5);
		gtk_widget_show_all(vbox);
		if (gtk_dialog_run(GTK_DIALOG(dialog)) != GTK_RESPONSE_ACCEPT) {
			gtk_widget_destroy(dialog);\
			return;
		}
		//let's see what was selected
		int undo_rv = 0;


		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(composition_check)) == TRUE) {
			undo_rv |= IMPORT_SELECT_COMPOSITION;
			change_all_values_composition(xi);

		}
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(geometry_check)) == TRUE) {
			undo_rv |= IMPORT_SELECT_GEOMETRY;
			change_all_values_geometry(xi);
		}
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(excitation_check)) == TRUE) {
			undo_rv |= IMPORT_SELECT_EXCITATION;
			change_all_values_excitation(xi);
		}
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(beamabsorbers_check)) == TRUE) {
			undo_rv |= IMPORT_SELECT_BEAMABSORBERS;
			change_all_values_beamabsorbers(xi);
		}
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(detectionabsorbers_check)) == TRUE) {
			undo_rv |= IMPORT_SELECT_DETECTIONABSORBERS;
			change_all_values_detectionabsorbers(xi);
		}
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(detectorsettings_check)) == TRUE) {
			undo_rv |= IMPORT_SELECT_DETECTORSETTINGS;
			change_all_values_detectorsettings(xi);
		}
		if (undo_rv == 0) {
			//nothing selected
			gtk_widget_destroy(dialog);
			gtk_widget_grab_focus(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook),input_page));
			return;
		}
		struct import_undo_data *iud = (struct import_undo_data *) g_malloc(sizeof(struct import_undo_data));
		iud->xi = xi;
		iud->undo_rv = undo_rv;
		iud->filename = filename;
		update_undo_buffer(IMPORT_FROM_FILE, (GtkWidget *) iud);
		if (xo) {
			xmi_free_output(xo);
			xi = NULL;
		}
		if (xi) {
			xmi_free_input(xi);
		}
		g_free(iud->filename);
		g_free(iud);

	}
	gtk_widget_destroy(dialog);
	gtk_widget_grab_focus(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook),input_page));
	return;
}

static void switch_tab_click(GtkWidget *widget, gpointer data) {
	gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), GPOINTER_TO_INT(data));
}

static void new_cb(GtkWidget *widget, gpointer data) {
	struct xmi_input *xi;



	//start a new inputfile, using default settings
	if (process_pre_file_operation((GtkWidget *) data) == FALSE)
		return;


	xi = xmi_init_empty_input();
	change_all_values(xi);
	reset_undo_buffer(xi,UNLIKELY_FILENAME);

	update_xmimsim_title_xmsi("New file", (GtkWidget *) data, NULL);
	adjust_save_buttons();
	gtk_widget_grab_focus(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook),input_page));

	return;
}


int kill_current_job(void) {
	if (xmimsim_pid != GPID_INACTIVE) {
		//if UNIX -> send sigterm
		//if WIN32 -> call TerminateProcess

#ifdef G_OS_UNIX
		int kill_rv;

		g_debug("killing %i UNIX style\n", (int) xmimsim_pid);
		kill_rv = kill((pid_t) xmimsim_pid, SIGTERM);
#if !GLIB_CHECK_VERSION (2, 35, 0)
		//starting with 2.36.0 (and some unstable versions before),
		//waitpid is called from within the main loop
		//causing all kinds of trouble if I would call wait here
		//wait(NULL);
		waitpid(xmimsim_pid, NULL, WNOHANG);
#endif
		if (kill_rv == 0) {
			g_debug("Process %i was successfully terminated before completion\n",(int) xmimsim_pid);
		}
		else {
			g_debug("Process %i could not be terminated with the SIGTERM signal\n",(int) xmimsim_pid);
		}
#elif defined(G_OS_WIN32)
		BOOL terminate_rv;

		terminate_rv = TerminateProcess((HANDLE) xmimsim_pid, (UINT) 1);

		if (terminate_rv == TRUE) {
			g_debug("Process %i was successfully terminated\n", real_xmimsim_pid);
		}
		else {
			g_debug("Process %i could not be terminated with the TerminateProcess call\n", real_xmimsim_pid);
		}
#endif
		g_spawn_close_pid(xmimsim_pid);
		xmimsim_pid = GPID_INACTIVE;
		return 1;
	}
	return 0;
}
#ifdef MAC_INTEGRATION
gboolean quit_blocker_mac_cb(GtkosxApplication *app, gpointer data){

	if (process_pre_file_operation((GtkWidget *) data) == FALSE)
		return TRUE;
	return FALSE;

}



void quit_program_cb(GtkosxApplication *app, gpointer data) {
#else
void quit_program_cb(GtkWidget *widget, gpointer data) {
	if (process_pre_file_operation((GtkWidget *) data) == FALSE)
		return;
#endif

	kill_current_job();

	g_debug("quitting\n");

	gtk_main_quit();

	return;
}


void load_from_file_cb(GtkWidget *widget, gpointer data) {
	GtkWidget *dialog = NULL;
	GtkFileFilter *filter1, *filter2, *filter3;
	gchar *filename;
	struct xmi_input *xi;
	gchar *title;




	filter1 = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter1,"*.[xX][mM][sS][iI]");
	gtk_file_filter_set_name(filter1,"XMI-MSIM inputfiles");
	filter2 = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter2,"*.[xX][mM][sS][oO]");
	gtk_file_filter_set_name(filter2,"XMI-MSIM outputfiles");
	filter3 = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter3,"*.[xX][mM][sS][aA]");
	gtk_file_filter_set_name(filter3,"XMI-MSIM archives");
	dialog = gtk_file_chooser_dialog_new ("Open simulation file",
		GTK_WINDOW((GtkWidget *) data),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
		NULL);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter1);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter2);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter3);

	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

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
			if (process_pre_file_operation((GtkWidget *) data) == FALSE) {
				gtk_widget_destroy (dialog);
				return;
			}
			gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook),input_page);
			if (xmi_read_input_xml(filename, &xi) == 1) {
				//success reading it in...
				change_all_values(xi);
				//reset redo_buffer
				reset_undo_buffer(xi, filename);
				title = g_path_get_basename(filename);
				update_xmimsim_title_xmsi(title, (GtkWidget *) data, filename);
				g_free(title);
				adjust_save_buttons();
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
				update_xmimsim_title_xmso(temp_base, (GtkWidget *) data, filename);
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
		else if (gtk_file_chooser_get_filter(GTK_FILE_CHOOSER(dialog)) == filter3) {
			gtk_widget_destroy(dialog);
			struct dialog_helper_xmsa_data *mydata = (struct dialog_helper_xmsa_data *) g_malloc(sizeof(struct dialog_helper_xmsa_data));
			mydata->window = (GtkWidget *) data;
			mydata->filename = filename;
			dialog_helper_xmsa_cb(mydata);
			return;
		}
		g_free (filename);
	}

	gtk_widget_destroy (dialog);
	gtk_widget_grab_focus(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook),input_page));
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
 detector_noiseC;
}



void saveas_cb(GtkWidget *widget, gpointer data) {

	saveas_function(widget, data);

	return;
}

void save_cb(GtkWidget *widget, gpointer data) {

	save_function(widget, data);

	return;
}

gboolean save_function(GtkWidget *widget, gpointer data) {
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
		if (xmi_write_input_xml(check_rv->filename, current->xi) != 1) {
			dialog = gtk_message_dialog_new (GTK_WINDOW((GtkWidget *)data),
				GTK_DIALOG_DESTROY_WITH_PARENT,
		       		GTK_MESSAGE_ERROR,
		       		GTK_BUTTONS_CLOSE,
		       		"Could not write to file %s: file writeable?",check_rv->filename
	               	);
	     		gtk_dialog_run (GTK_DIALOG (dialog));
			gtk_widget_destroy(dialog);
			return FALSE;

		}
		else {
			filename = g_strdup(last_saved->filename);
			g_free(last_saved->filename);
			xmi_free_input(last_saved->xi);
			g_free(last_saved);
			last_saved = (struct undo_single *) g_malloc(sizeof(struct undo_single));
			xmi_copy_input(current->xi, &(last_saved->xi));
			last_saved->filename = g_strdup(filename);
			g_free(filename);
		}
		gtk_widget_set_sensitive(saveW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
	}
	else if (check_status == CHECK_CHANGES_NEVER_SAVED ||
		check_status == CHECK_CHANGES_NEW) {
		//never saved -> call saveas
		return saveas_function(widget, data);

	}
	else if (check_status == CHECK_CHANGES_JUST_SAVED) {
		//do nothing
	}

	return TRUE;
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
	gtk_file_filter_add_pattern(filter,"*.[xX][mM][sS][iI]");
	gtk_file_filter_set_name(filter,"XMI-MSIM inputfiles");
	dialog = gtk_file_chooser_dialog_new ("Save simulation inputfile",
		GTK_WINDOW((GtkWidget *) data),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
		NULL);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);

	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		xmi_msim_gui_utils_ensure_extension(&filename, ".xmsi");
		//
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


		if (xmi_write_input_xml(filename, current->xi) == 1) {
			gtk_widget_destroy (dialog);
			if (last_saved != NULL) {
				g_free(last_saved->filename);
				xmi_free_input(last_saved->xi);
				g_free(last_saved);
			}
			last_saved = (struct undo_single *) g_malloc(sizeof(struct undo_single));
			xmi_copy_input(current->xi, &(last_saved->xi));
			last_saved->filename = g_strdup(filename);
			title = g_path_get_basename(filename);
			update_xmimsim_title_xmsi(title, (GtkWidget *) data, filename);
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

void xmi_open_email(const char *address) {
	char *link;

	link = g_strdup_printf("mailto:%s",address);

#ifdef MAC_INTEGRATION
	CFURLRef url = CFURLCreateWithBytes (
      	NULL,
      	(UInt8*)link,
      	strlen(link),
      	kCFStringEncodingASCII,
      	NULL
    	);
  	LSOpenCFURLRef(url,NULL);
  	CFRelease(url);
#elif defined(G_OS_WIN32)
	ShellExecute(NULL, "open", link, NULL, NULL, SW_SHOWNORMAL);
#else
	pid_t pid;
	char * const argv[] = {(char *) "xdg-email", link, NULL};
	//argv[0] = "xdg-email";
	//argv[1] = link;
	//argv[2] = NULL;

	pid = fork();
	if (!pid)
		execvp(argv[0], argv);
#endif
	g_free(link);
	return;

}

static void change_all_values_general(struct xmi_input *new_input) {
	char *buffer;
	GtkTextBuffer *commentsBuffer;

	n_photons_intervalC = 1;
	n_photons_lineC = 1;
	n_interactions_trajectoryC = 1;

	g_signal_handler_block(G_OBJECT(n_photons_intervalW), n_photons_intervalG);
	g_signal_handler_block(G_OBJECT(n_photons_lineW), n_photons_lineG);
	g_signal_handler_block(G_OBJECT(n_interactions_trajectoryW), n_interactions_trajectoryG);
	g_signal_handler_block(G_OBJECT(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW))),commentsG);

	gtk_entry_set_text(GTK_ENTRY(outputfileW),new_input->general->outputfile);
	buffer = g_strdup_printf("%li",new_input->general->n_photons_interval);
	gtk_entry_set_text(GTK_ENTRY(n_photons_intervalW),buffer);
	buffer = g_strdup_printf("%li",new_input->general->n_photons_line);
	gtk_entry_set_text(GTK_ENTRY(n_photons_lineW),buffer);
	buffer = g_strdup_printf("%i",new_input->general->n_interactions_trajectory);
	gtk_entry_set_text(GTK_ENTRY(n_interactions_trajectoryW),buffer);
	commentsBuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (commentsW));
	gtk_text_buffer_set_text(commentsBuffer,new_input->general->comments,-1);


	g_signal_handler_unblock(G_OBJECT(n_photons_intervalW), n_photons_intervalG);
	g_signal_handler_unblock(G_OBJECT(n_photons_lineW), n_photons_lineG);
	g_signal_handler_unblock(G_OBJECT(n_interactions_trajectoryW), n_interactions_trajectoryG);
	g_signal_handler_unblock(G_OBJECT(gtk_text_view_get_buffer(GTK_TEXT_VIEW(commentsW))),commentsG);

	return;
}


static void change_all_values_composition(struct xmi_input *new_input) {
	char *elementString;
	int i;
	GtkTreeIter iter;

	gtk_list_store_clear(compositionL);
	for (i = 0 ; i < new_input->composition->n_layers ; i++) {
		gtk_list_store_append(compositionL, &iter);
		elementString = xmi_msim_gui_utils_get_layer_element_string(new_input->composition->layers + i);
		gtk_list_store_set(compositionL, &iter,
			N_ELEMENTS_COLUMN, new_input->composition->layers[i].n_elements,
			ELEMENTS_COLUMN,elementString,
			DENSITY_COLUMN, new_input->composition->layers[i].density,
			THICKNESS_COLUMN, new_input->composition->layers[i].thickness,
			REFERENCE_COLUMN,(i+1 == new_input->composition->reference_layer) ? TRUE : FALSE,
			-1
			);
		g_free(elementString);
	}
	xmi_free_composition(compositionS);
	xmi_copy_composition(new_input->composition,&compositionS);
}

static void change_all_values_geometry(struct xmi_input *new_input) {
	char *buffer;

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

	buffer = g_strdup_printf("%lg",new_input->geometry->d_sample_source);
	gtk_entry_set_text(GTK_ENTRY(d_sample_sourceW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->n_sample_orientation[0]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_xW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->n_sample_orientation[1]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_yW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->n_sample_orientation[2]);
	gtk_entry_set_text(GTK_ENTRY(n_sample_orientation_zW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->p_detector_window[0]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_xW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->p_detector_window[1]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_yW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->p_detector_window[2]);
	gtk_entry_set_text(GTK_ENTRY(p_detector_window_zW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->n_detector_orientation[0]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_xW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->n_detector_orientation[1]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_yW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->n_detector_orientation[2]);
	gtk_entry_set_text(GTK_ENTRY(n_detector_orientation_zW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->area_detector);
	gtk_entry_set_text(GTK_ENTRY(area_detectorW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->collimator_height);
	gtk_entry_set_text(GTK_ENTRY(collimator_heightW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->collimator_diameter);
	gtk_entry_set_text(GTK_ENTRY(collimator_diameterW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->d_source_slit);
	gtk_entry_set_text(GTK_ENTRY(d_source_slitW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->slit_size_x);
	gtk_entry_set_text(GTK_ENTRY(slit_size_xW),buffer);
	buffer = g_strdup_printf("%lg",new_input->geometry->slit_size_y);
	gtk_entry_set_text(GTK_ENTRY(slit_size_yW),buffer);

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
}

static void change_all_values_excitation(struct xmi_input *new_input) {
	int i;
	GtkTreeIter iter;

	repopulate_discrete_energies(discWidget->store, (new_input)->excitation);
	repopulate_continuous_energies(contWidget->store, (new_input)->excitation);
}

static void change_all_values_beamabsorbers(struct xmi_input *new_input) {
	char *elementString;
	int i;
	GtkTreeIter iter;

	gtk_list_store_clear(exc_compositionL);
	for (i=0 ; i < new_input->absorbers->n_exc_layers ; i++) {
		gtk_list_store_append(exc_compositionL, &iter);
		elementString = xmi_msim_gui_utils_get_layer_element_string(new_input->absorbers->exc_layers + i);
		gtk_list_store_set(exc_compositionL, &iter,
			N_ELEMENTS_COLUMN, new_input->absorbers->exc_layers[i].n_elements,
			ELEMENTS_COLUMN,elementString,
			DENSITY_COLUMN, new_input->absorbers->exc_layers[i].density,
			THICKNESS_COLUMN, new_input->absorbers->exc_layers[i].thickness,
			-1
			);
		g_free(elementString);
	}
	xmi_free_composition(exc_compositionS);
	xmi_copy_abs_or_crystal2composition(new_input->absorbers->exc_layers, new_input->absorbers->n_exc_layers,&exc_compositionS);


	return;
}

static void change_all_values_detectionabsorbers(struct xmi_input *new_input) {
	char *elementString;
	int i;
	GtkTreeIter iter;

	gtk_list_store_clear(det_compositionL);
	for (i=0 ; i < new_input->absorbers->n_det_layers ; i++) {
		gtk_list_store_append(det_compositionL, &iter);
		elementString = xmi_msim_gui_utils_get_layer_element_string(new_input->absorbers->det_layers + i);
		gtk_list_store_set(det_compositionL, &iter,
			N_ELEMENTS_COLUMN, new_input->absorbers->det_layers[i].n_elements,
			ELEMENTS_COLUMN,elementString,
			DENSITY_COLUMN, new_input->absorbers->det_layers[i].density,
			THICKNESS_COLUMN, new_input->absorbers->det_layers[i].thickness,
			-1
			);
		g_free(elementString);
	}
	xmi_free_composition(det_compositionS);
	xmi_copy_abs_or_crystal2composition(new_input->absorbers->det_layers, new_input->absorbers->n_det_layers,&det_compositionS);


}

static void change_all_values_detectorsettings(struct xmi_input *new_input) {
	char *buffer, *elementString;
	int i;
	GtkTreeIter iter;

	detector_typeC = 1;
	detector_gainC = 1;
	detector_live_timeC = 1;
	detector_pulse_widthC = 1;
	detector_zeroC = 1;
	detector_fanoC = 1;
	detector_noiseC = 1;

	g_signal_handler_block(G_OBJECT(detector_typeW), detector_typeG);
	g_signal_handler_block(G_OBJECT(detector_gainW), detector_gainG);
	g_signal_handler_block(G_OBJECT(detector_pulse_widthW), detector_pulse_widthG);
	g_signal_handler_block(G_OBJECT(detector_live_timeW), detector_live_timeG);
	g_signal_handler_block(G_OBJECT(detector_zeroW), detector_zeroG);
	g_signal_handler_block(G_OBJECT(detector_noiseW), detector_noiseG);
	g_signal_handler_block(G_OBJECT(detector_fanoW), detector_fanoG);
	g_signal_handler_block(G_OBJECT(detector_nchannelsW), detector_nchannelsG);

	gtk_combo_box_set_active(GTK_COMBO_BOX(detector_typeW),new_input->detector->detector_type);

	buffer = g_strdup_printf("%lg",new_input->detector->gain);
	gtk_entry_set_text(GTK_ENTRY(detector_gainW),buffer);
	buffer = g_strdup_printf("%lg",new_input->detector->zero);
	gtk_entry_set_text(GTK_ENTRY(detector_zeroW),buffer);
	buffer = g_strdup_printf("%lg",new_input->detector->fano);
	gtk_entry_set_text(GTK_ENTRY(detector_fanoW),buffer);
	buffer = g_strdup_printf("%lg",new_input->detector->noise);
	gtk_entry_set_text(GTK_ENTRY(detector_noiseW),buffer);
	buffer = g_strdup_printf("%lg",new_input->detector->live_time);
	gtk_entry_set_text(GTK_ENTRY(detector_live_timeW),buffer);
	buffer = g_strdup_printf("%lg",new_input->detector->pulse_width);
	gtk_entry_set_text(GTK_ENTRY(detector_pulse_widthW),buffer);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(detector_nchannelsW), (double) new_input->detector->nchannels);

	gtk_list_store_clear(crystal_compositionL);
	for (i = 0 ; i < new_input->detector->n_crystal_layers ; i++) {
		gtk_list_store_append(crystal_compositionL, &iter);
		elementString = xmi_msim_gui_utils_get_layer_element_string(new_input->detector->crystal_layers + i);
		gtk_list_store_set(crystal_compositionL, &iter,
			N_ELEMENTS_COLUMN, new_input->detector->crystal_layers[i].n_elements,
			ELEMENTS_COLUMN,elementString,
			DENSITY_COLUMN, new_input->detector->crystal_layers[i].density,
			THICKNESS_COLUMN, new_input->detector->crystal_layers[i].thickness,
			-1
			);
		g_free(elementString);
	}
	xmi_free_composition(crystal_compositionS);
	xmi_copy_abs_or_crystal2composition(new_input->detector->crystal_layers, new_input->detector->n_crystal_layers,&crystal_compositionS);

	g_signal_handler_unblock(G_OBJECT(detector_nchannelsW), detector_nchannelsG);
	g_signal_handler_unblock(G_OBJECT(detector_typeW), detector_typeG);
	g_signal_handler_unblock(G_OBJECT(detector_gainW), detector_gainG);
	g_signal_handler_unblock(G_OBJECT(detector_zeroW), detector_zeroG);
	g_signal_handler_unblock(G_OBJECT(detector_pulse_widthW), detector_pulse_widthG);
	g_signal_handler_unblock(G_OBJECT(detector_live_timeW), detector_live_timeG);
	g_signal_handler_unblock(G_OBJECT(detector_noiseW), detector_noiseG);
	g_signal_handler_unblock(G_OBJECT(detector_fanoW), detector_fanoG);
}

#define GEOMETRY_HELP_SCALE_FACTOR_DEFAULT 3.0

static double geometry_help_scale_factor = GEOMETRY_HELP_SCALE_FACTOR_DEFAULT;

static double sample_source_distance_coords[2][2] = {{530, 498}, {652, 584}};
static double sample_orientation_coords[2][2] = {{1232, 200}, {1385, 290}};
static double detector_window_position_coords[2][2] = {{1417, 592}, {1573, 683}};
static double detector_window_normal_coords[2][2] = {{1604, 663}, {1744, 725}};
static double active_detector_area_coords[2][2] = {{1505, 497}, {1607, 564}};
static double collimator_height_coords[2][2] = {{1263, 647}, {1381, 780}};
static double collimator_diameter_coords[2][2] = {{1358, 450}, {1460, 540}};
static double source_slit_distance_coords[2][2] = {{374, 671}, {455, 750}};
static double slits_size_coords[2][2] = {{706, 510}, {981, 846}};


static gboolean cs_window_delete_event(void) {
	g_signal_handler_block(G_OBJECT(geometry_helpW), geometry_helpG);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(geometry_helpW), FALSE);
	g_signal_handler_unblock(G_OBJECT(geometry_helpW), geometry_helpG);
	gtk_button_set_label(GTK_BUTTON(geometry_helpW), "Show geometry help");

	//deactivate all eventbox callbacks
#define EB_DISCONNECT(type) g_signal_handler_disconnect((gpointer) type ## _ebW, type ## _enter_ebG);\
			    g_signal_handler_disconnect((gpointer) type ## _ebW, type ## _leave_ebG)

	EB_DISCONNECT(d_sample_source);
	EB_DISCONNECT(n_sample_orientation);
	EB_DISCONNECT(p_detector_window);
	EB_DISCONNECT(n_detector_orientation);
	EB_DISCONNECT(area_detector);
	EB_DISCONNECT(collimator_height);
	EB_DISCONNECT(collimator_diameter);
	EB_DISCONNECT(d_source_slit);
	EB_DISCONNECT(slit_size);
#undef EB_DISCONNECT
	return FALSE;
}


struct coordinate_pixbufs {
	GdkPixbuf *orig_pixbuf;
	GdkPixbuf *current_pixbuf;
};

/*
static gboolean coordinate_system_clicked_cb(GtkWidget *event_box, GdkEvent *event, struct coordinate_pixbufs *cp) {

	g_fprintf(stdout,"Click x: %lf\ty: %lf\n", event->button.x*geometry_help_scale_factor, event->button.y*geometry_help_scale_factor);
	//g_fprintf(stdout,"Raw Click x: %lf\ty: %lf\n", event->button.x, event->button.y);
	return TRUE;
}*/

#define COORDS_CHECK(coords, x, y) (x > coords[0][0] && x < coords[1][0] && y > coords[0][1] && y < coords[1][1])

/*static gboolean coordinate_system_enter_cb(GtkWidget *event_box, GdkEvent *event, gpointer data) {
	g_fprintf(stdout,"Entering coordinate system window\n");

	gdouble x = event->crossing.x*geometry_help_scale_factor;
	gdouble y = event->crossing.y*geometry_help_scale_factor;

	if (COORDS_CHECK(sample_source_distance_coords, x, y)) {
		gtk_widget_modify_bg(d_sample_source_ebW, GTK_STATE_NORMAL, &chartreuse_green);
	}

	return FALSE;
}*/

/*static gboolean coordinate_system_leave_cb(GtkWidget *event_box, GdkEvent *event, gpointer data) {
	g_fprintf(stdout,"Leaving coordinate system window\n");
	gtk_widget_modify_bg(d_sample_source_ebW, GTK_STATE_NORMAL, NULL);
	return TRUE;
}*/


static void draw_box(double coords[2][2], GtkWidget *image, struct coordinate_pixbufs *cp) {
	cairo_t *cr = gdk_cairo_create(gtk_widget_get_window(image));
	gdk_cairo_set_source_pixbuf(cr, gtk_image_get_pixbuf(GTK_IMAGE(image)), 0, 0);
	cairo_set_source_rgb(cr, 0, 0, 0);
	cairo_set_line_width (cr, 1.0);

	cairo_move_to(cr, coords[0][0]/geometry_help_scale_factor, coords[0][1]/geometry_help_scale_factor);
	cairo_line_to(cr, coords[0][0]/geometry_help_scale_factor, coords[1][1]/geometry_help_scale_factor);
	cairo_move_to(cr, coords[0][0]/geometry_help_scale_factor, coords[1][1]/geometry_help_scale_factor);
	cairo_line_to(cr, coords[1][0]/geometry_help_scale_factor, coords[1][1]/geometry_help_scale_factor);
	cairo_move_to(cr, coords[1][0]/geometry_help_scale_factor, coords[1][1]/geometry_help_scale_factor);
	cairo_line_to(cr, coords[1][0]/geometry_help_scale_factor, coords[0][1]/geometry_help_scale_factor);
	cairo_move_to(cr, coords[1][0]/geometry_help_scale_factor, coords[0][1]/geometry_help_scale_factor);
	cairo_line_to(cr, coords[0][0]/geometry_help_scale_factor, coords[0][1]/geometry_help_scale_factor);
	cairo_stroke(cr);
	cairo_destroy(cr);
	//gtk_image_set_from_pixbuf(GTK_IMAGE(image), copy);
	//gtk_widget_queue_draw_area(image, coords[0][0]/geometry_help_scale_factor-2, coords[0][1]/geometry_help_scale_factor-2,
	//	(coords[1][0]-coords[0][0])/geometry_help_scale_factor+4,(coords[1][1]-coords[0][1])/geometry_help_scale_factor+4);
}

static gboolean coordinate_system_motion_cb(GtkWidget *event_box, GdkEvent *event, struct coordinate_pixbufs *cp) {
	//g_fprintf(stdout,"Moving in coordinate system window\n");

	gdouble x = event->motion.x*geometry_help_scale_factor;
	gdouble y = event->motion.y*geometry_help_scale_factor;

	//g_fprintf(stdout, "x: %lf\ty: %lf\n", x, y);

	if (COORDS_CHECK(sample_source_distance_coords, x, y)) {
		gtk_widget_modify_bg(d_sample_source_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(sample_source_distance_coords, gtk_bin_get_child(GTK_BIN(event_box)), cp);
	}
	else if (COORDS_CHECK(sample_orientation_coords, x, y)) {
		gtk_widget_modify_bg(n_sample_orientation_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(sample_orientation_coords, gtk_bin_get_child(GTK_BIN(event_box)), cp);
	}
	else if (COORDS_CHECK(detector_window_position_coords, x, y)) {
		gtk_widget_modify_bg(p_detector_window_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(detector_window_position_coords, gtk_bin_get_child(GTK_BIN(event_box)), cp);
	}
	else if (COORDS_CHECK(detector_window_normal_coords, x, y)) {
		gtk_widget_modify_bg(n_detector_orientation_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(detector_window_normal_coords, gtk_bin_get_child(GTK_BIN(event_box)), cp);
	}
	else if (COORDS_CHECK(active_detector_area_coords, x, y)) {
		gtk_widget_modify_bg(area_detector_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(active_detector_area_coords, gtk_bin_get_child(GTK_BIN(event_box)), cp);
	}
	else if (COORDS_CHECK(collimator_height_coords, x, y)) {
		gtk_widget_modify_bg(collimator_height_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(collimator_height_coords, gtk_bin_get_child(GTK_BIN(event_box)), cp);
	}
	else if (COORDS_CHECK(collimator_diameter_coords, x, y)) {
		gtk_widget_modify_bg(collimator_diameter_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(collimator_diameter_coords, gtk_bin_get_child(GTK_BIN(event_box)), cp);
	}
	else if (COORDS_CHECK(source_slit_distance_coords, x, y)) {
		gtk_widget_modify_bg(d_source_slit_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(source_slit_distance_coords, gtk_bin_get_child(GTK_BIN(event_box)), cp);
	}
	else if (COORDS_CHECK(slits_size_coords, x, y)) {
		gtk_widget_modify_bg(slit_size_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(slits_size_coords, gtk_bin_get_child(GTK_BIN(event_box)), cp);
	}
	else {
		gtk_widget_modify_bg(d_sample_source_ebW, GTK_STATE_NORMAL, NULL);
		gtk_widget_modify_bg(n_sample_orientation_ebW, GTK_STATE_NORMAL, NULL);
		gtk_widget_modify_bg(p_detector_window_ebW, GTK_STATE_NORMAL, NULL);
		gtk_widget_modify_bg(n_detector_orientation_ebW, GTK_STATE_NORMAL, NULL);
		gtk_widget_modify_bg(area_detector_ebW, GTK_STATE_NORMAL, NULL);
		gtk_widget_modify_bg(collimator_height_ebW, GTK_STATE_NORMAL, NULL);
		gtk_widget_modify_bg(collimator_diameter_ebW, GTK_STATE_NORMAL, NULL);
		gtk_widget_modify_bg(d_source_slit_ebW, GTK_STATE_NORMAL, NULL);
		gtk_widget_modify_bg(slit_size_ebW, GTK_STATE_NORMAL, NULL);
		gtk_image_set_from_pixbuf(GTK_IMAGE(gtk_bin_get_child(GTK_BIN(event_box))), cp->current_pixbuf);
	}



	return FALSE;
}

static gint old_width;
static gint old_height;

#if GTK_MAJOR_VERSION == 3
static gboolean image_draw_event(GtkWidget *image, cairo_t *event, struct coordinate_pixbufs *cp)
#else
static gboolean image_expose_event(GtkWidget *image, GdkEvent *event, struct coordinate_pixbufs *cp)
#define gtk_widget_get_allocated_width(widget) widget->allocation.width
#define gtk_widget_get_allocated_height(widget) widget->allocation.height
#endif
	{
	gint new_width = gtk_widget_get_allocated_width(image);
	gint new_height = gtk_widget_get_allocated_height(image);

	if (new_width != old_width || new_height != old_height) {
		old_width = new_width;
		old_height = new_height;
		if (cp->current_pixbuf != NULL)
			g_object_unref(G_OBJECT(cp->current_pixbuf));
		cp->current_pixbuf = gdk_pixbuf_scale_simple(cp->orig_pixbuf, new_width, new_height, GDK_INTERP_HYPER);
		gtk_image_set_from_pixbuf(GTK_IMAGE(image), cp->current_pixbuf);
		geometry_help_scale_factor = (double) gdk_pixbuf_get_width(cp->orig_pixbuf)/ (double) gdk_pixbuf_get_width(cp->current_pixbuf);
	}
	return FALSE;
}


static gboolean geometry_eb_enter_cb(GtkWidget *event_box, GdkEvent *event, struct coordinate_pixbufs *cp) {
	//entering the eventbox!
	//1) turn it chartreuse_green
	//2) draw the box

	if (event_box == d_sample_source_ebW) {
		gtk_widget_modify_bg(d_sample_source_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(sample_source_distance_coords, gtk_bin_get_child(GTK_BIN(gtk_bin_get_child(GTK_BIN(cs_window)))), cp);
	}
	else if (event_box == n_sample_orientation_ebW) {
		gtk_widget_modify_bg(n_sample_orientation_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(sample_orientation_coords, gtk_bin_get_child(GTK_BIN(gtk_bin_get_child(GTK_BIN(cs_window)))), cp);
	}
	else if (event_box == p_detector_window_ebW) {
		gtk_widget_modify_bg(p_detector_window_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(detector_window_position_coords, gtk_bin_get_child(GTK_BIN(gtk_bin_get_child(GTK_BIN(cs_window)))), cp);
	}
	else if (event_box == n_detector_orientation_ebW) {
		gtk_widget_modify_bg(n_detector_orientation_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(detector_window_normal_coords, gtk_bin_get_child(GTK_BIN(gtk_bin_get_child(GTK_BIN(cs_window)))), cp);
	}
	else if (event_box == area_detector_ebW) {
		gtk_widget_modify_bg(area_detector_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(active_detector_area_coords, gtk_bin_get_child(GTK_BIN(gtk_bin_get_child(GTK_BIN(cs_window)))), cp);
	}
	else if (event_box == collimator_height_ebW) {
		gtk_widget_modify_bg(collimator_height_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(collimator_height_coords, gtk_bin_get_child(GTK_BIN(gtk_bin_get_child(GTK_BIN(cs_window)))), cp);
	}
	else if (event_box == collimator_diameter_ebW) {
		gtk_widget_modify_bg(collimator_diameter_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(collimator_diameter_coords, gtk_bin_get_child(GTK_BIN(gtk_bin_get_child(GTK_BIN(cs_window)))), cp);
	}
	else if (event_box == d_source_slit_ebW) {
		gtk_widget_modify_bg(d_source_slit_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(source_slit_distance_coords, gtk_bin_get_child(GTK_BIN(gtk_bin_get_child(GTK_BIN(cs_window)))), cp);
	}
	else if (event_box == slit_size_ebW) {
		gtk_widget_modify_bg(slit_size_ebW, GTK_STATE_NORMAL, &chartreuse_green);
		draw_box(slits_size_coords, gtk_bin_get_child(GTK_BIN(gtk_bin_get_child(GTK_BIN(cs_window)))), cp);
	}
	return FALSE;
}

static gboolean geometry_eb_leave_cb(GtkWidget *event_box, GdkEvent *event, struct coordinate_pixbufs *cp) {
	//leaving the eventbox!
	//1) turn it back to its normal color
	//2) remove the box

	if (event_box == d_sample_source_ebW) {
		gtk_widget_modify_bg(d_sample_source_ebW, GTK_STATE_NORMAL, NULL);
	}
	else if (event_box == n_sample_orientation_ebW) {
		gtk_widget_modify_bg(n_sample_orientation_ebW, GTK_STATE_NORMAL, NULL);
	}
	else if (event_box == p_detector_window_ebW) {
		gtk_widget_modify_bg(p_detector_window_ebW, GTK_STATE_NORMAL, NULL);
	}
	else if (event_box == n_detector_orientation_ebW) {
		gtk_widget_modify_bg(n_detector_orientation_ebW, GTK_STATE_NORMAL, NULL);
	}
	else if (event_box == area_detector_ebW) {
		gtk_widget_modify_bg(area_detector_ebW, GTK_STATE_NORMAL, NULL);
	}
	else if (event_box == collimator_height_ebW) {
		gtk_widget_modify_bg(collimator_height_ebW, GTK_STATE_NORMAL, NULL);
	}
	else if (event_box == collimator_diameter_ebW) {
		gtk_widget_modify_bg(collimator_diameter_ebW, GTK_STATE_NORMAL, NULL);
	}
	else if (event_box == d_source_slit_ebW) {
		gtk_widget_modify_bg(d_source_slit_ebW, GTK_STATE_NORMAL, NULL);
	}
	else if (event_box == slit_size_ebW) {
		gtk_widget_modify_bg(slit_size_ebW, GTK_STATE_NORMAL, NULL);
	}
	gtk_image_set_from_pixbuf(GTK_IMAGE(gtk_bin_get_child(GTK_BIN(gtk_bin_get_child(GTK_BIN(cs_window))))), cp->current_pixbuf);

	return FALSE;
}
static void geometry_help_clicked_cb(GtkWidget *window) {

	if (!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(geometry_helpW))) {
		cs_window_delete_event();
		gtk_widget_destroy(cs_window);
		return;
	}
	geometry_help_scale_factor = GEOMETRY_HELP_SCALE_FACTOR_DEFAULT;
	gtk_button_set_label(GTK_BUTTON(geometry_helpW), "Hide geometry help");
	cs_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(cs_window), 600, -1);
	gtk_widget_modify_bg(cs_window, GTK_STATE_NORMAL, &white);
	gtk_container_set_border_width(GTK_CONTAINER(cs_window),5);
	//gtk_window_set_resizable(GTK_WINDOW(cs_window), FALSE);
	gtk_window_set_title(GTK_WINDOW(cs_window), "Geometry coordinate system");
	//gtk_window_set_default_size(GTK_WINDOW(window),420,300);
	gtk_window_set_position(GTK_WINDOW(cs_window), GTK_WIN_POS_NONE);
	g_signal_connect(G_OBJECT(cs_window), "delete-event", G_CALLBACK(cs_window_delete_event), NULL);

	GtkWidget *event_box = gtk_event_box_new();
	struct coordinate_pixbufs *cp = (struct coordinate_pixbufs *) g_malloc(sizeof(struct coordinate_pixbufs));
	cp->current_pixbuf = NULL;
	//g_signal_connect(G_OBJECT(event_box), "button-press-event", G_CALLBACK(coordinate_system_clicked_cb), cp);
	//g_signal_connect(G_OBJECT(event_box), "enter-notify-event", G_CALLBACK(coordinate_system_enter_cb), NULL);
	//g_signal_connect(G_OBJECT(event_box), "leave-notify-event", G_CALLBACK(coordinate_system_leave_cb), NULL);
	g_signal_connect(G_OBJECT(event_box), "motion-notify-event", G_CALLBACK(coordinate_system_motion_cb), cp);
	GdkPixbuf *orig_pixbuf;
	gchar *coordinate_system_file = NULL;

#ifdef G_OS_WIN32
	if (xmi_registry_win_query(XMI_REGISTRY_WIN_COORDINATE_SYSTEM, &coordinate_system_file) == 0) {
		g_fprintf(stderr, "Could not open coordinate system file\n");
		exit(1);
	}
#elif defined(MAC_INTEGRATION)
	if (xmi_resources_mac_query(XMI_RESOURCES_MAC_COORDINATE_SYSTEM, &coordinate_system_file) == 0) {
		g_fprintf(stderr, "Could not open coordinate system file\n");
		exit(1);
	}
#else
	coordinate_system_file = g_strdup(XMIMSIM_COORDINATE_SYSTEM);
#endif

	GError *error = NULL;
	orig_pixbuf = gdk_pixbuf_new_from_file(coordinate_system_file, &error);

	if (error != NULL) {
		g_fprintf(stderr, "Could not open coordinate system file: %s\n", error->message);
		exit(1);
	}
	g_free(coordinate_system_file);

	cp->orig_pixbuf = orig_pixbuf;
	GdkPixbuf *pixbuf2 = gdk_pixbuf_scale_simple(orig_pixbuf, gdk_pixbuf_get_width(orig_pixbuf)/geometry_help_scale_factor, gdk_pixbuf_get_height(orig_pixbuf)/geometry_help_scale_factor, GDK_INTERP_HYPER);
	GtkWidget *coordinate_system_image = gtk_image_new_from_pixbuf(pixbuf2);
	gtk_widget_set_events(event_box, gtk_widget_get_events(event_box) | GDK_EXPOSURE_MASK | GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);
	gtk_misc_set_alignment(GTK_MISC(coordinate_system_image), 0.0, 0.0);
	gtk_misc_set_padding(GTK_MISC(coordinate_system_image), 0, 0);
	gtk_container_add(GTK_CONTAINER(event_box), coordinate_system_image);
	gtk_container_add(GTK_CONTAINER(cs_window), event_box);
	old_width = 0;
	old_height = 0;

	GdkGeometry geometry;
	geometry.min_aspect = (double) gdk_pixbuf_get_width(orig_pixbuf)/(double) gdk_pixbuf_get_height(orig_pixbuf);
	geometry.max_aspect = (double) gdk_pixbuf_get_width(orig_pixbuf)/(double) gdk_pixbuf_get_height(orig_pixbuf);

	gtk_window_set_geometry_hints(GTK_WINDOW(cs_window), cs_window, &geometry, GDK_HINT_ASPECT);
#if GTK_MAJOR_VERSION == 3
	g_signal_connect(G_OBJECT(coordinate_system_image), "draw", G_CALLBACK(image_draw_event), cp);
#else
	g_signal_connect(G_OBJECT(coordinate_system_image), "expose-event", G_CALLBACK(image_expose_event), cp);
#endif
	gtk_widget_set_size_request(cs_window, gdk_pixbuf_get_width(orig_pixbuf)/geometry_help_scale_factor, gdk_pixbuf_get_height(orig_pixbuf)/geometry_help_scale_factor);

	//activate all signals for *_ebW
#define EB_CONNECT(type) type ## _enter_ebG = g_signal_connect(G_OBJECT(type ## _ebW), "enter-notify-event", G_CALLBACK(geometry_eb_enter_cb), cp);\
			 type ## _leave_ebG = g_signal_connect(G_OBJECT(type ## _ebW), "leave-notify-event", G_CALLBACK(geometry_eb_leave_cb), cp);
	EB_CONNECT(d_sample_source);
	EB_CONNECT(n_sample_orientation);
	EB_CONNECT(p_detector_window);
	EB_CONNECT(n_detector_orientation);
	EB_CONNECT(area_detector);
	EB_CONNECT(collimator_height);
	EB_CONNECT(collimator_diameter);
	EB_CONNECT(d_source_slit);
	EB_CONNECT(slit_size);
#undef EB_CONNECT

	gtk_widget_show_all(cs_window);
}

#if !GLIB_CHECK_VERSION (2, 28, 0)
void
g_list_free_full (GList          *list,
                  GDestroyNotify  free_func)
{
  g_list_foreach (list, (GFunc) free_func, NULL);
  g_list_free (list);
	}
#endif
