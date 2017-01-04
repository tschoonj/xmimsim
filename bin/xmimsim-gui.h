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

//#define GDK_DISABLE_DEPRECATED
//#define GTK_DISABLE_DEPRECATED
#include <gtk/gtk.h>
#ifdef HAVE_CXX
  #include <gtkmm-plplot.h>
#else
  #include <gtkextra/gtkextra.h>
#endif
#include "xmi_data_structs.h"
#include "xmimsim-gui-controls.h"

#ifdef MAC_INTEGRATION
#include <gtkosxapplication.h>
#endif


#ifndef XMIMSIM_GUI_H
#define XMIMSIM_GUI_H

#if !GLIB_CHECK_VERSION (2, 28, 0)
void g_list_free_full (GList *list, GDestroyNotify  free_func);
#endif


struct undo_single {
	struct xmi_input *xi;
	gchar *message;
	int kind;
	GtkWidget *widget;
	char *filename;
	int *check;
};

/*
 *
 * Gdk colors
 *
 */

extern GdkColor white;
extern GdkColor red;

extern struct undo_single *current;
extern struct undo_single *last_saved;

/*
 *
 * enums
 *
 */

enum {
	OUTPUTFILE,
	N_PHOTONS_INTERVAL,
	N_PHOTONS_LINE,
	N_INTERACTIONS_TRAJECTORY,
	COMMENTS,
	COMPOSITION,
	COMPOSITION_ORDER,
	COMPOSITION_REFERENCE,
	COMPOSITION_DELETE,
	COMPOSITION_ADD,
	COMPOSITION_EDIT,
	COMPOSITION_CUT,
	COMPOSITION_PASTE,
	OPEN_FILE,
	D_SAMPLE_SOURCE,
	N_SAMPLE_ORIENTATION_X,
	N_SAMPLE_ORIENTATION_Y,
	N_SAMPLE_ORIENTATION_Z,
	P_DETECTOR_WINDOW_X,
	P_DETECTOR_WINDOW_Y,
	P_DETECTOR_WINDOW_Z,
	N_DETECTOR_ORIENTATION_X,
	N_DETECTOR_ORIENTATION_Y,
	N_DETECTOR_ORIENTATION_Z,
	AREA_DETECTOR,
	ACCEPTANCE_DETECTOR,
	COLLIMATOR_HEIGHT,
	COLLIMATOR_DIAMETER,
	D_SOURCE_SLIT,
	SLIT_SIZE_X,
	SLIT_SIZE_Y,
	DISCRETE_ENERGY_ADD,
	DISCRETE_ENERGY_EDIT,
	DISCRETE_ENERGY_DELETE,
	DISCRETE_ENERGY_CLEAR,
	DISCRETE_ENERGY_IMPORT_ADD,
	DISCRETE_ENERGY_IMPORT_REPLACE,
	DISCRETE_ENERGY_SCALE,
	CONTINUOUS_ENERGY_ADD,
	CONTINUOUS_ENERGY_EDIT,
	CONTINUOUS_ENERGY_DELETE,
	CONTINUOUS_ENERGY_CLEAR,
	CONTINUOUS_ENERGY_IMPORT_ADD,
	CONTINUOUS_ENERGY_IMPORT_REPLACE,
	CONTINUOUS_ENERGY_SCALE,
	EBEL_SPECTRUM_REPLACE,
	NUCLIDE_SPECTRUM_ADD,
	NUCLIDE_SPECTRUM_REPLACE,
	EXC_COMPOSITION,
	EXC_COMPOSITION_ORDER,
	EXC_COMPOSITION_DELETE,
	EXC_COMPOSITION_ADD,
	EXC_COMPOSITION_EDIT,
	EXC_COMPOSITION_CUT,
	EXC_COMPOSITION_PASTE,
	DET_COMPOSITION,
	DET_COMPOSITION_ORDER,
	DET_COMPOSITION_DELETE,
	DET_COMPOSITION_ADD,
	DET_COMPOSITION_EDIT,
	DET_COMPOSITION_CUT,
	DET_COMPOSITION_PASTE,
	CRYSTAL_COMPOSITION,
	CRYSTAL_COMPOSITION_ORDER,
	CRYSTAL_COMPOSITION_DELETE,
	CRYSTAL_COMPOSITION_ADD,
	CRYSTAL_COMPOSITION_EDIT,
	CRYSTAL_COMPOSITION_CUT,
	CRYSTAL_COMPOSITION_PASTE,
	DETECTOR_TYPE,
	DETECTOR_GAIN,
	DETECTOR_PULSE_WIDTH,
	DETECTOR_LIVE_TIME,
	DETECTOR_ZERO,
	DETECTOR_NCHANNELS,
	DETECTOR_FANO,
	DETECTOR_NOISE,
	IMPORT_FROM_FILE,
};

void update_undo_buffer(int kind, GtkWidget *widget);
void update_undo_buffer_with_error(int kind, GtkWidget *widget, int *check);

int check_changeables(void);

extern GtkWidget *saveW;
extern GtkWidget *save_asW;
extern GtkToolItem *saveasT;
extern GtkToolItem *saveT;
extern GtkWidget *commentsW;

//notebookpages
extern GtkWidget *notebook;
extern gint input_page;
extern gint control_page;
extern gint results_page;
extern gint current_page;

struct undo_single *check_changes_saved(int *status);


enum {
	CHECK_CHANGES_JUST_SAVED,
	CHECK_CHANGES_SAVED_BEFORE,
	CHECK_CHANGES_NEVER_SAVED,
	CHECK_CHANGES_NEW,
};

enum {
	GTK_RESPONSE_SAVEAS,
	GTK_RESPONSE_SAVE,
	GTK_RESPONSE_NOSAVE
};

enum {
	IMPORT_SELECT_COMPOSITION = 1,
	IMPORT_SELECT_GEOMETRY = 2,
	IMPORT_SELECT_EXCITATION = 4,
	IMPORT_SELECT_BEAMABSORBERS = 8,
	IMPORT_SELECT_DETECTIONABSORBERS = 16,
	IMPORT_SELECT_DETECTORSETTINGS = 32,
};

#ifdef MAC_INTEGRATION
  #define XMIMSIM_TITLE_PREFIX ""
#else
  #define XMIMSIM_TITLE_PREFIX "XMI-MSIM: "
#endif


void update_xmimsim_title_xmsi(const char *new_title, GtkWidget *my_window, const char *filename);
void update_xmimsim_title_xmso(const char *new_title, GtkWidget *my_window, const char *filename);

void xmi_open_url(const char *url);
void xmi_open_email(const char *address);
void adjust_save_buttons(void);

#define XMI_STOCK_RADIATION_WARNING "Radiation_warning_symbol"
#define XMI_STOCK_LOGO "Logo_xmi_msim"
#define XMI_STOCK_LOGO_RED "Logo_xmi_msim_red"
#define XMI_STOCK_LOGO_ARCHIVE "Logo_xmi_msim_archive"


#ifdef MAC_INTEGRATION
void quit_program_cb(GtkosxApplication *app, gpointer data);
#else
void quit_program_cb(GtkWidget *widget, gpointer data);
#endif

int kill_current_job(void);

GtkWidget *long_job_dialog(GtkWidget *parent, const gchar *message_with_markup);

#endif
