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

#include <gtk/gtk.h> 
#include "xmi_data_structs.h"
#include "xmimsim-gui-controls.h"


#ifndef XMIMSIM_GUI_H
#define XMIMSIM_GUI_H



struct undo_single {
	struct xmi_input *xi;
	char message[512];
	int kind;
	GtkWidget *widget;
	char *filename;
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
	CONTINUOUS_ENERGY_ADD,
	CONTINUOUS_ENERGY_EDIT,
	CONTINUOUS_ENERGY_DELETE,
	EXC_COMPOSITION,
	EXC_COMPOSITION_ORDER,
	EXC_COMPOSITION_DELETE,
	EXC_COMPOSITION_ADD,
	EXC_COMPOSITION_EDIT,
	DET_COMPOSITION,
	DET_COMPOSITION_ORDER,
	DET_COMPOSITION_DELETE,
	DET_COMPOSITION_ADD,
	DET_COMPOSITION_EDIT,
	CRYSTAL_COMPOSITION,
	CRYSTAL_COMPOSITION_ORDER,
	CRYSTAL_COMPOSITION_DELETE,
	CRYSTAL_COMPOSITION_ADD,
	CRYSTAL_COMPOSITION_EDIT,
	DETECTOR_TYPE,
	DETECTOR_GAIN,
	DETECTOR_PULSE_WIDTH,
	DETECTOR_LIVE_TIME,
	DETECTOR_ZERO,
	DETECTOR_FANO,
	DETECTOR_NOISE,
	DETECTOR_MAX_CONVOLUTION_ENERGY,
};

void update_undo_buffer(int kind, GtkWidget *widget);

int check_changeables(void);

void my_gtk_cell_renderer_toggle_set_activatable (GtkCellRendererToggle *toggle, gboolean setting);
void my_gtk_cell_renderer_set_alignment (GtkCellRenderer *cell, gfloat xalign, gfloat yalign);

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
};

enum {
	GTK_RESPONSE_SAVEAS,
	GTK_RESPONSE_SAVE,
	GTK_RESPONSE_NOSAVE
};

#ifdef MAC_INTEGRATION 
  #define XMIMSIM_TITLE_PREFIX ""
#else
  #define XMIMSIM_TITLE_PREFIX "XMI MSIM: "
#endif


void update_xmimsim_title_xmsi(char *new_title, GtkWidget *my_window, char *filename);
void update_xmimsim_title_xmso(char *new_title, GtkWidget *my_window, char *filename);

#endif
