/*
 * Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#ifndef XMIMSIM_GUI_PREFS_H
#define XMIMSIM_GUI_PREFS_H

#include <gtk/gtk.h>
#include "xmimsim-gui-application.h"

G_BEGIN_DECLS

enum {
	//gboolean
	XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES,
	//gboolean
	XMIMSIM_GUI_PREFS_M_LINES,
	//gboolean
	XMIMSIM_GUI_PREFS_RAD_CASCADE,
	//gboolean
	XMIMSIM_GUI_PREFS_NONRAD_CASCADE,
	//gboolean
	XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION,
	//gboolean
	XMIMSIM_GUI_PREFS_PILE_UP,
	//gchar **
	XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS,
	//gboolean
	XMIMSIM_GUI_PREFS_POISSON,
	//gboolean
	XMIMSIM_GUI_PREFS_ESCAPE_PEAKS,
	//gboolean
	XMIMSIM_GUI_PREFS_ADVANCED_COMPTON,
	//gboolean
	XMIMSIM_GUI_PREFS_OPENCL,
	//gboolean
	XMIMSIM_GUI_PREFS_NOTIFICATIONS,
	//gboolean
	XMIMSIM_GUI_PREFS_DEFAULT_SEEDS,
	//gchar *
	XMIMSIM_GUI_PREFS_CUSTOM_DETECTOR_RESPONSE,
	//gchar *
	XMIMSIM_GUI_PREFS_DEFAULT_SAVE_FOLDER,
	//gboolean
	XMIMSIM_GUI_PREFS_GOOGLE_ANALYTICS_SHOW_STARTUP_DIALOG,
	//gchar *
	XMIMSIM_GUI_PREFS_GOOGLE_ANALYTICS_UUID,
};

//user defined layers manipulation
gchar **xmimsim_gui_get_user_defined_layer_names(void);

struct xmi_layer* xmimsim_gui_get_user_defined_layer(const gchar *layer_name);

int xmimsim_gui_add_user_defined_layer(struct xmi_layer *layer, const gchar *layer_name);

int xmimsim_gui_remove_user_defined_layer(const gchar *layer_name);

//returns 1 on success, 0 on error
int xmimsim_gui_get_prefs(int kind, GValue *prefs);

//returns 1 on success, 0 on error
int xmimsim_gui_set_prefs(int kind, const GValue *prefs);

void xmimsim_gui_launch_preferences(GtkWindow *parent);

void preferences_error_handler(GtkWidget *window);

char *xmimsim_gui_get_preferences_filename(void);

int xmimsim_gui_create_prefs_file(GKeyFile *keyfile, gchar *prefs_file);

G_END_DECLS

#endif
