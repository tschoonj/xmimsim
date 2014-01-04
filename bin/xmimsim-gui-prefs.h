/*
 * Copyright (C) 2010-2012 Tom Schoonjans and Laszlo Vincze
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

#include <config.h>
#include <glib.h>
#include <gtk/gtk.h>

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
	//int
	XMIMSIM_GUI_PREFS_NCHANNELS,
	//gboolean
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	XMIMSIM_GUI_PREFS_OPENCL,
#endif
#if defined(MAC_INTEGRATION) || defined(HAVE_LIBNOTIFY)
	//gboolean
	XMIMSIM_GUI_PREFS_NOTIFICATIONS
#endif
};

union xmimsim_prefs_val{
	gboolean b;
	gint i;
	gchar **ss;
};



//returns 1 on success, 0 on error
int xmimsim_gui_get_prefs(int kind, union xmimsim_prefs_val *prefs);

//returns 1 on success, 0 on error
int xmimsim_gui_set_prefs(int kind, union xmimsim_prefs_val prefs);

struct xmi_preferences_data {
	GtkWidget *window;
	gint page;
};

void xmimsim_gui_launch_preferences(GtkWidget *widget, gpointer data);
void preferences_error_handler(GtkWidget *window);

#endif
