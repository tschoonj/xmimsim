/*
Copyright (C) 2010-2014 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMIMSIM_GUI_SOURCES_H
#define XMIMSIM_GUI_SOURCES_H

void xray_sources_button_clicked_cb(GtkButton *button, GtkWidget *main_window);
void export_canvas_image (GtkWidget *canvas, gchar *title);

struct xmi_ebel_parameters {
	double tube_voltage;
	double tube_current;
	double tube_solid_angle;
	double alpha;
	double beta;
	double interval_width;
	int anode_Z;
	double anode_rho;
	double anode_thickness;
	int window_Z;
	double window_rho;
	double window_thickness;
	int filter_Z;
	double filter_rho;
	double filter_thickness;
	gboolean transmission_tube;
	gchar *transmission_efficiency_file;
	gboolean log10_active;
};

struct xmi_nuclide_parameters {
	int radioNuclide;
	int activityUnit;
	double activity;
	gboolean log10_active;
};

enum {
	ACTIVITY_UNIT_mCi = 0,
	ACTIVITY_UNIT_Ci,
	ACTIVITY_UNIT_GBq,
	ACTIVITY_UNIT_Bq,
};

extern const gchar *activity_units[4];

#endif
