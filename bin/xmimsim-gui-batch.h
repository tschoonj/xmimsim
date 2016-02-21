/*
Copyright (C) 2010-2013 Tom Schoonjans and Laszlo Vincze

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


#ifndef XMIMSIM_GUI_BATCH_H
#define XMIMSIM_GUI_BATCH_H

#include <gtk/gtk.h>
#include "xmi_data_structs.h"

void batchmode_button_clicked_cb(GtkWidget *button, GtkWidget *window);

enum {
	INPUT_PARAMETER_COLUMN,
	INPUT_VALUE_COLUMN,
	INPUT_SELECTABLE_COLUMN,
	INPUT_XPATH_COLUMN,
	INPUT_ALLOWED_COLUMN,
	INPUT_N_COLUMNS
};

GtkWidget *get_inputfile_treeview(struct xmi_input *input, int with_colors);

enum {
	PARAMETER_DOUBLE = 1,
	PARAMETER_INT = 2,
	PARAMETER_LONG = 4,
	PARAMETER_STRICT_POSITIVE = 8,
	PARAMETER_POSITIVE = 16,
	PARAMETER_WEIGHT_FRACTION = 32,
};

void launch_archive_plot(struct xmi_archive *archive, GtkWidget *window);


#endif
