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

#ifndef XMIMSIM_GUI_ENERGIES_H
#define XMIMSIM_GUI_ENERGIES_H

GtkWidget *initialize_energies(struct xmi_excitation *excitation, GtkWidget *main_window); 

extern struct xmi_energy *energy;
extern int current_index;
extern int current_nindices;

struct energiesWidget {
	GtkListStore *store;
	GtkWidget *widget;
};

extern struct energiesWidget *contWidget;
extern struct energiesWidget *discWidget;

enum {
	ENERGY_COLUMN,
	HOR_INTENSITY_COLUMN,
	VER_INTENSITY_COLUMN,
	SIGMA_X_COLUMN,
	SIGMA_XP_COLUMN,
	SIGMA_Y_COLUMN,
	SIGMA_YP_COLUMN,
	NCOLUMNS_ENERGIES,
};

#endif
