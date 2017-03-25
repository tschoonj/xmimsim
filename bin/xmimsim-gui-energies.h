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

#include <gtk/gtk.h>
#include "xmi_data_structs.h"

#ifndef XMIMSIM_GUI_ENERGIES_H
#define XMIMSIM_GUI_ENERGIES_H


struct energiesWidget {
	GtkListStore *store;
	GtkWidget *widget;
};

GtkWidget *initialize_energies(struct xmi_excitation *excitation, GtkWidget *main_window, struct energiesWidget **discWidget, struct energiesWidget **contWidget);

enum {
	ENERGY_COLUMN,
	HOR_INTENSITY_COLUMN,
	VER_INTENSITY_COLUMN,
	SIGMA_X_COLUMN,
	SIGMA_XP_COLUMN,
	SIGMA_Y_COLUMN,
	SIGMA_YP_COLUMN,
	DISTRIBUTION_TYPE_COLUMN,
	SCALE_PARAMETER_COLUMN,
	NCOLUMNS_ENERGIES,
};

struct energiesUndoInfo {
	double scale_value;
	GArray *indices;
	int index;
	int n_energy_disc;
	struct xmi_energy_discrete *energy_disc;
	int n_energy_cont;
	struct xmi_energy_continuous *energy_cont;
};

void repopulate_discrete_energies(GtkListStore *store, struct xmi_excitation *excitation);
void repopulate_continuous_energies(GtkListStore *store, struct xmi_excitation *excitation);

#endif
