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

enum {
	LW_ADD,
	LW_EDIT,
};

struct layerWidget {
	GtkWidget *window;
	GtkListStore *store;
	struct xmi_layer ** my_layer;
	GtkWidget *sumEntry;
	GtkWidget *densityEntry;
	GtkWidget *thicknessEntry;
	GtkWidget *editButton;
	GtkWidget *okButton;
	GtkWidget *cancelButton;
	GtkWidget *removeButton;
	GtkWidget *addToCatalogButton;
	int matrixKind;
	int AddOrEdit;
	int layerNumber;
	GtkTreeIter iter;
	gulong densityG;
	gulong thicknessG;
	

};

struct layerWidget * initialize_layer_widget(struct xmi_layer **, GtkWidget *main_window); 

GtkWidget* my_gtk_dialog_get_widget_for_response (GtkDialog *dialog,gint response_id);
