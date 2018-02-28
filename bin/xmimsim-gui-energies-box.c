/*
Copyright (C) 2018 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-energies-box.h"
#include "xmimsim-gui-discrete-energy-dialog.h"
#include "xmimsim-gui-continuous-energy-dialog.h"
#include "xmimsim-gui-marshal.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-compat.h"
#include <stdlib.h>
#include <search.h>
#include <string.h>
#include <xmi_aux.h>

typedef struct _XmiMsimGuiEnergiesSingleBox XmiMsimGuiEnergiesSingleBox;

struct _XmiMsimGuiEnergiesBox {
	GtkBox parent_instance;
	XmiMsimGuiEnergiesSingleBox *discrete_box;
	XmiMsimGuiEnergiesSingleBox *continuous_box;
	GArray *discrete_array;
	GArray *continuous_array;
};

struct _XmiMsimGuiEnergiesSingleBox {
	XmiMsimGuiEnergiesBox *parent_box;
	XmiMsimGuiEnergiesSingleBoxType type;
	GtkWidget *delete_button;
	GtkWidget *edit_button;
	GtkWidget *scale_button;
	GtkWidget *add_button;
	GtkWidget *import_button;
	GtkWidget *clear_button;
	GtkListStore *store;
	GtkWidget *tree;
};

struct _XmiMsimGuiEnergiesBoxClass {
	GtkBoxClass parent_class;
};

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

enum {
	CHANGED,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

G_DEFINE_TYPE(XmiMsimGuiEnergiesBox, xmi_msim_gui_energies_box, GTK_TYPE_BOX)

static void xmi_msim_gui_energies_box_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_energies_box_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_energies_box_finalize(GObject *gobject) {
	XmiMsimGuiEnergiesBox *box = XMI_MSIM_GUI_ENERGIES_BOX(gobject);
	g_array_unref(box->discrete_array);
	g_array_free(box->discrete_array, TRUE);
	g_array_unref(box->continuous_array);
	g_array_free(box->continuous_array, TRUE);

	G_OBJECT_CLASS(xmi_msim_gui_energies_box_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_energies_box_class_init(XmiMsimGuiEnergiesBoxClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_energies_box_dispose;
	object_class->finalize = xmi_msim_gui_energies_box_finalize;

	signals[CHANGED] = g_signal_new(
		"changed",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__STRING,
		G_TYPE_NONE,
		1,
		G_TYPE_STRING // gchar*
	);
}

static GtkWidget *initialize_single_energies(XmiMsimGuiEnergiesBox *self, XmiMsimGuiEnergiesSingleBoxType type);

static void xmi_msim_gui_energies_box_init(XmiMsimGuiEnergiesBox *self) {
	g_object_set(
		self,
		"homogeneous", FALSE,
		"expand", FALSE,
		"orientation", GTK_ORIENTATION_VERTICAL,
		"border-width", 10,
		NULL
	);
	gtk_box_pack_start(GTK_BOX(self), gtk_label_new("Discrete energies"), FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(self), initialize_single_energies(self, XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE), FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(self), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 3);

	gtk_box_pack_start(GTK_BOX(self), gtk_label_new("Continuous energies"), FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(self), initialize_single_energies(self, XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS), FALSE, FALSE, 2);

	gtk_widget_show_all(GTK_WIDGET(self));

	self->discrete_array = g_array_new(FALSE, FALSE, sizeof(struct xmi_energy_discrete));
	g_array_ref(self->discrete_array);
	self->continuous_array = g_array_new(FALSE, FALSE, sizeof(struct xmi_energy_continuous));
	g_array_ref(self->continuous_array);
}

static void energy_print_double(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gdouble value;
	gchar *double_text;

	gtk_tree_model_get(tree_model,iter, GPOINTER_TO_INT(data), &value, -1);

	double_text = g_strdup_printf("%g",value);
	g_object_set(G_OBJECT(renderer), "text", double_text, NULL);

	g_free(double_text);
}

static void energy_print_scale_parameter(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gint type;
	gdouble scale_parameter;
	gchar *text = NULL;

	gtk_tree_model_get(tree_model,iter, DISTRIBUTION_TYPE_COLUMN, &type,-1);

	if (type == XMI_DISCRETE_MONOCHROMATIC) {
		text = g_strdup("n/a");
	}
	else if (type == XMI_DISCRETE_GAUSSIAN || type == XMI_DISCRETE_LORENTZIAN) {
		gtk_tree_model_get(tree_model,iter, SCALE_PARAMETER_COLUMN, &scale_parameter,-1);
		text = g_strdup_printf("%g", scale_parameter);
	}

	g_object_set(G_OBJECT(renderer), "text", text, NULL);

	g_free(text);
}

static void energy_print_distribution_type(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gint type;
	gchar *text = NULL;

	gtk_tree_model_get(tree_model,iter, DISTRIBUTION_TYPE_COLUMN, &type,-1);

	if (type == XMI_DISCRETE_MONOCHROMATIC) {
		text = g_strdup("Monochromatic");
	}
	else if (type == XMI_DISCRETE_GAUSSIAN) {
		text = g_strdup("Gaussian");
	}
	else if (type == XMI_DISCRETE_LORENTZIAN) {
		text = g_strdup("Lorentzian");
	}

	g_object_set(G_OBJECT(renderer), "text", text, NULL);

	g_free(text);
}

static void energy_selection_changed_cb(GtkTreeSelection *selection, XmiMsimGuiEnergiesSingleBox *single_box) {
	int nselected = gtk_tree_selection_count_selected_rows(selection);

	switch (nselected) {
		case 0:
			gtk_widget_set_sensitive(single_box->delete_button, FALSE);
			gtk_widget_set_sensitive(single_box->edit_button, FALSE);
			gtk_widget_set_sensitive(single_box->scale_button, FALSE);
			break;
		case 1:
			gtk_widget_set_sensitive(single_box->edit_button, TRUE);
			gtk_widget_set_sensitive(single_box->delete_button, TRUE);
			gtk_widget_set_sensitive(single_box->scale_button, TRUE);
			break;
		default:
			gtk_widget_set_sensitive(single_box->delete_button, TRUE);
			gtk_widget_set_sensitive(single_box->scale_button, TRUE);
			gtk_widget_set_sensitive(single_box->edit_button, FALSE);
	}
}

static gint energy_column_comparator(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, gpointer userdata) {
	gdouble energy_a, energy_b;

	gtk_tree_model_get(model, a, ENERGY_COLUMN, &energy_a, -1);
	gtk_tree_model_get(model, b, ENERGY_COLUMN, &energy_b, -1);

	double diff = energy_a - energy_b;

	if (diff > 0.000000001)
		return 1;
	else if (diff < -0.000000001)
		return -1;
	return 0;
}

static void edit_discrete_energy(XmiMsimGuiEnergiesSingleBox *single_box, int selected_index, struct xmi_energy_discrete *xed) {
	GtkTreeIter iter;
	GtkTreePath *path = gtk_tree_path_new_from_indices(selected_index, -1);

	gtk_tree_model_get_iter(GTK_TREE_MODEL(single_box->store), &iter, path);
	gtk_tree_path_free(path);

	gtk_list_store_set(single_box->store, &iter,
		ENERGY_COLUMN, xed->energy,
		HOR_INTENSITY_COLUMN, xed->horizontal_intensity,
		VER_INTENSITY_COLUMN, xed->vertical_intensity,
		SIGMA_X_COLUMN, xed->sigma_x,
		SIGMA_XP_COLUMN, xed->sigma_xp,
		SIGMA_Y_COLUMN, xed->sigma_y,
		SIGMA_YP_COLUMN, xed->sigma_yp,
		DISTRIBUTION_TYPE_COLUMN, xed->distribution_type,
		SCALE_PARAMETER_COLUMN, xed->scale_parameter,
		-1);

	g_array_index(single_box->parent_box->discrete_array, struct xmi_energy_discrete, selected_index) = *xed;
	g_array_sort(single_box->parent_box->discrete_array, xmi_cmp_struct_xmi_energy_discrete);
}

static void edit_continuous_energy(XmiMsimGuiEnergiesSingleBox *single_box, int selected_index, struct xmi_energy_continuous *xec) {
	GtkTreeIter iter;
	GtkTreePath *path = gtk_tree_path_new_from_indices(selected_index, -1);

	gtk_tree_model_get_iter(GTK_TREE_MODEL(single_box->store), &iter, path);
	gtk_tree_path_free(path);

	gtk_list_store_set(single_box->store, &iter,
		ENERGY_COLUMN, xec->energy,
		HOR_INTENSITY_COLUMN, xec->horizontal_intensity,
		VER_INTENSITY_COLUMN, xec->vertical_intensity,
		SIGMA_X_COLUMN, xec->sigma_x,
		SIGMA_XP_COLUMN, xec->sigma_xp,
		SIGMA_Y_COLUMN, xec->sigma_y,
		SIGMA_YP_COLUMN, xec->sigma_yp,
		-1);

	g_array_index(single_box->parent_box->continuous_array, struct xmi_energy_continuous, selected_index) = *xec;
	g_array_sort(single_box->parent_box->continuous_array, xmi_cmp_struct_xmi_energy_continuous);
}

static void scale_discrete_energies(XmiMsimGuiEnergiesSingleBox *single_box, double value, GArray *selected_indices) {
	GtkTreeIter iter;
	guint i;

	for (i = 0 ; i < selected_indices->len ; i++){
		GtkTreePath *path = gtk_tree_path_new_from_indices(g_array_index(selected_indices, int, i), -1);
		gtk_tree_model_get_iter(GTK_TREE_MODEL(single_box->store), &iter, path);
		gtk_tree_path_free(path);
		double hor_intensity, ver_intensity;
		gtk_tree_model_get(GTK_TREE_MODEL(single_box->store), &iter,
			HOR_INTENSITY_COLUMN, &hor_intensity,
			VER_INTENSITY_COLUMN, &ver_intensity,
			-1);
		hor_intensity *= value;
		ver_intensity *= value;
		gtk_list_store_set(single_box->store, &iter,
			HOR_INTENSITY_COLUMN, hor_intensity,
			VER_INTENSITY_COLUMN, ver_intensity,
			-1);

		g_array_index(single_box->parent_box->discrete_array, struct xmi_energy_discrete, i).horizontal_intensity *= value;
		g_array_index(single_box->parent_box->discrete_array, struct xmi_energy_discrete, i).vertical_intensity *= value;
	}
}

static void scale_continuous_energies(XmiMsimGuiEnergiesSingleBox *single_box, double value, GArray *selected_indices) {
	GtkTreeIter iter;
	guint i;

	for (i = 0 ; i < selected_indices->len ; i++){
		GtkTreePath *path = gtk_tree_path_new_from_indices(g_array_index(selected_indices, int, i), -1);
		gtk_tree_model_get_iter(GTK_TREE_MODEL(single_box->store), &iter, path);
		gtk_tree_path_free(path);
		double hor_intensity, ver_intensity;
		gtk_tree_model_get(GTK_TREE_MODEL(single_box->store), &iter,
			HOR_INTENSITY_COLUMN, &hor_intensity,
			VER_INTENSITY_COLUMN, &ver_intensity,
			-1);
		hor_intensity *= value;
		ver_intensity *= value;
		gtk_list_store_set(single_box->store, &iter,
			HOR_INTENSITY_COLUMN, hor_intensity,
			VER_INTENSITY_COLUMN, ver_intensity,
			-1);

		g_array_index(single_box->parent_box->continuous_array, struct xmi_energy_continuous, i).horizontal_intensity *= value;
		g_array_index(single_box->parent_box->continuous_array, struct xmi_energy_continuous, i).vertical_intensity *= value;
	}
}

static void delete_energies(XmiMsimGuiEnergiesSingleBox *single_box, GArray *data_array, GArray *selected_indices) {
	int i;
	GtkTreeIter iter;

	for (i = selected_indices->len - 1 ; i >= 0 ; i--) {
		int index = g_array_index(selected_indices, int, i);
		GtkTreePath *path = gtk_tree_path_new_from_indices(index, -1);
		gtk_tree_model_get_iter(GTK_TREE_MODEL(single_box->store), &iter, path);
		gtk_tree_path_free(path);
		gtk_list_store_remove(single_box->store, &iter);
		g_array_remove_index(data_array, index);
	}
}

static void scale_entry_changed_cb(GtkWidget *scaleEntry, GtkWidget *okButton) {
	double value;
	const char *textPtr, *lastPtr;
	char *endPtr;

	textPtr = gtk_entry_get_text(GTK_ENTRY(scaleEntry));
	value = strtod(textPtr, &endPtr);
	lastPtr = textPtr + strlen(textPtr);

	GtkStyleContext *style_context = gtk_widget_get_style_context(GTK_WIDGET(scaleEntry));

	if (strlen(textPtr) == 0 || lastPtr != endPtr || value <= 0.0) {
      		gtk_style_context_add_class(style_context, "red");
		gtk_widget_set_sensitive(okButton, FALSE);
	}
	else {
      		gtk_style_context_remove_class(style_context, "red");
		gtk_widget_set_sensitive(okButton, TRUE);
	}
}

static void add_discrete_energies(XmiMsimGuiEnergiesSingleBox *single_box, gboolean clear, guint n_energies, struct xmi_energy_discrete *xed) {
	GtkTreeIter iter;
	guint i;
	GtkListStore *store = single_box->store;
	GArray *array = single_box->parent_box->discrete_array;

	if (clear) {
		gtk_list_store_clear(store);
		g_array_ref(array);
		g_array_free(array, TRUE);
	}

	for (i = 0 ; i < n_energies ; i++) {
		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
			ENERGY_COLUMN, xed[i].energy,
			HOR_INTENSITY_COLUMN, xed[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, xed[i].vertical_intensity,
			SIGMA_X_COLUMN, xed[i].sigma_x,
			SIGMA_XP_COLUMN, xed[i].sigma_xp,
			SIGMA_Y_COLUMN, xed[i].sigma_y,
			SIGMA_YP_COLUMN, xed[i].sigma_yp,
			DISTRIBUTION_TYPE_COLUMN, xed[i].distribution_type,
			SCALE_PARAMETER_COLUMN, xed[i].scale_parameter,
			-1);
	}
	if (n_energies) {
		g_array_append_vals(array, xed, n_energies);
		g_array_sort(array, xmi_cmp_struct_xmi_energy_discrete);
	}
}

static void add_continuous_energies(XmiMsimGuiEnergiesSingleBox *single_box, gboolean clear, guint n_energies, struct xmi_energy_continuous *xec) {
	GtkTreeIter iter;
	guint i;
	GtkListStore *store = single_box->store;
	GArray *array = single_box->parent_box->continuous_array;

	if (clear) {
		gtk_list_store_clear(store);
		g_array_ref(array);
		g_array_free(array, TRUE);
	}

	for (i = 0 ; i < n_energies ; i++) {
		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
			ENERGY_COLUMN, xec[i].energy,
			HOR_INTENSITY_COLUMN, xec[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, xec[i].vertical_intensity,
			SIGMA_X_COLUMN, xec[i].sigma_x,
			SIGMA_XP_COLUMN, xec[i].sigma_xp,
			SIGMA_Y_COLUMN, xec[i].sigma_y,
			SIGMA_YP_COLUMN, xec[i].sigma_yp,
			-1);
	}
	if (n_energies) {
		g_array_append_vals(array, xec, n_energies);
		g_array_sort(array, xmi_cmp_struct_xmi_energy_continuous);
	}
}

static void energy_add_button_clicked_cb(GtkWidget *button, XmiMsimGuiEnergiesSingleBox *single_box) {

	GtkWidget *dialog;

	if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
		dialog = xmi_msim_gui_discrete_energy_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))), XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_ADD);
	}
	else if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS) {
		dialog = xmi_msim_gui_continuous_energy_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))), XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_ADD);
	}

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
			struct xmi_energy_discrete *xed = xmi_msim_gui_discrete_energy_dialog_get_discrete_energy(XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG(dialog));
			//check if the energy is not present already
			if (single_box->parent_box->discrete_array->len > 0 && bsearch(xed, single_box->parent_box->discrete_array->data, single_box->parent_box->discrete_array->len, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
				GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy line: the energy already exists in the list of lines.");
				gtk_dialog_run(GTK_DIALOG(error_dialog));
				gtk_widget_destroy(error_dialog);
			}
			else {
				// modify store and GArray
				add_discrete_energies(single_box, FALSE, 1, xed);
				// emit signal for UndoManager
				g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "addition of discrete energy");
			}
			g_free(xed);
		}
		else if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS) {
			struct xmi_energy_continuous *xec = xmi_msim_gui_continuous_energy_dialog_get_continuous_energy(XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG(dialog));
			//check if the energy is not present already
			if (single_box->parent_box->continuous_array->len > 0 && bsearch(xec, single_box->parent_box->continuous_array->data, single_box->parent_box->continuous_array->len, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
				GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy interval: the energy already exists in the list of intervals.");
				gtk_dialog_run(GTK_DIALOG(error_dialog));
				gtk_widget_destroy(error_dialog);
			}
			else {
				// add to store and GArray
				add_continuous_energies(single_box, FALSE, 1, xec);
				// emit signal for UndoManager
				g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "addition of continuous energy");
			}
			g_free(xec);
		}
	}
	gtk_widget_destroy(dialog);
}

static void energy_edit_button_clicked_cb(GtkWidget *button, XmiMsimGuiEnergiesSingleBox *single_box) {

	GtkWidget *dialog;

	// get currently selected index
	GArray *selected_indices = xmi_msim_gui_utils_tree_view_get_selected_indices(GTK_TREE_VIEW(single_box->tree));
	int selected_index = g_array_index(selected_indices, int, 0); 
	g_array_free(selected_indices, TRUE);

	if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
		dialog = xmi_msim_gui_discrete_energy_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))), XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_EDIT);
		xmi_msim_gui_discrete_energy_dialog_set_discrete_energy(XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG(dialog), &g_array_index(single_box->parent_box->discrete_array, struct xmi_energy_discrete, selected_index));
	}
	else if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS) {
		dialog = xmi_msim_gui_continuous_energy_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))), XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_EDIT);
		xmi_msim_gui_continuous_energy_dialog_set_continuous_energy(XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG(dialog), &g_array_index(single_box->parent_box->continuous_array, struct xmi_energy_continuous, selected_index));
	}

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
			struct xmi_energy_discrete *xed = xmi_msim_gui_discrete_energy_dialog_get_discrete_energy(XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG(dialog));
			// check if the new xed is different from the old one
			struct xmi_energy_discrete *xed_current = &g_array_index(single_box->parent_box->discrete_array, struct xmi_energy_discrete, selected_index);

			if (xmi_equal_energy_discrete(xed, xed_current)) {
				// do nothing
			}
			else if (bsearch(xed, single_box->parent_box->discrete_array->data, single_box->parent_box->discrete_array->len, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
				// check if the energy is not present already
				GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not modify energy line: the energy already exists in the list of lines.");
				gtk_dialog_run(GTK_DIALOG(error_dialog));
				gtk_widget_destroy(error_dialog);
			}
			else {
				// modify store and GArray
				edit_discrete_energy(single_box, selected_index, xed);
				// emit signal for UndoManager
				g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "editing of discrete energy");
			}
			g_free(xed);
		}
		else if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS) {
			struct xmi_energy_continuous *xec = xmi_msim_gui_continuous_energy_dialog_get_continuous_energy(XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG(dialog));
			// check if the new xec is different from the old one
			struct xmi_energy_continuous *xec_current = &g_array_index(single_box->parent_box->continuous_array, struct xmi_energy_continuous, selected_index);

			if (xmi_equal_energy_continuous(xec, xec_current)) {
				// do nothing
			}
			else if (bsearch(xec, single_box->parent_box->continuous_array->data, single_box->parent_box->continuous_array->len, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
				// check if the energy is not present already
				GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not modify energy density: the energy already exists in the list.");
				gtk_dialog_run(GTK_DIALOG(error_dialog));
				gtk_widget_destroy(error_dialog);
			}
			else {
				// modify store and GArray
				edit_continuous_energy(single_box, selected_index, xec);
				// emit signal for UndoManager
				g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "editing of continuous energy");
			}
			g_free(xec);
		}
	}
	gtk_widget_destroy(dialog);
}

static void energy_scale_button_clicked_cb(GtkWidget *button, XmiMsimGuiEnergiesSingleBox *single_box) {

	//Launch dialog to select the scale factor
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Intensity scale factor", GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, "_Ok", GTK_RESPONSE_ACCEPT, "_Cancel", GTK_RESPONSE_REJECT, NULL);

	GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	GtkWidget *label = gtk_label_new("Intensity scale factor");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
	GtkWidget *entry = gtk_entry_new();
  	gtk_widget_set_name(entry, "color_entry");
	gtk_editable_set_editable(GTK_EDITABLE(entry), TRUE);
	gtk_box_pack_end(GTK_BOX(hbox), entry, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(content_area), hbox, FALSE, FALSE,0);
	gtk_container_set_border_width(GTK_CONTAINER(dialog),5);
	gtk_widget_show_all(hbox);
	gtk_widget_set_size_request(dialog, 300, -1);

	GtkWidget *okButton = gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
	gtk_widget_set_sensitive(okButton, FALSE);
	g_signal_connect(G_OBJECT(entry), "changed", G_CALLBACK(scale_entry_changed_cb), (gpointer) okButton);

	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
	gtk_widget_set_can_default(okButton, TRUE);
	gtk_widget_grab_default(okButton);
	int rv = gtk_dialog_run(GTK_DIALOG(dialog));

	if (rv != GTK_RESPONSE_ACCEPT) {
		gtk_widget_destroy(dialog);
		return;
	}

	const char *textPtr;
	double value;
	int i;

	textPtr = gtk_entry_get_text(GTK_ENTRY(entry));
	value = strtod(textPtr, NULL);
	gtk_widget_destroy(dialog);
	GtkTreeIter iter;

	GArray *selected_indices = xmi_msim_gui_utils_tree_view_get_selected_indices(GTK_TREE_VIEW(single_box->tree));

	if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
		// modify store and GArray
		scale_discrete_energies(single_box, value, selected_indices);
		// emit signal for UndoManager
		g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "scaling of discrete energies");
	}
	else if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS) {
		scale_continuous_energies(single_box, value, selected_indices);
		g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "scaling of continuous energies");
	}

	g_array_free(selected_indices, TRUE);
}

static void energy_clear_button_clicked_cb(GtkWidget *button, XmiMsimGuiEnergiesSingleBox *single_box) {

	if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
		// modify store and GArray
		add_discrete_energies(single_box, TRUE, 0, NULL);
		// emit signal for UndoManager
		g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "clearing of all discrete energies");
	}
	else if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS) {
		// modify store and GArray
		add_continuous_energies(single_box, TRUE, 0, NULL);
		// emit signal for UndoManager
		g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "clearing of all continuous energies");
	}

}

static void energy_delete_button_clicked_cb(GtkWidget *button, XmiMsimGuiEnergiesSingleBox *single_box) {
	int i;
	GtkTreeIter iter;
	GArray *selected_indices = xmi_msim_gui_utils_tree_view_get_selected_indices(GTK_TREE_VIEW(single_box->tree));

	if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
		// modify store and GArray
		delete_energies(single_box, single_box->parent_box->discrete_array, selected_indices);
		// emit signal for UndoManager
		g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "deleting of discrete energies");
	}
	else if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS) {
		// modify store and GArray
		delete_energies(single_box, single_box->parent_box->continuous_array, selected_indices);
		// emit signal for UndoManager
		g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "deleting of continuous energies");
	}

	g_array_free(selected_indices, TRUE);
}

static void energy_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, XmiMsimGuiEnergiesSingleBox *single_box) {
	energy_edit_button_clicked_cb(NULL, single_box);	
}

static void energy_right_click_menu_delete_cb(GtkWidget *button, XmiMsimGuiEnergiesSingleBox *single_box) {
	energy_delete_button_clicked_cb(NULL, single_box);
}

static void energy_right_click_menu_select_all_cb(GtkWidget *button, GtkWidget *tree) {
	GtkTreeSelection *selection;
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_select_all(selection);
}

static gboolean energy_backspace_key_clicked(GtkWidget *widget, GdkEventKey *event, XmiMsimGuiEnergiesSingleBox *single_box) {
	if (event->keyval == gdk_keyval_from_name("BackSpace") &&
		gtk_tree_selection_count_selected_rows(gtk_tree_view_get_selection(GTK_TREE_VIEW(single_box->tree))) > 0) {
		energy_delete_button_clicked_cb(widget, single_box);
		return TRUE;
	}
	return FALSE;
}

static void create_popup_menu(GtkWidget *tree, GdkEventButton *event, XmiMsimGuiEnergiesSingleBox *single_box) {
	GtkWidget *menu, *menuitem;

	menu = gtk_menu_new();
	menuitem = gtk_menu_item_new_with_mnemonic("Delete");

	//count how many rows are selected
	GtkTreeSelection *selection;
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));

	if (gtk_tree_selection_count_selected_rows(selection) == 0) {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}
	else {
		g_signal_connect(menuitem, "activate", G_CALLBACK(energy_right_click_menu_delete_cb), single_box);
	}

	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	menuitem = gtk_menu_item_new_with_mnemonic("Select All");
	//count how many rows are in the treeview
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree));

	if (gtk_tree_model_iter_n_children(model, NULL) == 0) {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}
	else {
		g_signal_connect(menuitem, "activate", G_CALLBACK(energy_right_click_menu_select_all_cb), (gpointer) tree);
	}

	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	gtk_widget_show_all(menu);
#if GTK_CHECK_VERSION(3, 22, 0)
	gtk_menu_popup_at_pointer(GTK_MENU(menu), (const GdkEvent *) event);
#else
	gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, event != NULL ? event->button : 0, gdk_event_get_time((GdkEvent *) event));
#endif
}

static gboolean energy_right_click_cb(GtkWidget *tree, GdkEventButton *event, XmiMsimGuiEnergiesSingleBox *single_box) {
	if (event->type == GDK_BUTTON_PRESS && event->button == 3) {
		//if clicked layer is not selected -> select it
		GtkTreeSelection *selection;
		selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
		GtkTreePath *path;
		if (gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(tree), (gint) event->x, (gint) event->y, &path, NULL, NULL, NULL) &&
		    !gtk_tree_selection_path_is_selected(selection, path)) {
			gtk_tree_selection_select_path(selection, path);
			gtk_tree_path_free(path);
		}
		create_popup_menu(tree, event, single_box);
		return TRUE;
	}
	return FALSE;
}

static gboolean energy_popup_menu_cb(GtkWidget *tree, XmiMsimGuiEnergiesSingleBox *single_box) {
	create_popup_menu(tree, NULL, single_box);

	return TRUE;
}

static void row_deleted_or_inserted_cb(GtkTreeModel *tree_model, XmiMsimGuiEnergiesSingleBox *single_box) {
	gint kids = gtk_tree_model_iter_n_children(tree_model, NULL);

	if (kids > 0) {
		gtk_widget_set_sensitive(single_box->clear_button, TRUE);
	}
	else {
		gtk_widget_set_sensitive(single_box->clear_button, FALSE);
	}
}

static void row_deleted_cb(GtkTreeModel *tree_model, GtkTreePath *path, XmiMsimGuiEnergiesSingleBox *single_box) {
       row_deleted_or_inserted_cb(tree_model, single_box);
}

static void row_inserted_cb (GtkTreeModel *tree_model, GtkTreePath  *path, GtkTreeIter *iter, XmiMsimGuiEnergiesSingleBox *single_box) {
       row_deleted_or_inserted_cb(tree_model, single_box);
}

static void radio_button_toggled_cb(GtkToggleButton *button, GtkWidget *spinner){
	if (gtk_toggle_button_get_active(button))
		gtk_widget_set_sensitive(spinner, TRUE);
	else
		gtk_widget_set_sensitive(spinner, FALSE);
}

static GArray* xmi_read_energies_from_ascii_file_discrete(const gchar *filename, unsigned int start_line, unsigned int nlines, GError **error) {
	GArray *result = g_array_new(FALSE, FALSE, sizeof(struct xmi_energy_discrete));
	g_array_ref(result);

	GFile *file = g_file_new_for_path(filename);
	GFileInputStream *file_stream = g_file_read(file, NULL, error);
	g_object_unref(file);
	if (!file_stream)
		return result;

	GDataInputStream *data_stream = g_data_input_stream_new(G_INPUT_STREAM(file_stream));
	g_data_input_stream_set_newline_type(data_stream, G_DATA_STREAM_NEWLINE_TYPE_ANY);

	//read line per line...
	char *line = (char *) 1;
	size_t linecap = 0;
	int values;
	double energy;
	double horizontal_intensity;
	double vertical_intensity;
	double sigma_x;
	double sigma_y;
	double sigma_xp;
	double sigma_yp;
	struct xmi_energy_discrete temp;
	unsigned int lines_read = 0;

	while (line) {
		gsize linelen = -1;
		line = g_data_input_stream_read_line(data_stream, &linelen, NULL, error);
		if (*error != NULL) {
			g_object_unref(file_stream);
			g_object_unref(data_stream);
			g_array_ref(result);
			g_array_free(result, TRUE);
			return result;
		}
		else if (line == NULL) {
			// end of stream
			break;
		}
		lines_read++;
		if (lines_read < start_line)
			continue;
		//ignore empty lines
		if (linelen == 0 || strlen(g_strstrip(line)) == 0) {
			nlines--;
			continue;
		}
		if (nlines == result->len)
			break;
		values = sscanf(line,"%lg %lg %lg %lg %lg %lg %lg", &energy, &horizontal_intensity, &vertical_intensity, &sigma_x, &sigma_y, &sigma_xp, &sigma_yp);
		g_free(line);
		temp.sigma_x = 0.0;
		temp.sigma_y = 0.0;
		temp.sigma_xp = 0.0;
		temp.sigma_yp = 0.0;
		temp.distribution_type = XMI_DISCRETE_MONOCHROMATIC;
		temp.scale_parameter = 0.0;

		switch (values) {
			case 7:
				temp.sigma_x = sigma_x;
				temp.sigma_y = sigma_y;
				temp.sigma_xp = sigma_xp;
				temp.sigma_yp = sigma_yp;
			case 3:
				temp.horizontal_intensity = horizontal_intensity;
				temp.vertical_intensity = vertical_intensity;
				temp.energy = energy;
				break;
			case 2:
				temp.horizontal_intensity = horizontal_intensity/2.0;
				temp.vertical_intensity = horizontal_intensity/2.0;
				temp.energy = energy;
				break;
			default:
				g_set_error(error, XMI_MSIM_GUI_ENERGIES_BOX_ERROR, XMI_MSIM_GUI_ENERGIES_BOX_ERROR_IMPORT_FILE, "Syntax error in file %s at line %i after reading %u lines of %i requested\nNumber of columns must be 2, 3 or 7!\n", filename, lines_read, result->len, nlines);
				g_object_unref(file_stream);
				g_object_unref(data_stream);
				g_array_ref(result);
				g_array_free(result, TRUE);
				return result;
		};
		//ignore the useless lines
		if (temp.energy <= 0.0000000001 || temp.energy > 200.0 || temp.horizontal_intensity + temp.vertical_intensity <= 0.000000001 || temp.horizontal_intensity < -0.0000000001 || temp.vertical_intensity < -0.0000000001) {
			nlines--;
			continue;
		}
		if (nlines == result->len)
			break;
		if (result->len == 0) {
			g_array_append_val(result, temp);
		}
		else {
			//make sure the value was not already in the list
			struct xmi_energy_discrete *find_res;
#ifdef G_OS_WIN32
			unsigned int result_len = result->len;
			if((find_res = (struct xmi_energy_discrete *) _lfind(&temp, result->data, &result_len, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete)) == NULL)
#else
			size_t result_len = result->len;
			if((find_res = (struct xmi_energy_discrete *) lfind(&temp, result->data, &result_len, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete)) == NULL)
#endif
				{
				g_array_append_val(result, temp);
			}
			else  {
				g_info("xmi_read_energies_from_ascii_file_discrete: Duplicate discrete line energy detected\nAdding to existing discrete line");
				find_res->horizontal_intensity += temp.horizontal_intensity;
				find_res->vertical_intensity += temp.vertical_intensity;
			}
		}
		if (result->len == nlines)
			break;
	}

	g_object_unref(file_stream);
	g_object_unref(data_stream);

	return result;
}

static GArray* xmi_read_energies_from_ascii_file_continuous(const gchar *filename, unsigned int start_line, unsigned int nlines, GError **error) {
	GArray *result = g_array_new(FALSE, FALSE, sizeof(struct xmi_energy_continuous));
	g_array_ref(result);

	GFile *file = g_file_new_for_path(filename);
	GFileInputStream *file_stream = g_file_read(file, NULL, error);
	g_object_unref(file);
	if (!file_stream)
		return result;

	GDataInputStream *data_stream = g_data_input_stream_new(G_INPUT_STREAM(file_stream));
	g_data_input_stream_set_newline_type(data_stream, G_DATA_STREAM_NEWLINE_TYPE_ANY);

	//read line per line...
	char *line = (char *) 1;
	size_t linecap = 0;
	int values;
	double energy;
	double horizontal_intensity;
	double vertical_intensity;
	double sigma_x;
	double sigma_y;
	double sigma_xp;
	double sigma_yp;
	struct xmi_energy_continuous temp;
	unsigned int lines_read = 0;

	while (line) {
		gsize linelen = -1;
		line = g_data_input_stream_read_line(data_stream, &linelen, NULL, error);
		if (*error != NULL) {
			g_object_unref(file_stream);
			g_object_unref(data_stream);
			g_array_ref(result);
			g_array_free(result, TRUE);
			return result;
		}
		else if (line == NULL) {
			// end of stream
			break;
		}
		lines_read++;
		if (lines_read < start_line)
			continue;
		//ignore empty lines
		if (linelen == 0 || strlen(g_strstrip(line)) == 0) {
			nlines--;
			continue;
		}
		if (nlines == result->len)
			break;
		values = sscanf(line,"%lg %lg %lg %lg %lg %lg %lg", &energy, &horizontal_intensity, &vertical_intensity, &sigma_x, &sigma_y, &sigma_xp, &sigma_yp);
		g_free(line);
		temp.sigma_x = 0.0;
		temp.sigma_y = 0.0;
		temp.sigma_xp = 0.0;
		temp.sigma_yp = 0.0;

		switch (values) {
			case 7:
				temp.sigma_x = sigma_x;
				temp.sigma_y = sigma_y;
				temp.sigma_xp = sigma_xp;
				temp.sigma_yp = sigma_yp;
			case 3:
				temp.horizontal_intensity = horizontal_intensity;
				temp.vertical_intensity = vertical_intensity;
				temp.energy = energy;
				break;
			case 2:
				temp.horizontal_intensity = horizontal_intensity/2.0;
				temp.vertical_intensity = horizontal_intensity/2.0;
				temp.energy = energy;
				break;
			default:
				g_set_error(error, XMI_MSIM_GUI_ENERGIES_BOX_ERROR, XMI_MSIM_GUI_ENERGIES_BOX_ERROR_IMPORT_FILE, "Syntax error in file %s at line %i after reading %u lines of %i requested\nNumber of columns must be 2, 3 or 7!\n", filename, lines_read, result->len, nlines);
				g_object_unref(file_stream);
				g_object_unref(data_stream);
				g_array_ref(result);
				g_array_free(result, TRUE);
				return result;
		};
		//ignore the useless lines
		if (temp.energy <= 0.0 || temp.energy > 200.0 || temp.horizontal_intensity + temp.vertical_intensity < 0.0 || temp.horizontal_intensity < 0.0 || temp.vertical_intensity < 0.0) {
			nlines--;
			continue;
		}
		if (nlines == result->len)
			break;
		if (result->len == 0) {
			g_array_append_val(result, temp);
		}
		else {
			//make sure the value was not already in the list
			struct xmi_energy_continuous *find_res;
#ifdef G_OS_WIN32
			unsigned int result_len = result->len;
			if((find_res = (struct xmi_energy_continuous *) _lfind(&temp, result->data, &result_len, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous)) == NULL)
#else
			size_t result_len = result->len;
			if((find_res = (struct xmi_energy_continuous *) lfind(&temp, result->data, &result_len, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous)) == NULL)
#endif
				{
				g_array_append_val(result, temp);
			}
			else  {
				g_set_error(error, XMI_MSIM_GUI_ENERGIES_BOX_ERROR, XMI_MSIM_GUI_ENERGIES_BOX_ERROR_IMPORT_FILE, "Duplicate continuous energies found in %s!\n", filename);
				g_object_unref(file_stream);
				g_object_unref(data_stream);
				g_array_ref(result);
				g_array_free(result, TRUE);
				return result;
			}
		}
		if (result->len == nlines)
			break;
	}

	g_object_unref(file_stream);
	g_object_unref(data_stream);

	return result;
}

static void energy_import_button_clicked_cb(GtkWidget *widget, XmiMsimGuiEnergiesSingleBox *single_box) {

	//first launch a message box
	GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "Import spectrum from file");
	gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
	"A file will be considered suitable if it adheres to the following rules:\n"
	"* Files must be ascii files consisting of rows with either 2, 3 or 7 values.\n"
	"* First value must be the energy (expressed in keV).\n "
	"* Second value must be the intensity: if there are only two values, it is assumed to be unpolarized.\n"
	"* If three values are found on a line, then the second and third values are assumed to correspond to the horizontal and vertical polarized intensities.\n"
	"* Seven values on a line are considered to be identical to the three values case with additionally the source size x and y, as well as the source divergence x and y.\n"
	"* Empty lines are ignored.\n"
	);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) != GTK_RESPONSE_CLOSE) {
		gtk_widget_destroy(dialog);
		return;
	}
	gtk_widget_destroy(dialog);

	//open filechooser without filters
	XmiMsimGuiFileChooserDialog *file_dialog = xmi_msim_gui_file_chooser_dialog_new("Open File",
                 GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))),
                 GTK_FILE_CHOOSER_ACTION_OPEN,
                 "_Open",
                 "_Cancel"
                 );
	xmi_msim_gui_file_chooser_dialog_set_modal(file_dialog, TRUE);

	//add widget
	GtkWidget *start_at_begin;
	GtkWidget *start_at_line;
	GtkWidget *start_at_line_spinner;
	GtkWidget *read_all_lines;
	GtkWidget *read_only_lines;
	GtkWidget *read_only_lines_spinner;
	GtkWidget *vbox, *hbox;

	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 1);

	start_at_begin = gtk_radio_button_new_with_label_from_widget(NULL,"Start at first line");
	start_at_line = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(start_at_begin), "Start at line:");
	GtkAdjustment *adj = GTK_ADJUSTMENT(gtk_adjustment_new(1, 1, 10000, 1, 10, 0));
	start_at_line_spinner = gtk_spin_button_new(adj, 1, 0);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(start_at_line_spinner), GTK_UPDATE_IF_VALID);
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 1);
	gtk_box_pack_start(GTK_BOX(hbox), start_at_line, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), start_at_line_spinner, FALSE, FALSE, 1);
	g_signal_connect(G_OBJECT(start_at_line), "toggled", G_CALLBACK(radio_button_toggled_cb), start_at_line_spinner);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(start_at_begin), TRUE);
	gtk_widget_set_sensitive(start_at_line_spinner, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), start_at_begin, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(vbox), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 1);

	read_all_lines = gtk_radio_button_new_with_label_from_widget(NULL, "Read all lines");
	read_only_lines = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(read_all_lines), "Number of lines to be read:");
	adj = GTK_ADJUSTMENT(gtk_adjustment_new(1, 1, 10000, 1, 10, 0));
	read_only_lines_spinner = gtk_spin_button_new(adj, 1, 0);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(read_only_lines_spinner), GTK_UPDATE_IF_VALID);
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 1);
	gtk_box_pack_start(GTK_BOX(hbox), read_only_lines, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), read_only_lines_spinner, FALSE, FALSE, 1);
	g_signal_connect(G_OBJECT(read_only_lines), "toggled", G_CALLBACK(radio_button_toggled_cb), read_only_lines_spinner);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(read_all_lines), TRUE);
	gtk_widget_set_sensitive(read_only_lines_spinner, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), read_all_lines, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);

	gtk_widget_show_all(vbox);
	gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(file_dialog), vbox);

	if (xmi_msim_gui_file_chooser_dialog_run(file_dialog) == GTK_RESPONSE_ACCEPT) {
		gchar *filename;

		int start_line, nlines;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(start_at_begin))) {
			start_line = 1;
		}
		else {
			start_line = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(start_at_line_spinner));
		}

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(read_all_lines))) {
			nlines = -1;
		}
		else {
			nlines = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(read_only_lines_spinner));
		}

		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(file_dialog));
		xmi_msim_gui_file_chooser_dialog_destroy(file_dialog);
		dialog = NULL;
		GArray *results;
		GError *error = NULL;
		
		if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
    			results = xmi_read_energies_from_ascii_file_discrete(filename, start_line, nlines, &error);
		}
		else if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS) {
    			results = xmi_read_energies_from_ascii_file_continuous(filename, start_line, nlines, &error);
		}

		if (results->len > 0) {
			//success
			g_debug("File %s read in successfully\n", filename);

			//now ask if we have to add or replace...
			int rv;
			if ((single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE && single_box->parent_box->discrete_array->len > 0) ||
			    (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS && single_box->parent_box->continuous_array->len > 0)) {
				dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "Add spectrum from file to current spectrum or replace it completely?");
				gtk_dialog_add_buttons(GTK_DIALOG(dialog), "_Add", GTK_RESPONSE_OK, "_Replace", GTK_RESPONSE_CANCEL, NULL);
				//this may not work on all platforms -> Mac OS X
				gtk_window_set_deletable(GTK_WINDOW(dialog), FALSE);

				rv = gtk_dialog_run (GTK_DIALOG (dialog));
				gtk_widget_destroy(dialog);
			}
			else {
				rv = GTK_RESPONSE_CANCEL;
			}
			if (rv == GTK_RESPONSE_OK) {
				//add
				guint i;
				if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
					for (i = 0 ; i < results->len ; i++) {
						if (bsearch(&g_array_index(results, struct xmi_energy_discrete, i), single_box->parent_box->discrete_array->data, single_box->parent_box->discrete_array->len, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
							dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy lines: one or more of the new energies exist already in the list of lines.");
							gtk_dialog_run(GTK_DIALOG(dialog));
							gtk_widget_destroy(dialog);
							g_array_unref(results);
							g_array_free(results, TRUE);
							return;
						}
					}
					// modify store and GArray
					add_discrete_energies(single_box, FALSE, results->len, (struct xmi_energy_discrete *) results->data);
					// emit signal for UndoManager
					g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "addition of imported discrete energies");
				}
				else if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS) {
					for (i = 0 ; i < results->len ; i++) {
						if (bsearch(&g_array_index(results, struct xmi_energy_continuous, i), single_box->parent_box->continuous_array->data, single_box->parent_box->continuous_array->len, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
							dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy lines: one or more of the new energies exist already in the list of lines.");
							gtk_dialog_run(GTK_DIALOG(dialog));
							gtk_widget_destroy(dialog);
							g_array_unref(results);
							g_array_free(results, TRUE);
							return;
						}
					}
					// modify store and GArray
					add_continuous_energies(single_box, FALSE, results->len, (struct xmi_energy_continuous *) results->data);
					// emit signal for UndoManager
					g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "addition of imported continuous energies");
				}
			}
			else if (rv == GTK_RESPONSE_CANCEL) {
				//replace -> no need to check for duplicates here
				if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
					// modify store and GArray
					add_discrete_energies(single_box, TRUE, results->len, (struct xmi_energy_discrete *) results->data);
					// emit signal for UndoManager
					g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "replacing with imported discrete energies");
				}
				else if (single_box->type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS) {
					// modify store and GArray
					add_continuous_energies(single_box, TRUE, results->len, (struct xmi_energy_continuous *) results->data);
					// emit signal for UndoManager
					g_signal_emit(single_box->parent_box, signals[CHANGED], 0, "replacing with imported continuous energies");
				}
			}
		}
		else {
			dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(single_box->parent_box))), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not import from %s!\n", filename);
			gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
			g_error_free(error);
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
		}
		g_free(filename);
		g_array_unref(results);
		g_array_free(results, TRUE);
  	}
	else {
		xmi_msim_gui_file_chooser_dialog_destroy(file_dialog);
	}
}

static GtkWidget *initialize_single_energies(XmiMsimGuiEnergiesBox *self, XmiMsimGuiEnergiesSingleBoxType type) {
	XmiMsimGuiEnergiesSingleBox *single_box = g_malloc(sizeof(XmiMsimGuiEnergiesSingleBox));
	single_box->parent_box = self;
	single_box->type = type;

	GtkWidget *rv = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(rv), FALSE);

	GtkListStore *store;

	if (type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE)
		store = gtk_list_store_new(NCOLUMNS_ENERGIES, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_INT, G_TYPE_DOUBLE);
	else if (type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS)
		store = gtk_list_store_new(NCOLUMNS_ENERGIES-2, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE);

	single_box->store = store;

	gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(store), ENERGY_COLUMN, energy_column_comparator, GINT_TO_POINTER(ENERGY_COLUMN), NULL);
	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(store), ENERGY_COLUMN, GTK_SORT_ASCENDING);

	GtkWidget *tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	g_object_unref(store);
	single_box->tree = tree;

	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Energy (keV)");
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(ENERGY_COLUMN), NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new();
	if (type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE)
		gtk_tree_view_column_set_title(column, "Horizontal intensity (ph/s)");
	else if (type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS)
		gtk_tree_view_column_set_title(column, "Horizontal intensity (ph/s/keV)");
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(HOR_INTENSITY_COLUMN), NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new();
	if (type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE)
		gtk_tree_view_column_set_title(column, "Vertical intensity (ph/s)");
	else if (type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS)
		gtk_tree_view_column_set_title(column, "Vertical intensity (ph/s/keV)");
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(VER_INTENSITY_COLUMN), NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma x (cm)");
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_X_COLUMN), NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma y (cm)");
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_Y_COLUMN), NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma xp (rad)");
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_XP_COLUMN), NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma yp (rad)");
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_YP_COLUMN), NULL);

	if (type == XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE) {
		renderer = gtk_cell_renderer_text_new();
		gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
		column = gtk_tree_view_column_new();
		gtk_tree_view_column_set_title(column, "Distribution type");
		gtk_tree_view_column_set_resizable(column, TRUE);
		gtk_tree_view_column_set_alignment(column, 0.5);
		gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
		gtk_tree_view_column_pack_start(column, renderer, TRUE);
		gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_distribution_type, NULL, NULL);

		renderer = gtk_cell_renderer_text_new();
		gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
		column = gtk_tree_view_column_new();
		gtk_tree_view_column_set_title(column, "Scale parameter (keV)");
		gtk_tree_view_column_set_resizable(column, TRUE);
		gtk_tree_view_column_set_alignment(column, 0.5);
		gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
		gtk_tree_view_column_pack_start(column, renderer, TRUE);
		gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_scale_parameter, NULL, NULL);
	}

	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_window, -1,230);
	gtk_container_add(GTK_CONTAINER(scrolled_window), tree);
	GtkWidget *tree_frame = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(tree_frame), scrolled_window);
	gtk_box_pack_start(GTK_BOX(rv), tree_frame, TRUE, TRUE, 3);

	GtkWidget *buttonbox;

	buttonbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	single_box->add_button = gtk_button_new_with_label("Add");
	single_box->edit_button = gtk_button_new_with_label("Edit");
	single_box->delete_button = gtk_button_new_with_label("Remove");

	gtk_box_pack_start(GTK_BOX(buttonbox), single_box->add_button, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), single_box->edit_button, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), single_box->delete_button, FALSE, FALSE, 3);
	GtkWidget *new_buttonbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_set_homogeneous(GTK_BOX(new_buttonbox), TRUE);
	gtk_box_pack_start(GTK_BOX(new_buttonbox), buttonbox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(rv), new_buttonbox, FALSE, FALSE, 2);

	buttonbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	single_box->import_button = gtk_button_new_with_label("Import");
	gtk_box_pack_start(GTK_BOX(buttonbox), single_box->import_button, FALSE, FALSE, 3);

	single_box->clear_button = gtk_button_new_with_label("Clear");
	gtk_box_pack_start(GTK_BOX(buttonbox), single_box->clear_button, FALSE, FALSE, 3);

	single_box->scale_button = gtk_button_new_with_label("Scale");
	gtk_box_pack_start(GTK_BOX(buttonbox), single_box->scale_button, FALSE, FALSE, 3);

	new_buttonbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_set_homogeneous(GTK_BOX(new_buttonbox), TRUE);
	gtk_box_pack_start(GTK_BOX(new_buttonbox), buttonbox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(rv), new_buttonbox, FALSE, FALSE, 2);

	gtk_widget_set_sensitive(single_box->edit_button, FALSE);
	gtk_widget_set_sensitive(single_box->delete_button, FALSE);
	gtk_widget_set_sensitive(single_box->scale_button, FALSE);
	gtk_widget_set_sensitive(single_box->clear_button, FALSE);

	// hook up signals
	GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
	g_signal_connect(G_OBJECT(selection), "changed", G_CALLBACK(energy_selection_changed_cb), (gpointer) single_box);
	g_signal_connect(G_OBJECT(single_box->add_button), "clicked", G_CALLBACK(energy_add_button_clicked_cb) , (gpointer) single_box);
	g_signal_connect(G_OBJECT(single_box->edit_button), "clicked", G_CALLBACK(energy_edit_button_clicked_cb) , (gpointer) single_box);
	g_signal_connect(G_OBJECT(single_box->delete_button), "clicked", G_CALLBACK(energy_delete_button_clicked_cb) , (gpointer) single_box);
	g_signal_connect(G_OBJECT(single_box->import_button), "clicked", G_CALLBACK(energy_import_button_clicked_cb), (gpointer) single_box);
	g_signal_connect(G_OBJECT(single_box->clear_button), "clicked", G_CALLBACK(energy_clear_button_clicked_cb), (gpointer) single_box);
	g_signal_connect(G_OBJECT(single_box->scale_button), "clicked", G_CALLBACK(energy_scale_button_clicked_cb), (gpointer) single_box);
	g_signal_connect(G_OBJECT(tree), "row-activated", G_CALLBACK(energy_row_activated_cb), (gpointer) single_box);
	g_signal_connect(G_OBJECT(tree), "key-press-event", G_CALLBACK(energy_backspace_key_clicked), (gpointer) single_box);
	g_signal_connect(G_OBJECT(tree), "button-press-event", G_CALLBACK(energy_right_click_cb), (gpointer) single_box);
	g_signal_connect(G_OBJECT(tree), "popup-menu", G_CALLBACK(energy_popup_menu_cb), (gpointer) single_box);
	g_signal_connect(G_OBJECT(store), "row-inserted", G_CALLBACK(row_inserted_cb), (gpointer) single_box);
	g_signal_connect(G_OBJECT(store), "row-deleted", G_CALLBACK(row_deleted_cb), (gpointer) single_box);

	return rv;
}

GtkWidget* xmi_msim_gui_energies_box_new(void) {
	return GTK_WIDGET(g_object_new(XMI_MSIM_GUI_TYPE_ENERGIES_BOX, NULL));
}

GQuark xmi_msim_gui_energies_box_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-gui-energies-box-error-quark");
}

void xmi_msim_gui_energies_box_set_excitation(XmiMsimGuiEnergiesBox *self, struct xmi_excitation *excitation) {
	add_discrete_energies(self->discrete_box, TRUE, excitation->n_discrete, excitation->discrete);
	add_continuous_energies(self->continuous_box, TRUE, excitation->n_continuous, excitation->continuous);
}

struct xmi_excitation* xmi_msim_gui_energies_box_get_excitation(XmiMsimGuiEnergiesBox *self) {
	struct xmi_excitation *exc = g_malloc(sizeof(struct xmi_excitation));
	exc->n_discrete = self->discrete_array->len;
	exc->discrete = g_memdup(self->discrete_array->data, sizeof(struct xmi_energy_discrete) * self->discrete_array->len);
	exc->n_continuous = self->continuous_array->len;
	exc->continuous = g_memdup(self->continuous_array->data, sizeof(struct xmi_energy_continuous) * self->continuous_array->len);

	return exc;
}

