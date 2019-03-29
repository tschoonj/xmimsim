/*
Copyright (C) 2017 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-xmsi-selection-scrolled-window.h"
#include <string.h>
#include <xraylib.h>
#include "xmi_gobject.h"

struct _XmiMsimGuiXmsiSelectionScrolledWindow {
	GtkScrolledWindow parent_instance;
	GtkTreeView *treeview;
	gboolean with_colors;
	xmi_input *input;
};

struct _XmiMsimGuiXmsiSelectionScrolledWindowClass
{
	GtkScrolledWindowClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiXmsiSelectionScrolledWindow, xmi_msim_gui_xmsi_selection_scrolled_window, GTK_TYPE_SCROLLED_WINDOW)

enum {
	PROP_0,
	PROP_INPUT,
	PROP_WITH_COLOR,
	N_PROPERTIES
};

static GParamSpec *props[N_PROPERTIES] = {NULL, };

static void xmi_msim_gui_xmsi_selection_scrolled_window_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_xmsi_selection_scrolled_window_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_xmsi_selection_scrolled_window_finalize(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_xmsi_selection_scrolled_window_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_xmsi_selection_scrolled_window_set_property(GObject          *object,
                                                guint             prop_id,
                                                const GValue     *value,
                                                GParamSpec       *pspec);

static void xmi_msim_gui_xmsi_selection_scrolled_window_get_property(GObject          *object,
                                                guint             prop_id,
                                                GValue     *value,
                                                GParamSpec       *pspec);

static void xmi_msim_gui_xmsi_selection_scrolled_window_class_init(XmiMsimGuiXmsiSelectionScrolledWindowClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_xmsi_selection_scrolled_window_dispose;
	object_class->finalize = xmi_msim_gui_xmsi_selection_scrolled_window_finalize;
	object_class->set_property = xmi_msim_gui_xmsi_selection_scrolled_window_set_property;
	object_class->get_property = xmi_msim_gui_xmsi_selection_scrolled_window_get_property;

	props[PROP_INPUT] = g_param_spec_boxed(
		"xmi-input",
		"xmi-input",
		"xmi-input",
		XMI_MSIM_TYPE_INPUT,
    		G_PARAM_READWRITE 
	);

	props[PROP_WITH_COLOR] = g_param_spec_boolean(
		"with-colors",
		"with-colors",
		"with-colors",
		TRUE,
    		G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY
	);

	g_object_class_install_properties(object_class, N_PROPERTIES, props);
}

static GtkCellRenderer* get_cell_renderer(XmiMsimGuiXmsiSelectionScrolledWindow *self, gint column_nr) {
	GtkTreeViewColumn *column = gtk_tree_view_get_column(self->treeview, column_nr);
	g_return_val_if_fail(column != NULL, NULL);

	GList *renderers = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(column));
	g_return_val_if_fail(renderers != NULL, NULL);

	GtkCellRenderer *rv = renderers->data;
	g_list_free(renderers);

	return rv;
}

static void update_colors(XmiMsimGuiXmsiSelectionScrolledWindow *self) {
	GtkCellRenderer *renderer1 = get_cell_renderer(self, 0);
	GtkCellRenderer *renderer2 = get_cell_renderer(self, 1);

	if (self->with_colors) {
		g_debug("update_colors: TRUE");
		g_object_set(renderer1, "cell-background", "Chartreuse", NULL);
		g_object_set(renderer2, "cell-background", "Chartreuse", NULL);
	}
	else {
		g_debug("update_colors: FALSE");
		g_object_set(renderer1, "cell-background", NULL, NULL);
		g_object_set(renderer2, "cell-background", NULL, NULL);
	}
}

static void xmi_msim_gui_xmsi_selection_scrolled_window_init(XmiMsimGuiXmsiSelectionScrolledWindow *self) {
	self->with_colors = TRUE;

	GtkTreeStore *model = gtk_tree_store_new(INPUT_N_COLUMNS, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_STRING, G_TYPE_INT);
	GtkWidget *treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(model));
	self->treeview = GTK_TREE_VIEW(treeview);
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Parameter", renderer, "text", INPUT_PARAMETER_COLUMN, NULL);
	gtk_tree_view_column_add_attribute(column, renderer, "cell-background-set", INPUT_SELECTABLE_COLUMN);
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Value", renderer, "text", INPUT_VALUE_COLUMN, NULL);
	gtk_tree_view_column_add_attribute(column, renderer, "cell-background-set", INPUT_SELECTABLE_COLUMN);
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), column);

	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(self), treeview);

	gtk_widget_set_hexpand(GTK_WIDGET(self), TRUE);
	gtk_widget_set_vexpand(GTK_WIDGET(self), TRUE);
	gtk_container_set_border_width(GTK_CONTAINER(self), 5);
	gtk_widget_show_all(GTK_WIDGET(self));
}

static void populate_model(XmiMsimGuiXmsiSelectionScrolledWindow *self) {
	GtkTreeStore *model = GTK_TREE_STORE(gtk_tree_view_get_model(self->treeview));

	// clear whats in there
	gtk_tree_store_clear(model);

	if (self->input == NULL)
		return;
	
	g_debug("Populating model with data");

	int i, j;
	GtkTreeIter iter1, iter2, iter3, iter4, iter5;
	xmi_input *input = self->input;

	//general
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1,
		INPUT_PARAMETER_COLUMN, "general",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general",
		-1
	);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "outputfile",
		INPUT_VALUE_COLUMN, input->general->outputfile,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general/outputfile",
		-1
	);
	gchar *buffer, *buffer2;
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%li", input->general->n_photons_interval);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_photons_interval",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general/n_photons_interval",
		INPUT_ALLOWED_COLUMN, PARAMETER_LONG | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);

	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%li", input->general->n_photons_line);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_photons_line",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general/n_photons_line",
		INPUT_ALLOWED_COLUMN, PARAMETER_LONG | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);

	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%i", input->general->n_interactions_trajectory);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_interactions_trajectory",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general/n_interactions_trajectory",
		INPUT_ALLOWED_COLUMN, PARAMETER_INT | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);

	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "comments",
		INPUT_VALUE_COLUMN, input->general->comments,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general/n_interactions_comments",
		-1
	);

	//composition
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1,
		INPUT_PARAMETER_COLUMN, "composition",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/composition",
		-1
	);

	for (i = 0 ; i < input->composition->n_layers ; i++) {
		buffer = g_strdup_printf("layer %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]", i+1);
		gtk_tree_store_append(model, &iter2, &iter1);
		gtk_tree_store_set(model, &iter2,
			INPUT_PARAMETER_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		for (j = 0 ; j < input->composition->layers[i].n_elements ; j++) {
			buffer = g_strdup_printf("element %i", j+1);
			buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]/element[%i]", i+1, j+1);
			gtk_tree_store_append(model, &iter3, &iter2);
			gtk_tree_store_set(model, &iter3,
				INPUT_PARAMETER_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			g_free(buffer2);
			g_free(buffer);
			buffer = AtomicNumberToSymbol(input->composition->layers[i].Z[j], NULL);
			buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]/element[%i]/atomic_number", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, "atomic_number",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			xrlFree(buffer);
			g_free(buffer2);
			buffer = g_strdup_printf("%g", input->composition->layers[i].weight[j]*100.0);
			buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]/element[%i]/weight_fraction", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, "weight_fraction",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, input->composition->layers[i].n_elements > 1 ? TRUE : FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_WEIGHT_FRACTION,
				-1
			);
			g_free(buffer);
			g_free(buffer2);
		}

		buffer = g_strdup_printf("%g", input->composition->layers[i].density);
		buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]/density", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "density",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		buffer = g_strdup_printf("%g", input->composition->layers[i].thickness);
		buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]/thickness", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "thickness",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
	}
	buffer = g_strdup_printf("%i", input->composition->reference_layer);
	buffer2 = g_strdup_printf("/xmimsim/composition/reference_layer");
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "reference_layer",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, buffer2,
		-1
	);
	g_free(buffer);
	g_free(buffer2);

	//geometry
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1,
		INPUT_PARAMETER_COLUMN, "geometry",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry",
		-1
	);

	buffer = g_strdup_printf("%g", input->geometry->d_sample_source);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "d_sample_source",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/d_sample_source",
		-1
	);
	g_free(buffer);

	buffer = g_strdup_printf("%g", input->geometry->n_sample_orientation[0]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_sample_orientation_x",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_sample_orientation/x",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->n_sample_orientation[1]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_sample_orientation_y",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_sample_orientation/y",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->n_sample_orientation[2]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_sample_orientation_z",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_sample_orientation/z",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);

	buffer = g_strdup_printf("%g", input->geometry->p_detector_window[0]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "p_detector_window_x",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/p_detector_window/x",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->p_detector_window[1]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "p_detector_window_y",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/p_detector_window/y",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->p_detector_window[2]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "p_detector_window_z",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/p_detector_window/z",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);

	buffer = g_strdup_printf("%g", input->geometry->n_detector_orientation[0]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_detector_orientation_x",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_detector_orientation/x",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->n_detector_orientation[1]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_detector_orientation_y",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_detector_orientation/y",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->n_detector_orientation[2]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_detector_orientation_z",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_detector_orientation/z",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->area_detector);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "area_detector",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/area_detector",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->collimator_height);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "collimator_height",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, input->geometry->collimator_height > 0.0 && input->geometry->collimator_diameter > 0.0 ? TRUE : FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/collimator_height",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->collimator_diameter);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "collimator_diameter",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, input->geometry->collimator_height > 0.0 && input->geometry->collimator_diameter > 0.0 ? TRUE : FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/collimator_diameter",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->d_source_slit);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "d_source_slit",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/d_source_slit",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->slit_size_x);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "slit_size_x",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/slit_size/slit_size_x",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%g", input->geometry->slit_size_y);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "slit_size_y",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/slit_size/slit_size_y",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);

	//excitation
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1,
		INPUT_PARAMETER_COLUMN, "excitation",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/excitation",
		-1
	);
	for (i = 0 ; i < input->excitation->n_discrete ; i++) {
		gtk_tree_store_append(model, &iter2, &iter1);
		buffer = g_strdup_printf("discrete %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]", i+1);
		gtk_tree_store_set(model, &iter2,
			INPUT_PARAMETER_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->discrete[i].energy);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/energy", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "energy",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, input->excitation->n_discrete == 1 ? TRUE : FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->discrete[i].horizontal_intensity);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/horizontal_intensity", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "horizontal_intensity",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->discrete[i].vertical_intensity);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/vertical_intensity", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "vertical_intensity",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->discrete[i].sigma_x);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/sigma_x", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_x",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->discrete[i].sigma_y);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/sigma_y", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_y",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->discrete[i].sigma_xp);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/sigma_xp", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_xp",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->discrete[i].sigma_yp);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/sigma_yp", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_yp",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/distribution_type", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "distribution_type",
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer2);
		switch (input->excitation->discrete[i].distribution_type) {
			case XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC:
				gtk_tree_store_set(model, &iter3,
					INPUT_VALUE_COLUMN, "monochromatic",
					-1
				);
				break;
			case XMI_ENERGY_DISCRETE_DISTRIBUTION_GAUSSIAN:
				gtk_tree_store_set(model, &iter3,
					INPUT_VALUE_COLUMN, "gaussian",
					-1);
				gtk_tree_store_append(model, &iter3, &iter2);
				buffer = g_strdup_printf("%g", input->excitation->discrete[i].scale_parameter);
				buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/scale_parameter", i+1);
				gtk_tree_store_set(model, &iter3,
					INPUT_PARAMETER_COLUMN, "standard_deviation",
					INPUT_SELECTABLE_COLUMN, TRUE,
					INPUT_VALUE_COLUMN, buffer,
					INPUT_XPATH_COLUMN, buffer2,
					INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
					-1
				);
				g_free(buffer);
				g_free(buffer2);
				break;
			case XMI_ENERGY_DISCRETE_DISTRIBUTION_LORENTZIAN:
				gtk_tree_store_set(model, &iter3,
					INPUT_VALUE_COLUMN, "lorentzian",
					-1
				);
				gtk_tree_store_append(model, &iter3, &iter2);
				buffer = g_strdup_printf("%g", input->excitation->discrete[i].scale_parameter);
				buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/scale_parameter", i+1);
				gtk_tree_store_set(model, &iter3,
					INPUT_PARAMETER_COLUMN, "scale_parameter",
					INPUT_SELECTABLE_COLUMN, TRUE,
					INPUT_VALUE_COLUMN, buffer,
					INPUT_XPATH_COLUMN, buffer2,
					INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
					-1
				);
				g_free(buffer);
				g_free(buffer2);
				break;
		}
	}
	for (i = 0 ; i < input->excitation->n_continuous ; i++) {
		gtk_tree_store_append(model, &iter2, &iter1);
		buffer = g_strdup_printf("continuous %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]", i+1);
		gtk_tree_store_set(model, &iter2,
			INPUT_PARAMETER_COLUMN, "continuous",
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->continuous[i].energy);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/energy", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "energy",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->continuous[i].horizontal_intensity);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/horizontal_intensity", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "horizontal_intensity",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->continuous[i].vertical_intensity);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/vertical_intensity", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "vertical_intensity",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->continuous[i].sigma_x);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/sigma_x", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_x",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->continuous[i].sigma_y);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/sigma_y", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_y",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->continuous[i].sigma_xp);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/sigma_xp", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_xp",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%g", input->excitation->continuous[i].sigma_yp);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/sigma_yp", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_yp",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
	}

	//absorbers
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1,
		INPUT_PARAMETER_COLUMN, "absorbers",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/absorbers",
		-1
	);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "excitation_path",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/absorbers/excitation_path",
		-1
	);
	for (i = 0 ; i < input->absorbers->n_exc_layers ; i++) {
		buffer = g_strdup_printf("layer %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		for (j = 0 ; j < input->absorbers->exc_layers[i].n_elements ; j++) {
			buffer = g_strdup_printf("element %i", j+1);
			buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]/element[%i]", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			g_free(buffer2);
			g_free(buffer);
			buffer = AtomicNumberToSymbol(input->absorbers->exc_layers[i].Z[j], NULL);
			buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]/element[%i]/atomic_number", i+1, j+1);
			gtk_tree_store_append(model, &iter5, &iter4);
			gtk_tree_store_set(model, &iter5,
				INPUT_PARAMETER_COLUMN, "atomic_number",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			xrlFree(buffer);
			g_free(buffer2);
			buffer = g_strdup_printf("%g", input->absorbers->exc_layers[i].weight[j]*100.0);
			buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]/element[%i]/weight_fraction", i+1, j+1);
			gtk_tree_store_append(model, &iter5, &iter4);
			gtk_tree_store_set(model, &iter5,
				INPUT_PARAMETER_COLUMN, "weight_fraction",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, input->absorbers->exc_layers[i].n_elements > 1 ? TRUE : FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_WEIGHT_FRACTION,
				-1
			);
			g_free(buffer);
			g_free(buffer2);
		}

		buffer = g_strdup_printf("%g", input->absorbers->exc_layers[i].density);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]/density", i+1);
		gtk_tree_store_append(model, &iter4, &iter3);
		gtk_tree_store_set(model, &iter4,
			INPUT_PARAMETER_COLUMN, "density",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		buffer = g_strdup_printf("%g", input->absorbers->exc_layers[i].thickness);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]/thickness", i+1);
		gtk_tree_store_append(model, &iter4, &iter3);
		gtk_tree_store_set(model, &iter4,
			INPUT_PARAMETER_COLUMN, "thickness",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
	}
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "detector_path",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/absorbers/detector_path",
		-1
	);
	for (i = 0 ; i < input->absorbers->n_det_layers ; i++) {
		buffer = g_strdup_printf("layer %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		for (j = 0 ; j < input->absorbers->det_layers[i].n_elements ; j++) {
			buffer = g_strdup_printf("element %i", j+1);
			buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]/element[%i]", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			g_free(buffer2);
			buffer = AtomicNumberToSymbol(input->absorbers->det_layers[i].Z[j], NULL);
			buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]/element[%i]/atomic_number", i+1, j+1);
			gtk_tree_store_append(model, &iter5, &iter4);
			gtk_tree_store_set(model, &iter5,
				INPUT_PARAMETER_COLUMN, "atomic_number",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			xrlFree(buffer);
			g_free(buffer2);
			buffer = g_strdup_printf("%g", input->absorbers->det_layers[i].weight[j]*100.0);
			buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]/element[%i]/weight_fraction", i+1, j+1);
			gtk_tree_store_append(model, &iter5, &iter4);
			gtk_tree_store_set(model, &iter5,
				INPUT_PARAMETER_COLUMN, "weight_fraction",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, input->absorbers->det_layers[i].n_elements > 1 ? TRUE : FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_WEIGHT_FRACTION,
				-1
			);
			g_free(buffer);
			g_free(buffer2);
		}

		buffer = g_strdup_printf("%g", input->absorbers->det_layers[i].density);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]/density", i+1);
		gtk_tree_store_append(model, &iter4, &iter3);
		gtk_tree_store_set(model, &iter4,
			INPUT_PARAMETER_COLUMN, "density",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		buffer = g_strdup_printf("%g", input->absorbers->det_layers[i].thickness);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]/thickness", i+1);
		gtk_tree_store_append(model, &iter4, &iter3);
		gtk_tree_store_set(model, &iter4,
			INPUT_PARAMETER_COLUMN, "thickness",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
	}

	//detector
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1,
		INPUT_PARAMETER_COLUMN, "detector",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector",
		-1
	);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "detector_type",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/detector_type",
		-1
	);
	switch (input->detector->detector_type) {
		case XMI_DETECTOR_CONVOLUTION_PROFILE_SILI:
			gtk_tree_store_set(model, &iter2,
				INPUT_VALUE_COLUMN, "SiLi",
				-1
			);
			break;
		case XMI_DETECTOR_CONVOLUTION_PROFILE_GE:
			gtk_tree_store_set(model, &iter2,
				INPUT_VALUE_COLUMN, "Ge",
				-1
			);
			break;
		case XMI_DETECTOR_CONVOLUTION_PROFILE_SI_SDD:
			gtk_tree_store_set(model, &iter2,
				INPUT_VALUE_COLUMN, "Si Drift Detector",
				-1
			);
			break;
	}
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%g", input->detector->live_time);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "live_time",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/live_time",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%g", input->detector->pulse_width);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "pulse_width",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/pulse_width",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%i", input->detector->nchannels);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "nchannels",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/nchannels",
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%g", input->detector->gain);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "gain",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/gain",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%g", input->detector->zero);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "zero",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/zero",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%g", input->detector->fano);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "fano",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/fano",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%g", input->detector->noise);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "noise",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/noise",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "crystal",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/crystal",
		-1
	);
	for (i = 0 ; i < input->detector->n_crystal_layers ; i++) {
		buffer = g_strdup_printf("layer %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		for (j = 0 ; j < input->detector->crystal_layers[i].n_elements ; j++) {
			buffer = g_strdup_printf("element %i", j+1);
			buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]/element[%i]", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			g_free(buffer2);
			g_free(buffer);
			buffer = AtomicNumberToSymbol(input->detector->crystal_layers[i].Z[j], NULL);
			buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]/element[%i]/atomic_number", i+1, j+1);
			gtk_tree_store_append(model, &iter5, &iter4);
			gtk_tree_store_set(model, &iter5,
				INPUT_PARAMETER_COLUMN, "atomic_number",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			xrlFree(buffer);
			g_free(buffer2);
			buffer = g_strdup_printf("%g", input->detector->crystal_layers[i].weight[j]*100.0);
			buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]/element[%i]/weight_fraction", i+1, j+1);
			gtk_tree_store_append(model, &iter5, &iter4);
			gtk_tree_store_set(model, &iter5,
				INPUT_PARAMETER_COLUMN, "weight_fraction",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, input->detector->crystal_layers[i].n_elements > 1 ? TRUE : FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_WEIGHT_FRACTION,
				-1
			);
			g_free(buffer);
			g_free(buffer2);
		}

		buffer = g_strdup_printf("%g", input->detector->crystal_layers[i].density);
		buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]/density", i+1);
		gtk_tree_store_append(model, &iter4, &iter3);
		gtk_tree_store_set(model, &iter4,
			INPUT_PARAMETER_COLUMN, "density",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		buffer = g_strdup_printf("%g", input->detector->crystal_layers[i].thickness);
		buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]/thickness", i+1);
		gtk_tree_store_append(model, &iter4, &iter3);
		gtk_tree_store_set(model, &iter4,
			INPUT_PARAMETER_COLUMN, "thickness",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
	}
}

GtkWidget* xmi_msim_gui_xmsi_selection_scrolled_window_new(xmi_input *input, gboolean with_colors) {
	return g_object_new(XMI_MSIM_GUI_TYPE_XMSI_SELECTION_SCROLLED_WINDOW, "xmi-input", input, "with-colors", with_colors, NULL);
}

GtkTreeView *xmi_msim_gui_xmsi_selection_scrolled_window_get_tree_view(XmiMsimGuiXmsiSelectionScrolledWindow *scrolled_window) {
	return scrolled_window->treeview;
}

static void xmi_msim_gui_xmsi_selection_scrolled_window_set_property(GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec) {
  XmiMsimGuiXmsiSelectionScrolledWindow *self = XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW(object);

  switch (prop_id) {
    case PROP_INPUT:
      xmi_input_free(self->input);
      self->input = g_value_dup_boxed(value);
      populate_model(self);
      break;
    case PROP_WITH_COLOR:
      self->with_colors = g_value_get_boolean(value);
      update_colors(self);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static void xmi_msim_gui_xmsi_selection_scrolled_window_get_property(GObject *object, guint prop_id, GValue *value, GParamSpec *pspec) {
  XmiMsimGuiXmsiSelectionScrolledWindow *self = XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW(object);

  switch (prop_id) {
    case PROP_INPUT:
      g_value_set_boxed(value, self->input);
      break;
    case PROP_WITH_COLOR:
      g_value_set_boolean(value, self->with_colors);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }

}
