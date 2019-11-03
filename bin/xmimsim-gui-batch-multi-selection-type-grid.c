/*
Copyright (C) 2019 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-batch-multi-selection-type-grid.h"
#include "xmimsim-gui-type-builtins.h"

static G_DEFINE_QUARK("selection-type", selection_type)
#define SELECTION_TYPE_QUARK selection_type_quark()

struct _XmiMsimGuiBatchMultiSelectionTypeGrid {
	GtkGrid parent_instance;
	XmiMsimGuiBatchMultiSelectionType type;
};

struct _XmiMsimGuiBatchMultiSelectionTypeGridClass {
	GtkGridClass parent_class;
};

enum {
	PROP_0,
	PROP_TYPE,
	N_PROPERTIES
};

static GParamSpec *props[N_PROPERTIES] = {NULL, };

G_DEFINE_TYPE(XmiMsimGuiBatchMultiSelectionTypeGrid, xmi_msim_gui_batch_multi_selection_type_grid, GTK_TYPE_GRID)

static void xmi_msim_gui_batch_multi_selection_type_grid_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_batch_multi_selection_type_grid_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_batch_multi_selection_type_grid_finalize(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_batch_multi_selection_type_grid_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_batch_multi_selection_type_grid_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiBatchMultiSelectionTypeGrid *grid = XMI_MSIM_GUI_BATCH_MULTI_SELECTION_TYPE_GRID(object);

  switch (prop_id) {
    case PROP_TYPE:
      grid->type = g_value_get_enum(value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static void xmi_msim_gui_batch_multi_selection_type_grid_get_property(GObject *object, guint prop_id, GValue *value, GParamSpec *pspec) {
  XmiMsimGuiBatchMultiSelectionTypeGrid *box = XMI_MSIM_GUI_BATCH_MULTI_SELECTION_TYPE_GRID(object);

  switch (prop_id) {
    case PROP_TYPE:
      g_value_set_enum(value, box->type);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static void toggled_cb(GtkToggleButton *button, XmiMsimGuiBatchMultiSelectionTypeGrid *self) {
	if (!gtk_toggle_button_get_active(button))
		return;

	self->type = GPOINTER_TO_INT(g_object_get_qdata(G_OBJECT(button), SELECTION_TYPE_QUARK));

	g_object_notify_by_pspec(G_OBJECT(self), props[PROP_TYPE]);
}

static void xmi_msim_gui_batch_multi_selection_type_grid_init(XmiMsimGuiBatchMultiSelectionTypeGrid *self) {
	g_object_set(
		self,
		"halign", GTK_ALIGN_FILL,
		"valign", GTK_ALIGN_FILL,
		"hexpand", TRUE,
		"vexpand", TRUE,
		"border-width", 10,
		"row-homogeneous", TRUE,
		"row-spacing", 10,
		NULL
	);

	self->type = XMI_MSIM_GUI_BATCH_MULTI_SELECTION_TYPE_SINGLE_OPTION;

	GtkWidget *button = gtk_radio_button_new_with_label(NULL, "Use single set of options for all input-files");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), TRUE);
	gtk_widget_set_valign(button, GTK_ALIGN_END);
	gtk_grid_attach(GTK_GRID(self), button, 0, 0, 1, 1);
	g_object_set_qdata(G_OBJECT(button), SELECTION_TYPE_QUARK, GINT_TO_POINTER(XMI_MSIM_GUI_BATCH_MULTI_SELECTION_TYPE_SINGLE_OPTION));
	g_signal_connect(button, "toggled", G_CALLBACK(toggled_cb), self);
	gtk_widget_show(button);

	button = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(button), "Use different options set for each input-file (advanced)");
	gtk_widget_set_valign(button, GTK_ALIGN_START);
	gtk_grid_attach(GTK_GRID(self), button, 0, 1, 1, 1);
	g_object_set_qdata(G_OBJECT(button), SELECTION_TYPE_QUARK, GINT_TO_POINTER(XMI_MSIM_GUI_BATCH_MULTI_SELECTION_TYPE_MULTI_OPTION));
	g_signal_connect(button, "toggled", G_CALLBACK(toggled_cb), self);
	gtk_widget_show(button);
}

static void xmi_msim_gui_batch_multi_selection_type_grid_class_init(XmiMsimGuiBatchMultiSelectionTypeGridClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_batch_multi_selection_type_grid_dispose;
	object_class->finalize = xmi_msim_gui_batch_multi_selection_type_grid_finalize;
	object_class->set_property = xmi_msim_gui_batch_multi_selection_type_grid_set_property;
	object_class->get_property = xmi_msim_gui_batch_multi_selection_type_grid_get_property;

	props[PROP_TYPE] = g_param_spec_enum(
		"selection-type",
		"Selection Type",
		"Selection Type",
		XMI_MSIM_GUI_TYPE_BATCH_MULTI_SELECTION_TYPE,
		XMI_MSIM_GUI_BATCH_MULTI_SELECTION_TYPE_SINGLE_OPTION,
		G_PARAM_READABLE
	);

	g_object_class_install_properties(object_class, N_PROPERTIES, props);
}

GtkWidget* xmi_msim_gui_batch_multi_selection_type_grid_new(void) {
	return g_object_new(XMI_MSIM_GUI_TYPE_BATCH_MULTI_SELECTION_TYPE_GRID, NULL);
}
