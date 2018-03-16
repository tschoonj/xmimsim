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
#include "xmimsim-gui-layer-box.h"
#include "xmimsim-gui-layer-dialog.h"
#include "xmimsim-gui-marshal.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-type-builtins.h"
#include <string.h>

#define LayerAtom gdk_atom_intern_static_string("xmi-msim-layer")

struct _XmiMsimGuiLayerBox {
	GtkBox parent_instance;
	GArray *layer_array;
	int reference_layer; // this value will 0 or greater, but must be returned + 1!
	XmiMsimGuiLayerBoxType type;
	GtkListStore *store;
	GtkWidget *tree;
	GtkWidget *add_button;
	GtkWidget *edit_button;
	GtkWidget *delete_button;
	GtkWidget *top_button;
	GtkWidget *down_button;
	GtkWidget *bottom_button;
	GtkWidget *up_button;
};

struct _XmiMsimGuiLayerBoxClass {
	GtkBoxClass parent_class;
};

enum {
	CHANGED,
	UPDATE_CLIPBOARD_BUTTONS,
	LAST_SIGNAL
};

enum {
	PROP_0,
	PROP_LAYER_BOX_TYPE
};

enum {
	N_ELEMENTS_COLUMN,
	ELEMENTS_COLUMN,
	DENSITY_COLUMN,
	THICKNESS_COLUMN,
	REFERENCE_COLUMN,
	NCOLUMNS_MATRIX
};

static guint signals[LAST_SIGNAL];

static const gchar* const type_names[4] = {"composition", "excitation absorbers", "detector absorbers", "detector crystal"};

G_DEFINE_TYPE(XmiMsimGuiLayerBox, xmi_msim_gui_layer_box, GTK_TYPE_BOX)

static void xmi_msim_gui_layer_box_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_layer_box_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_layer_box_finalize(GObject *gobject) {
	XmiMsimGuiLayerBox *box = XMI_MSIM_GUI_LAYER_BOX(gobject);
	g_array_unref(box->layer_array);
	g_array_free(box->layer_array, TRUE);

	G_OBJECT_CLASS(xmi_msim_gui_layer_box_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_layer_box_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiLayerBox *box = XMI_MSIM_GUI_LAYER_BOX(object);

  switch (prop_id) {
    case PROP_LAYER_BOX_TYPE:
      box->type = g_value_get_enum(value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static gboolean reference_layer_foreach(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter, GtkTreePath *active_path) {
	gboolean new_value;

	if (gtk_tree_path_compare(path, active_path) == 0)
		new_value = TRUE;
	else
		new_value = FALSE;

	gtk_list_store_set(GTK_LIST_STORE(model), iter, REFERENCE_COLUMN, new_value, -1);

	return FALSE;
}

static void reference_layer_toggled_cb(GtkCellRendererToggle *renderer, gchar *path, XmiMsimGuiLayerBox *self) {
	GtkTreePath *active_path;

	active_path = gtk_tree_path_new_from_string(path);
	gtk_tree_model_foreach(GTK_TREE_MODEL(self->store), (GtkTreeModelForeachFunc) reference_layer_foreach, active_path);
	self->reference_layer = gtk_tree_path_get_indices(active_path)[0];
	gtk_tree_path_free(active_path);

	// emit signal for UndoManager
	gchar *undo_string = g_strdup_printf("change of %s reference layer", type_names[self->type]);
	g_signal_emit(self, signals[CHANGED], 0, undo_string);
	g_free(undo_string);
}

static void update_buttons(XmiMsimGuiLayerBox *self) {
	GtkTreeIter iter;

	// after pushing the move buttons... their sensivity needs to be checked
	// if there is just one layer, or there is no selection at all, desensitize everythin
	if (gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(self->tree)), NULL, &iter) == FALSE) {
		gtk_widget_set_sensitive(self->down_button, FALSE);
		gtk_widget_set_sensitive(self->bottom_button, FALSE);
		gtk_widget_set_sensitive(self->up_button, FALSE);
		gtk_widget_set_sensitive(self->top_button, FALSE);
		gtk_widget_set_sensitive(self->delete_button, FALSE);
		gtk_widget_set_sensitive(self->edit_button, FALSE);
		g_signal_emit(self, signals[UPDATE_CLIPBOARD_BUTTONS], 0, FALSE, TRUE);
		return;
	}
	else if (self->layer_array->len == 1) {
		gtk_widget_set_sensitive(self->down_button, FALSE);
		gtk_widget_set_sensitive(self->bottom_button, FALSE);
		gtk_widget_set_sensitive(self->up_button, FALSE);
		gtk_widget_set_sensitive(self->top_button, FALSE);
		gtk_widget_set_sensitive(self->delete_button, TRUE);
		gtk_widget_set_sensitive(self->edit_button, TRUE);
		g_signal_emit(self, signals[UPDATE_CLIPBOARD_BUTTONS], 0, TRUE, TRUE);
		return;
	}

	GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(self->store), &iter);
	gint selected_index = gtk_tree_path_get_indices(path)[0];
	gtk_tree_path_free(path);

	if (selected_index == 0) {
		gtk_widget_set_sensitive(self->down_button, TRUE);
		gtk_widget_set_sensitive(self->bottom_button, TRUE);
		gtk_widget_set_sensitive(self->up_button, FALSE);
		gtk_widget_set_sensitive(self->top_button, FALSE);
	}
	else if (selected_index == self->layer_array->len-1) {
		gtk_widget_set_sensitive(self->down_button, FALSE);
		gtk_widget_set_sensitive(self->bottom_button, FALSE);
		gtk_widget_set_sensitive(self->up_button, TRUE);
		gtk_widget_set_sensitive(self->top_button, TRUE);
	}
	else {
		gtk_widget_set_sensitive(self->down_button, TRUE);
		gtk_widget_set_sensitive(self->bottom_button, TRUE);
		gtk_widget_set_sensitive(self->up_button, TRUE);
		gtk_widget_set_sensitive(self->top_button, TRUE);
	}

	gtk_widget_set_sensitive(self->delete_button, TRUE);
	gtk_widget_set_sensitive(self->edit_button, TRUE);

	g_signal_emit(self, signals[UPDATE_CLIPBOARD_BUTTONS], 0, TRUE, TRUE);
}

static void layer_reordering_cb(GtkTreeModel *model, GtkTreePath *bad_path, GtkTreeIter *bad_iter, gpointer new_order, XmiMsimGuiLayerBox *self) {
	update_buttons(self);
}

static void layer_selection_changed_cb (GtkTreeSelection *selection, XmiMsimGuiLayerBox *self) {
	update_buttons(self);
}

static void append_layer(XmiMsimGuiLayerBox *self, struct xmi_layer *layer) {
	g_array_append_val(self->layer_array, *layer);

	GtkListStore *store = self->store;

	gchar *element_string = xmi_msim_gui_utils_get_layer_element_string(layer);
	if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION && self->layer_array->len == 1)
		self->reference_layer = 0;

	GtkTreeIter iter;
	gtk_list_store_append(store, &iter);
	gtk_list_store_set(store, &iter,
		N_ELEMENTS_COLUMN, layer->n_elements,
		ELEMENTS_COLUMN, element_string,
		DENSITY_COLUMN, layer->density,
		THICKNESS_COLUMN, layer->thickness,
		-1
		);
	g_free(element_string);

	if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION) {
		gtk_list_store_set(store, &iter,
			REFERENCE_COLUMN, self->layer_array->len == 1 ? TRUE : FALSE,
			-1
			);
	}
}

static void layers_add_button_clicked_cb(GtkWidget *widget, XmiMsimGuiLayerBox *self) {
	GtkWidget *dialog = xmi_msim_gui_layer_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(widget)), XMI_MSIM_GUI_LAYER_DIALOG_ADD);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		struct xmi_layer *layer = xmi_msim_gui_layer_dialog_get_layer(XMI_MSIM_GUI_LAYER_DIALOG(dialog));

		append_layer(self, layer);
		g_free(layer);
		gchar *undo_string = g_strdup_printf("addition of %s layer", type_names[self->type]);
		g_signal_emit(self, signals[CHANGED], 0, undo_string);
		g_free(undo_string);
	}

	gtk_widget_destroy(dialog);
}

static void layers_edit_button_clicked_cb(GtkWidget *widget, XmiMsimGuiLayerBox *self) {
	// get index of selected layer
	GtkTreeIter iter;
	if (gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(self->tree)), NULL, &iter) == FALSE) {
		g_warning("layers_edit_button_clicked_cb: no row was selected!");
		return;
	}
	GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(self->store), &iter);
	gint selected_index = gtk_tree_path_get_indices(path)[0];
	gtk_tree_path_free(path);

	GtkWidget *dialog = xmi_msim_gui_layer_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(widget)), XMI_MSIM_GUI_LAYER_DIALOG_EDIT);
	struct xmi_layer *selected_layer = &g_array_index(self->layer_array, struct xmi_layer, selected_index);
	xmi_msim_gui_layer_dialog_set_layer(XMI_MSIM_GUI_LAYER_DIALOG(dialog), selected_layer);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		struct xmi_layer *layer = xmi_msim_gui_layer_dialog_get_layer(XMI_MSIM_GUI_LAYER_DIALOG(dialog));
		g_free(selected_layer->Z);
		g_free(selected_layer->weight);
		*selected_layer = *layer;
		g_free(layer);

		GtkListStore *store = self->store;

		gchar *element_string = xmi_msim_gui_utils_get_layer_element_string(selected_layer);
		gtk_list_store_set(store, &iter,
			N_ELEMENTS_COLUMN, selected_layer->n_elements,
			ELEMENTS_COLUMN, element_string,
			DENSITY_COLUMN, selected_layer->density,
			THICKNESS_COLUMN, selected_layer->thickness,
			-1
			);
		g_free(element_string);

		gchar *undo_string = g_strdup_printf("editing of %s layer", type_names[self->type]);
		g_signal_emit(self, signals[CHANGED], 0, undo_string);
		g_free(undo_string);
	}

	gtk_widget_destroy(dialog);
}

static void layers_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, XmiMsimGuiLayerBox *self) {
	layers_edit_button_clicked_cb(GTK_WIDGET(tree_view), self);
}

static void layers_delete_button_clicked_cb(GtkWidget *widget, XmiMsimGuiLayerBox *self) {
	// get the selected line
	GtkTreeIter iter;
	if (gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(self->tree)), NULL, &iter) == FALSE) {
		g_warning("layers_edit_button_clicked_cb: no row was selected!");
		return;
	}
	GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(self->store), &iter);
	gint selected_index = gtk_tree_path_get_indices(path)[0];
	gtk_tree_path_free(path);

	//update composition
	g_array_remove_index(self->layer_array, selected_index);
	gtk_list_store_remove(self->store, &iter);

	if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION) {
		//reference layer may have to be updated
		if (selected_index == self->reference_layer) {
			self->reference_layer = -1;
		}
	}

	gchar *undo_string = g_strdup_printf("deleting of %s layer", type_names[self->type]);
	g_signal_emit(self, signals[CHANGED], 0, undo_string);
	g_free(undo_string);

	gtk_widget_grab_focus(self->tree); // does this trigger focus-in-event?????
	/*if (gtk_tree_model_iter_n_children(GTK_TREE_MODEL(mb->store), NULL) == 0) {
		gtk_widget_set_sensitive(GTK_WIDGET(cutT), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyT), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(cutW), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(copyW), FALSE);
	}*/
}

static gboolean layers_backspace_key_clicked(GtkWidget *widget, GdkEventKey *event, XmiMsimGuiLayerBox *self) {
	if (event->keyval == gdk_keyval_from_name("BackSpace")) {
		layers_delete_button_clicked_cb(widget, self);
		return TRUE;
	}

	return FALSE;
}

static void clipboard_clear_layer_cb(GtkClipboard *clipboard, GByteArray *data) {
	g_byte_array_free(data, TRUE);
}

static void clipboard_get_layer_cb(GtkClipboard *clipboard, GtkSelectionData *selection_data, guint info, GByteArray *data) {
	gtk_selection_data_set(selection_data, LayerAtom, 8, data->data, data->len);
}

static gboolean layer_copy_base_cb(XmiMsimGuiLayerBox *self) {
	static const GtkTargetEntry LayerTE = {(gchar *) "xmi-msim-layer", 0, 0};

	GtkClipboard *clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
	if (!clipboard)
		return FALSE;

	GtkTreeIter iter;

	if (!gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(self->tree)), NULL, &iter)) {
		g_warning("Nothing selected in layer_copy_cb->this should not occur");
		return FALSE;
	}

	GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(self->store), &iter);
	gint selected_index = gtk_tree_path_get_indices(path)[0];
	gtk_tree_path_free(path);
	struct xmi_layer *selected_layer = &g_array_index(self->layer_array, struct xmi_layer, selected_index);

	//create data for clipboard
	GByteArray *data = g_byte_array_new();
	g_byte_array_append(data, (guint8 *) &selected_layer->n_elements, sizeof(int));
	g_byte_array_append(data, (guint8 *) selected_layer->Z, sizeof(int) * selected_layer->n_elements);
	g_byte_array_append(data, (guint8 *) selected_layer->weight, sizeof(double) * selected_layer->n_elements);
	g_byte_array_append(data, (guint8 *) &selected_layer->density, sizeof(double));
	g_byte_array_append(data, (guint8 *) &selected_layer->thickness, sizeof(double));

	return gtk_clipboard_set_with_data(clipboard, &LayerTE, 1, (GtkClipboardGetFunc) clipboard_get_layer_cb, (GtkClipboardClearFunc) clipboard_clear_layer_cb, data);
}

static void layer_copy_cb(GtkWidget *button, XmiMsimGuiLayerBox *self) {

	if (layer_copy_base_cb(self)) {
		g_signal_emit(self, signals[UPDATE_CLIPBOARD_BUTTONS], 0, TRUE, TRUE);
	}
	else {
		g_warning("layer_copy_cb: Could not set clipboard!!!");
	}

}
static void layer_cut_cb(GtkWidget *button, XmiMsimGuiLayerBox *self) {
	//cut is basically copying followed by deleting
	GtkTreeIter iter;
	GtkTreeModel *model;

	if (!gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(self->tree)), NULL, &iter)) {
		g_warning("Nothing selected in layer_cut_cb->this should not occur!");
		return;
	}

	//call my copy routine
	gboolean copied = layer_copy_base_cb(self);


	//delete the selected line
	GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(self->store), &iter);
	gint selected_index = gtk_tree_path_get_indices(path)[0];
	gtk_tree_path_free(path);

	//update composition
	g_array_remove_index(self->layer_array, selected_index);
	gtk_list_store_remove(self->store, &iter);

	if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION) {
		//reference layer may have to be updated
		if (selected_index == self->reference_layer) {
			self->reference_layer = -1;
		}
	}

	gchar *undo_string = g_strdup_printf("cutting of %s layer", type_names[self->type]);
	g_signal_emit(self, signals[CHANGED], 0, undo_string);
	g_free(undo_string);

	if (copied)
		g_signal_emit(self, signals[UPDATE_CLIPBOARD_BUTTONS], 0, gtk_tree_model_iter_n_children(GTK_TREE_MODEL(self->store), NULL) > 0 ? TRUE : FALSE, TRUE);
}

static void layer_right_click_menu_delete_cb(GtkWidget *widget, XmiMsimGuiLayerBox *self) {
	layers_delete_button_clicked_cb(widget, self);
}

static void clipboard_receive_layer_cb(GtkClipboard *clipboard, GtkSelectionData *selection_data, XmiMsimGuiLayerBox *self) {

	const guchar *data = gtk_selection_data_get_data(selection_data);
	struct xmi_layer *clipboard_layer = g_malloc(sizeof(struct xmi_layer));
	size_t offset = 0;
	memcpy(&clipboard_layer->n_elements, data + offset, sizeof(int));
	offset += sizeof(int);
	clipboard_layer->Z = g_memdup(data + offset, sizeof(int)*clipboard_layer->n_elements);
	offset += sizeof(int) * clipboard_layer->n_elements;
	clipboard_layer->weight = g_memdup(data + offset, sizeof(double)*clipboard_layer->n_elements);
	offset += sizeof(double) * clipboard_layer->n_elements;
	memcpy(&clipboard_layer->density, data + offset, sizeof(double));
	offset += sizeof(double);
	memcpy(&clipboard_layer->thickness, data + offset, sizeof(double));

	append_layer(self, clipboard_layer);
	g_free(clipboard_layer);

	gchar *undo_string = g_strdup_printf("pasting of %s layer", type_names[self->type]);
	g_signal_emit(self, signals[CHANGED], 0, undo_string);
	g_free(undo_string);

	//if there is one layer now -> select it
	if (gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(self->tree)), NULL, NULL) == FALSE) {
		GtkTreeIter iter;
		gtk_tree_model_get_iter_first(GTK_TREE_MODEL(self->store), &iter);
		GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(self->store), &iter);
		gtk_tree_selection_select_path(gtk_tree_view_get_selection(GTK_TREE_VIEW(self->tree)), path); // does this trigger and update clipboard event??
		gtk_tree_path_free(path);
	}
	update_buttons(self);
	g_signal_emit(self, signals[UPDATE_CLIPBOARD_BUTTONS], 0, TRUE, TRUE);
}

static void layer_paste_cb(GtkWidget *button, XmiMsimGuiLayerBox *self) {
	GtkClipboard *clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
	if (clipboard)
		gtk_clipboard_request_contents(clipboard, LayerAtom, (GtkClipboardReceivedFunc) clipboard_receive_layer_cb, self);
}

static void create_popup_menu(GtkWidget *tree, GdkEventButton *event, XmiMsimGuiLayerBox *self) {
	GtkClipboard *clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
	if (!clipboard)
		return;

	GtkWidget *menu, *menuitem;

	//sensitivity should be determined by clipboard state and whether or not a layer was activated!
	//paste works always if clipboard is filled with goodies
	gboolean cut_and_copy_and_delete = FALSE;
	if (gtk_tree_selection_count_selected_rows(gtk_tree_view_get_selection(GTK_TREE_VIEW(self->tree))) == 1) {
		cut_and_copy_and_delete = TRUE;
	}

	menu = gtk_menu_new();
	menuitem = gtk_menu_item_new_with_label("Cut");
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	if (cut_and_copy_and_delete) {
		g_signal_connect(menuitem, "activate", G_CALLBACK(layer_cut_cb), self);
	}
	else {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}

	menuitem = gtk_menu_item_new_with_label("Copy");
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	if (cut_and_copy_and_delete) {
		g_signal_connect(menuitem, "activate", G_CALLBACK(layer_copy_cb), self);
	}
	else {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}

	menuitem = gtk_menu_item_new_with_label("Paste");
	if (gtk_clipboard_wait_is_target_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD), LayerAtom)) {
		g_signal_connect(menuitem, "activate", G_CALLBACK(layer_paste_cb), self);
	}
	else {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label("Delete");
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	if (cut_and_copy_and_delete) {
		g_signal_connect(menuitem, "activate", G_CALLBACK(layer_right_click_menu_delete_cb), self);
	}
	else {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}

	gtk_widget_show_all(menu);

#if GTK_CHECK_VERSION(3, 22, 0)
	gtk_menu_popup_at_pointer(GTK_MENU(menu), (const GdkEvent *) event);
#else
	gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, event != NULL ? event->button : 0, gdk_event_get_time((GdkEvent *) event));
#endif
}

static gboolean layer_popup_menu_cb(GtkWidget *tree, XmiMsimGuiLayerBox *self) {
	//call menu
	create_popup_menu(tree, NULL, self);

	return TRUE;
}

static gboolean layers_right_click_cb(GtkWidget *tree, GdkEventButton *event, XmiMsimGuiLayerBox *self) {
	if (event->type == GDK_BUTTON_PRESS && event->button == 3) {
		//count total number of rows
		//if clicked layer is not selected -> select it
		GtkTreeSelection *selection;
		selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
		GtkTreePath *path;
		if (gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(tree), (gint) event->x, (gint) event->y, &path, NULL, NULL, NULL) &&
		    !gtk_tree_selection_path_is_selected(selection, path)) {
			gtk_tree_selection_select_path(selection, path);
			gtk_tree_path_free(path);
		}
		create_popup_menu(tree, event, self);
		return TRUE;
	}
	return FALSE;
}

static void xmi_msim_gui_layer_box_constructed(GObject *obj) {
	XmiMsimGuiLayerBox *self = XMI_MSIM_GUI_LAYER_BOX(obj);

	if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION) {
		self->store = gtk_list_store_new(NCOLUMNS_MATRIX, G_TYPE_INT, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_BOOLEAN);
		GtkCellRenderer *renderer = gtk_cell_renderer_toggle_new();
		gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
		gtk_cell_renderer_toggle_set_radio(GTK_CELL_RENDERER_TOGGLE(renderer), TRUE);
		gtk_cell_renderer_toggle_set_activatable(GTK_CELL_RENDERER_TOGGLE(renderer), TRUE);
		g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(reference_layer_toggled_cb), self);
		GtkTreeViewColumn *column = gtk_tree_view_column_new_with_attributes("Reference layer?", renderer, "active", REFERENCE_COLUMN, NULL);
		gtk_tree_view_column_set_resizable(column, FALSE);
		gtk_tree_view_column_set_alignment(column, 0.5);
		gtk_tree_view_append_column(GTK_TREE_VIEW(self->tree), column);
		gtk_tree_view_column_set_expand(column, FALSE);
	}
	else {
		self->store = gtk_list_store_new(NCOLUMNS_MATRIX-1, G_TYPE_INT, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE);
	}

	gtk_tree_view_set_model(GTK_TREE_VIEW(self->tree), GTK_TREE_MODEL(self->store));
	g_object_unref(self->store);

	g_signal_connect(G_OBJECT(self->store), "rows-reordered", G_CALLBACK(layer_reordering_cb), self);
}

static void layer_print_double(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gdouble value;
	gchar *double_text;

	gtk_tree_model_get(tree_model,iter, GPOINTER_TO_INT(data), &value,-1);

	g_object_set(G_OBJECT(renderer), "xalign", 0.5, NULL);
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	double_text = g_strdup_printf("%lg", value);
	g_object_set(G_OBJECT(renderer), "text", double_text, NULL);

	g_free(double_text);
}

static void layers_move_button_clicked_cb(GtkWidget *button, XmiMsimGuiLayerBox *self) {
	GtkTreeIter iter;

	if (gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(self->tree)), NULL, &iter) == FALSE) {
		g_warning("layers_move_button_clicked_cb: no layer selected!");
		return;
	}

	GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(self->store), &iter);
	gint selected_index = gtk_tree_path_get_indices(path)[0];
	gtk_tree_path_free(path);

	gint *indices = g_malloc(sizeof(gint) * self->layer_array->len);
	gint i;
	for (i = 0 ; i < self->layer_array->len ; i++)
		indices[i] = i;

	struct xmi_layer temp;
	xmi_copy_layer2(&g_array_index(self->layer_array, struct xmi_layer, selected_index), &temp);

	g_array_remove_index(self->layer_array, selected_index);
	
	if (button == self->top_button) {
		indices[0] = selected_index;
		for (i = 1 ; i < selected_index + 1 ; i++) {
			indices[i] = i - 1;
		}
		g_array_prepend_val(self->layer_array, temp);

		if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION) {
			if (self->reference_layer == selected_index) {
				//reference_layer is moved...
				self->reference_layer = 0;
			}
			else if (self->reference_layer < selected_index) {
				self->reference_layer++;
			}
		}
	}
	else if (button == self->bottom_button) {
		indices[self->layer_array->len - 1] = selected_index;
		for (i = self->layer_array->len - 2 ; i >= selected_index ; i--) {
			indices[i] = i + 1;
		}
		g_array_append_val(self->layer_array, temp);

		if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION) {
			if (self->reference_layer == selected_index) {
				//reference_layer is moved...
				self->reference_layer = self->layer_array->len - 1;
			}
			else if (self->reference_layer > selected_index) {
				self->reference_layer--;
			}
		}
	}
	else if (button == self->up_button) {
		indices[selected_index - 1] = selected_index;
		indices[selected_index] = selected_index - 1;
		g_array_insert_val(self->layer_array, selected_index - 1, temp);

		if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION) {
			if (self->reference_layer == selected_index) {
				//reference_layer is moved...
				self->reference_layer = selected_index - 1;
			}
			else if (self->reference_layer == selected_index - 1) {
				self->reference_layer++;
			}
		}
	}
	else if (button == self->down_button) {
		indices[selected_index + 1] = selected_index;
		indices[selected_index] = selected_index + 1;
		g_array_insert_val(self->layer_array, selected_index + 1, temp);

		if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION) {
			if (self->reference_layer == selected_index) {
				//reference_layer is moved...
				self->reference_layer = selected_index + 1;
			}
			else if (self->reference_layer == selected_index + 1) {
				self->reference_layer--;
			}
		}
	}

	gtk_list_store_reorder(self->store, indices);

	gchar *undo_string = g_strdup_printf("reordering of %s layers", type_names[self->type]);
	g_signal_emit(self, signals[CHANGED], 0, undo_string);
	g_free(undo_string);
	
	g_free(indices);
}

static gboolean layer_focus_in_cb(GtkTreeView *tree, GdkEvent *event, XmiMsimGuiLayerBox *self) {
	if (!gtk_clipboard_get(GDK_SELECTION_CLIPBOARD))
		return FALSE;

	g_signal_emit(self, signals[UPDATE_CLIPBOARD_BUTTONS], 0, gtk_tree_model_iter_n_children(GTK_TREE_MODEL(self->store), NULL) > 0 ? TRUE : FALSE, TRUE);

	return FALSE;
}

static gboolean layer_focus_out_cb(GtkTreeView *tree, GdkEvent *event, XmiMsimGuiLayerBox *self) {
	g_signal_emit(self, signals[UPDATE_CLIPBOARD_BUTTONS], 0, FALSE, FALSE);

	return FALSE;
}

static void xmi_msim_gui_layer_box_init(XmiMsimGuiLayerBox *self) {
	// self is the old mainbox, not mainbox2
	g_object_set(
		self,
		"homogeneous", FALSE,
		"expand", FALSE,
		"orientation", GTK_ORIENTATION_HORIZONTAL,
		"border-width", 10,
		NULL
	);

	self->layer_array = g_array_new(FALSE, FALSE, sizeof(struct xmi_layer));
	g_array_set_clear_func(self->layer_array, (GDestroyNotify) xmi_free_layer);
	g_array_ref(self->layer_array);
	self->reference_layer = -1; // intentionally invalid

	self->tree = gtk_tree_view_new();
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	
	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Number of elements", renderer, "text", N_ELEMENTS_COLUMN, NULL);
	gtk_tree_view_column_set_resizable(column, FALSE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->tree), column);
	gtk_tree_view_column_set_expand(column, FALSE);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Elements", renderer, "text", ELEMENTS_COLUMN, NULL);
	gtk_tree_view_column_set_resizable(column, FALSE);
	gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_column_set_expand(column, TRUE);
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->tree), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new();
	GtkWidget *label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), "Density (g/cm<sup>3</sup>)");
	gtk_widget_show(label);
	gtk_tree_view_column_set_widget(column, label);
	gtk_tree_view_column_set_resizable(column, FALSE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, layer_print_double, GINT_TO_POINTER(DENSITY_COLUMN), NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	g_object_set(G_OBJECT(renderer), "xalign", 0.5, NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Thickness (cm)");
	gtk_tree_view_column_set_resizable(column, FALSE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->tree), column);
	gtk_tree_view_column_set_cell_data_func(column, renderer, layer_print_double, GINT_TO_POINTER(THICKNESS_COLUMN), NULL);

	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), self->tree);
	GtkWidget *tree_frame = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(tree_frame), scrolled_window);
	gtk_box_pack_start(GTK_BOX(self), tree_frame, TRUE, TRUE, 3);

	GtkWidget *buttonbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	self->top_button = gtk_button_new_with_label("Top");
	self->up_button = gtk_button_new_with_label("Up");
	self->down_button = gtk_button_new_with_label("Down");
	self->bottom_button = gtk_button_new_with_label("Bottom");
	gtk_box_pack_start(GTK_BOX(buttonbox), self->top_button, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), self->up_button, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), self->down_button, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), self->bottom_button, FALSE, FALSE, 3);

	//set insensitive at start
	gtk_widget_set_sensitive(self->top_button, FALSE);
	gtk_widget_set_sensitive(self->up_button, FALSE);
	gtk_widget_set_sensitive(self->down_button, FALSE);
	gtk_widget_set_sensitive(self->bottom_button, FALSE);

	g_signal_connect(G_OBJECT(self->top_button), "clicked", G_CALLBACK(layers_move_button_clicked_cb), self);
	g_signal_connect(G_OBJECT(self->bottom_button), "clicked", G_CALLBACK(layers_move_button_clicked_cb), self);
	g_signal_connect(G_OBJECT(self->up_button), "clicked", G_CALLBACK(layers_move_button_clicked_cb), self);
	g_signal_connect(G_OBJECT(self->down_button), "clicked", G_CALLBACK(layers_move_button_clicked_cb), self);

	gtk_box_pack_start(GTK_BOX(self), buttonbox, FALSE, FALSE, 2);

	buttonbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);

	self->add_button = gtk_button_new_with_label("Add");
	self->edit_button = gtk_button_new_with_label("Edit");
	self->delete_button = gtk_button_new_with_label("Delete");

	gtk_box_pack_start(GTK_BOX(buttonbox), self->add_button, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), self->edit_button, FALSE, FALSE, 3);
	gtk_box_pack_end(GTK_BOX(buttonbox), self->delete_button, TRUE, FALSE, 3);

	gtk_box_pack_start(GTK_BOX(self), buttonbox, FALSE, FALSE, 2);

	gtk_widget_set_sensitive(self->edit_button, FALSE);
	gtk_widget_set_sensitive(self->delete_button, FALSE);

	g_signal_connect(G_OBJECT(self->add_button), "clicked", G_CALLBACK(layers_add_button_clicked_cb), self);
	g_signal_connect(G_OBJECT(self->edit_button), "clicked", G_CALLBACK(layers_edit_button_clicked_cb), self);
	g_signal_connect(G_OBJECT(self->delete_button), "clicked", G_CALLBACK(layers_delete_button_clicked_cb), self);
	g_signal_connect(G_OBJECT(self->tree), "key-press-event", G_CALLBACK(layers_backspace_key_clicked), self);
	g_signal_connect(G_OBJECT(self->tree), "button-press-event", G_CALLBACK(layers_right_click_cb), self);
	g_signal_connect(G_OBJECT(self->tree), "popup-menu", G_CALLBACK(layer_popup_menu_cb), self);
	g_signal_connect(G_OBJECT(self->tree), "row-activated", G_CALLBACK(layers_row_activated_cb), self);

	// clipboard stuff -> catch signals, then re-emit
	g_signal_connect(G_OBJECT(self->tree), "focus-in-event", G_CALLBACK(layer_focus_in_cb), self);
	g_signal_connect(G_OBJECT(self->tree), "focus-out-event", G_CALLBACK(layer_focus_out_cb), self);

	GtkTreeSelection *select = gtk_tree_view_get_selection(GTK_TREE_VIEW(self->tree));
	gtk_tree_selection_set_mode(select, GTK_SELECTION_SINGLE);
	g_signal_connect(G_OBJECT(select), "changed", G_CALLBACK(layer_selection_changed_cb), self);
}

static void xmi_msim_gui_layer_box_class_init(XmiMsimGuiLayerBoxClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_layer_box_dispose;
	object_class->finalize = xmi_msim_gui_layer_box_finalize;
	object_class->constructed = xmi_msim_gui_layer_box_constructed;
	object_class->set_property = xmi_msim_gui_layer_box_set_property;

	g_object_class_install_property(object_class,
		PROP_LAYER_BOX_TYPE,
		g_param_spec_enum("layer-box-type",
		"Layer Box Type",
		"The type of the layer box",
		XMI_MSIM_GUI_TYPE_LAYER_BOX_TYPE,
		XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION,
		G_PARAM_WRITABLE | G_PARAM_CONSTRUCT)
	);

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

	signals[UPDATE_CLIPBOARD_BUTTONS] = g_signal_new(
		"update-clipboard-buttons",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__BOOLEAN_BOOLEAN,
		G_TYPE_NONE,
		2,
		G_TYPE_BOOLEAN,// GBOOLEAN -> CUT/COPY
		G_TYPE_BOOLEAN // GBOOLEAN -> PASTE (implies focus!)
	);
}

GtkWidget* xmi_msim_gui_layer_box_new(XmiMsimGuiLayerBoxType layers_type) {
	return GTK_WIDGET(g_object_new(XMI_MSIM_GUI_TYPE_LAYER_BOX, "layer-box-type", layers_type, NULL)); 
}

XmiMsimGuiLayerBoxType xmi_msim_gui_layer_box_get_layers_type(XmiMsimGuiLayerBox *self) {
	return self->type;
}

void xmi_msim_gui_layer_box_get_layers(XmiMsimGuiLayerBox *self, guint *n_layers, struct xmi_layer **layers, int *reference_layer) {
	if (self->layer_array->len == 0) {
		*n_layers = 0;
		*layers = NULL;
		*reference_layer = -1;
		return;
	}

	*n_layers = self->layer_array->len;

	if (layers != NULL) {
		struct xmi_layer *rv = g_malloc(sizeof(struct xmi_layer) * self->layer_array->len);
		unsigned int i;
		for (i = 0 ; i < self->layer_array->len ; i++) {
			xmi_copy_layer2(&g_array_index(self->layer_array, struct xmi_layer, i), &rv[i]);
		}
		*layers = rv;
	}

	if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION && reference_layer != NULL) {
		*reference_layer = self->reference_layer;
	}
}

void xmi_msim_gui_layer_box_set_layers(XmiMsimGuiLayerBox *self, guint n_layers, struct xmi_layer *layers, int reference_layer) {
	gtk_list_store_clear(self->store);
	g_array_ref(self->layer_array);
	g_array_free(self->layer_array, TRUE);

	int i;

	if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION)
		self->reference_layer = reference_layer;

	for (i = 0 ; i < n_layers ; i++) {
		struct xmi_layer layer = layers[i];
		layer.Z = g_memdup(layer.Z, sizeof(int) * layer.n_elements);
		layer.weight = g_memdup(layer.weight, sizeof(double) * layer.n_elements);
		g_array_append_val(self->layer_array, layer);
		
		gchar *element_string = xmi_msim_gui_utils_get_layer_element_string(&layer);

		GtkTreeIter iter;
		gtk_list_store_append(self->store, &iter);
		gtk_list_store_set(self->store, &iter,
			N_ELEMENTS_COLUMN, layer.n_elements,
			ELEMENTS_COLUMN, element_string,
			DENSITY_COLUMN, layer.density,
			THICKNESS_COLUMN, layer.thickness,
			-1
		);
		g_free(element_string);

		if (self->type == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION) {
			gtk_list_store_set(self->store, &iter,
				REFERENCE_COLUMN, self->reference_layer == i ? TRUE : FALSE,
				-1
				);
		}
	}
}

void xmi_msim_gui_layer_box_clipboard_copy(XmiMsimGuiLayerBox *self) {
	layer_copy_cb(NULL, self);
}

void xmi_msim_gui_layer_box_clipboard_cut(XmiMsimGuiLayerBox *self) {
	layer_cut_cb(NULL, self);
}

void xmi_msim_gui_layer_box_clipboard_paste(XmiMsimGuiLayerBox *self) {
	layer_paste_cb(NULL, self);
}
