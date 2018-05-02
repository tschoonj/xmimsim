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
#include "xmimsim-gui-clipboard-manager.h"
#include "xmimsim-gui-marshal.h"
#include "xmimsim-gui-layer-box.h"

#define LayerAtom gdk_atom_intern_static_string("xmi-msim-layer")

typedef struct {
	GArray *signal_ids; // gulongs!
} XmiMsimGuiClipboardManagerHashValue;

static void clipboard_manager_hash_value_free(XmiMsimGuiClipboardManagerHashValue *hash_value) {
	g_array_free(hash_value->signal_ids, TRUE);
	g_free(hash_value);
}

struct _XmiMsimGuiClipboardManager {
	GObject parent_instance;
	GHashTable *hash_table;
	GtkWidget *focus_widget;
};

struct _XmiMsimGuiClipboardManagerClass {
	GObjectClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiClipboardManager, xmi_msim_gui_clipboard_manager, G_TYPE_OBJECT)

enum {
	UPDATE_CLIPBOARD_BUTTONS,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

static void xmi_msim_gui_clipboard_manager_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_clipboard_manager_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_clipboard_manager_finalize(GObject *gobject) {
	XmiMsimGuiClipboardManager *manager = XMI_MSIM_GUI_CLIPBOARD_MANAGER(gobject);

	g_hash_table_destroy(manager->hash_table);

	G_OBJECT_CLASS(xmi_msim_gui_clipboard_manager_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_clipboard_manager_class_init(XmiMsimGuiClipboardManagerClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_clipboard_manager_dispose;
	object_class->finalize = xmi_msim_gui_clipboard_manager_finalize;
	
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
		G_TYPE_BOOLEAN // GBOOLEAN -> PASTE
	);
}

static void xmi_msim_gui_clipboard_manager_init(XmiMsimGuiClipboardManager *manager) {
	manager->hash_table = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, (GDestroyNotify) clipboard_manager_hash_value_free);
	manager->focus_widget = NULL;
}

XmiMsimGuiClipboardManager* xmi_msim_gui_clipboard_manager_new() {
	return XMI_MSIM_GUI_CLIPBOARD_MANAGER(g_object_new(XMI_MSIM_GUI_TYPE_CLIPBOARD_MANAGER, NULL));
}

static void layer_box_update_clipboard_buttons(XmiMsimGuiLayerBox *box, gboolean cut_copy_status, gboolean paste_status, XmiMsimGuiClipboardManager *manager) {
	if (paste_status)
		manager->focus_widget = GTK_WIDGET(box);
	else
		manager->focus_widget = NULL;

	g_signal_emit(manager, signals[UPDATE_CLIPBOARD_BUTTONS], 0, cut_copy_status, paste_status && gtk_clipboard_wait_is_target_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD), LayerAtom));
}

static gboolean entry_focus_out_cb(GtkEntry *entry, GdkEvent *event, XmiMsimGuiClipboardManager *manager) {
	//make sure possible selections are removed
	manager->focus_widget = NULL;
	gtk_editable_select_region(GTK_EDITABLE(entry), 0, 0);

	g_signal_emit(manager, signals[UPDATE_CLIPBOARD_BUTTONS], 0, FALSE, FALSE);

	return FALSE;
}

static gboolean entry_focus_in_cb(GtkEntry *entry, GdkEvent *event, XmiMsimGuiClipboardManager *manager) {
	manager->focus_widget = GTK_WIDGET(entry);
	gboolean paste_status = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD) && gtk_clipboard_wait_is_text_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD));

	g_signal_emit(manager, signals[UPDATE_CLIPBOARD_BUTTONS], 0, FALSE, paste_status);

	return FALSE;
}

static void entry_notify_cursor_position_cb(GObject *entry, GParamSpec *pspec, XmiMsimGuiClipboardManager *manager) {
	//ignore if not in focus
	if (!gtk_widget_has_focus(GTK_WIDGET(entry))) {
		return;
	}

	gint selection_bound;
	gint current_pos;

	g_object_get(entry, "selection-bound", &selection_bound, "cursor-position", &current_pos, NULL);

	gboolean cut_copy_status = selection_bound != current_pos;
	gboolean paste_status = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD) && gtk_clipboard_wait_is_text_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD));

	g_signal_emit(manager, signals[UPDATE_CLIPBOARD_BUTTONS], 0, cut_copy_status, paste_status);
}

static gboolean text_view_focus_out_cb(GtkTextView *text_view, GdkEvent *event, XmiMsimGuiClipboardManager *manager) {
	//make sure possible selections are removed
	manager->focus_widget = NULL;

	GtkTextBuffer *text_buffer = gtk_text_view_get_buffer(text_view);
	GtkTextIter start;
	if (gtk_text_buffer_get_selection_bounds(text_buffer, &start, NULL)) {
		gtk_text_buffer_select_range(text_buffer, &start, &start);
	}

	g_signal_emit(manager, signals[UPDATE_CLIPBOARD_BUTTONS], 0, FALSE, FALSE);

	return FALSE;
}

static gboolean text_view_focus_in_cb(GtkTextView *text_view, GdkEvent *event, XmiMsimGuiClipboardManager *manager) {
	manager->focus_widget = GTK_WIDGET(text_view);
	gboolean paste_status = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD) && gtk_clipboard_wait_is_text_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD));

	g_signal_emit(manager, signals[UPDATE_CLIPBOARD_BUTTONS], 0, FALSE, paste_status);

	return FALSE;
}

static void text_buffer_notify_has_selection_cb(GtkTextBuffer *buffer, GParamSpec *pspec, XmiMsimGuiClipboardManager *manager) {
	gboolean cut_copy_status = gtk_text_buffer_get_has_selection(buffer);
	gboolean paste_status = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD) && gtk_clipboard_wait_is_text_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD));

	g_signal_emit(manager, signals[UPDATE_CLIPBOARD_BUTTONS], 0, cut_copy_status, paste_status);
}

void xmi_msim_gui_clipboard_manager_register_widget(XmiMsimGuiClipboardManager *manager, GtkWidget *widget) {
	g_return_if_fail(XMI_MSIM_GUI_IS_CLIPBOARD_MANAGER(manager));
	g_return_if_fail(GTK_IS_WIDGET(widget));

	XmiMsimGuiClipboardManagerHashValue *hv = g_malloc(sizeof(XmiMsimGuiClipboardManagerHashValue));
	hv->signal_ids = g_array_new(FALSE, FALSE, sizeof(gulong));

	if (XMI_MSIM_GUI_IS_LAYER_BOX(widget)) {
		gulong signal_id = g_signal_connect(widget, "update-clipboard-buttons", G_CALLBACK(layer_box_update_clipboard_buttons), manager);
		g_array_append_val(hv->signal_ids, signal_id);
	}
	else if (GTK_IS_ENTRY(widget)) {
		gulong signal_ids[4];
		signal_ids[0] = g_signal_connect(G_OBJECT(widget), "focus-out-event", G_CALLBACK(entry_focus_out_cb), manager);
		signal_ids[1] = g_signal_connect(G_OBJECT(widget), "focus-in-event", G_CALLBACK(entry_focus_in_cb), manager);
		signal_ids[2] = g_signal_connect(G_OBJECT(widget), "notify::cursor-position", G_CALLBACK(entry_notify_cursor_position_cb), manager);
		signal_ids[3] = g_signal_connect(G_OBJECT(widget), "notify::selection-bound", G_CALLBACK(entry_notify_cursor_position_cb), manager);
		g_array_append_vals(hv->signal_ids, signal_ids, 4);
	}
	else if (GTK_IS_TEXT_VIEW(widget)) {
		gulong signal_ids[3];
		signal_ids[0] = g_signal_connect(G_OBJECT(widget), "focus-out-event", G_CALLBACK(text_view_focus_out_cb), manager);
		signal_ids[1] = g_signal_connect(G_OBJECT(widget), "focus-in-event", G_CALLBACK(text_view_focus_in_cb), manager);
		signal_ids[2] = g_signal_connect(G_OBJECT(gtk_text_view_get_buffer(GTK_TEXT_VIEW(widget))), "notify::has-selection", G_CALLBACK(text_buffer_notify_has_selection_cb), manager);
		g_array_append_vals(hv->signal_ids, signal_ids, 3);
	}
	else {
		g_critical("xmi_msim_gui_clipboard_manager_register_widget: unsupported widget type %s detected", g_type_name(G_OBJECT_TYPE(widget)));
		clipboard_manager_hash_value_free(hv);
		return;
	}

	g_hash_table_insert(manager->hash_table, widget, hv);
}

GtkWidget* xmi_msim_gui_clipboard_manager_get_focus_widget(XmiMsimGuiClipboardManager *manager) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_CLIPBOARD_MANAGER(manager), NULL);

	return manager->focus_widget;
}

void xmi_msim_gui_clipboard_manager_cut(XmiMsimGuiClipboardManager *manager) {
	GtkWidget *focus_widget = manager->focus_widget;

	if (focus_widget == NULL) {
		g_warning("xmi_msim_gui_clipboard_manager_cut: focus_widget is NULL");
	}
	else if (GTK_IS_TEXT_VIEW(focus_widget)) {
		g_signal_emit_by_name(G_OBJECT(focus_widget), "cut-clipboard", NULL);
		//gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		//gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}
	else if (GTK_IS_ENTRY(focus_widget)) {
		g_signal_emit_by_name(G_OBJECT(focus_widget), "cut-clipboard", NULL);
		//gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		//gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}
	else if (XMI_MSIM_GUI_IS_LAYER_BOX(focus_widget)) {
		xmi_msim_gui_layer_box_clipboard_cut(XMI_MSIM_GUI_LAYER_BOX(focus_widget));
	}
	else {
		g_warning("xmi_msim_gui_clipboard_manager_cut: unknown focus_widget type");
	}
}

void xmi_msim_gui_clipboard_manager_copy(XmiMsimGuiClipboardManager *manager) {
	GtkWidget *focus_widget = manager->focus_widget;

	if (focus_widget == NULL) {
		g_warning("xmi_msim_gui_clipboard_manager_copy: focus_widget is NULL");
	}
	else if (GTK_IS_TEXT_VIEW(focus_widget)) {
		g_signal_emit_by_name(G_OBJECT(focus_widget), "copy-clipboard", NULL);
		//gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		//gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}
	else if (GTK_IS_ENTRY(focus_widget)) {
		g_signal_emit_by_name(G_OBJECT(focus_widget), "copy-clipboard", NULL);
		//gtk_widget_set_sensitive(GTK_WIDGET(pasteT), TRUE);
		//gtk_widget_set_sensitive(GTK_WIDGET(pasteW), TRUE);
	}
	else if (XMI_MSIM_GUI_IS_LAYER_BOX(focus_widget)) {
		xmi_msim_gui_layer_box_clipboard_copy(XMI_MSIM_GUI_LAYER_BOX(focus_widget));
	}
	else {
		g_warning("xmi_msim_gui_clipboard_manager_copy: unknown focus_widget type");
	}
}

void xmi_msim_gui_clipboard_manager_paste(XmiMsimGuiClipboardManager *manager) {
	GtkWidget *focus_widget = manager->focus_widget;
	GtkClipboard *clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);

	if (!clipboard) {
		g_warning("xmi_msim_gui_clipboard_manager_paste: could not get clipboard");
		return;
	}

	if (focus_widget == NULL) {
		g_warning("xmi_msim_gui_clipboard_manager_paste: focus_widget is NULL");
	}
	else if (GTK_IS_TEXT_VIEW(focus_widget) && gtk_clipboard_wait_is_text_available(clipboard)) {
		g_signal_emit_by_name(G_OBJECT(focus_widget), "paste-clipboard", NULL);
	}
	else if (GTK_IS_ENTRY(focus_widget) && gtk_clipboard_wait_is_text_available(clipboard)) {
		g_signal_emit_by_name(G_OBJECT(focus_widget), "paste-clipboard", NULL);
	}
	else if (XMI_MSIM_GUI_IS_LAYER_BOX(focus_widget) && gtk_clipboard_wait_is_target_available(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD), LayerAtom)) {
		xmi_msim_gui_layer_box_clipboard_paste(XMI_MSIM_GUI_LAYER_BOX(focus_widget));
	}
	else {
		g_warning("xmi_msim_gui_clipboard_manager_paste: unknown focus_widget type");
	}

}
