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
#include "xmimsim-gui-undo-manager.h"

typedef struct {
	gchar *message;
	GArray *signal_ids; // gulongs!
	gboolean valid;
} XmiMsimGuiUndoManagerHashValue;

static void undo_manager_hash_value_free(XmiMsimGuiUndoManagerHashValue *hash_value) {
	g_free(hash_value->message);
	g_array_free(hash_value->signal_ids, TRUE);
	g_free(hash_value);
}

struct _XmiMsimGuiUndoManager {
	GObject parent_instance;
	GHashTable *hash_table;
};

struct _XmiMsimGuiUndoManagerClass {
	GObjectClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiUndoManager, xmi_msim_gui_undo_manager, G_TYPE_OBJECT)

static void xmi_msim_gui_undo_manager_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_undo_manager_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_undo_manager_finalize(GObject *gobject) {
	XmiMsimGuiUndoManager *manager = XMI_MSIM_GUI_UNDO_MANAGER(gobject);

	g_hash_table_destroy(manager->hash_table);

	G_OBJECT_CLASS(xmi_msim_gui_undo_manager_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_undo_manager_class_init(XmiMsimGuiUndoManagerClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_undo_manager_dispose;
	object_class->finalize = xmi_msim_gui_undo_manager_finalize;
	
	// several signals will need to be added: for the save/save as buttons, undo/redo buttons, etc...
	/*signals[AFTER_EVENT] = g_signal_new(
		"after-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__POINTER,
		G_TYPE_NONE,
		1,
		G_TYPE_POINTER // GError *
	);*/

}

static void xmi_msim_gui_undo_manager_init(XmiMsimGuiUndoManager *manager) {
	manager->hash_table = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, (GDestroyNotify) undo_manager_hash_value_free);
}

XmiMsimGuiUndoManager* xmi_msim_gui_undo_manager_new() {
	return XMI_MSIM_GUI_UNDO_MANAGER(g_object_new(XMI_MSIM_GUI_TYPE_UNDO_MANAGER, NULL));
}

gboolean xmi_msim_gui_undo_manager_add_entry(XmiMsimGuiUndoManager *manager, GtkEntry *entry, const gchar *message, GError **error) {

	return TRUE;
}
