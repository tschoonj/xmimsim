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


#include <gtk/gtk.h>
#include "xmi_data_structs.h"

#ifndef XMI_MSIM_GUI_UNDO_MANAGER_H
#define XMI_MSIM_GUI_UNDO_MANAGER_H

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_UNDO_MANAGER 		      (xmi_msim_gui_undo_manager_get_type())
#define XMI_MSIM_GUI_UNDO_MANAGER(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_UNDO_MANAGER, XmiMsimGuiUndoManager))
#define XMI_MSIM_GUI_UNDO_MANAGER_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_UNDO_MANAGER, XmiMsimGuiUndoManagerClass))
#define XMI_MSIM_GUI_IS_UNDO_MANAGER(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_UNDO_MANAGER))
#define XMI_MSIM_GUI_IS_UNDO_MANAGER_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_UNDO_MANAGER))
#define XMI_MSIM_GUI_UNDO_MANAGER_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_UNDO_MANAGER, XmiMsimGuiUndoManagerClass))

typedef struct _XmiMsimGuiUndoManager		XmiMsimGuiUndoManager;
typedef struct _XmiMsimGuiUndoManagerClass   	XmiMsimGuiUndoManagerClass;


XmiMsimGuiUndoManager* xmi_msim_gui_undo_manager_new();

gboolean xmi_msim_gui_undo_manager_add_entry(XmiMsimGuiUndoManager *manager, GtkEntry *entry, const gchar *message, GError **error);

GType xmi_msim_gui_undo_manager_get_type(void) G_GNUC_CONST;

G_END_DECLS

#endif
