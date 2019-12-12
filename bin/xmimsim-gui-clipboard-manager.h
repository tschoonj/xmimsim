/*
Copyright (C) 2018-2019 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_MSIM_GUI_CLIPBOARD_MANAGER_H
#define XMI_MSIM_GUI_CLIPBOARD_MANAGER_H

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_CLIPBOARD_MANAGER 		      (xmi_msim_gui_clipboard_manager_get_type())
#define XMI_MSIM_GUI_CLIPBOARD_MANAGER(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_CLIPBOARD_MANAGER, XmiMsimGuiClipboardManager))
#define XMI_MSIM_GUI_CLIPBOARD_MANAGER_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_CLIPBOARD_MANAGER, XmiMsimGuiClipboardManagerClass))
#define XMI_MSIM_GUI_IS_CLIPBOARD_MANAGER(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_CLIPBOARD_MANAGER))
#define XMI_MSIM_GUI_IS_CLIPBOARD_MANAGER_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_CLIPBOARD_MANAGER))
#define XMI_MSIM_GUI_CLIPBOARD_MANAGER_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_CLIPBOARD_MANAGER, XmiMsimGuiClipboardManagerClass))

typedef struct _XmiMsimGuiClipboardManager		XmiMsimGuiClipboardManager;
typedef struct _XmiMsimGuiClipboardManagerClass   	XmiMsimGuiClipboardManagerClass;


XmiMsimGuiClipboardManager* xmi_msim_gui_clipboard_manager_new(void);

void xmi_msim_gui_clipboard_manager_register_widget(XmiMsimGuiClipboardManager *clipboard_manager, GtkWidget *widget);

GtkWidget* xmi_msim_gui_clipboard_manager_get_focus_widget(XmiMsimGuiClipboardManager *clipboard_manager);

void xmi_msim_gui_clipboard_manager_cut(XmiMsimGuiClipboardManager *clipboard_manager);
void xmi_msim_gui_clipboard_manager_copy(XmiMsimGuiClipboardManager *clipboard_manager);
void xmi_msim_gui_clipboard_manager_paste(XmiMsimGuiClipboardManager *clipboard_manager);

GType xmi_msim_gui_clipboard_manager_get_type(void) G_GNUC_CONST;

G_END_DECLS

#endif
