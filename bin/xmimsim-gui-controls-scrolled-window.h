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



#ifndef XMI_MSIM_GUI_CONTROLS_SCROLLED_WINDOW_H
#define XMI_MSIM_GUI_CONTROLS_SCROLLED_WINDOW_H

#include <gtk/gtk.h>
#include "xmimsim-gui-undo-manager.h"

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_CONTROLS_SCROLLED_WINDOW 		        (xmi_msim_gui_controls_scrolled_window_get_type())
#define XMI_MSIM_GUI_CONTROLS_SCROLLED_WINDOW(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_CONTROLS_SCROLLED_WINDOW, XmiMsimGuiControlsScrolledWindow))
#define XMI_MSIM_GUI_CONTROLS_SCROLLED_WINDOW_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_CONTROLS_SCROLLED_WINDOW, XmiMsimGuiControlsScrolledWindowClass))
#define XMI_MSIM_GUI_IS_CONTROLS_SCROLLED_WINDOW(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_CONTROLS_SCROLLED_WINDOW))
#define XMI_MSIM_GUI_IS_CONTROLS_SCROLLED_WINDOW_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_CONTROLS_SCROLLED_WINDOW))
#define XMI_MSIM_GUI_CONTROLS_SCROLLED_WINDOW_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_CONTROLS_SCROLLED_WINDOW, XmiMsimGuiControlsScrolledWindowClass))

typedef struct _XmiMsimGuiControlsScrolledWindow		XmiMsimGuiControlsScrolledWindow;
typedef struct _XmiMsimGuiControlsScrolledWindowClass   	XmiMsimGuiControlsScrolledWindowClass;

GtkWidget* xmi_msim_gui_controls_scrolled_window_new(XmiMsimGuiUndoManager *manager);

GType xmi_msim_gui_controls_scrolled_window_get_type(void) G_GNUC_CONST;

G_END_DECLS

#endif

