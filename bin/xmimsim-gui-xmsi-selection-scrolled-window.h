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


#include <gtk/gtk.h>
#include "xmi_data_structs.h"

#ifndef XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW_H
#define XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW_H

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_XMSI_SELECTION_SCROLLED_WINDOW 			(xmi_msim_gui_xmsi_selection_scrolled_window_get_type())
#define XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_XMSI_SELECTION_SCROLLED_WINDOW, XmiMsimGuiXmsiSelectionScrolledWindow))
#define XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_XMSI_SELECTION_SCROLLED_WINDOW, XmiMsimGuiXmsiSelectionScrolledWindowClass))
#define XMI_MSIM_GUI_IS_XMSI_SELECTION_SCROLLED_WINDOW(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_XMSI_SELECTION_SCROLLED_WINDOW))
#define XMI_MSIM_GUI_IS_XMSI_SELECTION_SCROLLED_WINDOW_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_XMSI_SELECTION_SCROLLED_WINDOW))
#define XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_XMSI_SELECTION_SCROLLED_WINDOW, XmiMsimGuiXmsiSelectionScrolledWindowClass))

typedef struct _XmiMsimGuiXmsiSelectionScrolledWindow		XmiMsimGuiXmsiSelectionScrolledWindow;
typedef struct _XmiMsimGuiXmsiSelectionScrolledWindowClass   	XmiMsimGuiXmsiSelectionScrolledWindowClass;

GtkWidget* xmi_msim_gui_xmsi_selection_scrolled_window_new(xmi_input *input, gboolean with_colors);

GtkTreeView *xmi_msim_gui_xmsi_selection_scrolled_window_get_tree_view(XmiMsimGuiXmsiSelectionScrolledWindow *scrolled_window);

GType xmi_msim_gui_xmsi_selection_scrolled_window_get_type(void) G_GNUC_CONST;

enum {
	INPUT_PARAMETER_COLUMN,
	INPUT_VALUE_COLUMN,
	INPUT_SELECTABLE_COLUMN,
	INPUT_XPATH_COLUMN,
	INPUT_ALLOWED_COLUMN,
	INPUT_N_COLUMNS
};

enum {
	PARAMETER_DOUBLE = 1,
	PARAMETER_INT = 2,
	PARAMETER_LONG = 4,
	PARAMETER_STRICT_POSITIVE = 8,
	PARAMETER_POSITIVE = 16,
	PARAMETER_WEIGHT_FRACTION = 32,
};

G_END_DECLS

#endif

