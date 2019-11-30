/*
Copyright (C) 2017-2019 Tom Schoonjans and Laszlo Vincze

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


#ifndef XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW_H
#define XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW_H

#include <gtk/gtk.h>
#include "xmi_data_structs.h"

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

GPtrArray* xmi_msim_gui_xmsi_selection_scrolled_window_get_xpath_expressions(XmiMsimGuiXmsiSelectionScrolledWindow *scrolled_window);

GType xmi_msim_gui_xmsi_selection_scrolled_window_get_type(void) G_GNUC_CONST;

typedef enum {
	XMI_MSIM_GUI_XMSI_SELECTION_XPATH_DOUBLE          = 1 << 0,
	XMI_MSIM_GUI_XMSI_SELECTION_XPATH_INT             = 1 << 1,
	XMI_MSIM_GUI_XMSI_SELECTION_XPATH_LONG            = 1 << 2,
	XMI_MSIM_GUI_XMSI_SELECTION_XPATH_STRICT_POSITIVE = 1 << 3,
	XMI_MSIM_GUI_XMSI_SELECTION_XPATH_POSITIVE        = 1 << 4,
	XMI_MSIM_GUI_XMSI_SELECTION_XPATH_WEIGHT_FRACTION = 1 << 5,
} XmiMsimGuiXmsiSelectionXPathFlags;

typedef struct _XmiMsimGuiXmsiSelectionXPathData      XmiMsimGuiXmsiSelectionXPathData;
typedef struct _XmiMsimGuiXmsiSelectionXPathDataClass XmiMsimGuiXmsiSelectionXPathDataClass;

#define XMI_MSIM_GUI_TYPE_XMSI_SELECTION_XPATH_DATA 			(xmi_msim_gui_xmsi_selection_xpath_data_get_type())
#define XMI_MSIM_GUI_XMSI_SELECTION_XPATH_DATA(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_XMSI_SELECTION_XPATH_DATA, XmiMsimGuiXmsiSelectionXPathData))
#define XMI_MSIM_GUI_XMSI_SELECTION_XPATH_DATA_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_XMSI_SELECTION_XPATH_DATA, XmiMsimGuiXmsiSelectionXPathDataClass))
#define XMI_MSIM_GUI_IS_XMSI_SELECTION_XPATH_DATA(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_XMSI_SELECTION_XPATH_DATA))
#define XMI_MSIM_GUI_IS_XMSI_SELECTION_XPATH_DATA_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_XMSI_SELECTION_XPATH_DATA))
#define XMI_MSIM_GUI_XMSI_SELECTION_XPATH_DATA_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_XMSI_SELECTION_XPATH_DATA, XmiMsimGuiXmsiSelectionXPathDataClass))

GType xmi_msim_gui_xmsi_selection_xpath_data_get_type(void) G_GNUC_CONST;

XmiMsimGuiXmsiSelectionXPathData* xmi_msim_gui_xmsi_selection_xpath_data_new(const gchar *string, const gchar *value, XmiMsimGuiXmsiSelectionXPathFlags flags);

gchar* xmi_msim_gui_xmsi_selection_xpath_data_get_string(XmiMsimGuiXmsiSelectionXPathData* data);

gchar* xmi_msim_gui_xmsi_selection_xpath_data_get_value(XmiMsimGuiXmsiSelectionXPathData* data);

XmiMsimGuiXmsiSelectionXPathFlags xmi_msim_gui_xmsi_selection_xpath_data_get_flags(XmiMsimGuiXmsiSelectionXPathData* data);


G_END_DECLS

#endif

