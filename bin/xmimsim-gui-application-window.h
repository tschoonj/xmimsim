/*
Copyright (C) 2018-2020 Tom Schoonjans and Laszlo Vincze

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



#ifndef XMI_MSIM_GUI_APPLICATION_WINDOW_H
#define XMI_MSIM_GUI_APPLICATION_WINDOW_H

#include "xmimsim-gui-application.h"

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_APPLICATION_WINDOW 		        (xmi_msim_gui_application_window_get_type())
#define XMI_MSIM_GUI_APPLICATION_WINDOW(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_APPLICATION_WINDOW, XmiMsimGuiApplicationWindow))
#define XMI_MSIM_GUI_APPLICATION_WINDOW_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_APPLICATION_WINDOW, XmiMsimGuiApplicationWindowClass))
#define XMI_MSIM_GUI_IS_APPLICATION_WINDOW(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_APPLICATION_WINDOW))
#define XMI_MSIM_GUI_IS_APPLICATION_WINDOW_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_APPLICATION_WINDOW))
#define XMI_MSIM_GUI_APPLICATION_WINDOW_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_APPLICATION_WINDOW, XmiMsimGuiApplicationWindowClass))

typedef struct _XmiMsimGuiApplicationWindow		XmiMsimGuiApplicationWindow;
typedef struct _XmiMsimGuiApplicationWindowClass   	XmiMsimGuiApplicationWindowClass;

GtkWidget* xmi_msim_gui_application_window_new(XmiMsimGuiApplication *app);

gboolean xmi_msim_gui_application_window_load_file(XmiMsimGuiApplicationWindow *window, const gchar *filename, GError **error);

GtkWidget* xmi_msim_gui_application_window_get_active_tab(XmiMsimGuiApplicationWindow *window);

GType xmi_msim_gui_application_window_get_type(void) G_GNUC_CONST;

typedef enum {
	XMI_MSIM_GUI_APPLICATION_WINDOW_ERROR_INVALID_ARGUMENT,
} XmiMsimGuiApplicationWindowError;

#define XMI_MSIM_GUI_APPLICATION_WINDOW_ERROR (xmi_msim_gui_application_window_error_quark())

GQuark xmi_msim_gui_application_window_error_quark(void);

G_END_DECLS

#endif




