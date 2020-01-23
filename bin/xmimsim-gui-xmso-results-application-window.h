/*
Copyright (C) 2019-2020 Tom Schoonjans and Laszlo Vincze

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


#ifndef XMI_MSIM_GUI_XMSO_RESULTS_APPLICATION_WINDOW_H
#define XMI_MSIM_GUI_XMSO_RESULTS_APPLICATION_WINDOW_H

#include "xmimsim-gui-application.h"

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_XMSO_RESULTS_APPLICATION_WINDOW 		      (xmi_msim_gui_xmso_results_application_window_get_type())
#define XMI_MSIM_GUI_XMSO_RESULTS_APPLICATION_WINDOW(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_XMSO_RESULTS_APPLICATION_WINDOW, XmiMsimGuiXmsoResultsApplicationWindow))
#define XMI_MSIM_GUI_XMSO_RESULTS_APPLICATION_WINDOW_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_XMSO_RESULTS_APPLICATION_WINDOW, XmiMsimGuiXmsoResultsApplicationWindowClass))
#define XMI_MSIM_GUI_IS_XMSO_RESULTS_APPLICATION_WINDOW(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_XMSO_RESULTS_APPLICATION_WINDOW))
#define XMI_MSIM_GUI_IS_XMSO_RESULTS_APPLICATION_WINDOW_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_XMSO_RESULTS_APPLICATION_WINDOW))
#define XMI_MSIM_GUI_XMSO_RESULTS_APPLICATION_WINDOW_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_XMSO_RESULTS_APPLICATION_WINDOW, XmiMsimGuiXmsoResultsApplicationWindowClass))

typedef struct _XmiMsimGuiXmsoResultsApplicationWindow		XmiMsimGuiXmsoResultsApplicationWindow;
typedef struct _XmiMsimGuiXmsoResultsApplicationWindowClass   	XmiMsimGuiXmsoResultsApplicationWindowClass;

GtkWidget* xmi_msim_gui_xmso_results_application_window_new(XmiMsimGuiApplication* app, gchar *filename, GError **error);

GType xmi_msim_gui_xmso_results_application_window_get_type(void) G_GNUC_CONST;

G_END_DECLS

#endif

