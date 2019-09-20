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


#ifndef XMI_MSIM_GUI_XMSA_VIEWER_WINDOW_H
#define XMI_MSIM_GUI_XMSA_VIEWER_WINDOW_H

#include <gtk/gtk.h>
#include "xmi_data_structs.h"
#include "xmimsim-gui-application.h"

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_XMSA_VIEWER_WINDOW 		      (xmi_msim_gui_xmsa_viewer_window_get_type())
#define XMI_MSIM_GUI_XMSA_VIEWER_WINDOW(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_XMSA_VIEWER_WINDOW, XmiMsimGuiXmsaViewerWindow))
#define XMI_MSIM_GUI_XMSA_VIEWER_WINDOW_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_XMSA_VIEWER_WINDOW, XmiMsimGuiXmsaViewerWindowClass))
#define XMI_MSIM_GUI_IS_XMSA_VIEWER_WINDOW(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_XMSA_VIEWER_WINDOW))
#define XMI_MSIM_GUI_IS_XMSA_VIEWER_WINDOW_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_XMSA_VIEWER_WINDOW))
#define XMI_MSIM_GUI_XMSA_VIEWER_WINDOW_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_XMSA_VIEWER_WINDOW, XmiMsimGuiXmsaViewerWindowClass))

typedef struct _XmiMsimGuiXmsaViewerWindow		XmiMsimGuiXmsaViewerWindow;
typedef struct _XmiMsimGuiXmsaViewerWindowClass   	XmiMsimGuiXmsaViewerWindowClass;

GtkWidget* xmi_msim_gui_xmsa_viewer_window_new(XmiMsimGuiApplication *app, gchar *filename, xmi_archive *archive);

const gchar* xmi_msim_gui_xmsa_viewer_window_get_filename(XmiMsimGuiXmsaViewerWindow *viewer);

GType xmi_msim_gui_xmsa_viewer_window_get_type(void) G_GNUC_CONST;

G_END_DECLS

#endif
