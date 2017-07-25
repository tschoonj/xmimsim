/*
Copyright (C) 2016-2017 Tom Schoonjans and Laszlo Vincze

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


#ifndef XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_H
#define XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_H

#include <gtk/gtk.h>
#include "xmimsim-gui-compat.h"


#if GTK_MAJOR_VERSION == 3
	#include <gtkmm-plplot.h>
#else
	#include <gtkextra/gtkextra.h>
#endif

G_BEGIN_DECLS

#define XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH  842
#define XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT 595

XmiMsimGuiFileChooserDialog *xmi_msim_gui_export_canvas_dialog_new(const gchar *title, GtkWindow *parent);

#if GTK_MAJOR_VERSION == 3
gboolean xmi_msim_gui_export_canvas_dialog_save(XmiMsimGuiFileChooserDialog *dialog, Gtk::PLplot::Canvas *canvas, GError **error);
#else
gboolean xmi_msim_gui_export_canvas_dialog_save(XmiMsimGuiFileChooserDialog *dialog, GtkWidget *canvas, GError **error);
#endif

typedef enum {
	XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR_CAIRO,
	XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR_UNKNOWN_FILEFILTER,
} XmiMsimGuiExportCanvasDialogError;

#define XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR (xmi_msim_gui_export_canvas_dialog_error_quark())

GQuark xmi_msim_gui_export_canvas_dialog_error_quark(void);

G_END_DECLS
#endif

