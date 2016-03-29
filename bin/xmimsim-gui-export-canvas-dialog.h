/*
Copyright (C) 2016 Tom Schoonjans and Laszlo Vincze

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

#include "config.h"
#include <gtk/gtk.h>

#ifdef HAVE_CXX
	#include <gtkmm-plplot/gtkmm-plplot.h>
#else
	#include <gtkextra.h>
#endif

#ifndef XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_H
#define XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_H

#define XMI_MSIM_GUI_TYPE_EXPORT_CANVAS_DIALOG                  (xmi_msim_gui_export_canvas_dialog_get_type ())
#define XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_EXPORT_CANVAS_DIALOG, XmiMsimGuiExportCanvasDialog))
#define XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_EXPORT_CANVAS_DIALOG, XmiMsimGuiExportCanvasDialogClass))
#define XMI_MSIM_GUI_IS_EXPORT_CANVAS_DIALOG(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_EXPORT_CANVAS_DIALOG))
#define XMI_MSIM_GUI_IS_EXPORT_CANVAS_DIALOG_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_EXPORT_CANVAS_DIALOG))
#define XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_EXPORT_CANVAS_DIALOG, XmiMsimGuiExportCanvasDialogClass))

typedef struct _XmiMsimGuiExportCanvasDialog        XmiMsimGuiExportCanvasDialog;
typedef struct _XmiMsimGuiExportCanvasDialogClass   XmiMsimGuiExportCanvasDialogClass;

#define XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH  842
#define XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT 595

struct _XmiMsimGuiExportCanvasDialog
{ 
  GtkFileChooserDialog parent_instance;
  GtkFileFilter *eps_filter;
  GtkFileFilter *pdf_filter;
  GtkFileFilter *png_filter;
#ifdef HAVE_CXX
  Gtk::PLplot::Canvas *canvas;
#else
  GtkWidget *canvas;
#endif
};

struct _XmiMsimGuiExportCanvasDialogClass
{
  GtkFileChooserDialogClass parent_class;

};

GType xmi_msim_gui_export_canvas_dialog_get_type(void) G_GNUC_CONST;

#ifdef HAVE_CXX
GtkWidget *xmi_msim_gui_export_canvas_dialog_new(const gchar *title, GtkWindow *parent, Gtk::PLplot::Canvas *canvas);
#else
GtkWidget *xmi_msim_gui_export_canvas_dialog_new(const gchar *title, GtkWindow *parent, GtkWidget *canvas);
#endif

gboolean xmi_msim_gui_export_canvas_dialog_save(XmiMsimGuiExportCanvasDialog *dialog);

#endif

