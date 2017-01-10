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

#include <config.h>
#include "xmimsim-gui-export-canvas-dialog.h"
#include <cairo-ps.h>
#include <cairo-pdf.h>

G_DEFINE_TYPE(XmiMsimGuiExportCanvasDialog, xmi_msim_gui_export_canvas_dialog, GTK_TYPE_FILE_CHOOSER_DIALOG)

static void xmi_msim_gui_export_canvas_dialog_class_init(XmiMsimGuiExportCanvasDialogClass *klass) {
  //nothing to init here
}

#ifdef HAVE_CXX
GtkWidget *xmi_msim_gui_export_canvas_dialog_new(const gchar *title, GtkWindow *parent, Gtk::PLplot::Canvas *canvas)
#else
GtkWidget *xmi_msim_gui_export_canvas_dialog_new(const gchar *title, GtkWindow *parent, GtkWidget *canvas)
#endif
  {
  GtkWidget *widget;

  g_return_val_if_fail(parent == NULL || GTK_IS_WINDOW(parent), NULL);

  widget = GTK_WIDGET(g_object_new(XMI_MSIM_GUI_TYPE_EXPORT_CANVAS_DIALOG, NULL));

  gtk_window_set_transient_for(GTK_WINDOW(widget),
                               GTK_WINDOW(parent));

  XmiMsimGuiExportCanvasDialog *dialog = XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG(widget);

  dialog->canvas = canvas;

  gtk_window_set_title(GTK_WINDOW(dialog), title);

  return widget;

}

static void xmi_msim_gui_export_canvas_dialog_init(XmiMsimGuiExportCanvasDialog *dialog) {
  gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
  gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

  dialog->eps_filter = gtk_file_filter_new();
  gtk_file_filter_add_pattern(dialog->eps_filter, "*.eps");
  gtk_file_filter_set_name(dialog->eps_filter, "EPS (Encapsulated PostScript)");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), dialog->eps_filter);

  dialog->pdf_filter = gtk_file_filter_new();
  gtk_file_filter_add_pattern(dialog->pdf_filter, "*.pdf");
  gtk_file_filter_set_name(dialog->pdf_filter, "PDF (Adobe Portable Document Format)");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), dialog->pdf_filter);

  dialog->png_filter = gtk_file_filter_new();
  gtk_file_filter_add_pattern(dialog->png_filter, "*.png");
  gtk_file_filter_set_name(dialog->png_filter, "PNG (Portable Network Graphics)");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), dialog->png_filter);


  gtk_file_chooser_set_action(GTK_FILE_CHOOSER(dialog), GTK_FILE_CHOOSER_ACTION_SAVE);
  gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog), TRUE);
}

gboolean xmi_msim_gui_export_canvas_dialog_save(XmiMsimGuiExportCanvasDialog *dialog, GError **error) {

  gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
  //get selected filter
  GtkFileFilter *filter = gtk_file_chooser_get_filter(GTK_FILE_CHOOSER(dialog));
  cairo_surface_t *surface;
  cairo_t *cairo;

  if (filter == dialog->eps_filter) {
    if (strcasecmp(filename + strlen(filename) - 4, ".eps") != 0) {
      filename = (gchar *) g_realloc(filename, sizeof(gchar) * (strlen(filename) + 5));
      strcat(filename, ".eps");
    }
    surface = cairo_ps_surface_create(filename,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT);
    if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
      g_set_error(error, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR_CAIRO, "Could not write to file %s due to a cairo error: %s", filename, cairo_status_to_string(cairo_surface_status(surface)));
      g_free(filename);
      return FALSE;
    }
    cairo_ps_surface_set_eps(surface,1);

  }
  else if (filter == dialog->pdf_filter) {
    if (strcasecmp(filename + strlen(filename) - 4, ".pdf") != 0) {
      filename = (gchar *) g_realloc(filename, sizeof(gchar) * (strlen(filename) + 5));
      strcat(filename, ".pdf");
    }
    surface = cairo_pdf_surface_create(filename,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT);
    if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
      g_set_error(error, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR_CAIRO, "Could not write to file %s due to a cairo error: %s", filename, cairo_status_to_string(cairo_surface_status(surface)));
      g_free(filename);
      return FALSE;
    }
  }
  else if (filter == dialog->png_filter) {
    if (strcasecmp(filename + strlen(filename) - 4, ".png") != 0) {
      filename = (gchar *) g_realloc(filename, sizeof(gchar) * (strlen(filename) + 5));
      strcat(filename,".png");
    }
    surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT);
    if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
      g_set_error(error, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR_CAIRO, "Could not write to file %s due to a cairo error: %s", filename, cairo_status_to_string(cairo_surface_status(surface)));
      g_free(filename);
      return FALSE;
    }
  }
  else {
    g_free(filename);
    fprintf(stderr, "Unknown file filter in xmi_msim_gui_export_canvas_dialog_save");
    g_set_error(error, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR_UNKNOWN_FILEFILTER, "Unknown filefilter detected");
    return FALSE;
  }
  cairo = cairo_create(surface);
#ifdef HAVE_CXX
  dialog->canvas->draw_plot(Cairo::RefPtr<Cairo::Context>(new Cairo::Context(cairo)),
    XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH,
    XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT);
#else
  gtk_plot_canvas_export_cairo(GTK_PLOT_CANVAS(dialog->canvas),cairo);
  gtk_plot_canvas_paint(GTK_PLOT_CANVAS(dialog->canvas));
#endif
  if (filter == dialog->png_filter) {
    cairo_status_t status = cairo_surface_write_to_png(surface, filename);
    if (status != CAIRO_STATUS_SUCCESS) {
      g_set_error(error, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR_CAIRO, "Could not write to file %s due to a cairo error: %s", filename, cairo_status_to_string(status));
      g_free(filename);
      return FALSE;
    }
  }
  else {
    cairo_show_page(cairo);
  }
  cairo_surface_destroy(surface);
  cairo_destroy(cairo);
  g_free(filename);
  return TRUE;
}

GQuark xmi_msim_gui_export_canvas_dialog_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-gui-export-canvas-dialog-error-quark");
}

