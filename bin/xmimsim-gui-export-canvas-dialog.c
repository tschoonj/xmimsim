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

#include <config.h>
#include "xmimsim-gui-export-canvas-dialog.h"
#include "xmimsim-gui-utils.h"
#include <cairo-ps.h>
#include <cairo-pdf.h>

#define EPS_FILTER "EPS (Encapsulated PostScript)"
#define PDF_FILTER "PDF (Adobe Portable Document Format)"
#define PNG_FILTER "PNG (Portable Network Graphics)"

XmiMsimGuiFileChooserDialog *xmi_msim_gui_export_canvas_dialog_new(const gchar *title, GtkWindow *parent)
  {
  XmiMsimGuiFileChooserDialog *rv;

  g_return_val_if_fail(parent == NULL || GTK_IS_WINDOW(parent), NULL);

  rv = xmi_msim_gui_file_chooser_dialog_new(title, parent, GTK_FILE_CHOOSER_ACTION_SAVE, GTK_STOCK_OK, GTK_STOCK_CANCEL);
  xmi_msim_gui_file_chooser_dialog_set_modal(rv, TRUE);

  GtkFileFilter *filter = gtk_file_filter_new();
  gtk_file_filter_add_pattern(filter, "*.eps");
  gtk_file_filter_set_name(filter, EPS_FILTER);
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(rv), filter);

  filter = gtk_file_filter_new();
  gtk_file_filter_add_pattern(filter, "*.pdf");
  gtk_file_filter_set_name(filter, PDF_FILTER);
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(rv), filter);

  filter = gtk_file_filter_new();
  gtk_file_filter_add_pattern(filter, "*.png");
  gtk_file_filter_set_name(filter, PNG_FILTER);
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(rv), filter);

  return rv;
}

#if GTK_MAJOR_VERSION == 3
gboolean xmi_msim_gui_export_canvas_dialog_save(XmiMsimGuiFileChooserDialog *dialog, Gtk::PLplot::Canvas *canvas, GError **error)
#else
gboolean xmi_msim_gui_export_canvas_dialog_save(XmiMsimGuiFileChooserDialog *dialog, GtkWidget *canvas, GError **error)
#endif
{
  gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
  //get selected filter
  GtkFileFilter *filter = gtk_file_chooser_get_filter(GTK_FILE_CHOOSER(dialog));
  cairo_surface_t *surface;
  cairo_t *cairo;

  if (g_strcmp0(gtk_file_filter_get_name(filter), EPS_FILTER) == 0) {
    xmi_msim_gui_utils_ensure_extension(&filename, ".eps");
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
  else if (g_strcmp0(gtk_file_filter_get_name(filter), PDF_FILTER) == 0) {
    xmi_msim_gui_utils_ensure_extension(&filename, ".pdf");
    surface = cairo_pdf_surface_create(filename,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT);
    if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
      g_set_error(error, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR_CAIRO, "Could not write to file %s due to a cairo error: %s", filename, cairo_status_to_string(cairo_surface_status(surface)));
      g_free(filename);
      return FALSE;
    }
  }
  else if (g_strcmp0(gtk_file_filter_get_name(filter), PNG_FILTER) == 0) {
    xmi_msim_gui_utils_ensure_extension(&filename, ".png");
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
  canvas->draw_plot(Cairo::RefPtr<Cairo::Context>(new Cairo::Context(cairo)),
    XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH,
    XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT);
#else
  gtk_plot_canvas_export_cairo(GTK_PLOT_CANVAS(canvas),cairo);
  gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
#endif
  if (g_strcmp0(gtk_file_filter_get_name(filter), PNG_FILTER) == 0) {
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

