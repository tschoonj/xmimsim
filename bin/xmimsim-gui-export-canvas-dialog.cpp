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
#include <cairo-svg.h>

#define EPS_FILTER_NAME "EPS (Encapsulated PostScript)"
#define PDF_FILTER_NAME "PDF (Adobe Portable Document Format)"
#define PNG_FILTER_NAME "PNG (Portable Network Graphics)"
#define SVG_FILTER_NAME "SVG (Scalable Vector Graphics)"

#define EPS_FILTER_EXT ".eps"
#define PDF_FILTER_EXT ".pdf"
#define PNG_FILTER_EXT ".png"
#define SVG_FILTER_EXT ".svg"

void filter_changed_cb(GObject *object, GParamSpec *pspec, gpointer user_data){
  GtkFileFilter *current_filter = gtk_file_chooser_get_filter(GTK_FILE_CHOOSER(object));
  gchar *current_ext = (gchar *) g_object_get_data(G_OBJECT(current_filter), "ext");
  gchar *current_name = gtk_file_chooser_get_current_name(GTK_FILE_CHOOSER(object));

  GHashTable *table = (GHashTable *) g_object_get_data(object, "table");
  GHashTableIter iter;
  gchar *ext;

  g_hash_table_iter_init(&iter, table);
  gboolean changed = FALSE;
  while (g_hash_table_iter_next(&iter, NULL, (gpointer *) &ext)) {
    if (strlen(current_name) <= strlen(ext))
      continue;
    if (g_ascii_strcasecmp(current_name + strlen(current_name) - strlen(ext), ext) == 0) {
      gchar *base_name = g_strndup(current_name, strlen(current_name) - strlen(ext));
      g_free(current_name);
      current_name = g_strdup_printf("%s%s", base_name, current_ext);
      g_free(base_name);
      changed = TRUE;
      break;
    }
  }

  if (!changed) {
    gchar *new_name = g_strdup_printf("%s%s", current_name, ext);
    g_free(current_name);
    current_name = new_name;
    changed = TRUE;
  }

  gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(object), current_name);
  g_free(current_name);
}

#define ADD_FILTER_TO_DIALOG(name, ext) \
  filter = gtk_file_filter_new(); \
  gtk_file_filter_add_pattern(filter, "*" ext); \
  gtk_file_filter_set_name(filter, name); \
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(rv), filter); \
  g_object_set_data_full(G_OBJECT(filter), "ext", g_strdup(ext), g_free);
 
#define ADD_FILTER_TO_TABLE(name, ext) \
  g_hash_table_insert(table, g_strdup(name), g_strdup(ext));

XmiMsimGuiFileChooserDialog *xmi_msim_gui_export_canvas_dialog_new(const gchar *title, GtkWindow *parent)
  {
  XmiMsimGuiFileChooserDialog *rv;

  g_return_val_if_fail(parent == NULL || GTK_IS_WINDOW(parent), NULL);

  rv = xmi_msim_gui_file_chooser_dialog_new(title, parent, GTK_FILE_CHOOSER_ACTION_SAVE, "_Ok", "_Cancel");
  xmi_msim_gui_file_chooser_dialog_set_modal(rv, TRUE);

  GtkFileFilter *filter;
  ADD_FILTER_TO_DIALOG(EPS_FILTER_NAME, EPS_FILTER_EXT)
  ADD_FILTER_TO_DIALOG(PDF_FILTER_NAME, PDF_FILTER_EXT)
  ADD_FILTER_TO_DIALOG(PNG_FILTER_NAME, PNG_FILTER_EXT)
  ADD_FILTER_TO_DIALOG(SVG_FILTER_NAME, SVG_FILTER_EXT)

  g_signal_connect(
    G_OBJECT(rv),
    "notify::filter",
    G_CALLBACK(filter_changed_cb),
    NULL);

  GHashTable *table = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_free);
  ADD_FILTER_TO_TABLE(EPS_FILTER_NAME, EPS_FILTER_EXT)
  ADD_FILTER_TO_TABLE(PDF_FILTER_NAME, PDF_FILTER_EXT)
  ADD_FILTER_TO_TABLE(PNG_FILTER_NAME, PNG_FILTER_EXT)
  ADD_FILTER_TO_TABLE(SVG_FILTER_NAME, SVG_FILTER_EXT)

  g_object_set_data_full(G_OBJECT(rv), "table", table, (GDestroyNotify) g_hash_table_destroy);

  return rv;
}

gboolean xmi_msim_gui_export_canvas_dialog_save(XmiMsimGuiFileChooserDialog *dialog, Gtk::PLplot::Canvas *canvas, GError **error)
{
  gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
  //get selected filter
  GtkFileFilter *filter = gtk_file_chooser_get_filter(GTK_FILE_CHOOSER(dialog));
  cairo_surface_t *surface;
  cairo_t *cairo;

  if (g_strcmp0(gtk_file_filter_get_name(filter), EPS_FILTER_NAME) == 0) {
    xmi_msim_gui_utils_ensure_extension(&filename, EPS_FILTER_EXT);
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
  else if (g_strcmp0(gtk_file_filter_get_name(filter), PDF_FILTER_NAME) == 0) {
    xmi_msim_gui_utils_ensure_extension(&filename, PDF_FILTER_EXT);
    surface = cairo_pdf_surface_create(filename,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT);
    if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
      g_set_error(error, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR_CAIRO, "Could not write to file %s due to a cairo error: %s", filename, cairo_status_to_string(cairo_surface_status(surface)));
      g_free(filename);
      return FALSE;
    }
  }
  else if (g_strcmp0(gtk_file_filter_get_name(filter), PNG_FILTER_NAME) == 0) {
    xmi_msim_gui_utils_ensure_extension(&filename, PNG_FILTER_EXT);
    surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH,
      XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT);
    if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
      g_set_error(error, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR, XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_ERROR_CAIRO, "Could not write to file %s due to a cairo error: %s", filename, cairo_status_to_string(cairo_surface_status(surface)));
      g_free(filename);
      return FALSE;
    }
  }
  else if (g_strcmp0(gtk_file_filter_get_name(filter), SVG_FILTER_NAME) == 0) {
    xmi_msim_gui_utils_ensure_extension(&filename, SVG_FILTER_EXT);
    surface = cairo_svg_surface_create(filename,
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
  canvas->draw_plot(Cairo::RefPtr<Cairo::Context>(new Cairo::Context(cairo)),
    XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH,
    XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT);
  if (g_strcmp0(gtk_file_filter_get_name(filter), PNG_FILTER_NAME) == 0) {
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

