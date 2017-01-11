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


#ifndef XMI_MSIM_GUI_COMPOUND_DIALOG_H
#define XMI_MSIM_GUI_COMPOUND_DIALOG_H

#include <config.h> // bad, bad, bad!!!
#include <gtk/gtk.h>
#include "xmi_data_structs.h"
#include "xmi_xml.h"

#if GTK_MAJOR_VERSION == 3
#include <gdkmm/rgba.h>
#endif

G_BEGIN_DECLS

void xmi_msim_gui_utils_update_button_text(GtkWidget *button, const gchar *text);

double xmi_msim_gui_utils_get_solid_angle_from_slits(struct xmi_geometry *geometry);

#if GTK_MAJOR_VERSION == 3
	#define XmiColor Gdk::RGBA *
#else
	#define XmiColor GdkColor
#endif

extern XmiColor white_plot;
extern XmiColor blue_plot;
extern XmiColor red_plot;
extern XmiColor green_plot;
extern XmiColor black_plot;
extern XmiColor purple_plot;
extern XmiColor yellow_plot;
extern XmiColor pink_plot;

void xmi_msim_gui_utils_init_colors();

GtkWidget *xmi_msim_gui_utils_long_job_dialog(GtkWidget *parent, const gchar *message_with_markup);

struct read_xmsa_data {
	gchar *filename;
	struct xmi_archive **archive;
};

gpointer xmi_msim_gui_utils_read_xmsa_thread(struct read_xmsa_data *rxd);

#ifdef HAVE_LIBCURL
gboolean xmi_msim_gui_utils_check_download_url(gchar *download_url);
#endif

void xmi_msim_gui_utils_open_url(const char *link);

double xmi_msim_gui_utils_get_tickstep(double xmin, double xmax);

void xmi_msim_gui_utils_ensure_extension(gchar **filename, const gchar *extension);

gchar* xmi_msim_gui_utils_get_layer_element_string(struct xmi_layer *layer);

void xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(GtkWidget *controlsLogW, GTimer *timer, GtkTextBuffer *buffer, const gchar *text, gint len, GtkTextTag *first_tag, ...);
G_END_DECLS

#endif
