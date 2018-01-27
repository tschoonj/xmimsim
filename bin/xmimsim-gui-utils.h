/*
Copyright (C) 2016-2018 Tom Schoonjans and Laszlo Vincze

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


#ifndef XMI_MSIM_GUI_UTILS_H
#define XMI_MSIM_GUI_UTILS_H

#include <gtk/gtk.h>
#include "xmi_data_structs.h"
#include "xmi_xml.h"


G_BEGIN_DECLS

void xmi_msim_gui_utils_update_button_text(GtkWidget *button, const gchar *text);

double xmi_msim_gui_utils_get_solid_angle_from_slits(struct xmi_geometry *geometry);

GtkWidget *xmi_msim_gui_utils_long_job_dialog(GtkWidget *parent, const gchar *message_with_markup);

struct read_xmsa_data {
	gchar *filename;
	struct xmi_archive **archive;
};

gpointer xmi_msim_gui_utils_read_xmsa_thread(struct read_xmsa_data *rxd);

void xmi_msim_gui_utils_open_url(const char *link);

void xmi_msim_gui_utils_open_email(const char *address);

void xmi_msim_gui_utils_ensure_extension(gchar **filename, const gchar *extension);

gchar* xmi_msim_gui_utils_get_layer_element_string(struct xmi_layer *layer);

void xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(GtkWidget *controlsLogW, GTimer *timer, GtkTextBuffer *buffer, const gchar *text, gint len, GtkTextTag *first_tag, ...);
G_END_DECLS

GArray* xmi_msim_gui_utils_tree_view_get_selected_indices(GtkTreeView *tree);

#endif
