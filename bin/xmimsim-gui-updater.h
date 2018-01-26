/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

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



#ifndef XMIMSIM_GUI_UPDATER_H
#define XMIMSIM_GUI_UPDATER_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

typedef enum {
	XMI_MSIM_UPDATES_ERROR,
	XMI_MSIM_UPDATES_AVAILABLE,
	XMI_MSIM_UPDATES_NONE
} XmiMsimGuiUpdaterCheck;

void xmi_msim_gui_updater_check_for_updates_async(GtkWidget *window, GAsyncReadyCallback callback, gpointer user_data);
XmiMsimGuiUpdaterCheck xmi_msim_gui_updater_check_for_updates_finish(GtkWidget *window, GAsyncResult *result, gchar **max_version, gchar **message, GError **error);

int xmi_msim_gui_updater_download_updates_dialog(GtkWidget *window, gchar *max_version, gchar *message);

void     xmi_msim_gui_updater_check_download_url_async(GtkListStore *store, gchar *download_url, GAsyncReadyCallback callback, gpointer user_data);
gboolean xmi_msim_gui_updater_check_download_url_finish(GtkListStore *store, GAsyncResult *result);

typedef enum {
	XMI_MSIM_GUI_UPDATER_JSON_TYPE_MISMATCH,
	XMI_MSIM_GUI_UPDATER_JSON_MISSING_MEMBER,
} XmiMsimGuiUpdaterError;

#define XMI_MSIM_GUI_UPDATER_ERROR (xmi_msim_gui_updater_error_quark())

GQuark xmi_msim_gui_updater_error_quark(void);
G_END_DECLS
#endif
