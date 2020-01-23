/*
Copyright (C) 2017-2020 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMIMSIM_GUI_OSX_H
#define XMIMSIM_GUI_OSX_H

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

void xmi_msim_gui_osx_app_disable_tabbing(void);

void xmi_msim_gui_osx_window_set_file(GtkWidget *window, const gchar *filename);

void xmi_msim_gui_osx_window_bring_to_front(GtkWidget *window);

void xmi_msim_gui_osx_window_enable_full_screen(GtkWidget *window);

typedef void XmiMsimGuiOSXApplicationDelegate;

XmiMsimGuiOSXApplicationDelegate* xmi_msim_gui_osx_app_delegate_new(void);

void xmi_msim_gui_osx_app_delegate_free(XmiMsimGuiOSXApplicationDelegate *delegate);

G_END_DECLS

#endif
