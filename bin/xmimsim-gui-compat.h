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

#ifndef XMIMSIM_GUI_COMPAT_H
#define XMIMSIM_GUI_COMPAT_H

#include <gtk/gtk.h>

// native file chooser dialogs!
// only in 3.22.17 was support for filters added (by me :-D)
#if GTK_CHECK_VERSION(3, 22, 17)
	typedef GtkFileChooserNative XmiMsimGuiFileChooserDialog;
	#define xmi_msim_gui_file_chooser_dialog_new(title, window, action, accept_label, cancel_label) \
		gtk_file_chooser_native_new( \
			title, \
			window, \
			action, \
			NULL, \
			NULL \
		)
	#define xmi_msim_gui_file_chooser_dialog_run(dialog) \
		gtk_native_dialog_run(GTK_NATIVE_DIALOG(dialog))
	#define xmi_msim_gui_file_chooser_dialog_destroy(dialog) \
		g_object_unref(dialog)
	#define xmi_msim_gui_file_chooser_dialog_set_modal(dialog, boolean) \
		gtk_native_dialog_set_modal(GTK_NATIVE_DIALOG(dialog), boolean)
#else
	typedef GtkWidget XmiMsimGuiFileChooserDialog;
	#define xmi_msim_gui_file_chooser_dialog_new(title, window, action, accept_label, cancel_label) \
		gtk_file_chooser_dialog_new( \
			title, \
			window, \
			action, \
			accept_label, \
			GTK_RESPONSE_ACCEPT, \
			cancel_label, \
			GTK_RESPONSE_CANCEL, \
			NULL \
		)
	#define xmi_msim_gui_file_chooser_dialog_run(dialog) \
		gtk_dialog_run(GTK_DIALOG(dialog))
	#define xmi_msim_gui_file_chooser_dialog_destroy(dialog) \
		gtk_widget_destroy(dialog)
	#define xmi_msim_gui_file_chooser_dialog_set_modal(dialog, boolean) \
		gtk_window_set_modal(GTK_WINDOW(dialog), boolean)
#endif


#if !GTK_CHECK_VERSION(3, 12, 0)
	#define gtk_widget_set_margin_start(widget, margin) (gtk_widget_set_margin_left(widget, margin))
#endif
#endif
