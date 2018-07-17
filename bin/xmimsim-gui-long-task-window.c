/*
Copyright (C) 2018 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-long-task-window.h"

struct _XmiMsimGuiLongTaskWindow {
	GtkWindow parent_instance;
	GtkWidget *label;
};

struct _XmiMsimGuiLongTaskWindowClass {
	GtkWindowClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiLongTaskWindow, xmi_msim_gui_long_task_window, GTK_TYPE_WINDOW)

static void xmi_msim_gui_long_task_window_class_init(XmiMsimGuiLongTaskWindowClass *klass) {

}

static void xmi_msim_gui_long_task_window_init(XmiMsimGuiLongTaskWindow *self) {

	GtkWidget *main_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	self->label = gtk_label_new(NULL);
	gtk_label_set_line_wrap(GTK_LABEL(self->label), TRUE);
	gtk_box_pack_start(GTK_BOX(main_vbox), self->label, TRUE, FALSE, 10);
	GtkWidget *label2 = gtk_label_new("This may take a while...");
	gtk_box_pack_start(GTK_BOX(main_vbox), label2, FALSE, FALSE, 10);
	gtk_container_add(GTK_CONTAINER(self), main_vbox);
	g_signal_connect(G_OBJECT(self), "delete-event", G_CALLBACK(gtk_true), NULL);
	gtk_widget_show_all(main_vbox);
}

GtkWidget* xmi_msim_gui_long_task_window_new(GtkWindow *parent_window) {
	GObject *rv = g_object_new(XMI_MSIM_GUI_TYPE_LONG_TASK_WINDOW,
		"transient-for", parent_window,
		"window-position", GTK_WIN_POS_CENTER,
		"modal", TRUE,
		"default-width", 200,
		"default-height", 50,
		"type", GTK_WINDOW_TOPLEVEL,
		"destroy-with-parent", TRUE,
		"decorated", FALSE,
		"border-width", 5,
		NULL);

	return GTK_WIDGET(rv);
}

void xmi_msim_gui_long_task_window_set_text(XmiMsimGuiLongTaskWindow *window, const gchar *text) {
	g_return_if_fail(XMI_MSIM_GUI_IS_LONG_TASK_WINDOW(window));
	gtk_label_set_markup(GTK_LABEL(window->label), text);
}
