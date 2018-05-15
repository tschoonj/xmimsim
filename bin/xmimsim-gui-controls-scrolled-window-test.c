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

#include "xmimsim-gui-controls-scrolled-window.h"
#include "xmi_xml.h"

static gboolean delete_event( GtkWidget *widget,
                              GdkEvent  *event,
                              gpointer   data ) {
	return FALSE;
}

static void destroy( GtkWidget *widget,
                     gpointer   data ) {
    gtk_main_quit ();
}


int main(int argc, char *argv[]) {

	gtk_init(&argc, &argv);
	xmi_xmlLoadCatalog();

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(window), 1200, 1200);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	g_signal_connect (window, "delete-event",
		      G_CALLBACK (delete_event), NULL);
	g_signal_connect (window, "destroy",
		      G_CALLBACK (destroy), NULL);
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	XmiMsimGuiUndoManager *manager = xmi_msim_gui_undo_manager_new();
	GtkWidget *sw = xmi_msim_gui_controls_scrolled_window_new(manager);
	g_object_unref(manager);
	gtk_widget_show(sw);
	gtk_container_add(GTK_CONTAINER(window), sw);

	// load example file
	g_assert(argc == 2);
	GError *error = NULL;
	gboolean read_rv = xmi_msim_gui_undo_manager_load_file(manager, argv[1], &error);
	if (!read_rv)
		fprintf(stderr, "error message: %s\n", error->message);

	gtk_widget_show(window);

	gtk_main();

	return 0;
}
