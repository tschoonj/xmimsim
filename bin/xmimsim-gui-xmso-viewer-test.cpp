/*
Copyright (C) 2017 Tom Schoonjans and Laszlo Vincze

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

#include "xmimsim-gui-xmso-results-scrolled-window.h"
#include "xmimsim-gui-colors.h"
#include "xmi_data_structs.h"
#include <gtkmm/main.h>
#include "xmi_xml.h"

#include <stdio.h>

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

	if (argc != 2) {
		fprintf(stderr, "Usage: %s xmso-file\n", argv[0]);
		return 1;
	}

	gtk_init(&argc, &argv);
	Gtk::Main::init_gtkmm_internals();
	xmi_msim_gui_utils_init_colors();

	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		return 1;
	}

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(window), 1200, 1200);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	g_signal_connect (window, "delete-event",
		      G_CALLBACK (delete_event), NULL);
	g_signal_connect (window, "destroy",
		      G_CALLBACK (destroy), NULL);
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	GtkWidget *xmso_sw = xmi_msim_gui_xmso_results_scrolled_window_new();
	gtk_container_add(GTK_CONTAINER(window), xmso_sw);

	GError *error = NULL;
	if (xmi_msim_gui_xmso_results_scrolled_window_load_from_file(XMI_MSIM_GUI_XMSO_RESULTS_SCROLLED_WINDOW(xmso_sw), argv[1], &error) == FALSE) {
		fprintf(stderr, "FATAL ERROR: %s\n", error->message);
		return 1;
	}

	gtk_widget_show_all(window);

	gtk_main();
}
