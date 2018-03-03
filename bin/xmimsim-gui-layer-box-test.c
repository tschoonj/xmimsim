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

#include "xmimsim-gui-layer-box.h"
#include "xmimsim-gui-colors.h"
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

static void changed(XmiMsimGuiLayerBox *lb, gchar *change, gpointer data) {
	guint n_layers;
	struct xmi_layer *layers = NULL;
	int reference_layer = -1;
	xmi_msim_gui_layer_box_get_layers(lb, &n_layers, &layers, &reference_layer);

	fprintf(stdout, "change: %s\n", change);
	xmi_print_layer(stdout, layers, n_layers);
	if (xmi_msim_gui_layer_box_get_layers_type(lb) == XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION)
		fprintf(stdout, "reference_layer: %d\n", reference_layer);
	fprintf(stdout, "\n");

	int i;
	for (i = 0 ; i < n_layers ; i++) {
		xmi_free_layer(&layers[i]);
	}
	g_free(layers);
}

int main(int argc, char *argv[]) {


	gtk_init(&argc, &argv);
	xmi_msim_gui_utils_init_colors();
	xmi_xmlLoadCatalog();

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(window), 1200, 1200);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	g_signal_connect (window, "delete-event",
		      G_CALLBACK (delete_event), NULL);
	g_signal_connect (window, "destroy",
		      G_CALLBACK (destroy), NULL);
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	GtkWidget *lb = xmi_msim_gui_layer_box_new(XMI_MSIM_GUI_LAYER_BOX_TYPE_COMPOSITION);
	int Z[3] = {12, 18, 26};
	double weight[3] = {0.3, 0.5, 0.2};
	struct xmi_layer layer = {.n_elements = 3, .Z = Z, .weight = weight, .density = 4.5, .thickness = 1.3};
	xmi_msim_gui_layer_box_set_layers(XMI_MSIM_GUI_LAYER_BOX(lb), 1, &layer, 0);
	gtk_container_add(GTK_CONTAINER(window), lb);
	g_signal_connect(G_OBJECT(lb), "changed", G_CALLBACK(changed), NULL);

	gtk_widget_show_all(window);

	gtk_main();
	return 0;
}
