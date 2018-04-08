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
#include "xmimsim-gui-clipboard-manager.h"
#include "xmi_xml.h"

GtkWidget *copy, *cut, *paste;

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
	struct xmi_composition *composition = xmi_msim_gui_layer_box_get_composition(lb);

	fprintf(stdout, "change: %s\n", change);
	xmi_print_layer(stdout, composition->layers, composition->n_layers);
	if (xmi_msim_gui_layer_box_get_layers_type(lb) == XMI_MSIM_GUI_LAYER_BOX_TYPE_SAMPLE_COMPOSITION)
		fprintf(stdout, "reference_layer: %d\n", composition->reference_layer);
	fprintf(stdout, "\n");

	xmi_free_composition(composition);
}

void update_clipboard_buttons(XmiMsimGuiClipboardManager *clipboard_manager, gboolean cut_copy_val, gboolean paste_val, gpointer data) {
	gtk_widget_set_sensitive(cut, cut_copy_val);
	gtk_widget_set_sensitive(copy, cut_copy_val);
	gtk_widget_set_sensitive(paste, paste_val);
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

	GtkWidget *grid = gtk_grid_new();
	gtk_widget_set_halign(grid, GTK_ALIGN_FILL);
	gtk_widget_set_valign(grid, GTK_ALIGN_FILL);
	gtk_widget_set_hexpand(grid, TRUE);
	gtk_widget_set_vexpand(grid, TRUE);

	copy = gtk_button_new_with_label("Copy");
	cut = gtk_button_new_with_label("Cut");
	paste = gtk_button_new_with_label("Paste");

	gtk_widget_set_halign(copy, GTK_ALIGN_END);
	gtk_widget_set_halign(cut, GTK_ALIGN_CENTER);
	gtk_widget_set_halign(paste, GTK_ALIGN_START);
	gtk_widget_set_valign(copy, GTK_ALIGN_START);
	gtk_widget_set_valign(cut, GTK_ALIGN_START);
	gtk_widget_set_valign(paste, GTK_ALIGN_START);
	gtk_widget_set_hexpand(copy, TRUE);
	gtk_widget_set_hexpand(paste, TRUE);

	gtk_grid_attach(GTK_GRID(grid), copy, 0, 0, 1, 1);
	gtk_grid_attach(GTK_GRID(grid), cut, 1, 0, 1, 1);
	gtk_grid_attach(GTK_GRID(grid), paste, 2, 0, 1, 1);

	gtk_widget_set_sensitive(copy, FALSE);
	gtk_widget_set_sensitive(cut, FALSE);
	gtk_widget_set_sensitive(paste, FALSE);
	gtk_widget_set_can_focus(copy, FALSE);
	gtk_widget_set_can_focus(cut, FALSE);
	gtk_widget_set_can_focus(paste, FALSE);

	GtkWidget *lb = xmi_msim_gui_layer_box_new(XMI_MSIM_GUI_LAYER_BOX_TYPE_SAMPLE_COMPOSITION);
	int Z[3] = {12, 18, 26};
	double weight[3] = {0.3, 0.5, 0.2};
	struct xmi_layer layer = {.n_elements = 3, .Z = Z, .weight = weight, .density = 4.5, .thickness = 1.3};
	struct xmi_composition composition = {.n_layers = 1, .layers = &layer, .reference_layer = 0};
	xmi_msim_gui_layer_box_set_composition(XMI_MSIM_GUI_LAYER_BOX(lb), &composition);
	g_signal_connect(G_OBJECT(lb), "changed", G_CALLBACK(changed), NULL);
	gtk_grid_attach(GTK_GRID(grid), lb, 0, 1, 3, 1);
	
	GtkWidget *lb2 = xmi_msim_gui_layer_box_new(XMI_MSIM_GUI_LAYER_BOX_TYPE_EXCITATION_ABSORBERS);
	g_signal_connect(G_OBJECT(lb2), "changed", G_CALLBACK(changed), NULL);
	gtk_grid_attach(GTK_GRID(grid), lb2, 0, 2, 3, 1);

	GtkWidget *entry = gtk_entry_new();
	gtk_grid_attach(GTK_GRID(grid), entry, 0, 3, 3, 1);

	gtk_container_add(GTK_CONTAINER(window), grid);

	XmiMsimGuiClipboardManager *clipboard_manager = xmi_msim_gui_clipboard_manager_new();
	xmi_msim_gui_clipboard_manager_register_widget(clipboard_manager, lb);
	xmi_msim_gui_clipboard_manager_register_widget(clipboard_manager, lb2);
	xmi_msim_gui_clipboard_manager_register_widget(clipboard_manager, entry);
	g_signal_connect(clipboard_manager, "update-clipboard-buttons", G_CALLBACK(update_clipboard_buttons), NULL);
	g_signal_connect_swapped(cut, "clicked", G_CALLBACK(xmi_msim_gui_clipboard_manager_cut), clipboard_manager);
	g_signal_connect_swapped(copy, "clicked", G_CALLBACK(xmi_msim_gui_clipboard_manager_copy), clipboard_manager);
	g_signal_connect_swapped(paste, "clicked", G_CALLBACK(xmi_msim_gui_clipboard_manager_paste), clipboard_manager);

	gtk_widget_show_all(window);

	gtk_main();
	return 0;
}
