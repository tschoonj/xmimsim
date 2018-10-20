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
#include "xmimsim-gui-energies-box.h"
#include "xmimsim-gui-colors.h"
#include "xmimsim-gui-clipboard-manager.h"
#include "xmimsim-gui-undo-manager.h"
#include "xmi_xml.h"
#include <string.h>

GtkWidget *copy, *cut, *paste;
GtkWidget *undo, *redo, *save, *saveas;

static gboolean delete_event( GtkWidget *widget,
                              GdkEvent  *event,
                              gpointer   data ) {
	return FALSE;
}

static void destroy( GtkWidget *widget,
                     gpointer   data ) {
    gtk_main_quit ();
}

static void changed(XmiMsimGuiLayerBox *lbc, gchar *change, gpointer data) {
	xmi_composition *composition = xmi_msim_gui_layer_box_get_composition(lbc);

	fprintf(stdout, "change: %s\n", change);
	if (composition == NULL) {
		fprintf(stdout, "composition empty\n");
		return;
	}
	//xmi_print_layer(stdout, composition->layers, composition->n_layers);
	if (xmi_msim_gui_layer_box_get_layers_type(lbc) == XMI_MSIM_GUI_LAYER_BOX_TYPE_SAMPLE_COMPOSITION)
		fprintf(stdout, "reference_layer: %d\n", composition->reference_layer);
	fprintf(stdout, "\n");

	xmi_composition_free(composition);
}

static void update_clipboard_buttons(XmiMsimGuiClipboardManager *clipboard_manager, gboolean cut_copy_val, gboolean paste_val, gpointer data) {
	gtk_widget_set_sensitive(cut, cut_copy_val);
	gtk_widget_set_sensitive(copy, cut_copy_val);
	gtk_widget_set_sensitive(paste, paste_val);
}

static void n_photons_interval_writer(GValue *value, const xmi_input *input) {
	g_value_init(value, G_TYPE_LONG);
	g_value_set_long(value, input->general->n_photons_interval);
}

static void n_photons_interval_reader(const GValue *value, xmi_input *input) {
	input->general->n_photons_interval = g_value_get_long(value);
}

static XmiMsimGuiUndoManagerValueValidatorResult n_photons_interval_validator(GtkWidget *widget, xmi_input *current_input, GValue *value) {
	const gchar *text = gtk_entry_get_text(GTK_ENTRY(widget));
	gchar *endptr;

	gint64 val = g_ascii_strtoll(text, &endptr, 10);
	if (val < 1 || val == G_MAXINT64 || endptr != text + strlen(text)) {
		return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_INVALID;
	}

	if (val == current_input->general->n_photons_interval)
		return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_EQUAL;

	g_value_init(value, G_TYPE_LONG);
	g_value_set_long(value, val);
	
	return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_VALID;
}

static void comments_writer(GValue *value, const xmi_input *input) {
	g_value_init(value, G_TYPE_STRING);
	g_value_set_string(value, input->general->comments);
}

static void comments_reader(const GValue *value, xmi_input *input) {
	const gchar *all_text = NULL;
	if (G_VALUE_HOLDS(value, XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_INSERT_DATA)) {
		all_text = xmi_msim_gui_undo_manager_text_view_insert_data_get_all_text((XmiMsimGuiUndoManagerTextViewInsertData *) g_value_get_boxed(value));
	}
	else if (G_VALUE_HOLDS(value, XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_DELETE_DATA)) {
		all_text = xmi_msim_gui_undo_manager_text_view_delete_data_get_all_text((XmiMsimGuiUndoManagerTextViewDeleteData *) g_value_get_boxed(value));
	}
	else {
		g_warning("comments reader: unknown type %s detected", G_VALUE_TYPE_NAME(value));
		return;
	}
	g_debug("all_text: %s", all_text);
	g_free(input->general->comments);
	input->general->comments = g_strdup(all_text);	
}

static void update_status_buttons(XmiMsimGuiUndoManager *manager, gboolean saveas_status, gboolean save_status, gchar *undo_string, gchar *redo_string, gpointer data) {
	gtk_widget_set_sensitive(saveas, saveas_status);
	gtk_widget_set_sensitive(save, save_status);
	gtk_widget_set_sensitive(undo, undo_string != NULL);
	gtk_widget_set_sensitive(redo, redo_string != NULL);
	gtk_widget_set_tooltip_text(undo, undo_string);
	gtk_widget_set_tooltip_text(redo, redo_string);
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

	GtkWidget *clipboard_grid = gtk_grid_new();
	gtk_widget_set_halign(clipboard_grid, GTK_ALIGN_FILL);
	gtk_widget_set_valign(clipboard_grid, GTK_ALIGN_START);
	gtk_widget_set_hexpand(clipboard_grid, TRUE);
	gtk_widget_set_vexpand(clipboard_grid, FALSE);
	gtk_grid_attach(GTK_GRID(clipboard_grid), copy, 0, 0, 1, 1);
	gtk_grid_attach(GTK_GRID(clipboard_grid), cut, 1, 0, 1, 1);
	gtk_grid_attach(GTK_GRID(clipboard_grid), paste, 2, 0, 1, 1);

	gtk_grid_attach(GTK_GRID(grid), clipboard_grid, 0, 0, 1, 1);

	gtk_widget_set_sensitive(copy, FALSE);
	gtk_widget_set_sensitive(cut, FALSE);
	gtk_widget_set_sensitive(paste, FALSE);
	gtk_widget_set_can_focus(copy, FALSE);
	gtk_widget_set_can_focus(cut, FALSE);
	gtk_widget_set_can_focus(paste, FALSE);

	save = gtk_button_new_with_label("Save");
	saveas = gtk_button_new_with_label("Save As");
	undo = gtk_button_new_with_label("Undo");
	redo = gtk_button_new_with_label("Redo");

	gtk_widget_set_halign(save, GTK_ALIGN_END);
	gtk_widget_set_halign(saveas, GTK_ALIGN_CENTER);
	gtk_widget_set_halign(undo, GTK_ALIGN_CENTER);
	gtk_widget_set_halign(redo, GTK_ALIGN_START);
	gtk_widget_set_valign(save, GTK_ALIGN_START);
	gtk_widget_set_valign(saveas, GTK_ALIGN_START);
	gtk_widget_set_valign(undo, GTK_ALIGN_START);
	gtk_widget_set_valign(redo, GTK_ALIGN_START);
	gtk_widget_set_hexpand(save, TRUE);
	gtk_widget_set_hexpand(redo, TRUE);

	GtkWidget *undo_grid = gtk_grid_new();
	gtk_widget_set_halign(undo_grid, GTK_ALIGN_FILL);
	gtk_widget_set_valign(undo_grid, GTK_ALIGN_START);
	gtk_widget_set_hexpand(undo_grid, TRUE);
	gtk_widget_set_vexpand(undo_grid, FALSE);
	gtk_grid_attach(GTK_GRID(undo_grid), save, 0, 0, 1, 1);
	gtk_grid_attach(GTK_GRID(undo_grid), saveas, 1, 0, 1, 1);
	gtk_grid_attach(GTK_GRID(undo_grid), undo, 2, 0, 1, 1);
	gtk_grid_attach(GTK_GRID(undo_grid), redo, 3, 0, 1, 1);

	gtk_grid_attach(GTK_GRID(grid), undo_grid, 0, 1, 1, 1);

	GtkWidget *lb = xmi_msim_gui_layer_box_new(XMI_MSIM_GUI_LAYER_BOX_TYPE_SAMPLE_COMPOSITION);
	int Z[3] = {12, 18, 26};
	double weight[3] = {0.3, 0.5, 0.2};
	xmi_layer layer = {.n_elements = 3, .Z = Z, .weight = weight, .density = 4.5, .thickness = 1.3};
	xmi_composition composition = {.n_layers = 1, .layers = &layer, .reference_layer = 0};
	//xmi_msim_gui_layer_box_set_composition(XMI_MSIM_GUI_LAYER_BOX(lb), &composition);
	g_signal_connect(G_OBJECT(lb), "changed", G_CALLBACK(changed), NULL);
	gtk_grid_attach(GTK_GRID(grid), lb, 0, 2, 1, 1);
	
	GtkWidget *lb2 = xmi_msim_gui_layer_box_new(XMI_MSIM_GUI_LAYER_BOX_TYPE_EXCITATION_ABSORBERS);
	g_signal_connect(G_OBJECT(lb2), "changed", G_CALLBACK(changed), NULL);
	gtk_grid_attach(GTK_GRID(grid), lb2, 0, 3, 1, 1);

	GtkWidget *entry = gtk_entry_new();
	gtk_grid_attach(GTK_GRID(grid), entry, 0, 4, 1, 1);

	GtkWidget *view = gtk_text_view_new();
	GtkWidget *frame = gtk_frame_new("Comments");
	gtk_widget_set_size_request(frame, -1, 100);
	gtk_container_add(GTK_CONTAINER(frame), view);
	gtk_grid_attach(GTK_GRID(grid), frame, 0, 5, 1, 1);
	
	GtkWidget* eb = xmi_msim_gui_energies_box_new();
	gtk_grid_attach(GTK_GRID(grid), eb, 0, 6, 1, 1);

	gtk_container_add(GTK_CONTAINER(window), grid);

	XmiMsimGuiClipboardManager *clipboard_manager = xmi_msim_gui_clipboard_manager_new();
	xmi_msim_gui_clipboard_manager_register_widget(clipboard_manager, lb);
	xmi_msim_gui_clipboard_manager_register_widget(clipboard_manager, lb2);
	xmi_msim_gui_clipboard_manager_register_widget(clipboard_manager, entry);
	xmi_msim_gui_clipboard_manager_register_widget(clipboard_manager, view);
	//xmi_msim_gui_clipboard_manager_register_widget(clipboard_manager, eb);
	g_signal_connect(clipboard_manager, "update-clipboard-buttons", G_CALLBACK(update_clipboard_buttons), NULL);
	g_signal_connect_swapped(cut, "clicked", G_CALLBACK(xmi_msim_gui_clipboard_manager_cut), clipboard_manager);
	g_signal_connect_swapped(copy, "clicked", G_CALLBACK(xmi_msim_gui_clipboard_manager_copy), clipboard_manager);
	g_signal_connect_swapped(paste, "clicked", G_CALLBACK(xmi_msim_gui_clipboard_manager_paste), clipboard_manager);
	XmiMsimGuiUndoManager *undo_manager = xmi_msim_gui_undo_manager_new();
	g_assert_true(xmi_msim_gui_undo_manager_register_text_view(undo_manager, GTK_TEXT_VIEW(view), comments_writer, comments_reader));
	g_assert_true(xmi_msim_gui_undo_manager_register_layer_box(undo_manager, XMI_MSIM_GUI_LAYER_BOX(lb)));
	g_assert_true(xmi_msim_gui_undo_manager_register_layer_box(undo_manager, XMI_MSIM_GUI_LAYER_BOX(lb2)));
	g_assert_true(xmi_msim_gui_undo_manager_register_energies_box(undo_manager, XMI_MSIM_GUI_ENERGIES_BOX(eb)));
	g_assert_true(xmi_msim_gui_undo_manager_register_entry(undo_manager, GTK_ENTRY(entry), "number of photons per interval changed", n_photons_interval_writer, n_photons_interval_reader, n_photons_interval_validator));
	g_signal_connect(undo_manager, "update-status-buttons", G_CALLBACK(update_status_buttons), NULL);
	g_signal_connect_swapped(undo, "clicked", G_CALLBACK(xmi_msim_gui_undo_manager_undo), undo_manager);
	g_signal_connect_swapped(redo, "clicked", G_CALLBACK(xmi_msim_gui_undo_manager_redo), undo_manager);

	// load example file
	GError *error = NULL;
	gboolean read_rv = xmi_msim_gui_undo_manager_load_file(undo_manager, SRM1155_XMSI, &error);
	if (!read_rv)
		fprintf(stderr, "error message: %s\n", error->message);

	g_assert_cmpstr(SRM1155_XMSI, ==, xmi_msim_gui_undo_manager_get_filename(undo_manager));

	gtk_widget_show_all(window);

	gtk_main();
	return 0;
}
