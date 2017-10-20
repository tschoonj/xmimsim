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

#include <config.h>
#include "xmimsim-gui-xmso-results-scrolled-window.h"
#include <xraylib.h>
#include "xmi_data_structs.h"
#include <gtkmm-plplot/plot2d.h>
#include <gtkmm-plplot/canvas.h>

enum XmiMsimGuiXmsoResultsScrolledWindowPlotMode {
	CONVOLUTED,
	UNCONVOLUTED
};

enum {
	ELEMENT_COLUMN,
	LINE_COLUMN,
	ENERGY_COLUMN,
	SHOW_LINE_COLUMN,
	CONSISTENT_COLUMN,
	CHILD_COLUMN,
	INTERACTION_COLUMN,
	COUNTS_COLUMN,
	N_COLUMNS
};

struct _XmiMsimGuiXmsoResultsScrolledWindow {
	GtkWindow parent_instance;
	GtkWidget *parent_window;
	struct xmi_output *results;	
	double plot_xmin;
	double plot_xmax;
	double plot_ymin_conv;
	double plot_ymin_unconv;
	double plot_ymax_conv;
	double plot_ymax_unconv;
	XmiMsimGuiXmsoResultsScrolledWindowPlotMode plot_mode;
	Gtk::PLplot::Canvas *canvas;
	GtkWidget *spectra_button_box;
	GtkWidget *x_coord;
	GtkWidget *y_coord;
	GtkWidget *channel_coord;
	GtkWidget *settings_button;
	GtkWidget *export_button;
	GtkTreeStore *counts_tree_store;
	GtkWidget *counts_tree_view;
};

struct _XmiMsimGuiXmsoResultsScrolledWindowClass
{
	GtkScrolledWindowClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiXmsoResultsScrolledWindow, xmi_msim_gui_xmso_results_scrolled_window, GTK_TYPE_SCROLLED_WINDOW)

static void xmi_msim_gui_xmso_results_scrolled_window_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_xmso_results_scrolled_window_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_xmso_results_scrolled_window_finalize(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_xmso_results_scrolled_window_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_xmso_results_scrolled_window_class_init(XmiMsimGuiXmsoResultsScrolledWindowClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_xmso_results_scrolled_window_dispose;
	object_class->finalize = xmi_msim_gui_xmso_results_scrolled_window_finalize;
}

static void zoom_out(void);

class Plot2D : public Gtk::PLplot::Plot2D {
	public:
	Plot2D(
		const Glib::ustring &axis_title_x,
		const Glib::ustring &axis_title_y) :
		Gtk::PLplot::Plot2D(axis_title_x, axis_title_y) {}
	virtual void on_double_press(double x, double y) override {
		//zoom_out();
	}

};

static void xmi_msim_gui_xmso_results_scrolled_window_init(XmiMsimGuiXmsoResultsScrolledWindow *self) {

	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);

	self->plot_mode = CONVOLUTED;

	GtkWidget *paned = gtk_paned_new(GTK_ORIENTATION_VERTICAL);
#if GTK_CHECK_VERSION(3, 16, 0)
	gtk_paned_set_wide_handle(GTK_PANED(paned), TRUE);
#endif
	gtk_container_add(GTK_CONTAINER(self), paned);

	GtkWidget *top_paned = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	GtkWidget *aspect_frame = gtk_aspect_frame_new("", 0.0, 0.0, 842.0/595.0, FALSE);
	self->canvas = Gtk::manage(new Gtk::PLplot::Canvas());
	self->canvas->set_hexpand(true);
	self->canvas->set_vexpand(true);
	gtk_widget_set_hexpand(aspect_frame, TRUE);
	gtk_widget_set_vexpand(aspect_frame, TRUE);
	gtk_container_add(GTK_CONTAINER(aspect_frame), GTK_WIDGET(self->canvas->gobj()));

	gtk_box_pack_start(GTK_BOX(top_paned), aspect_frame, TRUE, TRUE, 0);

	GtkWidget *spectra_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
	self->spectra_button_box = gtk_list_box_new();
	GtkWidget *frame = gtk_frame_new(NULL);
	//gtk_widget_set_vexpand(frame, TRUE);
	gtk_container_add(GTK_CONTAINER(frame), self->spectra_button_box);
	gtk_box_pack_start(GTK_BOX(spectra_box), frame, TRUE, TRUE, 2);
	gtk_box_pack_start(GTK_BOX(top_paned), spectra_box, FALSE, FALSE, 0);
	// add default label
	GtkWidget *label = gtk_label_new("Please run a simulation\nor load an XMSO file");
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	GtkWidget *list_box_row = gtk_list_box_row_new();
	gtk_container_add(GTK_CONTAINER(list_box_row), label);
#if GTK_CHECK_VERSION(3, 14, 0)
	gtk_list_box_row_set_activatable(GTK_LIST_BOX_ROW(list_box_row), FALSE);
	gtk_list_box_row_set_selectable(GTK_LIST_BOX_ROW(list_box_row), FALSE);
#endif
	gtk_widget_set_vexpand(list_box_row, TRUE);
	// cannot seem to get the row take up all the space!!!
	gtk_widget_set_valign(list_box_row, GTK_ALIGN_FILL);
	
	gtk_container_add(GTK_CONTAINER(self->spectra_button_box), list_box_row);

	GtkWidget *grid = gtk_grid_new();

	self->settings_button = gtk_button_new_with_label("Properties");
	//g_signal_connect(G_OBJECT(settings_button),"clicked",G_CALLBACK(settings_button_clicked_cb),(gpointer)window);
	gtk_grid_attach(GTK_GRID(grid), self->settings_button, 0, 0, 2, 1);
	gtk_widget_set_sensitive(self->settings_button, FALSE);
	self->export_button = gtk_button_new_with_label("Export plot");
	//g_signal_connect(G_OBJECT(export_button),"clicked",G_CALLBACK(export_button_clicked_cb),(gpointer)window);
	gtk_grid_attach(GTK_GRID(grid), self->export_button, 0, 1, 2, 1);
	gtk_widget_set_sensitive(self->export_button, FALSE);
	label = gtk_label_new("Energy (keV)");
	gtk_grid_attach(GTK_GRID(grid), label, 0, 2, 1, 1);
	self->x_coord = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->x_coord), FALSE);
	gtk_grid_attach(GTK_GRID(grid), self->x_coord, 1, 2, 1, 1);
	label = gtk_label_new("Channel number");
	gtk_grid_attach(GTK_GRID(grid), label, 0, 3, 1, 1);
	self->channel_coord = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->channel_coord), FALSE);
	gtk_grid_attach(GTK_GRID(grid), self->channel_coord, 1, 3, 1, 1);
	label = gtk_label_new("Intensity");
	gtk_grid_attach(GTK_GRID(grid), label, 0, 4, 1, 1);
	self->y_coord = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->y_coord), FALSE);
	gtk_grid_attach(GTK_GRID(grid), self->y_coord, 1, 4, 1, 1);
	gtk_box_pack_start(GTK_BOX(spectra_box), grid, FALSE, FALSE, 2);

	gtk_container_set_border_width(GTK_CONTAINER(top_paned), 5);
	gtk_widget_show_all(top_paned);

	gtk_paned_pack1(GTK_PANED(paned), top_paned, TRUE, FALSE);

	self->counts_tree_store = gtk_tree_store_new(
		N_COLUMNS,
		G_TYPE_STRING,
		G_TYPE_STRING,
		G_TYPE_DOUBLE,
		G_TYPE_BOOLEAN,
		G_TYPE_BOOLEAN,
		G_TYPE_POINTER,
		G_TYPE_STRING,
		G_TYPE_DOUBLE
	);

	self->counts_tree_view = gtk_tree_view_new_with_model(GTK_TREE_MODEL(self->counts_tree_store));
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	GtkTreeViewColumn *column = gtk_tree_view_column_new_with_attributes(
		"Element", renderer,
		"text", ELEMENT_COLUMN,
		NULL
	);
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->counts_tree_view), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes(
		"XRF line", renderer,
		"text", LINE_COLUMN,
		NULL
	);
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->counts_tree_view), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column,"Energy");
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->counts_tree_view), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	//gtk_tree_view_column_set_cell_data_func(column, renderer, cell_print_double, GINT_TO_POINTER(ENERGY_COLUMN), NULL);

	renderer = gtk_cell_renderer_toggle_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	gtk_cell_renderer_toggle_set_radio(GTK_CELL_RENDERER_TOGGLE(renderer),FALSE);
	gtk_cell_renderer_toggle_set_activatable(GTK_CELL_RENDERER_TOGGLE(renderer),TRUE);
	//g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(cell_active_toggle), NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column,"Show line");
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->counts_tree_view), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	//gtk_tree_view_column_set_cell_data_func(column, renderer, cell_visible_toggle, NULL, NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes(
		"# Interactions", renderer,
		"text", INTERACTION_COLUMN,
		NULL
	);
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->counts_tree_view), column);

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column,"Intensity");
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->counts_tree_view), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	//gtk_tree_view_column_set_cell_data_func(column, renderer, cell_print_double, GINT_TO_POINTER(COUNTS_COLUMN), NULL);

	gtk_widget_set_size_request(self->counts_tree_view,-1, 150);
	gtk_widget_show_all(self->counts_tree_view);

	gtk_paned_pack2(GTK_PANED(paned), self->counts_tree_view, TRUE, FALSE);
}

GtkWidget* xmi_msim_gui_xmso_results_scrolled_window_new(GtkWidget *parent_window) {

	g_return_val_if_fail(parent_window != NULL, NULL);

	XmiMsimGuiXmsoResultsScrolledWindow *scrolled_window = XMI_MSIM_GUI_XMSO_RESULTS_SCROLLED_WINDOW(g_object_new(XMI_MSIM_GUI_TYPE_XMSO_RESULTS_SCROLLED_WINDOW, NULL));

	scrolled_window->parent_window = parent_window;

	return GTK_WIDGET(scrolled_window);
}

gboolean xmi_msim_gui_xmso_results_scrolled_window_load_from_file(XmiMsimGuiXmsoResultsScrolledWindow *window, const gchar *xmsofile) {
	return TRUE;
}
