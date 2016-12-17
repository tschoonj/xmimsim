/*
Copyright (C) 2016 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-sources-dialog.h"
#include "xmimsim-gui-source-abstract.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-fonts.h"
#include "xmi_aux.h"

#if GTK_MAJOR_VERSION == 3
class Plot2DSources : public Gtk::PLplot::Plot2D {
	public:
	Plot2DSources(
		const Glib::ustring &axis_title_x,
		const Glib::ustring &axis_title_y) :
		Gtk::PLplot::Plot2D(axis_title_x, axis_title_y) {}
	virtual void on_double_press(double x, double y) override {
		// do nothing -> we will connect a signal handler
	}
};
#endif

G_DEFINE_TYPE(XmiMsimGuiSourcesDialog, xmi_msim_gui_sources_dialog, GTK_TYPE_DIALOG)

static void update_plot(XmiMsimGuiSourcesDialog *dialog, XmiMsimGuiSourceAbstract *source);

static void xmi_msim_gui_sources_dialog_class_init(XmiMsimGuiSourcesDialogClass *klass) {

}

static XmiMsimGuiSourceAbstract* get_active_source(XmiMsimGuiSourcesDialog *dialog) {
	gint current_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(dialog->notebookW));
	return XMI_MSIM_GUI_SOURCE_ABSTRACT(gtk_notebook_get_nth_page(GTK_NOTEBOOK(dialog->notebookW), current_page));
}

static gboolean activate_link_cb(GtkLabel *label, gchar *uri, gpointer data) {

	xmi_msim_gui_utils_open_url((char *) uri);
	return TRUE;
}

static void info_button_clicked_cb(XmiMsimGuiSourcesDialog *dialog) {
	XmiMsimGuiSourceAbstract *source = get_active_source(dialog);
	GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "%s", xmi_msim_gui_source_abstract_get_name(source));
	gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "%s", xmi_msim_gui_source_abstract_get_about_text(source));

	GtkWidget *area = gtk_message_dialog_get_message_area(GTK_MESSAGE_DIALOG(info_dialog));
	GList *children = gtk_container_get_children(GTK_CONTAINER(area));
	GtkWidget *temp = (GtkWidget *) g_list_nth_data(children, 1);
	g_list_free(children);
	g_signal_connect(G_OBJECT(temp), "activate-link", G_CALLBACK(activate_link_cb), NULL);

	gtk_dialog_run(GTK_DIALOG(info_dialog));
	gtk_widget_destroy(info_dialog);
}

static void generate_button_clicked_cb(XmiMsimGuiSourcesDialog *dialog) {
	// disable generate button
	gtk_widget_set_sensitive(dialog->generateButton, FALSE);

	// get currently active notebook page
	XmiMsimGuiSourceAbstract *source = get_active_source(dialog);
	xmi_msim_gui_source_abstract_generate(source);

}

static void scale_toggled_cb(GtkToggleButton *linearW, XmiMsimGuiSourcesDialog *dialog) {

	update_plot(dialog, get_active_source(dialog));
}

static void xmi_msim_gui_sources_dialog_init(XmiMsimGuiSourcesDialog *dialog) {
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
	gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
	gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
 	gtk_window_set_default_size(GTK_WINDOW(dialog), 900, -1);
	gtk_window_set_title(GTK_WINDOW(dialog), "X-ray sources");
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);

	GtkWidget *contentArea = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *mainHBox = gtk_hbox_new(FALSE, 5);
 	gtk_container_set_border_width(GTK_CONTAINER(mainHBox),5);
	gtk_container_add(GTK_CONTAINER(contentArea), mainHBox);

	// mainHBox will contain two columns -> one with source parameters (mainVBox), one with plot
	// 
	GtkWidget *mainVBox = gtk_vbox_new(FALSE, 5);
#if GTK_MAJOR_VERSION == 3
	gtk_box_pack_start(GTK_BOX(mainHBox), mainVBox, FALSE, FALSE, 2);
#else
	gtk_box_pack_start(GTK_BOX(mainHBox), mainVBox, TRUE, FALSE, 2);
#endif
	dialog->notebookW = gtk_notebook_new();
	gtk_box_pack_start(GTK_BOX(mainVBox), dialog->notebookW, TRUE, TRUE, 2);

	// add separator
	gtk_box_pack_start(GTK_BOX(mainVBox), gtk_hseparator_new(), FALSE, FALSE, 3);

	// add y-axis mode radiobuttons
	dialog->linearW= gtk_radio_button_new_with_label_from_widget(NULL,"Linear scaling");
	dialog->log10W = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(dialog->linearW), "Logarithmic scaling");
	gtk_box_pack_start(GTK_BOX(mainVBox), dialog->linearW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainVBox), dialog->log10W, FALSE, FALSE, 3);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(dialog->log10W), TRUE);
	
	// add separator
	gtk_box_pack_start(GTK_BOX(mainVBox), gtk_hseparator_new(), FALSE, FALSE, 3);

	// add buttons
	dialog->generateButton = gtk_button_new_from_stock(GTK_STOCK_EXECUTE);
	GtkWidget *infoButton = gtk_button_new_from_stock(GTK_STOCK_ABOUT);
	GtkWidget *exportButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	GtkWidget *imageButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	xmi_msim_gui_utils_update_button_text(dialog->generateButton, "Update spectrum");
	xmi_msim_gui_utils_update_button_text(exportButton, "Export spectrum");
	xmi_msim_gui_utils_update_button_text(imageButton, "Save image");

	GtkWidget *tempHBox = gtk_hbox_new(TRUE, 5);
	gtk_box_pack_start(GTK_BOX(tempHBox), dialog->generateButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(tempHBox), infoButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(mainVBox), tempHBox, FALSE, FALSE, 3);
	tempHBox = gtk_hbox_new(TRUE, 5);
	gtk_box_pack_start(GTK_BOX(tempHBox), exportButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(tempHBox), imageButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(mainVBox), tempHBox, FALSE, FALSE, 3);

	// now the plot canvas
#if GTK_MAJOR_VERSION == 3
	dialog->canvas = Gtk::manage(new Gtk::PLplot::Canvas());
	dialog->canvas->set_hexpand(true);
	dialog->canvas->set_vexpand(true);
	GtkWidget *aspect_frame = gtk_aspect_frame_new("", 0.5, 0.5, 842.0/595.0, FALSE);
	gtk_widget_set_hexpand(aspect_frame, TRUE);
	gtk_widget_set_vexpand(aspect_frame, TRUE);
	gtk_container_add(GTK_CONTAINER(aspect_frame), GTK_WIDGET(dialog->canvas->gobj()));
	gtk_box_pack_start(GTK_BOX(mainHBox), aspect_frame, TRUE, TRUE, 2);
#else
	dialog->canvas = gtk_plot_canvas_new(GTK_PLOT_A4_H, GTK_PLOT_A4_W, 0.9);
	GTK_PLOT_CANVAS_UNSET_FLAGS(GTK_PLOT_CANVAS(dialog->canvas), (GtkPlotCanvasFlags) (GTK_PLOT_CANVAS_CAN_SELECT | GTK_PLOT_CANVAS_CAN_SELECT_ITEM)); //probably needs to be unset when initializing, but set when data is available
	gtk_plot_canvas_set_background(GTK_PLOT_CANVAS(dialog->canvas),&white_plot);
	gtk_box_pack_start(GTK_BOX(mainHBox), dialog->canvas,FALSE,FALSE,2);
#endif
	


	// finish off with the signal handlers
	//g_signal_connect(G_OBJECT(exportButton), "clicked", G_CALLBACK(export_button_clicked_cb), (gpointer) source);
	g_signal_connect_swapped(G_OBJECT(infoButton), "clicked", G_CALLBACK(info_button_clicked_cb), (gpointer) dialog);
	g_signal_connect_swapped(G_OBJECT(dialog->generateButton), "clicked", G_CALLBACK(generate_button_clicked_cb), (gpointer) dialog);
	//g_signal_connect(G_OBJECT(imageButton), "clicked", G_CALLBACK(image_button_clicked_cb), (gpointer) source);
	g_signal_connect(G_OBJECT(dialog->linearW), "toggled", G_CALLBACK(scale_toggled_cb), (gpointer) dialog);
	gtk_widget_show_all(mainHBox);
}

static void update_plot(XmiMsimGuiSourcesDialog *dialog, XmiMsimGuiSourceAbstract *source) {
	// start by removing the current plot
#ifdef HAVE_CXX
	try {
		dialog->canvas->remove_plot(0);
	}
	catch (Gtk::PLplot::Exception &e) {
		//this will fail if there's no plot available
	}
#else
	GtkPlotCanvasChild *child;

	GList *list;
	list = GTK_PLOT_CANVAS(dialog->canvas)->childs;
	while (list) {
		child = GTK_PLOT_CANVAS_CHILD(list->data);
		gtk_plot_canvas_remove_child(GTK_PLOT_CANVAS(dialog->canvas), child);
		list = GTK_PLOT_CANVAS(dialog->canvas)->childs;
	}
#endif

	GArray *x, *y;
	xmi_msim_gui_source_abstract_get_plot_data(source, &x, &y);

	if (x == NULL || y == NULL) {
		g_warning("update_plot: xmi_msim_gui_source_abstract_get_plot_data returned no data!");
		return;
	}

	// add plot
#ifdef HAVE_CXX
	dialog->plot_window = Gtk::manage(new Plot2DSources("Energy (keV)", "Intensity (photons/s)"));
	dialog->plot_window->hide();
	dialog->plot_window->hide_legend();
	dialog->plot_window->set_box_style(Gtk::PLplot::BoxStyle::BOX_TICKS_TICK_LABELS_MAIN_AXES_MAJOR_TICK_GRID);
	dialog->canvas->add_plot(*(dialog->plot_window));
#else
	dialog->plot_window = gtk_plot_new_with_size(NULL,.65,.45);
	gtk_plot_set_background(GTK_PLOT(dialog->plot_window), &white_plot);
	gtk_plot_hide_legends(GTK_PLOT(dialog->plot_window));
#endif

	// determine extents of the plot
	double plot_xmin = 0.0;
	double plot_xmax = g_array_index(x, double, x->len-1);
	double plot_ymin = xmi_minval_double((double *) y->data, y->len);
	double plot_ymax = xmi_maxval_double((double *) y->data, y->len) * 1.2;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(dialog->linearW))) {
#ifdef HAVE_CXX
		dialog->plot_window->set_axis_logarithmic_y(false);
#else
		gtk_plot_set_ticks(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_Y, xmi_msim_gui_utils_get_tickstep(plot_ymin, plot_ymax), 5);
		gtk_plot_set_yscale(GTK_PLOT(dialog->plot_window), GTK_PLOT_SCALE_LINEAR);
#endif
	}
	else {
#ifdef HAVE_CXX
		dialog->plot_window->set_axis_logarithmic_y(true);
#else
		gtk_plot_set_yscale(GTK_PLOT(dialog->plot_window), GTK_PLOT_SCALE_LOG10);
#endif
	}

#ifdef HAVE_CXX
	dialog->plot_window->signal_double_press().connect([dialog, plot_xmin, plot_xmax, plot_ymin, plot_ymax](double x, double y){
		dialog->plot_window->set_region(plot_xmin, plot_xmax,
						plot_ymin, plot_ymax);
	});
	dialog->plot_window->show();
#else
	gtk_plot_set_ticks(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_X, xmi_msim_gui_utils_get_tickstep(plot_xmin, plot_xmax), 5);
	gtk_plot_set_range(GTK_PLOT(dialog->plot_window), plot_xmin, plot_xmax, plot_ymin, plot_ymax);
	gtk_plot_clip_data(GTK_PLOT(dialog->plot_window), TRUE);
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_TOP));
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_RIGHT));
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_LEFT), "Intensity (photons/s)");
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_BOTTOM), "Energy (keV)");
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_LEFT), "Helvetica", SOURCE_PLOT_TITLE, 90, NULL, NULL, TRUE, GTK_JUSTIFY_CENTER);
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_BOTTOM), "Helvetica", SOURCE_PLOT_TITLE, 0, NULL, NULL, TRUE, GTK_JUSTIFY_CENTER);
	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_BOTTOM), GTK_PLOT_LABEL_FLOAT,0);
        gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_TOP), GTK_PLOT_LABEL_FLOAT,0);

	if (plot_ymax < 10000.0 && plot_ymin >= 10.0) {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_LEFT), GTK_PLOT_LABEL_FLOAT, 0);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_FLOAT, 0);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_LEFT), "Helvetica", RESULTS_PLOT_LABELS_LR_FLOAT, 0, NULL, NULL, TRUE, GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_RIGHT), "Helvetica", RESULTS_PLOT_LABELS_LR_FLOAT, 0, NULL, NULL, TRUE, GTK_JUSTIFY_LEFT);
	}
	else {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_LEFT), GTK_PLOT_LABEL_EXP, 1);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_RIGHT), GTK_PLOT_LABEL_EXP, 1);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_LEFT), "Helvetica", SOURCE_PLOT_LABELS_LR_EXP, 0, NULL, NULL, TRUE, GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_RIGHT), "Helvetica", SOURCE_PLOT_LABELS_LR_EXP, 0, NULL, NULL, TRUE, GTK_JUSTIFY_LEFT);
	}

	gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_BOTTOM), "Helvetica", SOURCE_PLOT_LABELS_TP, 0, NULL, NULL, TRUE, GTK_JUSTIFY_CENTER);
	gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_TOP), "Helvetica", SOURCE_PLOT_LABELS_TP, 0, NULL, NULL, TRUE, GTK_JUSTIFY_CENTER);
	gtk_plot_axis_show_labels(gtk_plot_get_axis(GTK_PLOT(dialog->plot_window), GTK_PLOT_AXIS_TOP), 0);
        gtk_plot_grids_set_visible(GTK_PLOT(dialog->plot_window), TRUE, FALSE, TRUE, FALSE);
	child = gtk_plot_canvas_plot_new(GTK_PLOT(dialog->plot_window));
        gtk_plot_canvas_put_child(GTK_PLOT_CANVAS(dialog->canvas), child, .15, .05, .90, .85);
        gtk_widget_show(dialog->plot_window);
#endif
	// add data to plot_window
#ifdef HAVE_CXX
	std::vector<double> x_vals((double *) x->data, (double *) x->data + x->len);
	std::vector<double> y_vals((double *) y->data, (double *) y->data + y->len);

	Gtk::PLplot::PlotData2D *dataset= Gtk::manage(
		new Gtk::PLplot::PlotData2D(
			x_vals,
			y_vals
		)
	);
	dataset->set_color(*blue_plot);
	dataset->set_line_width(2.0);
	dataset->show();
	dialog->plot_window->add_data(*dataset);
	dialog->plot_window->set_region(plot_xmin, plot_xmax,
				plot_ymin, plot_ymax);
#else
	GtkPlotData *dataset;
	dataset = GTK_PLOT_DATA(gtk_plot_data_new());
	gtk_plot_add_data(GTK_PLOT(dialog->plot_window), dataset);
	gtk_plot_data_set_numpoints(dataset, x->len);
	gtk_plot_data_set_x(dataset, (double *) x->data);
	gtk_plot_data_set_y(dataset, (double *) y->data);
	gtk_widget_show(GTK_WIDGET(dataset));
	gtk_plot_data_set_line_attributes(dataset, (GtkPlotLineStyle) GTK_PLOT_LINE_SOLID, (GdkCapStyle) 0, (GdkJoinStyle) 0, 1, &blue_plot);
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(dialog->canvas));
	gtk_widget_queue_draw(GTK_WIDGET(dialog->canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(dialog->canvas));
	gtk_plot_paint(GTK_PLOT(dialog->plot_window));
	gtk_plot_refresh(GTK_PLOT(dialog->plot_window),NULL);
#endif
}

static void switch_page_cb(GtkNotebook *notebook, XmiMsimGuiSourceAbstract *source, guint page_num, XmiMsimGuiSourcesDialog *dialog) {

	update_plot(dialog, source);
}

static void after_generate_cb(XmiMsimGuiSourceAbstract *source, GError *error, XmiMsimGuiSourcesDialog *dialog) {
	g_debug("Source %s after-generate called\n", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	if (error != NULL) {
		g_warning("Error message: %s\n", error->message);
		// spawn error dialog with message
		GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error generating %s spectrum", xmi_msim_gui_source_abstract_get_name(source));
		gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "%s", error->message);

		gtk_dialog_run(GTK_DIALOG(info_dialog));
		gtk_widget_destroy(info_dialog);

		g_error_free(error);
	}
	else if (source == get_active_source(dialog)) {
		// update plot when this is the currently shown source
		g_debug("Updating source %s\n", g_type_name(G_TYPE_FROM_INSTANCE(source)));
		update_plot(dialog, source);
	}
	gtk_widget_set_sensitive(dialog->generateButton, TRUE);
}

GtkWidget *xmi_msim_gui_sources_dialog_new(GtkWindow *parent, struct xmi_input *current) {
	XmiMsimGuiSourcesDialog *widget;

	g_return_val_if_fail(parent == NULL || GTK_IS_WINDOW(parent), NULL);

	guint ntypes;
	GType *source_types = g_type_children(XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT, &ntypes);

	g_return_val_if_fail(ntypes > 0, NULL);

	widget = XMI_MSIM_GUI_SOURCES_DIALOG(g_object_new(XMI_MSIM_GUI_TYPE_SOURCES_DIALOG,
                                   NULL));

	gtk_window_set_transient_for(GTK_WINDOW(widget), GTK_WINDOW(parent));

	guint i;
	for (i = 0 ; i < ntypes ; i++) {
		GtkWidget *source = GTK_WIDGET(g_object_new(source_types[i], 
			"xmi-input-current",
			current,
			NULL));
			
		GtkWidget *label = gtk_label_new(xmi_msim_gui_source_abstract_get_name(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)));
		gtk_notebook_append_page(GTK_NOTEBOOK(widget->notebookW), source, label);
		// call generate on all sources
		xmi_msim_gui_source_abstract_generate(XMI_MSIM_GUI_SOURCE_ABSTRACT(source));

		g_signal_connect(G_OBJECT(source), "after-generate", G_CALLBACK(after_generate_cb), widget);
	}

	update_plot(widget, XMI_MSIM_GUI_SOURCE_ABSTRACT(gtk_notebook_get_nth_page(GTK_NOTEBOOK(widget->notebookW), 0)));
	g_signal_connect(G_OBJECT(widget->notebookW), "switch-page", G_CALLBACK(switch_page_cb), widget);
	gtk_widget_show_all(widget->notebookW);

	return GTK_WIDGET(widget);
}
