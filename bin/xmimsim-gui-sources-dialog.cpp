/*
Copyright (C) 2016-2017 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-sources-dialog.h"
#include "xmimsim-gui-source-abstract.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-colors.h"
#include "xmimsim-gui-export-canvas-dialog.h"
#include "xmi_aux.h"

#ifdef HAVE_GOOGLE_ANALYTICS
#include <xmimsim-gui-google-analytics.h>
#endif

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

G_DEFINE_TYPE(XmiMsimGuiSourcesDialog, xmi_msim_gui_sources_dialog, GTK_TYPE_DIALOG)

static void update_plot(XmiMsimGuiSourcesDialog *dialog, XmiMsimGuiSourceAbstract *source);

static void xmi_msim_gui_sources_dialog_class_init(XmiMsimGuiSourcesDialogClass *klass) {

}

static XmiMsimGuiSourceAbstract* get_active_source(XmiMsimGuiSourcesDialog *dialog) {
	gint current_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(dialog->notebookW));
	if (current_page < 0) {
		g_warning("get_active_source: no page is active!");
	}
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
	gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);

	// get currently active notebook page
	XmiMsimGuiSourceAbstract *source = get_active_source(dialog);
#ifdef HAVE_GOOGLE_ANALYTICS
	const XmiMsimGuiGoogleAnalyticsTracker *tracker = xmi_msim_gui_google_analytics_tracker_get_global();
	xmi_msim_gui_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "SOURCES-DIALOG-GENERATE", xmi_msim_gui_source_abstract_get_name(source), NULL);
#endif
	xmi_msim_gui_source_abstract_generate(source);

}

static void scale_toggled_cb(GtkToggleButton *linearW, XmiMsimGuiSourcesDialog *dialog) {

	update_plot(dialog, get_active_source(dialog));
}

static void save_button_clicked_cb(GtkButton *button, XmiMsimGuiSourcesDialog *dialog) {

	XmiMsimGuiSourceAbstract *source = get_active_source(dialog);

	XmiMsimGuiFileChooserDialog *save_dialog =
		xmi_msim_gui_file_chooser_dialog_new(
			"Save source parameters",
			GTK_WINDOW(dialog),
			GTK_FILE_CHOOSER_ACTION_SAVE,
			"_Save",
			"_Cancel"
		);
	xmi_msim_gui_file_chooser_dialog_set_modal(save_dialog, TRUE);
	gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(save_dialog), TRUE);

	if (xmi_msim_gui_file_chooser_dialog_run(save_dialog) == GTK_RESPONSE_ACCEPT) {
		gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(save_dialog));
		xmi_msim_gui_file_chooser_dialog_destroy(save_dialog);
		GError *error = NULL;
		if (!xmi_msim_gui_source_abstract_save_parameters(source, (const char *) filename, &error)) {
			GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error saving %s parameters to %s", xmi_msim_gui_source_abstract_get_name(source), filename);
			gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "%s", error->message);

			gtk_dialog_run(GTK_DIALOG(info_dialog));
			gtk_widget_destroy(info_dialog);

			g_error_free(error);
			g_free(filename);
		}
	}
	else {
		xmi_msim_gui_file_chooser_dialog_destroy(save_dialog);
	}
	return;
}

static void load_button_clicked_cb(GtkButton *button, XmiMsimGuiSourcesDialog *dialog) {

	XmiMsimGuiSourceAbstract *source = get_active_source(dialog);

	XmiMsimGuiFileChooserDialog *load_dialog =
		xmi_msim_gui_file_chooser_dialog_new(
			"Load source parameters",
			GTK_WINDOW(dialog),
			GTK_FILE_CHOOSER_ACTION_OPEN,
			"_Open",
			"_Cancel"
		);
	xmi_msim_gui_file_chooser_dialog_set_modal(load_dialog, TRUE);

	if (xmi_msim_gui_file_chooser_dialog_run(load_dialog) == GTK_RESPONSE_ACCEPT) {
		gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(load_dialog));
		xmi_msim_gui_file_chooser_dialog_destroy(load_dialog);
		GError *error = NULL;
		if (!xmi_msim_gui_source_abstract_load_parameters(source, (const char *) filename, &error)) {
			GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error loading %s parameters from %s", xmi_msim_gui_source_abstract_get_name(source), filename);
			gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "%s", error->message);

			gtk_dialog_run(GTK_DIALOG(info_dialog));
			gtk_widget_destroy(info_dialog);

			g_error_free(error);
			g_free(filename);
		}
	}
	else {
		xmi_msim_gui_file_chooser_dialog_destroy(load_dialog);
	}
	return;
}

static void export_button_clicked_cb(GtkButton *button, XmiMsimGuiSourcesDialog *dialog) {

	XmiMsimGuiSourceAbstract *source = get_active_source(dialog);

	XmiMsimGuiFileChooserDialog *export_dialog =
		xmi_msim_gui_file_chooser_dialog_new(
			"Export spectrum as ASCII file",
			GTK_WINDOW(dialog),
			GTK_FILE_CHOOSER_ACTION_SAVE,
			"_Save",
			"_Cancel"
		);
	xmi_msim_gui_file_chooser_dialog_set_modal(export_dialog, TRUE);
	gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(export_dialog), TRUE);

	if (xmi_msim_gui_file_chooser_dialog_run(export_dialog) == GTK_RESPONSE_ACCEPT) {
		gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(export_dialog));
		xmi_msim_gui_file_chooser_dialog_destroy(export_dialog);
		GError *error = NULL;
		if (!xmi_msim_gui_source_abstract_save(source, (const char *) filename, &error)) {
			GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(export_dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error exporting %s spectrum to %s", xmi_msim_gui_source_abstract_get_name(source), filename);
			gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "%s", error->message);

			gtk_dialog_run(GTK_DIALOG(info_dialog));
			gtk_widget_destroy(info_dialog);

			g_error_free(error);
			g_free(filename);
		}
	}
	else {
		xmi_msim_gui_file_chooser_dialog_destroy(export_dialog);
	}
	return;
}

static void image_button_clicked_cb(GtkButton *button, XmiMsimGuiSourcesDialog *dialog) {
	XmiMsimGuiFileChooserDialog *image_dialog = xmi_msim_gui_export_canvas_dialog_new(
		"Save spectrum as image",
		GTK_WINDOW(dialog)
		);

	if (xmi_msim_gui_file_chooser_dialog_run(image_dialog) == GTK_RESPONSE_ACCEPT) {
		GError *error = NULL;
		if (!xmi_msim_gui_export_canvas_dialog_save(image_dialog, dialog->canvas, &error)) {
			xmi_msim_gui_file_chooser_dialog_destroy(image_dialog);
			GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error saving %s spectrum", xmi_msim_gui_source_abstract_get_name(get_active_source(dialog)));
			gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "%s", error->message);

			gtk_dialog_run(GTK_DIALOG(info_dialog));
			gtk_widget_destroy(info_dialog);

			g_error_free(error);
			return;
		}
	}
	xmi_msim_gui_file_chooser_dialog_destroy(image_dialog);
	return;
}

static void xmi_msim_gui_sources_dialog_init(XmiMsimGuiSourcesDialog *dialog) {
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
	gtk_dialog_add_buttons(GTK_DIALOG(dialog), "_Ok", GTK_RESPONSE_ACCEPT, "_Cancel", GTK_RESPONSE_REJECT, NULL);
	gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
#if GTK_CHECK_VERSION(3, 22, 0) 
	GdkMonitor *monitor = gdk_display_get_primary_monitor(gdk_display_get_default());
	GdkRectangle workarea;
	gdk_monitor_get_workarea(monitor, &workarea);
	int screen_width = workarea.width;
#else
	int screen_width = gdk_screen_get_width(gdk_screen_get_default());
#endif
	gtk_window_set_default_size(GTK_WINDOW(dialog), MIN(1400, screen_width), -1);
	gtk_window_set_title(GTK_WINDOW(dialog), "X-ray sources");
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);

	GtkWidget *contentArea = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *mainHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(mainHBox), FALSE);
 	gtk_container_set_border_width(GTK_CONTAINER(mainHBox),5);
	gtk_container_add(GTK_CONTAINER(contentArea), mainHBox);

	// mainHBox will contain two columns -> one with source parameters (mainVBox), one with plot
	GtkWidget *mainVBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(mainVBox), FALSE);
	gtk_box_pack_start(GTK_BOX(mainHBox), mainVBox, FALSE, FALSE, 2);

	dialog->notebookW = gtk_notebook_new();
	gtk_box_pack_start(GTK_BOX(mainVBox), dialog->notebookW, TRUE, TRUE, 2);

	// add separator
	gtk_box_pack_start(GTK_BOX(mainVBox), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 3);

	// add y-axis mode radiobuttons
	dialog->linearW= gtk_radio_button_new_with_label_from_widget(NULL,"Linear scaling");
	dialog->log10W = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(dialog->linearW), "Logarithmic scaling");
	gtk_box_pack_start(GTK_BOX(mainVBox), dialog->linearW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainVBox), dialog->log10W, FALSE, FALSE, 3);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(dialog->log10W), TRUE);
	
	// add separator
	gtk_box_pack_start(GTK_BOX(mainVBox), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 3);

	// add buttons
	dialog->generateButton = gtk_button_new_with_label("Update spectrum");
	GtkWidget *infoButton = gtk_button_new_with_label("About source");
	GtkWidget *exportButton = gtk_button_new_with_label("Export spectrum");
	GtkWidget *imageButton = gtk_button_new_with_label("Save image");
	GtkWidget *loadButton = gtk_button_new_with_label("Load parameters");
	GtkWidget *saveButton = gtk_button_new_with_label("Save parameters");

	GtkWidget *tempHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(tempHBox), TRUE);
	gtk_box_pack_start(GTK_BOX(tempHBox), dialog->generateButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(tempHBox), infoButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(tempHBox), loadButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(mainVBox), tempHBox, FALSE, FALSE, 3);
	tempHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(tempHBox), TRUE);
	gtk_box_pack_start(GTK_BOX(tempHBox), exportButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(tempHBox), imageButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(tempHBox), saveButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(mainVBox), tempHBox, FALSE, FALSE, 3);

	// now the plot canvas
	dialog->canvas = Gtk::manage(new Gtk::PLplot::Canvas());
	dialog->canvas->set_hexpand(true);
	dialog->canvas->set_vexpand(true);
	GtkWidget *aspect_frame = gtk_aspect_frame_new("", 0.5, 0.5, 842.0/595.0, FALSE);
	gtk_widget_set_hexpand(aspect_frame, TRUE);
	gtk_widget_set_vexpand(aspect_frame, TRUE);
	gtk_container_add(GTK_CONTAINER(aspect_frame), GTK_WIDGET(dialog->canvas->gobj()));
	gtk_box_pack_start(GTK_BOX(mainHBox), aspect_frame, TRUE, TRUE, 2);

	// finish off with the signal handlers
	g_signal_connect(G_OBJECT(exportButton), "clicked", G_CALLBACK(export_button_clicked_cb), (gpointer) dialog);
	g_signal_connect_swapped(G_OBJECT(infoButton), "clicked", G_CALLBACK(info_button_clicked_cb), (gpointer) dialog);
	g_signal_connect_swapped(G_OBJECT(dialog->generateButton), "clicked", G_CALLBACK(generate_button_clicked_cb), (gpointer) dialog);
	g_signal_connect(G_OBJECT(imageButton), "clicked", G_CALLBACK(image_button_clicked_cb), (gpointer) dialog);
	g_signal_connect(G_OBJECT(loadButton), "clicked", G_CALLBACK(load_button_clicked_cb), (gpointer) dialog);
	g_signal_connect(G_OBJECT(saveButton), "clicked", G_CALLBACK(save_button_clicked_cb), (gpointer) dialog);
	g_signal_connect(G_OBJECT(dialog->linearW), "toggled", G_CALLBACK(scale_toggled_cb), (gpointer) dialog);
	gtk_widget_show_all(mainHBox);
}

static void clear_plot(XmiMsimGuiSourcesDialog *dialog) {
	// start by removing the current plot
	try {
		dialog->canvas->remove_plot(0);
	}
	catch (Gtk::PLplot::Exception &e) {
		//this will fail if there's no plot available
	}
}

static void update_plot(XmiMsimGuiSourcesDialog *dialog, XmiMsimGuiSourceAbstract *source) {

	clear_plot(dialog);

	GArray *x, *y;
	xmi_msim_gui_source_abstract_get_plot_data(source, &x, &y);

	if (x == NULL || y == NULL) {
		g_warning("update_plot: xmi_msim_gui_source_abstract_get_plot_data returned no data!");
		gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);
		return;
	}

	gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, TRUE);

	// add plot
	dialog->plot_window = Gtk::manage(new Plot2DSources("Energy (keV)", "Intensity (photons/s)"));
	dialog->plot_window->hide();
	dialog->plot_window->hide_legend();
	dialog->plot_window->set_box_style(Gtk::PLplot::BoxStyle::BOX_TICKS_TICK_LABELS_MAIN_AXES_MAJOR_TICK_GRID);
	dialog->canvas->add_plot(*(dialog->plot_window));

	// determine extents of the plot
	double plot_xmin = 0.0;
	double plot_xmax = g_array_index(x, double, x->len-1);
	double plot_ymin = xmi_minval_double((double *) y->data, y->len);
	double plot_ymax = xmi_maxval_double((double *) y->data, y->len) * 1.2;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(dialog->linearW))) {
		dialog->plot_window->set_axis_logarithmic_y(false);
	}
	else {
		dialog->plot_window->set_axis_logarithmic_y(true);
	}

	dialog->plot_window->signal_double_press().connect([dialog, plot_xmin, plot_xmax, plot_ymin, plot_ymax](double x, double y){
		dialog->plot_window->set_region(plot_xmin, plot_xmax,
						plot_ymin, plot_ymax);
	});
	dialog->plot_window->show();
	// add data to plot_window
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
}

static void switch_page_cb(GtkNotebook *notebook, XmiMsimGuiSourceAbstract *source, guint page_num, XmiMsimGuiSourcesDialog *dialog) {

	update_plot(dialog, source);
}

static void after_generate_cb(XmiMsimGuiSourceAbstract *source, GError *error, XmiMsimGuiSourcesDialog *dialog) {
	g_debug("Source %s after-generate called\n", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	if (error != NULL) {
		g_warning("after_generate_cb message: %s\n", error->message);
		// spawn error dialog with message
		GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error generating %s spectrum", xmi_msim_gui_source_abstract_get_name(source));
		gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "%s", error->message);

		gtk_dialog_run(GTK_DIALOG(info_dialog));
		gtk_widget_destroy(info_dialog);

		g_error_free(error);

		// set source data to NULL
		if (source->raw_data) {
			xmi_free_excitation(source->raw_data);
			source->raw_data = NULL;
		}
		if (source->x) {
			g_array_free(source->x, TRUE);
			source->x = NULL;
		}
		if (source->y) {
			g_array_free(source->y, TRUE);
			source->y = NULL;
		}

		clear_plot(dialog);
		gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);
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

struct xmi_excitation *xmi_msim_gui_sources_dialog_get_raw_data(XmiMsimGuiSourcesDialog *dialog) {
	struct xmi_excitation *rv = NULL;
	XmiMsimGuiSourceAbstract *source = get_active_source(dialog);
	unsigned int i;

	if (source == NULL || source->raw_data == NULL)
		return NULL;

	//xmi_copy_excitation(source->raw_data, &rv);
	// copy only those that make sense (intensity > 1)
	GArray *disc = g_array_sized_new(FALSE, FALSE, sizeof(struct xmi_energy_discrete), source->raw_data->n_discrete);

	for (i = 0 ; i < source->raw_data->n_discrete ; i++) {
		if (source->raw_data->discrete[i].horizontal_intensity +
		    source->raw_data->discrete[i].vertical_intensity >= 1.0)
			g_array_append_val(disc, source->raw_data->discrete[i]);
	}

	GArray *cont = g_array_sized_new(FALSE, FALSE, sizeof(struct xmi_energy_continuous), source->raw_data->n_continuous);

	guint skipped_zeroes = 0u;
	for (i = 0 ; i < source->raw_data->n_continuous ; i++) {
		if (source->raw_data->continuous[i].horizontal_intensity +
		    source->raw_data->continuous[i].vertical_intensity < 1.0) {
			if (++skipped_zeroes == 1 && i > 0) {
				struct xmi_energy_continuous temp = source->raw_data->continuous[i];
				temp.horizontal_intensity = temp.vertical_intensity = 0.0;
				g_array_append_val(cont, temp);
			}
		}
		else {
			if (skipped_zeroes >= 2) {
				struct xmi_energy_continuous temp = source->raw_data->continuous[i-1];
				temp.horizontal_intensity = temp.vertical_intensity = 0.0;
				g_array_append_val(cont, temp);
			}
			skipped_zeroes = 0;
			g_array_append_val(cont, source->raw_data->continuous[i]);
		}
	}

	rv = (struct xmi_excitation *) g_malloc(sizeof(struct xmi_excitation));
	rv->n_discrete = (int) disc->len;
	rv->discrete = (struct xmi_energy_discrete *) g_array_free(disc, FALSE); 
	rv->n_continuous = (int) cont->len;
	rv->continuous = (struct xmi_energy_continuous *) g_array_free(cont, FALSE); 

	return rv;
}
