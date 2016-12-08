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

#include "xmimsim-gui-sources-dialog.h"
#include "xmimsim-gui-utils.h"

#ifdef HAVE_CXX
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

static void xmi_msim_gui_sources_dialog_class_init(XmiMsimGuiSourcesDialogClass *klass) {

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
	gtk_box_pack_start(GTK_BOX(mainHBox), mainVBox, TRUE, FALSE, 2);
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
	GtkWidget *generateButton = gtk_button_new_from_stock(GTK_STOCK_EXECUTE);
	GtkWidget *infoButton = gtk_button_new_from_stock(GTK_STOCK_ABOUT);
	GtkWidget *exportButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	GtkWidget *imageButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	xmi_msim_gui_utils_update_button_text(generateButton, "Update spectrum");
	xmi_msim_gui_utils_update_button_text(exportButton, "Export spectrum");
	xmi_msim_gui_utils_update_button_text(imageButton, "Save image");

	GtkWidget *tempHBox = gtk_hbox_new(TRUE, 5);
	gtk_box_pack_start(GTK_BOX(tempHBox), generateButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(tempHBox), infoButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(mainVBox), tempHBox, FALSE, FALSE, 3);
	tempHBox = gtk_hbox_new(TRUE, 5);
	gtk_box_pack_start(GTK_BOX(tempHBox), exportButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(tempHBox), imageButton, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(mainVBox), tempHBox, FALSE, FALSE, 3);

	// now the plot canvas
#ifdef HAVE_CXX
	dialog->canvas = Gtk::manage(new Gtk::PLplot::Canvas());
	dialog->canvas->set_hexpand(true);
	dialog->canvas->set_vexpand(true);
	GtkWidget *aspect_frame = gtk_aspect_frame_new("", 0.5, 0.5, 842.0/595.0, FALSE);
	gtk_widget_set_hexpand(aspect_frame, TRUE);
	gtk_widget_set_vexpand(aspect_frame, TRUE);
	gtk_container_add(GTK_CONTAINER(aspect_frame), GTK_WIDGET(canvas->gobj()));
	gtk_box_pack_start(GTK_BOX(mainHBox), aspect_frame, TRUE, TRUE, 2);
#else
	dialog->canvas = gtk_plot_canvas_new(GTK_PLOT_A4_H, GTK_PLOT_A4_W, 0.9);
	GTK_PLOT_CANVAS_UNSET_FLAGS(GTK_PLOT_CANVAS(dialog->canvas), (GtkPlotCanvasFlags) (GTK_PLOT_CANVAS_CAN_SELECT | GTK_PLOT_CANVAS_CAN_SELECT_ITEM)); //probably needs to be unset when initializing, but set when data is available
	gtk_plot_canvas_set_background(GTK_PLOT_CANVAS(dialog->canvas),&white_plot);
	gtk_box_pack_start(GTK_BOX(mainHBox), dialog->canvas,FALSE,FALSE,2);
#endif
	


	// finish off with the signal handlers
	//g_signal_connect(G_OBJECT(exportButton), "clicked", G_CALLBACK(export_button_clicked_cb), (gpointer) source);
	//g_signal_connect(G_OBJECT(infoButton), "clicked", G_CALLBACK(info_button_clicked_cb), (gpointer) source);
	//g_signal_connect(G_OBJECT(generateButton), "clicked", G_CALLBACK(generate_button_clicked_cb), (gpointer) source);
	//g_signal_connect(G_OBJECT(imageButton), "clicked", G_CALLBACK(image_button_clicked_cb), (gpointer) source);
	// add signal handler for Y-axis scale
	gtk_widget_show_all(mainHBox);
}
