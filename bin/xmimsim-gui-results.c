/*
Copyright (C) 2010-2012 Tom Schoonjans and Laszlo Vincze

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

#include "xmimsim-gui-results.h"
#include "xmi_aux.h"
#include <stdlib.h>
#if GTKEXTRA_CHECK_VERSION(3,0,0)
//pdfs is just for version 3 :-(
#include <cairo-pdf.h>
#endif

GdkColor white_plot;
GdkColor blue_plot;
GdkColor red_plot;
GdkColor green_plot;
GdkColor black_plot;
GdkColor purple_plot;
GdkColor yellow_plot;
GdkColor pink_plot;

struct xmi_input *results_input;
struct xmi_fluorescence_line *results_brute_force_history;
int results_nbrute_force_history;
struct xmi_fluorescence_line *results_var_red_history;
int results_nvar_red_history;
double **results_channels_conv;
double **results_channels_unconv;
int results_ninteractions;
int results_nchannels;
char *results_inputfile;
int results_use_zero_interactions;

double plot_xmin, plot_xmax, plot_ymin, plot_ymax;


GtkWidget *spectra_button_box; //VButtonBox
GtkWidget *canvas;
GtkWidget *plot_window;

struct spectra_data {
	GtkPlotData *dataSet;
	GtkWidget *checkButton;
	GtkWidget *button;
};



static void settings_button_clicked_cb(GtkButton *button, gpointer data) {

	fprintf(stdout,"Entering settings_button_clicked_cb\n");

	return;
}

static void export_button_clicked_cb(GtkButton *button, gpointer data) {

	fprintf(stdout,"Entering export_button_clicked_cb\n");

	return;
}

#if GTKEXTRA_CHECK_VERSION(3,0,0)
static void print_button_clicked_cb(GtkButton *button, gpointer data) {

	fprintf(stdout,"Entering print_button_clicked_cb\n");

	return;
}
#endif

static void spectrum_button_clicked_cb(GtkButton *button, gpointer data){
	fprintf(stdout,"Entering spectrum_button_clicked_cb\n");

	return;
}

static void spectrum_button_toggled_cb(GtkToggleButton *toggleButton, gpointer data) {
	fprintf(stdout,"Entering spectrum_button_toggled_cb\n");

	return;

}

#define COLOR_INIT(color) gdk_color_parse(#color, &color ## _plot);\
			gdk_colormap_alloc_color(gdk_colormap_get_system(), &color ## _plot,FALSE,TRUE);	


GtkWidget *init_results(GtkWidget *window) {

	
	GtkWidget *superframe;
	GtkWidget *graphics_hbox;
	GtkWidget *scrolled_window;

	GtkWidget *spectra_box;//VBox
#if GTKEXTRA_CHECK_VERSION(3,0,0)
	GtkWidget *print_button;
#endif
	GtkWidget *export_button;
	GtkWidget *settings_button;



	/*initialize colors*/
	COLOR_INIT(white);
	COLOR_INIT(blue);
	COLOR_INIT(red);
	COLOR_INIT(green);
	COLOR_INIT(black);
	COLOR_INIT(purple);
	COLOR_INIT(yellow);
	COLOR_INIT(pink);



	superframe = gtk_vbox_new(FALSE,2);
	graphics_hbox = gtk_hbox_new(FALSE,2); 
	canvas = gtk_plot_canvas_new(GTK_PLOT_A4_H, GTK_PLOT_A4_W,0.75);
	GTK_PLOT_CANVAS_UNSET_FLAGS(GTK_PLOT_CANVAS(canvas), GTK_PLOT_CANVAS_CAN_SELECT | GTK_PLOT_CANVAS_CAN_SELECT_ITEM); //probably needs to be unset when initializing, but set when data is available
	gtk_plot_canvas_set_background(GTK_PLOT_CANVAS(canvas),&white_plot);
	gtk_box_pack_start(GTK_BOX(graphics_hbox),canvas,FALSE,FALSE,2);

	spectra_box = gtk_vbox_new(FALSE,1);
	spectra_button_box = gtk_vbutton_box_new();
	gtk_box_set_spacing(GTK_BOX(spectra_button_box),3);
	gtk_box_set_homogeneous(GTK_BOX(spectra_button_box),TRUE);
	gtk_box_pack_start(GTK_BOX(spectra_box),spectra_button_box,TRUE,FALSE,0);	

	settings_button = gtk_button_new_from_stock(GTK_STOCK_PROPERTIES);	
	g_signal_connect(G_OBJECT(settings_button),"clicked",G_CALLBACK(settings_button_clicked_cb),(gpointer)window);
	export_button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	g_signal_connect(G_OBJECT(export_button),"clicked",G_CALLBACK(export_button_clicked_cb),(gpointer)window);
#if GTKEXTRA_CHECK_VERSION(3,0,0)
	print_button = gtk_button_new_from_stock(GTK_STOCK_PRINT);
	g_signal_connect(G_OBJECT(print_button),"clicked",G_CALLBACK(print_button_clicked_cb),(gpointer)window);
#endif
	


	gtk_box_pack_end(GTK_BOX(spectra_box),settings_button, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(spectra_box),export_button, FALSE, FALSE, 2);
#if GTKEXTRA_CHECK_VERSION(3,0,0)
	gtk_box_pack_end(GTK_BOX(spectra_box),print_button, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(print_button,FALSE);
#endif
	gtk_widget_set_sensitive(export_button,FALSE);
	gtk_widget_set_sensitive(settings_button,FALSE);

	
	gtk_box_pack_end(GTK_BOX(graphics_hbox),spectra_box,TRUE,FALSE,2);

	gtk_box_pack_start(GTK_BOX(superframe),graphics_hbox,FALSE,FALSE,2);
	

	//set current results variables to NULL
	results_input = NULL;
	results_brute_force_history = NULL;
	results_nbrute_force_history = 0;
	results_var_red_history = NULL;
	results_nvar_red_history = 0;
	results_channels_conv = NULL;
	results_channels_unconv = NULL;
	results_nchannels = 0;
	results_inputfile = NULL;
	results_use_zero_interactions = 0;

	plot_window = NULL;


	
	//finalize widget
	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), superframe);


	return scrolled_window;
}

static void clear_container (GtkWidget *widget, gpointer data) {


	gtk_widget_destroy(widget);

}

int plot_spectra_from_file(char *xmsofile) {
	GList *buttonbox_children;
	int i,j;
	GtkWidget *checkButton;
	GtkWidget *button;
	struct spectra_data *sd;	
	char buffer[512];
	GList *list;
	double *temp_channels, *temp_energies;
	double temp_energy;
	GtkPlotCanvasChild *child;
	GtkPlotData *dataset;


	//free memory if necessary
	if (results_input != NULL)
		free(results_input);
	if (results_brute_force_history != NULL)
		free(results_brute_force_history);
	if (results_var_red_history != NULL)
		free(results_var_red_history);
	if (results_channels_conv != NULL)
		free(results_channels_conv);
	if (results_channels_unconv != NULL)
		free(results_channels_unconv);
	if (results_inputfile != NULL)
		free(results_inputfile);



	if (xmi_read_output_xml(xmsofile,
		&results_input,
		&results_brute_force_history,
		&results_nbrute_force_history,
		&results_var_red_history,
		&results_nvar_red_history,
		&results_channels_conv,
		&results_channels_unconv,
		&results_nchannels,
		&results_ninteractions,
		&results_inputfile,
		&results_use_zero_interactions
		) == 0) {
		fprintf(stderr,"%s could not be read\n", xmsofile);
		/*
		 *
		 * launch dialog in case of error
		 *
		 */


		return 0;
	}

	//clear plotwindow if necessary
	//start with buttonbox	
	//clear it if necessary
	buttonbox_children = gtk_container_get_children(GTK_CONTAINER(spectra_button_box));
	gtk_container_foreach(GTK_CONTAINER(spectra_button_box), clear_container, NULL);

	list = GTK_PLOT_CANVAS(canvas)->childs;
	while (list) {
		fprintf(stdout,"clearing canvas child\n");
		child = GTK_PLOT_CANVAS_CHILD(list->data);
		gtk_plot_canvas_remove_child(GTK_PLOT_CANVAS(canvas), child);
		list = GTK_PLOT_CANVAS(canvas)->childs;
	}

	//add box with default settings
	plot_window = gtk_plot_new_with_size(NULL,.65,.45);
	gtk_plot_set_background(GTK_PLOT(plot_window),&white_plot);
	gtk_plot_hide_legends(GTK_PLOT(plot_window));

	//calculate maximum x and y value
	temp_channels = (double *) malloc(sizeof(double) * results_nchannels);
	for (i=0 ; i < results_nchannels ; i++) {
		temp_channels[i] = results_channels_conv[i][results_ninteractions-1];
	}
	free(temp_channels);
	plot_ymax = xmi_maxval_double(temp_channels,results_nchannels)*1.2;
	plot_ymin = 1.0;
	plot_xmin = 0.0;
	plot_xmax = results_nchannels * results_input->detector->gain + results_input->detector->zero;
	gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_X,1.0,5);
	gtk_plot_set_yscale(GTK_PLOT(plot_window), GTK_PLOT_SCALE_LOG10);
	gtk_plot_set_range(GTK_PLOT(plot_window),plot_xmin, plot_xmax, plot_ymin, plot_ymax);
	gtk_plot_clip_data(GTK_PLOT(plot_window), TRUE);
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP));
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT));
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Intensity (counts/channel)");
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Energy (keV)");
	//font will be a problem. Helvetica should be ok for Mac OS X, Arial for Windows, but Linux... pfft
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",20,90,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Helvetica",20,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,0);
        gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_FLOAT,0);
        gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),GTK_PLOT_LABEL_FLOAT,0);
        gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_FLOAT,0);
        gtk_plot_grids_set_visible(GTK_PLOT(plot_window),TRUE,TRUE,TRUE,TRUE);
	child = gtk_plot_canvas_plot_new(GTK_PLOT(plot_window));
        gtk_plot_canvas_put_child(GTK_PLOT_CANVAS(canvas), child, .15,.15,.80,.80);
        gtk_widget_show(plot_window);
	GTK_PLOT_CANVAS_SET_FLAGS(GTK_PLOT_CANVAS(canvas), GTK_PLOT_CANVAS_CAN_SELECT );

	temp_energies = (double *) malloc(sizeof(double)*results_nchannels);
	for (i = 0 ; i < results_nchannels ; i++) {
		temp_energies[i] = results_input->detector->gain * (i+1) + results_input->detector->zero;
	}

	//fill it up again
	for (i = 0 ; i < results_ninteractions ; i++) {
		
		checkButton = gtk_check_button_new();	
		button = gtk_button_new();
		gtk_container_add(GTK_CONTAINER(checkButton),button);
		gtk_widget_show(checkButton);
		gtk_widget_show(button);
		gtk_container_add(GTK_CONTAINER(spectra_button_box),checkButton);
		dataset = GTK_PLOT_DATA(gtk_plot_data_new());
		gtk_plot_add_data(GTK_PLOT(plot_window),dataset);
		gtk_plot_data_set_numpoints(dataset, results_nchannels);
		gtk_plot_data_set_x(dataset,temp_energies);
		temp_channels = (double *) malloc(sizeof(double)*results_nchannels);
		for (j = 0 ; j < results_nchannels ; j++)
			temp_channels[j]=results_channels_conv[j][i];

		gtk_plot_data_set_y(dataset,temp_channels);
		gtk_widget_show(GTK_WIDGET(dataset));

		sd = (struct spectra_data *) malloc(sizeof(struct spectra_data));
		sd->checkButton = checkButton;
		sd->button = button;
		sd->dataSet = dataset;
		g_signal_connect(G_OBJECT(button), "clicked",G_CALLBACK(spectrum_button_clicked_cb),sd);
		g_signal_connect(G_OBJECT(checkButton), "toggled",G_CALLBACK(spectrum_button_toggled_cb),sd);
		switch (i) {
			case 0: 
				gtk_plot_data_set_line_attributes(dataset,GTK_PLOT_LINE_SOLID,0,0,1,&blue_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",0);
				}
				else {
					sprintf(buffer,"%i interaction",1);
				}
				break;
			case 1:
				gtk_plot_data_set_line_attributes(dataset,GTK_PLOT_LINE_SOLID,0,0,1,&red_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interaction",1);
				}
				else {
					sprintf(buffer,"%i interactions",2);
				}
				break;
			case 2:
				gtk_plot_data_set_line_attributes(dataset,GTK_PLOT_LINE_SOLID,0,0,1,&green_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interaction",2);
				}
				else {
					sprintf(buffer,"%i interactions",3);
				}
				break;
			case 3:
				gtk_plot_data_set_line_attributes(dataset,GTK_PLOT_LINE_SOLID,0,0,1,&purple_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",3);
				}
				else {
					sprintf(buffer,"%i interactions",4);
				}
				break;
			case 4:
				gtk_plot_data_set_line_attributes(dataset,GTK_PLOT_LINE_SOLID,0,0,1,&yellow_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",4);
				}
				else {
					sprintf(buffer,"%i interactions",5);
				}
				break;
			case 5:
				gtk_plot_data_set_line_attributes(dataset,GTK_PLOT_LINE_SOLID,0,0,1,&pink_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",5);
				}
				else {
					sprintf(buffer,"%i interactions",6);
				}
				break;
			default:
				gtk_plot_data_set_line_attributes(dataset,GTK_PLOT_LINE_SOLID,0,0,1,&black_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",i);
				}
				else {
					sprintf(buffer,"%i interactions",i+1);
				}
				break;
		}
		gtk_button_set_label(GTK_BUTTON(button),buffer);


	}	
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));
	gtk_plot_refresh(GTK_PLOT(plot_window),NULL);


	return 1;
}
