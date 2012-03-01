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


GtkWidget *spectra_button_box; //VButtonBox

struct spectra_data {
	GtkPlotData *dataset;
	GdkColor *color;
	gboolean shown;
	GtkWidget *checkButton;
	GtkWidget *colorButton;
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


#define COLOR_INIT(color) gdk_color_parse(#color, &color ## _plot);\
			gdk_colormap_alloc_color(gdk_colormap_get_system(), &color ## _plot,FALSE,TRUE);	


GtkWidget *init_results(GtkWidget *window) {

	
	GtkWidget *superframe;
	GtkWidget *graphics_hbox;
	GtkWidget *canvas;
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
	canvas = gtk_plot_canvas_new(GTK_PLOT_A4_H, GTK_PLOT_A4_W,1.);
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
	int i;
	GtkWidget *checkButton;
	GtkWidget *colorButton;
	struct spectra_data *sd;	
	char buffer[512];


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


	for (i = 0 ; i < results_ninteractions ; i++) {
		
		checkButton = gtk_check_button_new();	
		colorButton = gtk_color_button_new();
		sd = (struct spectra_data *) malloc(sizeof(struct spectra_data));
		sd->checkButton = checkButton;
		sd->colorButton = colorButton;
		switch (i) {
			case 0: 
				gtk_color_button_set_color(GTK_COLOR_BUTTON(colorButton),&blue_plot);
				sd->color = gdk_color_copy(&blue_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",0);
				}
				else {
					sprintf(buffer,"%i interaction",1);
				}
				break;
			case 1:
				gtk_color_button_set_color(GTK_COLOR_BUTTON(colorButton),&red_plot);
				sd->color = gdk_color_copy(&red_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interaction",1);
				}
				else {
					sprintf(buffer,"%i interactions",2);
				}
				break;
			case 2:
				gtk_color_button_set_color(GTK_COLOR_BUTTON(colorButton),&red_plot);
				sd->color = gdk_color_copy(&red_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interaction",2);
				}
				else {
					sprintf(buffer,"%i interactions",3);
				}
				break;
			case 3:
				gtk_color_button_set_color(GTK_COLOR_BUTTON(colorButton),&green_plot);
				sd->color = gdk_color_copy(&green_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",3);
				}
				else {
					sprintf(buffer,"%i interactions",4);
				}
				break;
			case 4:
				gtk_color_button_set_color(GTK_COLOR_BUTTON(colorButton),&purple_plot);
				sd->color = gdk_color_copy(&purple_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",4);
				}
				else {
					sprintf(buffer,"%i interactions",5);
				}
				break;
			case 5:
				gtk_color_button_set_color(GTK_COLOR_BUTTON(colorButton),&yellow_plot);
				sd->color = gdk_color_copy(&yellow_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",5);
				}
				else {
					sprintf(buffer,"%i interactions",6);
				}
				break;
			case 6:
				gtk_color_button_set_color(GTK_COLOR_BUTTON(colorButton),&pink_plot);
				sd->color = gdk_color_copy(&pink_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",6);
				}
				else {
					sprintf(buffer,"%i interactions",7);
				}
				break;
			default:
				gtk_color_button_set_color(GTK_COLOR_BUTTON(colorButton),&black_plot);
				sd->color = gdk_color_copy(&black_plot);
				if (results_use_zero_interactions == 1) {
					sprintf(buffer,"%i interactions",i);
				}
				else {
					sprintf(buffer,"%i interactions",i+1);
				}
				break;
		}

	}	


	return 1;
}
