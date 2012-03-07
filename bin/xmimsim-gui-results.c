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
#include <string.h>
#include <math.h>
#include <xraylib.h>
#if GTKEXTRA_CHECK_VERSION(3,0,0)
//pdfs is just for version 3 :-(
#include <cairo-pdf.h>
#include <cairo-ps.h>
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
struct xmi_fluorescence_line_counts *results_brute_force_history;
int results_nbrute_force_history;
struct xmi_fluorescence_line_counts *results_var_red_history;
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
GtkWidget *magnifierW;

GtkWidget *spectra_propertiesW;
GtkWidget *spectra_properties_linestyleW;
GtkWidget *spectra_properties_widthW;
GtkWidget *spectra_properties_colorW;
GtkPlotData *spectra_properties_dataset_active;

gulong spectra_properties_linestyleG;
gulong spectra_properties_widthG;
gulong spectra_properties_colorG;

#if GTKEXTRA_CHECK_VERSION(3,0,0)
GtkWidget *print_button;
GtkPrintSettings *print_settings;
GtkPageSetup *page_setup;
#endif
GtkWidget *export_button;
GtkWidget *settings_button;

GtkTreeStore *countsTS;


struct spectra_data {
	GtkPlotData *dataSet;
	GtkWidget *checkButton;
	GtkWidget *button;
};

enum {
	ELEMENT_COLUMN,
	LINE_COLUMN,
	SHOW_LINE_COLUMN,
	INTERACTION_COLUMN,
	COUNTS_COLUMN,
	N_COLUMNS
};


void init_spectra_properties(GtkWidget *parent);
gchar *get_style_font(GtkWidget *widget);

static void spectra_region_changed_cb(GtkPlotCanvas *widget, gdouble x1, gdouble y1, gdouble x2, gdouble y2, gpointer data) {

  	gdouble xmin, ymin, xmax, ymax;
  	gint px1, px2, py1, py2;

	xmin = MIN(x1, x2);
	ymin = MIN(y1, y2);
	xmax = MAX(x1, x2);
	ymax = MAX(y1, y2);


	fprintf(stdout,"Entering spectra_region_changed_cb\n");
	fprintf(stdout,"x1: %lf\n",x1);
	fprintf(stdout,"y1: %lf\n",y1);
	fprintf(stdout,"x2: %lf\n",x2);
	fprintf(stdout,"y2: %lf\n",y2);

	gtk_plot_canvas_get_pixel(GTK_PLOT_CANVAS(canvas), xmin, ymin, &px1, &py1);
	gtk_plot_canvas_get_pixel(GTK_PLOT_CANVAS(canvas), xmax, ymax, &px2, &py2);
	
	//minimum size of box should be 15 pixels square
	if (abs(px1-px2) < 15 || abs(py1-py2) < 15)
		return;

        gtk_plot_get_point(GTK_PLOT(plot_window), px1, py1, &xmin, &ymax);
	gtk_plot_get_point(GTK_PLOT(plot_window), px2, py2, &xmax, &ymin);


	//make sure to check that these functions return the same results in gtk+extra 2.1.2
	fprintf(stdout,"Transformed coordinates\n");
	fprintf(stdout,"xmin: %lf\n",xmin);
	fprintf(stdout,"xmax: %lf\n",xmax);
	fprintf(stdout,"ymin: %lf\n",ymin);
	fprintf(stdout,"ymax: %lf\n",ymax);

	gtk_plot_set_range(GTK_PLOT(plot_window), xmin,xmax,ymin,ymax);
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));




	return;
}

gboolean spectra_region_double_clicked_cb(GtkWidget *widget, GdkEvent *event, gpointer data) {

	fprintf(stdout,"Entering spectra_region_double_clicked\n");
	if (event->type == GDK_2BUTTON_PRESS) {
		fprintf(stdout,"Double click registered\n");
	}
	

	gtk_plot_set_range(GTK_PLOT(plot_window),plot_xmin, plot_xmax, plot_ymin, plot_ymax);
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));

	return FALSE;
}


static void spectra_color_changed_cb(GtkColorButton *widget, gpointer user_data) {
	GtkPlotLineStyle line_style;
	GdkCapStyle cap_style;
	GdkJoinStyle join_style;
	gfloat width;
	GdkColor color;
	GdkColor color_new;

	if (spectra_properties_dataset_active == NULL)
		return;
	//update active dataset with new setting
	//get old one first
	gtk_plot_data_get_line_attributes(spectra_properties_dataset_active, &line_style, &cap_style, &join_style, &width, &color);


	//get new color
	gtk_color_button_get_color(widget, &color_new);

	gtk_plot_data_set_line_attributes(spectra_properties_dataset_active, line_style, cap_style, join_style, width, &color_new);


	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));

	return;
}

static void magnifier_changed_cb(GtkSpinButton *spinbutton, gpointer user_data) {
	gdouble magnifier;

	magnifier = gtk_spin_button_get_value(spinbutton);
	gtk_plot_canvas_set_magnification(GTK_PLOT_CANVAS(canvas),magnifier);

	gtk_widget_queue_resize(GTK_WIDGET(canvas));
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));

	return;
}

static void spectra_width_changed_cb(GtkSpinButton *spinbutton, gpointer user_data) {
	GtkPlotLineStyle line_style;
	GdkCapStyle cap_style;
	GdkJoinStyle join_style;
	gfloat width;
	GdkColor color;
	gfloat width_new;

	if (spectra_properties_dataset_active == NULL)
		return;

	//update active dataset with new setting
	//get old one first
	gtk_plot_data_get_line_attributes(spectra_properties_dataset_active, &line_style, &cap_style, &join_style, &width, &color);

	width_new = (gfloat) gtk_spin_button_get_value(spinbutton);


	gtk_plot_data_set_line_attributes(spectra_properties_dataset_active, line_style, cap_style, join_style, width_new, &color);


	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));

	return;
}


static void spectra_linestyle_changed_cb(GtkComboBox *widget, gpointer user_data) {
	GtkPlotLineStyle line_style;
	GdkCapStyle cap_style;
	GdkJoinStyle join_style;
	gfloat width;
	GdkColor color;
	GtkPlotLineStyle line_style_new;


	if (spectra_properties_dataset_active == NULL)
		return;
	//update active dataset with new setting
	//get old one first
	gtk_plot_data_get_line_attributes(spectra_properties_dataset_active, &line_style, &cap_style, &join_style, &width, &color);

	//check content from combo box
	switch (gtk_combo_box_get_active(GTK_COMBO_BOX(spectra_properties_linestyleW))) {
		case 0:
		line_style_new = GTK_PLOT_LINE_SOLID;
		break;
		case 1:
		line_style_new = GTK_PLOT_LINE_DOTTED;
		break;
		case 2:
		line_style_new = GTK_PLOT_LINE_DASHED;
		break;
		case 3:
		line_style_new = GTK_PLOT_LINE_DOT_DASH;
		break;
		case 4:
		line_style_new = GTK_PLOT_LINE_DOT_DOT_DASH;
		break;
		case 5:
		line_style_new = GTK_PLOT_LINE_DOT_DASH_DASH;
		break;
	}

	gtk_plot_data_set_line_attributes(spectra_properties_dataset_active, line_style_new, cap_style, join_style, width, &color);


	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));

	return;
}



static void settings_button_clicked_cb(GtkButton *button, gpointer data) {

	fprintf(stdout,"Entering settings_button_clicked_cb\n");

	return;
}

static void export_button_clicked_cb(GtkButton *button, gpointer data) {
	GtkWidget *dialog;
	GtkFileFilter *filter;
	gchar *filename;
#if GTKEXTRA_CHECK_VERSION(3,0,0)
	cairo_t *cairo;
	cairo_surface_t *surface;
#endif

	dialog = gtk_file_chooser_dialog_new("Export spectra", 
		GTK_WINDOW(data), GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL);

	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(dialog),g_path_get_dirname(results_input->general->outputfile));
	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.eps");
	gtk_file_filter_set_name(filter,"Encapsulated PostScript");
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
#if GTKEXTRA_CHECK_VERSION(3,0,0)
	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.pdf");
	gtk_file_filter_set_name(filter,"Adobe Portable Document Format");
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.png");
	gtk_file_filter_set_name(filter,"Portable Network Graphics");
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
#endif

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		//get selected filter
		filter = gtk_file_chooser_get_filter(GTK_FILE_CHOOSER(dialog));
#if GTKEXTRA_CHECK_VERSION(3,0,0)
		if (strcmp(gtk_file_filter_get_name(filter),"Encapsulated PostScript"  ) == 0) {
			fprintf(stdout,"EPS selected\n");
			if (strcmp(filename+strlen(filename)-4, ".eps") != 0) {
				filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+5));
				strcat(filename,".eps");
			}
			//surface = cairo_ps_surface_create(filename,595.0,842);
			surface = cairo_ps_surface_create(filename,842,595);
			/*cairo_ps_surface_dsc_begin_page_setup (surface);
			cairo_ps_surface_dsc_comment (surface, "%%PageOrientation: Landscape");*/
			cairo_ps_surface_set_eps(surface,1);
			cairo = cairo_create(surface);
			/*cairo_translate (cairo, 0, 842);
			cairo_rotate(cairo, -M_PI/2);*/
			
			gtk_plot_canvas_export_cairo(GTK_PLOT_CANVAS(canvas),cairo);
			gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
			cairo_show_page(cairo);
			cairo_surface_destroy(surface);
			cairo_destroy(cairo);

		}
		else if (strcmp(gtk_file_filter_get_name(filter),"Adobe Portable Document Format"  ) == 0) {
			fprintf(stdout,"PDF selected\n");
			if (strcmp(filename+strlen(filename)-4, ".pdf") != 0) {
				filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+5));
				strcat(filename,".pdf");
			}
			surface = cairo_pdf_surface_create(filename,842.0,595.0);
			cairo = cairo_create(surface);
			gtk_plot_canvas_export_cairo(GTK_PLOT_CANVAS(canvas),cairo);
			gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
			cairo_show_page(cairo);
			cairo_surface_destroy(surface);
			cairo_destroy(cairo);
		}
		else if (strcmp(gtk_file_filter_get_name(filter),"Portable Network Graphics"  ) == 0) {
			fprintf(stdout,"PNG selected\n");
			if (strcmp(filename+strlen(filename)-4, ".png") != 0) {
				filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+5));
				strcat(filename,".png");
			}
			surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 842, 595);
			cairo = cairo_create(surface);
			gtk_plot_canvas_export_cairo(GTK_PLOT_CANVAS(canvas),cairo);
			gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
			cairo_surface_write_to_png(surface,filename);
			cairo_surface_destroy(surface);
			cairo_destroy(cairo);
		}			
#else
		if (strcmp(gtk_file_filter_get_name(filter),"Encapsulated PostScript"  ) == 0) {
			fprintf(stdout,"EPS selected\n");
			if (strcmp(filename+strlen(filename)-4, ".eps") != 0) {
				filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+5));
				strcat(filename,".eps");
			}
			gtk_plot_export_ps(GTK_PLOT(plot_window),filename,GTK_PLOT_LANDSCAPE,TRUE,GTK_PLOT_A4);
		}
#endif
		g_free(filename);
		gtk_widget_destroy(dialog);
	}
	else
		gtk_widget_destroy(dialog);


	return;
}

#if GTKEXTRA_CHECK_VERSION(3,0,0)

static void draw_page(GtkPrintOperation *operation, GtkPrintContext *context, gint page_nr, gpointer data) {
	cairo_t *cairo;

	cairo = gtk_print_context_get_cairo_context(context);
	/*cairo_translate (cairo, 0, 842);
	cairo_rotate(cairo, -M_PI/2);
	cairo_translate (cairo, 30, 30);*/
	gtk_plot_canvas_export_cairo(GTK_PLOT_CANVAS(canvas),cairo);

	return;
}

static void print_button_clicked_cb(GtkButton *button, gpointer data) {
	GtkPrintOperation *operation;
	GError *error = NULL;
	GtkPrintOperationResult res;

	fprintf(stdout,"Entering print_button_clicked_cb\n");

	operation = gtk_print_operation_new();
	gtk_print_operation_set_print_settings(operation,print_settings);
	gtk_print_operation_set_default_page_setup(operation,page_setup);
	gtk_print_operation_set_show_progress(operation,TRUE);
	gtk_print_operation_set_track_print_status(operation, TRUE);
	g_signal_connect(G_OBJECT(operation), "draw-page", G_CALLBACK(draw_page),NULL);
	gtk_print_operation_set_n_pages(operation, 1);
	//gtk_print_operation_set_use_full_page(operation,TRUE);

	gtk_print_operation_run(operation, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG, GTK_WINDOW(data),&error);

	if (res == GTK_PRINT_OPERATION_RESULT_APPLY) {
		g_object_unref(print_settings);
		print_settings = g_object_ref(gtk_print_operation_get_print_settings(operation));
	}
	

	g_object_unref(operation);



	return;
}
#endif

static void spectrum_button_clicked_cb(GtkButton *button, gpointer data){
	struct spectra_data *sd = (struct spectra_data *) data;
	GtkPlotLineStyle line_style;
	GdkCapStyle cap_style;
	GdkJoinStyle join_style;
	gfloat width;
	GdkColor color;

	fprintf(stdout,"Entering spectrum_button_clicked_cb\n");

	//suspend signals
	g_signal_handler_block((gpointer) spectra_properties_widthW, spectra_properties_widthG);
	g_signal_handler_block((gpointer) spectra_properties_linestyleW, spectra_properties_linestyleG);
	g_signal_handler_block((gpointer) spectra_properties_colorW, spectra_properties_colorG);

	//
	spectra_properties_dataset_active = sd->dataSet;

	//set properties
	gtk_plot_data_get_line_attributes(spectra_properties_dataset_active, &line_style, &cap_style, &join_style, &width, &color);
	gtk_color_button_set_color(GTK_COLOR_BUTTON(spectra_properties_colorW), &color);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spectra_properties_widthW),width);
	switch (line_style) {
		case GTK_PLOT_LINE_NONE:
		break;
		case GTK_PLOT_LINE_SOLID:
		gtk_combo_box_set_active(GTK_COMBO_BOX(spectra_properties_linestyleW),0);
		break;
		case GTK_PLOT_LINE_DOTTED:
		gtk_combo_box_set_active(GTK_COMBO_BOX(spectra_properties_linestyleW),1);
		break;
		case GTK_PLOT_LINE_DASHED:
		gtk_combo_box_set_active(GTK_COMBO_BOX(spectra_properties_linestyleW),2);
		break;
		case GTK_PLOT_LINE_DOT_DASH:
		gtk_combo_box_set_active(GTK_COMBO_BOX(spectra_properties_linestyleW),3);
		break;
		case GTK_PLOT_LINE_DOT_DOT_DASH:
		gtk_combo_box_set_active(GTK_COMBO_BOX(spectra_properties_linestyleW),4);
		break;
		case GTK_PLOT_LINE_DOT_DASH_DASH:
		gtk_combo_box_set_active(GTK_COMBO_BOX(spectra_properties_linestyleW),5);
		break;
	}
	

	//unblock
	g_signal_handler_unblock((gpointer) spectra_properties_widthW, spectra_properties_widthG);
	g_signal_handler_unblock((gpointer) spectra_properties_linestyleW, spectra_properties_linestyleG);
	g_signal_handler_unblock((gpointer) spectra_properties_colorW, spectra_properties_colorG);


	gtk_dialog_run(GTK_DIALOG(spectra_propertiesW));
	gtk_widget_hide(spectra_propertiesW);

	return;
}

static void spectrum_button_toggled_cb(GtkToggleButton *toggleButton, gpointer data) {
	struct spectra_data *sd = (struct spectra_data *) data;
	

	fprintf(stdout,"Entering spectrum_button_toggled_cb\n");
	

	if (gtk_toggle_button_get_active(toggleButton) == TRUE) {
		gtk_widget_show(GTK_WIDGET(sd->dataSet));
		gtk_widget_set_sensitive(sd->button,TRUE);
	}
	else {
		gtk_widget_hide(GTK_WIDGET(sd->dataSet));
		gtk_widget_set_sensitive(sd->button,FALSE);
	}
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));

	return;

}

#define COLOR_INIT(color) gdk_color_parse(#color, &color ## _plot);\
			gdk_colormap_alloc_color(gdk_colormap_get_system(), &color ## _plot,FALSE,TRUE);	


GtkWidget *init_results(GtkWidget *window) {

	
	GtkWidget *superframe;
	GtkWidget *graphics_hbox;
	GtkWidget *scrolled_window;

	GtkWidget *spectra_box;//VBox
	GtkWidget *magnifier_hbox;
	gdouble magnifier;
	gchar *default_font;



	/*initialize colors*/
	COLOR_INIT(white);
	COLOR_INIT(blue);
	COLOR_INIT(red);
	COLOR_INIT(green);
	COLOR_INIT(black);
	COLOR_INIT(purple);
	COLOR_INIT(yellow);
	COLOR_INIT(pink);

	default_font = get_style_font(window);

	fprintf(stdout,"default font: %s\n",default_font);



	magnifier = 0.75;

	superframe = gtk_vbox_new(FALSE,2);
	graphics_hbox = gtk_hbox_new(FALSE,2); 
	canvas = gtk_plot_canvas_new(GTK_PLOT_A4_H, GTK_PLOT_A4_W,magnifier);
	g_signal_connect(G_OBJECT(canvas),"select-region",G_CALLBACK(spectra_region_changed_cb),NULL);
	g_signal_connect(G_OBJECT(canvas),"button-press-event",G_CALLBACK(spectra_region_double_clicked_cb),NULL);
	GTK_PLOT_CANVAS_UNSET_FLAGS(GTK_PLOT_CANVAS(canvas), GTK_PLOT_CANVAS_CAN_SELECT | GTK_PLOT_CANVAS_CAN_SELECT_ITEM); //probably needs to be unset when initializing, but set when data is available
	gtk_plot_canvas_set_background(GTK_PLOT_CANVAS(canvas),&white_plot);
	gtk_box_pack_start(GTK_BOX(graphics_hbox),canvas,FALSE,FALSE,2);

	spectra_box = gtk_vbox_new(FALSE,1);
	spectra_button_box = gtk_vbox_new(TRUE,3);
	gtk_box_pack_start(GTK_BOX(spectra_button_box),gtk_label_new("Please run a simulation\nor load an XMSO file"),TRUE,TRUE,0);
	gtk_box_pack_start(GTK_BOX(spectra_box),spectra_button_box,FALSE,FALSE,0);	


	magnifier_hbox = gtk_hbox_new(FALSE,2);
	gtk_box_pack_start(GTK_BOX(magnifier_hbox), gtk_label_new("Canvas magnification"),FALSE, FALSE,1);
	magnifierW = gtk_spin_button_new(GTK_ADJUSTMENT(gtk_adjustment_new(magnifier, 0.25,2.0,0.05,0.1,0.0)),0.05,2);
	gtk_box_pack_start(GTK_BOX(magnifier_hbox), magnifierW,FALSE, FALSE,1);
	g_signal_connect(G_OBJECT(magnifierW),"value-changed",G_CALLBACK(magnifier_changed_cb),NULL);



	settings_button = gtk_button_new_from_stock(GTK_STOCK_PROPERTIES);	
	g_signal_connect(G_OBJECT(settings_button),"clicked",G_CALLBACK(settings_button_clicked_cb),(gpointer)window);
	export_button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	g_signal_connect(G_OBJECT(export_button),"clicked",G_CALLBACK(export_button_clicked_cb),(gpointer)window);
#if GTKEXTRA_CHECK_VERSION(3,0,0)
	print_button = gtk_button_new_from_stock(GTK_STOCK_PRINT);
	g_signal_connect(G_OBJECT(print_button),"clicked",G_CALLBACK(print_button_clicked_cb),(gpointer)window);
#endif
	

	gtk_box_pack_end(GTK_BOX(spectra_box),magnifier_hbox, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(spectra_box),settings_button, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(spectra_box),export_button, FALSE, FALSE, 2);
#if GTKEXTRA_CHECK_VERSION(3,0,0)
	gtk_box_pack_end(GTK_BOX(spectra_box),print_button, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(print_button,FALSE);

	//print settings
	print_settings = gtk_print_settings_new();
	gtk_print_settings_set_orientation(print_settings,GTK_PAGE_ORIENTATION_LANDSCAPE);
	gtk_print_settings_set_paper_size(print_settings,gtk_paper_size_new(GTK_PAPER_NAME_A4));
	page_setup = gtk_page_setup_new();
	gtk_page_setup_set_orientation(page_setup,GTK_PAGE_ORIENTATION_LANDSCAPE);
	gtk_page_setup_set_paper_size_and_default_margins(page_setup,gtk_paper_size_new(GTK_PAPER_NAME_A4));

#endif
	gtk_widget_set_sensitive(export_button,FALSE);
	gtk_widget_set_sensitive(settings_button,FALSE);

	
	gtk_box_pack_end(GTK_BOX(graphics_hbox),spectra_box,TRUE,FALSE,2);
	gtk_widget_show_all(spectra_box);

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

	//countsTS etc
	countsTS = gtk_tree_store_new(N_COLUMNS,
					G_TYPE_STRING,
					G_TYPE_STRING,
					G_TYPE_BOOLEAN,
					G_TYPE_STRING,
					G_TYPE_DOUBLE
					);



	gtk_widget_show_all(superframe);
	
	//finalize widget
	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), superframe);


	init_spectra_properties(window);

	//gtk_widget_show_all(scrolled_window);

	return scrolled_window;
}

static void clear_container (GtkWidget *widget, gpointer data) {


	//gtk_widget_destroy(widget);
	gtk_container_remove(GTK_CONTAINER(data),widget);

}

int plot_spectra_from_file(char *xmsofile) {
	GList *buttonbox_children;
	int i,j,k;
	GtkWidget *checkButton;
	GtkWidget *button;
	GtkWidget *spectrum_hbox;
	struct spectra_data *sd;	
	char buffer[512];
	GList *list;
	double *temp_channels, *temp_energies;
	double temp_energy;
	GtkPlotCanvasChild *child;
	GtkPlotData *dataset;
	GtkTreeIter iter1, iter2, iter3;
	char *symbol;



	//free memory if necessary
	if (results_input != NULL)
		free(results_input);
	if (results_brute_force_history != NULL)
		xmi_free_fluorescence_line_counts(results_brute_force_history);
	if (results_var_red_history != NULL)
		xmi_free_fluorescence_line_counts(results_var_red_history);
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
	gtk_container_foreach(GTK_CONTAINER(spectra_button_box), clear_container, spectra_button_box);

	//clear tree
	gtk_tree_store_clear(countsTS);


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
        gtk_plot_grids_set_visible(GTK_PLOT(plot_window),TRUE,FALSE,TRUE,FALSE);
	child = gtk_plot_canvas_plot_new(GTK_PLOT(plot_window));
        gtk_plot_canvas_put_child(GTK_PLOT_CANVAS(canvas), child, .15,.05,.90,.85);
        gtk_widget_show(plot_window);
	GTK_PLOT_CANVAS_SET_FLAGS(GTK_PLOT_CANVAS(canvas), GTK_PLOT_CANVAS_CAN_SELECT );

	temp_energies = (double *) malloc(sizeof(double)*results_nchannels);
	for (i = 0 ; i < results_nchannels ; i++) {
		temp_energies[i] = results_input->detector->gain * (i+1) + results_input->detector->zero;
	}

	//fill it up again
	for (i = 0 ; i < results_ninteractions ; i++) {
		
		checkButton = gtk_check_button_new();	
		button = gtk_button_new_from_stock(GTK_STOCK_PROPERTIES);
		spectrum_hbox = gtk_hbox_new(FALSE,2);
		gtk_box_pack_start(GTK_BOX(spectrum_hbox),checkButton,FALSE,FALSE,1);
		gtk_box_pack_end(GTK_BOX(spectrum_hbox),button,FALSE,FALSE,1);
		gtk_box_pack_start(GTK_BOX(spectra_button_box),spectrum_hbox,FALSE,FALSE,1);
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
		gtk_button_set_label(GTK_BUTTON(checkButton),buffer);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkButton),TRUE);
		g_signal_connect(G_OBJECT(button), "clicked",G_CALLBACK(spectrum_button_clicked_cb),sd);
		g_signal_connect(G_OBJECT(checkButton), "toggled",G_CALLBACK(spectrum_button_toggled_cb),sd);
		gtk_widget_show(checkButton);
		gtk_widget_show(button);
		gtk_widget_show(spectrum_hbox);

	}	
	gtk_widget_show_all(spectra_button_box);
	gtk_widget_queue_draw(spectra_button_box);
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));
	gtk_plot_refresh(GTK_PLOT(plot_window),NULL);

#if GTKEXTRA_CHECK_VERSION(3,0,0)
	gtk_widget_set_sensitive(print_button,TRUE);
#endif
	gtk_widget_set_sensitive(export_button,TRUE);
	gtk_widget_set_sensitive(settings_button,TRUE);


	//treestore stuff
	if (results_brute_force_history != NULL) {
		//brute force mode	
		

	}
	else if (results_var_red_history != NULL) {
		//variance reduction mode
		
		for (i = 0 ; i < results_nvar_red_history ; i++) {
			//iterating over atomic numbers -> highest level
			gtk_tree_store_append(countsTS, &iter1, NULL);
			symbol = AtomicNumberToSymbol(results_var_red_history[i].atomic_number);
			gtk_tree_store_set(countsTS, &iter1, 
				ELEMENT_COLUMN, symbol,
				LINE_COLUMN , "all",
				SHOW_LINE_COLUMN, TRUE,
				INTERACTION_COLUMN, "all",
				COUNTS_COLUMN, results_var_red_history[i].total_counts,
				-1);
			xrlFree(symbol);
			for (j = 0 ; j < results_var_red_history[i].n_lines ; j++) {
				gtk_tree_store_append(countsTS, &iter2, &iter1);
				gtk_tree_store_set(countsTS, &iter2,
					LINE_COLUMN, results_var_red_history[i].lines[j].line_type,
					SHOW_LINE_COLUMN, TRUE,
					INTERACTION_COLUMN, "all",
					COUNTS_COLUMN, results_var_red_history[i].lines[j].total_counts,
					-1);
				for (k = 0 ; k < results_var_red_history[i].lines[j].n_interactions ; k++) {
					gtk_tree_store_append(countsTS, &iter3, &iter2);
					gtk_tree_store_set(countsTS, &iter3,
						INTERACTION_COLUMN, "all",
						COUNTS_COLUMN, results_var_red_history[i].lines[j].interactions[k],
						-1);
				}
			}
		}

		

	}
	else {
		fprintf(stderr,"XMSO file contains no history!\n");
		//pop up a dialog or so with a warning...
	}



	return 1;
}


void init_spectra_properties(GtkWidget *parent) {



	spectra_propertiesW = gtk_dialog_new_with_buttons(NULL, GTK_WINDOW(parent), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_CLOSE, GTK_RESPONSE_ACCEPT,NULL);
	spectra_properties_dataset_active = NULL;


	GtkWidget *vbox = gtk_dialog_get_content_area(GTK_DIALOG(spectra_propertiesW));
	/* properties that can be changed:
	 * 	LineStyle
	 * 	Width
	 * 	color
	 */


	//linestyle
	GtkWidget *hbox = gtk_hbox_new(FALSE,3);
#if GTK_CHECK_VERSION(2,24,0)
	spectra_properties_linestyleW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Solid");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Dotted");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Dashed");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Dot - Dash");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Dot - Dot - Dash");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Dot - Dash - Dash");
#else
	spectra_properties_linestyleW = gtk_combo_box_new_text();
	gtk_combo_box_append_text(GTK_COMBO_BOX(spectra_properties_linestyleW),"Solid");
	gtk_combo_box_append_text(GTK_COMBO_BOX(spectra_properties_linestyleW),"Dotted");
	gtk_combo_box_append_text(GTK_COMBO_BOX(spectra_properties_linestyleW),"Dashed");
	gtk_combo_box_append_text(GTK_COMBO_BOX(spectra_properties_linestyleW),"Dot - Dash");
	gtk_combo_box_append_text(GTK_COMBO_BOX(spectra_properties_linestyleW),"Dot - Dot - Dash");
	gtk_combo_box_append_text(GTK_COMBO_BOX(spectra_properties_linestyleW),"Dot - Dash - Dash");
#endif
	spectra_properties_linestyleG = g_signal_connect(G_OBJECT(spectra_properties_linestyleW),"changed",G_CALLBACK(spectra_linestyle_changed_cb),NULL);
	gtk_box_pack_start(GTK_BOX(hbox),gtk_label_new("Line style"),FALSE,FALSE,2);
	gtk_box_pack_start(GTK_BOX(hbox),spectra_properties_linestyleW, FALSE, FALSE,2);

	gtk_box_pack_start(GTK_BOX(vbox),hbox,FALSE,FALSE,2);

	//width
	hbox = gtk_hbox_new(FALSE,3);
	spectra_properties_widthW = gtk_spin_button_new(
		GTK_ADJUSTMENT(gtk_adjustment_new(1.0, 0.1, 10.0, 0.1,0.5,0.0)),
		0.1,1
	);
	gtk_box_pack_start(GTK_BOX(hbox),gtk_label_new("Line width"),FALSE,FALSE,2);
	gtk_box_pack_start(GTK_BOX(hbox),spectra_properties_widthW,FALSE,FALSE,2);
	spectra_properties_widthG = g_signal_connect(G_OBJECT(spectra_properties_widthW),"value-changed",G_CALLBACK(spectra_width_changed_cb),NULL);
	
	gtk_box_pack_start(GTK_BOX(vbox),hbox,FALSE,FALSE,2);

	//color
	hbox = gtk_hbox_new(FALSE,3);
	spectra_properties_colorW = gtk_color_button_new();
	gtk_color_button_set_use_alpha(GTK_COLOR_BUTTON(spectra_properties_colorW),FALSE);
	gtk_box_pack_start(GTK_BOX(hbox),gtk_label_new("Line color"),FALSE,FALSE,2);
	gtk_box_pack_start(GTK_BOX(hbox),spectra_properties_colorW,FALSE,FALSE,2);
	spectra_properties_colorG = g_signal_connect(G_OBJECT(spectra_properties_colorW),"color-set",G_CALLBACK(spectra_color_changed_cb),NULL);
	gtk_box_pack_start(GTK_BOX(vbox),hbox,FALSE,FALSE,2);

	gtk_widget_show_all(vbox);
}

gchar *get_style_font(GtkWidget *widget) {

	GtkStyle *style = gtk_widget_get_style(widget);
	PangoFontDescription *font_desc = style->font_desc;

	return g_strdup(pango_font_description_get_family(font_desc));
} 

/*
 *
 *
 * Functions related to the list containing all counts 
 * of all interactions of all lines of all elements
 *
 *
 */




