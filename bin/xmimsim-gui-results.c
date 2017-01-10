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

#include <config.h>
#include "xmimsim-gui.h"
#include "xmimsim-gui-results.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-fonts.h"
#include "xmimsim-gui-export-canvas-dialog.h"
#include "xmi_aux.h"
#include <math.h>
#include <xraylib.h>
#include <cairo-pdf.h>
#include <cairo-ps.h>



static struct xmi_output *results;

static double plot_xmin, plot_xmax, plot_ymin, plot_ymax_conv, plot_ymax_unconv;

static GtkWidget *spectra_button_box; //VButtonBox
static gint canvas_height;

static GtkWidget *spectra_propertiesW;
static GtkWidget *spectra_properties_linestyleW;
static GtkWidget *spectra_properties_widthW;
static GtkWidget *spectra_properties_colorW;
#ifdef HAVE_CXX
static Gtk::PLplot::PlotData2D *spectra_properties_dataset_active;
#else
static GtkPlotData *spectra_properties_dataset_active;
#endif
static gulong spectra_properties_linestyleG;
static gulong spectra_properties_widthG;
static gulong spectra_properties_colorG;
#ifdef HAVE_CXX

#else
static gulong spectra_region_mouse_movedG;
static gulong spectra_region_changedG;
static gulong spectra_region_double_clickedG;
#endif


static gchar *xaxis_title = NULL;
static gchar *yaxis_title = NULL;
#ifdef HAVE_CXX
static bool current_scale;
#else
static GtkPlotScale current_scale;
#endif
static int current_conv;

enum {
	XMI_PLOT_CONVOLUTED,
	XMI_PLOT_UNCONVOLUTED
};

static GtkWidget *print_button;
static GtkPrintSettings *print_settings;
static GtkPageSetup *page_setup;
static GtkWidget *export_button;
static GtkWidget *settings_button;

static GtkTreeStore *countsTS;
static GtkWidget *countsTV;


struct spectra_data {
#ifdef HAVE_CXX
	Gtk::PLplot::PlotData2D *dataset_conv;
	Gtk::PLplot::PlotData2D *dataset_unconv;
#else
	GtkPlotData *dataset_conv;
	GtkPlotData *dataset_unconv;
#endif
	GtkWidget *checkButton;
	GtkWidget *button;
};

static struct spectra_data **sds = NULL;

GtkWidget *xCoordW;
GtkWidget *yCoordW;
GtkWidget *channelCoordW;

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


void init_spectra_properties(GtkWidget *parent);
gchar *get_style_font(GtkWidget *widget);
static void zoom_out(void);

#ifdef HAVE_CXX

class Plot2D : public Gtk::PLplot::Plot2D {
	public:
	Plot2D(
		const Glib::ustring &axis_title_x,
		const Glib::ustring &axis_title_y) :
		Gtk::PLplot::Plot2D(axis_title_x, axis_title_y) {}
	virtual void on_double_press(double x, double y) override {
		zoom_out();
	}

};

#endif

#ifdef HAVE_CXX
static Gtk::PLplot::Canvas *canvas;
static Plot2D *plot_window;
#else
static GtkWidget *canvas;
static GtkWidget *plot_window;
#endif

#ifndef HAVE_CXX
static gboolean resize_canvas_cb(GtkWidget *widget, GdkEvent *event, gpointer data) {
	gdouble magnifier;

	gint width = widget->allocation.height;
	//return if nothing changed, expose event calls occur with frame rate
	if (width == canvas_height)
		return FALSE;

	magnifier = (gdouble) width/((gdouble) GTK_PLOT_A4_W);

	gtk_plot_canvas_set_magnification(GTK_PLOT_CANVAS(canvas),magnifier);

	gtk_widget_queue_resize(GTK_WIDGET(canvas));
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));

	canvas_height = widget->allocation.height;

	return FALSE;
}
#endif

static void cell_print_double(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {

	gdouble value;
	gchar *double_text;
	gint depth;

	if (GPOINTER_TO_INT(data) == ENERGY_COLUMN) {
		depth = gtk_tree_store_iter_depth(countsTS,iter);
		if (depth != 1) {
			g_object_set(G_OBJECT(renderer), "visible",FALSE, NULL);
			return;
		}
		else
			g_object_set(G_OBJECT(renderer), "visible",TRUE, NULL);
	}


	gtk_tree_model_get(tree_model,iter, GPOINTER_TO_INT(data), &value,-1);

	double_text = g_strdup_printf("%lg",value);
	g_object_set(G_OBJECT(renderer), "text", double_text, NULL);

	g_free(double_text);

	return;
}

void cell_active_toggle(GtkCellRendererToggle *cell_renderer, gchar *path, gpointer user_data) {
	GtkTreeIter iter, parent, child;
	GtkTreePath *tree_path = gtk_tree_path_new_from_string(path);
	gboolean toggled, inconsistent, toggled2;
	gint depth;
#ifdef HAVE_CXX
	Gtk::PLplot::PlotData2D *plot_data;
	#define PLOT_SHOW plot_data->show()
	#define PLOT_HIDE plot_data->hide()
#else
	GtkPlotData *plot_data;
	#define PLOT_SHOW gtk_widget_show(GTK_WIDGET(plot_data))
	#define PLOT_HIDE gtk_widget_hide(GTK_WIDGET(plot_data))
#endif

	gtk_tree_model_get_iter(GTK_TREE_MODEL(countsTS), &iter, tree_path);
	depth = gtk_tree_store_iter_depth(countsTS,&iter);
	gtk_tree_model_get(GTK_TREE_MODEL(countsTS), &iter, SHOW_LINE_COLUMN, &toggled, -1);


	//if depth == 0, then check consistency first!!!
	if (depth == 0) {
		g_object_get(G_OBJECT(cell_renderer), "inconsistent", &inconsistent, NULL);
		if (inconsistent) {
			gtk_tree_store_set(countsTS, &iter, CONSISTENT_COLUMN, TRUE, SHOW_LINE_COLUMN, TRUE,-1);
			//set all children TRUE
			gtk_tree_model_iter_children(GTK_TREE_MODEL(countsTS), &child, &iter);
			do {
				gtk_tree_model_get(GTK_TREE_MODEL(countsTS), &child, CHILD_COLUMN, &plot_data, -1);
				PLOT_SHOW;
				gtk_tree_store_set(countsTS, &child, SHOW_LINE_COLUMN, TRUE, -1);
			} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(countsTS),&child));
		}
		else {
			if (toggled) {
				gtk_tree_store_set(countsTS, &iter, SHOW_LINE_COLUMN, FALSE, -1);
				//set all children to FALSE
				gtk_tree_model_iter_children(GTK_TREE_MODEL(countsTS), &child, &iter);
				do {
					gtk_tree_model_get(GTK_TREE_MODEL(countsTS), &child, CHILD_COLUMN, &plot_data, -1);
					PLOT_HIDE;
					gtk_tree_store_set(countsTS, &child, SHOW_LINE_COLUMN, FALSE, -1);
				} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(countsTS),&child));
			}
			else {
				gtk_tree_store_set(countsTS, &iter, SHOW_LINE_COLUMN, TRUE, -1);
				//set all children to TRUE
				gtk_tree_model_iter_children(GTK_TREE_MODEL(countsTS), &child, &iter);
				do {
					gtk_tree_model_get(GTK_TREE_MODEL(countsTS), &child, CHILD_COLUMN, &plot_data, -1);
					PLOT_SHOW;
					gtk_tree_store_set(countsTS, &child, SHOW_LINE_COLUMN, TRUE, -1);
				} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(countsTS),&child));
			}
		}


	}
	else {
		if (toggled == TRUE)
			toggled = FALSE;
		else
			toggled = TRUE;

		gtk_tree_store_set(countsTS, &iter, SHOW_LINE_COLUMN, toggled, -1);
		//set parent as inconsistent if necessary!!
		gtk_tree_model_iter_parent(GTK_TREE_MODEL(countsTS), &parent, &iter);
		//check all children
		gint n_children=gtk_tree_model_iter_n_children(GTK_TREE_MODEL(countsTS),&parent);
		gint toggle_sum=0;
		gtk_tree_model_iter_children(GTK_TREE_MODEL(countsTS), &child, &parent);
		do {
			gtk_tree_model_get(GTK_TREE_MODEL(countsTS), &child, SHOW_LINE_COLUMN, &toggled2, -1);
			toggle_sum += toggled2;
		} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(countsTS),&child));



		if (toggle_sum == 0) {
			gtk_tree_store_set(countsTS, &parent, CONSISTENT_COLUMN, TRUE, SHOW_LINE_COLUMN, FALSE, -1);
		}
		else if (toggle_sum == n_children) {
			gtk_tree_store_set(countsTS, &parent, CONSISTENT_COLUMN, TRUE, SHOW_LINE_COLUMN, TRUE, -1);
		}
		else
			gtk_tree_store_set(countsTS, &parent, CONSISTENT_COLUMN, FALSE, -1);

		//draw or remove line
		gtk_tree_model_get(GTK_TREE_MODEL(countsTS), &iter, CHILD_COLUMN, &plot_data, -1);
		if (toggled) {
			PLOT_SHOW;
		}
		else {
			PLOT_HIDE;
		}

	}

#ifndef HAVE_CXX
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
#endif

	gtk_tree_path_free(tree_path);

#undef PLOT_SHOW
#undef PLOT_HIDE

	return;
}

void cell_visible_toggle(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gint depth;
	gboolean show_line, consistent;

	depth = gtk_tree_store_iter_depth(countsTS, iter);

	if (depth == 2) {
		//set invisible
		g_object_set(G_OBJECT(renderer), "visible",FALSE, NULL);
		g_object_set(G_OBJECT(renderer), "activatable",FALSE, NULL);
	}
	else if (depth == 1) {
		g_object_set(G_OBJECT(renderer), "activatable",TRUE, NULL);
		g_object_set(G_OBJECT(renderer), "visible",TRUE, NULL);
		g_object_set(G_OBJECT(renderer), "inconsistent", FALSE, NULL);
		gtk_tree_model_get(tree_model,iter, SHOW_LINE_COLUMN, &show_line,-1);
		g_object_set(G_OBJECT(renderer), "active", show_line, NULL);

	}
	else {
		g_object_set(G_OBJECT(renderer), "activatable",TRUE, NULL);
		g_object_set(G_OBJECT(renderer), "visible",TRUE, NULL);
		gtk_tree_model_get(tree_model,iter, CONSISTENT_COLUMN, &consistent,-1);
		if (consistent) {
			gtk_tree_model_get(tree_model,iter, SHOW_LINE_COLUMN, &show_line,-1);
			g_object_set(G_OBJECT(renderer), "active", show_line, NULL);
			g_object_set(G_OBJECT(renderer), "inconsistent", FALSE, NULL);
		}
		else {
			g_object_set(G_OBJECT(renderer), "inconsistent", TRUE, NULL);
		}
	}

	return;
}

#ifndef HAVE_CXX
static void spectra_region_changed_cb(GtkPlotCanvas *widget, gdouble x1, gdouble y1, gdouble x2, gdouble y2, gpointer data) {

  	gdouble xmin, ymin, xmax, ymax;
  	gint px1, px2, py1, py2;

	xmin = MIN(x1, x2);
	ymin = MIN(y1, y2);
	xmax = MAX(x1, x2);
	ymax = MAX(y1, y2);



	gtk_plot_canvas_get_pixel(GTK_PLOT_CANVAS(canvas), xmin, ymin, &px1, &py1);
	gtk_plot_canvas_get_pixel(GTK_PLOT_CANVAS(canvas), xmax, ymax, &px2, &py2);

	//minimum size of box should be 15 pixels square
	if (abs(px1-px2) < 15 || abs(py1-py2) < 15)
		return;

        gtk_plot_get_point(GTK_PLOT(plot_window), px1, py1, &xmin, &ymax);
	gtk_plot_get_point(GTK_PLOT(plot_window), px2, py2, &xmax, &ymin);

	double plot_ymax = (current_conv == XMI_PLOT_CONVOLUTED) ? plot_ymax_conv : plot_ymax_unconv;

	xmin = MAX(xmin,plot_xmin);
	xmax = MIN(xmax,plot_xmax);
	ymin = MAX(ymin,plot_ymin);
	ymax = MIN(ymax,plot_ymax);

	if (xmax < plot_xmin)
		return;
	else if (xmin > plot_xmax)
		return;
	else if (ymax < plot_ymin)
		return;
	else if (ymin > plot_ymax)
		return;

	double tickstep = xmi_msim_gui_utils_get_tickstep(xmin, xmax);

	gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_X, tickstep,5);
	if (current_scale == GTK_PLOT_SCALE_LINEAR) {
		tickstep = xmi_msim_gui_utils_get_tickstep(ymin, ymax);
		gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_Y, tickstep,5);
	}
	if ((xmax - xmin) < 0.005) {
		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,4);
	}
	else if ((xmax - xmin) < 0.05) {
		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,3);
	}
	else if ((xmax - xmin) < 0.5) {
		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,2);
	}
	else if ((xmax - xmin) < 5) {
		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,1);
	}
	else {
		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,0);
	}

	if (ymax < 100000.0) {
       		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_FLOAT,0);
       		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_FLOAT,0);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",RESULTS_PLOT_LABELS_LR_FLOAT,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",RESULTS_PLOT_LABELS_LR_FLOAT,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}
	else {
       		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_EXP,1);
       		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_EXP,1);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",RESULTS_PLOT_LABELS_LR_EXP,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",RESULTS_PLOT_LABELS_LR_EXP,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}

	gtk_plot_set_range(GTK_PLOT(plot_window), xmin,xmax,ymin,ymax);
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));




	return;
}
#endif

static void zoom_out(void) {
	double plot_ymax = (current_conv == XMI_PLOT_CONVOLUTED) ? plot_ymax_conv : plot_ymax_unconv;
#ifdef HAVE_CXX
	plot_window->set_region(plot_xmin, plot_xmax, plot_ymin, plot_ymax);
#else
	double tickstep = xmi_msim_gui_utils_get_tickstep(plot_xmin, plot_xmax);
	gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_X, tickstep,5);
	if (current_scale == GTK_PLOT_SCALE_LINEAR) {
		tickstep = xmi_msim_gui_utils_get_tickstep(plot_ymin, plot_ymax);
		gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_Y, tickstep,5);
	}
	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,0);
	if (plot_ymax < 100000.0) {
       		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_FLOAT,0);
       		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_FLOAT,0);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",RESULTS_PLOT_LABELS_LR_FLOAT,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",RESULTS_PLOT_LABELS_LR_FLOAT,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}
	else {
       		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_EXP,1);
       		gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_EXP,1);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",RESULTS_PLOT_LABELS_LR_EXP,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",RESULTS_PLOT_LABELS_LR_EXP,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}
	gtk_plot_set_range(GTK_PLOT(plot_window),plot_xmin, plot_xmax, plot_ymin, plot_ymax);
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
#endif
}

#ifndef HAVE_CXX
static gboolean spectra_region_double_clicked_cb(GtkWidget *widget, GdkEvent *event, gpointer data) {

	if (event->type == GDK_2BUTTON_PRESS) {
		zoom_out();
	}



	return FALSE;
}
#endif

static void spectra_color_changed_cb(GtkColorButton *widget, gpointer user_data) {
	if (spectra_properties_dataset_active == NULL)
		return;
#ifdef HAVE_CXX
	GdkRGBA color;
	fprintf(stderr, "Before gtk_color_button_get_rgba\n");
	gtk_color_button_get_rgba(widget, &color);
	fprintf(stderr, "Before set_color\n");
	spectra_properties_dataset_active->set_color(Gdk::RGBA(&color, true));
	fprintf(stderr, "After set_color\n");
#else
	GtkPlotLineStyle line_style;
	GdkCapStyle cap_style;
	GdkJoinStyle join_style;
	gfloat width;
	GdkColor color;
	GdkColor color_new;

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
#endif
	return;
}

#ifndef HAVE_CXX
static gboolean spectra_region_mouse_moved_cb(GtkWidget *widget, GdkEvent *event, gpointer data) {

	gint x, y;
	gdouble px,py;
	gchar *buffer;

	gtk_widget_get_pointer(canvas, &x, &y);
	gtk_plot_get_point(GTK_PLOT(plot_window), x, y, &px, &py);
	gdouble xmin, xmax, ymin, ymax;

	//check if coordinates are within plot
	gtk_plot_get_xrange(GTK_PLOT(plot_window), &xmin, &xmax);
	gtk_plot_get_yrange(GTK_PLOT(plot_window), &ymin, &ymax);
	if (px < xmin || px > xmax)
		return FALSE;
	if (py < ymin || py > ymax)
		return FALSE;

	buffer = g_strdup_printf("%lg",px);
	gtk_entry_set_text(GTK_ENTRY(xCoordW), buffer);
	g_free(buffer);
	buffer = g_strdup_printf("%i",(int) ((px-results->input->detector->zero)/results->input->detector->gain));
	gtk_entry_set_text(GTK_ENTRY(channelCoordW), buffer);
	g_free(buffer);
	buffer = g_strdup_printf("%lg",py);
	gtk_entry_set_text(GTK_ENTRY(yCoordW), buffer);
	g_free(buffer);



	return FALSE;
}
#endif


static void spectra_width_changed_cb(GtkSpinButton *spinbutton, gpointer user_data) {
	if (spectra_properties_dataset_active == NULL)
		return;
#ifdef HAVE_CXX
	gdouble width_new;
	width_new = gtk_spin_button_get_value(spinbutton);
	spectra_properties_dataset_active->set_line_width(width_new);
#else
	GtkPlotLineStyle line_style;
	GdkCapStyle cap_style;
	GdkJoinStyle join_style;
	gfloat width;
	GdkColor color;
	gfloat width_new;

	//update active dataset with new setting
	//get old one first
	gtk_plot_data_get_line_attributes(spectra_properties_dataset_active, &line_style, &cap_style, &join_style, &width, &color);

	width_new = (gfloat) gtk_spin_button_get_value(spinbutton);


	gtk_plot_data_set_line_attributes(spectra_properties_dataset_active, line_style, cap_style, join_style, width_new, &color);


	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));
#endif
	return;
}

static void radio_conv_changed_cb(GtkToggleButton *widget, gpointer data) {
	int i;
	if (gtk_toggle_button_get_active(widget) == TRUE) {
		current_conv = XMI_PLOT_CONVOLUTED;
		for (i = (results->use_zero_interactions ? 0 : 1) ; i <= results->ninteractions ; i++) {
			if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(sds[i]->checkButton)) == TRUE) {
#ifdef HAVE_CXX
				sds[i]->dataset_unconv->hide();
				sds[i]->dataset_conv->show();
#else
				gtk_widget_hide(GTK_WIDGET(sds[i]->dataset_unconv));
				gtk_widget_show(GTK_WIDGET(sds[i]->dataset_conv));
#endif
			}
		}
	}
	else {
		current_conv = XMI_PLOT_UNCONVOLUTED;
		for (i = (results->use_zero_interactions ? 0 : 1) ; i <= results->ninteractions ; i++) {
			if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(sds[i]->checkButton)) == TRUE) {
#ifdef HAVE_CXX
				sds[i]->dataset_conv->hide();
				sds[i]->dataset_unconv->show();
#else
				gtk_widget_hide(GTK_WIDGET(sds[i]->dataset_conv));
				gtk_widget_show(GTK_WIDGET(sds[i]->dataset_unconv));
#endif
			}
		}
	}





	zoom_out();
}

static void scale_combo_changed_cb(GtkComboBox *widget, gpointer user_data) {
#ifdef HAVE_CXX
	if (gtk_combo_box_get_active(GTK_COMBO_BOX(widget)) == 0) {
		current_scale = false;
		plot_ymin = 0.0;
	}
	else {
		current_scale = true;
		plot_ymin = 1.0;
	}
	plot_window->set_axis_logarithmic_y(current_scale);


#else
	if (gtk_combo_box_get_active(GTK_COMBO_BOX(widget)) == 0) {
		current_scale = GTK_PLOT_SCALE_LINEAR;
	}
	else {
		current_scale = GTK_PLOT_SCALE_LOG10;
	}
	gtk_plot_set_yscale(GTK_PLOT(plot_window), current_scale);
	if (current_scale == GTK_PLOT_SCALE_LOG10) {
		plot_ymin = 1.0;
		gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_Y, 0.1,1);
	}
	else if (current_scale == GTK_PLOT_SCALE_LINEAR) {
		plot_ymin = 0.0;
	}

#endif
	//no matter what -> we need to bring everything to 1:1 scaling again
	zoom_out();
}

static void spectra_linestyle_changed_cb(GtkComboBox *widget, gpointer user_data) {
	if (spectra_properties_dataset_active == NULL)
		return;

#ifdef HAVE_CXX
	spectra_properties_dataset_active->set_line_style((Gtk::PLplot::LineStyle) (gtk_combo_box_get_active(GTK_COMBO_BOX(spectra_properties_linestyleW)) + 1));

#else
	GtkPlotLineStyle line_style;
	GdkCapStyle cap_style;
	GdkJoinStyle join_style;
	gfloat width;
	GdkColor color;
	GtkPlotLineStyle line_style_new;


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
#endif
}


static void axis_title_changed_cb(GtkEntry *axis_titleW, gpointer data) {
	int kind = GPOINTER_TO_INT(data);

	gchar *new_title = g_strdup(gtk_entry_get_text(GTK_ENTRY(axis_titleW)));
	if (kind == 0) {
		//X-axis
#ifdef HAVE_CXX
		plot_window->set_axis_title_x(new_title);
#else
		gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM), new_title);
#endif
		g_free(xaxis_title);
		xaxis_title = new_title;
	}
	else {
		//Y-axis
#ifdef HAVE_CXX
		plot_window->set_axis_title_y(new_title);
#else
		gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT), new_title);
#endif
		g_free(yaxis_title);
		yaxis_title = new_title;
	}

#ifdef HAVE_CXX

#else
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));
#endif
	return;
}


static void settings_button_clicked_cb(GtkButton *button, gpointer data) {
	//For now: lin/log, and titles
	GtkWidget *dialog= gtk_dialog_new_with_buttons("Plot properties", GTK_WINDOW(data), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_CLOSE, GTK_RESPONSE_ACCEPT,NULL);
	GtkWidget *vbox = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *scale_combo;
	scale_combo = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(scale_combo),"Linear");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(scale_combo),"Logarithmic");
#ifdef HAVE_CXX
	if (current_scale == false) {
		gtk_combo_box_set_active(GTK_COMBO_BOX(scale_combo), 0);
	}
	else if (current_scale == true) {
		gtk_combo_box_set_active(GTK_COMBO_BOX(scale_combo), 1);
	}
#else
	if (current_scale == GTK_PLOT_SCALE_LINEAR) {
		gtk_combo_box_set_active(GTK_COMBO_BOX(scale_combo), 0);
	}
	else if (current_scale == GTK_PLOT_SCALE_LOG10) {
		gtk_combo_box_set_active(GTK_COMBO_BOX(scale_combo), 1);
	}
#endif

	GtkWidget *lilHBox, *label;
	lilHBox = gtk_hbox_new(FALSE, 2);
	label = gtk_label_new("Y-axis scale");
	gtk_box_pack_start(GTK_BOX(lilHBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(lilHBox), scale_combo, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), lilHBox, FALSE, FALSE,2);
	gtk_box_pack_start(GTK_BOX(vbox), gtk_hseparator_new(), FALSE, FALSE,2);
	GtkWidget *radio_conv = gtk_radio_button_new_with_label_from_widget(NULL, "Use convoluted spectra");
	GtkWidget *radio_unconv= gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(radio_conv), "Use unconvoluted spectra");
	gtk_box_pack_start(GTK_BOX(vbox), radio_conv, FALSE, FALSE,2);
	gtk_box_pack_start(GTK_BOX(vbox), radio_unconv, FALSE, FALSE,2);

	if (current_conv == XMI_PLOT_CONVOLUTED) {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio_conv), TRUE);
	}
	else if (current_conv == XMI_PLOT_UNCONVOLUTED) {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio_unconv), TRUE);
	}
	gtk_box_pack_start(GTK_BOX(vbox), gtk_hseparator_new(), FALSE, FALSE,2);

	lilHBox = gtk_hbox_new(FALSE, 2);
	label = gtk_label_new("X-axis title");
	gtk_box_pack_start(GTK_BOX(lilHBox), label, FALSE, FALSE, 2);
	GtkWidget *xaxis_titleW = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(xaxis_titleW), TRUE);
	gtk_box_pack_start(GTK_BOX(lilHBox), xaxis_titleW, TRUE, TRUE, 2);
	gtk_box_pack_start(GTK_BOX(vbox),lilHBox,FALSE,FALSE,2);
	gtk_entry_set_text(GTK_ENTRY(xaxis_titleW), xaxis_title);

	lilHBox = gtk_hbox_new(FALSE, 2);
	label = gtk_label_new("Y-axis title");
	gtk_box_pack_start(GTK_BOX(lilHBox), label, FALSE, FALSE, 2);
	GtkWidget *yaxis_titleW = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(yaxis_titleW), TRUE);
	gtk_box_pack_start(GTK_BOX(lilHBox), yaxis_titleW, TRUE, TRUE, 2);
	gtk_box_pack_start(GTK_BOX(vbox),lilHBox,FALSE,FALSE,2);
	gtk_entry_set_text(GTK_ENTRY(yaxis_titleW), yaxis_title);

	gtk_widget_set_size_request(dialog, 350,-1);
	gtk_container_set_border_width(GTK_CONTAINER(dialog), 5);
	g_signal_connect(G_OBJECT(xaxis_titleW), "changed", G_CALLBACK(axis_title_changed_cb), GINT_TO_POINTER(0));
	g_signal_connect(G_OBJECT(yaxis_titleW), "changed", G_CALLBACK(axis_title_changed_cb), GINT_TO_POINTER(1));
	g_signal_connect(G_OBJECT(scale_combo),"changed",G_CALLBACK(scale_combo_changed_cb),NULL);
	g_signal_connect(G_OBJECT(radio_conv),"toggled",G_CALLBACK(radio_conv_changed_cb),NULL);

	gtk_widget_show_all(vbox);

	gtk_dialog_run(GTK_DIALOG(dialog));
	gtk_widget_destroy(dialog);

	return;
}

static void export_button_clicked_cb(GtkButton *button, gpointer data) {
	GtkWidget *dialog;

	dialog = xmi_msim_gui_export_canvas_dialog_new("Export spectra",
		GTK_WINDOW(data), canvas);

	gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(dialog),
	  	g_path_get_dirname(results->input->general->outputfile));

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		GError *error = NULL;
		if (!xmi_msim_gui_export_canvas_dialog_save(XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG(dialog), &error)) {
			GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error exporting spectrum");
			gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "%s", error->message);

			gtk_dialog_run(GTK_DIALOG(info_dialog));
			gtk_widget_destroy(info_dialog);

			g_error_free(error);
		}
	}
	gtk_widget_destroy(dialog);


	return;
}


static void draw_page(GtkPrintOperation *operation, GtkPrintContext *context, gint page_nr, gpointer data) {
	cairo_t *cairo;

	cairo = gtk_print_context_get_cairo_context(context);
#ifdef HAVE_CXX
	canvas->draw_plot(Cairo::RefPtr<Cairo::Context>(new Cairo::Context(cairo)),
	XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_WIDTH,
	XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG_A4_HEIGHT);
#else
	gtk_plot_canvas_export_cairo(GTK_PLOT_CANVAS(canvas),cairo);
#endif

	return;
}

static void print_button_clicked_cb(GtkButton *button, gpointer data) {
	GtkPrintOperation *operation;
	GError *error = NULL;
	GtkPrintOperationResult res;

	operation = gtk_print_operation_new();
	gtk_print_operation_set_print_settings(operation,print_settings);
	gtk_print_operation_set_default_page_setup(operation,page_setup);
	gtk_print_operation_set_show_progress(operation,TRUE);
	gtk_print_operation_set_track_print_status(operation, TRUE);
	g_signal_connect(G_OBJECT(operation), "draw-page", G_CALLBACK(draw_page),NULL);
	gtk_print_operation_set_n_pages(operation, 1);
	//gtk_print_operation_set_use_full_page(operation,TRUE);

	res = gtk_print_operation_run(operation, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG, GTK_WINDOW(data),&error);

	if (res == GTK_PRINT_OPERATION_RESULT_APPLY) {
		g_object_unref(print_settings);
		print_settings = (GtkPrintSettings *) g_object_ref(gtk_print_operation_get_print_settings(operation));
	}


	g_object_unref(operation);



	return;
}

static void spectrum_button_clicked_cb(GtkButton *button, gpointer data){
	struct spectra_data *sd = (struct spectra_data *) data;
	gfloat width;
#ifdef HAVE_CXX
	GdkRGBA *color;
	Gtk::PLplot::LineStyle line_style;
#else
	GtkPlotLineStyle line_style;
	GdkCapStyle cap_style;
	GdkJoinStyle join_style;
	GdkColor color;
#endif

	//suspend signals
	g_signal_handler_block((gpointer) spectra_properties_widthW, spectra_properties_widthG);
	g_signal_handler_block((gpointer) spectra_properties_linestyleW, spectra_properties_linestyleG);
	g_signal_handler_block((gpointer) spectra_properties_colorW, spectra_properties_colorG);

	//
	if (current_conv == XMI_PLOT_CONVOLUTED) {
		spectra_properties_dataset_active = sd->dataset_conv;
	}
	else {
		spectra_properties_dataset_active = sd->dataset_unconv;
	}

	//set properties
#ifdef HAVE_CXX
	width = spectra_properties_dataset_active->get_line_width();
	Gdk::RGBA rgba = spectra_properties_dataset_active->get_color();
	color = rgba.gobj();
	line_style = spectra_properties_dataset_active->get_line_style();
	gtk_color_button_set_rgba(GTK_COLOR_BUTTON(spectra_properties_colorW), color);
	gtk_combo_box_set_active(GTK_COMBO_BOX(spectra_properties_linestyleW), line_style - 1);
#else
	gtk_plot_data_get_line_attributes(spectra_properties_dataset_active, &line_style, &cap_style, &join_style, &width, &color);
	gtk_color_button_set_color(GTK_COLOR_BUTTON(spectra_properties_colorW), &color);
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
#endif
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spectra_properties_widthW),width);

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
#ifdef HAVE_CXX
	Gtk::PLplot::PlotData2D *dataset;
#else
	GtkPlotData *dataset;
#endif

	if (current_conv == XMI_PLOT_CONVOLUTED) {
		dataset = sd->dataset_conv;
	}
	else {
		dataset = sd->dataset_unconv;
	}


	if (gtk_toggle_button_get_active(toggleButton) == TRUE) {
#ifdef HAVE_CXX
		dataset->show();
#else
		gtk_widget_show(GTK_WIDGET(dataset));
#endif
		gtk_widget_set_sensitive(sd->button,TRUE);
	}
	else {
#ifdef HAVE_CXX
		dataset->hide();
#else
		gtk_widget_hide(GTK_WIDGET(dataset));
#endif
		gtk_widget_set_sensitive(sd->button,FALSE);
	}
#ifndef HAVE_CXX
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));
#endif

	return;

}

GtkWidget *init_results(GtkWidget *window) {


	GtkWidget *graphics_hbox;
	GtkWidget *scrolled_window;
	GtkWidget *paned = gtk_vpaned_new();
	GtkWidget *frame;

	GtkWidget *spectra_box;//VBox
	GtkWidget *entry;
	gdouble magnifier;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;

	//this could be fetched from preferences in the future
	xaxis_title = g_strdup("Energy (keV)");
	yaxis_title = g_strdup("Intensity (counts/channel)");
#ifdef HAVE_CXX
	current_scale = true;
#else
	current_scale = GTK_PLOT_SCALE_LOG10;
#endif
	current_conv = XMI_PLOT_CONVOLUTED;

	magnifier = 0.75;

	//superframe = gtk_vbox_new(FALSE,2);
	graphics_hbox = gtk_hbox_new(FALSE,2);
	GtkWidget *aspect_frame = gtk_aspect_frame_new("", 0.0, 0.0, 842.0/595.0, FALSE);

#ifdef HAVE_CXX
	canvas = Gtk::manage(new Gtk::PLplot::Canvas());
	canvas->set_hexpand(true);
	canvas->set_vexpand(true);
	gtk_widget_set_hexpand(aspect_frame, TRUE);
	gtk_widget_set_vexpand(aspect_frame, TRUE);
	gtk_container_add(GTK_CONTAINER(aspect_frame), GTK_WIDGET(canvas->gobj()));
#else
	canvas = gtk_plot_canvas_new(GTK_PLOT_A4_H, GTK_PLOT_A4_W,magnifier);
	canvas_height = 0;
	spectra_region_changedG = g_signal_connect(G_OBJECT(canvas),"select-region",G_CALLBACK(spectra_region_changed_cb),NULL);
	g_signal_handler_block(G_OBJECT(canvas),spectra_region_changedG);
	spectra_region_double_clickedG = g_signal_connect(G_OBJECT(canvas),"button-press-event",G_CALLBACK(spectra_region_double_clicked_cb),NULL);
	g_signal_handler_block(G_OBJECT(canvas),spectra_region_double_clickedG);
	g_signal_connect(G_OBJECT(canvas),"expose-event", G_CALLBACK(resize_canvas_cb),paned);
	GTK_PLOT_CANVAS_UNSET_FLAGS(GTK_PLOT_CANVAS(canvas), (GtkPlotCanvasFlags) (GTK_PLOT_CANVAS_CAN_SELECT | GTK_PLOT_CANVAS_CAN_SELECT_ITEM)); //probably needs to be unset when initializing, but set when data is available
	gtk_plot_canvas_set_background(GTK_PLOT_CANVAS(canvas),&white_plot);
	gtk_container_add(GTK_CONTAINER(aspect_frame), canvas);
#endif

	gtk_box_pack_start(GTK_BOX(graphics_hbox), aspect_frame, TRUE, TRUE, 0);

	spectra_box = gtk_vbox_new(FALSE,1);
	spectra_button_box = gtk_vbox_new(TRUE,3);
	GtkWidget *label = gtk_label_new("Please run a simulation\nor load an XMSO file");
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_box_pack_start(GTK_BOX(spectra_button_box),label,TRUE,TRUE,0);
	gtk_box_pack_start(GTK_BOX(spectra_box),spectra_button_box,FALSE,FALSE,0);


	GtkWidget *table = gtk_table_new(3, 2 , FALSE);
	label = gtk_label_new("Energy (keV)");
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 0, 1);
	entry = gtk_entry_new();
	gtk_table_attach_defaults(GTK_TABLE(table), entry, 1, 2, 0, 1);
	gtk_widget_set_size_request(GTK_WIDGET(entry), 100, -1);
	//gtk_entry_set_max_length(GTK_ENTRY(entry), 10);
	gtk_editable_set_editable(GTK_EDITABLE(entry), FALSE);
	//gtk_widget_set_sensitive(GTK_WIDGET(entry), FALSE);
	xCoordW = entry;

	label = gtk_label_new("Channel number");
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 1, 2);
	entry = gtk_entry_new();
	gtk_table_attach_defaults(GTK_TABLE(table), entry, 1, 2, 1, 2);
	gtk_widget_set_size_request(GTK_WIDGET(entry), 100, -1);
	//gtk_entry_set_max_length(GTK_ENTRY(entry), 10);
	gtk_editable_set_editable(GTK_EDITABLE(entry), FALSE);
	//gtk_widget_set_sensitive(GTK_WIDGET(entry), FALSE);
	channelCoordW = entry;

	label = gtk_label_new("Intensity");
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 2, 3);
	entry = gtk_entry_new();
	gtk_table_attach_defaults(GTK_TABLE(table), entry, 1, 2, 2, 3);
	gtk_widget_set_size_request(GTK_WIDGET(entry), 100, -1);
	//gtk_entry_set_max_length(GTK_ENTRY(entry), 10);
	gtk_editable_set_editable(GTK_EDITABLE(entry), FALSE);
	//gtk_widget_set_sensitive(GTK_WIDGET(entry), FALSE);
	yCoordW = entry;
	gtk_box_pack_end(GTK_BOX(spectra_box), table, FALSE, FALSE, 2);

#ifdef HAVE_CXX
	//In Gtkmm-PLplot, cursor movements are connected to plots, not canvases.
#else
	spectra_region_mouse_movedG = g_signal_connect(G_OBJECT(canvas),"motion-notify-event",G_CALLBACK(spectra_region_mouse_moved_cb), NULL);
	g_signal_handler_block((gpointer) canvas, spectra_region_mouse_movedG);
#endif

	settings_button = gtk_button_new_from_stock(GTK_STOCK_PROPERTIES);
	g_signal_connect(G_OBJECT(settings_button),"clicked",G_CALLBACK(settings_button_clicked_cb),(gpointer)window);
	export_button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	g_signal_connect(G_OBJECT(export_button),"clicked",G_CALLBACK(export_button_clicked_cb),(gpointer)window);
	print_button = gtk_button_new_from_stock(GTK_STOCK_PRINT);
	g_signal_connect(G_OBJECT(print_button),"clicked",G_CALLBACK(print_button_clicked_cb),(gpointer)window);

	gtk_box_pack_end(GTK_BOX(spectra_box),settings_button, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(spectra_box),export_button, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(spectra_box),print_button, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(print_button,FALSE);

	//print settings
	print_settings = gtk_print_settings_new();
	gtk_print_settings_set_orientation(print_settings,GTK_PAGE_ORIENTATION_LANDSCAPE);
	gtk_print_settings_set_paper_size(print_settings,gtk_paper_size_new(GTK_PAPER_NAME_A4));
	page_setup = gtk_page_setup_new();
	gtk_page_setup_set_orientation(page_setup,GTK_PAGE_ORIENTATION_LANDSCAPE);
	gtk_page_setup_set_paper_size_and_default_margins(page_setup,gtk_paper_size_new(GTK_PAPER_NAME_A4));

	gtk_widget_set_sensitive(export_button,FALSE);
	gtk_widget_set_sensitive(settings_button,FALSE);


	gtk_box_pack_end(GTK_BOX(graphics_hbox),spectra_box,FALSE,FALSE,2);
	gtk_container_set_border_width(GTK_CONTAINER(graphics_hbox), 5);
	gtk_widget_show_all(spectra_box);

	//gtk_box_pack_start(GTK_BOX(superframe),graphics_hbox,FALSE,FALSE,2);
	frame = gtk_frame_new(NULL);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(frame), graphics_hbox);
	gtk_widget_set_size_request(frame,-1, (gint) (595*0.25));
	gtk_paned_pack1(GTK_PANED(paned), frame, TRUE, FALSE);

	//set current result variable to NULL
	results = NULL;

	plot_window = NULL;

	//countsTS etc
	countsTS = gtk_tree_store_new(N_COLUMNS,
					G_TYPE_STRING,
					G_TYPE_STRING,
					G_TYPE_DOUBLE,
					G_TYPE_BOOLEAN,
					G_TYPE_BOOLEAN,
					G_TYPE_POINTER,
					G_TYPE_STRING,
					G_TYPE_DOUBLE
					);

	countsTV = gtk_tree_view_new_with_model(GTK_TREE_MODEL(countsTS));
	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Element", renderer, "text", ELEMENT_COLUMN, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(countsTV), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("XRF line", renderer, "text", LINE_COLUMN, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(countsTV), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column,"Energy");
	gtk_tree_view_append_column(GTK_TREE_VIEW(countsTV), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, cell_print_double, GINT_TO_POINTER(ENERGY_COLUMN), NULL);

	renderer = gtk_cell_renderer_toggle_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	gtk_cell_renderer_toggle_set_radio(GTK_CELL_RENDERER_TOGGLE(renderer),FALSE);
	gtk_cell_renderer_toggle_set_activatable(GTK_CELL_RENDERER_TOGGLE(renderer),TRUE);
	g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(cell_active_toggle), NULL);
	/*column = gtk_tree_view_column_new_with_attributes("Show line",renderer, "active", SHOW_LINE_COLUMN,NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(countsTV), column);*/
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column,"Show line");
	gtk_tree_view_append_column(GTK_TREE_VIEW(countsTV), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, cell_visible_toggle, NULL, NULL);




	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("# Interactions", renderer, "text", INTERACTION_COLUMN, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(countsTV), column);

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column,"Intensity");
	gtk_tree_view_append_column(GTK_TREE_VIEW(countsTV), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, cell_print_double, GINT_TO_POINTER(COUNTS_COLUMN), NULL);

	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), countsTV);

	//gtk_box_pack_start(GTK_BOX(superframe),scrolled_window,TRUE, TRUE,2);
	frame = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(frame), scrolled_window);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
	gtk_paned_pack2(GTK_PANED(paned), frame, TRUE, FALSE);

	gtk_widget_show_all(paned);


	//finalize widget
	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), paned);


	init_spectra_properties(window);

	//gtk_widget_show_all(scrolled_window);

	return scrolled_window;
}

static void clear_container (GtkWidget *widget, gpointer data) {


	//gtk_widget_destroy(widget);
	gtk_container_remove(GTK_CONTAINER(data),widget);

}

int plot_spectra_from_file(char *xmsofile) {
	int i,j,k;
	GtkWidget *checkButton;
	GtkWidget *button;
	GtkWidget *spectrum_hbox;
	struct spectra_data *sd;
	char *buffer;
	double *temp_channels, *temp_energies;
	GtkTreeIter iter1, iter2, iter3;
	char *symbol;
	gchar *txt;
#ifdef HAVE_CXX
	Gtk::PLplot::PlotData2D *plot_data;
	Gtk::PLplot::PlotData2D *dataset_conv, *dataset_unconv;
	#define PLOT_SHOW plot_data->show()
	#define PLOT_HIDE plot_data->hide()
#else
	GtkPlotCanvasChild *child;
	GtkPlotData *plot_data;
	GtkPlotData *dataset_conv, *dataset_unconv;
	#define PLOT_SHOW gtk_widget_show(GTK_WIDGET(plot_data))
	#define PLOT_HIDE gtk_widget_hide(GTK_WIDGET(plot_data))
#endif
	gdouble *plot_data_x;
	gdouble *plot_data_y;



	//free memory if necessary
	if (results != NULL)
		xmi_free_output(results);


	if (xmi_read_output_xml(xmsofile, &results) == 0) {
		fprintf(stderr,"%s could not be read\n", xmsofile);
		return 0;
	}

#ifndef HAVE_CXX
	g_signal_handler_unblock(G_OBJECT(canvas),spectra_region_changedG);
	g_signal_handler_unblock(G_OBJECT(canvas),spectra_region_double_clickedG);
	g_signal_handler_unblock((gpointer) canvas, spectra_region_mouse_movedG);
#endif

	//clear plotwindow if necessary
	//start with buttonbox
	//clear it if necessary
	gtk_container_foreach(GTK_CONTAINER(spectra_button_box), clear_container, spectra_button_box);

	//clear tree
	gtk_tree_store_clear(countsTS);

	//clear plot
#ifdef HAVE_CXX
	try {
		canvas->remove_plot(0);
	}
	catch (Gtk::PLplot::Exception &e) {
		//this will fail if there's no plot available
	}
#else
	GList *list;
	list = GTK_PLOT_CANVAS(canvas)->childs;
	while (list) {
		child = GTK_PLOT_CANVAS_CHILD(list->data);
		gtk_plot_canvas_remove_child(GTK_PLOT_CANVAS(canvas), child);
		list = GTK_PLOT_CANVAS(canvas)->childs;
	}
#endif

	//calculate maximum x and y value
	temp_channels = (double *) g_malloc(sizeof(double) * results->input->detector->nchannels);
	for (i=0 ; i < results->input->detector->nchannels ; i++) {
		temp_channels[i] = results->channels_conv[results->ninteractions][i];
	}
	plot_ymax_conv = xmi_maxval_double(temp_channels,results->input->detector->nchannels)*1.2;
	for (i=0 ; i < results->input->detector->nchannels ; i++) {
		temp_channels[i] = results->channels_unconv[results->ninteractions][i];
	}
	plot_ymax_unconv = xmi_maxval_double(temp_channels,results->input->detector->nchannels)*1.2;
	g_free(temp_channels);
#ifdef HAVE_CXX
	if (current_scale == true) {
		plot_ymin = 1.0;
	}
	else if (current_scale == false) {
		plot_ymin = 0.0;
	}
#else
	if (current_scale == GTK_PLOT_SCALE_LOG10) {
		plot_ymin = 1.0;
	}
	else if (current_scale == GTK_PLOT_SCALE_LINEAR) {
		plot_ymin = 0.0;
	}
#endif
	plot_xmin = 0.0;
	plot_xmax = results->input->detector->nchannels * results->input->detector->gain + results->input->detector->zero;
	double plot_ymax = (current_conv == XMI_PLOT_CONVOLUTED) ? plot_ymax_conv : plot_ymax_unconv;

	temp_energies = (double *) g_malloc(sizeof(double)*results->input->detector->nchannels);
	for (i = 0 ; i < results->input->detector->nchannels ; i++) {
		temp_energies[i] = results->input->detector->gain * i + results->input->detector->zero;
	}

#ifdef HAVE_CXX
	plot_window = Gtk::manage(new Plot2D(xaxis_title, yaxis_title));
	plot_window->hide();
	plot_window->hide_legend();
	plot_window->set_axis_logarithmic_y(current_scale);
	plot_window->set_box_style(Gtk::PLplot::BoxStyle::BOX_TICKS_TICK_LABELS_MAIN_AXES_MAJOR_TICK_GRID);
	plot_window->signal_cursor_motion().connect([](double x, double y){
		gchar *buffer = g_strdup_printf("%lg", x);
		gtk_entry_set_text(GTK_ENTRY(xCoordW), buffer);
		g_free(buffer);
		buffer = g_strdup_printf("%i",(int) ((x-results->input->detector->zero)/results->input->detector->gain));
		gtk_entry_set_text(GTK_ENTRY(channelCoordW), buffer);
		g_free(buffer);
		buffer = g_strdup_printf("%lg",y);
		gtk_entry_set_text(GTK_ENTRY(yCoordW), buffer);
		g_free(buffer);
        });
	canvas->add_plot(*plot_window);
#else
	//add box with default settings
	plot_window = gtk_plot_new_with_size(NULL,.65,.45);
	gtk_plot_set_background(GTK_PLOT(plot_window),&white_plot);
	gtk_plot_hide_legends(GTK_PLOT(plot_window));


	//x-axis number of ticks continues to be a problem
	//it's quite clear that the gtkextra developers were too lazy to deal with it themselves :-)

	double tickstep = xmi_msim_gui_utils_get_tickstep(plot_xmin, plot_xmax);

	gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_X, tickstep,5);
	if (current_scale == GTK_PLOT_SCALE_LINEAR) {
		tickstep = xmi_msim_gui_utils_get_tickstep(plot_ymin, plot_ymax);
		gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_Y, tickstep,5);
	}
	gtk_plot_set_yscale(GTK_PLOT(plot_window), current_scale);
	gtk_plot_set_range(GTK_PLOT(plot_window),plot_xmin, plot_xmax, plot_ymin, plot_ymax);
	gtk_plot_clip_data(GTK_PLOT(plot_window), TRUE);
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP));
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT));
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),yaxis_title);
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),xaxis_title);
	//font will be a problem. Helvetica should be ok for Mac OS X, Arial for Windows, but Linux... pfft
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica", RESULTS_PLOT_TITLE,90,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Helvetica",RESULTS_PLOT_TITLE,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,0);
        gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),GTK_PLOT_LABEL_FLOAT,0);
	if (plot_ymax < 100000.0) {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_FLOAT,0);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_FLOAT,0);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",RESULTS_PLOT_LABELS_LR_FLOAT,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",RESULTS_PLOT_LABELS_LR_FLOAT,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}
	else {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_EXP,1);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_EXP,1);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",RESULTS_PLOT_LABELS_LR_EXP,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",RESULTS_PLOT_LABELS_LR_EXP,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}
	gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Helvetica",RESULTS_PLOT_LABELS_TP,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),"Helvetica",RESULTS_PLOT_LABELS_TP,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_show_labels(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),0);
        gtk_plot_grids_set_visible(GTK_PLOT(plot_window),TRUE,FALSE,TRUE,FALSE);
	child = gtk_plot_canvas_plot_new(GTK_PLOT(plot_window));
        gtk_plot_canvas_put_child(GTK_PLOT_CANVAS(canvas), child, .15,.05,.90,.85);
        gtk_widget_show(plot_window);
	GTK_PLOT_CANVAS_SET_FLAGS(GTK_PLOT_CANVAS(canvas), GTK_PLOT_CANVAS_CAN_SELECT );
#endif

	//fill it up again

	if (sds != NULL)
		g_free(sds);
	sds = (struct spectra_data **) g_malloc(sizeof(struct spectra_data*)*(results->ninteractions+1));

	for (i = (results->use_zero_interactions ? 0 : 1) ; i <= results->ninteractions ; i++) {

		checkButton = gtk_check_button_new();
		button = gtk_button_new_from_stock(GTK_STOCK_PROPERTIES);
		spectrum_hbox = gtk_hbox_new(FALSE,2);
		gtk_box_pack_start(GTK_BOX(spectrum_hbox),checkButton,FALSE,FALSE,1);
		gtk_box_pack_end(GTK_BOX(spectrum_hbox),button,FALSE,FALSE,1);
		gtk_box_pack_start(GTK_BOX(spectra_button_box),spectrum_hbox,FALSE,FALSE,1);

		temp_channels = (double *) g_malloc(sizeof(double)*results->input->detector->nchannels);
		for (j = 0 ; j < results->input->detector->nchannels ; j++)
			temp_channels[j]=results->channels_conv[i][j];

#ifdef HAVE_CXX
		std::vector<double> x_vals(temp_energies, temp_energies + results->input->detector->nchannels);
		std::vector<double> y_vals(temp_channels, temp_channels + results->input->detector->nchannels);
		std::for_each(std::begin(y_vals), std::end(y_vals), [](double &a) { if (a < 1.0 ) a = 1.0;});

		dataset_conv = Gtk::manage(
			new Gtk::PLplot::PlotData2D(
				x_vals,
				y_vals
			)
		);
		dataset_conv->hide();
		plot_window->add_data(*dataset_conv);
#else
		dataset_conv = GTK_PLOT_DATA(gtk_plot_data_new());
		gtk_plot_add_data(GTK_PLOT(plot_window),dataset_conv);
		gtk_plot_data_set_numpoints(dataset_conv, results->input->detector->nchannels);
		gtk_plot_data_set_x(dataset_conv, temp_energies);
		gtk_plot_data_set_y(dataset_conv, temp_channels);
#endif


		temp_channels = (double *) g_malloc(sizeof(double)*results->input->detector->nchannels);
		for (j = 0 ; j < results->input->detector->nchannels ; j++)
			temp_channels[j]=results->channels_unconv[i][j];

#ifdef HAVE_CXX
		std::vector<double> x_vals2(temp_energies, temp_energies + results->input->detector->nchannels);
		std::vector<double> y_vals2(temp_channels, temp_channels + results->input->detector->nchannels);
		std::for_each(std::begin(y_vals2), std::end(y_vals2), [](double &a) { if (a < 1.0 ) a = 1.0;});
		dataset_unconv = Gtk::manage(
			new Gtk::PLplot::PlotData2D(
				x_vals2,
				y_vals2
			)
		);
		dataset_unconv->hide();
		plot_window->add_data(*dataset_unconv);
#else
		dataset_unconv = GTK_PLOT_DATA(gtk_plot_data_new());
		gtk_plot_add_data(GTK_PLOT(plot_window),dataset_unconv);
		gtk_plot_data_set_numpoints(dataset_unconv, results->input->detector->nchannels);
		gtk_plot_data_set_x(dataset_unconv, temp_energies);
		gtk_plot_data_set_y(dataset_unconv, temp_channels);
#endif

		sd = (struct spectra_data *) g_malloc(sizeof(struct spectra_data));
		sds[i] = sd;
		sd->checkButton = checkButton;
		sd->button = button;
		sd->dataset_conv = dataset_conv;
		sd->dataset_unconv = dataset_unconv;

		XmiColor *color_plot;

		switch (i-(results->use_zero_interactions ? 0 : 1)) {
			case 0:
				color_plot = &blue_plot;
				if (results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions",0);
				}
				else {
					buffer = g_strdup_printf("%i interaction",1);
				}
				break;
			case 1:
				color_plot = &red_plot;
				if (results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interaction",1);
				}
				else {
					buffer = g_strdup_printf("%i interactions",2);
				}
				break;
			case 2:
				color_plot = &green_plot;
				if (results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interaction",2);
				}
				else {
					buffer = g_strdup_printf("%i interactions",3);
				}
				break;
			case 3:
				color_plot = &purple_plot;
				if (results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions",3);
				}
				else {
					buffer = g_strdup_printf("%i interactions",4);
				}
				break;
			case 4:
				color_plot = &yellow_plot;
				if (results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions",4);
				}
				else {
					buffer = g_strdup_printf("%i interactions",5);
				}
				break;
			case 5:
				color_plot = &pink_plot;
				if (results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions",5);
				}
				else {
					buffer = g_strdup_printf("%i interactions",6);
				}
				break;
			default:
				color_plot = &black_plot;
				if (results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions",i-(results->use_zero_interactions ? 0 : 1));
				}
				else {
					buffer = g_strdup_printf("%i interactions",i+1-(results->use_zero_interactions ? 0 : 1));
				}
				break;
		}
#ifdef HAVE_CXX
		dataset_conv->set_color(*(*color_plot));
		dataset_unconv->set_color(*(*color_plot));
		if (current_conv == XMI_PLOT_CONVOLUTED) {
			dataset_conv->show();
			dataset_unconv->hide();
		}
		else {
			dataset_conv->hide();
			dataset_unconv->show();
		}
#else
		gtk_plot_data_set_line_attributes(dataset_conv, (GtkPlotLineStyle) GTK_PLOT_LINE_SOLID, (GdkCapStyle) 0, (GdkJoinStyle) 0, 1, color_plot);
		gtk_plot_data_set_line_attributes(dataset_unconv, (GtkPlotLineStyle) GTK_PLOT_LINE_SOLID, (GdkCapStyle) 0, (GdkJoinStyle) 0, 1, color_plot);
		if (current_conv == XMI_PLOT_CONVOLUTED) {
			gtk_widget_show(GTK_WIDGET(dataset_conv));
		}
		else {
			gtk_widget_show(GTK_WIDGET(dataset_unconv));
		}
#endif

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
#ifndef HAVE_CXX
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));
	gtk_plot_refresh(GTK_PLOT(plot_window),NULL);
#endif
	gtk_widget_set_sensitive(print_button,TRUE);
	gtk_widget_set_sensitive(export_button,TRUE);
	gtk_widget_set_sensitive(settings_button,TRUE);


	//treestore stuff
	if (results->brute_force_history != NULL) {
		//brute force mode
		for (i = 0 ; i < results->nbrute_force_history ; i++) {
			//iterating over atomic numbers -> highest level
			gtk_tree_store_append(countsTS, &iter1, NULL);
			symbol = AtomicNumberToSymbol(results->brute_force_history[i].atomic_number);
			gtk_tree_store_set(countsTS, &iter1,
				ELEMENT_COLUMN, symbol,
				LINE_COLUMN , "all",
				SHOW_LINE_COLUMN, FALSE,
				CONSISTENT_COLUMN, TRUE,
				INTERACTION_COLUMN, "all",
				COUNTS_COLUMN, results->brute_force_history[i].total_counts,
				-1);
			xrlFree(symbol);
			for (j = 0 ; j < results->brute_force_history[i].n_lines ; j++) {
				plot_data_x = (gdouble *) g_malloc(sizeof(gdouble)*2);
				plot_data_y = (gdouble *) g_malloc(sizeof(gdouble)*2);
				plot_data_x[0] = results->brute_force_history[i].lines[j].energy;
				plot_data_x[1] = results->brute_force_history[i].lines[j].energy;
				plot_data_y[0] = plot_ymin;
				plot_data_y[1] = plot_ymax_unconv;
#ifdef HAVE_CXX
				plot_data = Gtk::manage(
					new Gtk::PLplot::PlotData2D(
						std::vector<double>(plot_data_x, plot_data_x + 2),
						std::vector<double>(plot_data_y, plot_data_y + 2),
						*black_plot
					)
				);
				plot_data->hide();
				plot_window->add_data(*plot_data);
#else
				plot_data = GTK_PLOT_DATA(gtk_plot_data_new());
				gtk_plot_data_set_numpoints(plot_data,2);
				gtk_plot_data_set_x(plot_data,plot_data_x);
				gtk_plot_data_set_y(plot_data,plot_data_y);
				gtk_plot_data_set_line_attributes(plot_data, (GtkPlotLineStyle) GTK_PLOT_LINE_SOLID, (GdkCapStyle) 0, (GdkJoinStyle) 0,1,&black_plot);

				gtk_plot_add_data(GTK_PLOT(plot_window),plot_data);
				gtk_widget_hide(GTK_WIDGET(plot_data));
#endif


				gtk_tree_store_append(countsTS, &iter2, &iter1);
				gtk_tree_store_set(countsTS, &iter2,
					LINE_COLUMN, results->brute_force_history[i].lines[j].line_type,
					ENERGY_COLUMN, results->brute_force_history[i].lines[j].energy,
					SHOW_LINE_COLUMN, FALSE,
					CONSISTENT_COLUMN, TRUE,
					INTERACTION_COLUMN, "all",
					CHILD_COLUMN, plot_data,
					COUNTS_COLUMN, results->brute_force_history[i].lines[j].total_counts,
					-1);



				for (k = 0 ; k < results->brute_force_history[i].lines[j].n_interactions ; k++) {
					gtk_tree_store_append(countsTS, &iter3, &iter2);
					txt = g_strdup_printf("%i",results->brute_force_history[i].lines[j].interactions[k].interaction_number);
					gtk_tree_store_set(countsTS, &iter3,
						INTERACTION_COLUMN,txt,
						COUNTS_COLUMN, results->brute_force_history[i].lines[j].interactions[k].counts,
						-1);
					g_free(txt);
				}
			}
		}
	}
	else if (results->var_red_history != NULL) {
		//variance reduction mode

		for (i = 0 ; i < results->nvar_red_history ; i++) {
			//iterating over atomic numbers -> highest level
			gtk_tree_store_append(countsTS, &iter1, NULL);
			symbol = AtomicNumberToSymbol(results->var_red_history[i].atomic_number);
			gtk_tree_store_set(countsTS, &iter1,
				ELEMENT_COLUMN, symbol,
				LINE_COLUMN , "all",
				SHOW_LINE_COLUMN, FALSE,
				CONSISTENT_COLUMN, TRUE,
				INTERACTION_COLUMN, "all",
				COUNTS_COLUMN, results->var_red_history[i].total_counts,
				-1);
			xrlFree(symbol);
			for (j = 0 ; j < results->var_red_history[i].n_lines ; j++) {
				plot_data_x = (gdouble *) g_malloc(sizeof(gdouble)*2);
				plot_data_y = (gdouble *) g_malloc(sizeof(gdouble)*2);
				plot_data_x[0] = results->var_red_history[i].lines[j].energy;
				plot_data_x[1] = results->var_red_history[i].lines[j].energy;
				plot_data_y[0] = plot_ymin;
				plot_data_y[1] = plot_ymax_unconv;
#ifdef HAVE_CXX
				plot_data = Gtk::manage(
					new Gtk::PLplot::PlotData2D(
						std::vector<double>(plot_data_x, plot_data_x + 2),
						std::vector<double>(plot_data_y, plot_data_y + 2),
						*black_plot
					)
				);
				plot_data->hide();
				plot_window->add_data(*plot_data);
#else
				plot_data = GTK_PLOT_DATA(gtk_plot_data_new());
				gtk_plot_data_set_numpoints(plot_data,2);
				gtk_plot_data_set_x(plot_data,plot_data_x);
				gtk_plot_data_set_y(plot_data,plot_data_y);
				gtk_plot_data_set_line_attributes(plot_data,(GtkPlotLineStyle) GTK_PLOT_LINE_SOLID, (GdkCapStyle) 0, (GdkJoinStyle) 0,1,&black_plot);

				gtk_plot_add_data(GTK_PLOT(plot_window),plot_data);
				gtk_widget_hide(GTK_WIDGET(plot_data));
#endif


				gtk_tree_store_append(countsTS, &iter2, &iter1);
				gtk_tree_store_set(countsTS, &iter2,
					LINE_COLUMN, results->var_red_history[i].lines[j].line_type,
					ENERGY_COLUMN, results->var_red_history[i].lines[j].energy,
					SHOW_LINE_COLUMN, FALSE,
					CONSISTENT_COLUMN, TRUE,
					INTERACTION_COLUMN, "all",
					CHILD_COLUMN, plot_data,
					COUNTS_COLUMN, results->var_red_history[i].lines[j].total_counts,
					-1);


				for (k = 0 ; k < results->var_red_history[i].lines[j].n_interactions ; k++) {
					gtk_tree_store_append(countsTS, &iter3, &iter2);
					txt = g_strdup_printf("%i",results->var_red_history[i].lines[j].interactions[k].interaction_number);
					gtk_tree_store_set(countsTS, &iter3,
						INTERACTION_COLUMN,txt,
						COUNTS_COLUMN, results->var_red_history[i].lines[j].interactions[k].counts,
						-1);
					g_free(txt);
				}
			}
		}



	}
	else {
		fprintf(stderr,"XMSO file contains no history!\n");
		//pop up a dialog or so with a warning...
	}

#ifdef HAVE_CXX
	plot_window->set_region(plot_xmin, plot_xmax, plot_ymin, plot_ymax);
	plot_window->show();
#endif

	return 1;
}


void init_spectra_properties(GtkWidget *parent) {


	spectra_propertiesW = gtk_dialog_new_with_buttons("Spectrum properties", GTK_WINDOW(parent), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_CLOSE, GTK_RESPONSE_ACCEPT,NULL);
	spectra_properties_dataset_active = NULL;


	GtkWidget *vbox = gtk_dialog_get_content_area(GTK_DIALOG(spectra_propertiesW));
	/* properties that can be changed:
	 * 	LineStyle
	 * 	Width
	 * 	color
	 */


	//linestyle
	GtkWidget *hbox = gtk_hbox_new(FALSE,3);
	spectra_properties_linestyleW = gtk_combo_box_text_new();
#ifdef HAVE_CXX
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Solid");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Short dash - Short gap");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Long dash - Long gap");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Long dash - Short gap");
#else
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Solid");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Dotted");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Dashed");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Dot - Dash");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Dot - Dot - Dash");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Dot - Dash - Dash");
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
