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
#include "xmimsim-gui-colors.h"
#include "xmimsim-gui-export-canvas-dialog.h"
#include <xraylib.h>
#include "xmi_data_structs.h"
#include "xmi_xml.h"
#include <gtkmm-plplot/plot2d.h>
#include <gtkmm-plplot/plotobject2dline.h>
#include <gtkmm-plplot/canvas.h>
#include <gtkmm-plplot/exception.h>
#include <gtkmm/box.h>
#include <gtkmm/checkbutton.h>

enum XmiMsimGuiXmsoResultsScrolledWindowPlotMode {
	CONVOLUTED,
	UNCONVOLUTED
};

static void spectra_linestyle_changed_cb(GtkComboBox *widget, gpointer user_data) {
	Gtk::PLplot::PlotData2D *spectra_properties_dataset_active = (Gtk::PLplot::PlotData2D *) user_data;

	spectra_properties_dataset_active->set_line_style((Gtk::PLplot::LineStyle) (gtk_combo_box_get_active(GTK_COMBO_BOX(widget)) + 1));
}

static void spectra_color_changed_cb(GtkColorButton *widget, gpointer user_data) {
	Gtk::PLplot::PlotData2D *spectra_properties_dataset_active = (Gtk::PLplot::PlotData2D *) user_data;

	GdkRGBA color;
	gtk_color_chooser_get_rgba(GTK_COLOR_CHOOSER(widget), &color);
	spectra_properties_dataset_active->set_color(Gdk::RGBA(&color, true));
}

static void spectra_width_changed_cb(GtkSpinButton *spinbutton, gpointer user_data) {
	Gtk::PLplot::PlotData2D *spectra_properties_dataset_active = (Gtk::PLplot::PlotData2D *) user_data;

	gdouble width_new;
	width_new = gtk_spin_button_get_value(spinbutton);
	spectra_properties_dataset_active->set_line_width(width_new);
}

class SpectraDataBox : public Gtk::Box {
private:
	Gtk::PLplot::PlotData2D *dataset_conv;
	Gtk::PLplot::PlotData2D *dataset_unconv;
	Gtk::CheckButton checkButton;
	Gtk::Button dialogButton;
	enum XmiMsimGuiXmsoResultsScrolledWindowPlotMode plot_mode;

	void dialog_button_clicked() {
		gfloat width;
		GdkRGBA *color;
		Gtk::PLplot::LineStyle line_style;

		GtkWidget *spectra_propertiesW;
		GtkWidget *spectra_properties_linestyleW;
		GtkWidget *spectra_properties_widthW;
		GtkWidget *spectra_properties_colorW;
		Gtk::PLplot::PlotData2D *spectra_properties_dataset_active;

		if (plot_mode == CONVOLUTED) {
			spectra_properties_dataset_active = dataset_conv;
		}
		else {
			spectra_properties_dataset_active = dataset_unconv;
		}

		spectra_propertiesW = gtk_dialog_new_with_buttons("Spectrum properties", GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(this->gobj()))), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), "_Close", GTK_RESPONSE_ACCEPT,NULL);

		GtkWidget *vbox = gtk_dialog_get_content_area(GTK_DIALOG(spectra_propertiesW));

		//linestyle
		GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 3);
		spectra_properties_linestyleW = gtk_combo_box_text_new();
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Solid");
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Short dash - Short gap");
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Long dash - Long gap");
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(spectra_properties_linestyleW),"Long dash - Short gap");
		gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new("Line style"), FALSE, FALSE, 2);
		gtk_box_pack_start(GTK_BOX(hbox), spectra_properties_linestyleW, FALSE, FALSE, 2);

		gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);

		//width
		hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 3);
		spectra_properties_widthW = gtk_spin_button_new(
			GTK_ADJUSTMENT(gtk_adjustment_new(1.0, 0.1, 10.0, 0.1,0.5,0.0)),
			0.1,1
		);
		gtk_box_pack_start(GTK_BOX(hbox),gtk_label_new("Line width"), FALSE, FALSE, 2);
		gtk_box_pack_start(GTK_BOX(hbox),spectra_properties_widthW, FALSE, FALSE, 2);

		gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);

		// get current properties
		width = spectra_properties_dataset_active->get_line_width();
		Gdk::RGBA rgba = spectra_properties_dataset_active->get_color();
		color = rgba.gobj();
		line_style = spectra_properties_dataset_active->get_line_style();

		//color
		hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 3);
		spectra_properties_colorW = gtk_color_button_new_with_rgba(color);
		gtk_color_chooser_set_use_alpha(GTK_COLOR_CHOOSER(spectra_properties_colorW), FALSE);
		gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new("Line color"), FALSE, FALSE, 2);
		gtk_box_pack_start(GTK_BOX(hbox), spectra_properties_colorW, FALSE, FALSE, 2);
		gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);

		gtk_widget_show_all(vbox);

		gtk_combo_box_set_active(GTK_COMBO_BOX(spectra_properties_linestyleW), line_style - 1);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(spectra_properties_widthW), width);

		g_signal_connect(G_OBJECT(spectra_properties_linestyleW), "changed", G_CALLBACK(spectra_linestyle_changed_cb), (gpointer) spectra_properties_dataset_active);
		g_signal_connect(G_OBJECT(spectra_properties_colorW), "color-set", G_CALLBACK(spectra_color_changed_cb), (gpointer) spectra_properties_dataset_active);
		g_signal_connect(G_OBJECT(spectra_properties_widthW), "value-changed", G_CALLBACK(spectra_width_changed_cb), (gpointer) spectra_properties_dataset_active);

		gtk_dialog_run(GTK_DIALOG(spectra_propertiesW));
		gtk_widget_destroy(spectra_propertiesW);

	}

public:
	SpectraDataBox(
		const Glib::ustring &label,
		Gtk::PLplot::PlotData2D *_dataset_conv,
		Gtk::PLplot::PlotData2D *_dataset_unconv,
		enum XmiMsimGuiXmsoResultsScrolledWindowPlotMode _plot_mode) :
		Box(Gtk::ORIENTATION_HORIZONTAL, 3),
		dataset_conv(_dataset_conv),
		dataset_unconv(_dataset_unconv),
		checkButton(label),
		dialogButton("Properties"),
       		plot_mode(_plot_mode) {

		set_data("self", this); // for future use from within Gtk+ widgets...

		pack_start(checkButton, false, false, 1);
		pack_end(dialogButton, false, false, 1);

		checkButton.signal_toggled().connect([this](){
			Gtk::PLplot::PlotData2D *dataset;

			if (plot_mode == CONVOLUTED)
				dataset = dataset_conv;
			else if (plot_mode == UNCONVOLUTED)
				dataset = dataset_unconv;

			if (checkButton.get_active()) {
				dataset->show();
			}
			else{
				dataset->hide();
			}
			dialogButton.set_sensitive(checkButton.get_active());
		});
		dialogButton.signal_clicked().connect(sigc::mem_fun(*this, &SpectraDataBox::dialog_button_clicked));

		checkButton.set_active();
		checkButton.show();
		dialogButton.show();
	}

	void set_plot_mode(enum XmiMsimGuiXmsoResultsScrolledWindowPlotMode _plot_mode) {
		g_debug("Calling set_plot_mode");
		if (_plot_mode == plot_mode)
			return;
		if (!checkButton.get_active())
			return;
		plot_mode = _plot_mode;
		if (plot_mode == CONVOLUTED) {
			g_debug("Switching to convoluted");
			dataset_unconv->hide();
			dataset_conv->show();
		}
		else {
			g_debug("Switching to unconvoluted");
			dataset_conv->hide();
			dataset_unconv->show();
		}
	}
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

extern "C" struct _XmiMsimGuiXmsoResultsScrolledWindow {
	GtkScrolledWindow parent_instance;
	xmi_output *results;
	/*double plot_xmin;
	double plot_xmax;
	double plot_ymin_conv;
	double plot_ymin_unconv;
	double plot_ymax_conv;
	double plot_ymax_unconv;*/
	XmiMsimGuiXmsoResultsScrolledWindowPlotMode plot_mode;
	Gtk::PLplot::Canvas *canvas;
	GtkWidget *spectra_button_box;
	GtkWidget *x_coord_entry;
	GtkWidget *y_coord_entry;
	GtkWidget *channel_coord_entry;
	GtkWidget *settings_button;
	GtkWidget *export_button;
	GtkTreeStore *counts_tree_store;
	GtkWidget *counts_tree_view;
	gchar *xaxis_title;
	gchar *yaxis_title;
	GtkWidget *xaxis_title_entry;
	GtkWidget *yaxis_title_entry;
	std::vector<SpectraDataBox *> spectra_data_boxes;
};

extern "C" struct _XmiMsimGuiXmsoResultsScrolledWindowClass
{
	GtkScrolledWindowClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiXmsoResultsScrolledWindow, xmi_msim_gui_xmso_results_scrolled_window, GTK_TYPE_SCROLLED_WINDOW)

static void xmi_msim_gui_xmso_results_scrolled_window_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_xmso_results_scrolled_window_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_xmso_results_scrolled_window_finalize(GObject *gobject) {
	XmiMsimGuiXmsoResultsScrolledWindow *window = XMI_MSIM_GUI_XMSO_RESULTS_SCROLLED_WINDOW(gobject);
	if (window->results != NULL)
		xmi_output_free(window->results);

	G_OBJECT_CLASS(xmi_msim_gui_xmso_results_scrolled_window_parent_class)->finalize(gobject);
}

enum {
	FINISHED_LOADING,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

static void xmi_msim_gui_xmso_results_scrolled_window_class_init(XmiMsimGuiXmsoResultsScrolledWindowClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_xmso_results_scrolled_window_dispose;
	object_class->finalize = xmi_msim_gui_xmso_results_scrolled_window_finalize;

	signals[FINISHED_LOADING] = g_signal_new(
		"finished-loading",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		NULL,
		G_TYPE_NONE,
		1,
		G_TYPE_STRING // filename
	);
}

class Plot2D : public Gtk::PLplot::Plot2D {
	public:
	Plot2D(
		const Glib::ustring &axis_title_x,
		const Glib::ustring &axis_title_y) :
		Gtk::PLplot::Plot2D(axis_title_x, axis_title_y) {}
};

static void cell_print_double(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {

	gdouble value;
	gchar *double_text;
	gint depth;

	if (GPOINTER_TO_INT(data) == ENERGY_COLUMN) {
		depth = gtk_tree_store_iter_depth(GTK_TREE_STORE(tree_model), iter);
		if (depth != 1) {
			g_object_set(G_OBJECT(renderer), "visible", FALSE, NULL);
			return;
		}
		else
			g_object_set(G_OBJECT(renderer), "visible", TRUE, NULL);
	}


	gtk_tree_model_get(tree_model, iter, GPOINTER_TO_INT(data), &value, -1);

	double_text = g_strdup_printf("%lg",value);
	g_object_set(G_OBJECT(renderer), "text", double_text, NULL);

	g_free(double_text);

	return;
}

static void cell_active_toggle(GtkCellRendererToggle *cell_renderer, gchar *path, GtkTreeStore *counts_tree_store) {
	GtkTreeIter iter, parent, child;
	GtkTreePath *tree_path = gtk_tree_path_new_from_string(path);
	gboolean toggled, inconsistent, toggled2;
	gint depth;

	Gtk::PLplot::PlotObject2DLine *line;

	gtk_tree_model_get_iter(GTK_TREE_MODEL(counts_tree_store), &iter, tree_path);
	depth = gtk_tree_store_iter_depth(counts_tree_store, &iter);
	gtk_tree_model_get(GTK_TREE_MODEL(counts_tree_store), &iter, SHOW_LINE_COLUMN, &toggled, -1);


	//if depth == 0, then check consistency first!!!
	if (depth == 0) {
		g_object_get(G_OBJECT(cell_renderer), "inconsistent", &inconsistent, NULL);
		if (inconsistent) {
			gtk_tree_store_set(counts_tree_store, &iter, CONSISTENT_COLUMN, TRUE, SHOW_LINE_COLUMN, TRUE,-1);
			//set all children TRUE
			gtk_tree_model_iter_children(GTK_TREE_MODEL(counts_tree_store), &child, &iter);
			do {
				gtk_tree_model_get(GTK_TREE_MODEL(counts_tree_store), &child, CHILD_COLUMN, &line, -1);
				line->show();
				gtk_tree_store_set(counts_tree_store, &child, SHOW_LINE_COLUMN, TRUE, -1);
			} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(counts_tree_store),&child));
		}
		else {
			if (toggled) {
				gtk_tree_store_set(counts_tree_store, &iter, SHOW_LINE_COLUMN, FALSE, -1);
				//set all children to FALSE
				gtk_tree_model_iter_children(GTK_TREE_MODEL(counts_tree_store), &child, &iter);
				do {
					gtk_tree_model_get(GTK_TREE_MODEL(counts_tree_store), &child, CHILD_COLUMN, &line, -1);
					line->hide();
					gtk_tree_store_set(counts_tree_store, &child, SHOW_LINE_COLUMN, FALSE, -1);
				} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(counts_tree_store),&child));
			}
			else {
				gtk_tree_store_set(counts_tree_store, &iter, SHOW_LINE_COLUMN, TRUE, -1);
				//set all children to TRUE
				gtk_tree_model_iter_children(GTK_TREE_MODEL(counts_tree_store), &child, &iter);
				do {
					gtk_tree_model_get(GTK_TREE_MODEL(counts_tree_store), &child, CHILD_COLUMN, &line, -1);
					line->show();
					gtk_tree_store_set(counts_tree_store, &child, SHOW_LINE_COLUMN, TRUE, -1);
				} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(counts_tree_store),&child));
			}
		}


	}
	else {
		if (toggled == TRUE)
			toggled = FALSE;
		else
			toggled = TRUE;

		gtk_tree_store_set(counts_tree_store, &iter, SHOW_LINE_COLUMN, toggled, -1);
		//set parent as inconsistent if necessary!!
		gtk_tree_model_iter_parent(GTK_TREE_MODEL(counts_tree_store), &parent, &iter);
		//check all children
		gint n_children=gtk_tree_model_iter_n_children(GTK_TREE_MODEL(counts_tree_store),&parent);
		gint toggle_sum=0;
		gtk_tree_model_iter_children(GTK_TREE_MODEL(counts_tree_store), &child, &parent);
		do {
			gtk_tree_model_get(GTK_TREE_MODEL(counts_tree_store), &child, SHOW_LINE_COLUMN, &toggled2, -1);
			toggle_sum += toggled2;
		} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(counts_tree_store),&child));



		if (toggle_sum == 0) {
			gtk_tree_store_set(counts_tree_store, &parent, CONSISTENT_COLUMN, TRUE, SHOW_LINE_COLUMN, FALSE, -1);
		}
		else if (toggle_sum == n_children) {
			gtk_tree_store_set(counts_tree_store, &parent, CONSISTENT_COLUMN, TRUE, SHOW_LINE_COLUMN, TRUE, -1);
		}
		else
			gtk_tree_store_set(counts_tree_store, &parent, CONSISTENT_COLUMN, FALSE, -1);

		//draw or remove line
		gtk_tree_model_get(GTK_TREE_MODEL(counts_tree_store), &iter, CHILD_COLUMN, &line, -1);
		if (toggled) {
			line->show();
		}
		else {
			line->hide();
		}

	}

	gtk_tree_path_free(tree_path);

	return;
}

static void cell_visible_toggle(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gint depth;
	gboolean show_line, consistent;

	depth = gtk_tree_store_iter_depth(GTK_TREE_STORE(tree_model), iter);

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

static void axis_title_changed_cb(XmiMsimGuiXmsoResultsScrolledWindow *self, GtkEntry *axis_title_entry) {

	const gchar *new_title = gtk_entry_get_text(GTK_ENTRY(axis_title_entry));

	if (axis_title_entry == GTK_ENTRY(self->xaxis_title_entry)) {
		//X-axis
		self->canvas->get_plot(0)->set_axis_title_x(new_title);
		g_free(self->xaxis_title);
		self->xaxis_title = g_strdup(new_title);
	}
	else if (axis_title_entry == GTK_ENTRY(self->yaxis_title_entry)) {
		//Y-axis
		self->canvas->get_plot(0)->set_axis_title_y(new_title);
		g_free(self->yaxis_title);
		self->yaxis_title = g_strdup(new_title);
	}
}

static void scale_combo_changed_cb(XmiMsimGuiXmsoResultsScrolledWindow *self, GtkComboBox *widget) {
	if (gtk_combo_box_get_active(GTK_COMBO_BOX(widget)) == 0) {
		dynamic_cast<Gtk::PLplot::Plot2D*>(self->canvas->get_plot(0))->set_axis_logarithmic_y(false);
	}
	else {
		dynamic_cast<Gtk::PLplot::Plot2D*>(self->canvas->get_plot(0))->set_axis_logarithmic_y(true);
	}
}

static void radio_conv_changed_cb(XmiMsimGuiXmsoResultsScrolledWindow *self, GtkToggleButton *button) {
	int i;
	if (gtk_toggle_button_get_active(button) == TRUE) {
		self->plot_mode = CONVOLUTED;
	}
	else {
		self->plot_mode = UNCONVOLUTED;
	}

	for (SpectraDataBox *box : self->spectra_data_boxes) {
		box->set_plot_mode(self->plot_mode);
	}
}

static void export_button_clicked_cb(XmiMsimGuiXmsoResultsScrolledWindow *self, gpointer data) {
	XmiMsimGuiFileChooserDialog *dialog = xmi_msim_gui_export_canvas_dialog_new("Export spectra", GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(self))));

	gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(dialog), g_path_get_dirname(self->results->input->general->outputfile));

	gchar *basename = g_path_get_basename(self->results->input->general->outputfile);

	if (g_ascii_strcasecmp(basename + strlen(basename) - 5, ".xmso") == 0)
		strcpy(basename + strlen(basename) - 5, ".eps");

	gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(dialog), basename);
	g_free(basename);

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		GError *error = NULL;
		if (!xmi_msim_gui_export_canvas_dialog_save(dialog, self->canvas, &error)) {
			xmi_msim_gui_file_chooser_dialog_destroy(dialog);
			GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(self))), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error exporting spectrum");
			gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "%s", error->message);

			gtk_dialog_run(GTK_DIALOG(info_dialog));
			gtk_widget_destroy(info_dialog);

			g_error_free(error);
			return;
		}
	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);

	return;
}

static void settings_button_clicked_cb(XmiMsimGuiXmsoResultsScrolledWindow *self, gpointer data) {
	//For now: lin/log, and titles
	GtkWidget *dialog= gtk_dialog_new_with_buttons("Plot properties", GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(self))), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), "_Close", GTK_RESPONSE_ACCEPT,NULL);
	GtkWidget *vbox = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *scale_combo;
	scale_combo = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(scale_combo),"Linear");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(scale_combo),"Logarithmic");

	Gtk::PLplot::Plot2D *plot = dynamic_cast<Gtk::PLplot::Plot2D *>(self->canvas->get_plot(0));
	bool current_scale_log10 = plot->get_axis_logarithmic_y();

	if (current_scale_log10 == false) {
		gtk_combo_box_set_active(GTK_COMBO_BOX(scale_combo), 0);
	}
	else if (current_scale_log10 == true) {
		gtk_combo_box_set_active(GTK_COMBO_BOX(scale_combo), 1);
	}

	GtkWidget *lilHBox, *label;
	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	label = gtk_label_new("Y-axis scale");
	gtk_box_pack_start(GTK_BOX(lilHBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(lilHBox), scale_combo, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), lilHBox, FALSE, FALSE,2);
	gtk_box_pack_start(GTK_BOX(vbox), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 2);
	GtkWidget *radio_conv = gtk_radio_button_new_with_label_from_widget(NULL, "Use convoluted spectra");
	GtkWidget *radio_unconv= gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(radio_conv), "Use unconvoluted spectra");
	gtk_box_pack_start(GTK_BOX(vbox), radio_conv, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), radio_unconv, FALSE, FALSE, 2);

	if (self->plot_mode == CONVOLUTED) {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio_conv), TRUE);
	}
	else if (self->plot_mode == UNCONVOLUTED) {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio_unconv), TRUE);
	}
	gtk_box_pack_start(GTK_BOX(vbox), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	label = gtk_label_new("X-axis title");
	gtk_box_pack_start(GTK_BOX(lilHBox), label, FALSE, FALSE, 2);
	self->xaxis_title_entry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->xaxis_title_entry), TRUE);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->xaxis_title_entry, TRUE, TRUE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), lilHBox, FALSE, FALSE, 2);
	gtk_entry_set_text(GTK_ENTRY(self->xaxis_title_entry), self->xaxis_title);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	label = gtk_label_new("Y-axis title");
	gtk_box_pack_start(GTK_BOX(lilHBox), label, FALSE, FALSE, 2);
	self->yaxis_title_entry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->yaxis_title_entry), TRUE);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->yaxis_title_entry, TRUE, TRUE, 2);
	gtk_box_pack_start(GTK_BOX(vbox),lilHBox, FALSE, FALSE, 2);
	gtk_entry_set_text(GTK_ENTRY(self->yaxis_title_entry), self->yaxis_title);

	gtk_widget_set_size_request(dialog, 350,-1);
	gtk_container_set_border_width(GTK_CONTAINER(dialog), 5);
	g_signal_connect_swapped(G_OBJECT(self->xaxis_title_entry), "changed", G_CALLBACK(axis_title_changed_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->yaxis_title_entry), "changed", G_CALLBACK(axis_title_changed_cb), self);
	g_signal_connect_swapped(G_OBJECT(scale_combo), "changed", G_CALLBACK(scale_combo_changed_cb), self);
	g_signal_connect_swapped(G_OBJECT(radio_conv), "toggled", G_CALLBACK(radio_conv_changed_cb), self);

	gtk_widget_show_all(vbox);

	gtk_dialog_run(GTK_DIALOG(dialog));
	gtk_widget_destroy(dialog);

	return;
}

static void xmi_msim_gui_xmso_results_scrolled_window_init(XmiMsimGuiXmsoResultsScrolledWindow *self) {

	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);

	self->plot_mode = CONVOLUTED;
	self->xaxis_title = g_strdup("Energy (keV)");
	self->yaxis_title = g_strdup("Intensity (counts/channel)");

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

	GtkWidget *grid = gtk_grid_new();
	gtk_grid_set_row_spacing(GTK_GRID(grid), 2);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	self->spectra_button_box = gtk_list_box_new();
	gtk_container_add(GTK_CONTAINER(scrolled_window), self->spectra_button_box);
	GtkWidget *frame = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(frame), scrolled_window);
	gtk_grid_attach(GTK_GRID(grid), frame, 0, 0, 2, 1);
	gtk_widget_set_vexpand(frame, TRUE);
	gtk_widget_set_valign(frame, GTK_ALIGN_FILL);
	gtk_box_pack_start(GTK_BOX(top_paned), grid, FALSE, FALSE, 0);
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

	self->settings_button = gtk_button_new_with_label("Properties");
	g_signal_connect_swapped(G_OBJECT(self->settings_button), "clicked", G_CALLBACK(settings_button_clicked_cb), (gpointer) self);
	gtk_grid_attach(GTK_GRID(grid), self->settings_button, 0, 1, 2, 1);
	gtk_widget_set_sensitive(self->settings_button, FALSE);
	self->export_button = gtk_button_new_with_label("Export plot");
	g_signal_connect_swapped(G_OBJECT(self->export_button), "clicked", G_CALLBACK(export_button_clicked_cb), (gpointer) self);
	gtk_grid_attach(GTK_GRID(grid), self->export_button, 0, 2, 2, 1);
	gtk_widget_set_sensitive(self->export_button, FALSE);
	label = gtk_label_new("Energy (keV)");
	gtk_grid_attach(GTK_GRID(grid), label, 0, 3, 1, 1);
	self->x_coord_entry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->x_coord_entry), FALSE);
	gtk_grid_attach(GTK_GRID(grid), self->x_coord_entry, 1, 3, 1, 1);
	label = gtk_label_new("Channel number");
	gtk_grid_attach(GTK_GRID(grid), label, 0, 4, 1, 1);
	self->channel_coord_entry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->channel_coord_entry), FALSE);
	gtk_grid_attach(GTK_GRID(grid), self->channel_coord_entry, 1, 4, 1, 1);
	label = gtk_label_new("Intensity");
	gtk_grid_attach(GTK_GRID(grid), label, 0, 5, 1, 1);
	self->y_coord_entry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->y_coord_entry), FALSE);
	gtk_grid_attach(GTK_GRID(grid), self->y_coord_entry, 1, 5, 1, 1);

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
	gtk_tree_view_column_set_cell_data_func(column, renderer, cell_print_double, GINT_TO_POINTER(ENERGY_COLUMN), NULL);

	renderer = gtk_cell_renderer_toggle_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	gtk_cell_renderer_toggle_set_radio(GTK_CELL_RENDERER_TOGGLE(renderer),FALSE);
	gtk_cell_renderer_toggle_set_activatable(GTK_CELL_RENDERER_TOGGLE(renderer),TRUE);
	g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(cell_active_toggle), self->counts_tree_store);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column,"Show line");
	gtk_tree_view_append_column(GTK_TREE_VIEW(self->counts_tree_view), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, cell_visible_toggle, NULL, NULL);

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
	gtk_tree_view_column_set_cell_data_func(column, renderer, cell_print_double, GINT_TO_POINTER(COUNTS_COLUMN), NULL);

	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), self->counts_tree_view);

	frame = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(frame), scrolled_window);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
	gtk_widget_set_size_request(frame, -1, 150);
	gtk_widget_show_all(frame);

	gtk_paned_pack2(GTK_PANED(paned), frame, TRUE, FALSE);
	gtk_widget_show_all(GTK_WIDGET(self));
}

GtkWidget* xmi_msim_gui_xmso_results_scrolled_window_new(void) {

	XmiMsimGuiXmsoResultsScrolledWindow *scrolled_window = XMI_MSIM_GUI_XMSO_RESULTS_SCROLLED_WINDOW(g_object_new(XMI_MSIM_GUI_TYPE_XMSO_RESULTS_SCROLLED_WINDOW, NULL));

	return GTK_WIDGET(scrolled_window);
}

gboolean xmi_msim_gui_xmso_results_scrolled_window_load_from_file(XmiMsimGuiXmsoResultsScrolledWindow *window, const gchar *xmsofile, GError **error) {

	xmi_output *output;

	// before doing anything else, ensure that the file can be read properly
	if ((output = xmi_output_read_from_xml_file(xmsofile, error)) == NULL)
		return FALSE;

	// if we get here, then the file could be read...
	if (window->results != NULL)
		xmi_output_free(window->results);

	window->results = output;
	
	// clear list box
	for (auto *box : window->spectra_data_boxes) {
		delete box;
	}
	window->spectra_data_boxes.clear();

	// the initial label may still be there -> remove that one too.
	GList *children, *iter;
	children = gtk_container_get_children(GTK_CONTAINER(window->spectra_button_box));
	for (iter = children ; iter != NULL ; iter = g_list_next(iter)) {
		gtk_widget_destroy(GTK_WIDGET(iter->data));
	}
	g_list_free(children);

	//clear tree
	gtk_tree_store_clear(window->counts_tree_store);

	//clear plot
	try {
		// this will also clear all datasets and objects on the plot since they are managed by Gtkmm::PLplot
		window->canvas->remove_plot(0);
	}
	catch (Gtk::PLplot::Exception &e) {
		//this will fail if there's no plot available
	}

	double *temp_energies = (double *) g_malloc(sizeof(double) * window->results->input->detector->nchannels);
	for (int i = 0 ; i < window->results->input->detector->nchannels ; i++) {
		temp_energies[i] = window->results->input->detector->gain * i + window->results->input->detector->zero;
	}

	Plot2D *plot_window = Gtk::manage(new Plot2D(window->xaxis_title, window->yaxis_title));
	plot_window->hide();
	plot_window->hide_legend();
	plot_window->set_axis_logarithmic_y(true);  // make this persistent?
	plot_window->set_box_style(Gtk::PLplot::BoxStyle::BOX_TICKS_TICK_LABELS_MAIN_AXES_MAJOR_TICK_GRID);
	plot_window->signal_cursor_motion().connect([window](double x, double y){
		gchar *buffer = g_strdup_printf("%lg", x);
		gtk_entry_set_text(GTK_ENTRY(window->x_coord_entry), buffer);
		g_free(buffer);
		buffer = g_strdup_printf("%i",(int) ((x - window->results->input->detector->zero)/window->results->input->detector->gain));
		gtk_entry_set_text(GTK_ENTRY(window->channel_coord_entry), buffer);
		g_free(buffer);
		buffer = g_strdup_printf("%lg",y);
		gtk_entry_set_text(GTK_ENTRY(window->y_coord_entry), buffer);
		g_free(buffer);
        });
	window->canvas->add_plot(*plot_window);

	for (int i = (window->results->use_zero_interactions ? 0 : 1) ; i <= window->results->ninteractions ; i++) {

		double *temp_channels = (double *) g_malloc(sizeof(double) * window->results->input->detector->nchannels);
		for (int j = 0 ; j < window->results->input->detector->nchannels ; j++)
			temp_channels[j] = window->results->channels_conv[i][j];

		std::vector<double> x_vals(temp_energies, temp_energies + window->results->input->detector->nchannels);
		std::vector<double> y_vals(temp_channels, temp_channels + window->results->input->detector->nchannels);
		g_free(temp_channels);
		std::for_each(std::begin(y_vals), std::end(y_vals), [](double &a) { if (a < 1.0 ) a = 1.0;});

		auto dataset_conv = Gtk::manage(
			new Gtk::PLplot::PlotData2D(
				x_vals,
				y_vals
			)
		);
		dataset_conv->hide();
		plot_window->add_data(*dataset_conv);

		temp_channels = (double *) g_malloc(sizeof(double) * window->results->input->detector->nchannels);
		for (int j = 0 ; j < window->results->input->detector->nchannels ; j++)
			temp_channels[j] = window->results->channels_unconv[i][j];

		std::vector<double> x_vals2(temp_energies, temp_energies + window->results->input->detector->nchannels);
		std::vector<double> y_vals2(temp_channels, temp_channels + window->results->input->detector->nchannels);
		g_free(temp_channels);
		std::for_each(std::begin(y_vals2), std::end(y_vals2), [](double &a) { if (a < 1.0 ) a = 1.0;});
		auto dataset_unconv = Gtk::manage(
			new Gtk::PLplot::PlotData2D(
				x_vals2,
				y_vals2
			)
		);
		dataset_unconv->hide();
		plot_window->add_data(*dataset_unconv);

		GdkRGBA color_plot;
		gchar *buffer;

		switch (i - (window->results->use_zero_interactions ? 0 : 1)) {
			case 0:
				color_plot = blue_plot;
				if (window->results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions", 0);
				}
				else {
					buffer = g_strdup_printf("%i interaction", 1);
				}
				break;
			case 1:
				color_plot = red_plot;
				if (window->results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interaction", 1);
				}
				else {
					buffer = g_strdup_printf("%i interactions", 2);
				}
				break;
			case 2:
				color_plot = green_plot;
				if (window->results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions", 2);
				}
				else {
					buffer = g_strdup_printf("%i interactions", 3);
				}
				break;
			case 3:
				color_plot = purple_plot;
				if (window->results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions", 3);
				}
				else {
					buffer = g_strdup_printf("%i interactions", 4);
				}
				break;
			case 4:
				color_plot = yellow_plot;
				if (window->results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions", 4);
				}
				else {
					buffer = g_strdup_printf("%i interactions", 5);
				}
				break;
			case 5:
				color_plot = pink_plot;
				if (window->results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions", 5);
				}
				else {
					buffer = g_strdup_printf("%i interactions", 6);
				}
				break;
			default:
				color_plot = black_plot;
				if (window->results->use_zero_interactions == 1) {
					buffer = g_strdup_printf("%i interactions", i);
				}
				else {
					buffer = g_strdup_printf("%i interactions", i+1);
				}
				break;
		}
		dataset_conv->set_color(Gdk::RGBA(&color_plot));
		dataset_unconv->set_color(Gdk::RGBA(&color_plot));
		if (window->plot_mode == CONVOLUTED) {
			dataset_conv->show();
		}
		else {
			dataset_unconv->show();
		}
		SpectraDataBox *box = new SpectraDataBox(Glib::ustring(buffer), dataset_conv, dataset_unconv, window->plot_mode);
		GtkWidget *list_box_row = gtk_list_box_row_new();
		gtk_container_add(GTK_CONTAINER(list_box_row), GTK_WIDGET(box->gobj()));
#if GTK_CHECK_VERSION(3, 14, 0)
		gtk_list_box_row_set_activatable(GTK_LIST_BOX_ROW(list_box_row), FALSE);
		gtk_list_box_row_set_selectable(GTK_LIST_BOX_ROW(list_box_row), FALSE);
#endif
		gtk_list_box_insert(GTK_LIST_BOX(window->spectra_button_box), list_box_row, -1);
		window->spectra_data_boxes.push_back(box);
		g_free(buffer);
	}
	gtk_widget_set_sensitive(window->export_button,TRUE);
	gtk_widget_set_sensitive(window->settings_button,TRUE);

	gtk_widget_show_all(window->spectra_button_box);

	plot_window->show();

	//treestore stuff
	GtkTreeIter iter1, iter2, iter3;

	if (window->results->brute_force_history != NULL) {
		//brute force mode
		for (int i = 0 ; i < window->results->nbrute_force_history ; i++) {
			//iterating over atomic numbers -> highest level
			gtk_tree_store_append(window->counts_tree_store, &iter1, NULL);
			gchar *symbol = AtomicNumberToSymbol(window->results->brute_force_history[i].atomic_number);
			gtk_tree_store_set(window->counts_tree_store, &iter1,
				ELEMENT_COLUMN, symbol,
				LINE_COLUMN , "all",
				SHOW_LINE_COLUMN, FALSE,
				CONSISTENT_COLUMN, TRUE,
				INTERACTION_COLUMN, "all",
				COUNTS_COLUMN, window->results->brute_force_history[i].total_counts,
				-1);
			xrlFree(symbol);
			for (int j = 0 ; j < window->results->brute_force_history[i].n_lines ; j++) {
				auto line = Gtk::manage(
					new Gtk::PLplot::PlotObject2DLine(
						Gtk::ORIENTATION_VERTICAL,
						window->results->brute_force_history[i].lines[j].energy
					)
				);
				line->hide();
				plot_window->add_object(*line);

				gtk_tree_store_append(window->counts_tree_store, &iter2, &iter1);
				gtk_tree_store_set(window->counts_tree_store, &iter2,
					LINE_COLUMN, window->results->brute_force_history[i].lines[j].line_type,
					ENERGY_COLUMN, window->results->brute_force_history[i].lines[j].energy,
					SHOW_LINE_COLUMN, FALSE,
					CONSISTENT_COLUMN, TRUE,
					INTERACTION_COLUMN, "all",
					CHILD_COLUMN, line,
					COUNTS_COLUMN, window->results->brute_force_history[i].lines[j].total_counts,
					-1);



				for (int k = 0 ; k < window->results->brute_force_history[i].lines[j].n_interactions ; k++) {
					gtk_tree_store_append(window->counts_tree_store, &iter3, &iter2);
					gchar *txt = g_strdup_printf("%i",window->results->brute_force_history[i].lines[j].interactions[k].interaction_number);
					gtk_tree_store_set(window->counts_tree_store, &iter3,
						INTERACTION_COLUMN,txt,
						COUNTS_COLUMN, window->results->brute_force_history[i].lines[j].interactions[k].counts,
						-1);
					g_free(txt);
				}
			}
		}
	}
	else if (window->results->var_red_history != NULL) {
		//variance reduction mode

		for (int i = 0 ; i < window->results->nvar_red_history ; i++) {
			//iterating over atomic numbers -> highest level
			gtk_tree_store_append(window->counts_tree_store, &iter1, NULL);
			gchar *symbol = AtomicNumberToSymbol(window->results->var_red_history[i].atomic_number);
			gtk_tree_store_set(window->counts_tree_store, &iter1,
				ELEMENT_COLUMN, symbol,
				LINE_COLUMN , "all",
				SHOW_LINE_COLUMN, FALSE,
				CONSISTENT_COLUMN, TRUE,
				INTERACTION_COLUMN, "all",
				COUNTS_COLUMN, window->results->var_red_history[i].total_counts,
				-1);
			xrlFree(symbol);
			for (int j = 0 ; j < window->results->var_red_history[i].n_lines ; j++) {
				auto line = Gtk::manage(
					new Gtk::PLplot::PlotObject2DLine(
						Gtk::ORIENTATION_VERTICAL,
						window->results->var_red_history[i].lines[j].energy
					)
				);
				line->hide();
				plot_window->add_object(*line);

				gtk_tree_store_append(window->counts_tree_store, &iter2, &iter1);
				gtk_tree_store_set(window->counts_tree_store, &iter2,
					LINE_COLUMN, window->results->var_red_history[i].lines[j].line_type,
					ENERGY_COLUMN, window->results->var_red_history[i].lines[j].energy,
					SHOW_LINE_COLUMN, FALSE,
					CONSISTENT_COLUMN, TRUE,
					INTERACTION_COLUMN, "all",
					CHILD_COLUMN, line,
					COUNTS_COLUMN, window->results->var_red_history[i].lines[j].total_counts,
					-1);


				for (int k = 0 ; k < window->results->var_red_history[i].lines[j].n_interactions ; k++) {
					gtk_tree_store_append(window->counts_tree_store, &iter3, &iter2);
					gchar *txt = g_strdup_printf("%i",window->results->var_red_history[i].lines[j].interactions[k].interaction_number);
					gtk_tree_store_set(window->counts_tree_store, &iter3,
						INTERACTION_COLUMN,txt,
						COUNTS_COLUMN, window->results->var_red_history[i].lines[j].interactions[k].counts,
						-1);
					g_free(txt);
				}
			}
		}
	}
	else {
		// no history is actually quite plausible and should not be treated as an error at all
	}

	g_signal_emit(window, signals[FINISHED_LOADING], 0, xmsofile);

	return TRUE;
}

const gchar* xmi_msim_gui_xmso_results_scrolled_window_get_filename(XmiMsimGuiXmsoResultsScrolledWindow *window) {
	if (window->results == NULL) 
		return NULL;

	return window->results->outputfile;
}
