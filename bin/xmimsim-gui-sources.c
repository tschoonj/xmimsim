/*
Copyright (C) 2010-2014 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-sources.h"
#include "xmimsim-gui-spline.h"
#ifdef HAVE_CXX
  #include <gtkmm-plplot.h>
#else
  #include <gtkextra/gtkextra.h>
#endif
#include <xraylib.h>
#include <cairo-pdf.h>
#include <cairo-ps.h>
#ifdef CAIRO_HAS_SVG_SURFACE
  #include <cairo-svg.h>
#endif
#include "xmi_aux.h"
#include "xmi_private.h"
#include "xmi_ebel.h"
#include "xmimsim-gui-results.h"
#include "xmimsim-gui-energies.h"
#include "xmimsim-gui-fonts.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-export-canvas-dialog.h"
#include <math.h>
#include <string.h>
#include <glib/gstdio.h>

const gchar *activity_units[4] = {"mCi", "Ci", "GBq", "Bq"};

#ifdef HAVE_CXX
class Plot2DSources;
#endif

struct generate {
	struct xmi_excitation *excitation_tube;
	struct xmi_excitation *excitation_nuclide;
	GtkWidget *tubeVoltageW;
	GtkWidget *transmissionW;
	GtkWidget *anodeMaterialW;
	GtkWidget *anodeThicknessW;
	GtkWidget *anodeDensityW;
	GtkWidget *filterMaterialW;
	GtkWidget *filterThicknessW;
	GtkWidget *filterDensityW;
	GtkWidget *windowMaterialW;
	GtkWidget *windowThicknessW;
	GtkWidget *windowDensityW;
	GtkWidget *alphaElectronW;
	GtkWidget *alphaXrayW;
	GtkWidget *deltaEnergyW;
	GtkWidget *tubeCurrentW;
	GtkWidget *tubeSolidAngleW;
	GtkWidget *transmissionEffW;
	GtkWidget *transmissionEffFileW;
#ifdef HAVE_CXX
	Gtk::PLplot::Canvas *canvas_tube;
	Plot2DSources *plot_window_tube;
#else
	GtkWidget *canvas_tube;
	GtkWidget *plot_window_tube;
#endif
	GtkWidget *linear_tubeW;
	GtkWidget *log10_tubeW;
	double plot_xmin_tube;
	double plot_xmax_tube;
	double plot_ymax_tube;
	double plot_ymin_tube;
	double plot_ymin_lin_tube;
	double plot_ymin_log10_tube;
#ifdef HAVE_CXX
	Gtk::PLplot::Canvas *canvas_nuclide;
	Plot2DSources *plot_window_nuclide;
#else
	GtkWidget *canvas_nuclide;
	GtkWidget *plot_window_nuclide;
#endif
	GtkWidget *linear_nuclideW;
	GtkWidget *log10_nuclideW;
	double plot_xmin_nuclide;
	double plot_xmax_nuclide;
	double plot_ymax_nuclide;
	double plot_ymin_nuclide;
	double plot_ymin_lin_nuclide;
	double plot_ymin_log10_nuclide;
	GtkWidget *notebook;
	GtkWidget *radioNuclideW;
	GtkWidget *activityW;
	GtkWidget *activityUnitW;
	GtkWidget *nuclideSolidAngleW;
	GtkWidget *window;
};

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


static struct xmi_nuclide_parameters *get_nuclide_parameters(struct generate *gen) {
	struct xmi_nuclide_parameters *xnp = (struct xmi_nuclide_parameters *) g_malloc(sizeof(struct xmi_nuclide_parameters));

	xnp->radioNuclide = gtk_combo_box_get_active(GTK_COMBO_BOX(gen->radioNuclideW));
	xnp->activityUnit = gtk_combo_box_get_active(GTK_COMBO_BOX(gen->activityUnitW));
	xnp->log10_active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gen->log10_nuclideW));

	const gchar *text = gtk_entry_get_text(GTK_ENTRY(gen->activityW));
	xnp->activity = strtod(text, NULL);
	if (xnp->activity <= 0.0) {
		g_fprintf(stderr, "Warning: invalid activity in get_nuclide_parameters\n");
		g_free(xnp);
		return NULL;
	}
	text = gtk_entry_get_text(GTK_ENTRY(gen->nuclideSolidAngleW));
	xnp->nuclide_solid_angle = strtod(text, NULL);
	if (xnp->nuclide_solid_angle <= 0.0 || xnp->nuclide_solid_angle > 4.0*M_PI) {
		g_fprintf(stderr, "Warning: invalid solid angle in get_nuclide_parameters\n");
		g_free(xnp);
		return NULL;
	}

	return xnp;
}

static struct xmi_ebel_parameters *get_ebel_parameters(struct generate *gen) {
	struct xmi_ebel_parameters *xep = (struct xmi_ebel_parameters *) g_malloc(sizeof(struct xmi_ebel_parameters));

	xep->tube_voltage = gtk_spin_button_get_value(GTK_SPIN_BUTTON(gen->tubeVoltageW));
	xep->transmission_tube = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gen->transmissionW));
	xep->alpha= gtk_spin_button_get_value(GTK_SPIN_BUTTON(gen->alphaElectronW));
	xep->beta = gtk_spin_button_get_value(GTK_SPIN_BUTTON(gen->alphaXrayW));
	xep->tube_current = gtk_spin_button_get_value(GTK_SPIN_BUTTON(gen->tubeCurrentW));
	const gchar *text = gtk_entry_get_text(GTK_ENTRY(gen->tubeSolidAngleW));
	xep->tube_solid_angle = strtod(text, NULL);
	if (xep->tube_solid_angle <= 0.0) {
		g_fprintf(stderr, "Warning: invalid tube_solid_angle in get_ebel_parameters\n");
		g_free(xep);
		return NULL;
	}
	xep->interval_width = gtk_spin_button_get_value(GTK_SPIN_BUTTON(gen->deltaEnergyW));
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gen->transmissionEffW)) == TRUE) {
		xep->transmission_efficiency_file = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(gen->transmissionEffFileW));
		if (xep->transmission_efficiency_file == NULL)
			xep->transmission_efficiency_file = g_strdup("(None)");
	}
	else {
		xep->transmission_efficiency_file = g_strdup("(None)");
	}
	char *endPtr;
	xep->anode_Z = gtk_combo_box_get_active(GTK_COMBO_BOX(gen->anodeMaterialW))+1;
	text = gtk_entry_get_text(GTK_ENTRY(gen->anodeThicknessW));
	xep->anode_thickness = strtod(text, &endPtr);
	if (xep->anode_thickness <= 0.0) {
		g_fprintf(stderr, "Warning: invalid anode_thickness in get_ebel_parameters\n");
		g_free(xep);
		return NULL;
	}
	text = gtk_entry_get_text(GTK_ENTRY(gen->anodeDensityW));
	xep->anode_rho = strtod(text, &endPtr);
	if (xep->anode_rho <= 0.0) {
		g_fprintf(stderr, "Warning: invalid anode_rho in get_ebel_parameters\n");
		g_free(xep);
		return NULL;
	}
	xep->window_Z = gtk_combo_box_get_active(GTK_COMBO_BOX(gen->windowMaterialW))+1;
	text = gtk_entry_get_text(GTK_ENTRY(gen->windowThicknessW));
	xep->window_thickness = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->window_thickness < 0.0) {
		g_fprintf(stderr, "Warning: invalid window_thickness in get_ebel_parameters\n");
		g_free(xep);
		return NULL;
	}
	text = gtk_entry_get_text(GTK_ENTRY(gen->windowDensityW));
	xep->window_rho = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->window_rho < 0.0) {
		g_fprintf(stderr, "Warning: invalid window_rho in get_ebel_parameters\n");
		g_free(xep);
		return NULL;
	}
	xep->filter_Z = gtk_combo_box_get_active(GTK_COMBO_BOX(gen->filterMaterialW))+1;
	text = gtk_entry_get_text(GTK_ENTRY(gen->filterThicknessW));
	xep->filter_thickness = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->filter_thickness < 0.0) {
		g_fprintf(stderr, "Warning: invalid filter_thickness in get_ebel_parameters\n");
		g_free(xep);
		return NULL;
	}
	text = gtk_entry_get_text(GTK_ENTRY(gen->filterDensityW));
	xep->filter_rho = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->filter_rho < 0.0) {
		g_fprintf(stderr, "Warning: invalid filter_rho in get_ebel_parameters\n");
		g_free(xep);
		return NULL;
	}
	xep->log10_active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gen->log10_tubeW));
	return xep;
}

static void material_changed_cb(GtkComboBox *widget, GtkWidget *densityW) {
	int Z = gtk_combo_box_get_active(widget)+1;
	float density = ElementDensity(Z);
	char buffer[512];

	sprintf(buffer,"%g", density);
	gtk_entry_set_text(GTK_ENTRY(densityW), buffer);
}


struct transmission_data {
	GtkWidget *anodeDensityW;
	GtkWidget *anodeThicknessW;
};

static void transmission_clicked_cb(GtkToggleButton *button, struct transmission_data *td) {
	if (gtk_toggle_button_get_active(button)) {
		gtk_widget_set_sensitive(td->anodeDensityW, TRUE);
		gtk_widget_set_sensitive(td->anodeThicknessW, TRUE);
	}
	else {
		gtk_widget_set_sensitive(td->anodeDensityW, FALSE);
		gtk_widget_set_sensitive(td->anodeThicknessW, FALSE);
	}

	return;
}

static void linear_log10_toggled_cb(GtkToggleButton *button, struct generate *gen) {
#ifdef HAVE_CXX
	Plot2DSources *plot_window;
	Gtk::PLplot::Canvas *canvas;
#else
	GtkWidget *plot_window = NULL, *canvas = NULL;
#endif
	double plot_xmin, plot_xmax, plot_ymax, plot_ymin_lin, plot_ymin_log10;
	if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 0) {
		plot_xmin = gen->plot_xmin_tube;
		plot_xmax = gen->plot_xmax_tube;
		plot_ymax = gen->plot_ymax_tube;
		plot_ymin_lin = gen->plot_ymin_lin_tube;
		plot_ymin_log10 = gen->plot_ymin_log10_tube;
		plot_window = gen->plot_window_tube;
		canvas = gen->canvas_tube;
	}
	else if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 1) {
		plot_xmin = gen->plot_xmin_nuclide;
		plot_xmax = gen->plot_xmax_nuclide;
		plot_ymax = gen->plot_ymax_nuclide;
		plot_ymin_lin = gen->plot_ymin_lin_nuclide;
		plot_ymin_log10 = gen->plot_ymin_log10_nuclide;
		plot_window = gen->plot_window_nuclide;
		canvas = gen->canvas_nuclide;
	}

	if (gtk_toggle_button_get_active(button)) {
		//linear mode
#ifdef HAVE_CXX
		plot_window->set_axis_logarithmic_y(false);
		plot_window->set_region(plot_xmin, plot_xmax, plot_ymin_lin, plot_ymax);
#else
		gtk_plot_set_yscale(GTK_PLOT(plot_window), GTK_PLOT_SCALE_LINEAR);
		gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_Y,get_tickstep(plot_ymin_lin, plot_ymax),5);
		gtk_plot_set_range(GTK_PLOT(plot_window), plot_xmin, plot_xmax, plot_ymin_lin, plot_ymax);
#endif
	}
	else {
		//logarithmic mode
#ifdef HAVE_CXX
		plot_window->set_axis_logarithmic_y(true);
		plot_window->set_region(plot_xmin, plot_xmax, plot_ymin_log10, plot_ymax);
#else
		gtk_plot_set_yscale(GTK_PLOT(plot_window), GTK_PLOT_SCALE_LOG10);
		gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_Y, 0.1,1);
		gtk_plot_set_range(GTK_PLOT(plot_window), plot_xmin, plot_xmax, plot_ymin_log10, plot_ymax);
#endif
	}
#ifndef HAVE_CXX
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(canvas));
#endif

}

static void transmissioneff_clicked_cb(GtkToggleButton *button, GtkWidget *filechooser) {
	if (gtk_toggle_button_get_active(button)) {
		gtk_widget_set_sensitive(filechooser, TRUE);
	}
	else {
		gtk_widget_set_sensitive(filechooser, FALSE);
	}

	return;
}
static void generate_tube_spectrum(struct generate *gen);
static void generate_nuclide_spectrum(struct generate *gen);

static void generate_button_clicked_cb(GtkButton *button, struct generate *gen) {
	if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 0) {
		generate_tube_spectrum(gen);
	}
	else if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 1) {
		generate_nuclide_spectrum(gen);
	}

}

static void image_button_clicked_cb(GtkButton *button, struct generate *gen) {
#ifdef HAVE_CXX
	Gtk::PLplot::Canvas *canvas = NULL;
#else
	GtkWidget *canvas = NULL;
#endif
	if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 0) {
		canvas = gen->canvas_tube;
	}
	else if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 1) {
		canvas = gen->canvas_nuclide;
	}
	GtkWidget *dialog;

	dialog = xmi_msim_gui_export_canvas_dialog_new("Save spectrum as image",
		GTK_WINDOW(gen->window), canvas);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		// error handling??
		xmi_msim_gui_export_canvas_dialog_save(XMI_MSIM_GUI_EXPORT_CANVAS_DIALOG(dialog));
	}
	gtk_widget_destroy(dialog);

	return;
}
static void export_button_clicked_cb(GtkButton *button, struct generate *gen) {
	struct xmi_excitation *excitation;
	if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 0) {
		excitation= gen->excitation_tube;
	}
	else if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 1) {
		excitation = gen->excitation_nuclide;
	}
	GtkWidget *dialog = gtk_file_chooser_dialog_new("Export spectrum as ASCII file",GTK_WINDOW(gen->window), GTK_FILE_CHOOSER_ACTION_SAVE, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL);
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	GtkWidget *label;
	if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 0) {
		label = gtk_label_new("First the continuous intervals will be printed, marked by their start energy and intensity. This will be followed by a list of discrete lines, each defined by their energy and intensity.");
		gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(dialog), label);
		gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
		gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(dialog), "tube-spectrum.txt");
	}
	else if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 1) {
		gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(dialog), "nuclide-spectrum.txt");
	}
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		char *filename;
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		FILE *filePtr;
		if ((filePtr = fopen(filename, "w")) == NULL) {
			gtk_widget_destroy(dialog);
			dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not write to %s.", filename);
			gtk_dialog_run(GTK_DIALOG(dialog));
		}
		else {
			int i;
			for (i = 0 ; i < excitation->n_continuous ; i++) {
				fprintf(filePtr, "%g     %g\n", excitation->continuous[i].energy, excitation->continuous[i].horizontal_intensity*2.0);
			}
			for (i = 0 ; i < excitation->n_discrete ; i++) {
				fprintf(filePtr, "%g     %g\n", excitation->discrete[i].energy, excitation->discrete[i].horizontal_intensity*2.0);
			}
			fclose(filePtr);
		}
	}
	gtk_widget_destroy(dialog);

	return;
}


static void slits_button_clicked_cb(GtkButton *button, GtkEntry *tubeSolidAngleW) {
	//calculate solid angle based on slits
	double solid_angle = 4.0 * atan(current->xi->geometry->slit_size_x * current->xi->geometry->slit_size_y/(2.0*current->xi->geometry->d_source_slit*sqrt(4.0 * current->xi->geometry->d_source_slit * current->xi->geometry->d_source_slit + current->xi->geometry->slit_size_x * current->xi->geometry->slit_size_x + current->xi->geometry->slit_size_y + current->xi->geometry->slit_size_y)));

	char buf[200];
	sprintf(buf, "%g", solid_angle);
	gtk_entry_set_text(GTK_ENTRY(tubeSolidAngleW), buf);

	return;
}

static void cancel_button_clicked_cb(GtkButton *button, struct generate *gen) {
	union xmimsim_prefs_val xpv;

	/*
	xpv.xep = get_ebel_parameters(gen);

	if (xpv.xep != NULL && xmimsim_gui_set_prefs(XMIMSIM_GUI_EBEL_LAST_USED, xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error setting preferences for last used Ebel configuration\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
	}
	if (xpv.xep)
		g_free(xpv.xep->transmission_efficiency_file);
	g_free(xpv.xep);

	xpv.xnp = get_nuclide_parameters(gen);

	if (xpv.xnp != NULL && xmimsim_gui_set_prefs(XMIMSIM_GUI_NUCLIDE_LAST_USED, xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(gen->window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error setting preferences for last used Nuclide configuration\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
	}
	g_free(xpv.xnp);
	*/
	xpv.i = gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_SOURCES_LAST_USED, xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(gen->window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error setting preferences for last used Sources\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
	}

	gtk_widget_destroy(gen->window);
}

static gboolean ebel_delete_event_cb(GtkWidget *widget, GdkEvent *event, struct generate *gen) {
	union xmimsim_prefs_val xpv;
	/*
	xpv.xep = get_ebel_parameters(gen);

	if (xpv.xep != NULL && xmimsim_gui_set_prefs(XMIMSIM_GUI_EBEL_LAST_USED, xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(gen->window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error setting preferences for last used Ebel configuration\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
	}
	if (xpv.xep)
		g_free(xpv.xep->transmission_efficiency_file);
	g_free(xpv.xep);

	xpv.xnp = get_nuclide_parameters(gen);

	if (xpv.xnp != NULL && xmimsim_gui_set_prefs(XMIMSIM_GUI_NUCLIDE_LAST_USED, xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(gen->window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error setting preferences for last used Nuclide configuration\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
	}
	g_free(xpv.xnp);
	*/
	xpv.i = gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_SOURCES_LAST_USED, xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(gen->window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error setting preferences for last used Sources\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
	}

	gtk_widget_destroy(widget);
	return FALSE;
}
static void ok_button_clicked_cb(GtkButton *button, struct generate *gen) {
	union xmimsim_prefs_val xpv;
	/*
	xpv.xep = get_ebel_parameters(gen);

	if (xpv.xep != NULL && xmimsim_gui_set_prefs(XMIMSIM_GUI_EBEL_LAST_USED, xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(gen->window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error setting preferences for last used Ebel configuration\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
		return;
	}
	if (xpv.xep)
		g_free(xpv.xep->transmission_efficiency_file);
	g_free(xpv.xep);

	xpv.xnp = get_nuclide_parameters(gen);

	if (xpv.xnp != NULL && xmimsim_gui_set_prefs(XMIMSIM_GUI_NUCLIDE_LAST_USED, xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(gen->window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error setting preferences for last used Nuclide configuration\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
	}
	g_free(xpv.xnp);
	*/
	if (gen->excitation_tube && gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 0) {
		update_undo_buffer(EBEL_SPECTRUM_REPLACE, (GtkWidget *) gen->excitation_tube);
		gtk_list_store_clear(discWidget->store);
		int i;
		GtkTreeIter iter;
		for (i = 0 ; i < (current)->xi->excitation->n_discrete ; i++) {
			gtk_list_store_append(discWidget->store, &iter);
			gtk_list_store_set(discWidget->store, &iter,
			ENERGY_COLUMN, (current)->xi->excitation->discrete[i].energy,
			HOR_INTENSITY_COLUMN, (current)->xi->excitation->discrete[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, (current)->xi->excitation->discrete[i].vertical_intensity,
			SIGMA_X_COLUMN, (current)->xi->excitation->discrete[i].sigma_x,
			SIGMA_XP_COLUMN,(current)->xi->excitation->discrete[i].sigma_xp,
			SIGMA_Y_COLUMN,(current)->xi->excitation->discrete[i].sigma_y,
			SIGMA_YP_COLUMN,(current)->xi->excitation->discrete[i].sigma_yp,
			DISTRIBUTION_TYPE_COLUMN,(current)->xi->excitation->discrete[i].distribution_type,
			SCALE_PARAMETER_COLUMN,(current)->xi->excitation->discrete[i].scale_parameter,
			-1);
		}
		gtk_list_store_clear(contWidget->store);
		for (i = 0 ; i < (current)->xi->excitation->n_continuous ; i++) {
			gtk_list_store_append(contWidget->store, &iter);
			gtk_list_store_set(contWidget->store, &iter,
			ENERGY_COLUMN, (current)->xi->excitation->continuous[i].energy,
			HOR_INTENSITY_COLUMN, (current)->xi->excitation->continuous[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, (current)->xi->excitation->continuous[i].vertical_intensity,
			SIGMA_X_COLUMN, (current)->xi->excitation->continuous[i].sigma_x,
			SIGMA_XP_COLUMN,(current)->xi->excitation->continuous[i].sigma_xp,
			SIGMA_Y_COLUMN,(current)->xi->excitation->continuous[i].sigma_y,
			SIGMA_YP_COLUMN,(current)->xi->excitation->continuous[i].sigma_yp,
			-1);
		}
	}
	else if (gen->excitation_nuclide && gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 1) {
		GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "Add radionuclide emission spectrum to current spectrum or replace it completely?");
		gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_ADD, GTK_RESPONSE_OK, GTK_STOCK_REFRESH, GTK_RESPONSE_CANCEL, NULL);
		GtkWidget *button = gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);
		update_button_text(button, "Replace");
		//this may not work on all platforms -> Mac OS X
		gtk_window_set_deletable(GTK_WINDOW(dialog), FALSE);

		int rv = gtk_dialog_run (GTK_DIALOG (dialog));
		if (rv == GTK_RESPONSE_OK) {
			//add
			int i;
			if (current->xi->excitation->n_discrete > 0) {
				for (i = 0 ; i < current->xi->excitation->n_discrete ; i++) {
					if (bsearch(gen->excitation_nuclide->discrete+i, current->xi->excitation->discrete, current->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
						gtk_widget_destroy(dialog);
						dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy lines: one or more of the new energies exist already in the list of lines.");
						gtk_dialog_run(GTK_DIALOG(dialog));
						gtk_widget_destroy(dialog);
						free(energy_disc);
						energy_disc = NULL;
						return;
					}
				}
			}
			update_undo_buffer(NUCLIDE_SPECTRUM_ADD, (GtkWidget *) gen->excitation_nuclide);
		}
		else if (rv == GTK_RESPONSE_CANCEL) {
			//replace -> no need to check for duplicates here
			update_undo_buffer(NUCLIDE_SPECTRUM_REPLACE, (GtkWidget *) gen->excitation_nuclide);
		}
		else {
			gtk_widget_destroy(dialog);
			return;
		}
		gtk_list_store_clear(discWidget->store);
		int i;
		GtkTreeIter iter;
		for (i = 0 ; i < (current)->xi->excitation->n_discrete ; i++) {
			gtk_list_store_append(discWidget->store, &iter);
			gtk_list_store_set(discWidget->store, &iter,
			ENERGY_COLUMN, (current)->xi->excitation->discrete[i].energy,
			HOR_INTENSITY_COLUMN, (current)->xi->excitation->discrete[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, (current)->xi->excitation->discrete[i].vertical_intensity,
			SIGMA_X_COLUMN, (current)->xi->excitation->discrete[i].sigma_x,
			SIGMA_XP_COLUMN,(current)->xi->excitation->discrete[i].sigma_xp,
			SIGMA_Y_COLUMN,(current)->xi->excitation->discrete[i].sigma_y,
			SIGMA_YP_COLUMN,(current)->xi->excitation->discrete[i].sigma_yp,
			DISTRIBUTION_TYPE_COLUMN,(current)->xi->excitation->discrete[i].distribution_type,
			SCALE_PARAMETER_COLUMN,(current)->xi->excitation->discrete[i].scale_parameter,
			-1);
		}
		gtk_list_store_clear(contWidget->store);
		for (i = 0 ; i < (current)->xi->excitation->n_continuous ; i++) {
			gtk_list_store_append(contWidget->store, &iter);
			gtk_list_store_set(contWidget->store, &iter,
			ENERGY_COLUMN, (current)->xi->excitation->continuous[i].energy,
			HOR_INTENSITY_COLUMN, (current)->xi->excitation->continuous[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, (current)->xi->excitation->continuous[i].vertical_intensity,
			SIGMA_X_COLUMN, (current)->xi->excitation->continuous[i].sigma_x,
			SIGMA_XP_COLUMN,(current)->xi->excitation->continuous[i].sigma_xp,
			SIGMA_Y_COLUMN,(current)->xi->excitation->continuous[i].sigma_y,
			SIGMA_YP_COLUMN,(current)->xi->excitation->continuous[i].sigma_yp,
			-1);
		}
	}

	xpv.i = gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_SOURCES_LAST_USED, xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(gen->window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error setting preferences for last used Sources\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
	}


	adjust_save_buttons();
	gtk_widget_destroy(gen->window);
}

static gboolean activate_link_cb(GtkLabel *label, gchar *uri, gpointer data) {

	xmi_open_url((char *) uri);
	return TRUE;
}

static void info_button_clicked_cb(GtkWidget *button, struct generate *gen) {
	GtkWidget *dialog = NULL;
	if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 0) {
		dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "X-ray tube spectrum");
		gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(dialog),
		"The model is based on the equations that can be found in "
		"the papers written by Horst Ebel and published in <a href='http://dx.doi.org/10.1002/(SICI)1097-4539(199907/08)28:4<255::AID-XRS347>3.0.CO;2-Y'>X-ray Spectrometry "
		"28 (1999), 255-266</a> and <a href='http://dx.doi.org/10.1002/xrs.610'>X-ray Spectrometry 32 (2003), 46-51</a>."
		);
	}
	else if (gtk_notebook_get_current_page(GTK_NOTEBOOK(gen->notebook)) == 1) {
		dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "Radionuclide");
		gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(dialog),
		"The X-ray and gamma spectra of the provided radionuclides "
		"have been obtained using the <a href='http://github.com/tschoonj/xraylib/wiki/The-xraylib-API-list-of-all-functions'>xraylib API</a> for radionuclides. "
		"Follow the references in the xraylib documentation in "
		"order to find the origin of the datasets."
		);
	}
	GtkWidget *area = gtk_message_dialog_get_message_area(GTK_MESSAGE_DIALOG(dialog));
	GList *children = gtk_container_get_children(GTK_CONTAINER(area));
	GtkWidget *temp = (GtkWidget *) g_list_nth_data(children, 1);
	g_list_free(children);
	//children = gtk_container_get_children(GTK_CONTAINER(temp));
	//temp = g_list_nth_data(children, 0);
	//g_list_free(children);
	g_signal_connect(G_OBJECT(temp), "activate-link", G_CALLBACK(activate_link_cb), NULL);

	gtk_dialog_run(GTK_DIALOG(dialog));
	gtk_widget_destroy(dialog);

	return;

}

static void generate_tube_spectrum(struct generate *gen) {


	//calculate spectrum based on current parameters
	gchar *text;
	gchar *endPtr;
	GtkWidget *dialog;
	double tube_voltage = gtk_spin_button_get_value(GTK_SPIN_BUTTON(gen->tubeVoltageW));

	int transmission;
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gen->transmissionW))) {
		transmission = 1;
	}
	else {
		transmission = 0;
	}

	struct xmi_layer *anode;
	anode = (struct xmi_layer *) malloc(sizeof(struct xmi_layer));
	anode->n_elements = 1;
	anode->Z = (int *) malloc(sizeof(int));
	anode->weight =  (double *) malloc(sizeof(double));
	anode->weight[0] = 1.0;
	anode->Z[0] = gtk_combo_box_get_active(GTK_COMBO_BOX(gen->anodeMaterialW))+1;
	if (transmission) {
		text = (gchar*) gtk_entry_get_text(GTK_ENTRY(gen->anodeDensityW));
		anode->density = strtod(text, &endPtr);
		if (strlen(text) == 0 || text + strlen(text) != endPtr || anode->density <= 0.0) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Invalid anode density: must be greater than zero");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		text = (gchar*) gtk_entry_get_text(GTK_ENTRY(gen->anodeThicknessW));
		anode->thickness = strtod(text, &endPtr);
		if (strlen(text) == 0 || text + strlen(text) != endPtr || anode->thickness <= 0.0) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Invalid anode thickness: must be greater than zero");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
	}

	double density;
	double thickness;

	struct xmi_layer *filter;
	text = (gchar*) gtk_entry_get_text(GTK_ENTRY(gen->filterDensityW));
	density = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || density < 0.0) {
		dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Invalid filter density: must be greater than or equal to zero");
		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		return;
	}
	text = (gchar*) gtk_entry_get_text(GTK_ENTRY(gen->filterThicknessW));
	thickness = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || thickness < 0.0) {
		dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Invalid filter thickness: must be greater than or equal to zero");
		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		return;
	}

	if (thickness > 0.0 && density > 0.0) {
		filter = (struct xmi_layer *) malloc(sizeof(struct xmi_layer));
		filter->n_elements = 1;
		filter->Z = (int *) malloc(sizeof(int));
		filter->weight = (double *) malloc(sizeof(double));
		filter->Z[0] = gtk_combo_box_get_active(GTK_COMBO_BOX(gen->filterMaterialW))+1;
		filter->weight[0] = 1.0;
		filter->density = density;
		filter->thickness = thickness;
	}
	else
		filter = NULL;

	struct xmi_layer *window;
	text = (gchar*) gtk_entry_get_text(GTK_ENTRY(gen->windowDensityW));
	density = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || density < 0.0) {
		dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Invalid window density: must be greater than or equal to zero");
		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		return;
	}
	text = (gchar*) gtk_entry_get_text(GTK_ENTRY(gen->windowThicknessW));
	thickness = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || thickness < 0.0) {
		dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Invalid window thickness: must be greater than or equal to zero");
		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		return;
	}

	if (thickness > 0.0 && density > 0.0) {
		window = (struct xmi_layer *) malloc(sizeof(struct xmi_layer));
		window->n_elements = 1;
		window->Z = (int *) malloc(sizeof(int));
		window->weight = (double *) malloc(sizeof(double));
		window->Z[0] = gtk_combo_box_get_active(GTK_COMBO_BOX(gen->windowMaterialW))+1;
		window->weight[0] = 1.0;
		window->density = density;
		window->thickness = thickness;
	}
	else
		window = NULL;


	double tube_alphaElectron = gtk_spin_button_get_value(GTK_SPIN_BUTTON(gen->alphaElectronW));
	double tube_alphaXray = gtk_spin_button_get_value(GTK_SPIN_BUTTON(gen->alphaXrayW));
	double tube_current = gtk_spin_button_get_value(GTK_SPIN_BUTTON(gen->tubeCurrentW));

	double tube_deltaE = gtk_spin_button_get_value(GTK_SPIN_BUTTON(gen->deltaEnergyW));

	text = (gchar *) gtk_entry_get_text(GTK_ENTRY(gen->tubeSolidAngleW));
	double tube_solid_angle = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || tube_solid_angle <= 0.0) {
		dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Invalid tube solid angle: must be greater than zero");
		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		return;
	}

	//TODO: make a copy here of the existing excitation_tube and set it back if there errors from here onwards
	struct xmi_excitation *excitation_tube_old;
	xmi_copy_excitation(gen->excitation_tube, &excitation_tube_old);


	//ebel main function
	xmi_tube_ebel(anode, window, filter, tube_voltage, tube_current, tube_alphaElectron, tube_alphaXray, tube_deltaE, tube_solid_angle, transmission, &gen->excitation_tube);


	//read transmission efficiency file if appropriate
	double *eff_x = NULL;
	double *eff_y = NULL;
	size_t n_eff = 0;
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gen->transmissionEffW)) == TRUE) {
		g_fprintf(stdout,"Found transmission efficiency file\n");
		gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(gen->transmissionEffFileW));
		if (filename == NULL || strlen(filename) == 0) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Please provide a transmission efficiency file or switch off the transmission efficiency toggle-button.");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			xmi_free_excitation(gen->excitation_tube);
			gen->excitation_tube = excitation_tube_old;
			return;
		}
		FILE *fp;
		if ((fp = fopen(filename, "r")) == NULL) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not open %s for reading.", filename);
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			xmi_free_excitation(gen->excitation_tube);
			gen->excitation_tube = excitation_tube_old;
			return;

		}
		char *line = NULL;
		double energy, efficiency;
		ssize_t linelen;
		size_t linecap = 0;
		int values;
		while ((linelen = getline(&line, &linecap, fp)) > -1) {
			if (linelen == 0 || strlen(g_strstrip(line)) == 0) {
				continue;
			}
			values = sscanf(line,"%lg %lg", &energy, &efficiency);
			if (values != 2 || energy < 0.0 || efficiency < 0.0 || efficiency > 1.0 || (n_eff > 0 && energy <= eff_x[n_eff-1]) || (n_eff == 0 && energy >= 1.0)) {
				dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error reading %s. The transmission efficiency file should contain two columns with energies (keV) in the left column and the transmission efficiency (value between 0 and 1) in the second column. Empty lines are ignored. First energy must be between 0 and 1 keV. The last value must be greater or equal to the tube voltage. At least 10 values are required.", filename);
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				xmi_free_excitation(gen->excitation_tube);
				gen->excitation_tube = excitation_tube_old;
				return;

			}
			n_eff++;
			eff_x = (double *) g_realloc(eff_x, sizeof(double)*n_eff);
			eff_y = (double *) g_realloc(eff_y, sizeof(double)*n_eff);
			eff_x[n_eff-1] = energy;
			eff_y[n_eff-1] = efficiency;
			g_fprintf(stdout,"Efficiency: %f -> %f\n",energy,efficiency);
			free(line);
			line = NULL;
		}
		fclose(fp);
		g_fprintf(stdout,"File closed. n_eff: %i\n",(int) n_eff);
		if (n_eff < 10 || gen->excitation_tube->continuous[gen->excitation_tube->n_continuous-1].energy > eff_x[n_eff-1]) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error reading %s. The transmission efficiency file should contain two columns with energies (keV) in the left column and the transmission efficiency (value between 0 and 1) in the second column. Empty lines are ignored. First energy must be between 0 and 1 keV. The last value must be greater or equal to the tube voltage. At least 10 values are required.", filename);
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			xmi_free_excitation(gen->excitation_tube);
			gen->excitation_tube = excitation_tube_old;
			return;
		}
	}


#ifdef HAVE_CXX
	try {
		gen->canvas_tube->remove_plot(0);
	}
	catch (Gtk::PLplot::Exception &e) {
		//this will fail if there's no plot available
	}
#else
	GtkPlotCanvasChild *child;

	GList *list;
	list = GTK_PLOT_CANVAS(gen->canvas_tube)->childs;
	while (list) {
		child = GTK_PLOT_CANVAS_CHILD(list->data);
		gtk_plot_canvas_remove_child(GTK_PLOT_CANVAS(gen->canvas_tube), child);
		list = GTK_PLOT_CANVAS(gen->canvas_tube)->childs;
	}
#endif

	//apply transmission efficiencies if required
	int i,j;
	if (n_eff > 0) {
		struct xmi_cubic_spline *spline = xmi_cubic_spline_init(eff_x, eff_y, n_eff);

		for (i = 0 ; i < gen->excitation_tube->n_continuous ; i++) {
			gen->excitation_tube->continuous[i].horizontal_intensity =
			gen->excitation_tube->continuous[i].vertical_intensity =
			gen->excitation_tube->continuous[i].horizontal_intensity *
			xmi_cubic_spline_eval(spline, gen->excitation_tube->continuous[i].energy);
		}

		for (i = 0 ; i < gen->excitation_tube->n_discrete ; i++) {
			gen->excitation_tube->discrete[i].horizontal_intensity =
			gen->excitation_tube->discrete[i].vertical_intensity =
			gen->excitation_tube->discrete[i].horizontal_intensity *
			xmi_cubic_spline_eval(spline, gen->excitation_tube->discrete[i].energy);
		}
		g_free(eff_x);
		g_free(eff_y);
		xmi_cubic_spline_free(spline);
	}


	//TODO:let's clean up the data a bit here...
	//we could be dealing with zeroes and/or ridiculously small intensities,
	//especially when using the transmission data efficiency or when using filters


	//add box with default settings
#ifdef HAVE_CXX
	Plot2DSources *plot_window = Gtk::manage(new Plot2DSources("Energy (keV)", "Intensity (photons/s)"));
	plot_window->hide();
	plot_window->hide_legend();
	plot_window->set_box_style(Gtk::PLplot::BoxStyle::BOX_TICKS_TICK_LABELS_MAIN_AXES_MAJOR_TICK_GRID);
	gen->canvas_tube->add_plot(*plot_window);
#else
	GtkWidget *plot_window;
	plot_window = gtk_plot_new_with_size(NULL,.65,.45);
	gtk_plot_set_background(GTK_PLOT(plot_window),&white_plot);
	gtk_plot_hide_legends(GTK_PLOT(plot_window));
#endif

	double *bins = (double *) malloc(sizeof(double) * gen->excitation_tube->n_continuous);

	for (i = 0 ; i < gen->excitation_tube->n_continuous ; i++)
		bins[i] = gen->excitation_tube->continuous[i].horizontal_intensity*2.0*tube_deltaE;

	for (i = 0 ; i < gen->excitation_tube->n_discrete ; i++) {
		for (j = 0 ; j < gen->excitation_tube->n_continuous ; j++) {
			if (gen->excitation_tube->discrete[i].energy < gen->excitation_tube->continuous[j].energy && j != 0) {
				bins[j-1] += gen->excitation_tube->discrete[i].horizontal_intensity*2.0;
				break;
			}
		}
	}

	double plot_ymax = xmi_maxval_double(bins,gen->excitation_tube->n_continuous)*1.2;
	//double plot_ymin = xmi_minval_double(bins,gen->excitation->n_continuous)*0.8;
	double plot_ymin_lin = 0.0;
	double plot_xmin = 0.0;
	double plot_xmax = gen->excitation_tube->continuous[gen->excitation_tube->n_continuous-1].energy;

	//if logarithmic -> search for an appropriate minimum
	double new_min = plot_ymax;
	for (i = 0 ; i < gen->excitation_tube->n_continuous ; i++) {
		if (bins[i] < new_min && bins[i] > 0.0)
			new_min = bins[i];
	}
	double plot_ymin_log10 = MAX(new_min, plot_ymax*1E-5);
	double plot_ymin;


	gen->plot_xmin_tube = plot_xmin;
	gen->plot_xmax_tube = plot_xmax;
	gen->plot_ymax_tube = plot_ymax;
	gen->plot_ymin_lin_tube = plot_ymin_lin;
	gen->plot_ymin_log10_tube = plot_ymin_log10;
	gen->plot_window_tube = plot_window;


	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gen->linear_tubeW))) {
#ifdef HAVE_CXX
		plot_window->set_axis_logarithmic_y(false);
#else
		gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_Y,get_tickstep(plot_ymin_lin, plot_ymax),5);
		gtk_plot_set_yscale(GTK_PLOT(plot_window), GTK_PLOT_SCALE_LINEAR);
#endif

		plot_ymin = plot_ymin_lin;
	}
	else {
#ifdef HAVE_CXX
		plot_window->set_axis_logarithmic_y(true);
#else
		gtk_plot_set_yscale(GTK_PLOT(plot_window), GTK_PLOT_SCALE_LOG10);
#endif
		plot_ymin = plot_ymin_log10;
	}

	gen->plot_ymin_tube = plot_ymin;


#ifdef HAVE_CXX
	plot_window->signal_double_press().connect([plot_window, gen](double x, double y){
		plot_window->set_region(gen->plot_xmin_tube, gen->plot_xmax_tube,
					gen->plot_ymin_tube, gen->plot_ymax_tube);
	});
	plot_window->show();
#else
	gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_X, get_tickstep(plot_xmin, plot_xmax),5);
	gtk_plot_set_range(GTK_PLOT(plot_window),plot_xmin, plot_xmax, plot_ymin, plot_ymax);
	gtk_plot_clip_data(GTK_PLOT(plot_window), TRUE);
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP));
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT));
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Intensity (photons/s)");
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Energy (keV)");
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",TUBE_PLOT_TITLE,90,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Helvetica",TUBE_PLOT_TITLE,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	//gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",30,90,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);

	//gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),"Helvetica",30,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,0);
        gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),GTK_PLOT_LABEL_FLOAT,0);

	if (plot_ymax < 10000.0 && plot_ymin >= 10.0) {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_FLOAT,0);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_FLOAT,0);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",RESULTS_PLOT_LABELS_LR_FLOAT,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",RESULTS_PLOT_LABELS_LR_FLOAT,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}
	else {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_EXP,1);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_EXP,1);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",TUBE_PLOT_LABELS_LR_EXP,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",TUBE_PLOT_LABELS_LR_EXP,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}


	gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Helvetica",TUBE_PLOT_LABELS_TP,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),"Helvetica",TUBE_PLOT_LABELS_TP,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_show_labels(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),0);
        gtk_plot_grids_set_visible(GTK_PLOT(plot_window),TRUE,FALSE,TRUE,FALSE);
	child = gtk_plot_canvas_plot_new(GTK_PLOT(plot_window));
        gtk_plot_canvas_put_child(GTK_PLOT_CANVAS(gen->canvas_tube), child, .15,.05,.90,.85);
        gtk_widget_show(plot_window);
#endif
	double *energies = (double *) malloc(sizeof(double) * gen->excitation_tube->n_continuous);
	for (i=0 ; i < gen->excitation_tube->n_continuous ; i++)
		energies[i] = gen->excitation_tube->continuous[i].energy;


#ifdef HAVE_CXX
	std::vector<double> x_vals(energies, energies + gen->excitation_tube->n_continuous);
	std::vector<double> y_vals(bins, bins + gen->excitation_tube->n_continuous);
	std::for_each(std::begin(y_vals), std::end(y_vals), [plot_ymin_log10](double &a) { if (a < plot_ymin_log10) a = plot_ymin_log10;});

	Gtk::PLplot::PlotData2D *dataset= Gtk::manage(
		new Gtk::PLplot::PlotData2D(
			x_vals,
			y_vals
		)
	);
	dataset->set_color(*blue_plot);
	dataset->set_line_width(2.0);
	dataset->show();
	plot_window->add_data(*dataset);
	plot_window->set_region(gen->plot_xmin_tube, gen->plot_xmax_tube,
				gen->plot_ymin_tube, gen->plot_ymax_tube);
#else
	GtkPlotData *dataset;
	dataset = GTK_PLOT_DATA(gtk_plot_data_new());
	gtk_plot_add_data(GTK_PLOT(plot_window),dataset);
	gtk_plot_data_set_numpoints(dataset, gen->excitation_tube->n_continuous);
	gtk_plot_data_set_x(dataset, energies);
	gtk_plot_data_set_y(dataset, bins);
	gtk_widget_show(GTK_WIDGET(dataset));
	gtk_plot_data_set_line_attributes(dataset, (GtkPlotLineStyle) GTK_PLOT_LINE_SOLID, (GdkCapStyle) 0, (GdkJoinStyle) 0,1,&blue_plot);
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(gen->canvas_tube));
	gtk_widget_queue_draw(GTK_WIDGET(gen->canvas_tube));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(gen->canvas_tube));
	gtk_plot_paint(GTK_PLOT(plot_window));
	gtk_plot_refresh(GTK_PLOT(plot_window),NULL);
#endif
	xmi_free_excitation(excitation_tube_old);
}

static void generate_nuclide_spectrum(struct generate *gen) {
	gchar *text;
	gchar *endPtr;
	double activity;
	double nuclide_solid_angle_fraction = 1.0/(4.0*M_PI);
	GtkWidget *dialog;
	int activityUnit, radioNuclide;

	text = (gchar*) gtk_entry_get_text(GTK_ENTRY(gen->activityW));
	activity = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || activity <= 0.0) {
		dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Invalid activity: must be greater than zero");
		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		return;
	}

	text = (gchar*) gtk_entry_get_text(GTK_ENTRY(gen->nuclideSolidAngleW));
	nuclide_solid_angle_fraction *= strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || nuclide_solid_angle_fraction <= 0.0 || nuclide_solid_angle_fraction > 1.0) {
		dialog = gtk_message_dialog_new(GTK_WINDOW(gen->window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Invalid solid angle: must be greater than zero and less or equal to 4π");
		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		return;
	}

	activity *= nuclide_solid_angle_fraction;

	activityUnit = gtk_combo_box_get_active(GTK_COMBO_BOX(gen->activityUnitW));

	if (activityUnit == ACTIVITY_UNIT_mCi) {
		activity *= 3.7E7;
	}
	else if (activityUnit == ACTIVITY_UNIT_Ci) {
		activity *= 3.7E10;
	}
	else if (activityUnit == ACTIVITY_UNIT_GBq) {
		activity *= 1E9;
	}
	else if (activityUnit == ACTIVITY_UNIT_Bq) {
		//do nothing
	}
	radioNuclide = gtk_combo_box_get_active(GTK_COMBO_BOX(gen->radioNuclideW));

#ifdef HAVE_CXX
	try {
		gen->canvas_nuclide->remove_plot(0);
	}
	catch (Gtk::PLplot::Exception &e) {
		//this will fail if there's no plot available
	}
#else
	GtkPlotCanvasChild *child;

	GList *list;
	list = GTK_PLOT_CANVAS(gen->canvas_nuclide)->childs;
	while (list) {
		child = GTK_PLOT_CANVAS_CHILD(list->data);
		gtk_plot_canvas_remove_child(GTK_PLOT_CANVAS(gen->canvas_nuclide), child);
		list = GTK_PLOT_CANVAS(gen->canvas_nuclide)->childs;
	}
#endif


	struct radioNuclideData *rnd = GetRadioNuclideDataByIndex(radioNuclide);

	int i;

	gen->excitation_nuclide = (struct xmi_excitation *) malloc(sizeof(struct xmi_excitation));
	gen->excitation_nuclide->n_continuous = 0;
	gen->excitation_nuclide->continuous = NULL;
	gen->excitation_nuclide->n_discrete= 0;
	gen->excitation_nuclide->discrete= NULL;

	double plot_xmax = 0.0;

	for (i = 0 ; i < rnd->nXrays ; i++) {
		double energy = LineEnergy(rnd->Z_xray, rnd->XrayLines[i]);
		if (energy < 1.0 || energy > 200.0)
			continue;

		if (energy > plot_xmax)
			plot_xmax = energy;

		gen->excitation_nuclide->discrete = (struct xmi_energy_discrete *) realloc(gen->excitation_nuclide->discrete, sizeof(struct xmi_energy_discrete)*++gen->excitation_nuclide->n_discrete);
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].energy = energy;
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].horizontal_intensity =
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].vertical_intensity =
		rnd->XrayIntensities[i]*activity/2.0;
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].sigma_x =
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].sigma_xp =
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].sigma_y =
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].sigma_yp =
		0.0;
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].distribution_type = XMI_DISCRETE_MONOCHROMATIC;
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].scale_parameter= 0.0;
	}
	for (i = 0 ; i < rnd->nGammas ; i++) {
		double energy = rnd->GammaEnergies[i];
		if (energy < 1.0 || energy > 200.0)
			continue;

		if (energy > plot_xmax)
			plot_xmax = energy;

		gen->excitation_nuclide->discrete = (struct xmi_energy_discrete *) realloc(gen->excitation_nuclide->discrete, sizeof(struct xmi_energy_discrete)*++gen->excitation_nuclide->n_discrete);
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].energy = energy;
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].horizontal_intensity =
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].vertical_intensity =
		rnd->GammaIntensities[i]*activity/2.0;
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].sigma_x =
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].sigma_xp =
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].sigma_y =
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].sigma_yp =
		0.0;
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].distribution_type = XMI_DISCRETE_MONOCHROMATIC;
		gen->excitation_nuclide->discrete[gen->excitation_nuclide->n_discrete-1].scale_parameter= 0.0;
	}
	FreeRadioNuclideData(rnd);

	//add box with default settings
#ifdef HAVE_CXX
	Plot2DSources *plot_window = Gtk::manage(new Plot2DSources("Energy (keV)", "Intensity (photons/s)"));
	plot_window->hide();
	plot_window->hide_legend();
	plot_window->set_box_style(Gtk::PLplot::BoxStyle::BOX_TICKS_TICK_LABELS_MAIN_AXES_MAJOR_TICK_GRID);
	gen->canvas_nuclide->add_plot(*plot_window);
#else
	GtkWidget *plot_window;
	plot_window = gtk_plot_new_with_size(NULL,.65,.45);
	gtk_plot_set_background(GTK_PLOT(plot_window),&white_plot);
	gtk_plot_hide_legends(GTK_PLOT(plot_window));
#endif

	double *bins = (double *) calloc(1000, sizeof(double));

	plot_xmax *= 1.2;

	double *energies = (double *) malloc(sizeof(double) * 1000);
	for (i=0 ; i < 1000 ; i++)
		energies[i] = i*plot_xmax/999.0;

	for (i = 0 ; i < gen->excitation_nuclide->n_discrete ; i++) {
		int j = (int) floor(gen->excitation_nuclide->discrete[i].energy*999.0/plot_xmax);
		//fprintf(stdout, "j : %i\n", j);
		bins[j] += gen->excitation_nuclide->discrete[i].horizontal_intensity*2.0;
	}



	double plot_ymax = xmi_maxval_double(bins,1000)*1.2;
	double plot_ymin_lin = 0.0;
	double plot_xmin = 0.0;
	//
	//if logarithmic -> search for an appropriate minimum
	double new_min = plot_ymax;
	for (i = 0 ; i < 1000 ; i++) {
		//printf("%f -> %f\n", energies[i], bins[i]);
		if (bins[i] < new_min && bins[i] > 0.0)
			new_min = bins[i];
	}
	double plot_ymin_log10 = MAX(new_min, plot_ymax*1E-5);
	double plot_ymin;


	gen->plot_xmin_nuclide = plot_xmin;
	gen->plot_xmax_nuclide = plot_xmax;
	gen->plot_ymax_nuclide = plot_ymax;
	gen->plot_ymin_lin_nuclide = plot_ymin_lin;
	gen->plot_ymin_log10_nuclide = plot_ymin_log10;
	gen->plot_window_nuclide = plot_window;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gen->linear_nuclideW))) {
#ifdef HAVE_CXX
		plot_window->set_axis_logarithmic_y(false);
#else
		gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_Y,get_tickstep(plot_ymin_lin, plot_ymax),5);
		gtk_plot_set_yscale(GTK_PLOT(plot_window), GTK_PLOT_SCALE_LINEAR);
#endif
		plot_ymin = plot_ymin_lin;
	}
	else {
#ifdef HAVE_CXX
		plot_window->set_axis_logarithmic_y(true);
#else
		gtk_plot_set_yscale(GTK_PLOT(plot_window), GTK_PLOT_SCALE_LOG10);
#endif
		plot_ymin = plot_ymin_log10;
	}

	gen->plot_ymin_nuclide = plot_ymin;

#ifdef HAVE_CXX
	plot_window->signal_double_press().connect([plot_window, gen](double x, double y){
		plot_window->set_region(gen->plot_xmin_nuclide, gen->plot_xmax_nuclide,
					gen->plot_ymin_nuclide, gen->plot_ymax_nuclide);
	});
	plot_window->show();
#else
	gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_X, get_tickstep(plot_xmin, plot_xmax),5);
	gtk_plot_set_range(GTK_PLOT(plot_window),plot_xmin, plot_xmax, plot_ymin, plot_ymax);
	gtk_plot_clip_data(GTK_PLOT(plot_window), TRUE);
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP));
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT));
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Intensity (photons/s)");
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Energy (keV)");
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",TUBE_PLOT_TITLE,90,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Helvetica",TUBE_PLOT_TITLE,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	//gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",30,90,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);

	//gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),"Helvetica",30,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,0);
        gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),GTK_PLOT_LABEL_FLOAT,0);

	if (plot_ymax < 10000.0 && plot_ymin >= 10.0) {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_FLOAT,0);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_FLOAT,0);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",RESULTS_PLOT_LABELS_LR_FLOAT,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",RESULTS_PLOT_LABELS_LR_FLOAT,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}
	else {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_EXP,1);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_EXP,1);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",TUBE_PLOT_LABELS_LR_EXP,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",TUBE_PLOT_LABELS_LR_EXP,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}


	gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Helvetica",TUBE_PLOT_LABELS_TP,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),"Helvetica",TUBE_PLOT_LABELS_TP,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_show_labels(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),0);
        gtk_plot_grids_set_visible(GTK_PLOT(plot_window),TRUE,FALSE,TRUE,FALSE);
	child = gtk_plot_canvas_plot_new(GTK_PLOT(plot_window));
        gtk_plot_canvas_put_child(GTK_PLOT_CANVAS(gen->canvas_nuclide), child, .15,.05,.90,.85);
        gtk_widget_show(plot_window);
#endif

#ifdef HAVE_CXX
	std::vector<double> x_vals(energies, energies + 1000);
	std::vector<double> y_vals(bins, bins + 1000);
	std::for_each(std::begin(y_vals), std::end(y_vals), [plot_ymin_log10](double &a) { if (a < plot_ymin_log10) a = plot_ymin_log10;});

	Gtk::PLplot::PlotData2D *dataset= Gtk::manage(
		new Gtk::PLplot::PlotData2D(
			x_vals,
			y_vals
		)
	);
	dataset->set_color(*blue_plot);
	dataset->set_line_width(2.0);
	dataset->show();
	plot_window->add_data(*dataset);
	plot_window->set_region(gen->plot_xmin_nuclide, gen->plot_xmax_nuclide,
				gen->plot_ymin_nuclide, gen->plot_ymax_nuclide);
#else
	GtkPlotData *dataset;
	dataset = GTK_PLOT_DATA(gtk_plot_data_new());
	gtk_plot_add_data(GTK_PLOT(plot_window),dataset);
	gtk_plot_data_set_numpoints(dataset, 1000);
	gtk_plot_data_set_x(dataset, energies);
	gtk_plot_data_set_y(dataset, bins);
	gtk_widget_show(GTK_WIDGET(dataset));
	gtk_plot_data_set_line_attributes(dataset, (GtkPlotLineStyle) GTK_PLOT_LINE_SOLID, (GdkCapStyle) 0, (GdkJoinStyle) 0,1,&blue_plot);
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(gen->canvas_nuclide));
	gtk_widget_queue_draw(GTK_WIDGET(gen->canvas_nuclide));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(gen->canvas_nuclide));
	gtk_plot_paint(GTK_PLOT(plot_window));
	gtk_plot_refresh(GTK_PLOT(plot_window),NULL);
#endif
}

void xray_sources_button_clicked_cb(GtkButton *button, GtkWidget *main_window) {

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "X-ray sources");
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	gtk_window_set_transient_for(GTK_WINDOW(window), GTK_WINDOW(main_window));
	gtk_window_set_default_size(GTK_WINDOW(window), 900, -1);

	GtkWidget *notebook = gtk_notebook_new();


	GtkWidget *mainVBox = gtk_vbox_new(FALSE, 2);
#ifdef HAVE_CXX
	gtk_widget_set_hexpand(mainVBox, FALSE);
	gtk_widget_set_vexpand(mainVBox, FALSE);
#endif

	union xmimsim_prefs_val xpv;

	/*
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_EBEL_LAST_USED, &xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(main_window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error getting preferences for last used Ebel configuration\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
		return;
	}
	*/
	//struct xmi_ebel_parameters *xep = xpv.xep;
	struct xmi_ebel_parameters *xep;

	GtkWidget *label;
	GtkWidget *hbox;
	GtkAdjustment *adj = GTK_ADJUSTMENT(gtk_adjustment_new(xep->tube_voltage,5,100,1,10,0));

	label = gtk_label_new("Tube voltage (kV)");
	GtkWidget *tubeVoltageW = gtk_spin_button_new(GTK_ADJUSTMENT(adj), 0.1, 1);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(tubeVoltageW), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), tubeVoltageW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);

	GtkAdjustment *adjCurrent = GTK_ADJUSTMENT(gtk_adjustment_new(xep->tube_current,0.001,1000,0.1,1.0,0));
	label = gtk_label_new("Tube current (mA)");
	GtkWidget *tubeCurrentW = gtk_spin_button_new(GTK_ADJUSTMENT(adjCurrent), 0.1, 4);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(tubeCurrentW), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), tubeCurrentW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);

	GtkWidget *tubeSolidAngleW = gtk_entry_new();
	hbox = gtk_hbox_new(FALSE, 3);
	label = gtk_label_new("Tube solid angle (sr)");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	GtkWidget *slitsButton = gtk_button_new_with_label("Get from slits");
	gtk_box_pack_end(GTK_BOX(hbox), slitsButton, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), tubeSolidAngleW, FALSE, FALSE, 2);
	g_signal_connect(G_OBJECT(slitsButton), "clicked", G_CALLBACK(slits_button_clicked_cb), (gpointer) tubeSolidAngleW);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);

	char buf[200];
	sprintf(buf, "%g", xep->tube_solid_angle);
	gtk_entry_set_text(GTK_ENTRY(tubeSolidAngleW), buf);



	GtkWidget *table = gtk_table_new(4, 4, FALSE);
	gtk_table_set_row_spacings(GTK_TABLE(table), 2);
	gtk_table_set_col_spacings(GTK_TABLE(table), 2);

	//row 0
	gtk_table_attach(GTK_TABLE(table), gtk_label_new("Material"), 1, 2, 0, 1, GTK_EXPAND, GTK_EXPAND, 1, 1);
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label),"Density (g/cm<sup>3</sup>)");
	gtk_table_attach(GTK_TABLE(table), label, 2, 3, 0, 1, GTK_EXPAND, GTK_EXPAND, 1, 1);
	gtk_table_attach(GTK_TABLE(table), gtk_label_new("Thickness"), 3, 4, 0, 1, GTK_EXPAND, GTK_EXPAND, 1, 1);

	//row 1
	gtk_table_attach(GTK_TABLE(table), gtk_label_new("Anode"), 0, 1, 1, 2, GTK_EXPAND, GTK_EXPAND, 1, 1);
	GtkWidget *anodeDensityW = gtk_entry_new();
	GtkWidget *anodeMaterialW = gtk_combo_box_text_new();
	g_signal_connect(G_OBJECT(anodeMaterialW), "changed", G_CALLBACK(material_changed_cb), (gpointer) anodeDensityW);
	int i;
	gchar *symbol;
	for (i = 1 ; i <= 94 ; i++) {
		symbol = AtomicNumberToSymbol(i);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(anodeMaterialW), symbol);
		xrlFree(symbol);
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(anodeMaterialW), xep->anode_Z-1);
	gtk_table_attach(GTK_TABLE(table), anodeMaterialW, 1, 2, 1, 2, GTK_EXPAND, GTK_EXPAND, 1, 1);
	gtk_table_attach(GTK_TABLE(table), anodeDensityW, 2, 3, 1, 2, GTK_EXPAND, GTK_EXPAND, 1, 1);
	GtkWidget *anodeThicknessW = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), anodeThicknessW, 3, 4, 1, 2, GTK_EXPAND, GTK_EXPAND, 1, 1);

	if (xep->transmission_tube == FALSE) {
		gtk_widget_set_sensitive(anodeThicknessW, FALSE);
		gtk_widget_set_sensitive(anodeDensityW, FALSE);
	}
	sprintf(buf, "%g", xep->anode_rho);
	gtk_entry_set_text(GTK_ENTRY(anodeDensityW), buf);
	sprintf(buf, "%g", xep->anode_thickness);
	gtk_entry_set_text(GTK_ENTRY(anodeThicknessW), buf);

	//row 2
	gtk_table_attach(GTK_TABLE(table), gtk_label_new("Window"), 0, 1, 2, 3, GTK_EXPAND, GTK_EXPAND, 1, 1);
	GtkWidget *windowMaterialW = gtk_combo_box_text_new();
	GtkWidget *windowDensityW = gtk_entry_new();
	g_signal_connect(G_OBJECT(windowMaterialW), "changed", G_CALLBACK(material_changed_cb), (gpointer) windowDensityW);
	for (i = 1 ; i <= 94 ; i++) {
		symbol = AtomicNumberToSymbol(i);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(windowMaterialW), symbol);
		xrlFree(symbol);
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(windowMaterialW), xep->window_Z-1);
	gtk_table_attach(GTK_TABLE(table), windowMaterialW, 1, 2, 2, 3, GTK_EXPAND, GTK_EXPAND, 1, 1);
	gtk_table_attach(GTK_TABLE(table), windowDensityW, 2, 3, 2, 3, GTK_EXPAND, GTK_EXPAND, 1, 1);
	GtkWidget *windowThicknessW = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), windowThicknessW, 3, 4, 2, 3, GTK_EXPAND, GTK_EXPAND, 1, 1);
	sprintf(buf, "%g", xep->window_rho);
	gtk_entry_set_text(GTK_ENTRY(windowDensityW), buf);
	sprintf(buf, "%g", xep->window_thickness);
	gtk_entry_set_text(GTK_ENTRY(windowThicknessW), buf);

	//row 3
	gtk_table_attach(GTK_TABLE(table), gtk_label_new("Filter"), 0, 1, 3, 4, GTK_EXPAND, GTK_EXPAND, 1, 1);
	GtkWidget *filterMaterialW = gtk_combo_box_text_new();
	GtkWidget *filterDensityW = gtk_entry_new();
	g_signal_connect(G_OBJECT(filterMaterialW), "changed", G_CALLBACK(material_changed_cb), (gpointer) filterDensityW);
	for (i = 1 ; i <= 94 ; i++) {
		symbol = AtomicNumberToSymbol(i);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(filterMaterialW), symbol);
		xrlFree(symbol);
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(filterMaterialW), xep->filter_Z-1);
	gtk_table_attach(GTK_TABLE(table), filterMaterialW, 1, 2, 3, 4, GTK_EXPAND, GTK_EXPAND, 1, 1);
	gtk_table_attach(GTK_TABLE(table), filterDensityW, 2, 3, 3, 4, GTK_EXPAND, GTK_EXPAND, 1, 1);
	GtkWidget *filterThicknessW = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), filterThicknessW, 3, 4, 3, 4, GTK_EXPAND, GTK_EXPAND, 1, 1);
	sprintf(buf, "%g", xep->filter_rho);
	gtk_entry_set_text(GTK_ENTRY(filterDensityW), buf);
	sprintf(buf, "%g", xep->filter_thickness);
	gtk_entry_set_text(GTK_ENTRY(filterThicknessW), buf);

	GtkAdjustment *adj2 = GTK_ADJUSTMENT(gtk_adjustment_new(xep->alpha,50,90,1,10,0));
	GtkWidget *alphaElectronW = gtk_spin_button_new(GTK_ADJUSTMENT(adj2), 0.1, 1);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(alphaElectronW), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 3);
	label = gtk_label_new("Electron incidence angle (degrees)");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), alphaElectronW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);

	GtkAdjustment *adj3 = GTK_ADJUSTMENT(gtk_adjustment_new(xep->beta,5,90,1,10,0));
	GtkWidget *alphaXrayW = gtk_spin_button_new(GTK_ADJUSTMENT(adj3), 0.1, 1);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(alphaXrayW), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 3);
	label = gtk_label_new("X-ray take-off angle (degrees)");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), alphaXrayW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);

	GtkAdjustment *adjDelta = GTK_ADJUSTMENT(gtk_adjustment_new(xep->interval_width,0.0001,10,0.01,10,0));
	GtkWidget *deltaEnergyW = gtk_spin_button_new(GTK_ADJUSTMENT(adjDelta), 0.01, 3);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(deltaEnergyW), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 3);
	label = gtk_label_new("Interval width (keV)");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), deltaEnergyW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), table, TRUE, FALSE, 2);

	GtkWidget *transmissionW = gtk_check_button_new_with_label("Transmission tube");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(transmissionW), xep->transmission_tube);

	gtk_box_pack_start(GTK_BOX(mainVBox), transmissionW, TRUE, FALSE, 2);
	struct transmission_data *td = (struct transmission_data *) malloc(sizeof(struct transmission_data));
	g_signal_connect(G_OBJECT(transmissionW), "toggled", G_CALLBACK(transmission_clicked_cb), (gpointer) td);
	td->anodeDensityW = anodeDensityW;
	td->anodeThicknessW = anodeThicknessW;

	GtkWidget *transmissionEffW = gtk_check_button_new_with_label("Transmission efficiency file");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(transmissionEffW), strcmp(xep->transmission_efficiency_file, "(None)") == 0 ? FALSE : TRUE);
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), transmissionEffW, FALSE, FALSE, 0);
	GtkWidget *transmissionEffFileW = gtk_file_chooser_button_new("Select a transmission efficiency file", GTK_FILE_CHOOSER_ACTION_OPEN);
	gtk_widget_set_sensitive(transmissionEffFileW, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(transmissionEffW)));
	gtk_box_pack_start(GTK_BOX(hbox), transmissionEffFileW, TRUE, TRUE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);
	g_signal_connect(G_OBJECT(transmissionEffW), "toggled", G_CALLBACK(transmissioneff_clicked_cb), (gpointer) transmissionEffFileW);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(transmissionEffW))) {
		gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(transmissionEffFileW), xep->transmission_efficiency_file);
	}


	//buttons
	struct generate *gen = (struct generate*) malloc(sizeof(struct generate));
	gen->notebook = notebook;
	gen->excitation_tube = NULL;
	gen->excitation_nuclide = NULL;
	gtk_box_pack_start(GTK_BOX(mainVBox), gtk_hseparator_new(), FALSE, FALSE, 3);


	GtkWidget *linearW= gtk_radio_button_new_with_label_from_widget(NULL,"Linear scaling");
	GtkWidget *log10W = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(linearW), "Logarithmic scaling");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(log10W), xep->log10_active);
	gtk_box_pack_start(GTK_BOX(mainVBox), linearW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainVBox), log10W, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(linearW), "toggled",G_CALLBACK(linear_log10_toggled_cb), (gpointer) gen);
	g_free(xep->transmission_efficiency_file);
	g_free(xep);

	gtk_box_pack_start(GTK_BOX(mainVBox), gtk_hseparator_new(), FALSE, FALSE, 3);
	GtkWidget *okButton = gtk_button_new_from_stock(GTK_STOCK_OK);
	GtkWidget *cancelButton = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
	GtkWidget *generateButton = gtk_button_new_from_stock(GTK_STOCK_EXECUTE);
	GtkWidget *infoButton = gtk_button_new_from_stock(GTK_STOCK_ABOUT);
	GtkWidget *exportButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	GtkWidget *imageButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	g_signal_connect(G_OBJECT(exportButton), "clicked", G_CALLBACK(export_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(infoButton), "clicked", G_CALLBACK(info_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(cancelButton), "clicked", G_CALLBACK(cancel_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(generateButton), "clicked", G_CALLBACK(generate_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(okButton), "clicked", G_CALLBACK(ok_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(imageButton), "clicked", G_CALLBACK(image_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(window), "delete-event", G_CALLBACK(ebel_delete_event_cb), (gpointer) gen);
	update_button_text(generateButton, "Update spectrum");
	update_button_text(exportButton, "Export spectrum");
	update_button_text(imageButton, "Save image");
	GtkWidget *buttonbox = gtk_table_new(2, 3, TRUE);
	gtk_table_set_col_spacings(GTK_TABLE(buttonbox), 2);
	gtk_table_set_row_spacings(GTK_TABLE(buttonbox), 2);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , generateButton, 0, 1, 0, 1);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , exportButton, 1, 2, 0, 1);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , imageButton, 2, 3, 0, 1);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , okButton, 0, 1, 1, 2);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , cancelButton, 1, 2, 1, 2);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , infoButton, 2, 3, 1, 2);


	gtk_box_pack_start(GTK_BOX(mainVBox), buttonbox, FALSE, FALSE, 2);



	GtkWidget *mainHBox = gtk_hbox_new(FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainHBox), mainVBox, FALSE, FALSE, 1);
#ifdef HAVE_CXX
	Gtk::PLplot::Canvas *canvas = Gtk::manage(new Gtk::PLplot::Canvas());
	canvas->set_hexpand(true);
	canvas->set_vexpand(true);
	GtkWidget *aspect_frame = gtk_aspect_frame_new("", 0.5, 0.5, 842.0/595.0, FALSE);
	gtk_widget_set_hexpand(aspect_frame, TRUE);
	gtk_widget_set_vexpand(aspect_frame, TRUE);
	gtk_container_add(GTK_CONTAINER(aspect_frame), GTK_WIDGET(canvas->gobj()));
	gtk_box_pack_start(GTK_BOX(mainHBox), aspect_frame, TRUE, TRUE, 2);
#else
	GtkWidget *canvas = gtk_plot_canvas_new(GTK_PLOT_A4_H, GTK_PLOT_A4_W, 0.9);
	GTK_PLOT_CANVAS_UNSET_FLAGS(GTK_PLOT_CANVAS(canvas), (GtkPlotCanvasFlags) (GTK_PLOT_CANVAS_CAN_SELECT | GTK_PLOT_CANVAS_CAN_SELECT_ITEM)); //probably needs to be unset when initializing, but set when data is available
	gtk_plot_canvas_set_background(GTK_PLOT_CANVAS(canvas),&white_plot);
	gtk_box_pack_start(GTK_BOX(mainHBox),canvas,FALSE,FALSE,2);
#endif



	gtk_container_set_border_width(GTK_CONTAINER(mainHBox),5);
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">X-ray tube</span>");
	gtk_widget_show_all(mainHBox);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), mainHBox, label);

	gen->tubeVoltageW = tubeVoltageW;
	gen->transmissionW = transmissionW;
	gen->transmissionEffW = transmissionEffW;
	gen->transmissionEffFileW = transmissionEffFileW;
	gen->anodeMaterialW = anodeMaterialW;
	gen->anodeThicknessW = anodeThicknessW;
	gen->anodeDensityW = anodeDensityW;
	gen->filterMaterialW = filterMaterialW;
	gen->filterThicknessW = filterThicknessW;
	gen->filterDensityW = filterDensityW;
	gen->windowMaterialW = windowMaterialW;
	gen->windowThicknessW = windowThicknessW;
	gen->windowDensityW = windowDensityW;
	gen->alphaElectronW = alphaElectronW;
	gen->alphaXrayW = alphaXrayW;
	gen->deltaEnergyW = deltaEnergyW;
	gen->tubeSolidAngleW = tubeSolidAngleW;
	gen->tubeCurrentW = tubeCurrentW;
	gen->canvas_tube = canvas;
	gen->linear_tubeW = linearW;
	gen->log10_tubeW = log10W;



	//and now the radionuclides
	mainVBox = gtk_vbox_new(FALSE, 2);
#ifdef HAVE_CXX
	gtk_widget_set_hexpand(mainVBox, FALSE);
	gtk_widget_set_vexpand(mainVBox, FALSE);
#endif

	/*
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_NUCLIDE_LAST_USED, &xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(main_window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error getting preferences for last used radionuclide configuration\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
		return;
	}
	*/
	//struct xmi_nuclide_parameters *xnp = xpv.xnp;
	struct xmi_nuclide_parameters *xnp;

	GtkWidget *lilVBox = gtk_vbox_new(FALSE, 2);
	hbox = gtk_hbox_new(FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new("Radionuclide"), FALSE, FALSE, 2);
	GtkWidget *radioNuclideW = gtk_combo_box_text_new();

	gchar **nuclides;
	int nNuclides;
	nuclides = GetRadioNuclideDataList(&nNuclides);
	for (i = 0 ; i < nNuclides ; i++) {
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(radioNuclideW), nuclides[i]);
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(radioNuclideW), xnp->radioNuclide);
	g_strfreev(nuclides);
	gtk_box_pack_end(GTK_BOX(hbox), radioNuclideW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(lilVBox), hbox, FALSE, FALSE, 2);


	hbox = gtk_hbox_new(FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new("Activity"), FALSE, FALSE, 2);
	GtkWidget *activityW = gtk_entry_new();
	sprintf(buf, "%g", xnp->activity);
	gtk_entry_set_text(GTK_ENTRY(activityW), buf);

	GtkWidget *activityUnitW= gtk_combo_box_text_new();
	for (i = 0 ; i < 4 ; i++) {
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(activityUnitW), activity_units[i]);
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(activityUnitW), xnp->activityUnit);
	gtk_box_pack_end(GTK_BOX(hbox), activityUnitW, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), activityW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(lilVBox), hbox, FALSE, FALSE, 2);

	GtkWidget *nuclideSolidAngleW = gtk_entry_new();
	hbox = gtk_hbox_new(FALSE, 3);
	label = gtk_label_new("Source solid angle (sr)");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	GtkWidget *slitsButton2 = gtk_button_new_with_label("Get from slits");
	gtk_box_pack_end(GTK_BOX(hbox), slitsButton2, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), nuclideSolidAngleW, FALSE, FALSE, 2);
	g_signal_connect(G_OBJECT(slitsButton2), "clicked", G_CALLBACK(slits_button_clicked_cb), (gpointer) nuclideSolidAngleW);
	gtk_box_pack_start(GTK_BOX(lilVBox), hbox, TRUE, FALSE, 2);

	sprintf(buf, "%g", xnp->nuclide_solid_angle);
	gtk_entry_set_text(GTK_ENTRY(nuclideSolidAngleW), buf);


	gtk_box_pack_start(GTK_BOX(mainVBox), lilVBox, TRUE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(mainVBox), gtk_hseparator_new(), FALSE, FALSE, 3);


	linearW= gtk_radio_button_new_with_label_from_widget(NULL,"Linear scaling");
	log10W = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(linearW), "Logarithmic scaling");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(log10W), xnp->log10_active);
	gtk_box_pack_start(GTK_BOX(mainVBox), linearW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainVBox), log10W, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(linearW), "toggled",G_CALLBACK(linear_log10_toggled_cb), (gpointer) gen);
	g_free(xnp);

	gtk_box_pack_start(GTK_BOX(mainVBox), gtk_hseparator_new(), FALSE, FALSE, 3);

	okButton = gtk_button_new_from_stock(GTK_STOCK_OK);
	cancelButton = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
	generateButton = gtk_button_new_from_stock(GTK_STOCK_EXECUTE);
	infoButton = gtk_button_new_from_stock(GTK_STOCK_ABOUT);
	exportButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	imageButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	g_signal_connect(G_OBJECT(exportButton), "clicked", G_CALLBACK(export_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(infoButton), "clicked", G_CALLBACK(info_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(cancelButton), "clicked", G_CALLBACK(cancel_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(generateButton), "clicked", G_CALLBACK(generate_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(okButton), "clicked", G_CALLBACK(ok_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(imageButton), "clicked", G_CALLBACK(image_button_clicked_cb), (gpointer) gen);
	g_signal_connect(G_OBJECT(window), "delete-event", G_CALLBACK(ebel_delete_event_cb), (gpointer) gen);
	update_button_text(generateButton, "Update spectrum");
	update_button_text(exportButton, "Export spectrum");
	update_button_text(imageButton, "Save image");
	buttonbox = gtk_table_new(2, 3, TRUE);
	gtk_table_set_col_spacings(GTK_TABLE(buttonbox), 2);
	gtk_table_set_row_spacings(GTK_TABLE(buttonbox), 2);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , generateButton, 0, 1, 0, 1);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , exportButton, 1, 2, 0, 1);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , imageButton, 2, 3, 0, 1);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , okButton, 0, 1, 1, 2);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , cancelButton, 1, 2, 1, 2);
	gtk_table_attach_defaults(GTK_TABLE(buttonbox) , infoButton, 2, 3, 1, 2);


	gtk_box_pack_start(GTK_BOX(mainVBox), buttonbox, FALSE, FALSE, 2);

	mainHBox = gtk_hbox_new(FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainHBox), mainVBox, FALSE, FALSE, 1);
#ifdef HAVE_CXX
	canvas = Gtk::manage(new Gtk::PLplot::Canvas());
	canvas->set_hexpand(true);
	canvas->set_vexpand(true);
	aspect_frame = gtk_aspect_frame_new("", 0.5, 0.5, 842.0/595.0, FALSE);
	gtk_widget_set_hexpand(aspect_frame, TRUE);
	gtk_widget_set_vexpand(aspect_frame, TRUE);
	gtk_container_add(GTK_CONTAINER(aspect_frame), GTK_WIDGET(canvas->gobj()));
	gtk_box_pack_start(GTK_BOX(mainHBox), aspect_frame, TRUE, TRUE, 2);
#else
	canvas = gtk_plot_canvas_new(GTK_PLOT_A4_H, GTK_PLOT_A4_W, 0.9);
	GTK_PLOT_CANVAS_UNSET_FLAGS(GTK_PLOT_CANVAS(canvas), (GtkPlotCanvasFlags) (GTK_PLOT_CANVAS_CAN_SELECT | GTK_PLOT_CANVAS_CAN_SELECT_ITEM)); //probably needs to be unset when initializing, but set when data is available
	gtk_plot_canvas_set_background(GTK_PLOT_CANVAS(canvas),&white_plot);
	gtk_box_pack_end(GTK_BOX(mainHBox),canvas,FALSE,FALSE,2);
#endif

	gtk_container_set_border_width(GTK_CONTAINER(mainHBox),5);
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Radionuclide</span>");
	gtk_widget_show_all(mainHBox);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), mainHBox, label);


	gen->radioNuclideW = radioNuclideW;
	gen->nuclideSolidAngleW = nuclideSolidAngleW;
	gen->activityW = activityW;
	gen->activityUnitW = activityUnitW;
	gen->linear_nuclideW = linearW;
	gen->log10_nuclideW = log10W;
	gen->canvas_nuclide = canvas;
	gen->window = window;


	gtk_container_add(GTK_CONTAINER(window), notebook);


	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_SOURCES_LAST_USED, &xpv) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(main_window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_CLOSE,
		"Error getting preferences for Sources last used\nFatal error."
	       	);
	     	gtk_dialog_run (GTK_DIALOG (dialog));
	     	gtk_widget_destroy (dialog);
		return;
	}
	gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), xpv.i);

	gtk_widget_show_all(window);

	generate_tube_spectrum(gen);
	generate_nuclide_spectrum(gen);
}
