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
#include "xmimsim-gui-options-box.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-compat.h"
#include "xmi_detector.h"
#include <string.h>

struct _XmiMsimGuiOptionsBox {
	GtkBox parent_instance;
	GtkWidget *MlinesW;
	GtkWidget *rad_cascadeW;
	GtkWidget *nonrad_cascadeW;
	GtkWidget *variance_reductionW;
	GtkWidget *pile_upW;
	GtkWidget *poissonW;
	GtkWidget *escape_peaksW;
	GtkWidget *advanced_comptonW;
	GtkWidget *default_seedsW;
	GtkWidget *custom_detector_responseE;
	GtkWidget *custom_detector_responseB;
	GtkWidget *custom_detector_responseC;
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	GtkWidget *openclW;
#endif
};


struct _XmiMsimGuiOptionsBoxClass
{
	GtkBoxClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiOptionsBox, xmi_msim_gui_options_box, GTK_TYPE_BOX)

static void xmi_msim_gui_options_box_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_options_box_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_options_box_finalize(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_options_box_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_options_box_class_init(XmiMsimGuiOptionsBoxClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_options_box_dispose;
	object_class->finalize = xmi_msim_gui_options_box_finalize;
}

static void custom_detector_response_toggled_cb(GtkToggleButton *button, XmiMsimGuiOptionsBox *self) {
	if (gtk_toggle_button_get_active(button) == TRUE) {
		gtk_widget_set_sensitive(self->custom_detector_responseE, TRUE);
		gtk_widget_set_sensitive(self->custom_detector_responseB, TRUE);
	}
	else {
		gtk_widget_set_sensitive(self->custom_detector_responseE, FALSE);
		gtk_widget_set_sensitive(self->custom_detector_responseB, FALSE);
	}
}

static gboolean detector_response_dlm_filter(const GtkFileFilterInfo *filter_info, gpointer data) {
	GtkFileFilter *filter = gtk_file_filter_new();

	gtk_file_filter_add_pattern(filter, "*." G_MODULE_SUFFIX);
	if (gtk_file_filter_filter(filter, filter_info) == TRUE && xmi_check_detector_convolute_plugin((char *) filter_info->filename) == 1)
		return TRUE;

	return FALSE;
}

static void custom_detector_response_clicked_cb(GtkToggleButton *button, GtkWidget *entry) {
	XmiMsimGuiFileChooserDialog *dialog;
	GtkFileFilter *filter;
	gchar *filename;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_custom(filter, GTK_FILE_FILTER_FILENAME, detector_response_dlm_filter, NULL, NULL);
	gtk_file_filter_set_name(filter, "Detector response DLM");
	dialog = xmi_msim_gui_file_chooser_dialog_new("Select detector response function DLM",
		GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(button))),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		"document-open",
		"_Cancel"
		);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);
	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		gtk_entry_set_text(GTK_ENTRY(entry), filename);
		g_free(filename);
	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);
}

static void xmi_msim_gui_options_box_init(XmiMsimGuiOptionsBox *self) {

	g_object_set(
		self,
		"spacing", 2,
		"homogeneous", TRUE,
		"expand", FALSE,
		"orientation", GTK_ORIENTATION_VERTICAL,
		"border-width", 10,
		NULL
	);

	union xmimsim_prefs_val xpv;

	self->MlinesW = gtk_check_button_new_with_label("Simulate M-lines");
	gtk_widget_set_tooltip_text(self->MlinesW, "Enables the simulation of M-lines. Disabling this option may lead to a significant performance increase. Should always be enabled when high atomic number elements are present in the sample.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_M_LINES, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->MlinesW), xpv.b);
	gtk_box_pack_start(GTK_BOX(self), self->MlinesW, TRUE, FALSE, 0);

	self->rad_cascadeW = gtk_check_button_new_with_label("Simulate the radiative cascade effect");
	gtk_widget_set_tooltip_text(self->rad_cascadeW, "Enables the simulation of the radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_RAD_CASCADE, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->rad_cascadeW), xpv.b);
	gtk_box_pack_start(GTK_BOX(self), self->rad_cascadeW, TRUE, FALSE, 0);

	self->nonrad_cascadeW = gtk_check_button_new_with_label("Simulate the non-radiative cascade effect");
	gtk_widget_set_tooltip_text(self->nonrad_cascadeW, "Enables the simulation of the non-radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the non-radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NONRAD_CASCADE, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->nonrad_cascadeW), xpv.b);
	gtk_box_pack_start(GTK_BOX(self), self->nonrad_cascadeW, TRUE, FALSE, 0);

	self->variance_reductionW = gtk_check_button_new_with_label("Enable variance reduction techniques");
	gtk_widget_set_tooltip_text(self->variance_reductionW, "Disabling this option enables the brute-force method. Should only be used in combination with a high number of simulated photons.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->variance_reductionW), xpv.b);
	gtk_box_pack_start(GTK_BOX(self), self->variance_reductionW, TRUE, FALSE, 0);

	self->pile_upW = gtk_check_button_new_with_label("Enable pulse pile-up simulation");
	gtk_widget_set_tooltip_text(self->pile_upW, "When activated, will estimate detector electronics pulse pile-up. Determined by the pulse width parameter in Detector settings.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_PILE_UP, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->pile_upW), xpv.b);
	gtk_box_pack_start(GTK_BOX(self), self->pile_upW, TRUE, FALSE, 0);

	self->poissonW = gtk_check_button_new_with_label("Enable Poisson noise generation");
	gtk_widget_set_tooltip_text(self->poissonW, "Enabling this feature will add noise according to a Poisson distribution the convoluted spectra");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_POISSON, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->poissonW), xpv.b);
	gtk_box_pack_start(GTK_BOX(self), self->poissonW, TRUE, FALSE, 0);

	self->escape_peaksW = gtk_check_button_new_with_label("Enable escape peaks support");
	gtk_widget_set_tooltip_text(self->escape_peaksW, "Enabling this feature will add fluorescence and Compton escape peaks to the convoluted spectra");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_ESCAPE_PEAKS, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->escape_peaksW), xpv.b);
	gtk_box_pack_start(GTK_BOX(self), self->escape_peaksW, TRUE, FALSE, 0);

	self->advanced_comptonW = gtk_check_button_new_with_label("Enable advanced Compton scattering simulation");
	gtk_widget_set_tooltip_text(self->advanced_comptonW, "Enabling this feature will improve the simulation of the Compton scattering, and add support for the Compton fluorescence photons. Warning: due to the added complexity, the code will slow down considerably (at least a factor of 2, and increases with higher atomic number)");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_ADVANCED_COMPTON, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->advanced_comptonW), xpv.b);
	gtk_box_pack_start(GTK_BOX(self), self->advanced_comptonW, TRUE, FALSE, 0);

	self->default_seedsW = gtk_check_button_new_with_label("Enable default seeds support");
	gtk_widget_set_tooltip_text(self->default_seedsW, "Enabling this feature will set the seeds that will be used during the simulation to default values, instead of random ones. This is useful when exactly reproducible simulation results are required, usually for testing purposes");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_DEFAULT_SEEDS, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->default_seedsW), xpv.b);
	gtk_box_pack_start(GTK_BOX(self), self->default_seedsW, TRUE, FALSE, 0);

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	self->openclW = gtk_check_button_new_with_label("Enable OpenCL");
	gtk_widget_set_tooltip_text(self->openclW, "Enabling OpenCL will have the simulation use the GPU in order to calculate the solid angle grids, resulting in considerably speed-up. Requires the installation of OpenCL drivers. Consult the website of the manufacturer of your videocard for more information");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_OPENCL, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->openclW), xpv.b);
	gtk_box_pack_start(GTK_BOX(self), self->openclW, TRUE, FALSE, 0);
#endif

	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_set_homogeneous(GTK_BOX(hbox), FALSE);
	gtk_box_pack_start(GTK_BOX(self), hbox, TRUE, FALSE, 0);
	self->custom_detector_responseC = gtk_check_button_new_with_label("Custom detector response");
	gtk_widget_set_tooltip_text(self->custom_detector_responseC, "Loads an alternative detector response routine from a dynamically loadable module. This module must export a function called \"xmi_detector_convolute_all_custom\". More information can be found in the manual");
	g_signal_connect(G_OBJECT(self->custom_detector_responseC), "toggled", G_CALLBACK(custom_detector_response_toggled_cb), self);
	gtk_box_pack_start(GTK_BOX(hbox), self->custom_detector_responseC, FALSE, FALSE, 0);
	self->custom_detector_responseE = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->custom_detector_responseE), FALSE);
	gtk_box_pack_start(GTK_BOX(hbox), self->custom_detector_responseE, TRUE, TRUE, 3);
	self->custom_detector_responseB = gtk_button_new_from_icon_name("document-open", GTK_ICON_SIZE_BUTTON);
	g_signal_connect(G_OBJECT(self->custom_detector_responseB), "clicked", G_CALLBACK(custom_detector_response_clicked_cb), self->custom_detector_responseE);
	gtk_box_pack_end(GTK_BOX(hbox), self->custom_detector_responseB, FALSE, FALSE, 0);
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_CUSTOM_DETECTOR_RESPONSE, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	if (xpv.s != NULL) {
		gtk_widget_set_sensitive(self->custom_detector_responseE, TRUE);
		gtk_widget_set_sensitive(self->custom_detector_responseB, TRUE);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->custom_detector_responseC), TRUE);
		gtk_entry_set_text(GTK_ENTRY(self->custom_detector_responseE), xpv.s);
	}
	else {
		gtk_widget_set_sensitive(self->custom_detector_responseE, FALSE);
		gtk_widget_set_sensitive(self->custom_detector_responseB, FALSE);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->custom_detector_responseC), FALSE);
	}
	g_free(xpv.s);
}

GtkWidget* xmi_msim_gui_options_box_new(void) {
	return GTK_WIDGET(g_object_new(XMI_MSIM_GUI_TYPE_OPTIONS_BOX, NULL));
}

struct xmi_main_options* xmi_msim_gui_options_box_get_options(XmiMsimGuiOptionsBox *self) {
	struct xmi_main_options *rv = g_malloc(sizeof(struct xmi_main_options));

	*rv = xmi_get_default_main_options();

	rv->use_M_lines = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->MlinesW));
	rv->use_cascade_radiative = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->rad_cascadeW));
	rv->use_cascade_auger =  gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->nonrad_cascadeW));
	rv->use_variance_reduction = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->variance_reductionW));
	rv->use_sum_peaks = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->pile_upW));
	rv->use_poisson = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->poissonW));
	rv->use_escape_peaks = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->escape_peaksW));
	rv->use_advanced_compton = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->advanced_comptonW));
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	rv->use_opencl = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->openclW));
#endif
	rv->use_default_seeds = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->default_seedsW));
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->custom_detector_responseC)) == TRUE &&
		strlen(gtk_entry_get_text(GTK_ENTRY(self->custom_detector_responseE))) > 0) {
		rv->custom_detector_response = g_strdup(gtk_entry_get_text(GTK_ENTRY(self->custom_detector_responseE)));
	}
	else
		rv->custom_detector_response = NULL;

	return rv;
}

void xmi_msim_gui_options_box_save_to_prefs(XmiMsimGuiOptionsBox *self) {
	union xmimsim_prefs_val xpv;

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->MlinesW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_M_LINES, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->rad_cascadeW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_RAD_CASCADE, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->nonrad_cascadeW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_NONRAD_CASCADE, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->variance_reductionW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->pile_upW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_PILE_UP, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->poissonW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_POISSON, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->escape_peaksW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_ESCAPE_PEAKS, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->advanced_comptonW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_ADVANCED_COMPTON, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->default_seedsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_DEFAULT_SEEDS, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->openclW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_OPENCL, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
#endif
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->custom_detector_responseC)) == TRUE &&
		strlen(gtk_entry_get_text(GTK_ENTRY(self->custom_detector_responseE))) > 0) {
		xpv.s = g_strdup(gtk_entry_get_text(GTK_ENTRY(self->custom_detector_responseE)));
	}
	else
		xpv.s = NULL;
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_CUSTOM_DETECTOR_RESPONSE, xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	g_free(xpv.s);
}
