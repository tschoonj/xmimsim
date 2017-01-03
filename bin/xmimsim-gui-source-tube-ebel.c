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
#include "xmimsim-gui-source-tube-ebel.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include "xmi_ebel.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <xraylib.h>
#include <string.h>
#include <stdlib.h>
#include "xmimsim-gui-spline.h"
#include <stdio.h>
#include <math.h>

struct xmi_ebel_parameters {
	double tube_voltage;
	double tube_current;
	double tube_solid_angle;
	double alpha_electron;
	double alpha_xray;
	double interval_width;
	int anode_Z;
	double anode_rho;
	double anode_thickness;
	int window_Z;
	double window_rho;
	double window_thickness;
	int filter_Z;
	double filter_rho;
	double filter_thickness;
	gboolean transmission_tube;
	gchar *transmission_efficiency_file;
};

XMI_MSIM_GUI_DEFINE_DYNAMIC_SOURCE_TYPE(XmiMsimGuiSourceTubeEbel, xmi_msim_gui_source_tube_ebel, XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT)

static void xmi_msim_gui_source_tube_ebel_real_generate(XmiMsimGuiSourceAbstract *source);

static const gchar *xmi_msim_gui_source_tube_ebel_real_get_name(XmiMsimGuiSourceAbstract *source);

static const gchar *xmi_msim_gui_source_tube_ebel_real_get_about_text(XmiMsimGuiSourceAbstract *source);

static void xmi_msim_gui_source_tube_ebel_dispose(GObject *object);

static void xmi_msim_gui_source_tube_ebel_finalize(GObject *object);

static void slits_button_clicked_cb(XmiMsimGuiSourceTubeEbel *source);

static void material_changed_cb(GtkComboBox *widget, GtkWidget *densityW);

static void transmissioneff_clicked_cb(GtkToggleButton *button, GtkWidget *filechooser);

static void transmission_clicked_cb(XmiMsimGuiSourceTubeEbel *source, GtkToggleButton *button);

static void xmi_msim_gui_source_tube_ebel_class_init(XmiMsimGuiSourceTubeEbelClass *klass) {

	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_source_tube_ebel_dispose;
	object_class->finalize = xmi_msim_gui_source_tube_ebel_finalize;

	XmiMsimGuiSourceAbstractClass *parent_klass = XMI_MSIM_GUI_SOURCE_ABSTRACT_CLASS(klass);

	parent_klass->generate = xmi_msim_gui_source_tube_ebel_real_generate;
	//parent_klass->save = xmi_msim_gui_source_tube_ebel_real_save; // do not override
	parent_klass->get_name = xmi_msim_gui_source_tube_ebel_real_get_name;
	parent_klass->get_about_text = xmi_msim_gui_source_tube_ebel_real_get_about_text;
}

static struct xmi_ebel_parameters* get_parameters(XmiMsimGuiSourceTubeEbel *source, GError **error) {
	struct xmi_ebel_parameters *xep = (struct xmi_ebel_parameters *) g_malloc(sizeof(struct xmi_ebel_parameters));

	xep->tube_voltage = gtk_spin_button_get_value(GTK_SPIN_BUTTON(source->tubeVoltageW));
	xep->tube_current = gtk_spin_button_get_value(GTK_SPIN_BUTTON(source->tubeCurrentW));

	const gchar *text = gtk_entry_get_text(GTK_ENTRY(source->tubeSolidAngleW));
	gchar *endPtr;
	xep->tube_solid_angle = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->tube_solid_angle <= 0.0 || xep->tube_solid_angle >= 2*M_PI) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_INVALID_DATA, "Invalid tube solid angle: must be greater than zero and less than 2\317\200");
		g_free(xep);
		return NULL;
	}

	xep->alpha_electron = gtk_spin_button_get_value(GTK_SPIN_BUTTON(source->alphaElectronW));
	xep->alpha_xray = gtk_spin_button_get_value(GTK_SPIN_BUTTON(source->alphaXrayW));
	xep->interval_width = gtk_spin_button_get_value(GTK_SPIN_BUTTON(source->deltaEnergyW));
	xep->anode_Z = gtk_combo_box_get_active(GTK_COMBO_BOX(source->anodeMaterialW))+1;

	xep->transmission_tube = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(source->transmissionW));

	text = gtk_entry_get_text(GTK_ENTRY(source->anodeDensityW));
	xep->anode_rho = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->anode_rho <= 0.0) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_INVALID_DATA, "Invalid anode density: must be greater than zero");
		g_free(xep);
		return NULL;
	}
	text = gtk_entry_get_text(GTK_ENTRY(source->anodeThicknessW));
	xep->anode_thickness = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->anode_thickness <= 0.0) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_INVALID_DATA, "Invalid anode thickness: must be greater than zero");
		g_free(xep);
		return NULL;
	}

	text = gtk_entry_get_text(GTK_ENTRY(source->windowDensityW));
	xep->window_rho = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->window_rho < 0.0) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_INVALID_DATA, "Invalid window density: must be greater than or equal to zero");
		g_free(xep);
		return NULL;
	}
	text = gtk_entry_get_text(GTK_ENTRY(source->windowThicknessW));
	xep->window_thickness = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->window_thickness < 0.0) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_INVALID_DATA, "Invalid window thickness: must be greater than or equal to zero");
		g_free(xep);
		return NULL;
	}
	xep->window_Z = gtk_combo_box_get_active(GTK_COMBO_BOX(source->windowMaterialW))+1;


	text = gtk_entry_get_text(GTK_ENTRY(source->filterDensityW));
	xep->filter_rho = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->window_rho < 0.0) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_INVALID_DATA, "Invalid filter density: must be greater than or equal to zero");
		g_free(xep);
		return NULL;
	}
	text = gtk_entry_get_text(GTK_ENTRY(source->filterThicknessW));
	xep->filter_thickness = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xep->window_thickness < 0.0) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_INVALID_DATA, "Invalid filter thickness: must be greater than or equal to zero");
		g_free(xep);
		return NULL;
	}
	xep->filter_Z = gtk_combo_box_get_active(GTK_COMBO_BOX(source->filterMaterialW))+1;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(source->transmissionEffW))) {
		gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(source->transmissionEffFileW));
		if (filename == NULL || strlen(filename) == 0) {
			g_set_error(error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_INVALID_DATA, "Invalid transmission efficiency filename");
			g_free(xep);
			return NULL;
		}
		xep->transmission_efficiency_file = filename;
	}
	else
		xep->transmission_efficiency_file = g_strdup("(None)");

	return xep;
}

static void set_parameters(XmiMsimGuiSourceTubeEbel *source, struct xmi_ebel_parameters *xep) {
	gchar *buf;

	gtk_spin_button_set_value(GTK_SPIN_BUTTON(source->tubeVoltageW), xep->tube_voltage);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(source->tubeCurrentW), xep->tube_current);

	buf = g_strdup_printf("%g", xep->tube_solid_angle);
	gtk_entry_set_text(GTK_ENTRY(source->tubeSolidAngleW), buf);
	g_free(buf);

	gtk_combo_box_set_active(GTK_COMBO_BOX(source->anodeMaterialW), xep->anode_Z-1);
	gtk_widget_set_sensitive(source->anodeThicknessW, xep->transmission_tube);
	gtk_widget_set_sensitive(source->anodeDensityW, xep->transmission_tube);

	buf = g_strdup_printf("%g", xep->anode_rho);
	gtk_entry_set_text(GTK_ENTRY(source->anodeDensityW), buf);
	g_free(buf);

	buf = g_strdup_printf("%g", xep->anode_thickness);
	gtk_entry_set_text(GTK_ENTRY(source->anodeThicknessW), buf);
	g_free(buf);

	gtk_combo_box_set_active(GTK_COMBO_BOX(source->windowMaterialW), xep->window_Z-1);

	buf = g_strdup_printf("%g", xep->window_rho);
	gtk_entry_set_text(GTK_ENTRY(source->windowDensityW), buf);
	g_free(buf);
	buf = g_strdup_printf("%g", xep->window_thickness);
	gtk_entry_set_text(GTK_ENTRY(source->windowThicknessW), buf);
	g_free(buf);

	gtk_combo_box_set_active(GTK_COMBO_BOX(source->filterMaterialW), xep->filter_Z-1);

	buf = g_strdup_printf("%g", xep->filter_rho);
	gtk_entry_set_text(GTK_ENTRY(source->filterDensityW), buf);
	g_free(buf);
	buf = g_strdup_printf("%g", xep->filter_thickness);
	gtk_entry_set_text(GTK_ENTRY(source->filterThicknessW), buf);
	g_free(buf);

	gtk_spin_button_set_value(GTK_SPIN_BUTTON(source->alphaElectronW), xep->alpha_electron);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(source->alphaXrayW), xep->alpha_xray);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(source->deltaEnergyW), xep->interval_width);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(source->transmissionW), xep->transmission_tube);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(source->transmissionEffW), strcmp(xep->transmission_efficiency_file, "(None)") == 0 ? FALSE : TRUE);
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(source->transmissionEffW))) {
		gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(source->transmissionEffFileW), xep->transmission_efficiency_file);
	}
}

static void set_preferences(struct xmi_ebel_parameters *xep) {
	gchar *prefs_file;
	GKeyFile *keyfile;

	prefs_file = xmimsim_gui_get_preferences_filename();

	keyfile = g_key_file_new();

	if (!g_key_file_load_from_file(keyfile, prefs_file, (GKeyFileFlags) (G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS), NULL)) {
		if (!xmimsim_gui_create_prefs_file(keyfile, prefs_file)) {
			g_error("Could not create preferences file %s with default settings!", prefs_file);
			return;
		}
	}

	g_key_file_set_double(keyfile, "Ebel last used", "Tube voltage", xep->tube_voltage);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube current", xep->tube_current);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube solid angle", xep->tube_solid_angle);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube alpha electron", xep->alpha_electron);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube alpha xray", xep->alpha_xray);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube interval width", xep->interval_width);
	g_key_file_set_integer(keyfile, "Ebel last used", "Tube anode element", xep->anode_Z);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube anode density", xep->anode_rho);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube anode thickness", xep->anode_thickness);
	g_key_file_set_integer(keyfile, "Ebel last used", "Tube filter element", xep->filter_Z);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube filter density", xep->filter_rho);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube filter thickness", xep->filter_thickness);
	g_key_file_set_integer(keyfile, "Ebel last used", "Tube window element", xep->window_Z);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube window density", xep->window_rho);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube window thickness", xep->window_thickness);
	g_key_file_set_boolean(keyfile, "Ebel last used", "Tube transmission mode", xep->transmission_tube);
	g_key_file_set_string(keyfile, "Ebel last used", "Tube transmission efficiency file", xep->transmission_efficiency_file);

	//save file
	gchar *prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
	GError *error = NULL;
	if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, &error)) {
		g_error("Could not write to %s: %s\n", prefs_file, error->message);
	}
	g_free(prefs_file_contents);
	g_free(prefs_file);
	g_key_file_free(keyfile);

	return;
} 

static struct xmi_ebel_parameters* get_preferences() {
	struct xmi_ebel_parameters *xep = (struct xmi_ebel_parameters *) g_malloc(sizeof(struct xmi_ebel_parameters));

	gchar *prefs_file;
	GKeyFile *keyfile;

	prefs_file = xmimsim_gui_get_preferences_filename();

	keyfile = g_key_file_new();

	if (!g_key_file_load_from_file(keyfile, prefs_file, (GKeyFileFlags) (G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS), NULL)) {
		if (!xmimsim_gui_create_prefs_file(keyfile, prefs_file))
			g_error("Could not create preferences file %s with default settings!", prefs_file);
	}
	
	gboolean update_file = FALSE;
	GError *error = NULL;

	xep->tube_voltage = g_key_file_get_double(keyfile, "Ebel last used", "Tube voltage", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube voltage not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube voltage", 40.0);
		xep->tube_voltage = 40.0;
		error = NULL;
		update_file = TRUE;
	}
	xep->tube_current = g_key_file_get_double(keyfile, "Ebel last used", "Tube current", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube current not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube current", 1.0);
		xep->tube_current = 1.0;
		error = NULL;
		update_file = TRUE;
	}
	xep->tube_solid_angle = g_key_file_get_double(keyfile, "Ebel last used", "Tube solid angle", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube solid angle not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube solid angle", 1E-10);
		xep->tube_solid_angle = 1E-10;
		error = NULL;
		update_file = TRUE;
	}
	xep->alpha_electron = g_key_file_get_double(keyfile, "Ebel last used", "Tube alpha electron", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube alpha electron not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube alpha electron", 60.0);
		xep->alpha_electron = 60.0;
		error = NULL;
		update_file = TRUE;
	}
	xep->alpha_xray = g_key_file_get_double(keyfile, "Ebel last used", "Tube alpha xray", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube alpha xray not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube alpha xray", 60.0);
		xep->alpha_xray = 60.0;
		error = NULL;
		update_file = TRUE;
	}
	xep->interval_width = g_key_file_get_double(keyfile, "Ebel last used", "Tube interval width", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube interval width not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube interval width", 0.1);
		xep->interval_width = 0.1;
		error = NULL;
		update_file = TRUE;
	}
	xep->anode_Z = g_key_file_get_integer(keyfile, "Ebel last used", "Tube anode element", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube anode element not found in preferences file\n");
		g_key_file_set_integer(keyfile, "Ebel last used", "Tube anode element", 47);
		xep->anode_Z = 47;
		error = NULL;
		update_file = TRUE;
	}
	xep->anode_rho = g_key_file_get_double(keyfile, "Ebel last used", "Tube anode density", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube anode density not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube anode density", 10.5);
		xep->anode_rho = 10.5;
		error = NULL;
		update_file = TRUE;
	}
	xep->anode_thickness = g_key_file_get_double(keyfile, "Ebel last used", "Tube anode thickness", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube anode thickness not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube anode thickness", 0.0002);
		xep->anode_thickness = 0.0002;
		error = NULL;
		update_file = TRUE;
	}
	xep->window_Z = g_key_file_get_integer(keyfile, "Ebel last used", "Tube window element", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube window element not found in preferences file\n");
		g_key_file_set_integer(keyfile, "Ebel last used", "Tube window element", 4);
		xep->window_Z= 4;
		error = NULL;
		update_file = TRUE;
	}
	xep->window_rho = g_key_file_get_double(keyfile, "Ebel last used", "Tube window density", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube window density not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube window density", 1.848);
		xep->window_rho = 1.848;
		update_file = TRUE;
		error = NULL;
	}
	xep->window_thickness = g_key_file_get_double(keyfile, "Ebel last used", "Tube window thickness", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube window thickness not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube window thickness", 0.0125);
		xep->window_thickness = 0.0125;
		error = NULL;
		update_file = TRUE;
	}
	xep->filter_Z = g_key_file_get_integer(keyfile, "Ebel last used", "Tube filter element", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube filter element not found in preferences file\n");
		g_key_file_set_integer(keyfile, "Ebel last used", "Tube filter element", 2);
		xep->filter_Z = 2;
		error = NULL;
		update_file = TRUE;
	}
	xep->filter_rho = g_key_file_get_double(keyfile, "Ebel last used", "Tube filter density", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube filter density not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube filter density", 0.000166);
		xep->filter_rho = 0.000166;
		error = NULL;
		update_file = TRUE;
	}
	xep->filter_thickness = g_key_file_get_double(keyfile, "Ebel last used", "Tube filter thickness", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube filter thickness not found in preferences file\n");
		g_key_file_set_double(keyfile, "Ebel last used", "Tube filter thickness", 0);
		xep->filter_thickness = 0;
		error = NULL;
		update_file = TRUE;
	}
	xep->transmission_tube = g_key_file_get_boolean(keyfile, "Ebel last used", "Tube transmission mode", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube transmission mode not found in preferences file\n");
		g_key_file_set_boolean(keyfile, "Ebel last used", "Tube transmission mode", FALSE);
		xep->transmission_tube = FALSE;
		error = NULL;
		update_file = TRUE;
	}
	xep->transmission_efficiency_file= g_key_file_get_string(keyfile, "Ebel last used", "Tube transmission efficiency file", &error);
	if (error != NULL) {
		g_warning("Ebel last used Tube transmission efficiency file not found in preferences file\n");
		g_key_file_set_string(keyfile, "Ebel last used", "Tube transmission efficiency file", "(None)");
		xep->transmission_efficiency_file = g_strdup("(None)");
		error = NULL;
		update_file = TRUE;
	}
	if (update_file) {
		//save file
		gchar *prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
		if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, &error)) {
			g_error("Could not write to %s: %s\n", prefs_file, error->message);
		}
		g_free(prefs_file_contents);
	}
	g_free(prefs_file);
	g_key_file_free(keyfile);
	return xep;
}


static void xmi_msim_gui_source_tube_ebel_init(XmiMsimGuiSourceTubeEbel *source) {
	source->dispose_called = FALSE;

	// construct the widgets, and set them to their values as we go along...
	GtkWidget *mainVBox = GTK_WIDGET(source);

	GtkWidget *label;
	GtkWidget *hbox;

	label = gtk_label_new("Tube voltage (kV)");
	source->tubeVoltageW = gtk_spin_button_new_with_range(5, 100, 1.0);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(source->tubeVoltageW), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), source->tubeVoltageW, FALSE, FALSE, 2);
	gtk_widget_show_all(hbox);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);

	label = gtk_label_new("Tube current (mA)");
	source->tubeCurrentW = gtk_spin_button_new_with_range(0.001, 1000, 0.1);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(source->tubeCurrentW), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), source->tubeCurrentW, FALSE, FALSE, 2);
	gtk_widget_show_all(hbox);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);

	source->tubeSolidAngleW = gtk_entry_new();
	hbox = gtk_hbox_new(FALSE, 3);
	label = gtk_label_new("Tube solid angle (sr)");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	GtkWidget *slitsButton = gtk_button_new_with_label("Get from slits");
	gtk_box_pack_end(GTK_BOX(hbox), slitsButton, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), source->tubeSolidAngleW, FALSE, FALSE, 2);
	gtk_widget_show_all(hbox);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);

	GtkWidget *table = gtk_table_new(4, 4, FALSE);
	gtk_table_set_row_spacings(GTK_TABLE(table), 2);
	gtk_table_set_col_spacings(GTK_TABLE(table), 2);

	//row 0
	gtk_table_attach(GTK_TABLE(table), gtk_label_new("Material"), 1, 2, 0, 1, GTK_EXPAND, GTK_EXPAND, 1, 1);
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label),"Density (g/cm<sup>3</sup>)");
	gtk_table_attach(GTK_TABLE(table), label, 2, 3, 0, 1, GTK_EXPAND, GTK_EXPAND, 1, 1);
	gtk_table_attach(GTK_TABLE(table), gtk_label_new("Thickness (cm)"), 3, 4, 0, 1, GTK_EXPAND, GTK_EXPAND, 1, 1);

	//row 1
	gtk_table_attach(GTK_TABLE(table), gtk_label_new("Anode"), 0, 1, 1, 2, GTK_EXPAND, GTK_EXPAND, 1, 1);
	source->anodeDensityW = gtk_entry_new();
	source->anodeMaterialW = gtk_combo_box_text_new();
	int i;
	gchar *symbol;
	for (i = 1 ; i <= 94 ; i++) {
		symbol = AtomicNumberToSymbol(i);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(source->anodeMaterialW), symbol);
		xrlFree(symbol);
	}
	gtk_table_attach(GTK_TABLE(table), source->anodeMaterialW, 1, 2, 1, 2, GTK_EXPAND, GTK_EXPAND, 1, 1);
	gtk_table_attach(GTK_TABLE(table), source->anodeDensityW, 2, 3, 1, 2, GTK_EXPAND, GTK_EXPAND, 1, 1);
	source->anodeThicknessW = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), source->anodeThicknessW, 3, 4, 1, 2, GTK_EXPAND, GTK_EXPAND, 1, 1);

	//row 2
	gtk_table_attach(GTK_TABLE(table), gtk_label_new("Window"), 0, 1, 2, 3, GTK_EXPAND, GTK_EXPAND, 1, 1);
	source->windowMaterialW = gtk_combo_box_text_new();
	source->windowDensityW = gtk_entry_new();
	g_signal_connect(G_OBJECT(source->windowMaterialW), "changed", G_CALLBACK(material_changed_cb), (gpointer) source->windowDensityW);
	for (i = 1 ; i <= 94 ; i++) {
		symbol = AtomicNumberToSymbol(i);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(source->windowMaterialW), symbol);
		xrlFree(symbol);
	}
	gtk_table_attach(GTK_TABLE(table), source->windowMaterialW, 1, 2, 2, 3, GTK_EXPAND, GTK_EXPAND, 1, 1);
	gtk_table_attach(GTK_TABLE(table), source->windowDensityW, 2, 3, 2, 3, GTK_EXPAND, GTK_EXPAND, 1, 1);
	source->windowThicknessW = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), source->windowThicknessW, 3, 4, 2, 3, GTK_EXPAND, GTK_EXPAND, 1, 1);

	//row 3
	gtk_table_attach(GTK_TABLE(table), gtk_label_new("Filter"), 0, 1, 3, 4, GTK_EXPAND, GTK_EXPAND, 1, 1);
	source->filterMaterialW = gtk_combo_box_text_new();
	source->filterDensityW = gtk_entry_new();
	g_signal_connect(G_OBJECT(source->filterMaterialW), "changed", G_CALLBACK(material_changed_cb), (gpointer) source->filterDensityW);
	for (i = 1 ; i <= 94 ; i++) {
		symbol = AtomicNumberToSymbol(i);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(source->filterMaterialW), symbol);
		xrlFree(symbol);
	}
	gtk_table_attach(GTK_TABLE(table), source->filterMaterialW, 1, 2, 3, 4, GTK_EXPAND, GTK_EXPAND, 1, 1);
	gtk_table_attach(GTK_TABLE(table), source->filterDensityW, 2, 3, 3, 4, GTK_EXPAND, GTK_EXPAND, 1, 1);
	source->filterThicknessW = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), source->filterThicknessW, 3, 4, 3, 4, GTK_EXPAND, GTK_EXPAND, 1, 1);

	source->alphaElectronW = gtk_spin_button_new_with_range(50, 90, 1);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(source->alphaElectronW), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 3);
	label = gtk_label_new("Electron incidence angle (degrees)");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), source->alphaElectronW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);
	gtk_widget_show_all(hbox);

	source->alphaXrayW = gtk_spin_button_new_with_range(50, 90, 1);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(source->alphaXrayW), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 3);
	label = gtk_label_new("X-ray take-off angle (degrees)");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), source->alphaXrayW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);
	gtk_widget_show_all(hbox);

	source->deltaEnergyW = gtk_spin_button_new_with_range(0.0001, 10.0, 0.01);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(source->deltaEnergyW), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 3);
	label = gtk_label_new("Interval width (keV)");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), source->deltaEnergyW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);
	gtk_widget_show_all(hbox);
	gtk_box_pack_start(GTK_BOX(mainVBox), table, TRUE, FALSE, 2);
	gtk_widget_show_all(table);

	source->transmissionW = gtk_check_button_new_with_label("Transmission tube");

	gtk_box_pack_start(GTK_BOX(mainVBox), source->transmissionW, TRUE, FALSE, 2);
	gtk_widget_show_all(source->transmissionW);

	source->transmissionEffW = gtk_check_button_new_with_label("Transmission efficiency file");
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), source->transmissionEffW, FALSE, FALSE, 0);
	source->transmissionEffFileW = gtk_file_chooser_button_new("Select a transmission efficiency file", GTK_FILE_CHOOSER_ACTION_OPEN);
	gtk_widget_set_sensitive(source->transmissionEffFileW, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(source->transmissionEffW)));
	gtk_box_pack_start(GTK_BOX(hbox), source->transmissionEffFileW, TRUE, TRUE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, TRUE, FALSE, 2);
	gtk_widget_show_all(hbox);

	// load the preferences
	struct xmi_ebel_parameters *xep = get_preferences();
	
	set_parameters(source, xep);

	g_free(xep->transmission_efficiency_file);
	g_free(xep);

	// connect signal handlers
	g_signal_connect(G_OBJECT(source->anodeMaterialW), "changed", G_CALLBACK(material_changed_cb), (gpointer) source->anodeDensityW);
	g_signal_connect_swapped(G_OBJECT(slitsButton), "clicked", G_CALLBACK(slits_button_clicked_cb), (gpointer) source);
	g_signal_connect_swapped(G_OBJECT(source->transmissionW), "toggled", G_CALLBACK(transmission_clicked_cb), (gpointer) source);
	g_signal_connect(G_OBJECT(source->transmissionEffW), "toggled", G_CALLBACK(transmissioneff_clicked_cb), (gpointer) source->transmissionEffFileW);
}

static void slits_button_clicked_cb(XmiMsimGuiSourceTubeEbel *source) {
	//calculate solid angle based on slits
	double solid_angle = xmi_msim_gui_utils_get_solid_angle_from_slits(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->current->geometry);

	gchar *buf = g_strdup_printf("%g", solid_angle);
	gtk_entry_set_text(GTK_ENTRY(source->tubeSolidAngleW), buf);
	g_free(buf);
	return;
}

static void material_changed_cb(GtkComboBox *widget, GtkWidget *densityW) {
	int Z = gtk_combo_box_get_active(widget)+1;
	float density = ElementDensity(Z);

	gchar *buf = g_strdup_printf("%g", density);
	gtk_entry_set_text(GTK_ENTRY(densityW), buf);
	g_free(buf);
}

static void transmissioneff_clicked_cb(GtkToggleButton *button, GtkWidget *filechooser) {
	gtk_widget_set_sensitive(filechooser, gtk_toggle_button_get_active(button));

	return;
}

static void transmission_clicked_cb(XmiMsimGuiSourceTubeEbel *source, GtkToggleButton *button) {
	gtk_widget_set_sensitive(source->anodeDensityW, gtk_toggle_button_get_active(button));
	gtk_widget_set_sensitive(source->anodeThicknessW, gtk_toggle_button_get_active(button));

	return;
}

static struct xmi_layer* create_layer(int Z, double rho, double thickness) {
	struct xmi_layer* rv = (struct xmi_layer *) malloc(sizeof(struct xmi_layer));
	rv->n_elements = 1;
	rv->Z = (int *) malloc(sizeof(int));
	rv->Z[0] = Z;
	rv->weight = (double *) malloc(sizeof(double));
	rv->weight[0] = 1.0;
	rv->density = rho;
	rv->thickness = thickness;

	return rv;
}

static void xmi_msim_gui_source_tube_ebel_real_generate(XmiMsimGuiSourceAbstract *source) {
	GError *error = NULL;

	// read the parameters
	struct xmi_ebel_parameters *xep = get_parameters(XMI_MSIM_GUI_SOURCE_TUBE_EBEL(source), &error);

	if (xep == NULL) {
		g_signal_emit_by_name((gpointer) source, "after-generate", error);
		return;
	}

	struct xmi_layer *anode = create_layer(xep->anode_Z, xep->anode_rho, xep->anode_thickness);
	struct xmi_layer *window = xep->window_thickness > 0 && xep->window_rho > 0 ? create_layer(xep->window_Z, xep->window_rho, xep->window_thickness) : NULL;
	struct xmi_layer *filter = xep->filter_thickness > 0 && xep->filter_rho > 0 ? create_layer(xep->filter_Z, xep->filter_rho, xep->filter_thickness) : NULL;

	struct xmi_excitation* excitation_tube = NULL;

	// prepare arguments for the fortran function call
	int ebel_rv = xmi_tube_ebel(anode, window, filter, xep->tube_voltage, xep->tube_current, xep->alpha_electron, xep->alpha_xray, xep->interval_width, xep->tube_solid_angle, xep->transmission_tube, &excitation_tube);

	xmi_free_layer(anode);
	free(anode);
	if (window != NULL) {
		xmi_free_layer(window);
		free(window);
	}
	if (filter != NULL) {
		xmi_free_layer(filter);
		free(filter);
	}

	if (ebel_rv == 0) {
		g_set_error(&error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_GENERATE, "Error generating spectrum: see console for more details");
		g_free(xep->transmission_efficiency_file);
		g_free(xep);
		g_signal_emit_by_name((gpointer) source, "after-generate", error);
		return;
	}

	// apply transmission efficiency file, if required...
	if (xep->transmission_efficiency_file != NULL && 
	    strlen(xep->transmission_efficiency_file) > 0 &&
	    strcmp(xep->transmission_efficiency_file, "(None)") != 0) {

		GArray *eff_x = g_array_new(FALSE, FALSE, sizeof(double));
		GArray *eff_y = g_array_new(FALSE, FALSE, sizeof(double));

		FILE *fp;
		if ((fp = fopen(xep->transmission_efficiency_file, "r")) == NULL) {
			g_set_error(&error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_TRANSMISSION_FILE, "Could not open %s for reading", xep->transmission_efficiency_file);
			g_free(xep->transmission_efficiency_file);
			g_free(xep);
			g_signal_emit_by_name((gpointer) source, "after-generate", error);
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
			if (values != 2 || energy < 0.0 || efficiency < 0.0 || efficiency > 1.0 || (eff_x->len > 0 && energy <= g_array_index(eff_x, double, eff_x->len - 1)) || (eff_x->len == 0 && energy >= 1.0)) {
				g_set_error(&error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_TRANSMISSION_FILE, "Error reading %s. The transmission efficiency file should contain two columns with energies (keV) in the left column and the transmission efficiency (value between 0 and 1) in the second column. Empty lines are ignored. First energy must be between 0 and 1 keV. The last value must be greater or equal to the tube voltage. At least 10 values are required.", xep->transmission_efficiency_file);
				g_signal_emit_by_name((gpointer) source, "after-generate", error);
				return;
			}
			g_array_append_val(eff_x, energy);
			g_array_append_val(eff_y, efficiency);
			free(line);
			line = NULL;
		}
		fclose(fp);

		if (eff_x->len < 10 || excitation_tube->continuous[excitation_tube->n_continuous-1].energy > g_array_index(eff_x, double, eff_x->len - 1)) {
			g_set_error(&error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_TRANSMISSION_FILE, "Error reading %s. The transmission efficiency file should contain two columns with energies (keV) in the left column and the transmission efficiency (value between 0 and 1) in the second column. Empty lines are ignored. First energy must be between 0 and 1 keV. The last value must be greater or equal to the tube voltage. At least 10 values are required.", xep->transmission_efficiency_file);
			g_free(xep->transmission_efficiency_file);
			g_free(xep);
			g_signal_emit_by_name((gpointer) source, "after-generate", error);
			return;
		}

		struct xmi_cubic_spline *spline = xmi_cubic_spline_init((double *) eff_x->data, (double *)eff_y->data, eff_x->len);

		int i;
		for (i = 0 ; i < excitation_tube->n_continuous ; i++) {
			excitation_tube->continuous[i].horizontal_intensity =
			excitation_tube->continuous[i].vertical_intensity =
			excitation_tube->continuous[i].horizontal_intensity *
			xmi_cubic_spline_eval(spline, excitation_tube->continuous[i].energy);
		}

		for (i = 0 ; i < excitation_tube->n_discrete ; i++) {
			excitation_tube->discrete[i].horizontal_intensity =
			excitation_tube->discrete[i].vertical_intensity =
			excitation_tube->discrete[i].horizontal_intensity *
			xmi_cubic_spline_eval(spline, excitation_tube->discrete[i].energy);
		}
		g_array_free(eff_x, TRUE);
		g_array_free(eff_y, TRUE);
		xmi_cubic_spline_free(spline);
	}

	// ideally I investigate the intensities and remove those whose intensity is really, really low...
	GArray *x = g_array_sized_new(FALSE, FALSE, sizeof(double), excitation_tube->n_continuous);
	GArray *y = g_array_sized_new(FALSE, FALSE, sizeof(double), excitation_tube->n_continuous);
	

	int i, j;
	for (i = 0 ; i < excitation_tube->n_continuous ; i++) {
		g_array_append_val(x, excitation_tube->continuous[i].energy);
		double intensity = excitation_tube->continuous[i].horizontal_intensity * 2.0 * xep->interval_width;
		g_array_append_val(y, intensity);
	}

	for (i = 0 ; i < excitation_tube->n_discrete ; i++) {
		for (j = 0 ; j < excitation_tube->n_continuous ; j++) {
			if (excitation_tube->discrete[i].energy < excitation_tube->continuous[j].energy && j != 0) {
				double *intensity = &g_array_index(y, double, j-1); 
				*intensity += excitation_tube->discrete[i].horizontal_intensity*2.0;
				break;
			}
		}
	}

	// find the smallest value greater than zero (1E-1)
	double ymax = xmi_maxval_double((double *) y->data, y->len);
	if (ymax < 1.0) {
		g_set_error(&error, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR, XMI_MSIM_GUI_SOURCE_TUBE_EBEL_ERROR_MAXIMUM, "Maximum value is too low: %f\nConsider changing the parameters", ymax);
		g_free(xep->transmission_efficiency_file);
		g_free(xep);
		g_array_free(x, TRUE);
		g_array_free(y, TRUE);
		g_signal_emit_by_name((gpointer) source, "after-generate", error);
		return;
	}
	double new_min = ymax;
	for (i = 0 ; i < y->len ; i++) {
		if (g_array_index(y, double, i) < new_min && g_array_index(y, double, i) > 1E-1)
			new_min = g_array_index(y, double, i);
	}

	for (i = 0 ; i < y->len ; i++) {
		double *intensity = &g_array_index(y, double, i);
		if (*intensity < new_min)
			*intensity = new_min;
	}

	// update member variables
	if (XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->raw_data != NULL)
		xmi_free_excitation(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->raw_data);

	XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->raw_data = excitation_tube;

	if (XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->x)
		g_array_free(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->x, TRUE);
	if (XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y)
		g_array_free(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y, TRUE);

	XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->x = x;
	XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y = y;

	g_free(xep->transmission_efficiency_file);
	g_free(xep);
	g_signal_emit_by_name((gpointer) source, "after-generate", error);

	return;
}

static const gchar *xmi_msim_gui_source_tube_ebel_real_get_name(XmiMsimGuiSourceAbstract *source) {
	static const gchar name[] = "X-ray tube (Ebel model)";

	return name;
}

static const gchar *xmi_msim_gui_source_tube_ebel_real_get_about_text(XmiMsimGuiSourceAbstract *source) {
	static const gchar about_text[] =
		"The model is based on the equations that can be found in "
		"the papers written by Horst Ebel and published in <a href='http://dx.doi.org/10.1002/(SICI)1097-4539(199907/08)28:4<255::AID-XRS347>3.0.CO;2-Y'>X-ray Spectrometry "
		"28 (1999), 255-266</a> and <a href='http://dx.doi.org/10.1002/xrs.610'>X-ray Spectrometry 32 (2003), 46-51</a>.";
	return about_text;
}

static void xmi_msim_gui_source_tube_ebel_dispose(GObject *object) {
	XmiMsimGuiSourceTubeEbel *source = XMI_MSIM_GUI_SOURCE_TUBE_EBEL(object);

	if (source->dispose_called == FALSE) {
		// save current input in preferences if valid
		// this can only occur the first time the dispose method is called though!
		struct xmi_ebel_parameters *xep = get_parameters(source, NULL);
		if (xep != NULL) {
			set_preferences(xep);
			g_free(xep->transmission_efficiency_file);
			g_free(xep);
		}
		source->dispose_called = TRUE;
	}
	G_OBJECT_CLASS(xmi_msim_gui_source_tube_ebel_parent_class)->dispose(object);
}

static void xmi_msim_gui_source_tube_ebel_finalize(GObject *object) {

	G_OBJECT_CLASS(xmi_msim_gui_source_tube_ebel_parent_class)->finalize(object);
}

GQuark xmi_msim_gui_source_tube_ebel_error_quark(void) {
	return g_quark_from_string("xmi-msim-gui-source-tube-ebel-error-quark");
}

static void xmi_msim_gui_source_tube_ebel_class_finalize(XmiMsimGuiSourceTubeEbelClass *klass) {

}

