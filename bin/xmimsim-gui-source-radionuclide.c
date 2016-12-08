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
#include "xmimsim-gui-source-radionuclide.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include "xmi_aux.h"
#include <xraylib.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

enum {
	ACTIVITY_UNIT_mCi = 0,
	ACTIVITY_UNIT_Ci,
	ACTIVITY_UNIT_GBq,
	ACTIVITY_UNIT_Bq,
};

struct xmi_nuclide_parameters {
	int radioNuclide;
	int activityUnit;
	double activity;
	double nuclide_solid_angle;
};

static const gchar *activity_units[4] = {"mCi", "Ci", "GBq", "Bq"};

XMI_MSIM_GUI_DEFINE_DYNAMIC_SOURCE_TYPE(XmiMsimGuiSourceRadionuclide, xmi_msim_gui_source_radionuclide, XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT)

static gboolean xmi_msim_gui_source_radionuclide_real_generate(XmiMsimGuiSourceAbstract *source, GError **error);

static const gchar *xmi_msim_gui_source_radionuclide_real_get_name(XmiMsimGuiSourceAbstract *source);

static const gchar *xmi_msim_gui_source_radionuclide_real_get_about_text(XmiMsimGuiSourceAbstract *source);

static void xmi_msim_gui_source_radionuclide_dispose(GObject *object);

static void xmi_msim_gui_source_radionuclide_finalize(GObject *object);

static void slits_button_clicked_cb(XmiMsimGuiSourceRadionuclide *source);

static void xmi_msim_gui_source_radionuclide_class_init(XmiMsimGuiSourceRadionuclideClass *klass) {

	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_source_radionuclide_dispose;
	object_class->finalize = xmi_msim_gui_source_radionuclide_finalize;

	XmiMsimGuiSourceAbstractClass *parent_klass = XMI_MSIM_GUI_SOURCE_ABSTRACT_CLASS(klass);

	parent_klass->generate = xmi_msim_gui_source_radionuclide_real_generate;
	//parent_klass->save = xmi_msim_gui_source_radionuclide_real_save; // do not override
	parent_klass->get_name = xmi_msim_gui_source_radionuclide_real_get_name;
	parent_klass->get_about_text = xmi_msim_gui_source_radionuclide_real_get_about_text;
}

static struct xmi_nuclide_parameters* get_parameters(XmiMsimGuiSourceRadionuclide *source, GError **error) {
	struct xmi_nuclide_parameters *xnp = (struct xmi_nuclide_parameters *) g_malloc(sizeof(struct xmi_nuclide_parameters));

	xnp->radioNuclide = gtk_combo_box_get_active(GTK_COMBO_BOX(source->radioNuclideW));
	xnp->activityUnit = gtk_combo_box_get_active(GTK_COMBO_BOX(source->activityUnitW));
	
	const gchar *text = gtk_entry_get_text(GTK_ENTRY(source->activityW));
	gchar *endPtr;
	xnp->activity = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xnp->activity <= 0.0) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_ERROR, XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_ERROR_INVALID_DATA, "Invalid activity: must be greater than zero");
		g_free(xnp);
		return NULL;
	}
	text = gtk_entry_get_text(GTK_ENTRY(source->nuclideSolidAngleW));
	xnp->nuclide_solid_angle = strtod(text, &endPtr);
	if (strlen(text) == 0 || text + strlen(text) != endPtr || xnp->nuclide_solid_angle <= 0.0 || xnp->nuclide_solid_angle >= 2.0*M_PI) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_ERROR, XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_ERROR_INVALID_DATA, "Invalid solid angle: must be greater than zero and less than 2\317\200");
		g_free(xnp);
		return NULL;
	}

	return xnp;
}

static void set_parameters(XmiMsimGuiSourceRadionuclide *source, struct xmi_nuclide_parameters *xnp) {
	gchar *buf;

	gtk_combo_box_set_active(GTK_COMBO_BOX(source->radioNuclideW), xnp->radioNuclide);
	gtk_combo_box_set_active(GTK_COMBO_BOX(source->activityUnitW), xnp->activityUnit);

	buf = g_strdup_printf("%g", xnp->activity);
	gtk_entry_set_text(GTK_ENTRY(source->activityW), buf);
	g_free(buf);

	buf = g_strdup_printf("%g", xnp->nuclide_solid_angle);
	gtk_entry_set_text(GTK_ENTRY(source->nuclideSolidAngleW), buf);
	g_free(buf);
}

static void set_preferences(struct xmi_nuclide_parameters *xnp) {
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
	g_key_file_set_double(keyfile, "Radionuclide last used", "Activity", xnp->activity);
	g_key_file_set_double(keyfile, "Radionuclide last used", "Solid angle", xnp->nuclide_solid_angle);
	g_key_file_set_string(keyfile, "Radionuclide last used", "Unit", activity_units[xnp->activityUnit]);
	int nNuclides;
	gchar **nuclides = GetRadioNuclideDataList(&nNuclides);
	if (xnp->radioNuclide < 0 || xnp->radioNuclide >= nNuclides) {
		g_warning("Invalid radioNuclide %i detected\n", xnp->radioNuclide);
	}
	else {
		g_key_file_set_string(keyfile, "Radionuclide last used", "Radionuclide", nuclides[xnp->radioNuclide]);
	}
	int i;
	for (i = 0 ; i < nNuclides ; i++)
		free(nuclides[i]);
	free(nuclides);

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

static struct xmi_nuclide_parameters* get_preferences() {
	struct xmi_nuclide_parameters *xnp = (struct xmi_nuclide_parameters *) g_malloc(sizeof(struct xmi_nuclide_parameters));

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

	xnp->activity= g_key_file_get_double(keyfile, "Radionuclide last used", "Activity", &error);
	if (error != NULL) {
		g_warning("Radionuclide last used Activity not found in preferences file\n");
		g_key_file_set_double(keyfile, "Radionuclide last used", "Activity", 100.0);
		xnp->activity = 100.0;
		error = NULL;
		update_file = TRUE;
	}
	gchar *unit = g_key_file_get_string(keyfile, "Radionuclide last used", "Unit", &error);
	if (error != NULL) {
		g_warning("Radionuclide last used Unit not found in preferences file\n");
		g_key_file_set_string(keyfile, "Radionuclide last used", "Unit", activity_units[ACTIVITY_UNIT_mCi]);
		xnp->activityUnit = ACTIVITY_UNIT_mCi;
		error = NULL;
		update_file = TRUE;
	}
	else {
		gboolean matched = FALSE;
		int i;
		for (i = 0 ; i < 4 ; i++) {
			if (strcmp(activity_units[i], unit) == 0) {
				xnp->activityUnit = i;
				matched = TRUE;
				break;
			}
		}
		if (!matched) {
			g_error("Could not find a match for Radionuclide last used Unit in preferences file\n");
			xnp->activityUnit = ACTIVITY_UNIT_mCi;
			g_key_file_set_string(keyfile, "Radionuclide last used", "Unit", activity_units[ACTIVITY_UNIT_mCi]);
			update_file = TRUE;
		}
		g_free(unit);
	}

	xnp->nuclide_solid_angle = g_key_file_get_double(keyfile, "Radionuclide last used", "Solid angle", &error);
	if (error != NULL) {
		g_warning("Radionuclide last used Solid angle not found in preferences file\n");
		g_key_file_set_double(keyfile, "Radionuclide last used", "Solid angle", 1.0);
		xnp->nuclide_solid_angle = 1.0;
		error = NULL;
		update_file = TRUE;
	}

	int nNuclides;
	gchar **nuclides = GetRadioNuclideDataList(&nNuclides);
	gchar *nuclide = g_key_file_get_string(keyfile, "Radionuclide last used", "Radionuclide", &error);

	if (error != NULL) {
		g_warning("Radionuclide last used Radionuclide not found in preferences file\n");
		g_key_file_set_string(keyfile, "Radionuclide last used", "Radionuclide", nuclides[0]);
		xnp->radioNuclide = 0;
		error = NULL;
		update_file = TRUE;
	}
	else {
		gboolean matched = FALSE;
		int i;
		for (i = 0 ; i < nNuclides ; i++) {
			if (strcmp(nuclides[i], nuclide) == 0) {
				xnp->radioNuclide= i;
				matched = TRUE;
				break;
			}
		}
		if (!matched) {
			g_error("Could not find a match for Radionuclide last used Radionuclide in preferences file\n");
			g_key_file_set_string(keyfile, "Radionuclide last used", "Radionuclide", nuclides[0]);
			xnp->radioNuclide = 0;
			update_file = TRUE;
		}
		g_free(nuclide);
	}
	int i;
	for (i = 0 ; i < nNuclides ; i++)
		free(nuclides[i]);
	free(nuclides);

	if (update_file) {
		//save file
		gchar *prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
		if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, &error))
			g_error("Could not write to %s: %s\n", prefs_file, error->message);
		g_free(prefs_file_contents);
	}
	g_free(prefs_file);
	g_key_file_free(keyfile);
	return xnp;
}

static void xmi_msim_gui_source_radionuclide_init(XmiMsimGuiSourceRadionuclide *source) {
	// construct the widgets, and set them to their values as we go along...
	GtkWidget *mainVBox = GTK_WIDGET(source);

	GtkWidget *hbox = gtk_hbox_new(FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new("Radionuclide"), FALSE, FALSE, 2);
	source->radioNuclideW = gtk_combo_box_text_new();

	gchar **nuclides;
	int nNuclides, i;
	nuclides = GetRadioNuclideDataList(&nNuclides);
	for (i = 0 ; i < nNuclides ; i++) {
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(source->radioNuclideW), nuclides[i]);
		g_free(nuclides[i]);
	}
	g_free(nuclides);
	gtk_box_pack_end(GTK_BOX(hbox), source->radioNuclideW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, FALSE, FALSE, 2);
	gtk_widget_show_all(hbox);


	hbox = gtk_hbox_new(FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new("Activity"), FALSE, FALSE, 2);
	source->activityW = gtk_entry_new();

	source->activityUnitW = gtk_combo_box_text_new();
	for (i = 0 ; i < 4 ; i++) {
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(source->activityUnitW), activity_units[i]);
	}
	gtk_box_pack_end(GTK_BOX(hbox), source->activityUnitW, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), source->activityW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, FALSE, FALSE, 2);
	gtk_widget_show_all(hbox);

	source->nuclideSolidAngleW = gtk_entry_new();
	hbox = gtk_hbox_new(FALSE, 3);
	GtkWidget *label = gtk_label_new("Source solid angle (sr)");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	GtkWidget *slitsButton = gtk_button_new_with_label("Get from slits");
	gtk_box_pack_end(GTK_BOX(hbox), slitsButton, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(hbox), source->nuclideSolidAngleW, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), hbox, FALSE, FALSE, 2);
	gtk_widget_show_all(hbox);

	// load the preferences
	struct xmi_nuclide_parameters *xnp = get_preferences();
	
	set_parameters(source, xnp);

	g_free(xnp);

	// signal handlers
	g_signal_connect_swapped(G_OBJECT(slitsButton), "clicked", G_CALLBACK(slits_button_clicked_cb), (gpointer) source);

}

static void slits_button_clicked_cb(XmiMsimGuiSourceRadionuclide *source) {
	//calculate solid angle based on slits
	double solid_angle = xmi_msim_gui_utils_get_solid_angle_from_slits(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->current->geometry);

	gchar *buf = g_strdup_printf("%g", solid_angle);
	gtk_entry_set_text(GTK_ENTRY(source->nuclideSolidAngleW), buf);
	g_free(buf);
	return;
}

static gboolean xmi_msim_gui_source_radionuclide_real_generate(XmiMsimGuiSourceAbstract *source, GError **error) {
	// read the parameters
	struct xmi_nuclide_parameters *xnp = get_parameters(XMI_MSIM_GUI_SOURCE_RADIONUCLIDE(source), error);

	if (xnp == NULL)
		return FALSE;

	const double nuclide_solid_angle_fraction = 1.0/(4.0*M_PI);
	double activity = xnp->activity * nuclide_solid_angle_fraction * xnp->nuclide_solid_angle;

	if (xnp->activityUnit == ACTIVITY_UNIT_mCi) {
		activity *= 3.7E7;
	}
	else if (xnp->activityUnit == ACTIVITY_UNIT_Ci) {
		activity *= 3.7E10;
	}
	else if (xnp->activityUnit == ACTIVITY_UNIT_GBq) {
		activity *= 1E9;
	}
	else if (xnp->activityUnit == ACTIVITY_UNIT_Bq) {
		//do nothing
	}

	struct radioNuclideData *rnd = GetRadioNuclideDataByIndex(xnp->radioNuclide);

	int i;

	struct xmi_excitation *excitation_nuclide = (struct xmi_excitation *) malloc(sizeof(struct xmi_excitation));
	excitation_nuclide->n_continuous = 0;
	excitation_nuclide->continuous = NULL;
	excitation_nuclide->n_discrete= 0;
	excitation_nuclide->discrete= NULL;

	double plot_xmax = 0.0;

	for (i = 0 ; i < rnd->nXrays ; i++) {
		double energy = LineEnergy(rnd->Z_xray, rnd->XrayLines[i]);
		if (energy < 1.0 || energy > 200.0)
			continue;

		if (energy > plot_xmax)
			plot_xmax = energy;

		excitation_nuclide->discrete = (struct xmi_energy_discrete *) realloc(excitation_nuclide->discrete, sizeof(struct xmi_energy_discrete)*++excitation_nuclide->n_discrete);
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].energy = energy;
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].horizontal_intensity =
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].vertical_intensity =
		rnd->XrayIntensities[i] * activity/2.0;
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].sigma_x =
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].sigma_xp =
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].sigma_y =
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].sigma_yp =
		0.0;
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].distribution_type = XMI_DISCRETE_MONOCHROMATIC;
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].scale_parameter= 0.0;
	}
	for (i = 0 ; i < rnd->nGammas ; i++) {
		double energy = rnd->GammaEnergies[i];
		if (energy < 1.0 || energy > 200.0)
			continue;

		if (energy > plot_xmax)
			plot_xmax = energy;

		excitation_nuclide->discrete = (struct xmi_energy_discrete *) realloc(excitation_nuclide->discrete, sizeof(struct xmi_energy_discrete)*++excitation_nuclide->n_discrete);
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].energy = energy;
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].horizontal_intensity =
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].vertical_intensity =
		rnd->GammaIntensities[i]*activity/2.0;
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].sigma_x =
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].sigma_xp =
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].sigma_y =
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].sigma_yp =
		0.0;
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].distribution_type = XMI_DISCRETE_MONOCHROMATIC;
		excitation_nuclide->discrete[excitation_nuclide->n_discrete-1].scale_parameter= 0.0;
	}
	FreeRadioNuclideData(rnd);

	// update member variables
	if (XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->raw_data != NULL)
		xmi_free_excitation(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->raw_data);

	XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->raw_data = excitation_nuclide;

	g_array_free(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->x, TRUE);
	g_array_free(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y, TRUE);

	XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->x = g_array_sized_new(FALSE, FALSE, sizeof(double), 1000);
	XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y = g_array_sized_new(FALSE, FALSE, sizeof(double), 1000);

	for (i = 0 ; i < 1000 ; i++) {
		double energy = i * plot_xmax/999.0;
		double intensity = 0.0;
		g_array_append_val(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->x, energy);
		g_array_append_val(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y, intensity);
	}
	for (i = 0 ; i < excitation_nuclide->n_discrete ; i++) {
		int channel = (int) floor(excitation_nuclide->discrete[i].energy * 999.0/plot_xmax);
		double *intensity = &g_array_index(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y, double, channel); 
		*intensity += excitation_nuclide->discrete[i].horizontal_intensity*2.0;
	}

	// find the smallest value greater than zero (1E-1)
	double ymax = xmi_maxval_double((double *) XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y->data, XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y->len);
	if (ymax < 1.0) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_ERROR, XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_ERROR_MAXIMUM, "Maximum value is too low: %f\nConsider changing the parameters", ymax);
		g_free(xnp);
		return FALSE;
	}
	double new_min = ymax;
	for (i = 0 ; i < XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y->len ; i++) {
		if (g_array_index(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y, double, i) < new_min && g_array_index(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y, double, i) > 1E-1)
			new_min = g_array_index(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y, double, i);
	}

	for (i = 0 ; i < XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y->len ; i++) {
		double *intensity = &g_array_index(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y, double, i);
		if (*intensity < new_min)
			*intensity = new_min;
	}

	g_free(xnp);

	return TRUE;
}

static const gchar *xmi_msim_gui_source_radionuclide_real_get_name(XmiMsimGuiSourceAbstract *source) {
	static const gchar name[] = "Radionuclides";

	return name;
}

static const gchar *xmi_msim_gui_source_radionuclide_real_get_about_text(XmiMsimGuiSourceAbstract *source) {
	static const gchar about_text[] =
		"The X-ray and gamma spectra of the provided radionuclides "
		"have been obtained using the <a href='http://github.com/tschoonj/xraylib/wiki/The-xraylib-API-list-of-all-functions'>xraylib API</a> for radionuclides. "
		"Follow the references in the xraylib documentation in "
		"order to find the origin of the datasets.";
		
	return about_text;
}

static void xmi_msim_gui_source_radionuclide_dispose(GObject *object) {
	static gboolean first_entry = TRUE;

	if (first_entry == TRUE) {
		// save current input in preferences if valid
		// this can only occur the first time the dispose method is called though!
		struct xmi_nuclide_parameters *xnp = get_parameters(XMI_MSIM_GUI_SOURCE_RADIONUCLIDE(object), NULL);
		if (xnp != NULL) {
			set_preferences(xnp);
			g_free(xnp);
		}
	}
	first_entry = FALSE;
	G_OBJECT_CLASS(xmi_msim_gui_source_radionuclide_parent_class)->dispose(object);
}

static void xmi_msim_gui_source_radionuclide_finalize(GObject *object) {

	G_OBJECT_CLASS(xmi_msim_gui_source_radionuclide_parent_class)->finalize(object);
}

GQuark xmi_msim_gui_source_radionuclide_error_quark(void) {
	return g_quark_from_string("xmi-msim-gui-source-radionuclide-error-quark");
}

static void xmi_msim_gui_source_radionuclide_class_finalize(XmiMsimGuiSourceRadionuclideClass *klass) {

}

