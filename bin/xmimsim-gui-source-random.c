/*
Copyright (C) 2016-2017 Tom Schoonjans and Laszlo Vincze

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
#include <time.h>
#ifdef HAVE_EASYRNG
  #include <easy_rng.h>
  #include <easy_randist.h>
  typedef easy_rng_type xmi_rng_type;
  #define xmi_rng_default easy_rng_default
  typedef easy_rng xmi_rng;
  #define xmi_rng_alloc easy_rng_alloc
  #define xmi_ran_gaussian easy_ran_gaussian
  #define xmi_rng_free easy_rng_free
  #define xmi_rng_set easy_rng_set
#else
  #include <gsl/gsl_rng.h>
  #include <gsl/gsl_randist.h> 
  typedef gsl_rng_type xmi_rng_type;
  #define xmi_rng_default gsl_rng_default
  typedef gsl_rng xmi_rng;
  #define xmi_rng_alloc gsl_rng_alloc
  #define xmi_ran_gaussian gsl_ran_gaussian
  #define xmi_rng_free gsl_rng_free
  #define xmi_rng_set gsl_rng_set
#endif
#include "xmimsim-gui-source-random.h"
#include "xmi_aux.h"
#include <math.h>
#include <stdio.h>


XMI_MSIM_GUI_DEFINE_DYNAMIC_SOURCE_TYPE(XmiMsimGuiSourceRandom, xmi_msim_gui_source_random, XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT)

static void xmi_msim_gui_source_random_real_generate(XmiMsimGuiSourceAbstract *source);

static const gchar *xmi_msim_gui_source_random_real_get_name(XmiMsimGuiSourceAbstract *source);

static const gchar *xmi_msim_gui_source_random_real_get_about_text(XmiMsimGuiSourceAbstract *source);

static void xmi_msim_gui_source_random_dispose(GObject *object);

static void xmi_msim_gui_source_random_finalize(GObject *object);

static void xmi_msim_gui_source_random_class_init(XmiMsimGuiSourceRandomClass *klass) {

	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_source_random_dispose;
	object_class->finalize = xmi_msim_gui_source_random_finalize;

	XmiMsimGuiSourceAbstractClass *parent_klass = XMI_MSIM_GUI_SOURCE_ABSTRACT_CLASS(klass);

	parent_klass->generate = xmi_msim_gui_source_random_real_generate;
	parent_klass->get_name = xmi_msim_gui_source_random_real_get_name;
	parent_klass->get_about_text = xmi_msim_gui_source_random_real_get_about_text;
}

static void xmi_msim_gui_source_random_init(XmiMsimGuiSourceRandom *source) {
}

static void xmi_msim_gui_source_random_real_generate(XmiMsimGuiSourceAbstract *source) {
	int i;

	struct xmi_excitation *excitation_random = (struct xmi_excitation *) g_malloc(sizeof(struct xmi_excitation));
	excitation_random->n_continuous = 0;
	excitation_random->continuous = NULL;
	excitation_random->n_discrete= 0;
	excitation_random->discrete= NULL;

	double plot_xmax = 10.0;

	const xmi_rng_type *type = xmi_rng_default;
	xmi_rng *rng = xmi_rng_alloc(type);
	xmi_rng_set(rng, (long unsigned int) time(NULL));

	for (i = 0 ; i < 10 ; i++) {
		double energy = i + 0.5;

		excitation_random->discrete = (struct xmi_energy_discrete *) g_realloc(excitation_random->discrete, sizeof(struct xmi_energy_discrete)*++excitation_random->n_discrete);
		excitation_random->discrete[excitation_random->n_discrete-1].energy = energy;
		excitation_random->discrete[excitation_random->n_discrete-1].horizontal_intensity =
		excitation_random->discrete[excitation_random->n_discrete-1].vertical_intensity =
		MAX(1.0, xmi_ran_gaussian(rng, 10.0)) + 1000.0;
		excitation_random->discrete[excitation_random->n_discrete-1].sigma_x =
		excitation_random->discrete[excitation_random->n_discrete-1].sigma_xp =
		excitation_random->discrete[excitation_random->n_discrete-1].sigma_y =
		excitation_random->discrete[excitation_random->n_discrete-1].sigma_yp =
		0.0;
		excitation_random->discrete[excitation_random->n_discrete-1].distribution_type = XMI_DISCRETE_MONOCHROMATIC;
		excitation_random->discrete[excitation_random->n_discrete-1].scale_parameter= 0.0;
	}
	
	xmi_rng_free(rng);

	GArray *x, *y;

	x = g_array_sized_new(FALSE, FALSE, sizeof(double), 1000);
	y = g_array_sized_new(FALSE, FALSE, sizeof(double), 1000);

	for (i = 0 ; i < 1000 ; i++) {
		double energy = i * plot_xmax/999.0;
		double intensity = 0.0;
		g_array_append_val(x, energy);
		g_array_append_val(y, intensity);
	}
	for (i = 0 ; i < excitation_random->n_discrete ; i++) {
		int channel = (int) floor(excitation_random->discrete[i].energy * 999.0/plot_xmax);
		double *intensity = &g_array_index(y, double, channel); 
		*intensity += excitation_random->discrete[i].horizontal_intensity*2.0;
	}

	// find the smallest value greater than zero (1E-1)
	double ymax = xmi_maxval_double((double *) y->data, y->len);
	if (ymax < 1.0) {
		GError *error = g_error_new(XMI_MSIM_GUI_SOURCE_RANDOM_ERROR, XMI_MSIM_GUI_SOURCE_RANDOM_ERROR_MAXIMUM, "Maximum value is too low: %f\nConsider changing the parameters", ymax);
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

	// update member variables -> if we get here, everything must be fine.
	if (XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->raw_data != NULL)
		xmi_free_excitation(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->raw_data);

	XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->raw_data = excitation_random;

	if (XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->x)
		g_array_free(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->x, TRUE);
	if (XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y)
		g_array_free(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y, TRUE);

	XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->x = x;
	XMI_MSIM_GUI_SOURCE_ABSTRACT(source)->y = y;

	g_signal_emit_by_name((gpointer) source, "after-generate", NULL);
	return;
}

static const gchar *xmi_msim_gui_source_random_real_get_name(XmiMsimGuiSourceAbstract *source) {
	static const gchar name[] = "Random numbers";

	return name;
}

static const gchar *xmi_msim_gui_source_random_real_get_about_text(XmiMsimGuiSourceAbstract *source) {
	static const gchar about_text[] = "Random numbers source plug-in: for testing purposes only!";
		
	return about_text;
}

static void xmi_msim_gui_source_random_dispose(GObject *object) {
	G_OBJECT_CLASS(xmi_msim_gui_source_random_parent_class)->dispose(object);
}

static void xmi_msim_gui_source_random_finalize(GObject *object) {

	G_OBJECT_CLASS(xmi_msim_gui_source_random_parent_class)->finalize(object);
}

GQuark xmi_msim_gui_source_random_error_quark(void) {
	return g_quark_from_string("xmi-msim-gui-source-random-error-quark");
}

static void xmi_msim_gui_source_random_class_finalize(XmiMsimGuiSourceRandomClass *klass) {

}


