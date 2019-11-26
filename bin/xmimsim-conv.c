/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

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

#include "xmi_main.h"
#include "xmi_private.h"
#include "xmi_data_structs.h"
#include "xmi_xml.h"
#include "xmi_aux.h"
#include "xmi_random.h"
#include "xmi_xslt.h"
#include "xmi_detector.h"
#include <unistd.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <locale.h>
#include <xraylib.h>

#include <stdio.h>
#include <gmodule.h>



int main(int argc, char **argv) {
	xmi_main_options *options = xmi_main_options_new();
	xmi_inputFPtr inputFPtr;
	xmi_escape_ratios *escape_ratios_def=NULL;
	char *xmi_input_string;
	GError *error = NULL;
	GOptionContext *context;
	int version = 0;
	gchar **filenames = NULL;

	GArray *entries = g_array_sized_new(TRUE, FALSE, sizeof(GOptionEntry), 30);
#define ADD_OPTION(long_name, short_name, flags, arg, arg_data, description, arg_description) \
	{ \
		GOptionEntry entry = {long_name, short_name, flags, arg, arg_data, description, arg_description}; \
		g_array_append_val(entries, entry); \
	}

	ADD_OPTION("enable-pile-up", 0, 0, G_OPTION_ARG_NONE, &options->use_sum_peaks, "Enable pile-up", NULL );
	ADD_OPTION("disable-pile-up", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_sum_peaks, "Disable pile-up (default)", NULL );
	ADD_OPTION("enable-escape-peaks", 0, 0, G_OPTION_ARG_NONE, &options->use_escape_peaks, "Enable escape peaks (default)", NULL );
	ADD_OPTION("disable-escape-peaks", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_escape_peaks, "Disable escape peaks", NULL );
	ADD_OPTION("enable-poisson", 0, 0, G_OPTION_ARG_NONE, &options->use_poisson, "Generate Poisson noise in the spectra", NULL );
	ADD_OPTION("disable-poisson", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_poisson, "Disable the generating of spectral Poisson noise (default)", NULL );
	ADD_OPTION("custom-detector-response",0,0,G_OPTION_ARG_FILENAME, &options->custom_detector_response,"Use the supplied library for the detector convolution routine",NULL);
	ADD_OPTION("set-threads",0,0,G_OPTION_ARG_INT,&options->omp_num_threads,"Set the number of threads (default=max)",NULL);
	ADD_OPTION("verbose", 'v', 0, G_OPTION_ARG_NONE, &options->verbose, "Verbose mode", NULL );
	ADD_OPTION("very-verbose", 'V', 0, G_OPTION_ARG_NONE, &options->extra_verbose, "Even more verbose mode", NULL );
	ADD_OPTION("version", 0, 0, G_OPTION_ARG_NONE, &version, "display version information", NULL );
	ADD_OPTION(G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &filenames, "xmsi-file", NULL);

	xmi_init_hdf5();

#if defined(G_OS_WIN32)
	setlocale(LC_ALL,"English_United States");
#else
	g_setenv("LANG","en_US",TRUE);
#endif

#ifdef G_OS_WIN32
	argv = g_win32_get_command_line();
#else
	argv = g_strdupv(argv);
#endif

	//parse options
	context = g_option_context_new("XMSO-file XMSI-file XMSO-file");
	g_option_context_add_main_entries(context, (const GOptionEntry *) entries->data, NULL);
	g_option_context_set_summary(context, "xmimsim-conv");
	if (!g_option_context_parse_strv(context, &argv, &error)) {
		g_print("option parsing failed: %s\n", error->message);
		return 1;
	}

	g_strfreev(argv);

	if (version) {
		g_fprintf(stdout, "%s", xmi_version_string());
		return 0;
	}

	if (filenames == NULL || g_strv_length(filenames) != 3) {
		g_fprintf(stderr, "%s\n", g_option_context_get_help(context, TRUE, NULL));
		return 1;
	}

	g_option_context_free(context);

	g_array_free(entries, TRUE);

	if (options->omp_num_threads > xmi_omp_get_max_threads() ||
			options->omp_num_threads < 1) {
		options->omp_num_threads = xmi_omp_get_max_threads();
	}

	if (options->extra_verbose) {
		options->verbose = 1;
		//print all selected options
		g_fprintf(stdout,"Option pile-up: %i\n", options->use_sum_peaks);
		g_fprintf(stdout,"Option Poisson noise: %i\n", options->use_poisson);
		g_fprintf(stdout,"Option escape peaks: %i\n", options->use_escape_peaks);
		g_fprintf(stdout,"Option number of threads: %i\n", options->omp_num_threads);
	}

	//load xml catalog
	if (xmi_xmlLoadCatalog(&error) == 0) {
		g_fprintf(stderr, "Could not load XML catalog: %s\n", error->message);
		return 1;
	}

	//start random number acquisition
	if (xmi_start_random_acquisition() == 0) {
		return 1;
	}

	//read in the outputfile
	char *xmsofile = filenames[0];
	char *xmsifile = filenames[1];
	char *new_xmsofile = filenames[2];
	xmi_output *xmso_in, *xmso_out;
	xmi_input *xmsi_in;

	if ((xmso_in = xmi_output_read_from_xml_file(xmsofile, &error)) == NULL) {
		fprintf(stderr,"%s could not be read: %s\n", xmsofile, error->message);
		return 1;
	}

	if ((xmsi_in = xmi_input_read_from_xml_file(xmsifile, &error)) == NULL) {
		fprintf(stderr,"%s could not be read: %s\n", xmsifile, error->message);
		return 1;
	}

	//copy xmsi_in detector settings to xmso_in
	xmso_in->input->detector->detector_type = xmsi_in->detector->detector_type;
	xmso_in->input->detector->pulse_width = xmsi_in->detector->pulse_width;

	xmi_input_free(xmsi_in);

	static char *xmimsim_hdf5_escape_ratios = NULL;

	//read escape ratios
	if (options->use_escape_peaks) {
		if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0)
			return 1;

		//get the name of the HDF5 data file
		gchar *hdf5_file=NULL;
		if (xmi_get_hdf5_data_file(&hdf5_file) == 0) {
			return 1;
		}
		if (options->verbose)
			g_fprintf(stdout,"Querying %s for escape peak ratios\n",xmimsim_hdf5_escape_ratios);

		//check if escape ratios are already precalculated
		if (xmi_find_escape_ratios_match(xmimsim_hdf5_escape_ratios , xmso_in->input, &escape_ratios_def, options) == 0)
			return 1;
		if (escape_ratios_def == NULL) {
			if (options->verbose)
				g_fprintf(stdout,"Precalculating escape peak ratios\n");
			//doesn't exist yet
			//convert input to string
			if (!xmi_input_write_to_xml_string(xmso_in->input, &xmi_input_string, &error)) {
				g_fprintf(stderr, "Could not convert to XML string: %s\n", error->message);
				return 1;
			}
			xmi_escape_ratios_calculation(xmso_in->input, &escape_ratios_def, xmi_input_string,hdf5_file,options, xmi_get_default_escape_ratios_options());
			g_free(xmi_input_string);
			//update hdf5 file
			if( xmi_update_escape_ratios_hdf5_file(xmimsim_hdf5_escape_ratios , escape_ratios_def) == 0)
				return 1;
			else if (options->verbose)
				g_fprintf(stdout,"%s was successfully updated with new escape peak ratios\n",xmimsim_hdf5_escape_ratios);
		}
		else if (options->verbose)
			g_fprintf(stdout,"Escape peak ratios already present in %s\n",xmimsim_hdf5_escape_ratios);
	}
	else if (options->verbose)
		g_fprintf(stdout,"No escape peaks requested: escape peak calculation is redundant\n");


	//copy to the corresponding fortran variable
	xmi_input_C2F(xmso_in->input,&inputFPtr);

	//initialization
	if (xmi_init_input(&inputFPtr) == 0) {
		return 1;
	}

	double **channels_conv = g_malloc(sizeof(double *)*(xmso_in->input->general->n_interactions_trajectory+1));

	if (options->custom_detector_response == NULL) {
		xmi_detector_convolute_all(inputFPtr, xmso_in->channels_unconv, channels_conv, NULL, NULL, options, escape_ratios_def, xmso_in->input->general->n_interactions_trajectory, xmso_in->use_zero_interactions);
	}
	else {
		XmiDetectorConvoluteAll xmi_detector_convolute_all_custom;
		GModule *module = NULL;
		if (!g_module_supported()) {
			fprintf(stderr,"No module support on this platform: cannot use custom detector convolution routine\n");
			return 1;
		}
		module = g_module_open(options->custom_detector_response, 0);
		if (!module) {
			fprintf(stderr,"Could not open %s: %s\n", options->custom_detector_response, g_module_error());
			return 1;
		}
		if (!g_module_symbol(module, "xmi_detector_convolute_all_custom", (gpointer *) &xmi_detector_convolute_all_custom)) {
			fprintf(stderr,"Error retrieving xmi_detector_convolute_all_custom in %s: %s\n", options->custom_detector_response, g_module_error());
			return 1;
		}
		else if (options->verbose)
			g_fprintf(stdout,"xmi_detector_convolute_all_custom loaded from %s\n", options->custom_detector_response);
		xmi_detector_convolute_all_custom(inputFPtr, xmso_in->channels_unconv, channels_conv, NULL, NULL, options, escape_ratios_def, xmso_in->input->general->n_interactions_trajectory, xmso_in->use_zero_interactions);
		if (!g_module_close(module)) {
			fprintf(stderr,"Warning: could not close module %s: %s\n",options->custom_detector_response, g_module_error());
		}
	}

	if (options->use_escape_peaks) {
		xmi_free_escape_ratios(escape_ratios_def);
	}

	xmso_out = g_malloc(sizeof(xmi_output));
	xmso_out->inputfile = xmso_in->inputfile;
	xmso_out->input = xmso_in->input;
	xmso_out->brute_force_history = xmso_in->brute_force_history;
	xmso_out->var_red_history = xmso_in->var_red_history;
	xmso_out->nbrute_force_history = xmso_in->nbrute_force_history;
	xmso_out->nvar_red_history = xmso_in->nvar_red_history;
	xmso_out->channels_conv = channels_conv;
	xmso_out->channels_unconv = xmso_in->channels_unconv;
	xmso_out->use_zero_interactions = xmso_in->use_zero_interactions;

	if (!xmi_output_write_to_xml_file(xmso_out, new_xmsofile, &error)) {
		g_fprintf(stderr, "Could not write to %s: %s\n", new_xmsofile, error->message);
		return 1;
	}
	else if (options->verbose)
		g_fprintf(stdout, "Output written to XMSO file %s\n", new_xmsofile);

	return 0;
}
