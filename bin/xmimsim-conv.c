/*
Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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
#include <stdlib.h>

#include <stdio.h>
#include <string.h>
#include <gmodule.h>



XMI_MAIN

	static struct xmi_main_options options;

	xmi_inputFPtr inputFPtr;
	int rv;
	gchar filename[512];

	struct xmi_escape_ratios *escape_ratios_def=NULL;
	char *xmi_input_string;
	FILE *outPtr;
	GError *error = NULL;
	GOptionContext *context;
	static int version = 0;

	static GOptionEntry entries[] = {
		{"enable-pile-up", 0, 0, G_OPTION_ARG_NONE, &(options.use_sum_peaks), "Enable pile-up", NULL },
		{"disable-pile-up", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_sum_peaks), "Disable pile-up (default)", NULL },
		{"enable-escape-peaks", 0, 0, G_OPTION_ARG_NONE, &(options.use_escape_peaks), "Enable escape peaks (default)", NULL },
		{"disable-escape-peaks", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_escape_peaks), "Disable escape peaks", NULL },
		{"enable-poisson", 0, 0, G_OPTION_ARG_NONE, &(options.use_poisson), "Generate Poisson noise in the spectra", NULL },
		{"disable-poisson", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_poisson), "Disable the generating of spectral Poisson noise (default)", NULL },
		{"custom-detector-response",0,0,G_OPTION_ARG_FILENAME,&options.custom_detector_response,"Use the supplied library for the detector convolution routine",NULL},
		{"set-threads",0,0,G_OPTION_ARG_INT,&(options.omp_num_threads),"Set the number of threads (default=max)",NULL},
		{"verbose", 'v', 0, G_OPTION_ARG_NONE, &(options.verbose), "Verbose mode", NULL },
		{"very-verbose", 'V', 0, G_OPTION_ARG_NONE, &(options.extra_verbose), "Even more verbose mode", NULL },
		{"version", 0, 0, G_OPTION_ARG_NONE, &version, "display version information", NULL },
		{NULL}
	};

	xmi_init_hdf5();

	options.use_M_lines = 1;
	options.use_cascade_auger = 1;
	options.use_cascade_radiative = 1;
	options.use_variance_reduction = 1;
	options.use_sum_peaks = 0;
	options.use_escape_peaks = 1;
	options.use_poisson = 0;
	options.verbose = 0;
	options.use_opencl = 0;
	options.extra_verbose = 0;
	options.omp_num_threads = xmi_omp_get_max_threads();

#if defined(G_OS_WIN32)
	setlocale(LC_ALL,"English_United States");
#else
	g_setenv("LANG","en_US",TRUE);
#endif


	//parse options
	context = g_option_context_new ("XMSO-file XMSI-file XMSO-file");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmimsim-conv");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		exit (1);
	}

	if (version) {
		g_fprintf(stdout,"%s",xmi_version_string());	
		return 0;
	}

	if (argc != 4) {
		fprintf(stderr,"%s\n",g_option_context_get_help(context, TRUE, NULL));
		return 1;
	}

	g_option_context_free(context);
	
	if (options.omp_num_threads > xmi_omp_get_max_threads() ||
			options.omp_num_threads < 1) {
		options.omp_num_threads = xmi_omp_get_max_threads();
	}

	if (options.extra_verbose) {
		options.verbose = 1;
		//print all selected options
		g_fprintf(stdout,"Option pile-up: %i\n", options.use_sum_peaks);
		g_fprintf(stdout,"Option Poisson noise: %i\n", options.use_poisson);
		g_fprintf(stdout,"Option escape peaks: %i\n", options.use_escape_peaks);
		g_fprintf(stdout,"Option number of threads: %i\n", options.omp_num_threads);
	}

	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		return 1;
	}
	
	//start random number acquisition
	if (xmi_start_random_acquisition() == 0) {
		return 1;
	}

	//read in the outputfile
	char *xmsofile = argv[1];
	char *xmsifile = argv[2];
	char *new_xmsofile = argv[3];
	struct xmi_output *xmso_in, *xmso_out;
	struct xmi_input *xmsi_in;

	if (xmi_read_output_xml(xmsofile, &xmso_in) == 0) {
		fprintf(stderr,"%s could not be read\n", xmsofile);
		return 1;
	}

	if (xmi_read_input_xml(xmsifile, &xmsi_in) == 0) {
		fprintf(stderr,"%s could not be read\n", xmsifile);
		return 1;
	}

	//copy xmsi_in detector settings to xmso_in
	xmso_in->input->detector->detector_type = xmsi_in->detector->detector_type;	
	xmso_in->input->detector->pulse_width = xmsi_in->detector->pulse_width;

	xmi_free_input(xmsi_in);

	SetErrorMessages(0);
	static char *xmimsim_hdf5_escape_ratios = NULL;

	//read escape ratios
	if (options.use_escape_peaks) {
		if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0)
			return 1;

		//get the name of the HDF5 data file
		gchar *hdf5_file=NULL;
		if (xmi_get_hdf5_data_file(&hdf5_file) == 0) {
			return 1;
		}
		if (options.verbose)
			g_fprintf(stdout,"Querying %s for escape peak ratios\n",xmimsim_hdf5_escape_ratios);

		//check if escape ratios are already precalculated
		if (xmi_find_escape_ratios_match(xmimsim_hdf5_escape_ratios , xmso_in->input, &escape_ratios_def, options) == 0)
			return 1;
		if (escape_ratios_def == NULL) {
			if (options.verbose)
				g_fprintf(stdout,"Precalculating escape peak ratios\n");
			//doesn't exist yet
			//convert input to string
			if (xmi_write_input_xml_to_string(&xmi_input_string,xmso_in->input) == 0) {
				return 1;
			}
			xmi_escape_ratios_calculation(xmso_in->input, &escape_ratios_def, xmi_input_string,hdf5_file,options);
			//update hdf5 file
			if( xmi_update_escape_ratios_hdf5_file(xmimsim_hdf5_escape_ratios , escape_ratios_def) == 0)
				return 1;
			else if (options.verbose)
				g_fprintf(stdout,"%s was successfully updated with new escape peak ratios\n",xmimsim_hdf5_escape_ratios);
		}
		else if (options.verbose)
			g_fprintf(stdout,"Escape peak ratios already present in %s\n",xmimsim_hdf5_escape_ratios);
	}
	else if (options.verbose)
		g_fprintf(stdout,"No escape peaks requested: escape peak calculation is redundant\n");


	int i,j;

	//copy to the corresponding fortran variable
	xmi_input_C2F(xmso_in->input,&inputFPtr);

	//initialization
	if (xmi_init_input(&inputFPtr) == 0) {
		return 1;
	}

	double **channels_conv = malloc(sizeof(double *)*(xmso_in->input->general->n_interactions_trajectory+1));

	if (options.custom_detector_response == NULL)
		xmi_detector_convolute_all(inputFPtr, xmso_in->channels_unconv, channels_conv, options, escape_ratios_def, xmso_in->input->general->n_interactions_trajectory, xmso_in->use_zero_interactions);
	else {
		XmiDetectorConvoluteAll xmi_detector_convolute_all_custom;
		GModule *module = NULL;
		if (!g_module_supported()) {
			fprintf(stderr,"No module support on this platform: cannot use custom detector convolution routine\n");
			return 1;
		}
		module = g_module_open(options.custom_detector_response, 0);
		if (!module) {
			fprintf(stderr,"Could not open %s: %s\n", options.custom_detector_response, g_module_error());
			return 1;
		}
		if (!g_module_symbol(module, "xmi_detector_convolute_all_custom", (gpointer *) &xmi_detector_convolute_all_custom)) {
			fprintf(stderr,"Error retrieving xmi_detector_convolute_all_custom in %s: %s\n", options.custom_detector_response, g_module_error());
			return 1;
		}
		else if (options.verbose)
			g_fprintf(stdout,"xmi_detector_convolute_all_custom loaded from %s\n", options.custom_detector_response);
		xmi_detector_convolute_all_custom(inputFPtr, xmso_in->channels_unconv, channels_conv, options, escape_ratios_def, xmso_in->input->general->n_interactions_trajectory, xmso_in->use_zero_interactions);
		if (!g_module_close(module)) {
			fprintf(stderr,"Warning: could not close module %s: %s\n",options.custom_detector_response, g_module_error());
		}
	}

	if (options.use_escape_peaks) {
		xmi_free_escape_ratios(escape_ratios_def);
	}

	xmso_out = malloc(sizeof(struct xmi_output));
	xmso_out->inputfile = xmso_in->inputfile;
	xmso_out->input = xmso_in->input;
	xmso_out->brute_force_history = xmso_in->brute_force_history;
	xmso_out->var_red_history = xmso_in->var_red_history;
	xmso_out->nbrute_force_history = xmso_in->nbrute_force_history;
	xmso_out->nvar_red_history = xmso_in->nvar_red_history;
	xmso_out->channels_conv = channels_conv;
	xmso_out->channels_unconv = xmso_in->channels_unconv;
	xmso_out->use_zero_interactions = xmso_in->use_zero_interactions;

	if (xmi_write_output_xml(new_xmsofile, xmso_out) == 0) {
		return 1;
	}
	else if (options.verbose)
		g_fprintf(stdout,"Output written to XMSO file %s\n",new_xmsofile);

	return 0;
}
