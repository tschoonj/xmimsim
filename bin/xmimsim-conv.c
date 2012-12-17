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
#include <omp.h>

#include <stdio.h>
#include <string.h>



XMI_MAIN

	static struct xmi_main_options options;

	struct xmi_input *input;
	xmi_inputFPtr inputFPtr;
	int rv;
	gchar filename[512];
	static gchar *spe_file_conv=NULL;

	struct xmi_escape_ratios *escape_ratios_def=NULL;
	char *xmi_input_string;
	FILE *outPtr;
	GError *error = NULL;
	GOptionContext *context;
	static int nchannels=2048;

	static GOptionEntry entries[] = {
		{"spe-file",0,0,G_OPTION_ARG_FILENAME,&spe_file_conv,"Write detector convoluted spectra to file",NULL},
		{"set-channels",0,0,G_OPTION_ARG_INT,&nchannels,"Change number of channels (default=2048)",NULL},
		{ "enable-pile-up", 0, 0, G_OPTION_ARG_NONE, &(options.use_sum_peaks), "Enable pile-up", NULL },
		{ "disable-pile-up", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_sum_peaks), "Disable pile-up (default)", NULL },
		{NULL}
	};

	options.use_M_lines = 1;
	options.use_self_enhancement = 0;
	options.use_cascade_auger = 1;
	options.use_cascade_radiative = 1;
	options.use_variance_reduction = 1;
	options.use_optimizations = 1;
	options.use_sum_peaks = 0;
	options.verbose = 0;


	//parse options
	context = g_option_context_new ("XMSO-file");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmimsim-conv");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		exit (1);
	}

	if (argc != 2) {
		fprintf(stderr,"Call as xmimsim-conv XMSO-file\n");
		return 1;
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

	input = NULL;
	results_brute_force_history = NULL;
	results_nbrute_force_history = 0;
	results_var_red_history = NULL;
	results_nvar_red_history = 0;
	results_channels_conv = NULL;
	results_channels_unconv = NULL;
	results_nchannels = 0;
	results_inputfile = NULL;
	results_use_zero_interactions = 0;


	if (xmi_read_output_xml(xmsofile,
		&input,
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


		return 1;
	}
	char *hdf5_file = NULL;
	if (hdf5_file == NULL) {
		//no option detected
		//first look at default file
#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_DATA,&hdf5_file) == 0)
			return 1;


		if (g_access(hdf5_file, F_OK | R_OK) == 0) {
			//do nothing
		}
#elif defined(MAC_INTEGRATION)
		if (xmi_resources_mac_query(XMI_RESOURCES_MAC_DATA,&hdf5_file) == 0)
			return 1;


		if (g_access(hdf5_file, F_OK | R_OK) == 0) {
			//do nothing
		}
		else if (g_access(hdf5_file, F_OK | R_OK) != 0) {
			fprintf(stderr,"App bundle does not contain the HDF5 data file\n");
			return 1;
		}
#else
		//UNIX mode...
		if (g_access(XMIMSIM_HDF5_DEFAULT, F_OK | R_OK) == 0)
			hdf5_file = strdup(XMIMSIM_HDF5_DEFAULT);
#endif
		else if (g_access("xmimsimdata.h5", F_OK | R_OK) == 0) {
			//look in current folder
			hdf5_file = strdup("xmimsimdata.h5");
		}
		else {
			//if not found abort...	
			g_fprintf(stderr,"Could not detect the HDF5 data file\nCheck the xmimsim installation or\nuse the --with-hdf5-data option to manually pick the file\n");
			exit(1);
		}
	}

	//copy to the corresponding fortran variable
	xmi_input_C2F(input,&inputFPtr);

	//initialization
	if (xmi_init_input(&inputFPtr) == 0) {
		return 1;
	}

	SetErrorMessages(0);
	static char *xmimsim_hdf5_escape_ratios = NULL;

	//read escape ratios
	if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios) == 0)
		return 1;


	//check if escape ratios are already precalculated
	if (xmi_find_escape_ratios_match(xmimsim_hdf5_escape_ratios , input, &escape_ratios_def) == 0)
		return 1;
	if (escape_ratios_def == NULL) {
		if (options.verbose)
			g_fprintf(stdout,"Precalculating escape peak ratios\n");
		//doesn't exist yet
		//convert input to string
		if (xmi_write_input_xml_to_string(&xmi_input_string,input) == 0) {
			return 1;
		}
		xmi_escape_ratios_calculation(input, &escape_ratios_def, xmi_input_string,hdf5_file,options);
		//update hdf5 file
		if( xmi_update_escape_ratios_hdf5_file(xmimsim_hdf5_escape_ratios , escape_ratios_def) == 0)
			return 1;
		else if (options.verbose)
			g_fprintf(stdout,"%s was successfully updated with new escape peak ratios\n",xmimsim_hdf5_escape_ratios);
	}


	int i,j;

	double *channels_conv_temp;
	double **channels_conv = (double **) malloc(sizeof(double *)*(input->general->n_interactions_trajectory+1));
	for (i=0 ; i < input->general->n_interactions_trajectory ; i++) {
		double *channels = (double *) malloc(sizeof(double)*nchannels);
		for (j = 0 ; j < nchannels ; j++)
			channels[j] = results_channels_unconv[j][i];	
		xmi_detector_convolute(inputFPtr, channels, &channels_conv_temp, nchannels,options,escape_ratios_def);
		channels_conv[i] = channels_conv_temp;
		free(channels);

		if (spe_file_conv != NULL) {
			sprintf(filename,"%s_%i.spe",spe_file_conv,i+1);
			if ((outPtr=fopen(filename,"w")) == NULL ) {
				g_fprintf(stderr,"Could not write to %s\n",filename);
				exit(1);
			}
			else if (options.verbose)
				g_fprintf(stdout,"Writing to SPE file %s\n",filename);
			fprintf(outPtr,"$SPEC_ID:\n\n");
			fprintf(outPtr,"$DATA:\n");
			fprintf(outPtr,"1\t%i\n",nchannels);
			for (j=0 ; j < nchannels ; j++) {
				fprintf(outPtr,"%lg",channels_conv[i][j]);
				if ((j+1) % 8 == 0) {
					fprintf(outPtr,"\n");
				}
				else {
					fprintf(outPtr,"     ");
				}
			}
			fclose(outPtr);
		}

	}
	return 0;
}
