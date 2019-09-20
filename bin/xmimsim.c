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

#ifdef HAVE_OPENMPI
	#include <mpi.h>
#endif

#include "xmi_main.h"
#include "xmi_private.h"
#include "xmi_data_structs.h"
#include "xmi_xml.h"
#include "xmi_aux.h"
#include "xmi_random.h"
#include "xmi_xslt.h"
#include "xmi_detector.h"
#include "xmi_data.h"
#include <unistd.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <locale.h>
#include <xraylib.h>
#include <gmodule.h>

#include <stdio.h>

#ifdef _WIN32
  #define _UNICODE
  #define UNICODE
  #include <windows.h>
  #include "xmi_registry_win.h"
  #include <Shlobj.h>
  //bugfix in mingw headers
  #ifndef SHGFP_TYPE_CURRENT
    #define SHGFP_TYPE_CURRENT 0
  #endif
  //NO MPI
  #ifdef HAVE_OPENMPI
    #undef HAVE_OPENMPI
  #endif
#endif

#ifdef MAC_INTEGRATION
#include "xmi_resources_mac.h"
#endif


//#include <fenv.h>

XMI_MAIN

	//openmpi variables
#ifdef HAVE_OPENMPI
	int numprocs, rank, namelen;
	char processor_name[MPI_MAX_PROCESSOR_NAME];
//	int numreqs=0;
//	MPI_Request *reqs = 0;
#else
	int numprocs = 1;
#endif



	//general variables
	xmi_input *input;
	int rv;
	xmi_inputFPtr inputFPtr;
	xmi_hdf5FPtr hdf5FPtr;
	double *channels, *channelsdef;
	double **channels_conv;
	FILE *outPtr, *csv_convPtr, *csv_noconvPtr;
	char *filename;
	int i,j;
	GError *error = NULL;
	GOptionContext *context;
	xmi_main_options *options = xmi_main_options_new();

	double *brute_history;
	double *brute_historydef;
	double *var_red_history;
	double *var_red_historydef;
	gchar *hdf5_file=NULL;
	gchar *spe_file_noconv=NULL;
	gchar *spe_file_conv=NULL;
	gchar *csv_file_noconv=NULL;
	gchar *csv_file_conv=NULL;
	gchar *svg_file_noconv=NULL;
	gchar *svg_file_conv=NULL;
	gchar *htm_file_noconv=NULL;
	gchar *htm_file_conv=NULL;
	double zero_sum;
	xmi_solid_angle *solid_angle_def=NULL;
	xmi_escape_ratios *escape_ratios_def=NULL;
	char *xmi_input_string;
	char *xmimsim_hdf5_solid_angles = NULL;
	char *xmimsim_hdf5_escape_ratios = NULL;
	gchar *xmimsim_hdf5_solid_angles_utf8 = NULL;
	gchar *xmimsim_hdf5_escape_ratios_utf8 = NULL;
	gchar *hdf5_file_utf8 = NULL;
	int version = 0;

	GArray *entries = g_array_sized_new(TRUE, FALSE, sizeof(GOptionEntry), 30);
#define ADD_OPTION(long_name, short_name, flags, arg, arg_data, description, arg_description) \
	{ \
		GOptionEntry entry = {long_name, short_name, flags, arg, arg_data, description, arg_description}; \
		g_array_append_val(entries, entry); \
	}
	ADD_OPTION( "enable-M-lines", 0, 0, G_OPTION_ARG_NONE, &options->use_M_lines, "Enable M lines (default)", NULL );
	ADD_OPTION( "disable-M-lines", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_M_lines, "Disable M lines", NULL );
	ADD_OPTION( "enable-auger-cascade", 0, 0, G_OPTION_ARG_NONE, &options->use_cascade_auger, "Enable Auger (non radiative) cascade effects (default)", NULL );
	ADD_OPTION( "disable-auger-cascade", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_cascade_auger, "Disable Auger cascade effects", NULL );
	ADD_OPTION( "enable-radiative-cascade", 0, 0, G_OPTION_ARG_NONE, &options->use_cascade_radiative, "Enable radiative cascade effects (default)", NULL );
	ADD_OPTION( "disable-radiative-cascade", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_cascade_radiative, "Disable radiative cascade effects", NULL );
	ADD_OPTION( "enable-variance-reduction", 0, 0, G_OPTION_ARG_NONE, &options->use_variance_reduction, "Enable variance reduction (default)", NULL );
	ADD_OPTION( "disable-variance-reduction", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_variance_reduction, "Disable variance reduction", NULL );
	ADD_OPTION("with-hdf5-data",0,G_OPTION_FLAG_HIDDEN,G_OPTION_ARG_FILENAME,&hdf5_file,"Select a HDF5 data file (advanced usage)",NULL);
	ADD_OPTION("with-solid-angles-data",0,G_OPTION_FLAG_HIDDEN,G_OPTION_ARG_FILENAME,&xmimsim_hdf5_solid_angles,"Select a HDF5 solid angles file (advanced usage)",NULL);
	ADD_OPTION("with-escape-ratios-data",0,G_OPTION_FLAG_HIDDEN,G_OPTION_ARG_FILENAME,&xmimsim_hdf5_escape_ratios,"Select a HDF5 escape ratios file (advanced usage)",NULL);
	ADD_OPTION("spe-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&spe_file_noconv,"Write detector unconvoluted spectra to file",NULL);
	ADD_OPTION("spe-file",0,0,G_OPTION_ARG_FILENAME,&spe_file_conv,"Write detector convoluted spectra to file",NULL);
	ADD_OPTION("csv-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&csv_file_noconv,"Write detector unconvoluted spectra to CSV file",NULL);
	ADD_OPTION("csv-file",0,0,G_OPTION_ARG_FILENAME,&csv_file_conv,"Write detector convoluted spectra to CSV file",NULL);
	ADD_OPTION("svg-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&svg_file_noconv,"Write detector unconvoluted spectra to SVG file",NULL);
	ADD_OPTION("svg-file",0,0,G_OPTION_ARG_FILENAME,&svg_file_conv,"Write detector convoluted spectra to SVG file",NULL);
	ADD_OPTION("htm-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&htm_file_noconv,"Write detector unconvoluted spectra to HTML file",NULL);
	ADD_OPTION("htm-file",0,0,G_OPTION_ARG_FILENAME,&htm_file_conv,"Write detector convoluted spectra to HTML file",NULL);
	ADD_OPTION("enable-pile-up", 0, 0, G_OPTION_ARG_NONE, &options->use_sum_peaks, "Enable pile-up", NULL );
	ADD_OPTION("disable-pile-up", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_sum_peaks, "Disable pile-up (default)", NULL );
	ADD_OPTION("enable-escape-peaks", 0, 0, G_OPTION_ARG_NONE, &options->use_escape_peaks, "Enable escape peaks (default)", NULL );
	ADD_OPTION("disable-escape-peaks", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_escape_peaks, "Disable escape peaks", NULL );
	ADD_OPTION("enable-poisson", 0, 0, G_OPTION_ARG_NONE, &options->use_poisson, "Generate Poisson noise in the spectra", NULL );
	ADD_OPTION("disable-poisson", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_poisson, "Disable the generating of spectral Poisson noise (default)", NULL );
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	ADD_OPTION("enable-opencl", 0, 0, G_OPTION_ARG_NONE, &options->use_opencl, "Enable OpenCL", NULL );
	ADD_OPTION("disable-opencl", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_opencl, "Disable OpenCL (default)", NULL );
#endif
	ADD_OPTION("enable-advanced-compton", 0, 0, G_OPTION_ARG_NONE, &options->use_advanced_compton, "Enable advanced yet slower Compton simulation", NULL );
	ADD_OPTION("disable-advanced-compton", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_advanced_compton, "Disable advanced yet slower Compton simulation (default)", NULL );
	ADD_OPTION("custom-detector-response",0,0,G_OPTION_ARG_FILENAME, &options->custom_detector_response, "Use the supplied library for the detector response routine",NULL);
	ADD_OPTION("set-threads",0,0,G_OPTION_ARG_INT, &options->omp_num_threads, "Sets the number of threads to NTHREADS (default=max)", "NTHREADS");
	ADD_OPTION("enable-default-seeds", 0, 0, G_OPTION_ARG_NONE, &options->use_default_seeds, "Use default seeds for reproducible simulation results", NULL );
	ADD_OPTION("disable-default-seeds", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_default_seeds, "Disable default seeds for irreproducible simulation results (default)", NULL );
	ADD_OPTION("verbose", 'v', 0, G_OPTION_ARG_NONE, &options->verbose, "Verbose mode", NULL );
	ADD_OPTION("very-verbose", 'V', 0, G_OPTION_ARG_NONE, &options->extra_verbose, "Even more verbose mode", NULL );
	ADD_OPTION("version", 0, 0, G_OPTION_ARG_NONE, &version, "Display version information", NULL );



	xmi_init_hdf5();

#ifdef HAVE_OPENMPI
	MPI_Init(&argc, &argv);
#endif


	setbuf(stdout,NULL);
#ifdef G_OS_WIN32
	setbuf(stderr,NULL);
#endif
	//feenableexcept(FE_DIVBYZERO | FE_INVALID | FE_OVERFLOW);



#ifdef HAVE_OPENMPI
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Get_processor_name(processor_name, &namelen);
#endif

	//locale...
	//setlocale(LC_ALL,"C");
#if defined(G_OS_WIN32)
	setlocale(LC_ALL,"English_United States");
#else
	g_setenv("LANG","en_US",TRUE);
#endif

#ifdef G_OS_WIN32
	gchar *equalsignchar;
	for (i = 0 ; i < argc ; i++) {
		if (strncmp(argv[i], "--with-solid-angles-data=", strlen("--with-solid-angles-data=")) == 0) {
			equalsignchar = strchr(argv[i], '=');
			xmimsim_hdf5_solid_angles_utf8 = g_strdup(equalsignchar+1);
		}
		else if (strncmp(argv[i], "--with-escape-ratios-data=", strlen("--with-escape-ratios-data=")) == 0) {
			equalsignchar = strchr(argv[i], '=');
			xmimsim_hdf5_escape_ratios_utf8 = g_strdup(equalsignchar+1);
		}
		else if (strncmp(argv[i], "--with-hdf5-data=", strlen("--with-hdf5-data=")) == 0) {
			equalsignchar = strchr(argv[i], '=');
			hdf5_file_utf8 = g_strdup(equalsignchar+1);
		}
	}
#endif


	//parse options
	context = g_option_context_new("inputfile");
	g_option_context_add_main_entries(context, (const GOptionEntry *) entries->data, NULL);
	g_option_context_set_summary(context, "xmimsim: a program for the Monte-Carlo simulation of X-ray fluorescence spectra");
#ifdef HAVE_OPENMPI
	// unfortunately MPI_Init does not strip its options from argv...
	g_option_context_set_ignore_unknown_options(context, TRUE);
#endif
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_fprintf(stderr, "option parsing failed: %s\n", error->message);
		return 1;
	}

	if (hdf5_file_utf8)
		hdf5_file = hdf5_file_utf8;
	if (xmimsim_hdf5_solid_angles_utf8)
		xmimsim_hdf5_solid_angles = xmimsim_hdf5_solid_angles_utf8;
	if (xmimsim_hdf5_escape_ratios_utf8)
		xmimsim_hdf5_escape_ratios = xmimsim_hdf5_escape_ratios_utf8;

	if (version) {
		g_fprintf(stdout,"%s",xmi_version_string());
		return 0;
	}

	if (argc != 2) {
		fprintf(stderr,"%s\n",g_option_context_get_help(context, TRUE, NULL));
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
		g_fprintf(stdout,"Option M-lines: %i\n", options->use_M_lines);
		g_fprintf(stdout,"Option non-radiative cascade: %i\n", options->use_cascade_auger);
		g_fprintf(stdout,"Option radiative cascade: %i\n", options->use_cascade_radiative);
		g_fprintf(stdout,"Option variance reduction: %i\n", options->use_variance_reduction);
		g_fprintf(stdout,"Option pile-up: %i\n", options->use_sum_peaks);
		g_fprintf(stdout,"Option Poisson noise: %i\n", options->use_poisson);
		g_fprintf(stdout,"Option escape peaks: %i\n", options->use_escape_peaks);
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
		g_fprintf(stdout,"Option OpenCL: %i\n", options->use_opencl);
#endif
		g_fprintf(stdout,"Option number of threads: %i\n", options->omp_num_threads);
	}



	//load xml catalog
	if (xmi_xmlLoadCatalog(&error) == 0) {
		g_fprintf(stderr, "Could not load XML catalog: %s\n", error->message);
		return 1;
	}
	else if (options->verbose)
		g_fprintf(stdout,"XML catalog loaded\n");


	//get the name of the HDF5 data file
	if (xmi_get_hdf5_data_file(&hdf5_file) == 0) {
		return 1;
	}


	//start random number acquisition
	if (xmi_start_random_acquisition() == 0) {
		return 1;
	}



	//read in the inputfile
	input = xmi_input_read_from_xml_file(argv[1], &error);

	if (input == NULL) {
		g_fprintf(stderr, "Could not read %s: %s\n", argv[1], error->message);
		return 1;
	}
	else if (options->verbose)
		g_fprintf(stdout,"Inputfile %s successfully parsed\n",XMI_ARGV_ORIG[XMI_ARGC_ORIG-1]);

	if (options->extra_verbose)
		xmi_input_print(input, stdout);

	//copy to the corresponding fortran variable
	xmi_input_C2F(input,&inputFPtr);

	//initialization
	if (xmi_init_input(&inputFPtr) == 0) {
		return 1;
	}


	if (options->verbose)
		g_fprintf(stdout,"Reading HDF5 datafile\n");

	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5(hdf5_file,inputFPtr,&hdf5FPtr,options) == 0) {
		g_fprintf(stderr,"Could not initialize from hdf5 data file\n");
		return 1;
	}
	else if (options->verbose)
		g_fprintf(stdout,"HDF5 datafile %s successfully processed\n",hdf5_file);

	xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);



	//check solid_angles
#ifdef HAVE_OPENMPI
	if (rank == 0) {
#endif
	if (options->use_variance_reduction == 1) {
		if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0)
			return 1;

		//check if solid angles are already precalculated
		if (options->verbose)
			g_fprintf(stdout,"Querying %s for solid angle grid\n",xmimsim_hdf5_solid_angles);
		if (xmi_find_solid_angle_match(xmimsim_hdf5_solid_angles, input, &solid_angle_def, options) == 0)
			return 1;
		if (solid_angle_def == NULL) {
			if (options->verbose)
				g_fprintf(stdout, "Precalculating solid angle grid\n");
			//doesn't exist yet
			//convert input to string
			if (!xmi_input_write_to_xml_string(input, &xmi_input_string, &error)) {
				g_fprintf(stderr, "Could not write input to XML string: %s\n", error->message);
				return 1;
			}
			xmi_solid_angle_calculation(inputFPtr, &solid_angle_def, xmi_input_string, options);
			g_free(xmi_input_string);
			//update hdf5 file
			if( xmi_update_solid_angle_hdf5_file(xmimsim_hdf5_solid_angles, solid_angle_def) == 0)
				return 1;
			else if (options->verbose)
				g_fprintf(stdout, "%s was successfully updated with new solid angle grid\n", xmimsim_hdf5_solid_angles);
		}
		else if (options->verbose)
			g_fprintf(stdout,"Solid angle grid already present in %s\n", xmimsim_hdf5_solid_angles);

	}
	else if (options->verbose)
		g_fprintf(stdout,"Operating in brute-force mode: solid angle grid is redundant\n");


#ifdef HAVE_OPENMPI
	}
#endif

#ifdef HAVE_OPENMPI
	MPI_Barrier(MPI_COMM_WORLD);
	//read solid angles for the other nodes
	if (options->use_variance_reduction == 1 && rank != 0) {
		if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0)
			return 1;

		if (xmi_find_solid_angle_match(xmimsim_hdf5_solid_angles, input, &solid_angle_def, options) == 0)
			return 1;

		if (solid_angle_def == NULL) {
			g_fprintf(stdout,"Could not find solid angle in HDF5 file (but it should be there since it was created)\n");
			return 1;
		}
	}
	MPI_Barrier(MPI_COMM_WORLD);
#endif

	if (options->verbose)
		g_fprintf(stdout,"Simulating interactions\n");

	if (xmi_main_msim(inputFPtr, hdf5FPtr, numprocs, &channels, options, &brute_history, &var_red_history, solid_angle_def) == 0) {
		g_fprintf(stderr,"Error in xmi_main_msim\n");
		return 1;
	}
	if (solid_angle_def != NULL)
		xmi_free_solid_angle(solid_angle_def);

	if (options->verbose)
		g_fprintf(stdout,"Interactions simulation finished\n");



	//free what needs freeing
#ifdef HAVE_OPENMPI
	if (rank != 0) {
		xmi_free_input_F(&inputFPtr);
		if (xmi_end_random_acquisition() == 0) {
			return 1;
		}
	}
#endif
	xmi_free_hdf5_F(&hdf5FPtr);






#ifdef HAVE_OPENMPI
	/*	MPI_Isend(channels, 2048, MPI_DOUBLE, 0, rank, MPI_COMM_WORLD, reqs+numreqs);
		MPI_Waitall(numreqs, reqs, MPI_STATUSES_IGNORE);
		*/



	if (rank == 0) {
		channelsdef = (double *) g_malloc0((input->general->n_interactions_trajectory+1)*input->detector->nchannels*sizeof(double));
		brute_historydef = (double *) g_malloc0(100*(383+2)*input->general->n_interactions_trajectory*sizeof(double));
		if (options->use_variance_reduction == 1)
			var_red_historydef = (double *) g_malloc0(100*(383+2)*input->general->n_interactions_trajectory*sizeof(double));
	}
	MPI_Barrier(MPI_COMM_WORLD);

	//reduce channels
	MPI_Reduce(channels, channelsdef,(1+input->general->n_interactions_trajectory)*input->detector->nchannels, MPI_DOUBLE,MPI_SUM, 0, MPI_COMM_WORLD);
	//reduce brute_history
	MPI_Reduce(brute_history, brute_historydef,100*(383+2)*input->general->n_interactions_trajectory, MPI_DOUBLE,MPI_SUM, 0, MPI_COMM_WORLD);
	//reduce var_red_history
	if (options->use_variance_reduction == 1)
		MPI_Reduce(var_red_history, var_red_historydef,100*(383+2)*input->general->n_interactions_trajectory, MPI_DOUBLE,MPI_SUM, 0, MPI_COMM_WORLD);


	MPI_Finalize();


#else
	channelsdef = channels;
	brute_historydef = brute_history;
	var_red_historydef = var_red_history;
#endif


#define ARRAY3D_FORTRAN(array,i,j,k,Ni,Nj,Nk) (array[(Nj)*(Nk)*(i-1)+(Nk)*(j-1)+(k-1)])
//watch out, I'm doing something naughty here :-)
#define ARRAY2D_FORTRAN(array,i,j,Ni,Nj) (array[(Nj)*(i)+(j)])




#ifdef HAVE_OPENMPI
	if (rank == 0) {

#if DEBUG == 1
		fprintf(stdout,"Ba-KL2: %li\n",ARRAY3D_FORTRAN(brute_historydef,56,abs(KL2_LINE),1,100,385,1));
//		fprintf(stdout,"Ni-KL3: %i\n",ARRAY3D_FORTRAN(brute_historydef,28,abs(KL3_LINE),1,100,385,2));
//		fprintf(stdout,"Fe-KL3: %i\n",ARRAY3D_FORTRAN(brute_historydef,26,abs(KL3_LINE),1,100,385,2));
//		fprintf(stdout,"Ni-KL3: %i\n",ARRAY3D_FORTRAN(brute_historydef,28,abs(KL3_LINE),2,100,385,2));
//		fprintf(stdout,"Fe-KL3: %i\n",ARRAY3D_FORTRAN(brute_historydef,26,abs(KL3_LINE),2,100,385,2));
#endif



#endif
		//check sum of zero interaction... can only be different from zero if the detector is placed along the beam (which is probably not the smartest thing to do...)
		zero_sum = xmi_sum_double(channelsdef, input->detector->nchannels);

#if DEBUG == 2
		fprintf(stdout,"zero_sum: %g\n",zero_sum);
#endif




		//convolute spectrum
		channels_conv = (double **) g_malloc(sizeof(double *)*(input->general->n_interactions_trajectory+1));
#if DEBUG == 2
		for (i=(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++)
			fprintf(stdout,"channel 223 contents unspoiled: %g\n",channelsdef[i*input->detector->nchannels+222]);

#endif

		//read escape ratios
		if (options->use_escape_peaks) {
			if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0)
				return 1;

			if (options->verbose)
				g_fprintf(stdout,"Querying %s for escape peak ratios\n",xmimsim_hdf5_escape_ratios);

			//check if escape ratios are already precalculated
			if (xmi_find_escape_ratios_match(xmimsim_hdf5_escape_ratios , input, &escape_ratios_def, options) == 0)
				return 1;
			if (escape_ratios_def == NULL) {
				if (options->verbose)
					g_fprintf(stdout,"Precalculating escape peak ratios\n");
				//doesn't exist yet
				//convert input to string
				if (!xmi_input_write_to_xml_string(input, &xmi_input_string, &error)) {
					g_fprintf(stderr, "Could not write input to XML string: %s\n", error->message);
					return 1;
				}
				xmi_escape_ratios_calculation(input, &escape_ratios_def, xmi_input_string,hdf5_file,options, xmi_get_default_escape_ratios_options());
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

		double **channels_def_ptrs = g_malloc(sizeof(double *) * (input->general->n_interactions_trajectory+1));
		for (i = 0 ; i <= input->general->n_interactions_trajectory ; i++)
			channels_def_ptrs[i] = channelsdef+i*input->detector->nchannels;


		if (options->custom_detector_response == NULL) {
			xmi_detector_convolute_all(inputFPtr, channels_def_ptrs, channels_conv, brute_historydef, options->use_variance_reduction == 1 ? var_red_historydef : NULL, options, escape_ratios_def, input->general->n_interactions_trajectory, zero_sum > 0.0 ? 1 : 0);
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
			xmi_detector_convolute_all_custom(inputFPtr, channels_def_ptrs, channels_conv, brute_historydef, options->use_variance_reduction == 1 ? var_red_historydef : NULL, options, escape_ratios_def, input->general->n_interactions_trajectory, zero_sum > 0.0 ? 1 : 0);
			if (!g_module_close(module)) {
				fprintf(stderr,"Warning: could not close module %s: %s\n",options->custom_detector_response, g_module_error());
			}
		}


		g_free(channels_def_ptrs);
		if (options->use_escape_peaks) {
			xmi_free_escape_ratios(escape_ratios_def);
		}

		csv_convPtr = csv_noconvPtr = NULL;

		if (csv_file_noconv != NULL) {
			if ((csv_noconvPtr = fopen(csv_file_noconv,"w")) == NULL) {
				g_fprintf(stderr,"Could not write to %s\n",csv_file_noconv);
				return 1;
			}
			else if (options->verbose)
				g_fprintf(stdout,"Writing to CSV file %s\n",csv_file_noconv);
		}
		if (csv_file_conv != NULL) {
			if ((csv_convPtr = fopen(csv_file_conv,"w")) == NULL) {
				g_fprintf(stderr,"Could not write to %s\n",csv_file_conv);
				return 1;
			}
			else if (options->verbose)
				g_fprintf(stdout,"Writing to CSV file %s\n",csv_file_conv);
		}


		for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {

			//write it to outputfile... spe style
			if (spe_file_noconv != NULL) {
				filename = g_strdup_printf("%s_%i.spe",spe_file_noconv,i);
				if ((outPtr=fopen(filename,"w")) == NULL ) {
					g_fprintf(stderr,"Could not write to %s\n",filename);
					exit(1);
				}
				else if (options->verbose)
					g_fprintf(stdout,"Writing to SPE file %s\n",filename);
				g_free(filename);
				fprintf(outPtr,"$SPEC_ID:\n\n");
				fprintf(outPtr,"$MCA_CAL:\n2\n");
				fprintf(outPtr,"%g %g\n\n", input->detector->zero, input->detector->gain);
				fprintf(outPtr,"$DATA:\n");
				fprintf(outPtr,"0\t%i\n",input->detector->nchannels-1);
				for (j=0 ; j < input->detector->nchannels ; j++) {
					fprintf(outPtr,"%g",ARRAY2D_FORTRAN(channelsdef,i,j,input->general->n_interactions_trajectory+1,input->detector->nchannels));
					if ((j+1) % 8 == 0) {
						fprintf(outPtr,"\n");
					}
					else {
						fprintf(outPtr,"     ");
					}
				}
				fclose(outPtr);
			}
			//convoluted spectrum
			if (spe_file_conv != NULL) {
				filename = g_strdup_printf("%s_%i.spe",spe_file_conv,i);
				if ((outPtr=fopen(filename,"w")) == NULL ) {
					g_fprintf(stderr,"Could not write to %s\n",filename);
					exit(1);
				}
				else if (options->verbose)
					g_fprintf(stdout,"Writing to SPE file %s\n",filename);
				g_free(filename);
				fprintf(outPtr,"$SPEC_ID:\n\n");
				fprintf(outPtr,"$MCA_CAL:\n2\n");
				fprintf(outPtr,"%g %g\n\n", input->detector->zero, input->detector->gain);
				fprintf(outPtr,"$DATA:\n");
				fprintf(outPtr,"0\t%i\n", input->detector->nchannels-1);
				for (j=0 ; j < input->detector->nchannels ; j++) {
					fprintf(outPtr,"%g",channels_conv[i][j]);
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


		//csv file unconvoluted
		if (csv_noconvPtr != NULL) {
			for (j=0 ; j < input->detector->nchannels ; j++) {
				fprintf(csv_noconvPtr,"%i,%g",j,(j)*input->detector->gain+input->detector->zero);
				for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
					//channel number, energy, counts...
					fprintf(csv_noconvPtr,",%g",ARRAY2D_FORTRAN(channelsdef,i,j,input->general->n_interactions_trajectory+1, input->detector->nchannels));
				}
				fprintf(csv_noconvPtr,"\n");
			}
			fclose(csv_noconvPtr);
		}

		//csv file convoluted
		if (csv_convPtr != NULL) {
			for (j=0 ; j < input->detector->nchannels ; j++) {
				fprintf(csv_convPtr,"%i,%g",j,(j)*input->detector->gain+input->detector->zero);
				for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
					//channel number, energy, counts...
					fprintf(csv_convPtr,",%g",channels_conv[i][j]);
				}
				fprintf(csv_convPtr,"\n");
			}
			fclose(csv_convPtr);
		}





#if DEBUG == 1
		fprintf(stdout,"Writing outputfile\n");
#endif

		//write to xml outputfile
		xmi_output *output = xmi_output_raw2struct(input, brute_historydef, options->use_variance_reduction == 1 ? var_red_historydef : NULL, channels_conv, channelsdef, argv[1], zero_sum > 0.0 ? 1 : 0);
		if (!xmi_output_write_to_xml_file(output, input->general->outputfile, &error)) {
			g_fprintf(stderr, "Could not write input to XML file %s: %s\n", input->general->outputfile, error->message);

			return 1;
		}
		else if (options->verbose)
			g_fprintf(stdout,"Output written to XMSO file %s\n",input->general->outputfile);

		xmi_output_free(output);
		if (svg_file_conv != NULL) {
			// 1 = convoluted
			if (xmi_xmso_to_svg_xslt(input->general->outputfile, svg_file_conv, 1) == 0) {
				return 1;
			}
			else if (options->verbose)
				g_fprintf(stdout,"Output written to SVG file %s\n",svg_file_conv);
		}

		if (svg_file_noconv != NULL) {
                        // 0 = unconvoluted
			if (xmi_xmso_to_svg_xslt(input->general->outputfile, svg_file_noconv, 0) == 0) {
				return 1;
			}
			else if (options->verbose)
				g_fprintf(stdout,"Output written to SVG file %s\n",svg_file_noconv);
		}


		if (htm_file_conv != NULL) {
			// 1 = convoluted
			if (xmi_xmso_to_htm_xslt(input->general->outputfile, htm_file_conv, 1) == 0) {
				return 1;
			}
			else if (options->verbose)
				g_fprintf(stdout,"Output written to HTML file %s\n",htm_file_conv);
		}

		if (htm_file_noconv != NULL) {
                        // 0 = unconvoluted
			if (xmi_xmso_to_htm_xslt(input->general->outputfile, htm_file_noconv, 0) == 0) {
				return 1;
			}
			else if (options->verbose)
				g_fprintf(stdout,"Output written to HTML file %s\n",htm_file_noconv);
		}


		if (xmi_end_random_acquisition() == 0) {
			return 1;
		}

		for (i=(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
			g_free(channels_conv[i] );
		}
		g_free(channels_conv);
		/* Do not deallocate as problems may arise in OpenMPI mode
		g_free(channelsdef);
		g_free(brute_history);
		if (options->use_variance_reduction)
			g_free(var_red_history);
		*/
#ifdef HAVE_OPENMPI
	}
#endif

	xmi_input_free(input);
#ifdef HAVE_OPENMPI
	if (rank == 0) {
#endif
		xmi_free_input_F(&inputFPtr);
#ifdef HAVE_OPENMPI
	}
#endif

	return 0;
}


