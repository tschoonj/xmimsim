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

#include "xmi_main.h"
#include "xmi_aux.h"
#include "xmi_xml.h"
#include "xmi_xslt.h"
#include "xmi_hdf5.h"
#include <xraylib.h>
#include <glib.h>
#include <glib/gstdio.h>
#include "xmi_pymca.h"
#include "xmi_data_structs.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "xmi_random.h"
#include "xmi_detector.h"
#include <omp.h>
#include <math.h>

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
#ifdef _WIN64
  #warn Win 64 platform detected... proceed at your own risk...
#endif

#ifdef MAC_INTEGRATION
#include "xmi_resources_mac.h"
#endif

XMI_MAIN
	char *xmimsim_hdf5_solid_angles=NULL;
	struct xmi_input *xi = NULL;
	struct xmi_pymca *xp = NULL ;
	struct xmi_layer *matrix;
	double *weights_arr_quant;
	int i,j,k;
	xmi_inputFPtr inputFPtr;
	xmi_hdf5FPtr hdf5FPtr;
	GError *error = NULL;
	GOptionContext *context;
	static gchar *hdf5_file=NULL;
	static struct xmi_main_options options;
	static gchar *spe_file_noconv=NULL;
	static gchar *spe_file_conv=NULL;
	static gchar *csv_file_noconv=NULL;
	static gchar *csv_file_conv=NULL;
	static gchar *svg_file_noconv=NULL;
	static gchar *svg_file_conv=NULL;
	FILE *outPtr, *csv_convPtr, *csv_noconvPtr;
	char filename[512];
	static int use_rayleigh_normalization = 0;
	static int use_roi_normalization = 0;
	static int use_matrix_override= 0;
	static int use_single_run= 0;
	int rayleigh_channel;
	int matched;
	double max_scale;
	double *scale, sum_scale, *k_exp, *k_sim, *l_exp, *l_sim;
	struct xmi_escape_ratios *escape_ratios_def=NULL;
	char *xmimsim_hdf5_escape_ratios = NULL;
	double sum_roi;
	static int version = 0;
	
	
	static GOptionEntry entries[] = {
		{ "enable-M-lines", 0, 0, G_OPTION_ARG_NONE, &(options.use_M_lines), "Enable M lines (default)", NULL },
		{ "disable-M-lines", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_M_lines), "Disable M lines", NULL },
		{"spe-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&spe_file_noconv,"Write detector unconvoluted spectra to file",NULL},
		{"spe-file",0,0,G_OPTION_ARG_FILENAME,&spe_file_conv,"Write detector convoluted spectra to file",NULL},
		{"csv-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&csv_file_noconv,"Write detector unconvoluted spectra to CSV file",NULL},
		{"csv-file",0,0,G_OPTION_ARG_FILENAME,&csv_file_conv,"Write detector convoluted spectra to CSV file",NULL},
		{"svg-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&svg_file_noconv,"Write detector unconvoluted spectra to SVG file",NULL},
		{"svg-file",0,0,G_OPTION_ARG_FILENAME,&svg_file_conv,"Write detector convoluted spectra to SVG file",NULL},
		{"with-hdf5-data",0,0,G_OPTION_ARG_FILENAME,&hdf5_file,"Select a HDF5 data file (advanced usage)",NULL},
		{"enable-scatter-normalization", 0, 0, G_OPTION_ARG_NONE,&use_rayleigh_normalization,"Enable Rayleigh peak based intensity normalization",NULL},
		{"disable-scatter-normalization", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE,&use_rayleigh_normalization,"Disable Rayleigh peak based intensity normalization (default)",NULL},
		{"enable-roi-normalization", 0, 0, G_OPTION_ARG_NONE,&use_roi_normalization,"Enable region of interest integration based intensity normalization",NULL},
		{"disable-roi-normalization", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE,&use_roi_normalization,"Disable region of interest integration based intensity normalization (default)",NULL},
		{"enable-matrix-override", 0, 0, G_OPTION_ARG_NONE,&use_matrix_override,"If the matrix includes quantifiable elements, use a similar matrix instead",NULL},
		{"disable-matrix-override", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE,&use_matrix_override,"If the matrix includes quantifiable elements, do not use a similar matrix instead (default)",NULL},
		{ "enable-pile-up", 0, 0, G_OPTION_ARG_NONE, &(options.use_sum_peaks), "Enable pile-up", NULL },
		{ "disable-pile-up", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_sum_peaks), "Disable pile-up (default)", NULL },
		{ "enable-poisson", 0, 0, G_OPTION_ARG_NONE, &(options.use_poisson), "Generate Poisson noise in the spectra", NULL },
		{ "disable-poisson", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_poisson), "Disable the generating of spectral Poisson noise (default)", NULL },
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
		{"enable-opencl", 0, 0, G_OPTION_ARG_NONE, &(options.use_opencl), "Enable OpenCL (default)", NULL },
		{"disable-opencl", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_opencl), "Disable OpenCL", NULL },
#endif
		{"set-threads",0,0,G_OPTION_ARG_INT,&(options.omp_num_threads),"Set the number of threads (default=max)",NULL},
		{ "enable-single-run", 0, 0, G_OPTION_ARG_NONE, &use_single_run, "Force the simulation to run just once", NULL },
		{ "verbose", 'v', 0, G_OPTION_ARG_NONE, &(options.verbose), "Verbose mode", NULL },
		{ "version", 0, 0, G_OPTION_ARG_NONE, &version, "display version information", NULL },
		{NULL}
	};
	double *channels;
	double **channels_conv;
	double **channels_conv_temp;
	double *channels_conv_temp2;
	char tempFile[100];
	double zero_sum;
	double *brute_history;
	double *var_red_history;
	double sum_k, sum_l, sum_temp;
	struct xmi_solid_angle *solid_angle_def;
	char *xmi_input_string;



	options.use_M_lines = 1;
	options.use_cascade_auger = 1;
	options.use_cascade_radiative = 1;
	options.use_variance_reduction = 1;
	options.use_optimizations = 1;
	options.use_sum_peaks = 0;
	options.use_poisson = 0;
	options.verbose = 0;
	options.use_opencl = 1;
	options.extra_verbose = 0;
	options.omp_num_threads = omp_get_max_threads();


	//parse options
	context = g_option_context_new ("inputfile outputfile");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmimsim-pymca: a program for the quantification of X-ray fluorescence spectra using inverse Monte-Carlo simulations. Inputfiles should be prepared using PyMCA\n");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_fprintf (stderr, "option parsing failed: %s\n", error->message);
		return 1;
	}
	if (version) {
		g_fprintf(stdout,"%s",xmi_version_string());	
		return 0;
	}

	//check for conflicting options
	if (use_rayleigh_normalization + use_roi_normalization + use_matrix_override + use_single_run > 1) {
		g_fprintf(stderr,"Options conflict: Use either --enable-rayleigh-normalization or --enable-roi-normalization or --enable-matrix-override or --enable-single-run. No combinations of these are allowed\n");
		exit(1);
	}
	

	if (options.omp_num_threads > omp_get_max_threads() ||
			options.omp_num_threads < 1) {
		options.omp_num_threads = omp_get_max_threads();
	}

	//omp_set_num_threads(omp_num_threads);
	//omp_set_dynamic(0);

	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		return 1;
	}
	else if (options.verbose)
		g_fprintf(stdout,"XML catalog loaded\n");



	if (xmi_get_hdf5_data_file(&hdf5_file) == 0) {
		return 1;
	}


	if (argc != 3) {
		g_fprintf(stderr,"%s\n",g_option_context_get_help(context, TRUE, NULL));
		return 1;
	}


	//start random number acquisition
	if (xmi_start_random_acquisition() == 0) {
		return 1;
	}

	if(xmi_read_input_pymca(argv[1], &xi, &xp, use_matrix_override, use_roi_normalization, use_single_run) == 0)
		return 1;
	else if (options.verbose)
		g_fprintf(stdout,"Inputfile %s successfully parsed\n",XMI_ARGV_ORIG[XMI_ARGC_ORIG-2]);

#if DEBUG == 2
	xmi_print_input(stdout,xi);
#endif



	if (use_single_run) {
		//just simulate...
		//copy to the corresponding fortran variable
		xmi_input_C2F(xi,&inputFPtr);
		//initialization
		if (xmi_init_input(&inputFPtr) == 0) {
			return 1;
		}
		if (options.verbose)
			g_fprintf(stdout,"Reading HDF5 datafile\n");

		//initialize HDF5 data
		//read from HDF5 file what needs to be read in
		if (xmi_init_from_hdf5(hdf5_file,inputFPtr,&hdf5FPtr,options) == 0) {
			g_fprintf(stderr,"Could not initialize from hdf5 data file\n");
			return 1;
		}	
		else if (options.verbose)
			g_fprintf(stdout,"HDF5 datafile %s successfully processed\n",hdf5_file);

		xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);
		
		//determine filename first
		if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0)
			return 1;

		if (options.verbose)
			g_fprintf(stdout,"Querying %s for solid angle grid\n",xmimsim_hdf5_solid_angles);


		//check if solid angles are already precalculated
		if (xmi_find_solid_angle_match(xmimsim_hdf5_solid_angles , xi, &solid_angle_def, options) == 0)
			return 1;
		if (solid_angle_def == NULL) {
			if (options.verbose)
				g_fprintf(stdout,"Precalculating solid angle grid\n");
			//doesn't exist yet
			//convert input to string
			if (xmi_write_input_xml_to_string(&xmi_input_string,xi) == 0) {
				return 1;
			}
			xmi_solid_angle_calculation(inputFPtr, &solid_angle_def, xmi_input_string, options);
			//update hdf5 file
			if( xmi_update_solid_angle_hdf5_file(xmimsim_hdf5_solid_angles , solid_angle_def) == 0)
				return 1;
			else if (options.verbose)
				g_fprintf(stdout,"%s was successfully updated with new solid angle grid\n",xmimsim_hdf5_solid_angles);
		}
		else if (options.verbose)
			g_fprintf(stdout,"Solid angle grid already present in %s\n",xmimsim_hdf5_solid_angles);
		//launch simulation
		if (options.verbose)
			g_fprintf(stdout,"Simulating interactions\n");

		if (xmi_main_msim(inputFPtr, hdf5FPtr, 1, &channels, xp->nchannels ,options, &brute_history, &var_red_history, solid_angle_def) == 0) {
			g_fprintf(stderr,"Error in xmi_main_msim\n");
			return 1;
		}

		if (options.verbose)
			g_fprintf(stdout,"Interactions simulation finished\n");

		//read escape ratios
		if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0)
			return 1;

		if (options.verbose)
			g_fprintf(stdout,"Querying %s for escape peak ratios\n",xmimsim_hdf5_escape_ratios);

		//check if escape ratios are already precalculated
		if (xmi_find_escape_ratios_match(xmimsim_hdf5_escape_ratios , xi, &escape_ratios_def, options) == 0)
			return 1;
		if (escape_ratios_def == NULL) {
			if (options.verbose)
				g_fprintf(stdout,"Precalculating escape peak ratios\n");
			//doesn't exist yet
			//convert input to string
			if (xmi_write_input_xml_to_string(&xmi_input_string,xi) == 0) {
				return 1;
			}
			xmi_escape_ratios_calculation(xi, &escape_ratios_def, xmi_input_string,hdf5_file,options);
			//update hdf5 file
			if( xmi_update_escape_ratios_hdf5_file(xmimsim_hdf5_escape_ratios , escape_ratios_def) == 0)
				return 1;
			else if (options.verbose)
				g_fprintf(stdout,"%s was successfully updated with new escape peak ratios\n",xmimsim_hdf5_escape_ratios);
		}
		else if (options.verbose)
			g_fprintf(stdout,"Escape peak ratios already present in %s\n",xmimsim_hdf5_escape_ratios);
	
		//the dreaded goto statement
		goto single_run;
	}






	//calculate initial 
	xmi_copy_layer(xi->composition->layers + xp->ilay_pymca, &matrix);




	weights_arr_quant = (double *) malloc(sizeof(double)*xp->n_z_arr_quant);

	if (xp->n_z_arr_pymca_conc == 0) {
		for (i = 0 ; i < xp->n_z_arr_quant ; i++)
			weights_arr_quant[i] = XMI_PYMCA_START_CONC;
	}
	else {
#if DEBUG == 1
		fprintf(stdout,"using pymca concentrations\n");
#endif
		for (i = 0 ; i < xp->n_z_arr_quant ; i++) {
			weights_arr_quant[i] = XMI_PYMCA_START_CONC;
			for (j = 0 ; j < xp->n_z_arr_pymca_conc ; j++) {
				if (xp->z_arr_quant[i] == xp->z_arr_pymca_conc[j]) {
					weights_arr_quant[i] = xp->weight_arr_pymca_conc[j];
#if DEBUG == 1
					fprintf(stdout,"Element %i weight: %lf\n",xp->z_arr_quant[i],weights_arr_quant[i]);
#endif
					break;
				}	
			}
		}	
	}


	xi->composition->layers[xp->ilay_pymca] = xmi_ilay_composition_pymca(matrix, xp, weights_arr_quant); 

#if DEBUG == 2
	xmi_print_layer(stdout,xi->composition->layers+xp->ilay_pymca,1);
#endif

	//copy to the corresponding fortran variable
	xmi_input_C2F(xi,&inputFPtr);
	//initialization
	if (xmi_init_input(&inputFPtr) == 0) {
		return 1;
	}
	//initialize HDF5 data
	if (options.verbose)
		g_fprintf(stdout,"Reading HDF5 datafile\n");

	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5(hdf5_file,inputFPtr,&hdf5FPtr,options) == 0) {
		g_fprintf(stderr,"Could not initialize from hdf5 data file\n");
		return 1;
	}	
	else if (options.verbose)
		g_fprintf(stdout,"HDF5 datafile %s successfully processed\n",hdf5_file);

	xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);

	//determine filename first
	if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0)
		return 1;

	if (options.verbose)
		g_fprintf(stdout,"Querying %s for solid angle grid\n",xmimsim_hdf5_solid_angles);


	//check if solid angles are already precalculated
	if (xmi_find_solid_angle_match(xmimsim_hdf5_solid_angles , xi, &solid_angle_def, options) == 0)
		return 1;
	if (solid_angle_def == NULL) {
		if (options.verbose)
			g_fprintf(stdout,"Precalculating solid angle grid\n");
		//doesn't exist yet
		//convert input to string
		if (xmi_write_input_xml_to_string(&xmi_input_string,xi) == 0) {
			return 1;
		}
		xmi_solid_angle_calculation(inputFPtr, &solid_angle_def, xmi_input_string, options);
		//update hdf5 file
		if( xmi_update_solid_angle_hdf5_file(xmimsim_hdf5_solid_angles , solid_angle_def) == 0)
			return 1;
		else if (options.verbose)
			g_fprintf(stdout,"%s was successfully updated with new solid angle grid\n",xmimsim_hdf5_solid_angles);
	}
	else if (options.verbose)
		g_fprintf(stdout,"Solid angle grid already present in %s\n",xmimsim_hdf5_solid_angles);

	//everything is read in... start iteration
	i = 0;

#define ARRAY3D_FORTRAN(array,i,j,k,Ni,Nj,Nk) (array[Nj*Nk*(i-1)+Nk*(j-1)+(k-1)])

	sum_k = sum_l = XMI_PYMCA_CONV_THRESHOLD*10.0;

	channels = NULL;
	brute_history = NULL;
	var_red_history = NULL;



	//read escape ratios
	
	SetErrorMessages(0);

	if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0)
		return 1;

	if (options.verbose)
		g_fprintf(stdout,"Querying %s for escape peak ratios\n",xmimsim_hdf5_escape_ratios);


	//check if escape ratios are already precalculated
	if (xmi_find_escape_ratios_match(xmimsim_hdf5_escape_ratios , xi, &escape_ratios_def, options) == 0)
		return 1;
	if (escape_ratios_def == NULL) {
		if (options.verbose)
			g_fprintf(stdout,"Precalculating escape peak ratios\n");
		//doesn't exist yet
		//convert input to string
		if (xmi_write_input_xml_to_string(&xmi_input_string,xi) == 0) {
			return 1;
		}
		xmi_escape_ratios_calculation(xi, &escape_ratios_def, xmi_input_string,hdf5_file,options);
		//update hdf5 file
		if( xmi_update_escape_ratios_hdf5_file(xmimsim_hdf5_escape_ratios , escape_ratios_def) == 0)
			return 1;
		else if (options.verbose)
			g_fprintf(stdout,"%s was successfully updated with new escape peak ratios\n",xmimsim_hdf5_escape_ratios);
	}
	else if (options.verbose)
		g_fprintf(stdout,"Escape peak ratios already present in %s\n",xmimsim_hdf5_escape_ratios);

	if (use_rayleigh_normalization && xp->scatter_energy > 0.0 && xp->scatter_intensity > 0.0) {
		rayleigh_channel = (int) ((xp->scatter_energy - xi->detector->zero)/xi->detector->gain);
		if (rayleigh_channel > xp->nchannels) {
			g_fprintf(stderr,"Channel of excitation energy is not included in spectrum from pymca\n");
			return 1;
		}
#if DEBUG == 1
		fprintf(stdout,"rayleigh_channel: %i\n", rayleigh_channel);
#endif
	}



	scale = (double *) malloc(sizeof(double)*xp->n_z_arr_quant);
	k_exp= (double *) malloc(sizeof(double)*xp->n_z_arr_quant);
	k_sim = (double *) malloc(sizeof(double)*xp->n_z_arr_quant);
	l_exp = (double *) malloc(sizeof(double)*xp->n_z_arr_quant);
	l_sim= (double *) malloc(sizeof(double)*xp->n_z_arr_quant);


	//fill up k_exp and l_exp
	for (j = 0 ; j < xp->n_z_arr_quant ; j++) {
		for (k = 0 ; k < xp->n_peaks ; k++) {
			if (xp->z_arr[k] == xp->z_arr_quant[j]) {
				if (xp->k_alpha[k] > 0.0) {
					k_exp[j] = xp->k_alpha[k];
					l_exp[j] = 0.0;
				}
				else if (xp->l_alpha[k] > 0.0) {
					l_exp[j] = xp->l_alpha[k];
					k_exp[j] = 0.0;
				}
				else {
					g_fprintf(stdout,"k_exp/l_exp problem detected\n");
					return 1;
				}
				break;
			}
		}
	}

	//g_fprintf(stdout, "sum_xmin_xmax: %lf\n", xp->sum_xmin_xmax);

	if (use_roi_normalization) {
		sum_roi = 5*xp->sum_xmin_xmax;
	}
	else {
		sum_roi = xp->sum_xmin_xmax;
	}

#define ARRAY2D_FORTRAN(array,i,j,Ni,Nj) (array[(Nj)*(i)+(j)])

	while ((sum_k > XMI_PYMCA_CONV_THRESHOLD) || (sum_l > XMI_PYMCA_CONV_THRESHOLD) || fabs(sum_roi-xp->sum_xmin_xmax)/xp->sum_xmin_xmax > 0.05) {
		xmi_deallocate(channels);
		xmi_deallocate(brute_history);
		xmi_deallocate(var_red_history);

		if (i++ > XMI_PYMCA_MAX_ITERATIONS) {
			g_fprintf(stderr,"No convergence after %i iterations... Fatal error\n",i);
			return 0;
		}
#if DEBUG == 1
		sprintf(tempFile, "xmimsim-pymca_debug_%i.xmsi",i);
		xmi_write_input_xml(tempFile, xi);	
#endif



		//launch simulation
		if (options.verbose)
			g_fprintf(stdout,"Simulating interactions\n");

		if (xmi_main_msim(inputFPtr, hdf5FPtr, 1, &channels, xp->nchannels ,options, &brute_history, &var_red_history, solid_angle_def) == 0) {
			g_fprintf(stderr,"Error in xmi_main_msim\n");
			return 1;
		}
		if (options.verbose)
			g_fprintf(stdout,"Interactions simulation finished\n");
#if DEBUG == 1
		//write input structure
		//xmi_print_input(stdout,xi);
		zero_sum = xmi_sum_double(channels, xp->nchannels);
		//convolute_spectrum
		channels_conv_temp = (double **) malloc(sizeof(double *)*(xi->general->n_interactions_trajectory+1));
	
		for (j=(zero_sum > 0.0 ? 0 : 1) ; j <= xi->general->n_interactions_trajectory ; j++) {
			xmi_detector_convolute(inputFPtr, hdf5FPtr, channels+j*xp->nchannels, &channels_conv_temp2, xp->nchannels, options);
			channels_conv_temp[i] = xmi_memdup(channels_conv_temp2,sizeof(double)*xp->nchannels);
		}
		//write to xml outputfile
		sprintf(tempFile, "xmimsim-pymca_debug_%i.xmso",i);
		if (xmi_write_output_xml(tempFile, xi, brute_history, options.use_variance_reduction == 1 ? var_red_history : NULL, channels_conv_temp, channels, xp->nchannels, argv[1], zero_sum > 0.0 ? 1 : 0) == 0) {
			return 1;
		}



#endif

		

		//optimize concentrations
		//if normalization is enabled -> do not optimize after first run. Only the intensity of the exciting radiation will be adjusted in this case
		if (!(use_rayleigh_normalization && xp->scatter_energy > 0.0 && xp->scatter_intensity > 0.0 && i == 1) && !(use_roi_normalization && i % 2 == 1)) {
			if (options.verbose)
				g_fprintf(stdout, "Recalculating weight fractions\n");

			sum_k = sum_l = 0.0;
			sum_scale = 0.0;
			for (j = 0 ; j < xp->n_z_arr_quant ; j++) {
#if DEBUG == 1
				fprintf(stdout,"Element :%i\n",xp->z_arr_quant[j]);
#endif

				k_sim[j] = 0.0;	
				l_sim[j] = 0.0;	

				for (k = 0 ; k <= xi->general->n_interactions_trajectory ; k++) {
					k_sim[j] += ARRAY3D_FORTRAN(var_red_history, xp->z_arr_quant[j], abs(KL2_LINE),k,100,385,xi->general->n_interactions_trajectory);
					k_sim[j] += ARRAY3D_FORTRAN(var_red_history, xp->z_arr_quant[j], abs(KL3_LINE),k,100,385,xi->general->n_interactions_trajectory);
				}
				for (k = 0 ; k <= xi->general->n_interactions_trajectory ; k++) {
					l_sim[j] += ARRAY3D_FORTRAN(var_red_history, xp->z_arr_quant[j], abs(L3M4_LINE),k,100,385,xi->general->n_interactions_trajectory);
					l_sim[j] += ARRAY3D_FORTRAN(var_red_history, xp->z_arr_quant[j], abs(L3M5_LINE),k,100,385,xi->general->n_interactions_trajectory);
				}

				if (k_exp[j] > 0.0 && k_sim[j] > 0.0) {
					scale[j] = k_exp[j]/k_sim[j];
				}
				else if (l_exp[j] > 0.0 && l_sim[j] > 0.0) {
					scale[j] = l_exp[j]/l_sim[j];
				}
				sum_scale += scale[j]*weights_arr_quant[j];

				//K-lines
				//make history_sum
		


#if DEBUG == 1
				fprintf(stdout,"k_exp[j]: %lf\n",k_exp[j]);
				fprintf(stdout,"k_sim[j]: %lf\n",k_sim[j]);
				fprintf(stdout,"l_exp[j]: %lf\n",l_exp[j]);
				fprintf(stdout,"l_sim[j]: %lf\n",l_sim[j]);
				fprintf(stdout,"scale[j]: %lf\n",scale[j]);
	//			fprintf(stdout,"scatter_intensity from file: %lf\n",xp->scatter_intensity);
	//			fprintf(stdout,"scatter_intensity from MC: %lf\n",ARRAY2D_FORTRAN(channels,xi->general->n_interactions_trajectory,rayleigh_channel,xi->general->n_interactions_trajectory+1,xp->nchannels));
#endif

			}
			for (j = 0 ; j < xp->n_z_arr_quant ; j++) {
				//do not allow too large jumps! 
				//if weight <= 0.0001 then max is 100
				//else if weight <= 0.01 then max is 10
				//else if weight <= 0.1 then max is 2.5
				//else if weight <= 0.25 then max is 1.5
				//else if weight <= 0.375 then max is 1.20

				//scale[j] /= sum_scale;

				if (weights_arr_quant[j] <= 0.0001)
					max_scale = 100.0;
				else if (weights_arr_quant[j] <= 0.01)
					max_scale = 10.0;
				else if (weights_arr_quant[j] <= 0.1)
					max_scale = 2.5;
				else if (weights_arr_quant[j] <= 0.25)
					max_scale = 1.5;
				else if (weights_arr_quant[j] <= 0.375)
					max_scale = 1.2;
				else if (weights_arr_quant[j] <= 0.50)
					max_scale = 1.1;
				else if (weights_arr_quant[j] <= 0.6)
					max_scale = 1.05;
				else if (weights_arr_quant[j] <= 0.7)
					max_scale = 1.025;
				else 
					max_scale = 1.01;

				if (k_exp[j] > 0.0 && k_sim[j] > 0.0  ) {
					sum_temp = 0.0;
					sum_temp += (k_exp[j]-k_sim[j])*(k_exp[j]-k_sim[j]);
					sum_temp /= k_exp[j];
					sum_temp /= k_exp[j];
					sum_k += sum_temp;
					if (scale[j] > max_scale) {
						weights_arr_quant[j] *=	max_scale;
					}
					else {
						weights_arr_quant[j] *= scale[j];
					}
				}
				else if (l_exp[j] > 0.0 && l_sim[j] > 0.0  ) {
					sum_temp = 0.0;
					sum_temp += (l_exp[j]-l_sim[j])*(l_exp[j]-l_sim[j]);
					sum_temp /= l_exp[j];
					sum_temp /= l_exp[j];
					sum_l += sum_temp;
					if (scale[j] > max_scale) {
						weights_arr_quant[j] *=	max_scale;
					}
					else {
						weights_arr_quant[j] *= scale[j];
					}
				}
			}
		}
		//update energy intensities when required
		if (use_rayleigh_normalization && xp->scatter_energy > 0.0 && xp->scatter_intensity > 0.0) {
			if (options.verbose)
				g_fprintf(stdout, "Scaling beam intensity according to Rayleigh signal\n");
			for (j = 0 ; j < xi->excitation->n_discrete ; j++) {
				xi->excitation->discrete[j].horizontal_intensity *= xp->scatter_intensity/ARRAY2D_FORTRAN(channels,xi->general->n_interactions_trajectory,rayleigh_channel,xi->general->n_interactions_trajectory+1,xp->nchannels);
				xi->excitation->discrete[j].vertical_intensity *= xp->scatter_intensity/ARRAY2D_FORTRAN(channels,xi->general->n_interactions_trajectory,rayleigh_channel,xi->general->n_interactions_trajectory+1,xp->nchannels);
			}

			for (j = 0 ; j < xi->excitation->n_continuous ; j++) {
				xi->excitation->continuous[j].horizontal_intensity *= xp->scatter_intensity/ARRAY2D_FORTRAN(channels,xi->general->n_interactions_trajectory,rayleigh_channel,xi->general->n_interactions_trajectory+1,xp->nchannels);
				xi->excitation->continuous[j].vertical_intensity *= xp->scatter_intensity/ARRAY2D_FORTRAN(channels,xi->general->n_interactions_trajectory,rayleigh_channel,xi->general->n_interactions_trajectory+1,xp->nchannels);
			}
			if (i > 1) {
				//update concentrations in input	
				free(xi->composition->layers[xp->ilay_pymca].Z);
				free(xi->composition->layers[xp->ilay_pymca].weight);
				xi->composition->layers[xp->ilay_pymca] = xmi_ilay_composition_pymca(matrix, xp, weights_arr_quant); 
			}
		}
		else if (use_roi_normalization) {
			//integrate the region of interest
			//first apply detector convolution
			if (i % 2 == 1) {
				if (options.verbose)
					g_fprintf(stdout, "Scaling beam intensity according to region of interest intensity integration\n");
				xmi_detector_convolute(inputFPtr, channels+xi->general->n_interactions_trajectory*xp->nchannels, &channels_conv_temp2, xp->nchannels, options, escape_ratios_def);

				sum_roi = 0.0;
				for (j = xp->xmin ; j <= xp->xmax ; j++)
					sum_roi += channels_conv_temp2[j];

				//g_fprintf(stdout,"sum_roi: %lf\n", sum_roi);

				xmi_deallocate(channels_conv_temp2);

				for (j = 0 ; j < xi->excitation->n_discrete ; j++) {
					xi->excitation->discrete[j].horizontal_intensity *= xp->sum_xmin_xmax/sum_roi;
					xi->excitation->discrete[j].vertical_intensity *= xp->sum_xmin_xmax/sum_roi;
				}

				for (j = 0 ; j < xi->excitation->n_continuous ; j++) {
					xi->excitation->continuous[j].horizontal_intensity *= xp->sum_xmin_xmax/sum_roi;
					xi->excitation->continuous[j].vertical_intensity *= xp->sum_xmin_xmax/sum_roi;
				}
			}
			else if (i % 2 == 0) {
				//update concentrations in input	
				free(xi->composition->layers[xp->ilay_pymca].Z);
				free(xi->composition->layers[xp->ilay_pymca].weight);
				xi->composition->layers[xp->ilay_pymca] = xmi_ilay_composition_pymca(matrix, xp, weights_arr_quant); 
			}
		}
		else {
			//update concentrations in input	
			free(xi->composition->layers[xp->ilay_pymca].Z);
			free(xi->composition->layers[xp->ilay_pymca].weight);
			xi->composition->layers[xp->ilay_pymca] = xmi_ilay_composition_pymca(matrix, xp, weights_arr_quant); 
		}
		//reload fortran input
		xmi_free_input_F(&inputFPtr);
		xmi_input_C2F(xi, &inputFPtr);
		if (xmi_init_input(&inputFPtr) == 0) {
			return 1;
		}
		xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);
	
		g_fprintf(stdout,"Iteration: %i\n",i);
		g_fprintf(stdout,"sum_k: %lf\n",sum_k);
		g_fprintf(stdout,"sum_l: %lf\n",sum_l);


	}

single_run:

	
	xmi_free_hdf5_F(&hdf5FPtr);


	zero_sum = xmi_sum_double(channels, xp->nchannels);
	//convolute_spectrum
	channels_conv = (double **) malloc(sizeof(double *)*(xi->general->n_interactions_trajectory+1));
	
	for (i=(zero_sum > 0.0 ? 0 : 1) ; i <= xi->general->n_interactions_trajectory ; i++) {
		xmi_detector_convolute(inputFPtr, channels+i*xp->nchannels, &channels_conv_temp2, xp->nchannels, options,escape_ratios_def);
		channels_conv[i] = xmi_memdup(channels_conv_temp2,sizeof(double)*xp->nchannels);
	}

	if (xmi_end_random_acquisition() == 0) {
		return 1;
	}

	//write to xml outputfile
	if (xmi_write_output_xml(argv[2], xi, brute_history, options.use_variance_reduction == 1 ? var_red_history : NULL, channels_conv, channels, xp->nchannels, argv[1], zero_sum > 0.0 ? 1 : 0) == 0) {
		return 1;
	}
	else if (options.verbose)
		g_fprintf(stdout,"Output written to XMSO file %s\n",XMI_ARGV_ORIG[XMI_ARGC_ORIG-1]);

	//write to CSV and SPE if necessary...
#ifndef G_OS_WIN32
	csv_convPtr = csv_noconvPtr = NULL;

	if (csv_file_noconv != NULL) {
		if ((csv_noconvPtr = fopen(csv_file_noconv,"w")) == NULL) {
			fprintf(stdout,"Could not write to %s\n",csv_file_noconv);
			return 1;
		}
		else if (options.verbose)
			g_fprintf(stdout,"Writing to CSV file %s\n",csv_file_noconv);
	}
	if (csv_file_conv != NULL) {
		if ((csv_convPtr = fopen(csv_file_conv,"w")) == NULL) {
			fprintf(stdout,"Could not write to %s\n",csv_file_conv);
			return 1;
		}
		else if (options.verbose)
			g_fprintf(stdout,"Writing to CSV file %s\n",csv_file_conv);
	}


	for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= xi->general->n_interactions_trajectory ; i++) {

		//write it to outputfile... spe style
		if (spe_file_noconv != NULL) {
			sprintf(filename,"%s_%i.spe",spe_file_noconv,i);
			if ((outPtr=fopen(filename,"w")) == NULL ) {
				fprintf(stdout,"Could not write to %s\n",filename);
				exit(1);
			}
			else if (options.verbose)
				g_fprintf(stdout,"Writing to SPE file %s\n",filename);

			fprintf(outPtr,"$SPEC_ID:\n\n");
				fprintf(outPtr,"$MCA_CAL:\n2\n");
				fprintf(outPtr,"%lf %lf\n\n", xi->detector->zero, xi->detector->gain);
			fprintf(outPtr,"$DATA:\n");
			fprintf(outPtr,"0\t%i\n",xp->nchannels-1);
			for (j=0 ; j < xp->nchannels ; j++) {
				fprintf(outPtr,"%lg",ARRAY2D_FORTRAN(channels,i,j,xi->general->n_interactions_trajectory+1,xp->nchannels));
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
			sprintf(filename,"%s_%i.spe",spe_file_conv,i);
			if ((outPtr=fopen(filename,"w")) == NULL ) {
				fprintf(stdout,"Could not write to %s\n",filename);
				exit(1);
			}
			else if (options.verbose)
				g_fprintf(stdout,"Writing to SPE file %s\n",filename);

			fprintf(outPtr,"$SPEC_ID:\n\n");
			fprintf(outPtr,"$DATA:\n");
			fprintf(outPtr,"1\t%i\n",xp->nchannels);
			for (j=0 ; j < xp->nchannels ; j++) {
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



	//csv file unconvoluted
	if (csv_noconvPtr != NULL) {
		for (j=0 ; j < xp->nchannels ; j++) {
			fprintf(csv_noconvPtr,"%i,%lf",j,(j)*xi->detector->gain+xi->detector->zero);	
			for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= xi->general->n_interactions_trajectory ; i++) {
				//channel number, energy, counts...
				fprintf(csv_noconvPtr,",%lf",ARRAY2D_FORTRAN(channels,i,j,xi->general->n_interactions_trajectory+1,xp->nchannels));
			}
			fprintf(csv_noconvPtr,"\n");
		}
		fclose(csv_noconvPtr);
	}

	//csv file convoluted
	if (csv_convPtr != NULL) {
		for (j=0 ; j < xp->nchannels ; j++) {
			fprintf(csv_convPtr,"%i,%lf",j,(j)*xi->detector->gain+xi->detector->zero);	
			for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= xi->general->n_interactions_trajectory ; i++) {
				//channel number, energy, counts...
				fprintf(csv_convPtr,",%lf",channels_conv[i][j]);
			}
			fprintf(csv_convPtr,"\n");
		}
		fclose(csv_convPtr);
	}

#else
	//this piece of code is necessary because of some weird bug I'm getting on Windows. I hope I'll be able to remove it in the future

	if (csv_file_conv != NULL) {
		// 1 = convoluted
		if (xmi_xmso_to_csv_xslt(argv[2], csv_file_conv, 1) == 0) {
			return 1;
		}
		else if (options.verbose)
			g_fprintf(stdout,"Output written to CSV file %s\n",csv_file_conv);
		
	}
	if (csv_file_noconv != NULL) {
		// 0 = unconvoluted
		if (xmi_xmso_to_csv_xslt(argv[2], csv_file_noconv, 0) == 0) {
			return 1;
		}
		else if (options.verbose)
			g_fprintf(stdout,"Output written to CSV file %s\n",csv_file_conv);
		
	}

	if (spe_file_conv != NULL) {
		for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= xi->general->n_interactions_trajectory ; i++) {
			sprintf(filename,"%s_%i.spe",spe_file_conv,i);
			if (xmi_xmso_to_spe_xslt(argv[2], filename, 1, i) == 0) {
				return 1;
			}
			else if (options.verbose)
				g_fprintf(stdout,"Output written to SPE file %s\n", filename);
	
		}
	}

	if (spe_file_noconv != NULL) {
		for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= xi->general->n_interactions_trajectory ; i++) {
			sprintf(filename,"%s_%i.spe",spe_file_noconv,i);
			if (xmi_xmso_to_spe_xslt(argv[2], filename, 0, i) == 0) {
				return 1;
			}
			else if (options.verbose)
				g_fprintf(stdout,"Output written to SPE file %s\n", filename);
	
		}
	}



#endif


	//svg files
	if (svg_file_conv != NULL) {
		// 0 = convoluted
		if (xmi_xmso_to_svg_xslt(argv[2], svg_file_conv, 1) == 0) {
			return 1;
		}
		else if (options.verbose)
			g_fprintf(stdout,"Output written to SVG file %s\n",svg_file_conv);
	}

	if (svg_file_noconv != NULL) {
       		// 1 = unconvoluted
		if (xmi_xmso_to_svg_xslt(argv[2], svg_file_noconv, 0) == 0) {
			return 1;
		}
		else if (options.verbose)
			g_fprintf(stdout,"Output written to SVG file %s\n",svg_file_noconv);
	}

	return 0;
}


