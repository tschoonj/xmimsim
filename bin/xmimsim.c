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

#ifdef HAVE_OPENMPI
	#include <mpi.h>
#endif

//#include <omp.h>

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
	static char *xmimsim_hdf5_solid_angles = NULL;
	struct xmi_input *input;
	int rv;
	xmi_inputFPtr inputFPtr;
	xmi_hdf5FPtr hdf5FPtr;
	unsigned long int seed;
	double *channels, *channelsdef;
	double **channels_conv;
	double *channels_conv_temp;
	FILE *outPtr, *csv_convPtr, *csv_noconvPtr;
	char filename[512];
	int i,j;
	GError *error = NULL;
	GOptionContext *context;
	static struct xmi_main_options options;
	int use_M_lines;
	int use_self_enhancement;
	int use_cascade;
	int use_variance_reduction;
	double *brute_history;
	double *brute_historydef;
	double *var_red_history;
	double *var_red_historydef;
	static gchar *hdf5_file=NULL;
	static gchar *spe_file_noconv=NULL;
	static gchar *spe_file_conv=NULL;
	static gchar *csv_file_noconv=NULL;
	static gchar *csv_file_conv=NULL;
	static gchar *svg_file_noconv=NULL;
	static gchar *svg_file_conv=NULL;
	static gchar *htm_file_noconv=NULL;
	static gchar *htm_file_conv=NULL;
	static int nchannels=2048;
	static int omp_num_threads;
	double zero_sum;
	struct xmi_solid_angle *solid_angle_def=NULL;
	struct xmi_escape_ratios *escape_ratios_def=NULL;
	char *xmi_input_string;
	static char *xmimsim_hdf5_escape_ratios = NULL;

	omp_num_threads = omp_get_max_threads();

	static GOptionEntry entries[] = {
		{ "enable-M-lines", 0, 0, G_OPTION_ARG_NONE, &(options.use_M_lines), "Enable M lines (default)", NULL },
		{ "disable-M-lines", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_M_lines), "Disable M lines", NULL },
		{ "enable-lorentzian-broadening", 0, G_OPTION_FLAG_HIDDEN, G_OPTION_ARG_NONE, &(options.use_self_enhancement), "Enable Lorentzian line broadening", NULL },
		{ "disable-lorentzian-broadening", 0, G_OPTION_FLAG_REVERSE | G_OPTION_FLAG_HIDDEN, G_OPTION_ARG_NONE, &(options.use_self_enhancement), "Disable Lorentzian line broadening (default)", NULL },
		{ "enable-auger-cascade", 0, 0, G_OPTION_ARG_NONE, &(options.use_cascade_auger), "Enable Auger (non radiative) cascade effects (default)", NULL },
		{ "disable-auger-cascade", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_cascade_auger), "Disable Auger cascade effects", NULL },
		{ "enable-radiative-cascade", 0, 0, G_OPTION_ARG_NONE, &(options.use_cascade_radiative), "Enable radiative cascade effects (default)", NULL },
		{ "disable-radiative-cascade", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_cascade_radiative), "Disable radiative cascade effects", NULL },
		{ "enable-variance-reduction", 0, 0, G_OPTION_ARG_NONE, &(options.use_variance_reduction), "Enable variance reduction (default)", NULL },
		{ "disable-variance-reduction", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_variance_reduction), "Disable variance reduction", NULL },
		{"with-hdf5-data",0,G_OPTION_FLAG_HIDDEN,G_OPTION_ARG_FILENAME,&hdf5_file,"Select a HDF5 data file (advanced usage)",NULL},
		{"with-solid-angles-data",0,G_OPTION_FLAG_HIDDEN,G_OPTION_ARG_FILENAME,&xmimsim_hdf5_solid_angles,"Select a HDF5 solid angles file (advanced usage)",NULL},
		{"with-escape-ratios-data",0,G_OPTION_FLAG_HIDDEN,G_OPTION_ARG_FILENAME,&xmimsim_hdf5_escape_ratios,"Select a HDF5 escape ratios file (advanced usage)",NULL},
		{"spe-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&spe_file_noconv,"Write detector unconvoluted spectra to file",NULL},
		{"spe-file",0,0,G_OPTION_ARG_FILENAME,&spe_file_conv,"Write detector convoluted spectra to file",NULL},
		{"csv-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&csv_file_noconv,"Write detector unconvoluted spectra to CSV file",NULL},
		{"csv-file",0,0,G_OPTION_ARG_FILENAME,&csv_file_conv,"Write detector convoluted spectra to CSV file",NULL},
		{"svg-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&svg_file_noconv,"Write detector unconvoluted spectra to SVG file",NULL},
		{"svg-file",0,0,G_OPTION_ARG_FILENAME,&svg_file_conv,"Write detector convoluted spectra to SVG file",NULL},
		{"htm-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&htm_file_noconv,"Write detector unconvoluted spectra to HTML file",NULL},
		{"htm-file",0,0,G_OPTION_ARG_FILENAME,&htm_file_conv,"Write detector convoluted spectra to HTML file",NULL},
		{"set-channels",0,0,G_OPTION_ARG_INT,&nchannels,"Change number of channels (default=2048)",NULL},
		{ "enable-optimizations", 0, 0, G_OPTION_ARG_NONE, &(options.use_optimizations), "Enable optimizations (default)", NULL },
		{ "disable-optimizations", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_optimizations), "Disable optimizations", NULL },
		{ "enable-pile-up", 0, 0, G_OPTION_ARG_NONE, &(options.use_sum_peaks), "Enable pile-up", NULL },
		{ "disable-pile-up", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_sum_peaks), "Disable pile-up (default)", NULL },
		{ "enable-poisson", 0, 0, G_OPTION_ARG_NONE, &(options.use_poisson), "Generate Poisson noise in the spectra", NULL },
		{ "disable-poisson", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_poisson), "Disable the generating of spectral Poisson noise (default)", NULL },
		{"set-threads",0,0,G_OPTION_ARG_INT,&omp_num_threads,"Set the number of threads (default=max)",NULL},
		{ "verbose", 'v', 0, G_OPTION_ARG_NONE, &(options.verbose), "Verbose mode", NULL },
		{ NULL }
	};


#ifdef _WIN32
#define TOTALBYTES    8192
#define BYTEINCREMENT 4096
	LONG RegRV;
	DWORD QueryRV;
	LPTSTR subKey;
	HKEY key;
    	DWORD BufferSize = TOTALBYTES;
        DWORD cbdata;
	PPERF_DATA_BLOCK PerfData;

#endif









#ifdef HAVE_OPENMPI
	MPI_Init(&argc, &argv);
#endif


	setbuf(stdout,NULL);



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


	//
	//options...
	//1) use M-lines
	//2) use self-enhancement -> see paper of Fernandez/Scot
	//3) use cascade effect
	//4) use variance reduction

	options.use_M_lines = 1;
	options.use_self_enhancement = 0;
	options.use_cascade_auger = 1;
	options.use_cascade_radiative = 1;
	options.use_variance_reduction = 1;
	options.use_optimizations = 1;
	options.use_sum_peaks = 0;
	options.use_poisson = 0;
	options.verbose = 0;



	//parse options
	context = g_option_context_new ("inputfile");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmimsim: a program for the Monte-Carlo simulation of X-ray fluorescence spectra");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		return 1;
	}

	if (argc != 2) {
		fprintf(stderr,"%s\n",g_option_context_get_help(context, TRUE, NULL));
		return 1;
	}

	if (omp_num_threads > omp_get_max_threads() ||
			omp_num_threads < 1) {
		omp_num_threads = omp_get_max_threads();
	}

	omp_set_num_threads(omp_num_threads);


	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		return 1;
	}
	else if (options.verbose)
		g_fprintf(stdout,"XML catalog loaded\n");


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


	//start random number acquisition
	if (xmi_start_random_acquisition() == 0) {
		return 1;
	}



	//read in the inputfile
	rv = xmi_read_input_xml(argv[1],&input);

	if (rv != 1) {
		return 1;
	}
	else if (options.verbose)
		g_fprintf(stdout,"Inputfile %s successfully parsed\n",XMI_ARGV_ORIG[XMI_ARGC_ORIG-1]);

	//copy to the corresponding fortran variable
	xmi_input_C2F(input,&inputFPtr);

	//initialization
	if (xmi_init_input(&inputFPtr) == 0) {
		return 1;
	}


	if (options.verbose)
		g_fprintf(stdout,"Reading HDF5 datafile\n");
	
	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5(hdf5_file,inputFPtr,&hdf5FPtr) == 0) {
		g_fprintf(stderr,"Could not initialize from hdf5 data file\n");
		return 1;
	}	
	else if (options.verbose)
		g_fprintf(stdout,"HDF5 datafile %s successfully processed\n",hdf5_file);

	xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);



	//check solid_angles
#ifdef HAVE_OPENMPI
	if (rank == 0) {
#endif
	if (options.use_variance_reduction == 1) {
		if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0)
			return 1;

		//check if solid angles are already precalculated
		if (options.verbose)
			g_fprintf(stdout,"Querying %s for solid angle grid\n",xmimsim_hdf5_solid_angles);
		if (xmi_find_solid_angle_match(xmimsim_hdf5_solid_angles , input, &solid_angle_def) == 0)
			return 1;
		if (solid_angle_def == NULL) {
			if (options.verbose)
				g_fprintf(stdout,"Precalculating solid angle grid\n");
			//doesn't exist yet
			//convert input to string
			if (xmi_write_input_xml_to_string(&xmi_input_string,input) == 0) {
				return 1;
			}
			xmi_solid_angle_calculation(inputFPtr, &solid_angle_def, xmi_input_string,options);
			//update hdf5 file
			if( xmi_update_solid_angle_hdf5_file(xmimsim_hdf5_solid_angles , solid_angle_def) == 0)
				return 1;
			else if (options.verbose)
				g_fprintf(stdout,"%s was successfully updated with new solid angle grid\n",xmimsim_hdf5_solid_angles);
		}
		else if (options.verbose)
			g_fprintf(stdout,"Solid angle grid already present in %s\n",xmimsim_hdf5_solid_angles);

	}
	else if (options.verbose)
		g_fprintf(stdout,"Operating in brute-force mode: solid angle grid is redundant\n");


#ifdef HAVE_OPENMPI
	}
#endif

#ifdef HAVE_OPENMPI
	MPI_Barrier(MPI_COMM_WORLD);
	//read solid angles for the other nodes
	if (options.use_variance_reduction == 1 && rank != 0) {
		if (xmi_find_solid_angle_match(xmimsim_hdf5_solid_angles, input, &solid_angle_def) == 0)
			return 1;
		if (solid_angle_def == NULL) {
			g_fprintf(stdout,"Could not find solid angle in HDF5 file (but it should be there since it was created)\n");
			return 1;
		}	
	}
	MPI_Barrier(MPI_COMM_WORLD);
#endif

	if (options.verbose)
		g_fprintf(stdout,"Simulating interactions\n");

	if (xmi_main_msim(inputFPtr, hdf5FPtr, numprocs, &channels, nchannels,options, &brute_history, &var_red_history, solid_angle_def) == 0) {
		g_fprintf(stderr,"Error in xmi_main_msim\n");
		return 1;
	}

	
	if (options.verbose)
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
		channelsdef = (double *) calloc((input->general->n_interactions_trajectory+1)*nchannels,sizeof(double));
		brute_historydef = (double *) calloc(100*(383+2)*input->general->n_interactions_trajectory,sizeof(double));	
		if (options.use_variance_reduction == 1)
			var_red_historydef = (double *) calloc(100*(383+2)*input->general->n_interactions_trajectory,sizeof(double));	
	}
	MPI_Barrier(MPI_COMM_WORLD);

	//reduce channels
	MPI_Reduce(channels, channelsdef,(1+input->general->n_interactions_trajectory)*nchannels, MPI_DOUBLE,MPI_SUM, 0, MPI_COMM_WORLD);
	//reduce brute_history
	MPI_Reduce(brute_history, brute_historydef,100*(383+2)*input->general->n_interactions_trajectory, MPI_DOUBLE,MPI_SUM, 0, MPI_COMM_WORLD);
	//reduce var_red_history
	if (options.use_variance_reduction == 1)
		MPI_Reduce(var_red_history, var_red_historydef,100*(383+2)*input->general->n_interactions_trajectory, MPI_DOUBLE,MPI_SUM, 0, MPI_COMM_WORLD);

	
	MPI_Finalize();


#else
	channelsdef = channels;
	brute_historydef = brute_history;
	var_red_historydef = var_red_history;
#endif


#define ARRAY3D_FORTRAN(array,i,j,k,Ni,Nj,Nk) (array[Nj*Nk*(i-1)+Nk*(j-1)+(k-1)])
//watch out, I'm doing something naughty here :-)
#define ARRAY2D_FORTRAN(array,i,j,Ni,Nj) (array[Nj*(i)+(j-1)])




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
		zero_sum = xmi_sum_double(channelsdef, nchannels);

#if DEBUG == 2
		fprintf(stdout,"zero_sum: %lf\n",zero_sum);
#endif




		//convolute spectrum
		channels_conv = (double **) malloc(sizeof(double *)*(input->general->n_interactions_trajectory+1));
#if DEBUG == 2
		for (i=(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) 
			fprintf(stdout,"channel 223 contents unspoiled: %lf\n",channelsdef[i*nchannels+222]);

#endif

		//read escape ratios
		if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0)
			return 1;

		if (options.verbose)
			g_fprintf(stdout,"Querying %s for escape peak ratios\n",xmimsim_hdf5_escape_ratios);

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
		else if (options.verbose)
			g_fprintf(stdout,"Escape peak ratios already present in %s\n",xmimsim_hdf5_escape_ratios);


		
		for (i=(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
#if DEBUG == 2
			fprintf(stdout,"channel 223 contents: %lf\n",channelsdef[i*nchannels+222]);
#endif
			xmi_detector_convolute(inputFPtr, channelsdef+i*nchannels, &channels_conv_temp, nchannels,options,escape_ratios_def);
			//channels_conv[i] = xmi_memdup(channels_conv_temp,sizeof(double)*nchannels);
			channels_conv[i] = channels_conv_temp;
#if DEBUG == 2
			fprintf(stdout,"channel 223 contents after conv: %lf\n",channels_conv[i][222]);
			fprintf(stdout,"channel 223 contents modified?: %lf\n",channelsdef[i*nchannels+222]);
#endif
		}
#if DEBUG == 2
		fprintf(stdout,"After detector convolution\n");
#endif

#ifndef G_OS_WIN32

		csv_convPtr = csv_noconvPtr = NULL;

		if (csv_file_noconv != NULL) {
			if ((csv_noconvPtr = fopen(csv_file_noconv,"w")) == NULL) {
				g_fprintf(stderr,"Could not write to %s\n",csv_file_noconv);
				return 1;
			}
			else if (options.verbose)
				g_fprintf(stdout,"Writing to CSV file %s\n",csv_file_noconv);
		}
		if (csv_file_conv != NULL) {
			if ((csv_convPtr = fopen(csv_file_conv,"w")) == NULL) {
				g_fprintf(stderr,"Could not write to %s\n",csv_file_conv);
				return 1;
			}
			else if (options.verbose)
				g_fprintf(stdout,"Writing to CSV file %s\n",csv_file_conv);
		}


		for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {

			//write it to outputfile... spe style
			if (spe_file_noconv != NULL) {
				sprintf(filename,"%s_%i.spe",spe_file_noconv,i);
				if ((outPtr=fopen(filename,"w")) == NULL ) {
					g_fprintf(stderr,"Could not write to %s\n",filename);
					exit(1);
				}
				else if (options.verbose)
					g_fprintf(stdout,"Writing to SPE file %s\n",filename);
				fprintf(outPtr,"$SPEC_ID:\n\n");
				fprintf(outPtr,"$DATA:\n");
				fprintf(outPtr,"1\t%i\n",nchannels);
				for (j=1 ; j <= nchannels ; j++) {
					fprintf(outPtr,"%lg",ARRAY2D_FORTRAN(channelsdef,i,j,input->general->n_interactions_trajectory+1,nchannels));
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


		//csv file unconvoluted
		if (csv_noconvPtr != NULL) {
			for (j=1 ; j <= nchannels ; j++) {
				fprintf(csv_noconvPtr,"%i,%lf",j,(j)*input->detector->gain+input->detector->zero);	
				for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
					//channel number, energy, counts...
					fprintf(csv_noconvPtr,",%lf",ARRAY2D_FORTRAN(channelsdef,i,j,input->general->n_interactions_trajectory+1,nchannels));
				}
				fprintf(csv_noconvPtr,"\n");
			}
			fclose(csv_noconvPtr);
		}

		//csv file convoluted
		if (csv_convPtr != NULL) {
			for (j=0 ; j < nchannels ; j++) {
				fprintf(csv_convPtr,"%i,%lf",j+1,(j+1)*input->detector->gain+input->detector->zero);	
				for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
					//channel number, energy, counts...
					fprintf(csv_convPtr,",%lf",channels_conv[i][j]);
				}
				fprintf(csv_convPtr,"\n");
			}
			fclose(csv_convPtr);
		}


#endif



#if DEBUG == 1
		fprintf(stdout,"Writing outputfile\n");
#endif

		//write to xml outputfile
		if (xmi_write_output_xml(input->general->outputfile, input, brute_history, options.use_variance_reduction == 1 ? var_red_history : NULL, channels_conv, channelsdef, nchannels, argv[1], zero_sum > 0.0 ? 1 : 0) == 0) {
			return 1;
		}
		else if (options.verbose)
			g_fprintf(stdout,"Output written to XMSO file %s\n",input->general->outputfile);

	
#ifdef G_OS_WIN32
	//this piece of code is necessary because of some weird bug I'm getting on Windows. I hope I'll be able to remove it in the future

		if (csv_file_conv != NULL) {
			// 1 = convoluted
			if (xmi_xmso_to_csv_xslt(input->general->outputfile, csv_file_conv, 1) == 0) {
				return 1;
			}
			else if (options.verbose)
				g_fprintf(stdout,"Output written to CSV file %s\n",csv_file_conv);
			
		}
		if (csv_file_noconv != NULL) {
			// 0 = unconvoluted
			if (xmi_xmso_to_csv_xslt(input->general->outputfile, csv_file_noconv, 0) == 0) {
				return 1;
			}
			else if (options.verbose)
				g_fprintf(stdout,"Output written to CSV file %s\n",csv_file_conv);
			
		}

		if (spe_file_conv != NULL) {
			for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
				sprintf(filename,"%s_%i.spe",spe_file_conv,i);
				if (xmi_xmso_to_spe_xslt(input->general->outputfile, filename, 1, i) == 0) {
					return 1;
				}
				else if (options.verbose)
					g_fprintf(stdout,"Output written to SPE file %s\n", filename);
		
			}
		}

		if (spe_file_noconv != NULL) {
			for (i =(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
				sprintf(filename,"%s_%i.spe",spe_file_noconv,i);
				if (xmi_xmso_to_spe_xslt(input->general->outputfile, filename, 0, i) == 0) {
					return 1;
				}
				else if (options.verbose)
					g_fprintf(stdout,"Output written to SPE file %s\n", filename);
		
			}
		}


#endif
		if (svg_file_conv != NULL) {
			// 1 = convoluted
			if (xmi_xmso_to_svg_xslt(input->general->outputfile, svg_file_conv, 1) == 0) {
				return 1;
			}
			else if (options.verbose)
				g_fprintf(stdout,"Output written to SVG file %s\n",svg_file_conv);
		}

		if (svg_file_noconv != NULL) {
                        // 0 = unconvoluted
			if (xmi_xmso_to_svg_xslt(input->general->outputfile, svg_file_noconv, 0) == 0) {
				return 1;
			}
			else if (options.verbose)
				g_fprintf(stdout,"Output written to SVG file %s\n",svg_file_noconv);
		}


		if (htm_file_conv != NULL) {
			// 1 = convoluted
			if (xmi_xmso_to_htm_xslt(input->general->outputfile, htm_file_conv, 1) == 0) {
				return 1;
			}
			else if (options.verbose)
				g_fprintf(stdout,"Output written to HTML file %s\n",htm_file_conv);
		}

		if (htm_file_noconv != NULL) {
                        // 0 = unconvoluted
			if (xmi_xmso_to_htm_xslt(input->general->outputfile, htm_file_noconv, 0) == 0) {
				return 1;
			}
			else if (options.verbose)
				g_fprintf(stdout,"Output written to HTML file %s\n",htm_file_noconv);
		}


		if (xmi_end_random_acquisition() == 0) {
			return 1;
		}

		for (i=(zero_sum > 0.0 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
			xmi_deallocate(channels_conv[i] );
		}
		free(channels_conv);
		xmi_deallocate(channelsdef);
		xmi_deallocate(brute_history);
		if (options.use_variance_reduction)
			xmi_deallocate(var_red_history);

#ifdef HAVE_OPENMPI
	}	
#endif

	return 0;
}


