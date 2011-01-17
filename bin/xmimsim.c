#include <config.h>

#ifdef HAVE_OPENMPI
	#include <mpi.h>
#endif

#include <omp.h>

#include "xmi_main.h"
#include "xmi_data_structs.h"
#include "xmi_xml.h"
#include "xmi_aux.h"
#include "xmi_random.h"
#include <unistd.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <locale.h>
#include <xraylib.h>

#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <string.h>





int main (int argc, char *argv[]) {
	
	//openmpi variables
#ifdef HAVE_OPENMPI
	int numprocs, rank, namelen;
	char processor_name[MPI_MAX_PROCESSOR_NAME];
//	int numreqs=0;
//	MPI_Request *reqs = 0;
#endif

	//general variables
	struct xmi_input *input;
	int rv;
	xmi_inputFPtr inputFPtr;
	xmi_hdf5FPtr hdf5FPtr;
	const gsl_rng_type *rng_type;
	gsl_rng *rng;
	unsigned long int seed;
	double *channels, *channelsdef, *results;
	double *channels_conv;
	FILE *outPtr;
	int i,j;
	GError *error = NULL;
	GOptionContext *context;
	static struct xmi_main_options options;
	int use_M_lines;
	int use_self_enhancement;
	int use_cascade;
	int use_variance_reduction;
	int *history;
	int *historydef;
	static gchar *hdf5_file=NULL;
	static gchar *spe_file_noconv=NULL;
	static gchar *spe_file_conv=NULL;
	static int nchannels=2048;

	static GOptionEntry entries[] = {
		{ "enable-M-lines", 0, 0, G_OPTION_ARG_NONE, &(options.use_M_lines), "Enable M lines (default)", NULL },
		{ "disable-M-lines", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_M_lines), "Disable M lines", NULL },
		{ "enable-lorentzian-broadening", 0, 0, G_OPTION_ARG_NONE, &(options.use_self_enhancement), "Enable Lorentzian line broadening", NULL },
		{ "disable-lorentzian-broadening", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_self_enhancement), "Disable Lorentzian line broadening (default)", NULL },
		{ "enable-cascade", 0, 0, G_OPTION_ARG_NONE, &(options.use_cascade), "Enable cascade effects (default)", NULL },
		{ "disable-cascade", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_cascade), "Disable cascade effects", NULL },
		{ "enable-variance-reduction", 0, 0, G_OPTION_ARG_NONE, &(options.use_variance_reduction), "Enable variance reduction (default)", NULL },
		{ "disable-variance-reduction", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_variance_reduction), "Disable variance reduction", NULL },
		{"with-hdf5-data",0,0,G_OPTION_ARG_FILENAME,&hdf5_file,"Select a HDF5 data file (advanced usage)",NULL},
		{"spe-file-unconvoluted",0,0,G_OPTION_ARG_FILENAME,&spe_file_noconv,"Write detector unconvoluted spectrum to file",NULL},
		{"spe-file-convoluted",0,0,G_OPTION_ARG_FILENAME,&spe_file_conv,"Write detector convoluted spectrum to file",NULL},
		{"set-channels",0,0,G_OPTION_ARG_INT,&nchannels,"Change number of channels (default=2048)",NULL},
		{ NULL }
	};


	//locale...
	setlocale(LC_ALL,"C");







#ifdef HAVE_OPENMPI
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Get_processor_name(processor_name, &namelen);


	/*	if (rank == 0) {
		reqs = (MPI_Request *) malloc(2*numprocs*sizeof(MPI_Request));
		results = (double *) malloc(sizeof(numprocs)*2048*sizeof(double));
		for (i=0 ; i < numprocs ; i++) {
		MPI_Irecv(results+(i*2048), 2048, MPI_DOUBLE, MPI_ANY_SOURCE, i, MPI_COMM_WORLD, reqs+numreqs++);
		}
		}
		else {
		reqs = (MPI_Request *) malloc(numprocs*sizeof(MPI_Request));
		}
		*/


#endif
	channels = (double *) malloc(nchannels*sizeof(double));

	//
	//options...
	//1) use M-lines
	//2) use self-enhancement -> see paper of Fernandez/Scot
	//3) use cascade effect
	//4) use variance reduction

	options.use_M_lines = 1;
	options.use_self_enhancement = 0;
	options.use_cascade = 1;
	options.use_variance_reduction = 1;


	//parse options
	context = g_option_context_new ("inputfile");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmimsim: a program for the Monte-Carlo simulation of X-ray fluorescence spectra");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		exit (1);
	}


	if (hdf5_file == NULL) {
		//no option detected
		//first look at default file
		if (g_access(XMIMSIM_HDF5_DEFAULT, F_OK | R_OK) == 0)
			hdf5_file = strdup(XMIMSIM_HDF5_DEFAULT);
		else if (g_access("xmimsimdata.h5", F_OK | R_OK) == 0)
			hdf5_file = strdup("xmimsimdata.h5");
		else {
			//if not found abort...	
			g_printf("Could not detect the HDF5 data file\nCheck the xmimsim installation or\nuse the --with-hdf5-data option to manually pick the file\n");
			exit(1);
		}
	}

#if DEBUG == 1
	fprintf(stdout,"use_M_lines: %i\n",options.use_M_lines);
	fprintf(stdout,"use_self_enhancement: %i\n",options.use_self_enhancement);
	fprintf(stdout,"use_cascade: %i\n",options.use_cascade);
	fprintf(stdout,"use_variance_reduction: %i\n",options.use_variance_reduction);
#endif


	//start random number acquisition
	if (xmi_start_random_acquisition() == 0) {
		return 1;
	}



	//read in the inputfile
	rv = xmi_read_input_xml(argv[1],&input);
#if DEBUG == 1
	fprintf(stdout,"Finished reading the inputfile\n");
#endif

	if (rv != 1) {
		return 1;
	}

	//copy to the corresponding fortran variable
	xmi_input_C2F(input,&inputFPtr);

#if DEBUG == 1
	fprintf(stdout,"Copied to Fortran variable\n");
#endif
	//initialization
	if (xmi_init_input(&inputFPtr) == 0) {
		return 1;
	}


#if DEBUG == 1
	fprintf(stdout,"xmi_init_input called\n");
#endif
	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5(hdf5_file,inputFPtr,&hdf5FPtr) == 0) {
		fprintf(stderr,"Could not initialize from hdf5 data file\n");
		return 1;
	}	
#if DEBUG == 1
	fprintf(stdout,"Reading from HDF5 file\n");
#endif


	if (xmi_main_msim(inputFPtr, hdf5FPtr, numprocs, channels, nchannels,options, &history) == 0) {
		fprintf(stderr,"Error in xmi_main_msim\n");
		return 1;
	}

#if DEBUG == 1
	fprintf(stdout,"After MC simulation\n");
#endif




	//free what needs freeing 
#ifdef HAVE_OPENMPI
	if (rank != 0) {
#endif
		xmi_free_input_F(&inputFPtr);
		xmi_free_hdf5_F(&hdf5FPtr);
#ifdef HAVE_OPENMPI
	}
#endif

	if (xmi_end_random_acquisition() == 0) {
		return 1;
	}





#ifdef HAVE_OPENMPI
	/*	MPI_Isend(channels, 2048, MPI_DOUBLE, 0, rank, MPI_COMM_WORLD, reqs+numreqs);
		MPI_Waitall(numreqs, reqs, MPI_STATUSES_IGNORE);
		*/



	if (rank == 0) {
		channelsdef = (double *) calloc(nchannels,sizeof(double));
		historydef = (int *) calloc(100*(383+2)*input->general->n_interactions_trajectory,sizeof(int));	
	}

	//reduce channels
	MPI_Reduce(channels, channelsdef,nchannels, MPI_DOUBLE,MPI_SUM, 0, MPI_COMM_WORLD);
	//reduce history
	MPI_Reduce(history, historydef,100*(383+2)*input->general->n_interactions_trajectory, MPI_INT,MPI_SUM, 0, MPI_COMM_WORLD);

	
	MPI_Finalize();


#else
	channelsdef = channels;
	historydef = history;
#endif


#define ARRAY3D_FORTRAN(array,i,j,k,Ni,Nj,Nk) (array[Nj*Nk*(i-1)+Nk*(j-1)+(k-1)])




#ifdef HAVE_OPENMPI
	if (rank == 0) {
		
#if DEBUG == 1
		fprintf(stdout,"Ba-KL2: %i\n",ARRAY3D_FORTRAN(historydef,56,abs(KL2_LINE),1,100,385,1));	
//		fprintf(stdout,"Ni-KL3: %i\n",ARRAY3D_FORTRAN(historydef,28,abs(KL3_LINE),1,100,385,2));	
//		fprintf(stdout,"Fe-KL3: %i\n",ARRAY3D_FORTRAN(historydef,26,abs(KL3_LINE),1,100,385,2));	
//		fprintf(stdout,"Ni-KL3: %i\n",ARRAY3D_FORTRAN(historydef,28,abs(KL3_LINE),2,100,385,2));	
//		fprintf(stdout,"Fe-KL3: %i\n",ARRAY3D_FORTRAN(historydef,26,abs(KL3_LINE),2,100,385,2));	
#endif




#endif
		//convolute spectrum
		xmi_detector_convolute(inputFPtr, hdf5FPtr, channelsdef, &channels_conv, nchannels);
#if DEBUG == 1
		fprintf(stdout,"After detector convolution\n");
#endif

		//write it to outputfile...
		if (spe_file_noconv != NULL) {
			if ((outPtr=fopen(spe_file_noconv,"w")) == NULL ) {
				fprintf(stdout,"Could not write to %s\n",spe_file_noconv);
				exit(1);
			}
			fprintf(outPtr,"$DATA:\n");
			fprintf(outPtr,"0\t%i\n",nchannels-1);
			for (i=0 ; i < nchannels ; i++) {
				fprintf(outPtr,"%lg",channelsdef[i]);
				if ((i+1) % 8 == 0) {
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
			if ((outPtr=fopen(spe_file_conv,"w")) == NULL ) {
				fprintf(stdout,"Could not write to %s\n",spe_file_conv);
				exit(1);
			}
			fprintf(outPtr,"$DATA:\n");
			fprintf(outPtr,"0\t%i\n",nchannels-1);
			for (i=0 ; i < nchannels ; i++) {
				fprintf(outPtr,"%lg",channels_conv[i]);
				if ((i+1) % 8 == 0) {
					fprintf(outPtr,"\n");
				}
				else {
					fprintf(outPtr,"     ");
				}
			}
			fclose(outPtr);
		}

#if DEBUG == 1
		fprintf(stdout,"Writing outputfile\n");
#endif

		//write to xml outputfile
		if (xmi_write_output_xml(input->general->outputfile, input, history, channels_conv, channelsdef, nchannels, argv[1] ) == 0) {
			return 1;
		}




#ifdef HAVE_OPENMPI
	}	
#endif

	return 0;
}


