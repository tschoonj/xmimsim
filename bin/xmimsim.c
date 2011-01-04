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
#include <glib.h>
#include <locale.h>

#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>





int main (int argc, char *argv[]) {
	
	//openmpi variables
#ifdef HAVE_OPENMPI
	int numprocs, rank, namelen;
	char processor_name[MPI_MAX_PROCESSOR_NAME];
#endif

	//general variables
	struct xmi_input *input;
	int rv;
	xmi_inputFPtr inputFPtr;
	xmi_hdf5FPtr hdf5FPtr;
	const gsl_rng_type *rng_type;
	gsl_rng *rng;
	unsigned long int seed;
	double channels[2048];
	FILE *outPtr;
	int i;
	GError *error = NULL;
	GOptionContext *context;
	static struct xmi_main_options options;
	int use_M_lines;
        int use_self_enhancement;
        int use_cascade;
	int use_variance_reduction;

	static GOptionEntry entries[] = {
	  	{ "enable-M-lines", 0, 0, G_OPTION_ARG_NONE, &(options.use_M_lines), "Enable M lines (default)", NULL },
	  	{ "disable-M-lines", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_M_lines), "Disable M lines", NULL },
	  	{ "enable-self-enhancement", 0, 0, G_OPTION_ARG_NONE, &(options.use_self_enhancement), "Enable self-enhancement", NULL },
	  	{ "disable-self-enhancement", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_self_enhancement), "Disable self-enhancement (default)", NULL },
	  	{ "enable-cascade", 0, 0, G_OPTION_ARG_NONE, &(options.use_cascade), "Enable cascade effects (default)", NULL },
	  	{ "disable-cascade", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_cascade), "Disable cascade effects", NULL },
	  	{ "enable-variance-reduction", 0, 0, G_OPTION_ARG_NONE, &(options.use_variance_reduction), "Enable variance reduction (default)", NULL },
	  	{ "disable-variance-reduction", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &(options.use_variance_reduction), "Disable variance reduction", NULL },
		{ NULL }
	};


	//locale...
	setlocale(LC_ALL,"C");







#ifdef HAVE_OPENMPI
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Get_processor_name(processor_name, &namelen);
#endif
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
	if (xmi_init_from_hdf5("xmimsimdata.h5",inputFPtr,&hdf5FPtr) == 0) {
		fprintf(stderr,"Could not initialize from hdf5 data file\n");
		return 1;
	}	
#if DEBUG == 1
	fprintf(stdout,"Reading from HDF5 file\n");
#endif


	if (xmi_main_msim(inputFPtr, hdf5FPtr, numprocs, channels, 2048,options) == 0) {
		fprintf(stderr,"Error in xmi_main_msim\n");
		return 1;
	}

#if DEBUG == 1
	fprintf(stdout,"After MC simulation\n");
#endif
	



	//free what needs freeing 
	xmi_free_input_F(&inputFPtr);
	xmi_free_hdf5_F(&hdf5FPtr);


	if (xmi_end_random_acquisition() == 0) {
		return 1;
	}





#ifdef HAVE_OPENMPI
	MPI_Finalize();
#endif


	//write it to outputfile...
	if ((outPtr=fopen(input->general->outputfile,"w")) == NULL ) {
		fprintf(stdout,"Could not write to outputfile\n");
		exit(1);
	}
	fprintf(outPtr,"$DATA:\n");
	fprintf(outPtr,"0\t2047\n");
	for (i=0 ; i < 2048 ; i++) {
		fprintf(outPtr,"%lg",channels[i]);
		if (i % 8 == 0) {
			fprintf(outPtr,"\n");
		}
		else {
			fprintf(outPtr,"     ");
		}
	}
	fclose(outPtr);


	return 0;
}


