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


#ifdef HAVE_OPENMPI
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Get_processor_name(processor_name, &namelen);
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



	if (xmi_main_msim(inputFPtr, hdf5FPtr, numprocs, channels, 2048) == 0) {
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


	return 0;
}


