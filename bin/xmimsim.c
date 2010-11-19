#include <config.h>

#ifdef HAVE_OPENMPI
	#include <mpi.h>
#endif

#include <omp.h>

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

	//copy to the corresponding fortran variable
	xmi_input_C2F(input,&inputFPtr);

	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5("xmimsimdata.h5",inputFPtr,&hdf5FPtr) == 0) {
		fprintf(stdout,"Could not initialize from hdf5 data file\n");
		return 1;
	}	


	
	//set random number generator to Mersenne Twister
	rng_type = gsl_rng_mt19937;




#pragma omp parallel default(shared) private(rng,seed)
{

	rng = gsl_rng_alloc(rng_type);
	if (xmi_get_random_numbers(&seed,1) == 0) {
		exit(1);
	}

	//set seed
	gsl_rng_set(rng,seed);


	//fortran code must take as arguments number of hosts (MPI) and number of cpus per host (openMP)
	//ideally the code first checks the total number of cpus available
	//
	//think its maybe best to run the openMP code completely in Fortran
	//requires bindings for gsl_rng_alloc and xmi_get_random_numbers -> watch out with unsigned long int !!!!

}



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


