#include <config.h>

#ifdef HAVE_OPENMPI
	#include <mpi.h>
#endif


#include "xmi_data_structs.h"
#include "xmi_xml.h"
#include "xmi_aux.h"
#include <stdio.h>

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
	xmi_hdf5FPtr hdf5Ptr;
	


#ifdef HAVE_OPENMPI
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Get_processor_name(processor_name, &namelen);
#endif

	//read in the inputfile
	rv = xmi_read_input_xml(argv[1],&input);

	//copy to the corresponding fortran variable
	xmi_input_C2F(input,&inputFPtr);

	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5("xmimsimdata.h5",inputFPtr,&hdf5Ptr) == 0) {
		fprintf(stdout,"Could not initialize from hdf5 data file\n");
		return 1;
	}	


	xmi_free_input_F(&inputFPtr);
	

#ifdef HAVE_OPENMPI
	MPI_Finalize();
#endif


	return 0;
}


