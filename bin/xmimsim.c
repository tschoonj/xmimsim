#include <config.h>

#ifdef HAVE_OPENMPI
	#include <mpi.h>
#endif


#include "xmi_data_structs.h"
#include "xmi_xml.h"
#include "xmi_aux.h"

int main (int argc, char *argv[]) {
	
	//openmpi variables
#ifdef HAVE_OPENMPI
	int numprocs, rank, namelen;
	char processor_name[MPI_MAX_PROCESSOR_NAME];
#endif

	//general variables
	struct xmi_input *input;
	int rv;
	void *inputF;
	


#ifdef HAVE_OPENMPI
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Get_processor_name(processor_name, &namelen);
#endif

	//read in the inputfile
	rv = xmi_read_input_xml(argv[1],&input);

	//copy to the corresponding fortran variable
	xmi_input_C2F(input,&inputF);


#ifdef HAVE_OPENMPI
	MPI_Finalize();
#endif


	return 0;
}


