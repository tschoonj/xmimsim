#ifndef XMI_MAIN_H
#define XMI_MAIN_H

#include "xmi_data_structs.h"

extern int use_M_lines;
extern int use_self_enhancement;
extern int use_cascade;
extern int use_variance_reduction;

int xmi_main_msim (xmi_inputFPtr inputFPtr, xmi_hdf5FPtr hdf5FPtr, int n_mpi_hosts, double *channels, int nchannels);




#endif
