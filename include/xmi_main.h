#ifndef XMI_MAIN_H
#define XMI_MAIN_H

#include "xmi_data_structs.h"



struct xmi_main_options {
	int use_M_lines;
	int use_self_enhancement;
	int use_cascade;
	int use_variance_reduction;
};



int xmi_main_msim (xmi_inputFPtr inputFPtr, xmi_hdf5FPtr hdf5FPtr, int n_mpi_hosts, double **channels, int nchannels, struct xmi_main_options, int **history);

void xmi_detector_convolute(xmi_inputFPtr inputFPtr, xmi_hdf5FPtr hdf5FPtr, double *channels_noconv, double **channels_conv, int nchannels);


#endif
