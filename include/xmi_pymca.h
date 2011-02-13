#ifndef XMI_PYMCA_H
#define XMI_PYMCA_H



#include "xmi_data_structs.h"



#define XMI_PYMCA_START_CONC 0.0001
#define XMI_PYMCA_MAX_ITERATIONS 100
#define XMI_PYMCA_CONV_THRESHOLD 0.001


struct xmi_pymca {
	int *z_arr;
	int n_peaks;
	//KL2 and KL3 + escape peaks
	double *k_alpha;
	//L3M5 and L3M4 + escape peaks
	double *l_alpha;
	//layer that will be quantified
	int ilay_pymca;
	//elements that will be quantified... trace elements
	int *z_arr_quant;
	int n_z_arr_quant;
	//matrix flags
	int flags[100];
	//nchannels
	int nchannels;
};



//return 1 on success, 0 otherwise

//allocation of input occurs in function!
int xmi_read_input_pymca(char *pymca_file, struct xmi_input **input, struct xmi_pymca **);

struct xmi_layer xmi_ilay_composition_pymca(struct xmi_layer *matrix, struct xmi_pymca *pymca_aux , double *weights_arr_quant);


#endif
