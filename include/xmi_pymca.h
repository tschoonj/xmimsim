/*
Copyright (C) 2010-2019 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_PYMCA_H
#define XMI_PYMCA_H


#include "xmi_data_structs.h"

#ifdef __cplusplus
extern "C" {
#endif

#define XMI_PYMCA_START_CONC 0.0001
#define XMI_PYMCA_MAX_ITERATIONS 100
#define XMI_PYMCA_CONV_THRESHOLD 0.001

typedef struct _xmi_pymca xmi_pymca;
struct _xmi_pymca {
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
	//scatter intensity
	//to be used for adjusting the beam intensity
	double scatter_energy;
	double scatter_intensity;
	int *z_arr_pymca_conc;
	int n_z_arr_pymca_conc;
	double *weight_arr_pymca_conc;
	int xmin;
	int xmax;
	double sum_xmin_xmax;
	int usematrix;
	//0 if Auto, else Z
	int reference;
	int n_ignore_elements;
	int *ignore_elements;
};



//return 1 on success, 0 otherwise

//allocation of input occurs in function!
int xmi_read_input_pymca(char *pymca_file, xmi_input **input, xmi_pymca **, int use_matrix_override, int use_roi_normalization, int use_single_run);

xmi_layer xmi_ilay_composition_pymca(xmi_layer *matrix, xmi_pymca *pymca_aux , double *weights_arr_quant);

#ifdef __cplusplus
}
#endif

#endif
