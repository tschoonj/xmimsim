/*
Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_DATA_STRUCTS_H
#define XMI_DATA_STRUCTS_H

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif


struct xmi_general {
	float version;
	char *outputfile;
	long int n_photons_interval;
	long int n_photons_line;
	int n_interactions_trajectory;
	char *comments;
};


struct xmi_layer {
	int n_elements;
	int *Z;
	double *weight;
	double density;
	double thickness;
};





struct xmi_composition {
	int n_layers;
	struct xmi_layer *layers;
	int reference_layer;
};


struct xmi_geometry {
	double d_sample_source;
	double n_sample_orientation[3];
	double p_detector_window[3];
	double n_detector_orientation[3];
	double area_detector;
	double collimator_height;
	double collimator_diameter;
	double d_source_slit;
	double slit_size_x;
	double slit_size_y;
};

enum {
	XMI_DISCRETE_MONOCHROMATIC,
	XMI_DISCRETE_GAUSSIAN,
	XMI_DISCRETE_LORENTZIAN,
};



struct xmi_energy_discrete {
	double energy;
	double horizontal_intensity;
	double vertical_intensity;
	double sigma_x;
	double sigma_xp;
	double sigma_y;
	double sigma_yp;
	int distribution_type;
	double scale_parameter;
};

struct xmi_energy_continuous {
	double energy;
	double horizontal_intensity;
	double vertical_intensity;
	double sigma_x;
	double sigma_xp;
	double sigma_y;
	double sigma_yp;
};

struct xmi_excitation {
	int n_discrete;
	struct xmi_energy_discrete *discrete;
	int n_continuous;
	struct xmi_energy_continuous *continuous;
	double last_energy;
};


struct xmi_absorbers {
	int n_exc_layers;
	struct xmi_layer *exc_layers;
	int n_det_layers;
	struct xmi_layer *det_layers;
};

#define XMI_DETECTOR_SILI 0
#define XMI_DETECTOR_GE 1
#define XMI_DETECTOR_SI_SDD 2



struct xmi_detector {
	int detector_type;
	double live_time;
	double pulse_width;
	double gain;
	double zero;
	double fano;
	double noise;
	double max_convolution_energy;
	int n_crystal_layers;
	struct xmi_layer *crystal_layers;
};


struct xmi_input {
	struct xmi_general *general;
	struct xmi_composition *composition;
	struct xmi_geometry *geometry;
	struct xmi_excitation *excitation;
	struct xmi_absorbers *absorbers;
	struct xmi_detector *detector;
};

//one day I'll actually use this structure...
struct xmi_output {
	struct xmi_input *input;
	long int *brute_history;
	double *var_red_history;
	double **channels_conv;
	double *channels_unconv;
	int nchannels;
	char *inputfile;
	int use_zero_interactions;
};



struct xmi_main_options {
	int use_M_lines;
	int use_self_enhancement;
	int use_cascade_auger;
	int use_cascade_radiative;
	int use_variance_reduction;
	int use_optimizations;
	int use_sum_peaks;
	int escape_ratios_mode;
	int verbose;
	int use_poisson;
	int omp_num_threads;
	int extra_verbose;
};

//typedefs are clearer then using void *...
//these correspond in a more transparent way with the Fortran variables
typedef void* xmi_inputFPtr;  
typedef void* xmi_hdf5FPtr;

#define XMI_CONFLICT_GENERAL 1
#define XMI_CONFLICT_COMPOSITION 2
#define XMI_CONFLICT_GEOMETRY 4
#define XMI_CONFLICT_EXCITATION 8
#define XMI_CONFLICT_ABSORBERS 16
#define XMI_CONFLICT_DETECTOR 32

#define XMI_COMPARE_THRESHOLD 0.000001

void xmi_free_input(struct xmi_input *);

//returns 0 when identical, returns a number larger than 0 consisting of OR-ed XMI_CONFLICT_* macros if not identical
int xmi_compare_input(struct xmi_input *A, struct xmi_input *B);

//returns 0 when validated, returns a number larger than 0 consisting of OR-ed XMI_CONFLICT_* macros for every section where there is an error
int xmi_validate_input(struct xmi_input *);

void xmi_copy_input(struct xmi_input *A, struct xmi_input **B);

void xmi_free_composition(struct xmi_composition *);

void xmi_copy_composition(struct xmi_composition *A, struct xmi_composition **B);

void xmi_free_layer (struct xmi_layer *layer); 

void xmi_copy_layer(struct xmi_layer *, struct xmi_layer **B); 
void xmi_copy_layer2(struct xmi_layer *, struct xmi_layer *B); 

struct xmi_input *xmi_init_empty_input(void);

void xmi_free_absorbers(struct xmi_absorbers *);

void xmi_copy_absorbers(struct xmi_absorbers *A, struct xmi_absorbers **B);

void xmi_copy_abs_or_crystal2composition(struct xmi_layer *layers, int n_layers, struct xmi_composition **composition); 

void xmi_copy_composition2abs_or_crystal(struct xmi_composition *composition, struct xmi_layer **layers, int *n_layers);


//Fortran function that copies a C xmi_input structure to the corresponding Fortran TYPE variable. The function returns a pointer to the memory locatie of the Fortran variable
void xmi_input_C2F(struct xmi_input *xmi_inputC, xmi_inputFPtr *Ptr );

//Fortran function that frees a Fortran xmi_input TYPE variable. The value of the pointer shall be set to NULL afterwards.
void xmi_free_input_F(xmi_inputFPtr *inputFPtr);

//Fortran function that reads in from the HDF5 data file what it needs... return 1 on success, 0 otherwise
int xmi_init_from_hdf5(char *hdf5_file, xmi_inputFPtr inputFPtr, xmi_hdf5FPtr *hdf5FPtr );

//Fortran function that frees a Fortran xmi_hdf5 TYPE variable. The value of the pointer shall be set to NULL afterwards.
void xmi_free_hdf5_F(xmi_hdf5FPtr *hdf5FPtr);

int xmi_update_input_from_hdf5(xmi_inputFPtr inputFPtr, xmi_hdf5FPtr hdf5FPtr);

//Fortran function that further initializes the input
int xmi_init_input(xmi_inputFPtr *inputFPtr);

//prints the contents of the structure... useful when debugging
void xmi_print_input(FILE *fPtr, struct xmi_input *input);

void xmi_print_layer(FILE *fPtr, struct xmi_layer *layer, int n_layers);

#ifdef __cplusplus
}
#endif

#endif
