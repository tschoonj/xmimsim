/*
Copyright (C) 2010-2018 Tom Schoonjans and Laszlo Vincze

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

typedef struct _xmi_general xmi_general;
/**
 * xmi_general:
 * @version: XMI-MSIM version of the library that was used when writing the file.
 * @outputfile: the name of the file that the simulation results will be written into (the XMSO-file...).
 * @n_photons_interval: the number of photons that will be simulated per interval of the continuous excitation spectrum
 * @n_photons_line: the number of photons that will be simulated per line in the discrete excitation spectrum
 * @n_interactions_trajectory: the maximum number of interactions that will be experienced by each simulated photon. A typical value is 4.
 * @comments: text that may be added to the input-file that may help the reader to understand the purpose of the simulation.
 *
 * A struct containing the general settings for an XMI-MSIM simulation
 */
struct _xmi_general {
	float version;
	char *outputfile;
	long n_photons_interval;
	long n_photons_line;
	int n_interactions_trajectory;
	char *comments;
};

xmi_general* xmi_general_new(const char *outputfile, long n_photons_interval, long n_photons_line, int n_interactions_trajectory, const char *comments);
void xmi_general_copy(xmi_general *A, xmi_general **B);
void xmi_general_free(xmi_general *A);

typedef struct _xmi_layer xmi_layer;
/**
 * xmi_layer:
 * @n_elements: (skip): the number of elements that is contained within this layer.
 * @Z: (array length=n_elements): an array containing the atomic numbers of the elements within the layer.
 * @weight: (array length=n_elements): an array containing the element weights. The sum of these weights should be 1.
 * @density: the density of the layer, expressed in g/cm3.
 * @thickness: the thickness of the layer, expressed in cm.
 *
 * A struct containing a description of a layer, a concept used in several more types. In XMI-MSIM, layers are assumed to be defined within just one dimension, along the sample normal vector, and are infinite in the two dimensions orthogonal to this vector.
 */
struct _xmi_layer {
	int n_elements;
	int *Z;
	double *weight;
	double density;
	double thickness;
};

xmi_layer* xmi_layer_new(int n_elements, int *Z, double *weight, double density, double thickness);
void xmi_layer_free(xmi_layer *layer);
void xmi_layer_copy(xmi_layer *A, xmi_layer **B);
void xmi_layer_copy2(xmi_layer *A, xmi_layer *B);
void xmi_layer_print(xmi_layer *layer, FILE *fPtr);

typedef struct _xmi_composition xmi_composition;
/**
 * xmi_composition:
 * @n_layers: (skip): the number of layers that is contained within this sample composition.
 * @layers: (array length=n_layers): an array containing the layers that make up the sample composition.
 * @reference_layer: this value corresponds to the layer within #layers that will be used to calculate the sample to source distance. First layer corresponds to 1, not 0!
 *
 * A struct containing a description of the sample composition. Layers are assumed to be ordered according to increasing distance from source.
 */
struct _xmi_composition {
	int n_layers;
	xmi_layer *layers;
	int reference_layer;
};

xmi_composition* xmi_composition_new(int n_layers, xmi_layer *layers, int reference_layer);
xmi_layer* xmi_composition_get_layer(xmi_composition *composition, int index);
void xmi_composition_free(xmi_composition *composition);
void xmi_composition_copy(xmi_composition *A, xmi_composition **B);

typedef struct _xmi_geometry xmi_geometry;
/**
 * xmi_geometry:
 * @d_sample_source: the distance (in cm) between source and sample. The sample layer marked as reference_layer will be used for this calculation.
 * @n_sample_orientation: (array fixed-size=3): a three coordinate array describing the sample normal vector. The z-component has to be positive!
 * @p_detector_window: (array fixed-size=3): a three coordinate array describing the position (in cm) of the detector window.
 * @n_detector_orientation: (array fixed-size=3): a three coordinate array describing the normal vector of the detector window.
 * @area_detector: the active detector area (in cm2).
 * @collimator_height: the height of the collimator cone, measured from the detector window, in cm.
 * @collimator_diameter: the diameter of the collimator cone opening, in cm2
 * @d_source_slit: the distance between source and (virtual) slits, in cm.
 * @slit_size_x: the height of the slits, in cm.
 * @slit_size_y: the width of the slits, in cm.
 *
 * A struct containing a description of the experiment geometry. Collimators are optional: to disable them set both #collimator_height and #collimator_diameter to 0.
 */
struct _xmi_geometry {
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

xmi_geometry* xmi_geometry_new(double d_sample_source, double n_sample_orientation[3], double p_detector_window[3], double n_detector_orientation[3], double area_detector, double collimator_height, double collimator_diameter, double d_source_slit, double slit_size_x, double slit_size_y);
void xmi_geometry_copy(xmi_geometry *A, xmi_geometry **B);
void xmi_geometry_free(xmi_geometry *geometry);

/**
 * XmiEnergyDiscreteDistribution:
 * @XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC: Assume a purely monochromatic energy source. No sampling will be done as the energy as defined in xmi_energy_discrete will always be used as is.
 * @XMI_ENERGY_DISCRETE_DISTRIBUTION_GAUSSIAN: Energies will be sampled from a Gaussian distribution will mean set to the energy from the xmi_energy_discrete struct and the standard deviation to the scale_parameter.
 * @XMI_ENERGY_DISCRETE_DISTRIBUTION_LORENTZIAN: Energies will be sampled from a Lorentzian distribution with median set to the energy from the xmi_energy_discrete struct and the scale parameter to the scale_parameter.
 *
 * An enum covering the different types that can be used to sample from discrete energy distributions.
 */
typedef enum {
	XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC,
	XMI_ENERGY_DISCRETE_DISTRIBUTION_GAUSSIAN,
	XMI_ENERGY_DISCRETE_DISTRIBUTION_LORENTZIAN
} XmiEnergyDiscreteDistribution;

typedef struct _xmi_energy_discrete xmi_energy_discrete;
/**
 * xmi_energy_discrete:
 * @energy: The energy of the X-ray line (keV).
 * @horizontal_intensity: The horizontally polarized X-ray intensity (photons/sec)
 * @vertical_intensity: The vertically polarized X-ray intensity (photons/sec)
 * @sigma_x: If both sigma_x and sigma_y are non-zero, the photon starting position will be sampled from bi-Gaussian distribution based on these values. Otherwise a point source will be assumed.
 * @sigma_xp: If both sigma_xp and sigma_yp are non-zero, and the source is Gaussian, then the Source-slits distance takes on a new role as it becomes the distance between the actual focus and the source position. In this way a convergent beam can be defined, emitted by a Gaussian source at the origin. For the specific case of focusing on the sample the Sample-source distance should be set to the Source-slits distance.
 * @sigma_y: If both sigma_x and sigma_y are non-zero, the photon starting position will be sampled from bi-Gaussian distribution based on these values. Otherwise a point source will be assumed.
 * @sigma_yp: If both sigma_xp and sigma_yp are non-zero, and the source is Gaussian, then the Source-slits distance takes on a new role as it becomes the distance between the actual focus and the source position. In this way a convergent beam can be defined, emitted by a Gaussian source at the origin. For the specific case of focusing on the sample the Sample-source distance should be set to the Source-slits distance.
 * @distribution_type: The distribution type that can be assumed by the line.
 * @scale_parameter: If the distribution_type is not XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC, than this value will be used as scale parameter for sampling energies from the distribution.
 *
 * This struct contains a description of a single X-ray line from the exciting spectrum.
 */
struct _xmi_energy_discrete {
	double energy;
	double horizontal_intensity;
	double vertical_intensity;
	double sigma_x;
	double sigma_xp;
	double sigma_y;
	double sigma_yp;
	XmiEnergyDiscreteDistribution distribution_type;
	double scale_parameter;
};

xmi_energy_discrete* xmi_energy_discrete_new(double energy, double horizontal_intensity, double vertical_intensity, double sigma_x, double sigma_xp, double sigma_y, double sigma_yp, XmiEnergyDiscreteDistribution distribution_type, double scale_parameter);
void xmi_energy_discrete_copy(xmi_energy_discrete *A, xmi_energy_discrete **B);
void xmi_energy_discrete_free(xmi_energy_discrete *A);

typedef struct _xmi_energy_continuous xmi_energy_continuous;
/**
 * xmi_energy_continuous:
 * @energy: The energy at this sampling point in the X-ray exciting continuum (keV).
 * @horizontal_intensity: The horizontally polarized X-ray intensity density (photons/sec/keV)
 * @vertical_intensity: The vertically polarized X-ray intensity density (photons/sec/keV)
 * @sigma_x: If both sigma_x and sigma_y are non-zero, the photon starting position will be sampled from bi-Gaussian distribution based on these values. Otherwise a point source will be assumed.
 * @sigma_xp: If both sigma_xp and sigma_yp are non-zero, and the source is Gaussian, then the Source-slits distance takes on a new role as it becomes the distance between the actual focus and the source position. In this way a convergent beam can be defined, emitted by a Gaussian source at the origin. For the specific case of focusing on the sample the Sample-source distance should be set to the Source-slits distance.
 * @sigma_y: If both sigma_x and sigma_y are non-zero, the photon starting position will be sampled from bi-Gaussian distribution based on these values. Otherwise a point source will be assumed.
 * @sigma_yp: If both sigma_xp and sigma_yp are non-zero, and the source is Gaussian, then the Source-slits distance takes on a new role as it becomes the distance between the actual focus and the source position. In this way a convergent beam can be defined, emitted by a Gaussian source at the origin. For the specific case of focusing on the sample the Sample-source distance should be set to the Source-slits distance.
 *
 * This struct contains a description of a single X-ray sampling point within the continuous part of the exciting X-ray spectrum.
 */
struct _xmi_energy_continuous {
	double energy;
	double horizontal_intensity;
	double vertical_intensity;
	double sigma_x;
	double sigma_xp;
	double sigma_y;
	double sigma_yp;
};

xmi_energy_continuous* xmi_energy_continuous_new(double energy, double horizontal_intensity, double vertical_intensity, double sigma_x, double sigma_xp, double sigma_y, double sigma_yp);
void xmi_energy_continuous_copy(xmi_energy_continuous *A, xmi_energy_continuous **B);
void xmi_energy_continuous_free(xmi_energy_continuous *A);

typedef struct _xmi_excitation xmi_excitation;
/**
 * xmi_excitation:
 * @n_discrete: the number of discrete exciting X-ray lines in the exciting spectrum.
 * @discrete: (array length=n_discrete): an array containing the discrete components of the exciting spectrum.
 * @n_continuous: the number of sampling points within the continuous part of the exciting spectrum.
 * @continuous: (array length=n_continuous): an array containing the continuous components of the exciting spectrum.
 *
 * This struct contains a description of the excitation spectrum.
 */
struct _xmi_excitation {
	int n_discrete;
	xmi_energy_discrete *discrete;
	int n_continuous;
	xmi_energy_continuous *continuous;
};

xmi_excitation* xmi_excitation_new(int n_discrete, xmi_energy_discrete *discrete, int n_continuous, xmi_energy_continuous *continuous);
void xmi_excitation_copy(xmi_excitation *A, xmi_excitation **B);
void xmi_excitation_free(xmi_excitation *excitation);

xmi_energy_discrete* xmi_excitation_get_energy_discrete(xmi_excitation *excitation, int index);
xmi_energy_continuous* xmi_excitation_get_energy_continuous(xmi_excitation *excitation, int index);

typedef struct _xmi_absorbers xmi_absorbers;
/**
 * xmi_absorbers:
 * @n_exc_layers: the number of absorbing layers in the excitation channel (between source and sample).
 * @exc_layers: (array length=n_exc_layers): an array containing the excitation channel absorbing layers.
 * @n_det_layers: the number of absorbing layers in the detection channel (between sample and detector).
 * @det_layers: (array length=n_det_layers): an array containing the detection channel absorbing layers.
 *
 * This struct contains a description of the absorbers in the simulation.
 */
struct _xmi_absorbers {
	int n_exc_layers;
	xmi_layer *exc_layers;
	int n_det_layers;
	xmi_layer *det_layers;
};

xmi_absorbers* xmi_absorbers_new(int n_exc_layers, xmi_layer *exc_layers, int n_det_layers, xmi_layer *det_layers);
void xmi_absorbers_copy(xmi_absorbers *A, xmi_absorbers **B);
void xmi_absorbers_free(xmi_absorbers *absorbers);
xmi_layer* xmi_absorbers_get_exc_layer(xmi_absorbers *absorbers, int index);
xmi_layer* xmi_absorbers_get_det_layer(xmi_absorbers *absorbers, int index);

#define XMI_DETECTOR_SILI 0
#define XMI_DETECTOR_GE 1
#define XMI_DETECTOR_SI_SDD 2

typedef struct _xmi_detector xmi_detector;
struct _xmi_detector {
	int detector_type;
	double live_time;
	double pulse_width;
	double gain;
	double zero;
	double fano;
	double noise;
	int nchannels;
	int n_crystal_layers;
	xmi_layer *crystal_layers;
};

typedef struct _xmi_input xmi_input;
struct _xmi_input {
	xmi_general *general;
	xmi_composition *composition;
	xmi_geometry *geometry;
	xmi_excitation *excitation;
	xmi_absorbers *absorbers;
	xmi_detector *detector;
};

typedef struct _xmi_counts xmi_counts;
struct _xmi_counts {
	double counts;
	int interaction_number;
};

typedef struct _xmi_fluorescence_line xmi_fluorescence_line;
struct _xmi_fluorescence_line {
	char *line_type;
	double energy;
	double total_counts;
	int n_interactions;
	xmi_counts *interactions;
};

typedef struct _xmi_fluorescence_line_counts xmi_fluorescence_line_counts;
struct _xmi_fluorescence_line_counts {
	int atomic_number;
	double total_counts;
	int n_lines;
	xmi_fluorescence_line *lines;
};

typedef struct _xmi_output xmi_output;
struct _xmi_output {
	float version;
	char *inputfile;
	char *outputfile; // this will be the file XMSO filename, not necessarily the same as input->general->outputfile
	xmi_input *input;
	xmi_fluorescence_line_counts *brute_force_history;
	xmi_fluorescence_line_counts *var_red_history;
	int nbrute_force_history;
	int nvar_red_history;
	double **channels_conv;
	double **channels_unconv;
	int ninteractions;
	int use_zero_interactions;
};

typedef struct _xmi_archive xmi_archive;
struct _xmi_archive {
	float version;
	double start_value1;
	double end_value1;
	int nsteps1;
	char *xpath1;
	double start_value2;
	double end_value2;
	int nsteps2;
	char *xpath2;
	//input are just pointers to the input structs with output!
	xmi_input ***input;
	xmi_output ***output;
	//inputfiles and outputfiles are also just pointers to strings in input and output! don't free them!
	char ***inputfiles;
	char ***outputfiles;
};

typedef struct _xmi_main_options xmi_main_options;
/**
 * xmi_main_options:
 * @use_M_lines: should XRF M-lines be simulated
 * @use_cascade_auger: should the Auger contributions to the cascase effect be simulated
 * @use_cascade_radiative: should the radiative transition contributions to the cascade effect be simulated
 * @use_variance_reduction: if disabled, the simulation will operate in brute force mode
 * @use_sum_peaks: whether sum peaks (pile up) should be simulated
 * @escape_ratios_mode: (skip): do not use this!
 * @verbose: should output be verbose
 * @use_poisson: randomize output spectra according to Poisson distributions
 * @use_opencl: use OpenCL where possible
 * @omp_num_threads: the number of threads to be used by OpenMP
 * @extra_verbose: make it even more verbose!
 * @custom_detector_response: path to custom detector reponse plugin
 * @use_advanced_compton: use advanced and more accurate model to simulate Compton peaks
 * @use_default_seeds: whether a set of default seeds should be used. Required for exact reproducible simulations
 *
 * A struct containing all options for an XMI-MSIM simulation
 */
struct _xmi_main_options {
	int use_M_lines; //default : 1
	int use_cascade_auger; //default : 1
	int use_cascade_radiative; //default : 1
	int use_variance_reduction; //default : 1
	int use_sum_peaks; //default : 0
	int use_escape_peaks; //default : 1
	int escape_ratios_mode; //default : 0
	int verbose; //default : 0
	int use_poisson; //default : 0
	int use_opencl; //default : 1
	int omp_num_threads; //default : max
	int extra_verbose; //default : 0
	char *custom_detector_response; //default : NULL
	int use_advanced_compton; //default : 0
	int use_default_seeds; // default : 0
};

xmi_main_options* xmi_main_options_new(void);
void xmi_main_options_free(xmi_main_options *options);
void xmi_main_options_copy(xmi_main_options *A, xmi_main_options **B);

//typedefs are clearer then using void *...
//these correspond in a more transparent way with the Fortran variables
typedef void* xmi_inputFPtr;



#define XMI_CONFLICT_GENERAL 1
#define XMI_CONFLICT_COMPOSITION 2
#define XMI_CONFLICT_GEOMETRY 4
#define XMI_CONFLICT_EXCITATION 8
#define XMI_CONFLICT_ABSORBERS 16
#define XMI_CONFLICT_DETECTOR 32

#define XMI_COMPARE_THRESHOLD 1E-10

void xmi_input_free(xmi_input *input);

//returns 0 when identical, returns a number larger than 0 consisting of OR-ed XMI_CONFLICT_* macros if not identical
int xmi_input_compare(xmi_input *A, xmi_input *B);

//returns 0 when identical, returns 1 if not identical
int xmi_output_compare(xmi_output *A, xmi_output *B);

//returns 0 when identical, returns 1 if not identical
int xmi_archive_compare(xmi_archive *A, xmi_archive *B);

//returns 0 when validated, returns a number larger than 0 consisting of OR-ed XMI_CONFLICT_* macros for every section where there is an error
int xmi_input_validate(xmi_input *input);

// returns 1 when equal, 0 otherwise
int xmi_energy_discrete_equal(xmi_energy_discrete *a, xmi_energy_discrete *b);

// returns 1 when equal, 0 otherwise
int xmi_energy_continuous_equal(xmi_energy_continuous *a, xmi_energy_continuous *b);

void xmi_input_copy(xmi_input *A, xmi_input **B);

xmi_input *xmi_input_init_empty(void);


void xmi_copy_abs_or_crystal2composition(xmi_layer *layers, int n_layers, xmi_composition **composition);

void xmi_copy_composition2abs_or_crystal(xmi_composition *composition, xmi_layer **layers, int *n_layers);


//Fortran function that copies a C xmi_input structure to the corresponding Fortran TYPE variable. The function returns a pointer to the memory locatie of the Fortran variable
void xmi_input_C2F(xmi_input *xmi_inputC, xmi_inputFPtr *Ptr);
void xmi_input_F2C(xmi_inputFPtr Ptr, xmi_input **xmi_inputC);

//Fortran function that frees a Fortran xmi_input TYPE variable. The value of the pointer shall be set to NULL afterwards.
void xmi_free_input_F(xmi_inputFPtr *inputFPtr);


//Fortran function that further initializes the input
int xmi_init_input(xmi_inputFPtr *inputFPtr);

//prints the contents of the structure... useful when debugging
void xmi_input_print(xmi_input *input, FILE *fPtr);

xmi_output* xmi_output_raw2struct(xmi_input *input, double *brute_history, double *var_red_history,double **channels_conv, double *channels_unconv, char *inputfile, int use_zero_interactions );

void xmi_fluorescence_line_counts_free(xmi_fluorescence_line_counts *history, int nhistory);

void xmi_output_free(xmi_output *output);

void xmi_archive_copy(xmi_archive *A, xmi_archive **B);
void xmi_archive_free(xmi_archive *archive);

xmi_archive* xmi_archive_raw2struct(xmi_output ***output, double start_value1, double end_value1, int nsteps1, char *xpath1, double start_value2, double end_value2, int nsteps2, char *xpath2);

void xmi_output_copy(xmi_output *A, xmi_output **B);

double xmi_output_get_counts_for_element_line(xmi_output *output, int Z, int line);

void xmi_detector_copy(xmi_detector *A, xmi_detector **B);
void xmi_exc_absorbers_copy(xmi_absorbers *A, xmi_absorbers *B);
void xmi_det_absorbers_copy(xmi_absorbers *A, xmi_absorbers *B);

void xmi_detector_free(xmi_detector *A);
void xmi_exc_absorbers_free(xmi_absorbers *A);
void xmi_det_absorbers_free(xmi_absorbers *A);

#ifdef __cplusplus
}
#endif

#endif
