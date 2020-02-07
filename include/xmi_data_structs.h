/*
Copyright (C) 2010-2020 Tom Schoonjans and Laszlo Vincze

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
#include <glib.h>

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
gboolean xmi_general_equals(xmi_general *A, xmi_general *B);

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
gboolean xmi_layer_equals(xmi_layer *A, xmi_layer *B);

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
gboolean xmi_composition_equals(xmi_composition *A, xmi_composition *B);

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
gboolean xmi_geometry_equals(xmi_geometry *A, xmi_geometry *B);

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
gboolean xmi_energy_discrete_equals(xmi_energy_discrete *a, xmi_energy_discrete *b);

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
gboolean xmi_energy_continuous_equals(xmi_energy_continuous *a, xmi_energy_continuous *b);


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
gboolean xmi_excitation_equals(xmi_excitation *A, xmi_excitation *B);

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
gboolean xmi_absorbers_equals(xmi_absorbers *A, xmi_absorbers *B);
xmi_layer* xmi_absorbers_get_exc_layer(xmi_absorbers *absorbers, int index);
xmi_layer* xmi_absorbers_get_det_layer(xmi_absorbers *absorbers, int index);

/**
 * XmiDetectorConvolutionProfile:
 * @XMI_DETECTOR_CONVOLUTION_PROFILE_SILI: for SiLi detectors
 * @XMI_DETECTOR_CONVOLUTION_PROFILE_GE: for Ge detectors
 * @XMI_DETECTOR_CONVOLUTION_PROFILE_SI_SDD: for silicon drift detectors
 *
 * An enum covering the different convolution profiles supported by the detector response function.
 * If the available profiles are not suitable, consider writing a custom detector response function.
 */
typedef enum {
	XMI_DETECTOR_CONVOLUTION_PROFILE_SILI,
	XMI_DETECTOR_CONVOLUTION_PROFILE_GE,
	XMI_DETECTOR_CONVOLUTION_PROFILE_SI_SDD,
} XmiDetectorConvolutionProfile;

typedef struct _xmi_detector xmi_detector;
/**
 * xmi_detector:
 * @detector_type: sets which builtin detector convolution profile should be used in the detector response function.
 * @live_time: the total amount of time that photons were detected. This is equal to the real time corrected with the dead time.
 * @pulse_width: the time that is necessary for one incoming photon to be processed by the detector. Will only be taken into account when the simulation is started with pile-up support.
 * @gain: the channels width (keV/channel)
 * @zero: the spectrum offset, AKA the energy corresponding to the first channel (keV)
 * @fano: the Fano factor for the detector material.
 * @noise: electronic noise (keV)
 * @nchannels: number of channels of the generated spectrum.
 * @n_crystal_layers: number of layers in the detector crystal. This should always be one...
 * @crystal_layers: (array length=n_crystal_layers): an array containing the different layers of the detectory crystal.
 *
 * This struct contains a description of the detector and its associated electronics..
 */
struct _xmi_detector {
	XmiDetectorConvolutionProfile detector_type;
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

xmi_detector* xmi_detector_new(XmiDetectorConvolutionProfile detector_type, double live_time, double pulse_width, double gain, double zero, double fano, double noise, int nchannels, int n_crystal_layers, xmi_layer *crystal_layers);
xmi_layer* xmi_detector_get_crystal_layer(xmi_detector *detector, int index);
void xmi_detector_copy(xmi_detector *A, xmi_detector **B);
void xmi_detector_free(xmi_detector *detector);
gboolean xmi_detector_equals(xmi_detector *A, xmi_detector *B);


typedef struct _xmi_input xmi_input;
/**
 * xmi_input:
 * @general: a pointer to an #xmi_general struct
 * @composition: a pointer to an #xmi_composition struct
 * @geometry: a pointer to an #xmi_geometry struct
 * @excitation: a pointer to an #xmi_excitation struct
 * @absorbers: a pointer to an #xmi_absorbers struct
 * @detector: a pointer to an #xmi_detector struct
 *
 * This struct contains the description of all parameters that will determine the output of a simulation. Consult the structs within for more specific information.
 */
struct _xmi_input {
	xmi_general *general;
	xmi_composition *composition;
	xmi_geometry *geometry;
	xmi_excitation *excitation;
	xmi_absorbers *absorbers;
	xmi_detector *detector;
};

void xmi_input_free(xmi_input *input);
void xmi_input_copy(xmi_input *A, xmi_input **B);
xmi_input *xmi_input_init_empty(void);
/* the following xmi_input functions are mostly intended to be used from Gobject-Introspection */
xmi_input *xmi_input_new(xmi_general *general, xmi_composition *composition, xmi_geometry *geometry, xmi_excitation *excitation, xmi_absorbers *absorbers, xmi_detector *detector);
void xmi_input_set_general(xmi_input *input, xmi_general *general);
void xmi_input_set_composition(xmi_input *input, xmi_composition *composition);
void xmi_input_set_geometry(xmi_input *input, xmi_geometry *geometry);
void xmi_input_set_excitation(xmi_input *input, xmi_excitation *excitation);
void xmi_input_set_absorbers(xmi_input *input, xmi_absorbers *absorbers);
void xmi_input_set_detector(xmi_input *input, xmi_detector *detector);

#ifndef __GI_SCANNER__
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
#endif

typedef struct _xmi_output xmi_output;
/**
 * xmi_output:
 * @version: the version of XMI-MSIM that was used to write this file.
 * @inputfile: the name of the input-file that was used to generate this output.
 * @outputfile: the name of the output-file that contains this output
 * @input: an #xmi_input struct containing a description of the simulation parameters.
 * @brute_force_history: (skip): 
 * @var_red_history: (skip):
 * @nbrute_force_history: (skip):
 * @nvar_red_history: (skip):
 * @channels_conv: (skip):
 * @channels_unconv: (skip):
 * @ninteractions: the maximum number of interactions a photon could experience during the simulation.
 * @use_zero_interactions: if set to 1, this indicates that the simulation was ran in brute force mode and that it is possible that photons were recorded that experienced no interaction while moving through the sample, assuming the detector was positioned in the beampath.
 *
 * This struct contains the output of an XMI-MSIM simulation.
 */
struct _xmi_output {
	float version;
	char *inputfile;
	char *outputfile; // this will be the file XMSO filename, not necessarily the same as input->general->outputfile
	xmi_input *input;
#ifndef __GI_SCANNER__
	xmi_fluorescence_line_counts *brute_force_history;
	xmi_fluorescence_line_counts *var_red_history;
#else
	void *brute_force_history;
	void *var_red_history;
#endif
	int nbrute_force_history;
	int nvar_red_history;
	double **channels_conv;
	double **channels_unconv;
	int ninteractions;
	int use_zero_interactions;
};

void xmi_output_free(xmi_output *output);
void xmi_output_copy(xmi_output *A, xmi_output **B);
GArray* xmi_output_get_spectrum_convoluted(xmi_output *output, int after_interactions);
GArray* xmi_output_get_spectrum_unconvoluted(xmi_output *output, int after_interactions);
GHashTable* xmi_output_get_history(xmi_output *output);
double xmi_output_get_counts_for_element_line(xmi_output *output, int Z, int line);

typedef struct _xmi_batch_single_data xmi_batch_single_data;

/**
 * xmi_batch_single_data:
 * @xpath: XPath expression
 * @start: start value
 * @end: end value
 * @nsteps: number of steps to go from start to end
 *
 * A struct describing a single parameter for a XmiMsimBatchSingle task.
 */
struct _xmi_batch_single_data {
	gchar *xpath;
	gdouble start;
	gdouble end;
	guint nsteps;
};

xmi_batch_single_data* xmi_batch_single_data_new(gchar *xpath, gdouble start, gdouble end, guint nsteps);

void xmi_batch_single_data_copy(xmi_batch_single_data *A, xmi_batch_single_data **B);

void xmi_batch_single_data_free(xmi_batch_single_data *A);

//returns TRUE when identical, returns FALSE if not identical
gboolean xmi_batch_single_data_equals(xmi_batch_single_data *A, xmi_batch_single_data *B);

typedef struct _xmi_archive xmi_archive;
/**
 * xmi_archive:
 * @version: XMI-MSIM version that was used to create this file
 * @single_data: (element-type XmiMsim.BatchSingleData): array containing details of the grid that was used to generate the archive
 * @output: (element-type XmiMsim.Output): array containing the output data
 * @dims: (element-type gint): output dimensions array
 * @ref_count: (skip): number of steps to go from start to end
 *
 * A struct describing a single parameter for a XmiMsimBatchSingle task.
 */
struct _xmi_archive {
	float version;
	GPtrArray *single_data;
	GPtrArray *output;
	GArray *dims;
	gint ref_count;
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
 * @use_gpu: use GPU where possible
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
	int use_gpu; //default : 1
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
/**
 * xmi_inputFPtr: (skip):
 */
typedef void* xmi_inputFPtr;

typedef enum {
	XMI_INPUT_GENERAL = 1,
	XMI_INPUT_COMPOSITION = 2,
	XMI_INPUT_GEOMETRY = 4,
	XMI_INPUT_EXCITATION = 8,
	XMI_INPUT_ABSORBERS = 16,
	XMI_INPUT_DETECTOR = 32,
} XmiInputFlags;

#define XMI_COMPARE_THRESHOLD 1E-10


//returns 0 when identical, returns a number larger than 0 consisting of OR-ed XMI_INPUT_* macros if not identical
XmiInputFlags xmi_input_compare(xmi_input *A, xmi_input *B);

//returns TRUE when identical, returns FALSE if not identical
gboolean xmi_input_equals(xmi_input *A, xmi_input *B);

//returns TRUE when identical, returns FALSE if not identical
gboolean xmi_output_equals(xmi_output *A, xmi_output *B);

//returns TRUE when identical, returns FALSE if not identical
gboolean xmi_archive_equals(xmi_archive *A, xmi_archive *B);

//returns 0 when validated, returns a number larger than 0 consisting of OR-ed XMI_INPUT_* macros for every section where there is an error
XmiInputFlags xmi_input_validate(xmi_input *input);

/** 
 * xmi_copy_abs_or_crystal2composition: (skip):
 *
 */
void xmi_copy_abs_or_crystal2composition(xmi_layer *layers, int n_layers, xmi_composition **composition);

/** 
 * xmi_copy_composition2abs_or_crystal: (skip):
 *
 */
void xmi_copy_composition2abs_or_crystal(xmi_composition *composition, xmi_layer **layers, int *n_layers);


//Fortran function that copies a C xmi_input structure to the corresponding Fortran TYPE variable. The function returns a pointer to the memory locatie of the Fortran variable
/**
 * xmi_input_C2F: (skip):
 */
void xmi_input_C2F(xmi_input *xmi_inputC, xmi_inputFPtr *Ptr);
/**
 * xmi_input_F2C: (skip):
 */
void xmi_input_F2C(xmi_inputFPtr Ptr, xmi_input **xmi_inputC);

//Fortran function that frees a Fortran xmi_input TYPE variable. The value of the pointer shall be set to NULL afterwards.
/**
 * xmi_free_input_F: (skip):
 */
void xmi_free_input_F(xmi_inputFPtr *inputFPtr);


//Fortran function that further initializes the input
/**
 * xmi_init_input: (skip):
 */
int xmi_init_input(xmi_inputFPtr *inputFPtr);

//prints the contents of the structure... useful when debugging
void xmi_input_print(xmi_input *input, FILE *fPtr);

xmi_output* xmi_output_raw2struct(xmi_input *input, double *brute_history, double *var_red_history,double **channels_conv, double *channels_unconv, char *inputfile, int use_zero_interactions );

#ifndef __GI_SCANNER__
void xmi_fluorescence_line_counts_free(xmi_fluorescence_line_counts *history, int nhistory);
#endif

xmi_archive* xmi_archive_ref(xmi_archive *A);
void xmi_archive_unref(xmi_archive *archive);

xmi_archive* xmi_archive_new(GPtrArray *single_data, GPtrArray *output);

void xmi_exc_absorbers_copy(xmi_absorbers *A, xmi_absorbers *B);
void xmi_det_absorbers_copy(xmi_absorbers *A, xmi_absorbers *B);

void xmi_exc_absorbers_free(xmi_absorbers *A);
void xmi_det_absorbers_free(xmi_absorbers *A);

#ifdef __cplusplus
}
#endif

#endif
