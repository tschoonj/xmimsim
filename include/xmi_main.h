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

#ifndef XMI_MAIN_H
#define XMI_MAIN_H

#include "xmi_solid_angle.h"
#include "xmi_detector.h"
#include "xmi_data.h"

#ifdef __cplusplus
extern "C" {
#endif

struct xmi_main_options {
	int use_M_lines; //default : 1
	int use_cascade_auger; //default : 1
	int use_cascade_radiative; //default : 1
	int use_variance_reduction; //default : 1
	int use_sum_peaks; //default : 0
	int use_escape_peaks; //default : 1
	int escape_ratios_mode; //default : 0
	int verbose; //default : 0
	int use_poisson; //default : 0
	int use_opencl; //default : 0
	int omp_num_threads; //default : max
	int extra_verbose; //default : 0
	char *custom_detector_response; //default : NULL
	int use_advanced_compton; //default : 0
	int use_default_seeds; // default : 0
};

struct xmi_main_options xmi_get_default_main_options();

int xmi_main_msim (xmi_inputFPtr inputFPtr, xmi_hdf5FPtr hdf5FPtr, int n_mpi_hosts, double **channels, struct xmi_main_options, double **brute_history, double **var_red_history, struct xmi_solid_angle *solid_angles);

void xmi_detector_convolute_spectrum(xmi_inputFPtr inputFPtr, double *channels_noconv, double **channels_conv, struct xmi_main_options, struct xmi_escape_ratios *escape_ratios, int n_interactions);

void xmi_detector_convolute_all(xmi_inputFPtr inputFPtr, double **channels_noconv, double **channels_conv, double *brute_history, double *var_red_history, struct xmi_main_options, struct xmi_escape_ratios *escape_ratios, int n_interactions_all, int zero_interaction);

void xmi_detector_convolute_history(xmi_inputFPtr inputFPtr, double *history, struct xmi_main_options);

void xmi_deallocate(void *array);

typedef void (*XmiDetectorConvoluteAll) (xmi_inputFPtr inputFPtr, double **channels_noconv, double **channels_conv, double *brute_history, double *var_red_history, struct xmi_main_options, struct xmi_escape_ratios *escape_ratios, int n_interactions_all, int zero_interaction);

#ifdef __cplusplus
}
#endif

#endif
