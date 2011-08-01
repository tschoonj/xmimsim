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

#include "xmi_data_structs.h"
#include "xmi_solid_angle.h"
#include "xmi_detector.h"



struct xmi_main_options {
	int use_M_lines;
	int use_self_enhancement;
	int use_cascade_auger;
	int use_cascade_radiative;
	int use_variance_reduction;
	int use_optimizations;
	int use_sum_peaks;
	int escape_ratios_mode;
};



int xmi_main_msim (xmi_inputFPtr inputFPtr, xmi_hdf5FPtr hdf5FPtr, int n_mpi_hosts, double **channels, int nchannels, struct xmi_main_options, double **brute_history, double **var_red_history, struct xmi_solid_angle *solid_angles);

void xmi_detector_convolute(xmi_inputFPtr inputFPtr, double *channels_noconv, double **channels_conv, int nchannels, struct xmi_main_options, struct xmi_escape_ratios *escape_ratios);

void xmi_test_brute(xmi_inputFPtr inputFPtr);


#endif
