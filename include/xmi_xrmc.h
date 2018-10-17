/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_XRMC_H
#define XMI_XRMC_H

#include "xmi_data_structs.h"

#ifdef __cplusplus
extern "C" {
#endif

//returns 1 one on success, 0 on error
int xmi_copy_input_to_xrmc(	xmi_input *input,
				char *xrmc_inputfile,
				char *xrmc_compositionfile,
				char *xrmc_detectorfile,
				char *xrmc_geom3dfile,
				char *xrmc_quadricfile,
				char *xrmc_samplefile,
				char *xrmc_sourcefile,
				char *xrmc_spectrumfile,
				char *xrmc_convolutedspectrumfile,
				char *xrmc_unconvolutedspectrumfile,
				xmi_layer *collimator,
				xmi_main_options *options,
				double rotate_angle_z
				);



#ifdef __cplusplus
}
#endif

#endif

