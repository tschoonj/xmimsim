/*
Copyright (C) 2010-2013 Tom Schoonjans and Laszlo Vincze

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

#include "xmi_main.h"
#include <stdio.h>

/* Another example of how to use a custom detector response function,
 * this time written in C (although my Fortran function does the real work here,
 * but you could essentially write whatever you want here)
 * Although the inputFPtr is essentially useless here, it is possible to extract
 * the corresponding C input variable from it. Use the xmi_input_F2C subroutine for this
 * as shown here.
 */

#ifdef _WIN32
#  define       XMI_EXPORT         __declspec(dllexport)
#else 
#  define       XMI_EXPORT
#endif
XMI_EXPORT void xmi_detector_convolute_all_custom(xmi_inputFPtr inputFPtr, double **channels_noconv, double **channels_conv, struct xmi_main_options options, struct xmi_escape_ratios *escape_ratios, int n_interactions_all, int zero_interaction) {

	int i;
	struct xmi_input *input;

	xmi_input_F2C(inputFPtr, &input);

	//fprintf(stdout, "outputfile: %s\n", input->general->outputfile);


	for (i = (zero_interaction == 1 ? 0 : 1) ; i <= n_interactions_all ; i++) {
		xmi_detector_convolute(inputFPtr, channels_noconv[i], &channels_conv[i], options, escape_ratios, i);
	}

}
