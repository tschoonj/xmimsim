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
XMI_EXPORT void xmi_detector_convolute_all_custom(xmi_inputFPtr inputFPtr, double **channels_noconv, double **channels_conv, double *brute_history, double *var_red_history, xmi_main_options *options, xmi_escape_ratios *escape_ratios, int n_interactions_all, int zero_interaction) {

	int i;
	xmi_input *input;

	xmi_input_F2C(inputFPtr, &input);

	//fprintf(stdout, "outputfile: %s\n", input->general->outputfile);


#ifdef _OPENMP
#pragma omp parallel for default(shared), private(i)
#endif
	for (i = (zero_interaction == 1 ? 0 : 1) ; i <= n_interactions_all ; i++) {
		xmi_detector_convolute_spectrum(inputFPtr, channels_noconv[i], &channels_conv[i], options, escape_ratios, i);
	}

        if (options->use_variance_reduction == 1 && var_red_history != NULL) {
          if (options->verbose == 1)
            fprintf(stdout, "Calculating variance reduction history detector absorption correction\n");
          xmi_detector_convolute_history(
            inputFPtr,
            var_red_history,
            options);
	}

        if (brute_history != NULL) {
          if (options->verbose == 1)
            fprintf(stdout, "Calculating brute force history detector absorption correction\n");
          xmi_detector_convolute_history(
            inputFPtr,
            brute_history,
            options);
        }
}
