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

#include "config.h"
#include "xmi_msim.h"
#include <string.h>
#include <stdio.h>
#include <math.h>

static char *xmi_convert_xrmc_path(char *path) {
	int i,j;
	char *new_path = NULL;

	j = 0;
	for (i = 0 ; i < strlen(path) ; i++) {
		if (path[i] == ' ') {
			new_path = realloc(new_path, sizeof(char)*(j+3));
			strcat(new_path+j, "\\ ");
			j += 2;
		}
		else {
			new_path = realloc(new_path, sizeof(char)*(j+2));
			new_path[j] = path[i];
			j += 1;
		}
	}
	return new_path;
}

static int xmi_write_xrmc_inputfile(char *xrmc_inputfile, char *xrmc_compositionfile, char *xrmc_detectorfile, char *xrmc_geom3dfile, char *xrmc_quadricfile, char *xrmc_samplefile, char *xrmc_sourcefile, char *xrmc_spectrumfile, char *xrmc_convolutedspectrumfile, char *xrmc_unconvolutedspectrumfile) {
	FILE *filePtr;
	char *temp;

	if ((filePtr = fopen(xrmc_inputfile, "w")) == NULL) {
		fprintf(stderr, "Could not write to %s\n", xrmc_inputfile);
		return 0;
	}
	fprintf(filePtr, "; XRMC input-file for ED-XRF spectrometers\n; Produced through XMI-MSIM\n;\n");
	temp = xmi_convert_xrmc_path(xrmc_sourcefile);
	fprintf(filePtr, "Load %s\n", temp);
	free(temp);
	temp = xmi_convert_xrmc_path(xrmc_spectrumfile);
	fprintf(filePtr, "Load %s\n", temp);
	free(temp);
	temp = xmi_convert_xrmc_path(xrmc_samplefile);
	fprintf(filePtr, "Load %s\n", temp);
	free(temp);
	temp = xmi_convert_xrmc_path(xrmc_quadricfile);
	fprintf(filePtr, "Load %s\n", temp);
	free(temp);
	temp = xmi_convert_xrmc_path(xrmc_geom3dfile);
	fprintf(filePtr, "Load %s\n", temp);
	free(temp);
	temp = xmi_convert_xrmc_path(xrmc_compositionfile);
	fprintf(filePtr, "Load %s\n", temp);
	free(temp);
	temp = xmi_convert_xrmc_path(xrmc_detectorfile);
	fprintf(filePtr, "Load %s\n", temp);
	free(temp);
	fprintf(filePtr, "Run Detector\n");
	if (xrmc_convolutedspectrumfile) {
		temp = xmi_convert_xrmc_path(xrmc_convolutedspectrumfile);
		fprintf(filePtr, "Save Detector ConvolutedImage %s\n", temp);
		free(temp);
	}
	if (xrmc_unconvolutedspectrumfile) {
		temp = xmi_convert_xrmc_path(xrmc_unconvolutedspectrumfile);
		fprintf(filePtr, "Save Detector Image %s\n", temp);
		free(temp);
	}
	fprintf(filePtr, "End\n");
	fclose(filePtr);
	return 1;
}

static int xmi_write_xrmc_sourcefile(char *xrmc_sourcefile, struct xmi_input *input) {
	FILE *filePtr;

	if ((filePtr = fopen(xrmc_sourcefile, "w")) == NULL) {
		fprintf(stderr, "Could not write to %s\n", xrmc_sourcefile);
		return 0;
	}
	fprintf(filePtr, "Newdevice source\n");
	fprintf(filePtr, "Source\n");
	fprintf(filePtr, "SpectrumName Spectrum\n");
	fprintf(filePtr, "X 0.0 0.0 0.0\n");
	fprintf(filePtr, "uk 0.0 1.0 0.0\n");
	fprintf(filePtr, "ui -1.0 0.0 0.0\n");
	double div_x, div_y;
	div_x = atan(input->geometry->slit_size_y/2.0/input->geometry->d_source_slit);
	div_y = atan(input->geometry->slit_size_x/2.0/input->geometry->d_source_slit);

	fprintf(filePtr, "Divergence %g %g\n", div_x, div_y);
	fprintf(filePtr, "Size 0.0 0.0 0.0\n");
	fprintf(filePtr, "End\n");
	fclose(filePtr);
	return 1;
}

static int xmi_write_xrmc_spectrumfile(char *xrmc_spectrumfile, struct xmi_input *input) {
	FILE *filePtr;
	int i;

	if ((filePtr = fopen(xrmc_spectrumfile, "w")) == NULL) {
		fprintf(stderr, "Could not write to %s\n", xrmc_spectrumfile);
		return 0;
	}
	fprintf(filePtr, "Newdevice spectrum\n");
	fprintf(filePtr, "Spectrum\n");
	fprintf(filePtr, "PolarizedFlag 1\n");
	fprintf(filePtr, "LoopFlag 1\n");
	fprintf(filePtr, "ContinuousPhotonNum 1\n");
	fprintf(filePtr, "LinePhotonNum 1\n");
	fprintf(filePtr, "RandomEneFlag 1\n");
	if (input->excitation->n_discrete > 0) {
		fprintf(filePtr, "Lines\n%i\n", input->excitation->n_discrete);
		for (i = 0 ; i < input->excitation->n_discrete ; i++) {
			fprintf(filePtr, "%g %g %g %g\n", input->excitation->discrete[i].energy, (input->excitation->discrete[i].distribution_type == XMI_DISCRETE_GAUSSIAN) ? input->excitation->discrete[i].scale_parameter : 0.0, input->excitation->discrete[i].horizontal_intensity, input->excitation->discrete[i].vertical_intensity);
		}
	}
	if (input->excitation->n_continuous > 0) {
		fprintf(filePtr, "ContinuousSpectrum\n%i\n", input->excitation->n_continuous);
		for (i = 0 ; i < input->excitation->n_continuous ; i++) {
			fprintf(filePtr, "%g %g %g\n", input->excitation->continuous[i].energy, input->excitation->continuous[i].horizontal_intensity, input->excitation->continuous[i].vertical_intensity);
		}
		fprintf(filePtr, "Resample 0\n");
	}


	fprintf(filePtr, "End\n");
	fclose(filePtr);
	return 1;
}

int xmi_copy_input_to_xrmc(struct xmi_input *input, char *xrmc_inputfile, char *xrmc_compositionfile, char *xrmc_detectorfile, char *xrmc_geom3dfile, char *xrmc_quadricfile, char *xrmc_samplefile, char *xrmc_sourcefile, char *xrmc_spectrumfile, char *xrmc_convolutedspectrumfile, char *xrmc_unconvolutedspectrumfile, struct xmi_layer *collimator) {




	return 1;
}

