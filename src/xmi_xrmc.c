/*
Copyright (C) 2010-2014 Tom Schoonjans and Laszlo Vincze

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
#include <xraylib.h>

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
			new_path[j+1] = '\0';
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
	fprintf(filePtr, "ui 1.0 0.0 0.0\n");
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
	int i, j, k;

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
			double blb = 1.0;
			for (j = 0 ; j < input->absorbers->n_exc_layers ; j++) {
				double mu = 0.0;
				for (k = 0 ; k < input->absorbers->exc_layers[j].n_elements ; k++) {
					mu += CS_Total_Kissel(input->absorbers->exc_layers[j].Z[k], input->excitation->discrete[i].energy)*input->absorbers->exc_layers[j].weight[k];
				}
				blb *= exp(-1.0*mu*input->absorbers->exc_layers[j].density*input->absorbers->exc_layers[j].thickness);
			}
			fprintf(filePtr, "%g %g %g %g\n", input->excitation->discrete[i].energy, (input->excitation->discrete[i].distribution_type == XMI_DISCRETE_GAUSSIAN) ? input->excitation->discrete[i].scale_parameter : 0.0, blb*input->excitation->discrete[i].horizontal_intensity, blb*input->excitation->discrete[i].vertical_intensity);
		}
	}
	if (input->excitation->n_continuous > 0) {
		fprintf(filePtr, "ContinuousSpectrum\n%i\n", input->excitation->n_continuous);
		for (i = 0 ; i < input->excitation->n_continuous ; i++) {
			double blb = 1.0;
			for (j = 0 ; j < input->absorbers->n_exc_layers ; j++) {
				double mu = 0.0;
				for (k = 0 ; k < input->absorbers->exc_layers[j].n_elements ; k++) {
					mu += CS_Total_Kissel(input->absorbers->exc_layers[j].Z[k], input->excitation->continuous[i].energy)*input->absorbers->exc_layers[j].weight[k];
				}
				blb *= exp(-1.0*mu*input->absorbers->exc_layers[j].density*input->absorbers->exc_layers[j].thickness);
			}
			fprintf(filePtr, "%g %g %g\n", input->excitation->continuous[i].energy, blb*input->excitation->continuous[i].horizontal_intensity, blb*input->excitation->continuous[i].vertical_intensity);
		}
		fprintf(filePtr, "Resample 0\n");
	}


	fprintf(filePtr, "End\n");
	fclose(filePtr);
	return 1;
}

static int xmi_write_xrmc_samplefile(char *xrmc_samplefile, struct xmi_input *input) {
	FILE *filePtr;
	int i;
	
	if ((filePtr = fopen(xrmc_samplefile, "w")) == NULL) {
		fprintf(stderr, "Could not write to %s\n", xrmc_samplefile);
		return 0;
	}
	fprintf(filePtr, "Newdevice sample\n");
	fprintf(filePtr, "Sample\n");
	fprintf(filePtr, "SourceName Source\n");
	fprintf(filePtr, "Geom3DName Geom3D\n");
	fprintf(filePtr, "CompName Composition\n");
	fprintf(filePtr, "WeightedStepLength 0\n");
	fprintf(filePtr, "FluorFlag 1\n");
	fprintf(filePtr, "ScattOrderNum %i\n", input->general->n_interactions_trajectory);
	fprintf(filePtr, "0\n");
	for (i = 1 ; i <= input->general->n_interactions_trajectory ; i++) {
		fprintf(filePtr, "%i\n", 1);
	}

	fprintf(filePtr, "End\n");
	fclose(filePtr);
	return 1;
}

static int xmi_write_xrmc_quadricfile(char *xrmc_quadricfile, struct xmi_input *input) {
	FILE *filePtr;
	int i;

	double upper_normal[3], lower_normal[3], left_normal[3], right_normal[3], upside_normal[3], downside_normal[3];

	upper_normal[0] = input->geometry->n_sample_orientation[1];
	upper_normal[1] = input->geometry->n_sample_orientation[2];
	upper_normal[2] = input->geometry->n_sample_orientation[0];
	lower_normal[0] = -1.0*upper_normal[0];
	lower_normal[1] = -1.0*upper_normal[1];
	lower_normal[2] = -1.0*upper_normal[2];
	left_normal[0] = -1.0*fabs(upper_normal[1]);
	left_normal[1] = upper_normal[0];
	left_normal[2] = 0.0;
	right_normal[0] = fabs(upper_normal[1]);
	right_normal[1] = -1.0*upper_normal[0];
	right_normal[2] = 0.0;
	upside_normal[0] = 0.0;
	upside_normal[1] = 0.0;
	upside_normal[2] = 1.0;
	downside_normal[0] = 0.0;
	downside_normal[1] = 0.0;
	downside_normal[2] = -1.0;

	if ((filePtr = fopen(xrmc_quadricfile, "w")) == NULL) {
		fprintf(stderr, "Could not write to %s\n", xrmc_quadricfile);
		return 0;
	}
	fprintf(filePtr, "Newdevice quadricarray\n");
	fprintf(filePtr, "QuadricArray\n");

	//planes shared by all boxes
	fprintf(filePtr, "Plane Upside\n");
	fprintf(filePtr, "0 0 10000 %g %g %g\n", upside_normal[0], upside_normal[1], upside_normal[2]);
	fprintf(filePtr, "Plane Downside\n");
	fprintf(filePtr, "0 0 -10000 %g %g %g\n", downside_normal[0], downside_normal[1], downside_normal[2]);
	fprintf(filePtr, "Plane Rightside\n");
	fprintf(filePtr, "10000 0 0 %g %g %g\n", right_normal[0], right_normal[1], right_normal[2]);
	fprintf(filePtr, "Plane Leftside\n");
	fprintf(filePtr, "-10000 0 0 %g %g %g\n", left_normal[0], left_normal[1], left_normal[2]);


	//reference layer
	int reference_layer = input->composition->reference_layer-1;
	int counter = 0;

	fprintf(filePtr, "Plane Lower_%i\n", counter);
	fprintf(filePtr, "0 %g 0 %g %g %g\n", input->geometry->d_sample_source, lower_normal[0], lower_normal[1], lower_normal[2]);
	fprintf(filePtr, "Plane Higher_%i\n", counter);
	fprintf(filePtr, "%g %g %g %g %g %g\n", input->composition->layers[reference_layer].thickness*upper_normal[0],	
	input->geometry->d_sample_source+input->composition->layers[reference_layer].thickness*upper_normal[1],
	input->composition->layers[reference_layer].thickness*upper_normal[2],
	upper_normal[0], upper_normal[1], upper_normal[2]);
	counter++;

	double temp_point[3];
	temp_point[0] = 0.0;
	temp_point[1] = input->geometry->d_sample_source;
	temp_point[2] = 0.0;


	//before the reference_layer
	for (i = reference_layer-1 ; i >= 0 ; i--) {
		temp_point[0] += input->composition->layers[i].thickness*lower_normal[0];
		temp_point[1] += input->composition->layers[i].thickness*lower_normal[1]-1E-7;
		temp_point[2] += input->composition->layers[i].thickness*lower_normal[2];
		fprintf(filePtr, "Plane Lower_%i\n", counter);
		fprintf(filePtr, "%g %g %g %g %g %g\n",
		temp_point[0], temp_point[1], temp_point[2],
		lower_normal[0], lower_normal[1], lower_normal[2]);
		fprintf(filePtr, "Plane Higher_%i\n", counter);
		fprintf(filePtr, "%g %g %g %g %g %g\n",
		temp_point[0] + input->composition->layers[i].thickness*upper_normal[0],
		temp_point[1] + input->composition->layers[i].thickness*upper_normal[1],
		temp_point[2] + input->composition->layers[i].thickness*upper_normal[2],
		upper_normal[0], upper_normal[1], upper_normal[2]);
		counter++;
	}

	//behind the reference_layer
	temp_point[0] = input->composition->layers[reference_layer].thickness*upper_normal[0];
	temp_point[1] = input->composition->layers[reference_layer].thickness*upper_normal[1]+1E-7;
	temp_point[2] = input->composition->layers[reference_layer].thickness*upper_normal[2];
	for (i = reference_layer +1 ; i < input->composition->n_layers ; i++) {
		fprintf(filePtr, "Plane Lower_%i\n", counter);
		fprintf(filePtr, "%g %g %g %g %g %g\n",
		temp_point[0], temp_point[1], temp_point[2],
		lower_normal[0], lower_normal[1], lower_normal[2]);

		temp_point[0] += input->composition->layers[i].thickness*upper_normal[0];
		temp_point[1] += input->composition->layers[i].thickness*upper_normal[1];
		temp_point[2] += input->composition->layers[i].thickness*upper_normal[2];
		fprintf(filePtr, "Plane Higher_%i\n", counter);
		fprintf(filePtr, "%g %g %g %g %g %g\n",
		temp_point[0], temp_point[1], temp_point[2],
		upper_normal[0], upper_normal[1], upper_normal[2]);
		temp_point[1] += 1E-7;
		counter++;
	}

	//collimator code to follow...

	fprintf(filePtr, "End\n");
	fclose(filePtr);
	return 1;
}

static int xmi_write_xrmc_geom3dfile(char *xrmc_geom3dfile, struct xmi_input *input) {
	FILE *filePtr;
	int i;

	if ((filePtr = fopen(xrmc_geom3dfile, "w")) == NULL) {
		fprintf(stderr, "Could not write to %s\n", xrmc_geom3dfile);
		return 0;
	}
	fprintf(filePtr, "Newdevice geom3d\n");
	fprintf(filePtr, "Geom3D\n");
	fprintf(filePtr, "QArrName QuadricArray\n");
	fprintf(filePtr, "CompName Composition\n");

	//reference layer
	int reference_layer = input->composition->reference_layer-1;
	int counter = 0;
	fprintf(filePtr, "Object Box_%i\n", counter);
	fprintf(filePtr, "Comp_%i Vacuum\n6\n", counter);
	fprintf(filePtr, "Upside Downside Rightside Leftside Lower_%i Higher_%i\n", counter, counter);
	counter++;

	//before the reference layer
	for (i = reference_layer-1 ; i >= 0 ; i--) {
		fprintf(filePtr, "Object Box_%i\n", counter);
		fprintf(filePtr, "Comp_%i Vacuum\n6\n", counter);
		fprintf(filePtr, "Upside Downside Rightside Leftside Lower_%i Higher_%i\n", counter, counter);
		counter++;
	}

	//behind the reference_layer
	for (i = reference_layer +1 ; i < input->composition->n_layers ; i++) {
		fprintf(filePtr, "Object Box_%i\n", counter);
		fprintf(filePtr, "Comp_%i Vacuum\n6\n", counter);
		fprintf(filePtr, "Upside Downside Rightside Leftside Lower_%i Higher_%i\n", counter, counter);
		counter++;
	}
	//collimator code to follow...
	
	fprintf(filePtr, "End\n");
	fclose(filePtr);
	return 1;
}

static int xmi_write_xrmc_compositionfile(char *xrmc_compositionfile, struct xmi_input *input) {
	FILE *filePtr;
	int i,j;

	if ((filePtr = fopen(xrmc_compositionfile, "w")) == NULL) {
		fprintf(stderr, "Could not write to %s\n", xrmc_compositionfile);
		return 0;
	}
	fprintf(filePtr, "Newdevice composition\n");
	fprintf(filePtr, "Composition\n");

	//reference layer
	int reference_layer = input->composition->reference_layer-1;
	int counter = 0;
	fprintf(filePtr, "Phase Comp_%i\n", counter);
	fprintf(filePtr, "NElem %i\n", input->composition->layers[reference_layer].n_elements);
	for (j = 0 ; j < input->composition->layers[reference_layer].n_elements ; j++) {
		fprintf(filePtr, "%i %g\n", input->composition->layers[reference_layer].Z[j], input->composition->layers[reference_layer].weight[j]*100.0);
	}	
	fprintf(filePtr, "Rho %f\n", input->composition->layers[reference_layer].density);
	counter++;

	//before the reference layer
	for (i = reference_layer-1 ; i >= 0 ; i--) {
		fprintf(filePtr, "Phase Comp_%i\n", counter);
		fprintf(filePtr,"NElem %i\n", input->composition->layers[i].n_elements);
		for (j = 0 ; j < input->composition->layers[i].n_elements ; j++) {
			fprintf(filePtr, "%i %g\n", input->composition->layers[i].Z[j], input->composition->layers[i].weight[j]*100.0);
		}	
		fprintf(filePtr, "Rho %f\n", input->composition->layers[i].density);
		counter++;
	}

	//behind the reference_layer
	for (i = reference_layer +1 ; i < input->composition->n_layers ; i++) {
		fprintf(filePtr, "Phase Comp_%i\n", counter);
		fprintf(filePtr,"NElem %i\n", input->composition->layers[i].n_elements);
		for (j = 0 ; j < input->composition->layers[i].n_elements ; j++) {
			fprintf(filePtr, "%i %g\n", input->composition->layers[i].Z[j], input->composition->layers[i].weight[j]*100.0);
		}	
		fprintf(filePtr, "Rho %f\n", input->composition->layers[i].density);
		counter++;
	}

	//Crystal
	fprintf(filePtr, "Phase Crystal\n");
	fprintf(filePtr, "NElem %i\n", input->detector->crystal_layers[0].n_elements);
	for (j = 0 ; j < input->detector->crystal_layers[0].n_elements ; j++) {
		fprintf(filePtr, "%i %g\n", input->detector->crystal_layers[0].Z[j], input->detector->crystal_layers[0].weight[j]*100.0);
	}	
	fprintf(filePtr, "Rho %f\n", input->detector->crystal_layers[0].density);

	//Window
	if (input->absorbers->n_det_layers > 0) {
		//only first absorber taken into account for now...needs to be fixed in future versions
		fprintf(filePtr, "Phase Window\n");
		fprintf(filePtr, "NElem %i\n", input->absorbers->det_layers[0].n_elements);
		for (j = 0 ; j < input->absorbers->det_layers[0].n_elements ; j++) {
			fprintf(filePtr, "%i %g\n", input->absorbers->det_layers[0].Z[j], input->absorbers->det_layers[0].weight[j]*100.0);
		}	
		fprintf(filePtr, "Rho %f\n", input->absorbers->det_layers[0].density);
	}

	//collimator code to follow...
	
	fprintf(filePtr, "End\n");
	fclose(filePtr);
	return 1;
}

static int xmi_write_xrmc_detectorfile(char *xrmc_detectorfile, struct xmi_input *input, struct xmi_main_options options) {
	FILE *filePtr;

	if ((filePtr = fopen(xrmc_detectorfile, "w")) == NULL) {
		fprintf(stderr, "Could not write to %s\n", xrmc_detectorfile);
		return 0;
	}
	fprintf(filePtr, "Newdevice detectorconvolute\n");
	fprintf(filePtr, "Detector\n");
	fprintf(filePtr, "SourceName Sample\n");
	fprintf(filePtr, "CompositionName Composition\n");
	fprintf(filePtr, "NPixels 1 1\n");
	fprintf(filePtr, "PixelSize %g %g\n", sqrt(4.0*input->geometry->area_detector/M_PI), sqrt(4.0*input->geometry->area_detector/M_PI));
	fprintf(filePtr, "Shape 1\n");
	fprintf(filePtr, "X %g %g %g\n", input->geometry->p_detector_window[1], 
					 input->geometry->p_detector_window[2],
					 input->geometry->p_detector_window[0]);
	fprintf(filePtr, "uk %g %g %g\n", input->geometry->n_detector_orientation[1], 
					 input->geometry->n_detector_orientation[2],
					 input->geometry->n_detector_orientation[0]);
	fprintf(filePtr, "ui %g %g %g\n", 0.0, 0.0, 0.0); 
	fprintf(filePtr, "ExpTime %g\n", input->detector->live_time); 
	fprintf(filePtr, "PhotonNum %i\n", 100000); 
	fprintf(filePtr, "RandomPixelFlag 1\n");
	fprintf(filePtr, "PoissonFlag %i\n", options.use_poisson);
	fprintf(filePtr, "RoundFlag 0\n");
	fprintf(filePtr, "HeaderFlag 1\n");
	fprintf(filePtr, "ForceDetectFlag 1\n");
	fprintf(filePtr, "PixelType 2\n");
	fprintf(filePtr, "Emin %g\n", input->detector->zero);
	fprintf(filePtr, "Emax %g\n", input->detector->zero + input->detector->gain*(options.nchannels-1));
	fprintf(filePtr, "NBins %i\n", options.nchannels);
	fprintf(filePtr, "SaturateEmin 0\n");
	fprintf(filePtr, "SaturateEmax 0\n");
	fprintf(filePtr, "CrystalPhase Crystal\n");
	fprintf(filePtr, "CrystalThickness %g\n", input->detector->crystal_layers[0].thickness);

	if (input->absorbers->n_det_layers > 0) {
		fprintf(filePtr, "WindowPhase Window\n");
		fprintf(filePtr, "WindowThickness %g\n", input->absorbers->det_layers[0].thickness);
	}
	else {
		fprintf(filePtr, "WindowPhase Vacuum\n");
		fprintf(filePtr, "WindowThickness 0.0\n");
	}
	fprintf(filePtr, "FanoFactor %g\n", input->detector->fano);
	fprintf(filePtr, "Noise %g\n", input->detector->noise);
	if (options.use_sum_peaks) {
		fprintf(filePtr, "PulseWidth %g\n", input->detector->pulse_width);
	}

	fprintf(filePtr, "End\n");
	fclose(filePtr);
	return 1;
}

int xmi_copy_input_to_xrmc(struct xmi_input *input, char *xrmc_inputfile, char *xrmc_compositionfile, char *xrmc_detectorfile, char *xrmc_geom3dfile, char *xrmc_quadricfile, char *xrmc_samplefile, char *xrmc_sourcefile, char *xrmc_spectrumfile, char *xrmc_convolutedspectrumfile, char *xrmc_unconvolutedspectrumfile, struct xmi_layer *collimator, struct xmi_main_options options) {
	
	if (xmi_write_xrmc_inputfile(xrmc_inputfile, xrmc_compositionfile, xrmc_detectorfile, xrmc_geom3dfile, xrmc_quadricfile, xrmc_samplefile, xrmc_sourcefile, xrmc_spectrumfile, xrmc_convolutedspectrumfile, xrmc_unconvolutedspectrumfile) == 0) {
		fprintf(stderr, "Error in xmi_write_xrmc_inputfile: Aborting\n");
		return 0;
	}
	if (xmi_write_xrmc_sourcefile(xrmc_sourcefile, input) == 0) {
		fprintf(stderr, "Error in xmi_write_xrmc_sourcefile: Aborting\n");
		return 0;
	}
	if (xmi_write_xrmc_spectrumfile(xrmc_spectrumfile, input) == 0) {
		fprintf(stderr, "Error in xmi_write_xrmc_spectrumfile: Aborting\n");
		return 0;
	}
	if (xmi_write_xrmc_samplefile(xrmc_samplefile, input) == 0) {
		fprintf(stderr, "Error in xmi_write_xrmc_samplefile: Aborting\n");
		return 0;
	}
	if (xmi_write_xrmc_quadricfile(xrmc_quadricfile, input) == 0) {
		fprintf(stderr, "Error in xmi_write_xrmc_quadricfile: Aborting\n");
		return 0;
	}
	if (xmi_write_xrmc_geom3dfile(xrmc_geom3dfile, input) == 0) {
		fprintf(stderr, "Error in xmi_write_xrmc_geom3dfile: Aborting\n");
		return 0;
	}
	if (xmi_write_xrmc_compositionfile(xrmc_compositionfile, input) == 0) {
		fprintf(stderr, "Error in xmi_write_xrmc_compositionfile: Aborting\n");
		return 0;
	}
	if (xmi_write_xrmc_detectorfile(xrmc_detectorfile, input, options) == 0) {
		fprintf(stderr, "Error in xmi_write_xrmc_detectorfile: Aborting\n");
		return 0;
	}



	return 1;
}
