#include <stdio.h>
#include "xmi_ebel.h"
#include <stdlib.h>


int main(int argc, char *argv[]) {


	double tube_voltage = 30.0;
	struct xmi_layer tube_anode, tube_window, tube_filter;
	double tube_current = 1.0;
	double tube_angle_electron = 90.0;
	double tube_angle_xray= 90.0;
	double tube_delta_energy = 0.1;

	struct xmi_energy *ebel_spectrum;
	int n_ebel_spectrum_cont;
	int n_ebel_spectrum_disc;
	int tube_transmission = 1;

	tube_anode.n_elements = 1;
	tube_anode.Z = malloc(sizeof(int));
	tube_anode.weight = malloc(sizeof(double));
	tube_anode.Z[0] = 47;
	tube_anode.weight[0] = 1.0;
	tube_anode.density = 10.500000;
	tube_anode.thickness= 0.000200;

	tube_window.n_elements = 1;
	tube_window.Z = malloc(sizeof(int));
	tube_window.weight = malloc(sizeof(double));
	tube_window.Z[0] = 4;
	tube_window.weight[0] = 1.0;
	tube_window.thickness = 0.0125;
	tube_window.density = 1.848;

	tube_filter.n_elements = 1;
	tube_filter.Z = malloc(sizeof(int));
	tube_filter.weight = malloc(sizeof(double));
	tube_filter.Z[0] = 10;
	tube_filter.weight[0] = 1.0;
	tube_filter.thickness = 2.00000;
	tube_filter.density = 0.0009;




	xmi_tube_ebel(&tube_anode, &tube_window, &tube_filter, tube_voltage, tube_current, tube_angle_electron, tube_angle_xray,
	tube_delta_energy, tube_transmission, &ebel_spectrum, &n_ebel_spectrum_cont, &n_ebel_spectrum_disc);

	int i; 

	for (i = 0 ; i < n_ebel_spectrum_cont+n_ebel_spectrum_disc ; i++)
		fprintf(stdout, "%lf %lf %lf %lf %lf %lf %lf\n", ebel_spectrum[i].energy, 
			ebel_spectrum[i].horizontal_intensity,
			ebel_spectrum[i].vertical_intensity,
			ebel_spectrum[i].sigma_x,
			ebel_spectrum[i].sigma_y,
			ebel_spectrum[i].sigma_xp,
			ebel_spectrum[i].sigma_yp);

	return 0;

}
