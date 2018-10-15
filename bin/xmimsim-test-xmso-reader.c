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

#include "xmi_xml.h"


int main(int argc, char *argv[]) {

	char xmso_file[] = "nist.xmso";
	xmi_input *input;
	xmi_fluorescence_line_counts *brute_force_history;
	int nbrute_force_history;

	xmi_fluorescence_line_counts *var_red_history;
	int nvar_red_history;

	double **channels_conv;
	double **channels_unconv;
	int nchannels;

	char *xmsi_file;
	int use_zero_interactions;
	int n_interactions;

	xmi_xmlLoadCatalog();

	xmi_read_output_xml(	xmso_file,
				&input,
				&brute_force_history,
				&nbrute_force_history,
				&var_red_history,
				&nvar_red_history,
				&channels_conv,
				&channels_unconv,
				&nchannels,
				&n_interactions,
				&xmsi_file,
				&use_zero_interactions);



	return 0;
}


