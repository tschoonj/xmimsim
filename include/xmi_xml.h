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

#ifndef XMI_XML_H
#define XMI_XML_H

#include "xmi_data_structs.h"


struct xmi_counts {
	double counts;
	int interaction_number;
};


struct xmi_fluorescence_line {
	char line_type[10];
	double energy;
	double total_counts;
	int n_interactions;
	struct xmi_counts *interactions;
};

struct xmi_fluorescence_line_counts {
	int atomic_number;
	double total_counts;
	int n_lines;
	struct xmi_fluorescence_line *lines;
};



//return 1 on success, 0 otherwise

//allocation of input occurs in function!
int xmi_read_input_xml(char *xmlfile, struct xmi_input **input); 

int xmi_write_input_xml(char *xmlfile, struct xmi_input *input);

int xmi_write_input_xml_to_string(char **xmlstring, struct xmi_input *input);

int xmi_write_output_xml(char *xmlfile, struct xmi_input *input, double *brute_history, double *var_red_history, double **channels_conv, double *channels_unconv, int nchannels, char *inputfile, int use_zero_interactions );

int xmi_xmlfile_to_string(char *xmlfile, char **xmlstring, int *xmlstringlength);

int xmi_read_input_xml_from_string(char *xmlstring, struct xmi_input **input);

int xmi_xmlLoadCatalog(void);

int xmi_read_output_xml(	char *xmsofile,
				struct xmi_input **input,
				struct xmi_fluorescence_line_counts **brute_force_history,
				int *nbrute_force_history,
				struct xmi_fluorescence_line_counts **var_red_history,
				int *nvar_red_history,
				double ***channels_conv,
				double ***channels_unconv,
				int *nchannels,
				int *ninteractions,
				char **inputfile,
				int *use_zero_interactions);

void xmi_free_fluorescence_line_counts(struct xmi_fluorescence_line_counts *history);


#endif
