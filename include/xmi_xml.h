#ifndef XMI_XML_H
#define XMI_XML_H

#include "xmi_data_structs.h"


//return 1 on success, 0 otherwise

//allocation of input occurs in function!
int xmi_read_input_xml(char *xmlfile, struct xmi_input **input); 

int xmi_write_input_xml(char *xmlfile, struct xmi_input *input);

int xmi_write_output_xml(char *xmlfile, struct xmi_input *input, long int *brute_history, double *var_red_history, double **channels_conv, double *channels_unconv, int nchannels, char *inputfile, int use_zero_interactions );

int xmi_xmlfile_to_string(char *xmlfile, char **xmlstring, int *xmlstringlength);


#endif
