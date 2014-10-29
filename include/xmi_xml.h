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
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xmlwriter.h>

#ifdef __cplusplus
extern "C" {
#endif



//return 1 on success, 0 otherwise

//allocation of input occurs in function!
int xmi_read_input_xml(char *xmsifile, struct xmi_input **input); 
int xmi_write_input_xml(char *xmsifile, struct xmi_input *input);
int xmi_write_input_xml_to_string(char **xmlstring, struct xmi_input *input);
int xmi_write_output_xml(char *xmsofile, struct xmi_output *output);
int xmi_xmlfile_to_string(char *xmlfile, char **xmlstring, int *xmlstringlength);
int xmi_read_input_xml_from_string(char *xmsistring, struct xmi_input **input);
int xmi_xmlLoadCatalog(void);
int xmi_read_output_xml(char *xmsofile, struct xmi_output **output);
int xmi_read_archive_xml(char *xmsafile, struct xmi_archive **archive);
int xmi_write_archive_xml(char *xmsafile, struct xmi_archive *archive);


int xmi_read_input_layer(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_layer *layer);
int xmi_read_input_general(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_general **general);
int xmi_read_input_composition(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_composition **composition);
int xmi_read_input_geometry(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_geometry **geometry);
int xmi_read_input_excitation(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_excitation **excitation);
int xmi_read_input_absorbers(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_absorbers **absorbers);
int xmi_read_input_detector(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_detector **detector);
int xmi_read_input_spectrum(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_output *output, int conv);
int xmi_read_input_history(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_fluorescence_line_counts **history, int *nhistory);


int xmi_write_input_xml_body(xmlTextWriterPtr writer, struct xmi_input *input); 
int xmi_write_input_xml_svg(xmlTextWriterPtr writer, struct xmi_input *input, char *name, int interaction,  double *channels, double maximum); 
int xmi_write_output_xml_body(xmlTextWriterPtr writer, struct xmi_output *output, int step1, int step2, int with_svg);
int xmi_write_default_comments(xmlTextWriterPtr writer);
int xmi_read_input_xml_body(xmlDocPtr doc, xmlNodePtr subroot, struct xmi_input *input);
int xmi_read_output_xml_body(xmlDocPtr doc, xmlNodePtr subroot, struct xmi_output *output, int *step1, int *step2);
int xmi_write_layer_xml_body(xmlTextWriterPtr writer, struct xmi_layer *layers, int n_layers);
#ifdef __cplusplus
}
#endif

#endif
