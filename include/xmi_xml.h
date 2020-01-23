/*
Copyright (C) 2010-2020 Tom Schoonjans and Laszlo Vincze

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
#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif



//return 1 on success, 0 otherwise

//allocation of input occurs in function!
xmi_input* xmi_input_read_from_xml_file(const char *xmsifile, GError **error);
gboolean xmi_input_write_to_xml_file(xmi_input *input, const char *xmsifile, GError **error);
gboolean xmi_input_write_to_xml_string(xmi_input *input, char **xmlstring, GError **error);
gboolean xmi_output_write_to_xml_file(xmi_output *output, const char *xmsofile, GError **error);
xmi_input* xmi_input_read_from_xml_string(const char *xmsistring, GError **error);
gboolean xmi_xmlLoadCatalog(GError **error);
xmi_output* xmi_output_read_from_xml_file(const char *xmsofile, GError **error);
xmi_archive* xmi_archive_read_from_xml_file(const char *xmsafile, GError **error);
gboolean xmi_archive_write_to_xml_file(xmi_archive *archive, const char *xmsafile, GError **error);

gboolean xmi_write_input_xml_body(xmlDocPtr doc, xmlNodePtr node, xmi_input *input, GError **error);
gboolean xmi_write_input_xml_svg(xmlDocPtr doc, xmlNodePtr node, xmi_input *input, char *name, int interaction,  double *channels, double maximum, GError **error);
gboolean xmi_write_output_xml_body(xmlDocPtr doc, xmlNodePtr subroot, xmi_output *output, GArray *steps, int with_svg, GError **error);
gboolean xmi_write_default_comments(xmlDocPtr doc, xmlNodePtr root_node, GError **error);
gboolean xmi_write_layer_xml_body(xmlDocPtr doc, xmlNodePtr node, xmi_layer *layers, int n_layers, GError **error);
gboolean xmi_read_input_xml_body(xmlDocPtr doc, xmlNodePtr node, xmi_input *input, GError **error);
gboolean xmi_read_output_xml_body(xmlDocPtr doc, xmlNodePtr root, xmi_output *op, GArray *steps, GError **error);

#ifdef __cplusplus
}
#endif

#endif
