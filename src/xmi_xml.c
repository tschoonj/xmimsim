/*
Copyright (C) 2010-2017 Tom Schoonjans, Philip Brondeel and Laszlo Vincze

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
#include "xmi_xml.h"
#include "xmi_aux.h"
#include "xmi_lines.h"
#include "xmi_private.h"
#include "xmi_error.h"
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/catalog.h>
#include <libxml/globals.h>
#include <libxml/xpath.h>
#include <math.h>
#include <glib.h>
#include <search.h>

#ifdef G_OS_WIN32
#include "xmi_registry_win.h"
#elif defined(MAC_INTEGRATION)
#include "xmi_resources_mac.h"
#endif

#define SVG_DEFAULT_WIDTH 540
#define SVG_DEFAULT_HEIGHT 300
#define SVG_DEFAULT_BOX_WIDTH 500
#define SVG_DEFAULT_BOX_HEIGHT 250
#define SVG_DEFAULT_BOX_OFFSET_X 39
#define SVG_DEFAULT_BOX_OFFSET_Y 10


#define SVG_ENERGY_TO_SVG_COORDS(energy) (((SVG_DEFAULT_BOX_WIDTH)*(energy-channels[0])/(energies[nchannels-1]-energies[0]))+SVG_DEFAULT_BOX_OFFSET_X)

#define SVG_INTENSITY_TO_SVG_COORDS(intensity) ((SVG_DEFAULT_BOX_OFFSET_Y+SVG_DEFAULT_BOX_HEIGHT-2)+(5+2-SVG_DEFAULT_BOX_HEIGHT)*(log10(intensity)-minimum_log)/(maximum_log-minimum_log))

static int xmi_read_input_layer(xmlDocPtr doc, xmlNodePtr nodePtr, xmi_layer *layer, GError **error);
static int xmi_read_input_general(xmlDocPtr doc, xmlNodePtr nodePtr, xmi_general **general, GError **error);
static int xmi_read_input_composition(xmlDocPtr doc, xmlNodePtr nodePtr, xmi_composition **composition, GError **error);
static int xmi_read_input_geometry(xmlDocPtr doc, xmlNodePtr nodePtr, xmi_geometry **geometry, GError **error);
static int xmi_read_input_excitation(xmlDocPtr doc, xmlNodePtr nodePtr, xmi_excitation **excitation, GError **error);
static int xmi_read_input_absorbers(xmlDocPtr doc, xmlNodePtr nodePtr, xmi_absorbers **absorbers, GError **error);
static int xmi_read_input_detector(xmlDocPtr doc, xmlNodePtr nodePtr, xmi_detector **detector, GError **error);
static int xmi_read_output_spectrum(xmlDocPtr doc, xmlNodePtr nodePtr, xmi_output *output, int conv, GError **error);
static int xmi_read_output_history(xmlDocPtr doc, xmlNodePtr nodePtr, xmi_fluorescence_line_counts **history, int *nhistory, GError **error);

#ifndef QUICKLOOK
static float e2c(double energy, double * channels, double * energies, int nchannels );
static float i2c(double intensity, double maximum_log, double minimum_log);

static xmlNodePtr xmi_new_child_printf(xmlNodePtr nodePtr, const xmlChar *node_name, const gchar *message_format, ...) G_GNUC_PRINTF(3, 4);
static void xmi_new_prop_printf(xmlNodePtr nodePtr, const xmlChar *prop_name, const gchar *message_format, ...) G_GNUC_PRINTF(3, 4);
static xmlNodePtr xmi_new_child_with_index_printf(xmlNodePtr nodePtr, const xmlChar *node_name, unsigned int _index, const gchar *message_format, ...) G_GNUC_PRINTF(4, 5);
static void xmi_new_prop_with_index_printf(xmlNodePtr nodePtr, const xmlChar *prop_name, unsigned int _index, const gchar *message_format, ...) G_GNUC_PRINTF(4, 5);
#endif

static gboolean xml_catalog_loaded = FALSE;

static void handle_error(GError **error) {
	xmlErrorPtr _xmlError = xmlGetLastError();
	if (_xmlError) {
		g_set_error(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "%s", _xmlError->message);
	}
	else {
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "unknown error occurred");
	}
}

static gboolean xmi_xmlCatalogAdd(const gchar *path, GError **error) {
	const xmlChar uriStartString[] = "http://www.xmi.UGent.be/xml/";
	char *rewritePrefix = g_filename_to_uri(path, NULL, error);

	if (rewritePrefix == NULL)
		return FALSE;

	if (xmlCatalogAdd(BAD_CAST "catalog", NULL, NULL) == -1) {
		handle_error(error);
		return FALSE;
	}

	if (xmlCatalogAdd(BAD_CAST "rewriteURI", BAD_CAST uriStartString, BAD_CAST rewritePrefix) == -1) {
		handle_error(error);
		return FALSE;
	}

	g_free(rewritePrefix);

	return TRUE;
}

gboolean xmi_xmlLoadCatalog(GError **error) {
	gboolean rv = FALSE;

	if (xml_catalog_loaded)
		return TRUE;

	// look first for environment variable
	const gchar *xmi_catalog_path = g_getenv("XMI_CATALOG_PATH");
	if (xmi_catalog_path != NULL) {
		return xmi_xmlCatalogAdd(xmi_catalog_path, error);
	}

#ifdef G_OS_WIN32

	char *share;

	if (xmi_registry_win_query(XMI_REGISTRY_WIN_SHARE, &share) == 0) {
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "Could not get Share folder location from registry");
		return FALSE;
	}

	rv = xmi_xmlCatalogAdd(share, error);
	g_free(share);

#elif defined(QUICKLOOK)
// this function is not required for the quicklook plugin
#elif defined(MAC_INTEGRATION)

	gchar *resource_path = xmi_application_get_resource_path();
	if (resource_path == NULL) {
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "Could not get resource path from bunle");
		return FALSE;
	}

	GString *resource_path_string = g_string_new(resource_path);
	g_free(resource_path);
	g_string_append(resource_path_string, "/");

	rv = xmi_xmlCatalogAdd(resource_path_string->str, error);

	g_string_free(resource_path_string, TRUE);

#else
	char catalog[] = XMI_CATALOG;

	if (xmlLoadCatalog(catalog) != 0) {
		handle_error(error);
		rv = FALSE;
	}
	else
		rv = TRUE;

#endif

	if (rv == TRUE)
		xml_catalog_loaded = TRUE;

	return rv;
}

static int xmi_read_output_spectrum(xmlDocPtr doc, xmlNodePtr spectrumPtr, xmi_output *output, int conv, GError **error) {
	double **channels_loc;
	int channel_loc;
	xmlNodePtr channelPtr, countsPtr;
	int n_interactions_loc, interaction_loc;
	xmlChar *txt;
	xmlAttrPtr attr;
	int i;
	int nchannels;


	//count number of channels
	nchannels = (int) xmlChildElementCount(spectrumPtr);
	if (nchannels < 1 ) {
		fprintf(stderr,"xmi_read_output_spectrum: spectrum contains no channels\n");
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "spectrum contains no channels");
		return 0;
	}

	channels_loc = g_malloc0(sizeof(double *)* (output->ninteractions+1));
	for (i = 0 ; i <= output->ninteractions ; i++)
		channels_loc[i] = g_malloc0(nchannels * sizeof(double));

	if (conv)
		output->channels_conv = channels_loc;
	else
		output->channels_unconv = channels_loc;

	channel_loc = 0;
	channelPtr = xmlFirstElementChild(spectrumPtr);

	while (channelPtr != NULL) {
		if (!xmlStrcmp(channelPtr->name,(const xmlChar *) "channel")) {
			n_interactions_loc = (int) xmlChildElementCount(channelPtr);
			if (n_interactions_loc < 3) {
				fprintf(stderr,"xmi_read_output_spectrum: channel contains less than three entities\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "channel contains less than three entities");
				return 0;
			}
			countsPtr = xmlFirstElementChild(channelPtr);
			interaction_loc = 0;
			while (countsPtr != NULL) {
				if (!xmlStrcmp(countsPtr->name,(const xmlChar *) "counts")) {
					attr = countsPtr->properties;
					txt = xmlNodeGetContent(attr->children);
					interaction_loc = (int) strtol((char *) txt, NULL, 10);
					xmlFree(txt);
					txt = xmlNodeGetContent(countsPtr->children);
					if (sscanf((const char*) txt, "%lg", &(channels_loc[interaction_loc][channel_loc])) !=1) {
						fprintf(stderr,"xmi_read_output_spectrum: could not read counts\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "could not read counts");
						return 0;
					}
					xmlFree(txt);
				}
				countsPtr = xmlNextElementSibling(countsPtr);
			}
			channel_loc++;
		}
		channelPtr = xmlNextElementSibling(channelPtr);
	}

	return 1;
}

static int xmi_read_output_history(xmlDocPtr doc, xmlNodePtr nodePtr, xmi_fluorescence_line_counts **history, int *nhistory, GError **error) {

	//assume history will be a NULL terminated array...
	//count children
	unsigned int nchildren;
	xmlChar *txt;
	xmlNodePtr linePtr, countsPtr, subcountsPtr;
	xmi_fluorescence_line_counts *history_loc;
	xmlAttrPtr attr;
	int counter, counter2, counter3;

	nchildren = xmlChildElementCount(nodePtr);
	if (nchildren == 0) {
		*nhistory = 0;
		*history = NULL;
		return 1;
	}

	//malloc required memory
	*history = (xmi_fluorescence_line_counts *) g_malloc0(sizeof(xmi_fluorescence_line_counts)*(nchildren));
	history_loc = *history;
	linePtr = xmlFirstElementChild(nodePtr);
	*nhistory = nchildren;


	counter = 0;
	while(linePtr != NULL) {
		//read attributes
		attr = linePtr->properties;
		while (attr != NULL) {
			if (!xmlStrcmp(attr->name,(const xmlChar *) "atomic_number")) {
				txt =xmlNodeGetContent(attr->children);
				if(sscanf((const char *)txt,"%i",&(history_loc[counter].atomic_number)) != 1) {
					fprintf(stderr,"xmi_read_output_history: error reading in atomic_number\n");
					g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in atomic number");
					return 0;
				}
				xmlFree(txt);
			}
			else if (!xmlStrcmp(attr->name,(const xmlChar *) "total_counts")) {
				txt =xmlNodeGetContent(attr->children);
				if(sscanf((const char *)txt,"%lg",&(history_loc[counter].total_counts)) != 1) {
					fprintf(stderr,"xmi_read_output_history: error reading in total_counts\n");
					g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in total_counts");
					return 0;
				}
				xmlFree(txt);
			}


			attr = attr->next;
		}
		//determine number of children
		nchildren = xmlChildElementCount(linePtr);
		history_loc[counter].n_lines = nchildren;
		history_loc[counter].lines = (xmi_fluorescence_line *) g_malloc0(sizeof(xmi_fluorescence_line)*nchildren);

		countsPtr = xmlFirstElementChild(linePtr);
		counter2 = 0;
		while (countsPtr) {
			attr = countsPtr->properties;
			while (attr) {
				if (!xmlStrcmp(attr->name,(const xmlChar *) "type")) {
					txt =xmlNodeGetContent(attr->children);
					if (txt == NULL) {
						fprintf(stderr,"xmi_read_output_history: error reading in line_type\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in line_type");
						return 0;
					}
					history_loc[counter].lines[counter2].line_type = g_strdup((gchar*) txt);
					xmlFree(txt);
				}
				else if (!xmlStrcmp(attr->name,(const xmlChar *) "energy")) {
					txt =xmlNodeGetContent(attr->children);
					if(sscanf((const char *)txt,"%lg",&(history_loc[counter].lines[counter2].energy)) != 1) {
						fprintf(stderr,"xmi_read_output_history: error reading in energy\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in energy");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(attr->name,(const xmlChar *) "total_counts")) {
					txt =xmlNodeGetContent(attr->children);
					if(sscanf((const char *)txt,"%lg",&(history_loc[counter].lines[counter2].total_counts)) != 1) {
						fprintf(stderr,"xmi_read_output_history: error reading in total_counts lvl2\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in total_counts");
						return 0;
					}
					xmlFree(txt);
				}

				attr = attr->next;
			}
			counter3 = 0;
			subcountsPtr = xmlFirstElementChild(countsPtr);
			nchildren = xmlChildElementCount(countsPtr);
			history_loc[counter].lines[counter2].interactions = (xmi_counts *) g_malloc0(sizeof(xmi_counts)*nchildren);
			history_loc[counter].lines[counter2].n_interactions = nchildren;
			while (subcountsPtr) {
				attr = subcountsPtr->properties;
				while (attr) {
					if (!xmlStrcmp(attr->name,(const xmlChar *) "interaction_number")) {
						txt =xmlNodeGetContent(attr->children);
						if(sscanf((const char *)txt,"%i",&(history_loc[counter].lines[counter2].interactions[counter3].interaction_number)) != 1) {
							fprintf(stderr,"xmi_read_output_history: error reading in interaction_number\n");
							g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in interaction_number");
							return 0;
						}
						xmlFree(txt);
					}
					attr = attr->next;
				}
				txt = xmlNodeGetContent(subcountsPtr->children);
				if (sscanf((const char*) txt, "%lg",&(history_loc[counter].lines[counter2].interactions[counter3].counts)) !=1) {
					fprintf(stderr,"xmi_read_output_history: could not read counts\n");
					g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in counts");
					return 0;
				}
				xmlFree(txt);

				subcountsPtr = xmlNextElementSibling(subcountsPtr);
				counter3++;
			}

			counter2++;
			countsPtr = xmlNextElementSibling(countsPtr);
		}
		counter++;
		linePtr = xmlNextElementSibling(linePtr);
	}

	return 1;
}

static int xmi_read_input_general(xmlDocPtr doc, xmlNodePtr node, xmi_general **general, GError **error) {
	xmlNodePtr subnode;
	xmlChar *txt;
	xmlAttrPtr attr;

	//allocate memory
	*general = (xmi_general *) g_malloc0(sizeof(xmi_general));



	//version
	attr=node->properties;
	while (attr != NULL) {
		if (!xmlStrcmp(attr->name,(const xmlChar *) "version")) {
			txt =xmlNodeGetContent(attr->children);
			if(sscanf((const char *)txt,"%f",&((*general)->version)) != 1) {
				fprintf(stderr,"error reading in version of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in version of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		attr=attr->next;
	}

	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "outputfile")) {
			//outputfile
			txt = xmlNodeGetContent(subnode->children);
			if (txt == NULL) {
				(*general)->outputfile = g_strdup("");
			}
			else {
				(*general)->outputfile = g_strdup((char *) txt);
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_photons_interval")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%li",&((*general)->n_photons_interval)) != 1) {
				fprintf(stderr,"error reading in n_photons_interval of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in n_photons_interval of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_photons_line")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%li",&((*general)->n_photons_line)) != 1) {
				fprintf(stderr,"error reading in n_photons_line of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in n_photons_line of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_interactions_trajectory")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%i",&((*general)->n_interactions_trajectory)) != 1) {
				fprintf(stderr,"error reading in n_interactions_trajectory of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in n_interactions_trajectory of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "comments")) {
			//comments
			txt = xmlNodeGetContent(subnode->children);
			if (txt == NULL) {
				(*general)->comments = g_strdup("");
			}
			else {
				(*general)->comments = g_strdup((char *) txt);
			}
			xmlFree(txt);
		}
		subnode=xmlNextElementSibling(subnode);
	}

	return 1;

}

static int xmi_read_input_composition(xmlDocPtr doc, xmlNodePtr node, xmi_composition **composition, GError **error) {
	xmlNodePtr subnode;
	xmlChar *txt;

	//allocate memory
	*composition = (xmi_composition *) g_malloc0(sizeof(xmi_composition));

	(*composition)->n_layers = 0;
	(*composition)->layers = NULL;

	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "layer")){
			(*composition)->layers = (xmi_layer *) g_realloc((*composition)->layers,sizeof(xmi_layer)*++((*composition)->n_layers));
			//long live C and its deliciously complicated syntax :-)
			if (xmi_read_input_layer(doc, subnode, (*composition)->layers+(*composition)->n_layers-1, error) == 0) {
				return 0;
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "reference_layer")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%i",&((*composition)->reference_layer)) != 1) {
				fprintf(stderr,"error reading in reference_layer of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in reference_layer of xml file");
				return 0;
			}
			xmlFree(txt);
			if ((*composition)->reference_layer < 1 || (*composition)->reference_layer > (*composition)->n_layers) {
				fprintf(stderr,"invalid reference_layer value detected\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "invalid reference_layer value detected");
				return 0;
			}
		}
		subnode= xmlNextElementSibling(subnode);
	}

	return 1;
}

static int xmi_read_input_geometry(xmlDocPtr doc, xmlNodePtr node, xmi_geometry **geometry, GError **error) {
	xmlNodePtr subnode,subsubnode;
	xmlChar *txt;


	//allocate memory
	*geometry= (xmi_geometry *) g_malloc0(sizeof(xmi_geometry));

	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "d_sample_source")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->d_sample_source)) != 1) {
				fprintf(stderr,"error reading in d_sample_source of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in d_sample_source of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_sample_orientation")) {
			subsubnode = xmlFirstElementChild(subnode);
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "x")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_sample_orientation[0])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation x of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in n_sample_orientation x of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_sample_orientation[1])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation y of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in n_sample_orientation y of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_sample_orientation[2])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation y of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in n_sample_orientation z of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}

		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "p_detector_window")) {
			subsubnode = xmlFirstElementChild(subnode);
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "x")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->p_detector_window[0])) != 1) {
						fprintf(stderr,"error reading in p_detector_window x of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in p_detector_window x of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->p_detector_window[1])) != 1) {
						fprintf(stderr,"error reading in p_detector_window y of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in p_detector_window y of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->p_detector_window[2])) != 1) {
						fprintf(stderr,"error reading in p_detector_window z of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in p_detector_window z of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_detector_orientation")) {
			subsubnode = xmlFirstElementChild(subnode);
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "x")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_detector_orientation[0])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation x of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in n_detector_orientation x of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_detector_orientation[1])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation y of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in n_detector_orientation y of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_detector_orientation[2])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation z of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in n_detector_orientation z of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "area_detector")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->area_detector)) != 1) {
				fprintf(stderr,"error reading in area_detector of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in area_detector of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "collimator_height")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->collimator_height)) != 1) {
				fprintf(stderr,"error reading in collimator_height of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in collimator_height of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "collimator_diameter")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->collimator_diameter)) != 1) {
				fprintf(stderr,"error reading in collimator_diameter of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in collimator_diameter of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "d_source_slit")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->d_source_slit)) != 1) {
				fprintf(stderr,"error reading in d_source_slit of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in d_source_slit of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "slit_size")) {
			subsubnode = xmlFirstElementChild(subnode);
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "slit_size_x")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->slit_size_x)) != 1) {
						fprintf(stderr,"error reading in slit_size_x of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in slit_size_x of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "slit_size_y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->slit_size_y)) != 1) {
						fprintf(stderr,"error reading in slit_size_y of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in slit_size_y of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}
		}
		subnode = xmlNextElementSibling(subnode);
	}

#ifndef QUICKLOOK
	//normalize vectors to avoid problems later on...
	xmi_normalize_vector_double((*geometry)->n_sample_orientation, 3);
	xmi_normalize_vector_double((*geometry)->n_detector_orientation, 3);
#endif
	return 1;
}

static int xmi_read_input_excitation(xmlDocPtr doc, xmlNodePtr node, xmi_excitation **excitation, GError **error) {
	xmlNodePtr subnode,subsubnode;
	xmlChar *txt;
	xmlAttrPtr attr;
	double energy;
	double horizontal_intensity;
	double vertical_intensity;
	double sigma_x;
	double sigma_xp;
	double sigma_y;
	double sigma_yp;
	xmi_energy_discrete xed;
	xmi_energy_continuous xec;
	int distribution_type = XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC;
	double scale_parameter = 0.0;

	xmi_excitation *excitation_rv = g_malloc0(sizeof(xmi_excitation));

	excitation_rv->n_discrete = 0;
	excitation_rv->discrete = NULL;
	excitation_rv->n_continuous = 0;
	excitation_rv->continuous = NULL;

	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar*) "discrete")) {
			subsubnode = xmlFirstElementChild(subnode);
			distribution_type = XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC;
			scale_parameter = 0.0;
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "energy")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(energy)) != 1) {
						fprintf(stderr,"error reading in discrete energy of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in discrete energy of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "horizontal_intensity")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(horizontal_intensity)) != 1) {
						fprintf(stderr,"error reading in discrete horizontal_intensity of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in discrete horizontal_intensity of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "vertical_intensity")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(vertical_intensity)) != 1) {
						fprintf(stderr,"error reading in discrete vertical_intensity of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in discrete vertical_intensity of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_x")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_x)) != 1) {
						fprintf(stderr,"error reading in sigma_x of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in discrete sigma_x of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_xp")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_xp)) != 1) {
						fprintf(stderr,"error reading in sigma_xp of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in discrete sigma_xp of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_y)) != 1) {
						fprintf(stderr,"error reading in sigma_y of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in discrete sigma_y of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_yp")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_yp)) != 1) {
						fprintf(stderr,"error reading in sigma_yp of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in discrete sigma_yp of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "scale_parameter")) {
					//get distribution type
					attr=subsubnode->properties;
					while (attr != NULL) {
						if (!xmlStrcmp(attr->name,(const xmlChar *) "distribution_type")) {
							txt = xmlNodeGetContent(attr->children);
							if (xmlStrcmp(txt, BAD_CAST "monochromatic") == 0) {
								distribution_type = XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC;
								xmlFree(txt);
							}
							else if (xmlStrcmp(txt, BAD_CAST "gaussian") == 0) {
								distribution_type = XMI_ENERGY_DISCRETE_DISTRIBUTION_GAUSSIAN;
								xmlFree(txt);
								//read scale_parameter value
								txt = xmlNodeGetContent(subsubnode->children);
								if(sscanf((const char *)txt,"%lg",&scale_parameter) != 1) {
									fprintf(stderr,"error reading in scale_parameter of xml file\n");
									g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in scale_parameter of xml file");
									return 0;
								}
								xmlFree(txt);
							}
							else if (xmlStrcmp(txt, BAD_CAST "lorentzian") == 0) {
								distribution_type = XMI_ENERGY_DISCRETE_DISTRIBUTION_LORENTZIAN;
								xmlFree(txt);
								//read scale_parameter value
								txt = xmlNodeGetContent(subsubnode->children);
								if(sscanf((const char *)txt,"%lg",&scale_parameter) != 1) {
									fprintf(stderr,"error reading in scale_parameter of xml file\n");
									g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in scale_parameter of xml file");
									return 0;
								}
								xmlFree(txt);
							}
						}
						attr=attr->next;
					}

				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}
			xed.energy = energy;
			xmi_energy_discrete *xed_match;
#ifdef G_OS_WIN32
			unsigned int n_discrete = excitation_rv->n_discrete;
			if ((xed_match = _lfind(&xed, excitation_rv->discrete, &n_discrete, sizeof(xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete)) != NULL)
#else
			size_t n_discrete = excitation_rv->n_discrete;
			if ((xed_match = lfind(&xed, excitation_rv->discrete, &n_discrete, sizeof(xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete)) != NULL)
#endif
				{
				fprintf(stderr,"Warning: Duplicate discrete line energy detected\nAdding to existing discrete line\n");
				if (energy > 0.0 && energy <= 200.0 && horizontal_intensity >= 0.0 && vertical_intensity >= 0.0 && (horizontal_intensity + vertical_intensity) > 0.0) {
					xed_match->horizontal_intensity += horizontal_intensity;
					xed_match->vertical_intensity += vertical_intensity;
				}
				else {
					fprintf(stderr,"Error: Invalid discrete energy detected\n");
					g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "invalid discrete energy detected");
					return 0;
				}
			}
			else if (energy > 0.0 && energy <= 200.0 && horizontal_intensity >= 0.0 && vertical_intensity >= 0.0 && (horizontal_intensity + vertical_intensity) > 0.0) {
				excitation_rv->discrete = (xmi_energy_discrete *) g_realloc(excitation_rv->discrete,++(excitation_rv->n_discrete)*sizeof(xmi_energy_discrete));
				excitation_rv->discrete[excitation_rv->n_discrete-1].energy= energy ;
				excitation_rv->discrete[excitation_rv->n_discrete-1].horizontal_intensity = horizontal_intensity;
				excitation_rv->discrete[excitation_rv->n_discrete-1].vertical_intensity = vertical_intensity;
				excitation_rv->discrete[excitation_rv->n_discrete-1].sigma_x = sigma_x;
				excitation_rv->discrete[excitation_rv->n_discrete-1].sigma_xp = sigma_xp;
				excitation_rv->discrete[excitation_rv->n_discrete-1].sigma_y = sigma_y;
				excitation_rv->discrete[excitation_rv->n_discrete-1].sigma_yp = sigma_yp;
				excitation_rv->discrete[excitation_rv->n_discrete-1].scale_parameter = scale_parameter;
				excitation_rv->discrete[excitation_rv->n_discrete-1].distribution_type = distribution_type;
			}
			else {
				fprintf(stderr,"Error: Invalid discrete energy detected\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "invalid discrete energy detected");
				return 0;
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "continuous")) {
			subsubnode = xmlFirstElementChild(subnode);
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "energy")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(energy)) != 1) {
						fprintf(stderr,"error reading in continuous energy of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in continuous energy of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "horizontal_intensity")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(horizontal_intensity)) != 1) {
						fprintf(stderr,"error reading in continuous horizontal_intensity of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in continuous horizontal_intensity of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "vertical_intensity")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(vertical_intensity)) != 1) {
						fprintf(stderr,"error reading in continuous vertical_intensity of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in continuous vertical_intensity of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_x")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_x)) != 1) {
						fprintf(stderr,"error reading in sigma_x of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in sigma_x of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_xp")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_xp)) != 1) {
						fprintf(stderr,"error reading in sigma_xp of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in sigma_xp of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_y)) != 1) {
						fprintf(stderr,"error reading in sigma_y of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in sigma_y of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_yp")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_yp)) != 1) {
						fprintf(stderr,"error reading in sigma_yp of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in sigma_yp of xml file");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}
			xec.energy = energy;
#ifdef G_OS_WIN32
			unsigned int n_continuous = excitation_rv->n_continuous;
			if (_lfind(&xec, excitation_rv->continuous, &n_continuous, sizeof(xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
#else
			size_t n_continuous = excitation_rv->n_continuous;
			if (lfind(&xec, excitation_rv->continuous, &n_continuous, sizeof(xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
#endif
				fprintf(stderr,"Error: Duplicate continuous energy interval energy detected\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "Duplicate continuous energy interval energy detected");
				return 0;
			}
			else if (energy >= 0.0 && energy <= 200.0 && horizontal_intensity >= 0.0 && vertical_intensity >= 0.0 && (horizontal_intensity + vertical_intensity) >= 0.0) {
				excitation_rv->continuous = (xmi_energy_continuous *) g_realloc(excitation_rv->continuous,++(excitation_rv->n_continuous)*sizeof(xmi_energy_continuous));
				excitation_rv->continuous[excitation_rv->n_continuous-1].energy= energy ;
				excitation_rv->continuous[excitation_rv->n_continuous-1].horizontal_intensity = horizontal_intensity;
				excitation_rv->continuous[excitation_rv->n_continuous-1].vertical_intensity = vertical_intensity;
				excitation_rv->continuous[excitation_rv->n_continuous-1].sigma_x = sigma_x;
				excitation_rv->continuous[excitation_rv->n_continuous-1].sigma_xp = sigma_xp;
				excitation_rv->continuous[excitation_rv->n_continuous-1].sigma_y = sigma_y;
				excitation_rv->continuous[excitation_rv->n_continuous-1].sigma_yp = sigma_yp;
			}
			else {
				fprintf(stderr,"Error: Invalid continuous interval detected\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "Invalid continuous energy interval energy detected");
				return 0;
			}
		}
		subnode = xmlNextElementSibling(subnode);
	}

	if (excitation_rv->n_continuous < 2 && excitation_rv->n_discrete == 0) {
		fprintf(stderr,"Error: Found no valid discrete or continuous energies in xml file\n");
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "Found no valid discrete or continuous energies in xml file");
		return 0;
	}
	else if (excitation_rv->n_continuous == 1) {
		fprintf(stderr,"Error: Found only one continuous interval in xml file\nMust be either none or at least two\n");
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "Found only one continuous interval in xml file\nMust be either none or at least two");
		return 0;
	}


	// sort!
	if (excitation_rv->n_continuous > 1) {
		qsort(excitation_rv->continuous,excitation_rv->n_continuous,sizeof(xmi_energy_continuous),xmi_cmp_struct_xmi_energy_continuous);
	}
	if (excitation_rv->n_discrete > 1) {
		qsort(excitation_rv->discrete,excitation_rv->n_discrete,sizeof(xmi_energy_discrete),xmi_cmp_struct_xmi_energy_discrete);
	}

	// sanity check
	if (excitation_rv->n_continuous == 2) {
		if (excitation_rv->continuous[0].horizontal_intensity + excitation_rv->continuous[0].vertical_intensity +
		    excitation_rv->continuous[1].horizontal_intensity + excitation_rv->continuous[1].vertical_intensity == 0.0) {
			fprintf(stderr, "With only two continuous intervals, both cannot have zero intensity\n");
			g_set_error(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "With only two continuous intervals, both cannot have zero intensity\n");
			return 0;
		}
	}
	else if (excitation_rv->n_continuous > 2) {
		unsigned int i;
		for (i = 1 ; i < excitation_rv->n_continuous-1 ; i++) {
			int before = (excitation_rv->continuous[i-1].horizontal_intensity + excitation_rv->continuous[i-1].vertical_intensity) > 0.0;
			int current = (excitation_rv->continuous[i].horizontal_intensity + excitation_rv->continuous[i].vertical_intensity) > 0.0;
			int after = (excitation_rv->continuous[i+1].horizontal_intensity + excitation_rv->continuous[i+1].vertical_intensity) > 0.0;
			if (i == 1 && before + current == 0) {
				fprintf(stderr, "First two continuous intensity densities cannot have a total intensity of zero\n");
				g_set_error(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "First two continuous intensity densities cannot have a total intensity of zero");
				return 0;
			}
			else if (i == excitation_rv->n_continuous - 2 && current + after == 0) {
				fprintf(stderr, "Last two continuous intensity densities cannot have a total intensity of zero\n");
				g_set_error(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "Last two continuous intensity densities cannot have a total intensity of zero");
				return 0;
			}
			else if (before + current + after == 0) {
				fprintf(stderr, "Three consecutive continuous intensity densities cannot have a total intensity of zero\n");
				g_set_error(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "Three consecutive continuous intensity densities cannot have a total intensity of zero");
				return 0;
			}
		}
	}

	*excitation = excitation_rv;

	return 1;
}


static int xmi_read_input_absorbers(xmlDocPtr doc, xmlNodePtr node, xmi_absorbers **absorbers, GError **error) {

	xmlNodePtr subnode,subsubnode;


	*absorbers = (xmi_absorbers *) g_malloc0(sizeof(xmi_absorbers));

	(*absorbers)->n_exc_layers = 0;
	(*absorbers)->exc_layers = NULL;
	(*absorbers)->n_det_layers = 0;
	(*absorbers)->det_layers = NULL;


	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "excitation_path")) {
			subsubnode = xmlFirstElementChild(subnode);
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "layer")) {
					(*absorbers)->exc_layers = g_realloc((*absorbers)->exc_layers,sizeof(xmi_layer)*++(*absorbers)->n_exc_layers);
					if (xmi_read_input_layer(doc, subsubnode, (*absorbers)->exc_layers+(*absorbers)->n_exc_layers-1, error) == 0) {
						return 0;
					}
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "detector_path")) {
			subsubnode = xmlFirstElementChild(subnode);
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "layer")) {
					(*absorbers)->det_layers = g_realloc((*absorbers)->det_layers,sizeof(xmi_layer)*++(*absorbers)->n_det_layers);
					if (xmi_read_input_layer(doc, subsubnode, (*absorbers)->det_layers+(*absorbers)->n_det_layers-1, error) == 0) {
						return 0;
					}
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}
		}

		subnode = xmlNextElementSibling(subnode);
	}

	return 1;
}



static int xmi_read_input_detector(xmlDocPtr doc, xmlNodePtr node, xmi_detector **detector, GError **error) {
	xmlNodePtr subnode,subsubnode;
	xmlChar *txt;

	*detector= (xmi_detector *) g_malloc0(sizeof(xmi_detector));

	(*detector)->n_crystal_layers = 0;
	(*detector)->crystal_layers = NULL;
	(*detector)->nchannels = 2048;

	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "detector_type")) {
			txt = xmlNodeGetContent(subnode->children);
			if (!xmlStrcmp(txt,(const xmlChar *) "SiLi")) {
				(*detector)->detector_type = XMI_DETECTOR_CONVOLUTION_PROFILE_SILI;
			}
			else if (!xmlStrcmp(txt,(const xmlChar *) "Ge")) {
				(*detector)->detector_type = XMI_DETECTOR_CONVOLUTION_PROFILE_GE;
			}
			else if (!xmlStrcmp(txt,(const xmlChar *) "Si_SDD")) {
				(*detector)->detector_type = XMI_DETECTOR_CONVOLUTION_PROFILE_SI_SDD;
			}
			else {
				fprintf(stderr,"Unknown detector type\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "Unknown detector type");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "live_time")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->live_time)) != 1) {
				fprintf(stderr,"error reading in live_time of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in live_time of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "pulse_width")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->pulse_width)) != 1) {
				fprintf(stderr,"error reading in pulse_width of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in pulse_width of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "gain")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->gain)) != 1) {
				fprintf(stderr,"error reading in gain of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in gain of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "zero")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->zero)) != 1) {
				fprintf(stderr,"error reading in zero of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in zero of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "fano")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->fano)) != 1) {
				fprintf(stderr,"error reading in fano of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in fano of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "noise")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->noise)) != 1) {
				fprintf(stderr,"error reading in noise of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in noise of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "nchannels")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%i",&((*detector)->nchannels)) != 1) {
				fprintf(stderr,"error reading in nchannels of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in nchannels of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "crystal")) {
			subsubnode = xmlFirstElementChild(subnode);
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "layer")) {
					(*detector)->crystal_layers = g_realloc((*detector)->crystal_layers,sizeof(xmi_layer)*++(*detector)->n_crystal_layers);
					if (xmi_read_input_layer(doc, subsubnode, (*detector)->crystal_layers+(*detector)->n_crystal_layers-1, error) == 0) {
						return 0;
					}
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}


		}
		subnode= xmlNextElementSibling(subnode);
	}

	return 1;
}

static int xmi_read_input_layer(xmlDocPtr doc, xmlNodePtr node, xmi_layer *layer, GError **error) {
	xmlNodePtr subnode,subsubnode;
	xmlChar *txt;
	int n_elements, *Z,i;
	double *weight;
	int *sorted_Z_ind;

	layer->n_elements = 0;
	layer->Z = NULL;
	layer->weight = NULL;

	n_elements = 0;
	Z = NULL;
	weight = NULL;


	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "element")) {
			n_elements++;
			Z = (int *) g_realloc(Z,sizeof(int)*n_elements);
			weight = (double *) g_realloc(weight,sizeof(double)*n_elements);
			subsubnode = xmlFirstElementChild(subnode);
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "atomic_number")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%i",Z+n_elements-1) != 1) {
						fprintf(stderr,"error reading in atomic_number of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in atomic_number of xml file");
						return 0;
					}
					xmlFree(txt);

				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "weight_fraction")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",weight+n_elements-1) != 1) {
						fprintf(stderr,"error reading in weight_fraction of xml file\n");
						g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in weight_fraction of xml file");
						return 0;
					}
					//special case: if weight is equal to zero, skip the element
					//negative values will get caught by xmi_validate
					//normalization will be performed by xmi_input_C2F
					if (fabs(weight[n_elements-1]) < 1E-20) {
						xmlFree(txt);
						Z = g_realloc(Z, sizeof(int)*--n_elements);
						weight = g_realloc(weight, sizeof(double)*n_elements);
						break;
					}
					xmlFree(txt);
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "density")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&(layer->density)) != 1) {
				fprintf(stderr,"error reading in density of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in density of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "thickness")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&(layer->thickness)) != 1) {
				fprintf(stderr,"error reading in thickness of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in thickness of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		subnode = xmlNextElementSibling(subnode);

	}

#ifndef QUICKLOOK
	//sort!
	sorted_Z_ind = xmi_sort_idl_int(Z, n_elements);
	layer->n_elements = n_elements;
	layer->Z = (int *) g_malloc0(sizeof(int)*n_elements);
	layer->weight = (double*) g_malloc0(sizeof(double)*n_elements);
	for (i = 0 ; i < n_elements ; i++) {
		layer->Z[i] = Z[sorted_Z_ind[i]];
		layer->weight[i] = weight[sorted_Z_ind[i]];
	}
	//normalize and divide weight by 100 to get rid of the percentages
	xmi_scale_double(layer->weight, layer->n_elements, 1.0/xmi_sum_double(layer->weight, layer->n_elements));

	g_free(sorted_Z_ind);
	g_free(Z);
	g_free(weight);
#else
	layer->n_elements = n_elements;
	layer->Z = Z;
	layer->weight = weight;
#endif

	return 1;
}

/**
 * xmi_input_read_from_xml_file: (constructor):
 * @xmsifile: XMI-MSIM input filename.
 * @error: return location for a GError, or NULL
 *
 * Reads an existing file into an xmi_input struct, which will be allocated by the method.
 *
 * Returns: the xmi_input struct on success, NULL otherwise.
 */
xmi_input* xmi_input_read_from_xml_file(const char *xmsifile, GError **error) {

	xmlDocPtr doc;
	xmlNodePtr root;
	xmlParserCtxtPtr ctx;

	LIBXML_TEST_VERSION

	if ((ctx=xmlNewParserCtxt()) == NULL) {
		handle_error(error);
		return NULL;
	}

	if ((doc = xmlCtxtReadFile(ctx, xmsifile, NULL, XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		xmlFreeParserCtxt(ctx);
		handle_error(error);
		return NULL;
	}

	if (ctx->valid == 0) {
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		handle_error(error);
		return NULL;
	}

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		handle_error(error);
		return NULL;
	}

	if (xmlStrcmp(root->name,(const xmlChar*) "xmimsim") != 0) {
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		handle_error(error);
		return NULL;
	}

	//allocate memory for input
	xmi_input *input = g_malloc0(sizeof(xmi_input));

	if (xmi_read_input_xml_body(doc, root, input, error) == FALSE) {
		xmi_input_free(input);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		return NULL;
	}

#ifndef QUICKLOOK
	if (xmi_input_validate(input) != 0) {
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error validating input data");
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		xmi_input_free(input);
		return NULL;
	}
#endif

	xmlFreeParserCtxt(ctx);
	xmlFreeDoc(doc);

	return input;
}

#ifndef QUICKLOOK
/**
 * xmi_input_write_to_xml_string:
 * @input: an xmi_input instance.
 * @xmlstring: (out): return location for the string containing the xml file.
 * @error: return location for a GError, or NULL
 *
 * Writes an xmi_input struct to a string, while be allocated by the method.
 *
 * Returns: %TRUE on success, %FALSE otherwise.
 */
gboolean xmi_input_write_to_xml_string(xmi_input *input, char **xmlstring, GError **error) {
	xmlDocPtr doc = NULL;
	xmlNodePtr root_node = NULL;
	xmlDtdPtr dtd = NULL;
	int xmlstringlength;

	LIBXML_TEST_VERSION

	if ((doc = xmlNewDoc(BAD_CAST "1.0")) == NULL) {
		fprintf(stderr,"Error calling xmlNewDoc\n");
		handle_error(error);
		return FALSE;
	}
	if ((dtd = xmlCreateIntSubset(doc, BAD_CAST  "xmimsim", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd")) == NULL) {
		fprintf(stderr,"Error creating DTD\n");
		handle_error(error);
		return FALSE;
	}

	root_node = xmlNewNode(NULL, BAD_CAST "xmimsim");
	xmlDocSetRootElement(doc, root_node);

	if (xmi_write_input_xml_body(doc, root_node, input, error) == 0)
		return FALSE;

	xmlDocDumpMemory(doc,(xmlChar **) xmlstring, &xmlstringlength);
	xmlFreeDoc(doc);

	return TRUE;
}

/**
 * xmi_input_write_to_xml_file:
 * @input: an xmi_input instance.
 * @xmsifile: name of the file to write to.
 * @error: return location for a GError, or NULL
 *
 * Writes an xmi_input struct to a file.
 *
 * Returns: %TRUE on success, %FALSE otherwise.
 */
gboolean xmi_input_write_to_xml_file(xmi_input *input, const char *xmsifile, GError **error) {
	xmlDocPtr doc = NULL;
	xmlNodePtr root_node = NULL;
	xmlDtdPtr dtd = NULL;

	LIBXML_TEST_VERSION

	if ((doc = xmlNewDoc(BAD_CAST "1.0")) == NULL) {
		fprintf(stderr,"Error calling xmlNewDoc\n");
		handle_error(error);
		return FALSE;
	}
	xmlThrDefIndentTreeOutput(2);
	if ((dtd = xmlCreateIntSubset(doc, BAD_CAST  "xmimsim", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd")) == NULL) {
		fprintf(stderr,"Error creating DTD\n");
		handle_error(error);
		return FALSE;
	}

	root_node = xmlNewNode(NULL, BAD_CAST "xmimsim");
	xmlDocSetRootElement(doc, root_node);

	if (xmi_write_default_comments(doc, root_node, error) == 0) {
		return FALSE;
	}

	if (xmi_write_input_xml_body(doc, root_node, input, error) == 0)
		return FALSE;

	if (xmlSaveFormatFileEnc(xmsifile, doc, NULL, 1) == -1) {
		fprintf(stderr,"Could not write to %s\n", xmsifile);
		handle_error(error);
		return FALSE;
	}
	xmlFreeDoc(doc);

	return TRUE;
}

/**
 * xmi_write_output_xml_body:
 * @doc:
 * @subroot:
 * @output:
 * @steps: (element-type gint):
 * @with_svg:
 *
 * Returns: 1 on success, 0 otherwise
 */
gboolean xmi_write_output_xml_body(xmlDocPtr doc, xmlNodePtr subroot, xmi_output *output, GArray *steps, int with_svg, GError **error) {
	unsigned int i,j,k;
	double *maxima;
	double gl_conv_max;
	double gl_unconv_max;
	xmlNodePtr nodePtr1, nodePtr2, nodePtr3, nodePtr4;



	LIBXML_TEST_VERSION


	xmi_new_prop_printf(subroot, BAD_CAST "version", "%s", VERSION);

	if (steps) {
		for (i = 0 ; i < steps->len ; i++) {
			xmi_new_prop_with_index_printf(subroot, BAD_CAST "step", i + 1, "%i", g_array_index(steps, int, i));
		}
	}

	xmlNewChild(subroot, NULL, BAD_CAST "inputfile", BAD_CAST output->inputfile);

	//spectrum convoluted
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "spectrum_conv", NULL);

	for (j = 0 ; j < output->input->detector->nchannels ; j++) {
		nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "channel", NULL);
		xmi_new_child_printf(nodePtr2, BAD_CAST "channelnr", "%i", j);
		xmi_new_child_printf(nodePtr2, BAD_CAST "energy", "%g", output->input->detector->gain*j+output->input->detector->zero);

		for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {
			nodePtr3 = xmi_new_child_printf(nodePtr2, BAD_CAST "counts", "%g", output->channels_conv[i][j]);
			xmi_new_prop_printf(nodePtr3, BAD_CAST "interaction_number", "%i", i);
		}
	}

	//spectrum unconvoluted
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "spectrum_unconv", NULL);

	for (j = 0 ; j < output->input->detector->nchannels ; j++) {
		nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "channel", NULL);
		xmi_new_child_printf(nodePtr2, BAD_CAST "channelnr", "%i", j);
		xmi_new_child_printf(nodePtr2, BAD_CAST "energy", "%g", output->input->detector->gain*j+output->input->detector->zero);

		for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {
			nodePtr3 = xmi_new_child_printf(nodePtr2, BAD_CAST "counts", "%g", output->channels_unconv[i][j]);
			xmi_new_prop_printf(nodePtr3, BAD_CAST "interaction_number", "%i", i);
		}
	}

	//brute force
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "brute_force_history", NULL);
	char *symbol;
	//Z loop
	for (i = 0 ; i < output->nbrute_force_history; i++) {
		nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "fluorescence_line_counts", NULL);
		xmi_new_prop_printf(nodePtr2, BAD_CAST "atomic_number", "%i", output->brute_force_history[i].atomic_number);
		//two attributes: atomic_number and element
		xrl_error *xrlError = NULL;
		symbol = AtomicNumberToSymbol(output->brute_force_history[i].atomic_number, &xrlError);
		if (xrlError) {
			g_propagate_error(error, xmi_error_convert_xrl_to_glib(xrlError));
			xrl_error_free(xrlError);
			return 0;
		}
		xmi_new_prop_printf(nodePtr2, BAD_CAST "symbol", "%s", symbol);
		xrlFree(symbol);
		xmi_new_prop_printf(nodePtr2, BAD_CAST "total_counts", "%g", output->brute_force_history[i].total_counts);
		//start with the different lines
		//line loop
		for (j = 0 ; j < output->brute_force_history[i].n_lines ; j++) {
			nodePtr3 = xmlNewChild(nodePtr2, NULL, BAD_CAST "fluorescence_line", NULL);
			xmi_new_prop_printf(nodePtr3, BAD_CAST "type", "%s", output->brute_force_history[i].lines[j].line_type);
			xmi_new_prop_printf(nodePtr3, BAD_CAST "energy", "%g", output->brute_force_history[i].lines[j].energy);
			xmi_new_prop_printf(nodePtr3, BAD_CAST "total_counts", "%g", output->brute_force_history[i].lines[j].total_counts);
			//interactions loop
			for (k = 0 ; k < output->brute_force_history[i].lines[j].n_interactions ; k++) {
				nodePtr4 = xmi_new_child_printf(nodePtr3, BAD_CAST "counts", "%g", output->brute_force_history[i].lines[j].interactions[k].counts);
				xmi_new_prop_printf(nodePtr4, BAD_CAST "interaction_number", "%i", output->brute_force_history[i].lines[j].interactions[k].interaction_number);
			}
		}
	}

	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "variance_reduction_history", NULL);

	//variance reduction history
	if (output->var_red_history == NULL)
		goto after_var_red_history;

	//Z loop
	for (i = 0 ; i < output->nvar_red_history; i++) {
		nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "fluorescence_line_counts", NULL);
		xmi_new_prop_printf(nodePtr2, BAD_CAST "atomic_number", "%i", output->var_red_history[i].atomic_number);
		//two attributes: atomic_number and element
		xrl_error *xrlError = NULL;
		symbol = AtomicNumberToSymbol(output->var_red_history[i].atomic_number, &xrlError);
		if (xrlError) {
			g_propagate_error(error, xmi_error_convert_xrl_to_glib(xrlError));
			xrl_error_free(xrlError);
			return 0;
		}
		xmi_new_prop_printf(nodePtr2, BAD_CAST "symbol", "%s", symbol);
		xrlFree(symbol);
		xmi_new_prop_printf(nodePtr2, BAD_CAST "total_counts", "%g", output->var_red_history[i].total_counts);
		//start with the different lines
		//line loop
		for (j = 0 ; j < output->var_red_history[i].n_lines ; j++) {
			nodePtr3 = xmlNewChild(nodePtr2, NULL, BAD_CAST "fluorescence_line", NULL);
			xmi_new_prop_printf(nodePtr3, BAD_CAST "type", "%s", output->var_red_history[i].lines[j].line_type);
			xmi_new_prop_printf(nodePtr3, BAD_CAST "energy", "%g", output->var_red_history[i].lines[j].energy);
			xmi_new_prop_printf(nodePtr3, BAD_CAST "total_counts", "%g", output->var_red_history[i].lines[j].total_counts);
			//interactions loop
			for (k = 0 ; k < output->var_red_history[i].lines[j].n_interactions ; k++) {
				nodePtr4 = xmi_new_child_printf(nodePtr3, BAD_CAST "counts", "%g", output->var_red_history[i].lines[j].interactions[k].counts);
				xmi_new_prop_printf(nodePtr4, BAD_CAST "interaction_number", "%i", output->var_red_history[i].lines[j].interactions[k].interaction_number);
			}
		}
	}

after_var_red_history:

	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "xmimsim-input", NULL);
	if (xmi_write_input_xml_body(doc, nodePtr1, output->input, error) == 0)
		return 0;

	if (with_svg == 0) {
		goto after_svg;
	}

	//write svg stuff
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "svg_graphs", NULL);

        // calculate global channel max for conv
	maxima = (double *) g_malloc0(sizeof(double)*(output->input->general->n_interactions_trajectory+1));
	maxima[0]=0.0;
	for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {
		maxima[i]=xmi_maxval_double(output->channels_conv[i], output->input->detector->nchannels);
	}
	gl_conv_max = xmi_maxval_double(maxima,output->input->general->n_interactions_trajectory+1);
        g_free(maxima);
	maxima = NULL;

        // calculate global channel max for unconv
	maxima = (double *) g_malloc0(sizeof(double)*(output->input->general->n_interactions_trajectory+1));
	maxima[0]=0.0;
	for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {
		maxima[i]=xmi_maxval_double(output->channels_unconv[i], output->input->detector->nchannels);
	}
	gl_unconv_max = xmi_maxval_double(maxima,output->input->general->n_interactions_trajectory+1);
	g_free(maxima); maxima = NULL;


        //write svg_graph lines
	for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {

		//convoluted first

		if (xmi_write_input_xml_svg(doc, nodePtr1, output->input, "convoluted", i, output->channels_conv[i], gl_conv_max, error) == 0) {
			return 0;
		}

		//unconvoluted second

		if (xmi_write_input_xml_svg(doc, nodePtr1, output->input, "unconvoluted", i, output->channels_unconv[i], gl_unconv_max, error ) == 0) {
			return 0;
		}

	}

	//end it
after_svg:

	return 1;
}

/**
 * xmi_output_write_to_xml_file:
 * @output: an xmi_output instance.
 * @xmsofile: name of the file to write to.
 * @error: return location for a GError, or NULL
 *
 * Writes an xmi_output struct to a file.
 *
 * Returns: %TRUE on success, %FALSE otherwise.
 */
gboolean xmi_output_write_to_xml_file(xmi_output *output, const char *xmsofile, GError **error) {
	xmlDocPtr doc = NULL;
	xmlNodePtr root_node = NULL;
	xmlDtdPtr dtd = NULL;

	LIBXML_TEST_VERSION

	if ((doc = xmlNewDoc(BAD_CAST "1.0")) == NULL) {
		fprintf(stderr,"Error calling xmlNewDoc\n");
		handle_error(error);
		return FALSE;
	}
	xmlThrDefIndentTreeOutput(2);
	if ((dtd = xmlCreateIntSubset(doc, BAD_CAST  "xmimsim-results", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd")) == NULL) {
		fprintf(stderr,"Error creating DTD\n");
		handle_error(error);
		return FALSE;
	}

	root_node = xmlNewNode(NULL, BAD_CAST "xmimsim-results");
	xmlDocSetRootElement(doc, root_node);

	if (xmi_write_default_comments(doc, root_node, error) == 0) {
		return FALSE;
	}

	if(xmi_write_output_xml_body(doc, root_node, output, NULL, 1, error) == 0){
		return FALSE;
	}

	if (xmlSaveFormatFileEnc(xmsofile, doc, NULL, 1) == -1) {
		fprintf(stderr,"Could not write to %s\n", xmsofile);
		handle_error(error);
		return FALSE;
	}
	xmlFreeDoc(doc);

	return TRUE;
}


int xmi_write_input_xml_body(xmlDocPtr doc, xmlNodePtr subroot, xmi_input *input, GError **error) {
	int i;
	gchar *detector_type = NULL;
	xmlNodePtr nodePtr1, nodePtr2, nodePtr3;


	//general
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "general", NULL);
	xmi_new_prop_printf(nodePtr1, BAD_CAST "version", "%s", VERSION);
	xmlNewChild(nodePtr1, NULL, BAD_CAST "outputfile", BAD_CAST input->general->outputfile);
	xmi_new_child_printf(nodePtr1, BAD_CAST "n_photons_interval", "%li", input->general->n_photons_interval);
	xmi_new_child_printf(nodePtr1, BAD_CAST "n_photons_line", "%li", input->general->n_photons_line);
	xmi_new_child_printf(nodePtr1, BAD_CAST "n_interactions_trajectory", "%i", input->general->n_interactions_trajectory);
	xmlNewChild(nodePtr1, NULL, BAD_CAST "comments", BAD_CAST input->general->comments);

	//composition
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "composition", NULL);

	if (xmi_write_layer_xml_body(doc, nodePtr1, input->composition->layers, input->composition->n_layers, error) == 0)
		return 0;

	xmi_new_child_printf(nodePtr1, BAD_CAST "reference_layer", "%i", input->composition->reference_layer);

	//geometry
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "geometry", NULL);
	xmi_new_child_printf(nodePtr1, BAD_CAST "d_sample_source", "%g", input->geometry->d_sample_source);
	nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "n_sample_orientation", NULL);
	xmi_new_child_printf(nodePtr2, BAD_CAST "x", "%g", input->geometry->n_sample_orientation[0]);
	xmi_new_child_printf(nodePtr2, BAD_CAST "y", "%g", input->geometry->n_sample_orientation[1]);
	xmi_new_child_printf(nodePtr2, BAD_CAST "z", "%g", input->geometry->n_sample_orientation[2]);
	nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "p_detector_window", NULL);
	xmi_new_child_printf(nodePtr2, BAD_CAST "x", "%g", input->geometry->p_detector_window[0]);
	xmi_new_child_printf(nodePtr2, BAD_CAST "y", "%g", input->geometry->p_detector_window[1]);
	xmi_new_child_printf(nodePtr2, BAD_CAST "z", "%g", input->geometry->p_detector_window[2]);
	nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "n_detector_orientation", NULL);
	xmi_new_child_printf(nodePtr2, BAD_CAST "x", "%g", input->geometry->n_detector_orientation[0]);
	xmi_new_child_printf(nodePtr2, BAD_CAST "y", "%g", input->geometry->n_detector_orientation[1]);
	xmi_new_child_printf(nodePtr2, BAD_CAST "z", "%g", input->geometry->n_detector_orientation[2]);
	xmi_new_child_printf(nodePtr1, BAD_CAST "area_detector", "%g", input->geometry->area_detector);
	xmi_new_child_printf(nodePtr1, BAD_CAST "collimator_height", "%g", input->geometry->collimator_height);
	xmi_new_child_printf(nodePtr1, BAD_CAST "collimator_diameter", "%g", input->geometry->collimator_diameter);
	xmi_new_child_printf(nodePtr1, BAD_CAST "d_source_slit", "%g", input->geometry->d_source_slit);
	nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "slit_size", NULL);
	xmi_new_child_printf(nodePtr2, BAD_CAST "slit_size_x", "%g", input->geometry->slit_size_x);
	xmi_new_child_printf(nodePtr2, BAD_CAST "slit_size_y", "%g", input->geometry->slit_size_y);

	//excitation
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "excitation", NULL);
	if (input->excitation->n_discrete > 0) {
		for (i = 0 ; i < input->excitation->n_discrete ; i++) {
			nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "discrete", NULL);
			xmi_new_child_printf(nodePtr2, BAD_CAST "energy", "%g", input->excitation->discrete[i].energy);
			xmi_new_child_printf(nodePtr2, BAD_CAST "horizontal_intensity", "%g", input->excitation->discrete[i].horizontal_intensity);
			xmi_new_child_printf(nodePtr2, BAD_CAST "vertical_intensity", "%g", input->excitation->discrete[i].vertical_intensity);
			xmi_new_child_printf(nodePtr2, BAD_CAST "sigma_x", "%g", input->excitation->discrete[i].sigma_x);
			xmi_new_child_printf(nodePtr2, BAD_CAST "sigma_xp", "%g", input->excitation->discrete[i].sigma_xp);
			xmi_new_child_printf(nodePtr2, BAD_CAST "sigma_y", "%g", input->excitation->discrete[i].sigma_y);
			xmi_new_child_printf(nodePtr2, BAD_CAST "sigma_yp", "%g", input->excitation->discrete[i].sigma_yp);
			//only write scale_parameter if distribution type is not monochromatic
			//this is done to keep the file backwards compatible
			if (input->excitation->discrete[i].distribution_type != XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC) {
				nodePtr3 = xmi_new_child_printf(nodePtr2, BAD_CAST "scale_parameter", "%g", input->excitation->discrete[i].scale_parameter);
				if (input->excitation->discrete[i].distribution_type == XMI_ENERGY_DISCRETE_DISTRIBUTION_GAUSSIAN) {
					xmi_new_prop_printf(nodePtr3, BAD_CAST "distribution_type", "%s", "gaussian");
				}
				else if (input->excitation->discrete[i].distribution_type == XMI_ENERGY_DISCRETE_DISTRIBUTION_LORENTZIAN) {
					xmi_new_prop_printf(nodePtr3, BAD_CAST "distribution_type", "%s", "lorentzian");
				}
			}
		}
	}
	if (input->excitation->n_continuous > 0) {
		for (i = 0 ; i < input->excitation->n_continuous ; i++) {
			nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "continuous", NULL);
			xmi_new_child_printf(nodePtr2, BAD_CAST "energy", "%g", input->excitation->continuous[i].energy);
			xmi_new_child_printf(nodePtr2, BAD_CAST "horizontal_intensity", "%g", input->excitation->continuous[i].horizontal_intensity);
			xmi_new_child_printf(nodePtr2, BAD_CAST "vertical_intensity", "%g", input->excitation->continuous[i].vertical_intensity);
			xmi_new_child_printf(nodePtr2, BAD_CAST "sigma_x", "%g", input->excitation->continuous[i].sigma_x);
			xmi_new_child_printf(nodePtr2, BAD_CAST "sigma_xp", "%g", input->excitation->continuous[i].sigma_xp);
			xmi_new_child_printf(nodePtr2, BAD_CAST "sigma_y", "%g", input->excitation->continuous[i].sigma_y);
			xmi_new_child_printf(nodePtr2, BAD_CAST "sigma_yp", "%g", input->excitation->continuous[i].sigma_yp);
		}
	}

	//absorbers
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "absorbers", NULL);
	if (input->absorbers->n_exc_layers > 0) {
		nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "excitation_path", NULL);
		if (xmi_write_layer_xml_body(doc, nodePtr2, input->absorbers->exc_layers, input->absorbers->n_exc_layers, error) == 0) {
			return 0;
		}
	}
	if (input->absorbers->n_det_layers > 0) {
		nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "detector_path", NULL);
		if (xmi_write_layer_xml_body(doc, nodePtr2, input->absorbers->det_layers, input->absorbers->n_det_layers, error) == 0) {
			return 0;
		}
	}

	//detector
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "detector", NULL);
	if (input->detector->detector_type == XMI_DETECTOR_CONVOLUTION_PROFILE_SILI)
		detector_type = g_strdup("SiLi");
	else if (input->detector->detector_type == XMI_DETECTOR_CONVOLUTION_PROFILE_GE)
		detector_type = g_strdup("Ge");
	else if (input->detector->detector_type == XMI_DETECTOR_CONVOLUTION_PROFILE_SI_SDD)
		detector_type = g_strdup("Si_SDD");

	xmlNewChild(nodePtr1, NULL, BAD_CAST "detector_type", BAD_CAST detector_type);
	xmi_new_child_printf(nodePtr1, BAD_CAST "live_time", "%g", input->detector->live_time);
	xmi_new_child_printf(nodePtr1, BAD_CAST "pulse_width", "%g", input->detector->pulse_width);
	xmi_new_child_printf(nodePtr1, BAD_CAST "nchannels", "%i", input->detector->nchannels);
	xmi_new_child_printf(nodePtr1, BAD_CAST "gain", "%g", input->detector->gain);
	xmi_new_child_printf(nodePtr1, BAD_CAST "zero", "%g", input->detector->zero);
	xmi_new_child_printf(nodePtr1, BAD_CAST "fano", "%g", input->detector->fano);
	xmi_new_child_printf(nodePtr1, BAD_CAST "noise", "%g", input->detector->noise);
	nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "crystal", NULL);
	g_free(detector_type);
	if (xmi_write_layer_xml_body(doc, nodePtr2, input->detector->crystal_layers, input->detector->n_crystal_layers, error) == 0) {
		return 0;
	}

	return 1;
}
#endif

/**
 * xmi_input_read_from_xml_string: (constructor):
 * @xmsistring: string containing XMI-MSIM inputfile.
 * @error: return location for a GError, or NULL
 *
 * Reads an XMI-MSIM input-file in string format into an xmi_input struct, which whill be allocated by the method.
 *
 * Returns: The xmi_input struct if successful, NULL otherwise.
 */
xmi_input* xmi_input_read_from_xml_string(const char *xmsistring, GError **error) {
	xmlDocPtr doc;
	xmlNodePtr root;
	xmlParserCtxtPtr ctx;

	LIBXML_TEST_VERSION

	if ((ctx=xmlNewParserCtxt()) == NULL) {
		handle_error(error);
		return NULL;
	}

	if ((doc = xmlCtxtReadDoc(ctx, BAD_CAST xmsistring, "weirdness.xml", NULL, XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		xmlFreeParserCtxt(ctx);
		handle_error(error);
		return NULL;
	}

	if (ctx->valid == 0) {
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		handle_error(error);
		return NULL;
	}

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		handle_error(error);
		return NULL;
	}

	if (xmlStrcmp(root->name, (const xmlChar*) "xmimsim")) {
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		handle_error(error);
		return FALSE;
	}

	//allocate memory for input
	xmi_input *input = g_malloc0(sizeof(xmi_input));

	if (xmi_read_input_xml_body(doc, root, input, error) == FALSE) {
		xmi_input_free(input);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		return NULL;
	}

#ifndef QUICKLOOK
	if (xmi_input_validate(input) != 0) {
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error validating input data");
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		xmi_input_free(input);
		return NULL;
	}
#endif

	xmlFreeParserCtxt(ctx);
	xmlFreeDoc(doc);

	return input;
}

#ifndef QUICKLOOK
int xmi_write_input_xml_svg(xmlDocPtr doc, xmlNodePtr subroot, xmi_input *input, char *name, int interaction,
double *channels, double maximum2, GError **error) {

	double minimum;
	double maximum;
	double minimum_log, maximum_log;
	double *energies;
	int i;
	double energy;
	double intensity;
	int width = SVG_DEFAULT_BOX_WIDTH;
	int height = SVG_DEFAULT_BOX_HEIGHT;
 	int energy_step = 5.0;
	int max_channel;
	xmlNodePtr nodePtr1, nodePtr2, nodePtr3, nodePtr4;




	// max and min
//	maximum = xmi_maxval_double(channels, nchannels);
//	minimum = xmi_minval_double(channels, nchannels);
	maximum = maximum2;
	minimum = 1;
	maximum_log = log10(maximum);
	minimum_log = log10(minimum);


	max_channel = 0;
	for (i = input->detector->nchannels-1 ; i >= 0 ; i--) {
		if (channels[i] >= 1) {
		max_channel = i;
		break;
		}
	}


	energies = xmi_dindgen(input->detector->nchannels);
	//xmi_add_val_to_array_double(energies, nchannels, 1.0);
	xmi_scale_double(energies, input->detector->nchannels, input->detector->gain);
	xmi_add_val_to_array_double(energies, input->detector->nchannels, input->detector->zero);

	// start plot graphic
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "graphic", NULL);
	nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "id", NULL);
	xmlNewChild(nodePtr2, NULL, BAD_CAST "name", BAD_CAST name);
	xmi_new_child_printf(nodePtr2, BAD_CAST "interaction", "%i", interaction);

        //create box
	nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "rect", NULL);
	xmlNewChild(nodePtr2, NULL, BAD_CAST "view", NULL);
	nodePtr3 = xmlNewChild(nodePtr2, NULL, BAD_CAST "size", NULL);
	xmi_new_child_printf(nodePtr3, BAD_CAST "width", "%i", width);
	xmi_new_child_printf(nodePtr3, BAD_CAST "height", "%i", height);
	xmi_new_child_printf(nodePtr3, BAD_CAST "min_energy", "%g", energies[0]);
	xmi_new_child_printf(nodePtr3, BAD_CAST "max_energy", "%g", energies[max_channel-1]);

	// x-axis
	nodePtr3 = xmlNewChild(nodePtr2, NULL, BAD_CAST "x-axis", NULL);
	xmlNewChild(nodePtr3, NULL, BAD_CAST "name", BAD_CAST "Energy (keV)");
	//for now print energy every 5 keV on X-axis
	energy = 0.0;
	while (energy <= energies[max_channel]) {
		nodePtr4 = xmlNewChild(nodePtr3, NULL, BAD_CAST "index", NULL);
		xmi_new_child_printf(nodePtr4, BAD_CAST "value", "%g", e2c(energy, channels, energies, max_channel));
		xmi_new_child_printf(nodePtr4, BAD_CAST "name", "%.1f", energy);
		energy += energy_step;
	}
	// end x-axis

        // start y-axis
	nodePtr3 = xmlNewChild(nodePtr2, NULL, BAD_CAST "y-axis", NULL);
	xmlNewChild(nodePtr3, NULL, BAD_CAST "name", BAD_CAST "Intensity (counts)");

	if (minimum_log < 0.0) {
		minimum_log = 0.0;
	}

	intensity = 1.0;
	while (intensity <= maximum) {
		if (intensity < minimum) {
			intensity *= 10.0;
			continue;
		}
		nodePtr4 = xmlNewChild(nodePtr3, NULL, BAD_CAST "index", NULL);
		xmi_new_child_printf(nodePtr4, BAD_CAST "value", "%g", i2c(intensity, maximum_log, minimum_log));
		xmi_new_child_printf(nodePtr4, BAD_CAST "name", "%.0f", intensity);
		intensity *= 10.0;
	}

        //loop grafic
	nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "points", NULL);
	xmlNewChild(nodePtr2, NULL, BAD_CAST "color", BAD_CAST "blue");

	//loop point
	for (i = 0 ; i <= max_channel ; i++) {
		nodePtr3 = xmlNewChild(nodePtr2, NULL, BAD_CAST "point", NULL);
		xmi_new_child_printf(nodePtr3, BAD_CAST "x", "%g", e2c(energies[i], channels, energies, max_channel));
		xmi_new_child_printf(nodePtr3, BAD_CAST "y", "%g", i2c(channels[i], maximum_log, minimum_log));
	}
	//end loop point
        //end loop

	g_free(energies);

	return 1;
}

static float e2c(double energy, double * channels, double * energies, int nchannels ){
	float xval;
	int out_of_range;

	out_of_range = 0;

	xval = (float)((SVG_DEFAULT_BOX_WIDTH)*(energy - energies[0])/(energies[nchannels-1] - energies[0]));

	if (xval < 0) {xval = 0; out_of_range++;	}
	if (xval > SVG_DEFAULT_BOX_WIDTH) {xval = SVG_DEFAULT_BOX_WIDTH; out_of_range++;}

	//if(out_of_range > 0)fprintf(stderr, "svg x-values out of range \n");

	return xval;
}

static float i2c(double intensity, double maximum_log, double minimum_log) {
	float val;
	double intensity_corrected;
	int out_of_range;

	out_of_range = 0;
	intensity_corrected = intensity;
	if (intensity < 1)intensity_corrected = 1;

	val = (float) (SVG_DEFAULT_BOX_HEIGHT)*(log10(intensity_corrected)-minimum_log)/(maximum_log-minimum_log);

	if(val < 0) {val = 0; out_of_range++;	}
	if(val > SVG_DEFAULT_BOX_HEIGHT) {val = SVG_DEFAULT_BOX_HEIGHT; out_of_range++;}

	if(out_of_range > 0)fprintf(stderr, "svg y-values out of range");

	return val;
}
#endif

/**
 * xmi_output_read_from_xml_file: (constructor):
 * @xmsofile: XMI-MSIM output filename.
 * @error: return location for a GError, or NULL
 *
 * Reads an existing file into an xmi_output struct, which will be allocated by the method.
 *
 * Returns: an xmi_output struct on success, %NULL otherwise.
 */
xmi_output* xmi_output_read_from_xml_file(const char *xmsofile, GError **error) {
	xmlDocPtr doc;
	xmlNodePtr root;
	xmlParserCtxtPtr ctx;

	LIBXML_TEST_VERSION

	if ((ctx=xmlNewParserCtxt()) == NULL) {
		handle_error(error);
		return NULL;
	}

	if ((doc = xmlCtxtReadFile(ctx, xmsofile, NULL, XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		xmlFreeParserCtxt(ctx);
		handle_error(error);
		return NULL;
	}

	if (ctx->valid == 0) {
		xmlFreeDoc(doc);
		handle_error(error);
		return NULL;
	}
	xmlFreeParserCtxt(ctx);

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		xmlFreeDoc(doc);
		handle_error(error);
		return NULL;
	}

	if (xmlStrcmp(root->name,(const xmlChar*) "xmimsim-results") != 0) {
		xmlFreeDoc(doc);
		g_set_error(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "XML document is %s of wrong type, expected xmimsim-results\n", xmsofile);
		return NULL;
	}

	xmi_output *op = g_malloc0(sizeof(xmi_output));

	if (xmi_read_output_xml_body(doc, root, op, NULL, error) == 0) {
		xmi_output_free(op);
		return NULL;
	}

	op->outputfile = g_strdup(xmsofile);

	xmlFreeDoc(doc);

	return op;
}

int xmi_read_input_xml_body(xmlDocPtr doc, xmlNodePtr root, xmi_input *input, GError **error) {
	xmlNodePtr subroot = xmlFirstElementChild(root);

	while (subroot != NULL) {
		if (!xmlStrcmp(subroot->name,(const xmlChar *) "general")) {
			if (xmi_read_input_general(doc, subroot, &(input->general), error) == 0) {
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "composition")) {
			if (xmi_read_input_composition(doc, subroot, &(input->composition), error) == 0) {
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "geometry")) {
			if (xmi_read_input_geometry(doc, subroot, &(input->geometry), error) == 0) {
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "excitation")) {
			if (xmi_read_input_excitation(doc, subroot, &(input->excitation), error) == 0) {
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "absorbers")) {
			if (xmi_read_input_absorbers(doc, subroot, &(input->absorbers), error) == 0) {
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "detector")) {
			if (xmi_read_input_detector(doc, subroot, &(input->detector), error) == 0) {
				return 0;
			}
		}

		subroot = xmlNextElementSibling(subroot);
	}
	return 1;
}
#ifndef QUICKLOOK
int xmi_write_default_comments(xmlDocPtr doc, xmlNodePtr root_node, GError **error) {
	gchar *timestring;
	gchar *text;

#if GLIB_MAJOR_VERSION == 2 && GLIB_MINOR_VERSION >= 26
	GDateTime *time = g_date_time_new_now_local();
	timestring = g_date_time_format(time,"%F %H:%M:%S (%Z)");
	g_date_time_unref(time);
#else
	GTimeVal time;
	g_get_current_time(&time);
        timestring = g_time_val_to_iso8601(&time);
#endif
	text = g_strdup_printf(" <Creator>%s (%s)</Creator>\n <Timestamp>%s</Timestamp>\n <Hostname>%s</Hostname>", g_get_real_name(),g_get_user_name(),  timestring, g_get_host_name());

	xmlNodePtr cur_node = xmlAddPrevSibling(root_node, xmlNewComment(BAD_CAST text));
	xmlAddNextSibling(cur_node, xmlNewComment(BAD_CAST "DO NOT MODIFY THIS FILE UNLESS YOU KNOW WHAT YOU ARE DOING!"));

	g_free(timestring);
	g_free(text);

	return 1;
}
#endif

/**
 * xmi_read_output_xml_body:
 * @doc:
 * @root:
 * @op:
 * @steps: (element-type gint):
 *
 * Returns: 1 on success, 0 otherwise
 */
gboolean xmi_read_output_xml_body(xmlDocPtr doc, xmlNodePtr root, xmi_output *op, GArray *steps, GError **error) {
	xmlNodePtr subroot;
	xmlChar *txt;

	xmlAttrPtr attr = root->properties;

	while (attr != NULL) {
		if (!xmlStrcmp(attr->name,(const xmlChar *) "version")) {
			txt =xmlNodeGetContent(attr->children);
			if(sscanf((const char *)txt,"%f",&op->version) != 1) {
				fprintf(stderr,"error reading in version of xml file\n");
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in version of xml file");
				return 0;
			}
			xmlFree(txt);
		}
		attr=attr->next;
	}
	//read steps if necessary
	if (steps != NULL) {
		attr = root->properties;
		while (attr != NULL) {
			if (xmlStrncmp(attr->name, BAD_CAST "step", 4) == 0) {
				int stepnr = -1;
				if (sscanf((const char *) attr->name, "step%d", &stepnr) != 1) {
					g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error parsing step attribute of xml file");
					return 0;
				}
				stepnr--;
				if (stepnr < 0) {
					g_set_error(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "invalid step number %d in xml file", stepnr);
					return 0;
				}
				else if (stepnr >= steps->len) {
					// this is for backwards compatibility -> I always wrote step1 and step2, even if it was just a 1D simulation...
					attr = attr->next;
					continue;
				}

				txt = xmlNodeGetContent(attr->children);
				int step = -1;
				if(sscanf((const char *) txt, "%d", &step) != 1) {
					g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in step attribute of xml file");
					return 0;
				}
				g_array_index(steps, int, stepnr) = step;
				xmlFree(txt);
			}
			attr = attr->next;
		}
	}


	//read in xmimsim
	xmlXPathContextPtr xpathCtx;
	xmlXPathObjectPtr xpathObj;
	xpathCtx = xmlXPathNewContext(doc);
	if(xpathCtx == NULL) {
        	fprintf(stderr,"Error: unable to create new XPath context\n");
		handle_error(error);
		xmlFreeDoc(doc);
        	return 0;
	}
	xpathObj = xmlXPathNodeEval(root, BAD_CAST "xmimsim-input", xpathCtx);
	if(xpathObj == NULL || xpathObj->nodesetval->nodeNr == 0) {
		fprintf(stderr,"Error: unable to evaluate xpath expression xmimsim-input\n");
		handle_error(error);
		xmlXPathFreeContext(xpathCtx);
		if (xpathObj)
			xmlXPathFreeObject(xpathObj);
		xmlFreeDoc(doc);
		return 0;
	}

	op->input = g_malloc0(sizeof(xmi_input));

	if (xmi_read_input_xml_body(doc, xpathObj->nodesetval->nodeTab[0], op->input, error) == 0)
		return 0;

#ifndef QUICKLOOK
	if (xmi_input_validate(op->input) != 0) {
		xmlFreeDoc(doc);
		xmlXPathFreeContext(xpathCtx);
		xmlXPathFreeObject(xpathObj);
		fprintf(stderr, "Error validating input data\n");
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error validating input data");
		return 0;
	}
#endif
	op->ninteractions = op->input->general->n_interactions_trajectory;

	xmlXPathFreeObject(xpathObj);
	xmlXPathFreeContext(xpathCtx);

	subroot = xmlFirstElementChild(root);

	while (subroot != NULL) {
		if (!xmlStrcmp(subroot->name,(const xmlChar *) "inputfile")) {
			//inputfile
			txt = xmlNodeGetContent(subroot->children);
			op->inputfile = g_strdup((char *) txt);
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "spectrum_conv")) {
			//convoluted spectrum
			if (xmi_read_output_spectrum(doc,subroot, op, 1, error) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
			double sum = 0.0;
			int i;
			for (i = 0 ; i < op->input->detector->nchannels ; i++)
				sum += op->channels_conv[0][i];

			if (sum == 0.0)
				op->use_zero_interactions = 0;
			else
				op->use_zero_interactions = 1;
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "spectrum_unconv")) {
			//unconvoluted spectrum
			if (xmi_read_output_spectrum(doc,subroot, op, 0, error) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "brute_force_history")) {
			if (xmi_read_output_history(doc,subroot, &op->brute_force_history, &op->nbrute_force_history, error) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "variance_reduction_history")) {
			if (xmi_read_output_history(doc,subroot, &op->var_red_history, &op->nvar_red_history, error) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		subroot = xmlNextElementSibling(subroot);
	}
	return 1;
}

/**
 * xmi_archive_read_from_xml_file: (constructor):
 * @xmsafile: XMI-MSIM archive filename.
 * @error: return location for a GError, or NULL
 *
 * Reads an existing file into an xmi_archive struct, which will be allocated by the method.
 *
 * Returns: the xmi_archive struct, or NULL on error.
 */
xmi_archive* xmi_archive_read_from_xml_file(const char *xmsafile, GError **error) {
	xmlDocPtr doc;
	xmlNodePtr root;
	xmlParserCtxtPtr ctx;
	int files_read = 0;

	LIBXML_TEST_VERSION

	if ((ctx=xmlNewParserCtxt()) == NULL) {
		handle_error(error);
		return NULL;
	}

	if ((doc = xmlCtxtReadFile(ctx, xmsafile, NULL, XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		handle_error(error);
		xmlFreeParserCtxt(ctx);
		return NULL;
	}

	if (ctx->valid == 0) {
		handle_error(error);
		xmlFreeDoc(doc);
		return NULL;
	}
	xmlFreeParserCtxt(ctx);

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		xmlFreeDoc(doc);
		handle_error(error);
		return NULL;
	}

	if (xmlStrcmp(root->name,(const xmlChar*) "xmimsim-archive") != 0) {
		xmlFreeDoc(doc);
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "wrong type, expected xmimsim-archive");
		return NULL;
	}

	xmlAttrPtr attr = root->properties;
	xmlChar *txt;

	xmi_archive *ar = g_malloc0(sizeof(xmi_archive));
	ar->ref_count = 1;
	//in case it's not there, make sure the version is 0
	ar->version = 0.0;
	while (attr != NULL) {
		if (!xmlStrcmp(attr->name,(const xmlChar *) "version")) {
			txt =xmlNodeGetContent(attr->children);
			if(sscanf((const char *)txt,"%f",&ar->version) != 1) {
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "error reading in version of xml file");
				xmi_archive_unref(ar);
				return NULL;
			}
			xmlFree(txt);
		}
		attr=attr->next;
	}

	xmlNodePtr subroot;

	subroot = xmlFirstElementChild(root);

	ar->single_data = g_ptr_array_new_with_free_func((GDestroyNotify) xmi_batch_single_data_free);
	guint len = 1;

	while (subroot != NULL) {
		if (!xmlStrncmp(subroot->name,(const xmlChar *) "start_value", 11)) {
			txt = xmlNodeGetContent(subroot->children);
			xmi_batch_single_data *data = g_malloc0(sizeof(xmi_batch_single_data));
			g_ptr_array_add(ar->single_data, data);
			if (sscanf((const char*) txt, "%lg", &data->start) != 1) {
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "could not read start_value");
				xmi_archive_unref(ar);
				return NULL;
			}
			xmlFree(txt);
		}
		else if (!xmlStrncmp(subroot->name,(const xmlChar *) "end_value", 9)) {
			txt = xmlNodeGetContent(subroot->children);
			xmi_batch_single_data *data = g_ptr_array_index(ar->single_data, ar->single_data->len - 1);
			if (sscanf((const char*) txt, "%lg", &data->end) != 1) {
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "could not read end_value");
				xmi_archive_unref(ar);
				return NULL;
			}
			xmlFree(txt);
		}
		else if (!xmlStrncmp(subroot->name,(const xmlChar *) "nsteps", 5)) {
			txt = xmlNodeGetContent(subroot->children);
			xmi_batch_single_data *data = g_ptr_array_index(ar->single_data, ar->single_data->len - 1);
			if (sscanf((const char*) txt, "%d", &data->nsteps) != 1) {
				g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "could not read nsteps");
				xmi_archive_unref(ar);
				return NULL;
			}
			xmlFree(txt);
		}
		else if (!xmlStrncmp(subroot->name,(const xmlChar *) "xpath", 5)) {
			txt = xmlNodeGetContent(subroot->children);
			xmi_batch_single_data *data = g_ptr_array_index(ar->single_data, ar->single_data->len - 1);
			// This cannot work on the archive since it applies on the master input-file
			/*xmlXPathContextPtr xpathContext = xmlXPathNewContext(subroot->doc);
			if (xpathContext == NULL) {
				fprintf(stderr, "Could not get xpath1 xpathContext\n");
				handle_error(error);
				return 0;
			}
			xmlXPathObjectPtr xpathResult = xmlXPathEvalExpression(txt, xpathContext);
			if (xpathResult == NULL || xmlXPathNodeSetIsEmpty(xpathResult->nodesetval)) {
				fprintf(stderr, "Invalid xpath1: %s\n", txt);
				handle_error(error);
				if (xpathResult)
					xmlXPathFreeObject(xpathResult);
				xmlXPathFreeContext(xpathContext);
				return 0;
			}
			xmlXPathFreeObject(xpathResult);
			xmlXPathFreeContext(xpathContext);
			*/
			data->xpath = g_strdup((char*) txt);
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "xmimsim-results")) {
			// first time we get here, we need to create the GPtrArray
			if (ar->output == NULL) {
				guint i;
				ar->dims = g_array_sized_new(FALSE, TRUE, sizeof(int), ar->single_data->len);
				for (i = 0 ; i < ar->single_data->len ; i++) {
					xmi_batch_single_data *data = g_ptr_array_index(ar->single_data, i);
					int dim = data->nsteps + 1;
					len *= dim;
					g_array_append_val(ar->dims, dim);
				}
				ar->output = g_ptr_array_new_full(len, (GDestroyNotify) xmi_output_free);
				g_ptr_array_set_size(ar->output, len);
			}
			xmi_output *output = g_malloc0(sizeof(xmi_output));
			GArray *steps = g_array_sized_new(TRUE, TRUE, sizeof(int), ar->single_data->len);
			g_array_set_size(steps, ar->single_data->len);
			if (xmi_read_output_xml_body(doc, subroot, output, steps, error) == 0) {
				xmi_archive_unref(ar);
				return NULL;
			}
			output->outputfile = g_strdup(output->input->general->outputfile);
			gint offset = xmi_row_major_array_get_offset(ar->dims, steps);
			g_assert(offset >= 0);
			g_ptr_array_index(ar->output, offset) = output;
			g_array_unref(steps);
			//fix links
			/*ar->input[step1][step2] = ar->output[step1][step2]->input;
			ar->inputfiles[step1][step2] = ar->output[step1][step2]->inputfile;
			ar->outputfiles[step1][step2] = ar->output[step1][step2]->outputfile;*/
			files_read++;
		}
		subroot = xmlNextElementSibling(subroot);
	}


	xmlFreeDoc(doc);

	if (files_read != len) {
		g_set_error_literal(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML, "nfiles/nsteps mismatch");
		xmi_archive_unref(ar);
		return NULL;
	}

	return ar;
}

#ifndef QUICKLOOK
/**
 * xmi_archive_write_to_xml_file:
 * @archive: an xmi_archive instance.
 * @xmsafile: name of the file to write to.
 * @error: return location for a GError, or NULL
 *
 * Writes an xmi_archive struct to a file.
 *
 * Returns: %TRUE on success, %FALSE otherwise.
 */
gboolean xmi_archive_write_to_xml_file(xmi_archive *archive, const char *xmsafile, GError **error) {
	xmlDocPtr doc = NULL;
	xmlNodePtr root_node = NULL;
	xmlDtdPtr dtd = NULL;
	xmlNodePtr nodePtr1;


	LIBXML_TEST_VERSION

	if ((doc = xmlNewDoc(BAD_CAST "1.0")) == NULL) {
		handle_error(error);
		return FALSE;
	}
	xmlThrDefIndentTreeOutput(2);
	xmlSetDocCompressMode(doc, 9);

	if ((dtd = xmlCreateIntSubset(doc, BAD_CAST  "xmimsim-archive", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd")) == NULL) {
		handle_error(error);
		return FALSE;
	}

	root_node = xmlNewNode(NULL, BAD_CAST "xmimsim-archive");
	xmlDocSetRootElement(doc, root_node);

	if (xmi_write_default_comments(doc, root_node, error) == 0) {
		return FALSE;
	}

	//body
	unsigned int i, j;

	for (i = 0 ; i < archive->single_data->len ; i++) {
		xmi_batch_single_data *data = g_ptr_array_index(archive->single_data, i);

		xmi_new_child_with_index_printf(root_node, BAD_CAST "start_value", i + 1, "%g", data->start);
		xmi_new_child_with_index_printf(root_node, BAD_CAST "end_value", i + 1, "%g", data->end);
		xmi_new_child_with_index_printf(root_node, BAD_CAST "nsteps", i + 1, "%d", data->nsteps);
		xmi_new_child_with_index_printf(root_node, BAD_CAST "xpath", i + 1, "%s", data->xpath);
	}

	for (i = 0 ; i < archive->output->len ; i++) {
		g_debug("Printing xmi_output %u", i);
		nodePtr1 = xmlNewChild(root_node, NULL, BAD_CAST "xmimsim-results", NULL);

		GArray *indices = xmi_row_major_array_get_indices(archive->dims, i);
		g_assert(indices != NULL);

		if (xmi_write_output_xml_body(doc, nodePtr1, g_ptr_array_index(archive->output, i), indices, 0, error) == 0) {
			g_array_unref(indices);
			return FALSE;
		}
		g_array_unref(indices);
	}

	if (xmlSaveFileEnc(xmsafile, doc, NULL) == -1) {
		handle_error(error);
		return FALSE;
	}
	xmlFreeDoc(doc);

	return TRUE;
}

int xmi_write_layer_xml_body(xmlDocPtr doc, xmlNodePtr subroot, xmi_layer *layers, int n_layers, GError **error) {
	xmlNodePtr nodePtr1, nodePtr2;
	int i, j;

	for (i = 0 ; i < n_layers ; i++) {
		nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "layer", NULL);
		//normalize weights
		double *weight = xmi_memdup(layers[i].weight, layers[i].n_elements * sizeof(double));
		xmi_scale_double(weight, layers[i].n_elements, 1.0/xmi_sum_double(weight, layers[i].n_elements));
		for (j = 0 ; j < layers[i].n_elements ; j++) {
			//skip if weight fraction is equal to zero
			if (fabs(layers[i].weight[j]) < 1E-20)
				continue;
			nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "element", NULL);
			xmi_new_child_printf(nodePtr2, BAD_CAST "atomic_number", "%i", layers[i].Z[j]);
			xmi_new_child_printf(nodePtr2, BAD_CAST "weight_fraction", "%g", weight[j]*100.0);
		}
		g_free(weight);
		xmi_new_child_printf(nodePtr1, BAD_CAST "density", "%g", layers[i].density);
		xmi_new_child_printf(nodePtr1, BAD_CAST "thickness", "%g", layers[i].thickness);
	}

	return 1;
}

static xmlNodePtr xmi_new_child_printf(xmlNodePtr nodePtr, const xmlChar *node_name, const gchar *message_format, ...) {
	//we'll be mixing some glib and libxml strings here, but that should be ok
	gchar *msg = NULL;
	va_list args;
	xmlNodePtr rv = NULL;

	if (message_format) {
		va_start(args, message_format);
		msg = g_strdup_vprintf(message_format, args);
		va_end(args);
		rv = xmlNewChild(nodePtr, NULL, node_name, BAD_CAST msg);
		g_free(msg);
	}
	return rv;
}

static xmlNodePtr xmi_new_child_with_index_printf(xmlNodePtr nodePtr, const xmlChar *node_name, unsigned int _index, const gchar *message_format, ...) {
	//we'll be mixing some glib and libxml strings here, but that should be ok
	gchar *msg = NULL;
	va_list args;
	xmlNodePtr rv = NULL;
	gchar *node_name_full = g_strdup_printf("%s%d", node_name, _index);

	if (message_format) {
		va_start(args, message_format);
		msg = g_strdup_vprintf(message_format, args);
		va_end(args);
		rv = xmlNewChild(nodePtr, NULL, BAD_CAST node_name_full, BAD_CAST msg);
		g_free(msg);
	}
	g_free(node_name_full);
	return rv;
}

static void xmi_new_prop_printf(xmlNodePtr nodePtr, const xmlChar *prop_name, const gchar *message_format, ...) {
	gchar *msg = NULL;
	va_list args;

	if (message_format) {
		va_start(args, message_format);
		msg = g_strdup_vprintf(message_format, args);
		va_end(args);
		xmlNewProp(nodePtr, prop_name, BAD_CAST msg);
		g_free(msg);
	}
}

static void xmi_new_prop_with_index_printf(xmlNodePtr nodePtr, const xmlChar *prop_name, unsigned int _index, const gchar *message_format, ...) {
	gchar *msg = NULL;
	va_list args;
	gchar *prop_name_full = g_strdup_printf("%s%d", prop_name, _index);

	if (message_format) {
		va_start(args, message_format);
		msg = g_strdup_vprintf(message_format, args);
		va_end(args);
		xmlNewProp(nodePtr, BAD_CAST prop_name_full, BAD_CAST msg);
		g_free(msg);
	}
	g_free(prop_name_full);
}
#endif

int xmi_cmp_struct_xmi_energy_discrete(const void *a, const void *b) {
	double diff;

	diff = ((xmi_energy_discrete *)a)->energy - ((xmi_energy_discrete *)b)->energy;

	if (diff > 0.000000001)
		return 1;
	else if (diff < -0.000000001)
		return -1;
	return 0;
}

int xmi_cmp_struct_xmi_energy_continuous(const void *a, const void *b) {
	double diff;

	diff = ((xmi_energy_continuous *)a)->energy - ((xmi_energy_continuous *)b)->energy;

	if (diff > 0.000000001)
		return 1;
	else if (diff < -0.000000001)
		return -1;
	return 0;
}

/**
 * xmi_row_major_array_get_indices:
 * @dims: (array) (element-type int): The dimensions of the array for which the indices are needed
 * @offset: The offset of the element of interest, assumming a row-major ordering
 *
 * Returns: (transfer full) (array) (element-type int): the array containing the indices corresponding to offset
 */
GArray* xmi_row_major_array_get_indices(GArray *dims, int offset) {
	g_return_val_if_fail(dims != NULL && offset >= 0, NULL);
	g_return_val_if_fail(dims->len > 0 &&  dims->len <= 8, NULL);

	guint i, max_offset = 1;

	for (i = 0 ; i < dims->len ; i++)
		max_offset *= g_array_index(dims, int, i);

	g_return_val_if_fail(offset < max_offset, NULL);

	GArray *rv = g_array_sized_new(FALSE, TRUE, sizeof(int), dims->len);
	g_array_set_size(rv, dims->len);

	if (dims->len == 1) {
		g_array_index(rv, int, 0) = offset;
		return rv;
	}

	for (i = dims->len - 1 ; i >= 1 ; i--) {
		int index = offset % g_array_index(dims, int, i);
		offset = offset / g_array_index(dims, int, i);
		g_array_index(rv, int, i) = index;
	}
	
	g_array_index(rv, int, 0) = offset;

	return rv;
}

/**
 * xmi_row_major_array_get_offset:
 * @dims: (array) (element-type int): The dimensions of the array for which the indices are needed
 * @indices: (array) (element-type int): The indices of the requested element in the array, along the dimensions
 *
 * Returns: the offset
 */
gint xmi_row_major_array_get_offset(GArray *dims, GArray *indices) {
	g_return_val_if_fail(dims != NULL && indices != NULL, -1);
	g_return_val_if_fail(dims->len > 0 &&  dims->len <= 8, -1);
	g_return_val_if_fail(indices->len == dims->len, -1);

	guint i;
	for (i = 0 ; i < dims->len ; i++) {
		int _index = g_array_index(indices, int, i);
		g_return_val_if_fail(_index >= 0 && _index < g_array_index(dims, int, i), -1);
	}

	gint offset = 0;

	if (dims->len == 1)
		return g_array_index(indices, int, 0);

	// see https://en.wikipedia.org/wiki/Row-_and_column-major_order#Address_calculation_in_general
	guint k;
	for (k = 0 ; k < dims->len ; k++) {
		guint l;
		guint Nprod = 1;
		for (l = k + 1 ; l < dims->len ; l++)
			Nprod *= g_array_index(dims, int, l);
		offset += Nprod * g_array_index(indices, int, k);
	}

	return offset;
}

