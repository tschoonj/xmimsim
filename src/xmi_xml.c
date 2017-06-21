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
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/catalog.h>
#include <libxml/globals.h>
#include <libxml/xpath.h>
#include <math.h>
#include <glib.h>
#include <xraylib.h>
#include <search.h>


#define SVG_DEFAULT_WIDTH 540
#define SVG_DEFAULT_HEIGHT 300
#define SVG_DEFAULT_BOX_WIDTH 500
#define SVG_DEFAULT_BOX_HEIGHT 250
#define SVG_DEFAULT_BOX_OFFSET_X 39
#define SVG_DEFAULT_BOX_OFFSET_Y 10


#define SVG_ENERGY_TO_SVG_COORDS(energy) (((SVG_DEFAULT_BOX_WIDTH)*(energy-channels[0])/(energies[nchannels-1]-energies[0]))+SVG_DEFAULT_BOX_OFFSET_X)

#define SVG_INTENSITY_TO_SVG_COORDS(intensity) ((SVG_DEFAULT_BOX_OFFSET_Y+SVG_DEFAULT_BOX_HEIGHT-2)+(5+2-SVG_DEFAULT_BOX_HEIGHT)*(log10(intensity)-minimum_log)/(maximum_log-minimum_log))


static int xmi_read_input_layer(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_layer *layer);
static int xmi_read_input_general(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_general **general);
static int xmi_read_input_composition(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_composition **composition);
static int xmi_read_input_geometry(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_geometry **geometry);
static int xmi_read_input_excitation(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_excitation **excitation);
static int xmi_read_input_absorbers(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_absorbers **absorbers);
static int xmi_read_input_detector(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_detector **detector);
static int xmi_read_output_spectrum(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_output *output, int conv);
static int xmi_read_output_history(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_fluorescence_line_counts **history, int *nhistory);

static float e2c(double energy, double * channels, double * energies, int nchannels );
static float i2c(double intensity, double maximum_log, double minimum_log);

static xmlNodePtr xmi_new_child_printf(xmlNodePtr nodePtr, const xmlChar *node_name, const gchar *message_format, ...) G_GNUC_PRINTF(3, 4);
static void xmi_new_prop_printf(xmlNodePtr nodePtr, const xmlChar *prop_name, const gchar *message_format, ...) G_GNUC_PRINTF(3, 4);

static gboolean xml_catalog_loaded = FALSE;

#ifdef G_OS_WIN32
#include "xmi_registry_win.h"

int xmi_xmlLoadCatalog() {
	char *share;
	int rv;

	if (xml_catalog_loaded)
		return 1;

	if (xmi_registry_win_query(XMI_REGISTRY_WIN_SHARE, &share) == 0) {
		return 0;
	}

	const xmlChar uriStartString[] = "http://www.xmi.UGent.be/xml/";
	const xmlChar *rewritePrefix = (xmlChar*) g_filename_to_uri(share,NULL,NULL);


	if (xmlCatalogAdd(BAD_CAST "catalog",NULL,NULL) == -1) {
		fprintf(stderr,"Could not add catalog\n");
		rv = 0;
	}
	else
		rv =1;

	if (xmlCatalogAdd(BAD_CAST "rewriteURI",uriStartString,rewritePrefix) == -1) {
		fprintf(stderr,"Could not add catalog rewriteURI\n");
		rv = 0;
	}
	else
		rv =1;

	g_free(share);

	xml_catalog_loaded = TRUE;

	return rv;

}
#elif defined(MAC_INTEGRATION)
#include "xmi_resources_mac.h"

int xmi_xmlLoadCatalog() {
	int rv;

	if (xml_catalog_loaded)
		return 1;

	gchar *resource_path = xmi_application_get_resource_path();
	if (resource_path == NULL) {
		return 0;
	}

	GString *resource_path_string = g_string_new(resource_path);
	g_free(resource_path);
	g_string_append(resource_path_string, "/");

	const xmlChar uriStartString[] = "http://www.xmi.UGent.be/xml/";
	const xmlChar *rewritePrefix = (xmlChar*) g_filename_to_uri(resource_path_string->str, NULL, NULL);


	if (xmlCatalogAdd(BAD_CAST "catalog", NULL, NULL) == -1) {
		fprintf(stderr,"Could not add catalog\n");
		rv = 0;
	}
	else
		rv =1;

	if (xmlCatalogAdd(BAD_CAST "rewriteURI", uriStartString, rewritePrefix) == -1) {
		fprintf(stderr,"Could not add catalog rewriteURI\n");
		rv = 0;
	}
	else
		rv =1;

	g_string_free(resource_path_string, TRUE);


	xml_catalog_loaded = TRUE;

	return rv;

}
#else
int xmi_xmlLoadCatalog() {
	char catalog[] = XMI_CATALOG;
	int rv;

	if (xml_catalog_loaded)
		return 1;

	if (xmlLoadCatalog(catalog) != 0) {
		fprintf(stderr,"Could not load %s\n",catalog);
		rv=0;
	}
	else
		rv=1;

	xml_catalog_loaded = TRUE;

	return rv;
}
#endif

static int xmi_read_output_spectrum(xmlDocPtr doc, xmlNodePtr spectrumPtr, struct xmi_output *output, int conv) {
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
		return 0;
	}
	else {
		//debug
		//fprintf(stdout,"nchannels: %i\n", nchannels);
	}

	channels_loc = (double **) g_malloc(sizeof(double *)* (output->ninteractions+1));
	for (i = 0 ; i <= output->ninteractions ; i++)
		channels_loc[i] = (double *) g_malloc0(nchannels * sizeof(double));

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

static int xmi_read_output_history(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_fluorescence_line_counts **history, int *nhistory) {

	//assume history will be a NULL terminated array...
	//count children
	unsigned int nchildren;
	xmlChar *txt;
	xmlNodePtr linePtr, countsPtr, subcountsPtr;
	struct xmi_fluorescence_line_counts *history_loc;
	xmlAttrPtr attr;
	int counter, counter2, counter3;

	nchildren = xmlChildElementCount(nodePtr);
	if (nchildren == 0) {
		*nhistory = 0;
		*history = NULL;
		return 1;
	}

	//malloc required memory
	*history = (struct xmi_fluorescence_line_counts *) g_malloc(sizeof(struct xmi_fluorescence_line_counts)*(nchildren));
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
					return 0;
				}
				xmlFree(txt);
			}
			else if (!xmlStrcmp(attr->name,(const xmlChar *) "total_counts")) {
				txt =xmlNodeGetContent(attr->children);
				if(sscanf((const char *)txt,"%lg",&(history_loc[counter].total_counts)) != 1) {
					fprintf(stderr,"xmi_read_output_history: error reading in total_counts\n");
					return 0;
				}
				xmlFree(txt);
			}


			attr = attr->next;
		}
		//determine number of children
		nchildren = xmlChildElementCount(linePtr);
		history_loc[counter].n_lines = nchildren;
		history_loc[counter].lines = (struct xmi_fluorescence_line *) g_malloc(sizeof(struct xmi_fluorescence_line)*nchildren);

		countsPtr = xmlFirstElementChild(linePtr);
		counter2 = 0;
		while (countsPtr) {
			attr = countsPtr->properties;
			while (attr) {
				if (!xmlStrcmp(attr->name,(const xmlChar *) "type")) {
					txt =xmlNodeGetContent(attr->children);
					if (txt == NULL) {
						fprintf(stderr,"xmi_read_output_history: error reading in line_type\n");
						return 0;
					}
					history_loc[counter].lines[counter2].line_type = g_strdup((gchar*) txt);
					xmlFree(txt);
				}
				else if (!xmlStrcmp(attr->name,(const xmlChar *) "energy")) {
					txt =xmlNodeGetContent(attr->children);
					if(sscanf((const char *)txt,"%lg",&(history_loc[counter].lines[counter2].energy)) != 1) {
						fprintf(stderr,"xmi_read_output_history: error reading in energy\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(attr->name,(const xmlChar *) "total_counts")) {
					txt =xmlNodeGetContent(attr->children);
					if(sscanf((const char *)txt,"%lg",&(history_loc[counter].lines[counter2].total_counts)) != 1) {
						fprintf(stderr,"xmi_read_output_history: error reading in total_counts lvl2\n");
						return 0;
					}
					xmlFree(txt);
				}

				attr = attr->next;
			}
			counter3 = 0;
			subcountsPtr = xmlFirstElementChild(countsPtr);
			nchildren = xmlChildElementCount(countsPtr);
			history_loc[counter].lines[counter2].interactions = (struct xmi_counts *) g_malloc(sizeof(struct xmi_counts)*nchildren);
			history_loc[counter].lines[counter2].n_interactions = nchildren;
			while (subcountsPtr) {
				attr = subcountsPtr->properties;
				while (attr) {
					if (!xmlStrcmp(attr->name,(const xmlChar *) "interaction_number")) {
					txt =xmlNodeGetContent(attr->children);
					if(sscanf((const char *)txt,"%i",&(history_loc[counter].lines[counter2].interactions[counter3].interaction_number)) != 1) {
						fprintf(stderr,"xmi_read_output_history: error reading in interaction_number\n");
						return 0;
					}
					xmlFree(txt);
					}

					attr = attr->next;
				}
				txt = xmlNodeGetContent(subcountsPtr->children);
				if (sscanf((const char*) txt, "%lg",&(history_loc[counter].lines[counter2].interactions[counter3].counts)) !=1) {
					fprintf(stderr,"xmi_read_output_history: could not read counts\n");
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

static int xmi_read_input_general(xmlDocPtr doc, xmlNodePtr node, struct xmi_general **general) {
	xmlNodePtr subnode;
	xmlChar *txt;
	xmlAttrPtr attr;

	//allocate memory
	*general = (struct xmi_general *) g_malloc(sizeof(struct xmi_general));



	//version
	attr=node->properties;
	while (attr != NULL) {
		if (!xmlStrcmp(attr->name,(const xmlChar *) "version")) {
			txt =xmlNodeGetContent(attr->children);
			if(sscanf((const char *)txt,"%f",&((*general)->version)) != 1) {
				fprintf(stderr,"error reading in version of xml file\n");
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
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_photons_line")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%li",&((*general)->n_photons_line)) != 1) {
				fprintf(stderr,"error reading in n_photons_line of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_interactions_trajectory")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%i",&((*general)->n_interactions_trajectory)) != 1) {
				fprintf(stderr,"error reading in n_interactions_trajectory of xml file\n");
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

static int xmi_read_input_composition(xmlDocPtr doc, xmlNodePtr node, struct xmi_composition **composition) {
	xmlNodePtr subnode;
	xmlChar *txt;

	//allocate memory
	*composition = (struct xmi_composition *) g_malloc(sizeof(struct xmi_composition));

	(*composition)->n_layers = 0;
	(*composition)->layers = NULL;

	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "layer")){
			(*composition)->layers = (struct xmi_layer *) g_realloc((*composition)->layers,sizeof(struct xmi_layer)*++((*composition)->n_layers));
			//long live C and its deliciously complicated syntax :-)
			if (xmi_read_input_layer(doc, subnode, (*composition)->layers+(*composition)->n_layers-1) == 0) {
				return 0;
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "reference_layer")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%i",&((*composition)->reference_layer)) != 1) {
				fprintf(stderr,"error reading in reference_layer of xml file\n");
				return 0;
			}
			xmlFree(txt);
			if ((*composition)->reference_layer < 1 || (*composition)->reference_layer > (*composition)->n_layers) {
				fprintf(stderr,"invalid reference_layer value detected\n");
				return 0;
			}
		}
		subnode= xmlNextElementSibling(subnode);
	}

	return 1;
}

static int xmi_read_input_geometry(xmlDocPtr doc, xmlNodePtr node, struct xmi_geometry **geometry) {
	xmlNodePtr subnode,subsubnode;
	xmlChar *txt;


	//allocate memory
	*geometry= (struct xmi_geometry *) g_malloc(sizeof(struct xmi_geometry));

	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "d_sample_source")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->d_sample_source)) != 1) {
				fprintf(stderr,"error reading in d_sample_source of xml file\n");
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
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_sample_orientation[1])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_sample_orientation[2])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation y of xml file\n");
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
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->p_detector_window[1])) != 1) {
						fprintf(stderr,"error reading in p_detector_window y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->p_detector_window[2])) != 1) {
						fprintf(stderr,"error reading in p_detector_window z of xml file\n");
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
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_detector_orientation[1])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_detector_orientation[2])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation z of xml file\n");
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
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "collimator_height")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->collimator_height)) != 1) {
				fprintf(stderr,"error reading in collimator_height of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "collimator_diameter")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->collimator_diameter)) != 1) {
				fprintf(stderr,"error reading in collimator_diameter of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "d_source_slit")){
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->d_source_slit)) != 1) {
				fprintf(stderr,"error reading in d_source_slit of xml file\n");
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
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "slit_size_y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->slit_size_y)) != 1) {
						fprintf(stderr,"error reading in slit_size_y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}
		}
		subnode = xmlNextElementSibling(subnode);
	}

	//normalize vectors to avoid problems later on...
	xmi_normalize_vector_double((*geometry)->n_sample_orientation, 3);
	xmi_normalize_vector_double((*geometry)->n_detector_orientation, 3);

	return 1;
}

static int xmi_read_input_excitation(xmlDocPtr doc, xmlNodePtr node, struct xmi_excitation **excitation) {
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
	struct xmi_energy_discrete xed;
	struct xmi_energy_continuous xec;
	int distribution_type = XMI_DISCRETE_MONOCHROMATIC;
	double scale_parameter = 0.0;

	*excitation = (struct xmi_excitation *) g_malloc(sizeof(struct xmi_excitation));

	(*excitation)->n_discrete = 0;
	(*excitation)->discrete = NULL;
	(*excitation)->n_continuous = 0;
	(*excitation)->continuous = NULL;


	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar*) "discrete")) {
			subsubnode = xmlFirstElementChild(subnode);
			distribution_type = XMI_DISCRETE_MONOCHROMATIC;
			scale_parameter = 0.0;
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "energy")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(energy)) != 1) {
						fprintf(stderr,"error reading in discrete energy of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "horizontal_intensity")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(horizontal_intensity)) != 1) {
						fprintf(stderr,"error reading in discrete horizontal_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "vertical_intensity")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(vertical_intensity)) != 1) {
						fprintf(stderr,"error reading in discrete vertical_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_x")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_x)) != 1) {
						fprintf(stderr,"error reading in sigma_x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_xp")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_xp)) != 1) {
						fprintf(stderr,"error reading in sigma_xp of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_y)) != 1) {
						fprintf(stderr,"error reading in sigma_y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_yp")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_yp)) != 1) {
						fprintf(stderr,"error reading in sigma_yp of xml file\n");
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
								distribution_type = XMI_DISCRETE_MONOCHROMATIC;
								xmlFree(txt);
							}
							else if (xmlStrcmp(txt, BAD_CAST "gaussian") == 0) {
								distribution_type = XMI_DISCRETE_GAUSSIAN;
								xmlFree(txt);
								//read scale_parameter value
								txt = xmlNodeGetContent(subsubnode->children);
								if(sscanf((const char *)txt,"%lg",&scale_parameter) != 1) {
									fprintf(stderr,"error reading in scale_parameter of xml file\n");
									return 0;
								}
								xmlFree(txt);
							}
							else if (xmlStrcmp(txt, BAD_CAST "lorentzian") == 0) {
								distribution_type = XMI_DISCRETE_LORENTZIAN;
								xmlFree(txt);
								//read scale_parameter value
								txt = xmlNodeGetContent(subsubnode->children);
								if(sscanf((const char *)txt,"%lg",&scale_parameter) != 1) {
									fprintf(stderr,"error reading in scale_parameter of xml file\n");
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
			struct xmi_energy_discrete *xed_match;
#ifdef G_OS_WIN32
			unsigned int n_discrete = (*excitation)->n_discrete;
			if ((xed_match = _lfind(&xed, (*excitation)->discrete, &n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete)) != NULL)
#else
			size_t n_discrete = (*excitation)->n_discrete;
			if ((xed_match = lfind(&xed, (*excitation)->discrete, &n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete)) != NULL)
#endif
				{
				fprintf(stderr,"Warning: Duplicate discrete line energy detected\nAdding to existing discrete line\n");
				if (energy > 0.0 && energy <= 200.0 && horizontal_intensity >= 0.0 && vertical_intensity >= 0.0 && (horizontal_intensity + vertical_intensity) > 0.0) {
					xed_match->horizontal_intensity += horizontal_intensity;
					xed_match->vertical_intensity += vertical_intensity;
				}
				else {
					fprintf(stderr,"Error: Invalid discrete energy detected\n");
					return 0;
				}
			}
			else if (energy > 0.0 && energy <= 200.0 && horizontal_intensity >= 0.0 && vertical_intensity >= 0.0 && (horizontal_intensity + vertical_intensity) > 0.0) {
				(*excitation)->discrete = (struct xmi_energy_discrete *) g_realloc((*excitation)->discrete,++((*excitation)->n_discrete)*sizeof(struct xmi_energy_discrete));
				(*excitation)->discrete[(*excitation)->n_discrete-1].energy= energy ;
				(*excitation)->discrete[(*excitation)->n_discrete-1].horizontal_intensity = horizontal_intensity;
				(*excitation)->discrete[(*excitation)->n_discrete-1].vertical_intensity = vertical_intensity;
				(*excitation)->discrete[(*excitation)->n_discrete-1].sigma_x = sigma_x;
				(*excitation)->discrete[(*excitation)->n_discrete-1].sigma_xp = sigma_xp;
				(*excitation)->discrete[(*excitation)->n_discrete-1].sigma_y = sigma_y;
				(*excitation)->discrete[(*excitation)->n_discrete-1].sigma_yp = sigma_yp;
				(*excitation)->discrete[(*excitation)->n_discrete-1].scale_parameter = scale_parameter;
				(*excitation)->discrete[(*excitation)->n_discrete-1].distribution_type = distribution_type;
			}
			else {
				fprintf(stderr,"Error: Invalid discrete energy detected\n");
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
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "horizontal_intensity")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(horizontal_intensity)) != 1) {
						fprintf(stderr,"error reading in continuous horizontal_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "vertical_intensity")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(vertical_intensity)) != 1) {
						fprintf(stderr,"error reading in continuous vertical_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_x")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_x)) != 1) {
						fprintf(stderr,"error reading in sigma_x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_xp")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_xp)) != 1) {
						fprintf(stderr,"error reading in sigma_xp of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_y")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_y)) != 1) {
						fprintf(stderr,"error reading in sigma_y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_yp")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",&(sigma_yp)) != 1) {
						fprintf(stderr,"error reading in sigma_yp of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = xmlNextElementSibling(subsubnode);
			}
			xec.energy = energy;
#ifdef G_OS_WIN32
			unsigned int n_continuous = (*excitation)->n_continuous;
			if (_lfind(&xec, (*excitation)->continuous, &n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
#else
			size_t n_continuous = (*excitation)->n_continuous;
			if (lfind(&xec, (*excitation)->continuous, &n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
#endif
				fprintf(stderr,"Error: Duplicate continuous energy interval energy detected\n");
				return 0;
			}
			else if (energy >= 0.0 && energy <= 200.0 && horizontal_intensity >= 0.0 && vertical_intensity >= 0.0 && (horizontal_intensity + vertical_intensity) >= 0.0) {
				(*excitation)->continuous = (struct xmi_energy_continuous *) g_realloc((*excitation)->continuous,++((*excitation)->n_continuous)*sizeof(struct xmi_energy_continuous));
				(*excitation)->continuous[(*excitation)->n_continuous-1].energy= energy ;
				(*excitation)->continuous[(*excitation)->n_continuous-1].horizontal_intensity = horizontal_intensity;
				(*excitation)->continuous[(*excitation)->n_continuous-1].vertical_intensity = vertical_intensity;
				(*excitation)->continuous[(*excitation)->n_continuous-1].sigma_x = sigma_x;
				(*excitation)->continuous[(*excitation)->n_continuous-1].sigma_xp = sigma_xp;
				(*excitation)->continuous[(*excitation)->n_continuous-1].sigma_y = sigma_y;
				(*excitation)->continuous[(*excitation)->n_continuous-1].sigma_yp = sigma_yp;
			}
			else {
				fprintf(stderr,"Error: Invalid continuous interval detected\n");
				return 0;
			}
		}
		subnode = xmlNextElementSibling(subnode);
	}

	if ((*excitation)->n_continuous < 2 && (*excitation)->n_discrete == 0) {
		fprintf(stderr,"Error: Found no valid discrete or continuous energies in xml file\n");
		return 0;
	}
	else if ((*excitation)->n_continuous == 1) {
		fprintf(stderr,"Error: Found only one continuous interval in xml file\nMust be either none or at least two\n");
		return 0;
	}


	//sort!
	if ((*excitation)->n_continuous > 1) {
		qsort((*excitation)->continuous,(*excitation)->n_continuous,sizeof(struct xmi_energy_continuous),xmi_cmp_struct_xmi_energy_continuous);
	}
	if ((*excitation)->n_discrete > 1) {
		qsort((*excitation)->discrete,(*excitation)->n_discrete,sizeof(struct xmi_energy_discrete),xmi_cmp_struct_xmi_energy_discrete);
	}

	//make sure that two consecutive intervals do not have zero intensity
	if ((*excitation)->n_continuous > 2) {
		int i;
		for (i = 0 ; i < (*excitation)->n_continuous-1 ; i++) {
			if ((*excitation)->continuous[i].horizontal_intensity + (*excitation)->continuous[i].vertical_intensity +
			(*excitation)->continuous[i+1].horizontal_intensity + (*excitation)->continuous[i+1].vertical_intensity == 0.0
			) {
				fprintf(stderr, "Error: Two consecutive continuous intensity densities cannot both have a total intensity of zero\n");
				return 0;
			}
		}
	}

	return 1;
}


static int xmi_read_input_absorbers(xmlDocPtr doc, xmlNodePtr node, struct xmi_absorbers **absorbers) {

	xmlNodePtr subnode,subsubnode;


	*absorbers = (struct xmi_absorbers *) g_malloc(sizeof(struct xmi_absorbers));

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
					(*absorbers)->exc_layers = g_realloc((*absorbers)->exc_layers,sizeof(struct xmi_layer)*++(*absorbers)->n_exc_layers);
					if (xmi_read_input_layer(doc, subsubnode, (*absorbers)->exc_layers+(*absorbers)->n_exc_layers-1) == 0) {
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
					(*absorbers)->det_layers = g_realloc((*absorbers)->det_layers,sizeof(struct xmi_layer)*++(*absorbers)->n_det_layers);
					if (xmi_read_input_layer(doc, subsubnode, (*absorbers)->det_layers+(*absorbers)->n_det_layers-1) == 0) {
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



static int xmi_read_input_detector(xmlDocPtr doc, xmlNodePtr node, struct xmi_detector **detector) {
	xmlNodePtr subnode,subsubnode;
	xmlChar *txt;



	*detector= (struct xmi_detector *) g_malloc(sizeof(struct xmi_detector));

	(*detector)->n_crystal_layers = 0;
	(*detector)->crystal_layers = NULL;
	(*detector)->nchannels = 2048;

	subnode = xmlFirstElementChild(node);

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "detector_type")) {
			txt = xmlNodeGetContent(subnode->children);
			if (!xmlStrcmp(txt,(const xmlChar *) "SiLi")) {
				(*detector)->detector_type = XMI_DETECTOR_SILI;
			}
			else if (!xmlStrcmp(txt,(const xmlChar *) "Ge")) {
				(*detector)->detector_type = XMI_DETECTOR_GE;
			}
			else if (!xmlStrcmp(txt,(const xmlChar *) "Si_SDD")) {
				(*detector)->detector_type = XMI_DETECTOR_SI_SDD;
			}
			else {
				fprintf(stderr,"Unknown detector type\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "live_time")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->live_time)) != 1) {
				fprintf(stderr,"error reading in live_time of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "pulse_width")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->pulse_width)) != 1) {
				fprintf(stderr,"error reading in pulse_width of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "gain")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->gain)) != 1) {
				fprintf(stderr,"error reading in gain of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "zero")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->zero)) != 1) {
				fprintf(stderr,"error reading in zero of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "fano")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->fano)) != 1) {
				fprintf(stderr,"error reading in fano of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "noise")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&((*detector)->noise)) != 1) {
				fprintf(stderr,"error reading in noise of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "nchannels")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%i",&((*detector)->nchannels)) != 1) {
				fprintf(stderr,"error reading in nchannels of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "crystal")) {
			subsubnode = xmlFirstElementChild(subnode);
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "layer")) {
					(*detector)->crystal_layers = g_realloc((*detector)->crystal_layers,sizeof(struct xmi_layer)*++(*detector)->n_crystal_layers);
					if (xmi_read_input_layer(doc, subsubnode, (*detector)->crystal_layers+(*detector)->n_crystal_layers-1) == 0) {
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

static int xmi_read_input_layer(xmlDocPtr doc, xmlNodePtr node, struct xmi_layer *layer) {
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
						return 0;
					}
					xmlFree(txt);

				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "weight_fraction")) {
					txt = xmlNodeGetContent(subsubnode->children);
					if(sscanf((const char *)txt,"%lg",weight+n_elements-1) != 1) {
						fprintf(stderr,"error reading in weight_fraction of xml file\n");
						return 0;
					}
					//special case: if weight is equal to zero, skip the element
					//negative values will get caught by xmi_validate
					//normalization will be performed by xmi_input_C2F
					if (fabs(weight[n_elements-1]) < 1E-20) {
						xrlFree(txt);
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
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "thickness")) {
			txt = xmlNodeGetContent(subnode->children);
			if(sscanf((const char *)txt,"%lg",&(layer->thickness)) != 1) {
				fprintf(stderr,"error reading in thickness of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		subnode = xmlNextElementSibling(subnode);

	}

	//sort!
	sorted_Z_ind = xmi_sort_idl_int(Z, n_elements);
	layer->n_elements = n_elements;
	layer->Z = (int *) g_malloc(sizeof(int)*n_elements);
	layer->weight = (double*) g_malloc(sizeof(double)*n_elements);
	for (i = 0 ; i < n_elements ; i++) {
		layer->Z[i] = Z[sorted_Z_ind[i]];
		layer->weight[i] = weight[sorted_Z_ind[i]];
	}
	//normalize and divide weight by 100 to get rid of the percentages
	xmi_scale_double(layer->weight, layer->n_elements, 1.0/xmi_sum_double(layer->weight, layer->n_elements));

	g_free(sorted_Z_ind);
	g_free(Z);
	g_free(weight);

	return 1;
}


int xmi_read_input_xml (char *xmlfile, struct xmi_input **input) {

	xmlDocPtr doc;
	xmlNodePtr root;
	xmlParserCtxtPtr ctx;

	LIBXML_TEST_VERSION


	if ((ctx=xmlNewParserCtxt()) == NULL) {
		fprintf(stderr,"xmlNewParserCtxt error\n");
		return 0;
	}

	if ((doc = xmlCtxtReadFile(ctx,xmlfile,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		fprintf(stderr,"xmlCtxtReadFile error for %s\n",xmlfile);
		xmlFreeParserCtxt(ctx);
		return 0;
	}

	if (ctx->valid == 0) {
		fprintf(stderr,"Error validating %s\n",xmlfile);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		return 0;
	}

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		fprintf(stderr,"Error getting root element in file %s\n",xmlfile);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		return 0;
	}

	if (xmlStrcmp(root->name,(const xmlChar*) "xmimsim") != 0) {
		fprintf(stderr,"XML document is %s of wrong type, expected xmimsim\n",xmlfile);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		return 0;
	}

	//allocate memory for input
	*input = (struct xmi_input *) g_malloc(sizeof(struct xmi_input));

	if (xmi_read_input_xml_body(doc, root, *input) == 0)
		return 0;

	if (xmi_validate_input(*input) != 0) {
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		fprintf(stderr, "Error validating input data\n");
		return 0;
	}

	xmlFreeParserCtxt(ctx);
	xmlFreeDoc(doc);
	return 1;

}

int xmi_write_input_xml_to_string(char **xmlstring, struct xmi_input *input) {
	xmlDocPtr doc = NULL;
	xmlNodePtr root_node = NULL;
	xmlDtdPtr dtd = NULL;
	int xmlstringlength;



	LIBXML_TEST_VERSION


	if ((doc = xmlNewDoc(BAD_CAST "1.0")) == NULL) {
		fprintf(stderr,"Error calling xmlNewDoc\n");
		return 0;
	}
	if ((dtd = xmlCreateIntSubset(doc, BAD_CAST  "xmimsim", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd")) == NULL) {
		fprintf(stderr,"Error creating DTD\n");
		return 0;
	}

	root_node = xmlNewNode(NULL, BAD_CAST "xmimsim");
	xmlDocSetRootElement(doc, root_node);

	if (xmi_write_input_xml_body(doc, root_node, input) == 0)
		return 0;

	xmlDocDumpMemory(doc,(xmlChar **) xmlstring, &xmlstringlength);
	xmlFreeDoc(doc);

	return 1;
}

int xmi_write_input_xml(char *xmlfile, struct xmi_input *input) {
	xmlDocPtr doc = NULL;
	xmlNodePtr root_node = NULL;
	xmlDtdPtr dtd = NULL;


	LIBXML_TEST_VERSION


	if ((doc = xmlNewDoc(BAD_CAST "1.0")) == NULL) {
		fprintf(stderr,"Error calling xmlNewDoc\n");
		return 0;
	}
	xmlThrDefIndentTreeOutput(2);
	if ((dtd = xmlCreateIntSubset(doc, BAD_CAST  "xmimsim", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd")) == NULL) {
		fprintf(stderr,"Error creating DTD\n");
		return 0;
	}

	root_node = xmlNewNode(NULL, BAD_CAST "xmimsim");
	xmlDocSetRootElement(doc, root_node);

	if (xmi_write_default_comments(doc, root_node) == 0) {
		return 0;
	}

	if (xmi_write_input_xml_body(doc, root_node, input) == 0)
		return 0;

	if (xmlSaveFormatFileEnc(xmlfile, doc, NULL, 1) == -1) {
		fprintf(stderr,"Could not write to %s\n", xmlfile);
		return 0;
	}
	xmlFreeDoc(doc);

	return 1;
}


int xmi_write_output_xml_body(xmlDocPtr doc, xmlNodePtr subroot, struct xmi_output *output, int step1, int step2, int with_svg) {
	int i,j,k;
	double *maxima;
	double gl_conv_max;
	double gl_unconv_max;
	xmlNodePtr nodePtr1, nodePtr2, nodePtr3, nodePtr4;



	LIBXML_TEST_VERSION


	xmi_new_prop_printf(subroot, BAD_CAST "version", "%s", VERSION);

	if (step1 != -1 && step2 != -1) {
		xmi_new_prop_printf(subroot, BAD_CAST "step1", "%i", step1);
		xmi_new_prop_printf(subroot, BAD_CAST "step2", "%i", step2);
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
		symbol = AtomicNumberToSymbol(output->brute_force_history[i].atomic_number);
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
		symbol = AtomicNumberToSymbol(output->var_red_history[i].atomic_number);
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
	if (xmi_write_input_xml_body(doc, nodePtr1, output->input) == 0)
		return 0;

	if (with_svg == 0) {
		goto after_svg;
	}

	//write svg stuff
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "svg_graphs", NULL);

        // calculate global channel max for conv
	maxima = (double *) g_malloc(sizeof(double)*(output->input->general->n_interactions_trajectory+1));
	maxima[0]=0.0;
	for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {
		maxima[i]=xmi_maxval_double(output->channels_conv[i], output->input->detector->nchannels);
	}
	gl_conv_max = xmi_maxval_double(maxima,output->input->general->n_interactions_trajectory+1);
        g_free(maxima);
	maxima = NULL;

        // calculate global channel max for unconv
	maxima = (double *) g_malloc(sizeof(double)*(output->input->general->n_interactions_trajectory+1));
	maxima[0]=0.0;
	for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {
		maxima[i]=xmi_maxval_double(output->channels_unconv[i], output->input->detector->nchannels);
	}
	gl_unconv_max = xmi_maxval_double(maxima,output->input->general->n_interactions_trajectory+1);
	g_free(maxima); maxima = NULL;


        //write svg_graph lines
	for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {

		//convoluted first

		if (xmi_write_input_xml_svg(doc, nodePtr1, output->input, "convoluted", i, output->channels_conv[i], gl_conv_max) == 0) {
			fprintf(stderr,"Error in xmi_write_input_xml_svg\n");
			return 0;
		}

		//unconvoluted second

		if (xmi_write_input_xml_svg(doc, nodePtr1, output->input, "unconvoluted", i, output->channels_unconv[i], gl_unconv_max ) == 0) {
			fprintf(stderr,"Error in xmi_write_input_xml_svg\n");
			return 0;
		}

	}

	//end it
after_svg:

	return 1;
}

int xmi_write_output_xml(char *xmlfile, struct xmi_output *output) {
	xmlDocPtr doc = NULL;
	xmlNodePtr root_node = NULL;
	xmlDtdPtr dtd = NULL;


	LIBXML_TEST_VERSION


	if ((doc = xmlNewDoc(BAD_CAST "1.0")) == NULL) {
		fprintf(stderr,"Error calling xmlNewDoc\n");
		return 0;
	}
	xmlThrDefIndentTreeOutput(2);
	if ((dtd = xmlCreateIntSubset(doc, BAD_CAST  "xmimsim-results", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd")) == NULL) {
		fprintf(stderr,"Error creating DTD\n");
		return 0;
	}

	root_node = xmlNewNode(NULL, BAD_CAST "xmimsim-results");
	xmlDocSetRootElement(doc, root_node);

	if (xmi_write_default_comments(doc, root_node) == 0) {
		return 0;
	}

	if(xmi_write_output_xml_body(doc, root_node, output, -1, -1, 1) == 0){
		return 0;
	}

	if (xmlSaveFormatFileEnc(xmlfile, doc, NULL, 1) == -1) {
		fprintf(stderr,"Could not write to %s\n", xmlfile);
		return 0;
	}
	xmlFreeDoc(doc);

	return 1;
}


int xmi_write_input_xml_body(xmlDocPtr doc, xmlNodePtr subroot, struct xmi_input *input) {
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

	if (xmi_write_layer_xml_body(doc, nodePtr1, input->composition->layers, input->composition->n_layers) == 0)
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
			if (input->excitation->discrete[i].distribution_type != XMI_DISCRETE_MONOCHROMATIC) {
				nodePtr3 = xmi_new_child_printf(nodePtr2, BAD_CAST "scale_parameter", "%g", input->excitation->discrete[i].scale_parameter);
				if (input->excitation->discrete[i].distribution_type == XMI_DISCRETE_GAUSSIAN) {
					xmi_new_prop_printf(nodePtr3, BAD_CAST "distribution_type", "%s", "gaussian");
				}
				else if (input->excitation->discrete[i].distribution_type == XMI_DISCRETE_LORENTZIAN) {
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
		if (xmi_write_layer_xml_body(doc, nodePtr2, input->absorbers->exc_layers, input->absorbers->n_exc_layers) == 0) {
			return 0;
		}
	}
	if (input->absorbers->n_det_layers > 0) {
		nodePtr2 = xmlNewChild(nodePtr1, NULL, BAD_CAST "detector_path", NULL);
		if (xmi_write_layer_xml_body(doc, nodePtr2, input->absorbers->det_layers, input->absorbers->n_det_layers) == 0) {
			return 0;
		}
	}

	//detector
	nodePtr1 = xmlNewChild(subroot, NULL, BAD_CAST "detector", NULL);
	if (input->detector->detector_type == XMI_DETECTOR_SILI)
		detector_type = g_strdup("SiLi");
	else if (input->detector->detector_type == XMI_DETECTOR_GE)
		detector_type = g_strdup("Ge");
	else if (input->detector->detector_type == XMI_DETECTOR_SI_SDD)
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
	if (xmi_write_layer_xml_body(doc, nodePtr2, input->detector->crystal_layers, input->detector->n_crystal_layers) == 0) {
		return 0;
	}

	return 1;
}


int xmi_xmlfile_to_string(char *xmlfile, char **xmlstring, int *xmlstringlength) {

	xmlDocPtr doc;
	xmlNodePtr root;
	xmlParserCtxtPtr ctx;

	if ((ctx=xmlNewParserCtxt()) == NULL) {
		fprintf(stderr,"xmlNewParserCtxt error\n");
		return 0;
	}

	if ((doc = xmlCtxtReadFile(ctx,xmlfile,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		fprintf(stderr,"xmlCtxtReadFile error for %s\n",xmlfile);
		xmlFreeParserCtxt(ctx);
		return 0;
	}

	if (ctx->valid == 0) {
		fprintf(stderr,"Error validating %s\n",xmlfile);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		return 0;
	}

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		fprintf(stderr,"Error getting root element in file %s\n",xmlfile);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		return 0;
	}

	xmlDocDumpMemory(doc,(xmlChar **) xmlstring, xmlstringlength);
	xmlFreeDoc(doc);

	return 1;
}

int xmi_read_input_xml_from_string(char *xmlstring, struct xmi_input **input) {
	xmlDocPtr doc;
	xmlNodePtr root, subroot;
	xmlParserCtxtPtr ctx;

	LIBXML_TEST_VERSION


	if ((ctx=xmlNewParserCtxt()) == NULL) {
		fprintf(stderr,"xmlNewParserCtxt error\n");
		return 0;
	}

	if ((doc = xmlCtxtReadDoc(ctx, BAD_CAST xmlstring, "weirdness.xml" ,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		fprintf(stderr,"xmlCtxtReadFile error for %s\n",xmlstring);
		xmlFreeParserCtxt(ctx);
		return 0;
	}

	if (ctx->valid == 0) {
		fprintf(stderr,"Error validating %s\n",xmlstring);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		return 0;
	}

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		fprintf(stderr,"Error getting root element in file %s\n",xmlstring);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		return 0;
	}

	if (xmlStrcmp(root->name,(const xmlChar*) "xmimsim")) {
		fprintf(stderr,"XML document is %s of wrong type, expected xmimsim\n",xmlstring);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		return 0;
	}

	subroot= xmlFirstElementChild(root);

	//allocate memory for input
	*input = (struct xmi_input *) g_malloc(sizeof(struct xmi_input));




	while (subroot != NULL) {
		if (!xmlStrcmp(subroot->name,(const xmlChar *) "general")) {
			if (xmi_read_input_general(doc, subroot, &((**input).general)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "composition")) {
			if (xmi_read_input_composition(doc, subroot, &((**input).composition)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "geometry")) {
			if (xmi_read_input_geometry(doc, subroot, &((**input).geometry)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "excitation")) {
			if (xmi_read_input_excitation(doc, subroot, &((**input).excitation)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "absorbers")) {
			if (xmi_read_input_absorbers(doc, subroot, &((**input).absorbers)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "detector")) {
			if (xmi_read_input_detector(doc, subroot, &((**input).detector)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}
		}

		subroot = xmlNextElementSibling(subroot);
	}

	xmlFreeParserCtxt(ctx);
	xmlFreeDoc(doc);
	return 1;


}

int xmi_write_input_xml_svg(xmlDocPtr doc, xmlNodePtr subroot, struct xmi_input *input, char *name, int interaction,
double *channels, double maximum2 ) {

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


int xmi_read_output_xml(char *xmsofile, struct xmi_output **output) {

	xmlDocPtr doc;
	xmlNodePtr root;
	xmlParserCtxtPtr ctx;
	struct xmi_output *op = g_malloc(sizeof(struct xmi_output));

	LIBXML_TEST_VERSION

	if ((ctx=xmlNewParserCtxt()) == NULL) {
		fprintf(stderr,"xmlNewParserCtxt error\n");
		return 0;
	}

	if ((doc = xmlCtxtReadFile(ctx,xmsofile,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		fprintf(stderr,"xmlCtxtReadFile error for %s\n",xmsofile);
		xmlFreeParserCtxt(ctx);
		return 0;
	}

	if (ctx->valid == 0) {
		fprintf(stderr,"Error validating %s\n",xmsofile);
		xmlFreeDoc(doc);
		return 0;
	}
	xmlFreeParserCtxt(ctx);

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		fprintf(stderr,"Error getting root element in file %s\n",xmsofile);
		xmlFreeDoc(doc);
		return 0;
	}

	if (xmlStrcmp(root->name,(const xmlChar*) "xmimsim-results") != 0) {
		fprintf(stderr,"XML document is %s of wrong type, expected xmimsim-results\n",xmsofile);
		xmlFreeDoc(doc);
		return 0;
	}

	if (xmi_read_output_xml_body(doc, root, op, NULL, NULL) == 0)
		return 0;


	xmlFreeDoc(doc);

	*output = op;

	return 1;
}

int xmi_read_input_xml_body(xmlDocPtr doc, xmlNodePtr root, struct xmi_input *input) {
	xmlNodePtr subroot = xmlFirstElementChild(root);

	while (subroot != NULL) {
		if (!xmlStrcmp(subroot->name,(const xmlChar *) "general")) {
			if (xmi_read_input_general(doc, subroot, &(input->general)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "composition")) {
			if (xmi_read_input_composition(doc, subroot, &(input->composition)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "geometry")) {
			if (xmi_read_input_geometry(doc, subroot, &(input->geometry)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "excitation")) {
			if (xmi_read_input_excitation(doc, subroot, &(input->excitation)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "absorbers")) {
			if (xmi_read_input_absorbers(doc, subroot, &(input->absorbers)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "detector")) {
			if (xmi_read_input_detector(doc, subroot, &(input->detector)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}

		subroot = xmlNextElementSibling(subroot);
	}
	return 1;
}

int xmi_write_default_comments(xmlDocPtr doc, xmlNodePtr root_node) {
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

int xmi_read_output_xml_body(xmlDocPtr doc, xmlNodePtr root, struct xmi_output *op, int *step1, int *step2) {
	xmlNodePtr subroot;
	xmlChar *txt;

	xmlAttrPtr attr = root->properties;

	while (attr != NULL) {
		if (!xmlStrcmp(attr->name,(const xmlChar *) "version")) {
			txt =xmlNodeGetContent(attr->children);
			if(sscanf((const char *)txt,"%f",&op->version) != 1) {
				fprintf(stderr,"error reading in version of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		attr=attr->next;
	}
	//read step1 and step2 if necessary
	if (step1 != NULL || step2 != NULL) {
		attr = root->properties;
		while (attr != NULL) {
			if (xmlStrcmp(attr->name, BAD_CAST "step1") == 0) {
				txt = xmlNodeGetContent(attr->children);
				if(sscanf((const char *)txt,"%i",step1) != 1) {
					fprintf(stderr,"error reading in step1 attribute of xml file\n");
					return 0;
				}
				xmlFree(txt);
			}
			else if (xmlStrcmp(attr->name, BAD_CAST "step2") == 0 && step2 != NULL) {
				txt = xmlNodeGetContent(attr->children);
				if(sscanf((const char *)txt,"%i",step2) != 1) {
					fprintf(stderr,"error reading in step2 attribute of xml file\n");
					return 0;
				}
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
		xmlFreeDoc(doc);
        	return 0;
	}
	xpathObj = xmlXPathNodeEval(root, BAD_CAST "xmimsim-input", xpathCtx);
	if(xpathObj == NULL || xpathObj->nodesetval->nodeNr == 0) {
		fprintf(stderr,"Error: unable to evaluate xpath expression xmimsim-input\n");
		xmlXPathFreeContext(xpathCtx);
		xmlFreeDoc(doc);
		return 0;
	}

	op->input = (struct xmi_input *) g_malloc(sizeof(struct xmi_input));

	if (xmi_read_input_xml_body(doc, xpathObj->nodesetval->nodeTab[0], op->input) == 0)
		return 0;

	if (xmi_validate_input(op->input) != 0) {
		xmlFreeDoc(doc);
		fprintf(stderr, "Error validating input data\n");
		return 0;
	}
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
			if (xmi_read_output_spectrum(doc,subroot, op, 1) == 0) {
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
			if (xmi_read_output_spectrum(doc,subroot, op, 0) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "brute_force_history")) {
			if (xmi_read_output_history(doc,subroot, &op->brute_force_history, &op->nbrute_force_history) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "variance_reduction_history")) {
			if (xmi_read_output_history(doc,subroot, &op->var_red_history, &op->nvar_red_history) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		subroot = xmlNextElementSibling(subroot);
	}
	return 1;
}

int xmi_read_archive_xml(char *xmsafile, struct xmi_archive **archive) {

	xmlDocPtr doc;
	xmlNodePtr root;
	xmlParserCtxtPtr ctx;
	struct xmi_archive *ar = g_malloc(sizeof(struct xmi_archive));
	int step1, step2;
	int files_read = 0;

	LIBXML_TEST_VERSION

	if ((ctx=xmlNewParserCtxt()) == NULL) {
		fprintf(stderr,"xmlNewParserCtxt error\n");
		return 0;
	}

	if ((doc = xmlCtxtReadFile(ctx,xmsafile,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		fprintf(stderr,"xmlCtxtReadFile error for %s\n",xmsafile);
		xmlFreeParserCtxt(ctx);
		return 0;
	}

	if (ctx->valid == 0) {
		fprintf(stderr,"Error validating %s\n",xmsafile);
		xmlFreeDoc(doc);
		return 0;
	}
	xmlFreeParserCtxt(ctx);

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		fprintf(stderr,"Error getting root element in file %s\n",xmsafile);
		xmlFreeDoc(doc);
		return 0;
	}

	if (xmlStrcmp(root->name,(const xmlChar*) "xmimsim-archive") != 0) {
		fprintf(stderr,"XML document is %s of wrong type, expected xmimsim-archive\n",xmsafile);
		xmlFreeDoc(doc);
		return 0;
	}

	xmlAttrPtr attr = root->properties;
	xmlChar *txt;

	//in case it's not there, make sure the version is 0
	ar->version = 0.0;
	while (attr != NULL) {
		if (!xmlStrcmp(attr->name,(const xmlChar *) "version")) {
			txt =xmlNodeGetContent(attr->children);
			if(sscanf((const char *)txt,"%f",&ar->version) != 1) {
				fprintf(stderr,"error reading in version of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		attr=attr->next;
	}

	xmlNodePtr subroot;

	subroot = xmlFirstElementChild(root);

	ar->input = NULL;
	ar->output = NULL;
	ar->inputfiles = NULL;
	ar->outputfiles = NULL;
	ar->nsteps2 = 0;
	ar->xpath2 = NULL;

	while (subroot != NULL) {
		if (!xmlStrcmp(subroot->name,(const xmlChar *) "start_value1")) {
			txt = xmlNodeGetContent(subroot->children);
			if (sscanf((const char*) txt, "%lg", &ar->start_value1) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read start_value1\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "end_value1")) {
			txt = xmlNodeGetContent(subroot->children);
			if (sscanf((const char*) txt, "%lg", &ar->end_value1) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read end_value1\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "nsteps1")) {
			txt = xmlNodeGetContent(subroot->children);
			if (sscanf((const char*) txt, "%i", &ar->nsteps1) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read nsteps1\n");
				return 0;
			}
			xmlFree(txt);
			//allocate memory
			ar->output = g_malloc(sizeof(struct xmi_output**)*(ar->nsteps1+1));
			ar->input = g_malloc(sizeof(struct xmi_input**)*(ar->nsteps1+1));
			ar->inputfiles = g_malloc(sizeof(char**)*(ar->nsteps1+1));
			ar->outputfiles = g_malloc(sizeof(char**)*(ar->nsteps1+1));
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "xpath1")) {
			txt = xmlNodeGetContent(subroot->children);
			ar->xpath1 = g_strdup((char*) txt);
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "start_value2")) {
			txt = xmlNodeGetContent(subroot->children);
			if (sscanf((const char*) txt, "%lg", &ar->start_value2) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read start_value2\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "end_value2")) {
			txt = xmlNodeGetContent(subroot->children);
			if (sscanf((const char*) txt, "%lg", &ar->end_value2) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read end_value2\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "nsteps2")) {
			txt = xmlNodeGetContent(subroot->children);
			if (sscanf((const char*) txt, "%i", &ar->nsteps2) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read nsteps2\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "xpath2")) {
			txt = xmlNodeGetContent(subroot->children);
			ar->xpath2 = g_strdup((char*) txt);
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "xmimsim-results")) {
			struct xmi_output *output = g_malloc(sizeof(struct xmi_output));
			step1 = step2 = 0;
			if (xmi_read_output_xml_body(doc, subroot, output, &step1, &step2) == 0) {
				return 0;
			}
			if (step2 == 0) {
				ar->output[step1] = g_malloc(sizeof(struct xmi_output*)*(ar->nsteps2+1));
				ar->input[step1] = g_malloc(sizeof(struct xmi_input*)*(ar->nsteps2+1));
				ar->inputfiles[step1] = g_malloc(sizeof(char*)*(ar->nsteps2+1));
				ar->outputfiles[step1] = g_malloc(sizeof(char*)*(ar->nsteps2+1));
			}
			ar->output[step1][step2] = output;
			//fix links
			ar->input[step1][step2] = ar->output[step1][step2]->input;
			ar->inputfiles[step1][step2] = ar->output[step1][step2]->inputfile;
			ar->outputfiles[step1][step2] = ar->input[step1][step2]->general->outputfile;
			files_read++;
		}
		subroot = xmlNextElementSibling(subroot);
	}

	xmlFreeDoc(doc);

	if (files_read != (ar->nsteps1+1)*(ar->nsteps2+1)) {
		fprintf(stderr,"xmi_read_archive_xml: nfiles/nsteps mismatch\n");
		return 0;
	}


	*archive = ar;

	return 1;
}

int xmi_write_archive_xml(char *xmlfile, struct xmi_archive *archive) {
	xmlDocPtr doc = NULL;
	xmlNodePtr root_node = NULL;
	xmlDtdPtr dtd = NULL;
	xmlNodePtr nodePtr1;


	LIBXML_TEST_VERSION

	if ((doc = xmlNewDoc(BAD_CAST "1.0")) == NULL) {
		fprintf(stderr,"Error calling xmlNewDoc\n");
		return 0;
	}
	xmlThrDefIndentTreeOutput(2);
	xmlSetDocCompressMode(doc, 9);

	if ((dtd = xmlCreateIntSubset(doc, BAD_CAST  "xmimsim-archive", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd")) == NULL) {
		fprintf(stderr,"Error creating DTD\n");
		return 0;
	}

	root_node = xmlNewNode(NULL, BAD_CAST "xmimsim-archive");
	xmlDocSetRootElement(doc, root_node);

	if (xmi_write_default_comments(doc, root_node) == 0) {
		return 0;
	}

	//body
	xmi_new_child_printf(root_node, BAD_CAST "start_value1", "%g", archive->start_value1);
	xmi_new_child_printf(root_node, BAD_CAST "end_value1", "%g", archive->end_value1);
	xmi_new_child_printf(root_node, BAD_CAST "nsteps1", "%i", archive->nsteps1);
	xmi_new_child_printf(root_node, BAD_CAST "xpath1", "%s", archive->xpath1);
	if (archive->nsteps2 > 0) {
		xmi_new_child_printf(root_node, BAD_CAST "start_value2", "%g", archive->start_value2);
		xmi_new_child_printf(root_node, BAD_CAST "end_value2", "%g", archive->end_value2);
		xmi_new_child_printf(root_node, BAD_CAST "nsteps2", "%i", archive->nsteps2);
		xmi_new_child_printf(root_node, BAD_CAST "xpath2", "%s", archive->xpath2);
	}
	int i,j;

	for (i = 0 ; i <= archive->nsteps1 ; i++) {
		for (j = 0 ; j <= archive->nsteps2 ; j++) {
			nodePtr1 = xmlNewChild(root_node, NULL, BAD_CAST "xmimsim-results", NULL);
			if (xmi_write_output_xml_body(doc, nodePtr1, archive->output[i][j], i, j, 0) == 0) {
				return 0;
			}
		}
	}

	if (xmlSaveFileEnc(xmlfile,doc,NULL) == -1) {
		fprintf(stderr,"Could not write to %s\n",xmlfile);
		return 0;
	}
	xmlFreeDoc(doc);

	return 1;
}

int xmi_write_layer_xml_body(xmlDocPtr doc, xmlNodePtr subroot, struct xmi_layer *layers, int n_layers) {
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

int xmi_cmp_struct_xmi_energy_discrete(const void *a, const void *b) {
	double diff;

	diff = ((struct xmi_energy_discrete *)a)->energy - ((struct xmi_energy_discrete *)b)->energy;

	if (diff > 0.000000001)
		return 1;
	else if (diff < -0.000000001)
		return -1;
	return 0;
}

int xmi_cmp_struct_xmi_energy_continuous(const void *a, const void *b) {
	double diff;

	diff = ((struct xmi_energy_continuous *)a)->energy - ((struct xmi_energy_continuous *)b)->energy;

	if (diff > 0.000000001)
		return 1;
	else if (diff < -0.000000001)
		return -1;
	return 0;
}
