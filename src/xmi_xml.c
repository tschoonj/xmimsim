/*
Copyright (C) 2010-2011 Tom Schoonjans, Philip Brondeel and Laszlo Vincze

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
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xmlwriter.h>
#include <libxml/catalog.h>
#include <string.h>
#include <math.h>
#include <glib.h>
#include <libxslt/xslt.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>
#include <stdlib.h>
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

static int readLayerXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_layer *layer);
static int readGeneralXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_general **general);
static int readCompositionXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_composition **composition);
static int readGeometryXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_geometry **geometry);
static int readExcitationXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_excitation **excitation);
static int readAbsorbersXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_absorbers **absorbers);
static int readDetectorXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_detector **detector);
static int readSpectrumXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_output *output, int conv);
static int readHistoryXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_fluorescence_line_counts **history, int *nhistory);


static int xmi_write_input_xml_body(xmlTextWriterPtr writer, struct xmi_input *input); 
static int xmi_write_input_xml_svg(xmlTextWriterPtr writer, struct xmi_input *input, char *name, int interaction,  double *channels, double maximum); 
static int xmi_write_output_xml_body(xmlTextWriterPtr writer, struct xmi_output *output, int step1, int step2, int with_svg);
static int xmi_write_default_comments(xmlTextWriterPtr writer);
static int xmi_read_input_xml_body(xmlDocPtr doc, xmlNodePtr subroot, struct xmi_input *input);
static int xmi_read_output_xml_body(xmlDocPtr doc, xmlNodePtr subroot, struct xmi_output *output, int *step1, int *step2);
static int xmi_write_layer_xml_body(xmlTextWriterPtr writer, struct xmi_layer *layers, int n_layers);


static int write_start_element(xmlTextWriterPtr writer, char *element);
static int write_end_element(xmlTextWriterPtr writer, char *element);
static int write_value_element(xmlTextWriterPtr writer, char *element, float parm);
static int write_value_element_int(xmlTextWriterPtr writer, char *element, int parm);
static int write_char_element(xmlTextWriterPtr writer, char *element, char *parm);
static float e2c(double energy, double * channels, double * energies, int nchannels );
static float i2c(double intensity, double maximum_log, double minimum_log);



static gboolean xml_catalog_loaded = FALSE;

#ifdef G_OS_WIN32
#include "xmi_registry_win.h"

int xmi_xmlLoadCatalog() {
	char *catalog;
	int rv;
	
	if (xml_catalog_loaded)
		return 1;

	if (xmi_registry_win_query(XMI_REGISTRY_WIN_CATALOG,&catalog) == 0) {
		return 0;
	}

	if (xmlLoadCatalog((gchar *) catalog) != 0) {
		fprintf(stderr,"Could not load %s\n",(gchar *) catalog);
		rv=0;
	}
	else {
		rv=1;
		free(catalog);
	}

	xml_catalog_loaded = TRUE;

	return rv;

}
#elif defined(MAC_INTEGRATION)
#include "xmi_resources_mac.h"
#include <gtkosxapplication.h>
int xmi_xmlLoadCatalog() {
	int rv;
	
	if (xml_catalog_loaded)
		return 1;

	gchar *resource_path;
	resource_path = gtkosx_application_get_resource_path();
	resource_path = (gchar *) realloc(resource_path,sizeof(gchar *)*(strlen(resource_path)+2));
	strcat(resource_path,"/");

	const xmlChar uriStartString[] = "http://www.xmi.UGent.be/xml/";
	const xmlChar *rewritePrefix = (xmlChar*) g_filename_to_uri(resource_path,NULL,NULL);
	

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

	free(resource_path);


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

static int readSpectrumXML(xmlDocPtr doc, xmlNodePtr spectrumPtr, struct xmi_output *output, int conv) {
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
		fprintf(stderr,"readSpectrumXML: spectrum contains no channels\n");
		return 0;
	}
	else {
		//debug
		//fprintf(stdout,"nchannels: %i\n", nchannels);
	}

	channels_loc = (double **) malloc(sizeof(double *)* (output->ninteractions+1));
	for (i = 0 ; i <= output->ninteractions ; i++)
		channels_loc[i] = (double *) calloc(nchannels,sizeof(double));
		
	if (conv)
		output->channels_conv = channels_loc;
	else
		output->channels_unconv = channels_loc;
	
	channel_loc = 0;
	channelPtr = spectrumPtr->children;

	while (channelPtr != NULL) {
		if (!xmlStrcmp(channelPtr->name,(const xmlChar *) "channel")) {
			n_interactions_loc = (int) xmlChildElementCount(channelPtr);
			if (n_interactions_loc < 3) {
				fprintf(stderr,"readSpectrumXML: channel contains less than three entities\n");	
				return 0;
			}
			countsPtr = channelPtr->children;
			interaction_loc = 0;
			while (countsPtr != NULL) {
				if (!xmlStrcmp(countsPtr->name,(const xmlChar *) "counts")) {
					attr = countsPtr->properties;
					txt =xmlNodeListGetString(doc,attr->children,1);
					interaction_loc = (int) strtol((char *) txt, NULL, 10);
					xmlFree(txt);
					txt = xmlNodeListGetString(doc,countsPtr->children,1);	
					if (sscanf((const char*) txt, "%lg", &(channels_loc[interaction_loc][channel_loc])) !=1) {
						fprintf(stderr,"readSpectrumXML: could not read counts\n");
						return 0;
					}
					xmlFree(txt);
				}
				countsPtr = countsPtr->next;
			}
			channel_loc++;
		}
		channelPtr = channelPtr->next;
	}

	return 1;
}

static int readHistoryXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_fluorescence_line_counts **history, int *nhistory) {

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
	*history = (struct xmi_fluorescence_line_counts *) malloc(sizeof(struct xmi_fluorescence_line_counts)*(nchildren));
	history_loc = *history;
	linePtr = nodePtr->children;
	*nhistory = nchildren;


	counter = 0;
	while(linePtr != NULL) {
		//read attributes
		attr = linePtr->properties;
		while (attr != NULL) {
			if (!xmlStrcmp(attr->name,(const xmlChar *) "atomic_number")) {
				txt =xmlNodeListGetString(doc,attr->children,1);
				if(sscanf((const char *)txt,"%i",&(history_loc[counter].atomic_number)) != 1) {
					fprintf(stderr,"readHistoryXML: error reading in atomic_number\n");
					return 0;
				}
				xmlFree(txt);
			}
			else if (!xmlStrcmp(attr->name,(const xmlChar *) "total_counts")) {
				txt =xmlNodeListGetString(doc,attr->children,1);
				if(sscanf((const char *)txt,"%lg",&(history_loc[counter].total_counts)) != 1) {
					fprintf(stderr,"readHistoryXML: error reading in total_counts\n");
					return 0;
				}
				xmlFree(txt);
			}


			attr = attr->next;
		}
		//determine number of children
		nchildren = xmlChildElementCount(linePtr);
		history_loc[counter].n_lines = nchildren;
		history_loc[counter].lines = (struct xmi_fluorescence_line *) malloc(sizeof(struct xmi_fluorescence_line)*nchildren);
			
		countsPtr = linePtr->children;
		counter2 = 0;
		while (countsPtr) {
			attr = countsPtr->properties;	
			while (attr) {
				if (!xmlStrcmp(attr->name,(const xmlChar *) "type")) {
					txt =xmlNodeListGetString(doc,attr->children,1);
					if(sscanf((const char *)txt,"%s",history_loc[counter].lines[counter2].line_type) != 1) {
						fprintf(stderr,"readHistoryXML: error reading in line_type\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(attr->name,(const xmlChar *) "energy")) {
					txt =xmlNodeListGetString(doc,attr->children,1);
					if(sscanf((const char *)txt,"%lg",&(history_loc[counter].lines[counter2].energy)) != 1) {
						fprintf(stderr,"readHistoryXML: error reading in energy\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(attr->name,(const xmlChar *) "total_counts")) {
					txt =xmlNodeListGetString(doc,attr->children,1);
					if(sscanf((const char *)txt,"%lg",&(history_loc[counter].lines[counter2].total_counts)) != 1) {
						fprintf(stderr,"readHistoryXML: error reading in total_counts lvl2\n");
						return 0;
					}
					xmlFree(txt);
				}
			
				attr = attr->next;
			}
			counter3 = 0;
			subcountsPtr = countsPtr->children;
			nchildren = xmlChildElementCount(countsPtr);
			history_loc[counter].lines[counter2].interactions = (struct xmi_counts *) malloc(sizeof(struct xmi_counts)*nchildren);
			history_loc[counter].lines[counter2].n_interactions = nchildren;
			while (subcountsPtr) {
				attr = subcountsPtr->properties;
				while (attr) {
					if (!xmlStrcmp(attr->name,(const xmlChar *) "interaction_number")) {
					txt =xmlNodeListGetString(doc,attr->children,1);
					if(sscanf((const char *)txt,"%i",&(history_loc[counter].lines[counter2].interactions[counter3].interaction_number)) != 1) {
						fprintf(stderr,"readHistoryXML: error reading in interaction_number\n");
						return 0;
					}
					xmlFree(txt);
					}

					attr = attr->next;
				}
				txt = xmlNodeListGetString(doc,subcountsPtr->children,1);	
				if (sscanf((const char*) txt, "%lg",&(history_loc[counter].lines[counter2].interactions[counter3].counts)) !=1) {
					fprintf(stderr,"readHistoryXML: could not read counts\n");
					return 0;
				}
				xmlFree(txt);


				subcountsPtr = subcountsPtr->next;
				counter3++;
			}

			counter2++;	
			countsPtr = countsPtr->next;
		}
		counter++;
		linePtr = linePtr->next;
	} 

	return 1;
}
static int readGeneralXML(xmlDocPtr doc, xmlNodePtr node, struct xmi_general **general) {
	xmlNodePtr subnode;
	xmlChar *txt;
	xmlAttrPtr attr;

	//allocate memory
	*general = (struct xmi_general *) malloc(sizeof(struct xmi_general));



	//version
	attr=node->properties;
	while (attr != NULL) {
		if (!xmlStrcmp(attr->name,(const xmlChar *) "version")) {
			txt =xmlNodeListGetString(doc,attr->children,1);
			if(sscanf((const char *)txt,"%f",&((*general)->version)) != 1) {
				fprintf(stderr,"error reading in version of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		attr=attr->next;
	}

	subnode = node->children;

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "outputfile")) {
			//outputfile
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if (txt == NULL) {
				(*general)->outputfile = strdup("");
			}
			else {
				(*general)->outputfile = (char *) strdup((char *) txt); 
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_photons_interval")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%li",&((*general)->n_photons_interval)) != 1) {
				fprintf(stderr,"error reading in n_photons_interval of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_photons_line")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%li",&((*general)->n_photons_line)) != 1) {
				fprintf(stderr,"error reading in n_photons_line of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_interactions_trajectory")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%i",&((*general)->n_interactions_trajectory)) != 1) {
				fprintf(stderr,"error reading in n_interactions_trajectory of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "comments")) {
			//comments
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if (txt == NULL) {
				(*general)->comments = strdup("");
			}
			else {
				(*general)->comments = (char *) strdup((char *) txt); 
			}
			xmlFree(txt);
		}
		subnode=subnode->next;
	}

	return 1;

}


static int readCompositionXML(xmlDocPtr doc, xmlNodePtr node, struct xmi_composition **composition) {
	xmlNodePtr subnode;
	xmlChar *txt;

	//allocate memory
	*composition = (struct xmi_composition *) malloc(sizeof(struct xmi_composition));

	(*composition)->n_layers = 0;
	(*composition)->layers = NULL;

	subnode = node->children;

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "layer")){
			(*composition)->layers = (struct xmi_layer *) realloc((*composition)->layers,sizeof(struct xmi_layer)*++((*composition)->n_layers)); 
			//long live C and its deliciously complicated syntax :-)
			if (readLayerXML(doc, subnode, (*composition)->layers+(*composition)->n_layers-1) == 0) {
				return 0;
			}	
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "reference_layer")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
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
		subnode=subnode->next;
	}

	return 1;
}

static int readGeometryXML(xmlDocPtr doc, xmlNodePtr node, struct xmi_geometry **geometry) {
	xmlNodePtr subnode,subsubnode;
	xmlChar *txt;
	

	//allocate memory
	*geometry= (struct xmi_geometry *) malloc(sizeof(struct xmi_geometry));

	subnode = node->children;

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "d_sample_source")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->d_sample_source)) != 1) {
				fprintf(stderr,"error reading in d_sample_source of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_sample_orientation")) {
			subsubnode = subnode->children;	
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "x")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_sample_orientation[0])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_sample_orientation[1])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_sample_orientation[2])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = subsubnode->next;
			}

		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "p_detector_window")) {
			subsubnode = subnode->children;	
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "x")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->p_detector_window[0])) != 1) {
						fprintf(stderr,"error reading in p_detector_window x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->p_detector_window[1])) != 1) {
						fprintf(stderr,"error reading in p_detector_window y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->p_detector_window[2])) != 1) {
						fprintf(stderr,"error reading in p_detector_window z of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = subsubnode->next;
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "n_detector_orientation")) {
			subsubnode = subnode->children;	
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "x")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_detector_orientation[0])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_detector_orientation[1])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->n_detector_orientation[2])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation z of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = subsubnode->next;
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "area_detector")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->area_detector)) != 1) {
				fprintf(stderr,"error reading in area_detector of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "collimator_height")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->collimator_height)) != 1) {
				fprintf(stderr,"error reading in collimator_height of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "collimator_diameter")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->collimator_diameter)) != 1) {
				fprintf(stderr,"error reading in collimator_diameter of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "d_source_slit")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*geometry)->d_source_slit)) != 1) {
				fprintf(stderr,"error reading in d_source_slit of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "slit_size")) {
			subsubnode = subnode->children;	
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "slit_size_x")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->slit_size_x)) != 1) {
						fprintf(stderr,"error reading in slit_size_x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "slit_size_y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&((*geometry)->slit_size_y)) != 1) {
						fprintf(stderr,"error reading in slit_size_y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = subsubnode->next;
			}
		}
		subnode = subnode->next;
	}
	return 1;
}

static int readExcitationXML(xmlDocPtr doc, xmlNodePtr node, struct xmi_excitation **excitation) {
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
	
	*excitation = (struct xmi_excitation *) malloc(sizeof(struct xmi_excitation));

	(*excitation)->n_discrete = 0;
	(*excitation)->discrete = NULL;
	(*excitation)->n_continuous = 0;
	(*excitation)->continuous = NULL;


	subnode = node->children;

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar*) "discrete")) {
			subsubnode = subnode->children;
			distribution_type = XMI_DISCRETE_MONOCHROMATIC;
			scale_parameter = 0.0;
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "energy")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(energy)) != 1) {
						fprintf(stderr,"error reading in discrete energy of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "horizontal_intensity")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(horizontal_intensity)) != 1) {
						fprintf(stderr,"error reading in discrete horizontal_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "vertical_intensity")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(vertical_intensity)) != 1) {
						fprintf(stderr,"error reading in discrete vertical_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_x")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(sigma_x)) != 1) {
						fprintf(stderr,"error reading in sigma_x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_xp")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(sigma_xp)) != 1) {
						fprintf(stderr,"error reading in sigma_xp of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(sigma_y)) != 1) {
						fprintf(stderr,"error reading in sigma_y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_yp")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
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
							txt = xmlNodeListGetString(doc,attr->children,1);
							if (strcmp((char *) txt, "monochromatic") == 0) {
								distribution_type = XMI_DISCRETE_MONOCHROMATIC;
								xmlFree(txt);
							}
							else if (strcmp((char *) txt, "gaussian") == 0) {
								distribution_type = XMI_DISCRETE_GAUSSIAN;
								xmlFree(txt);
								//read scale_parameter value
								txt = xmlNodeListGetString(doc,subsubnode->children,1);
								if(sscanf((const char *)txt,"%lg",&scale_parameter) != 1) {
									fprintf(stderr,"error reading in scale_parameter of xml file\n");
									return 0;
								}
								xmlFree(txt);
							}
							else if (strcmp((char *) txt, "lorentzian") == 0) {
								distribution_type = XMI_DISCRETE_LORENTZIAN;
								xmlFree(txt);
								//read scale_parameter value
								txt = xmlNodeListGetString(doc,subsubnode->children,1);
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
				subsubnode = subsubnode->next;
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
				(*excitation)->discrete = (struct xmi_energy_discrete *) realloc((*excitation)->discrete,++((*excitation)->n_discrete)*sizeof(struct xmi_energy_discrete));
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
			subsubnode = subnode->children;
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "energy")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(energy)) != 1) {
						fprintf(stderr,"error reading in continuous energy of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "horizontal_intensity")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(horizontal_intensity)) != 1) {
						fprintf(stderr,"error reading in continuous horizontal_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "vertical_intensity")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(vertical_intensity)) != 1) {
						fprintf(stderr,"error reading in continuous vertical_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_x")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(sigma_x)) != 1) {
						fprintf(stderr,"error reading in sigma_x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_xp")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(sigma_xp)) != 1) {
						fprintf(stderr,"error reading in sigma_xp of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(sigma_y)) != 1) {
						fprintf(stderr,"error reading in sigma_y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_yp")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",&(sigma_yp)) != 1) {
						fprintf(stderr,"error reading in sigma_yp of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = subsubnode->next;
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
				(*excitation)->continuous = (struct xmi_energy_continuous *) realloc((*excitation)->continuous,++((*excitation)->n_continuous)*sizeof(struct xmi_energy_continuous));
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
		subnode = subnode->next;
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


static int readAbsorbersXML(xmlDocPtr doc, xmlNodePtr node, struct xmi_absorbers **absorbers) {

	xmlNodePtr subnode,subsubnode;


	*absorbers = (struct xmi_absorbers *) malloc(sizeof(struct xmi_absorbers));

	(*absorbers)->n_exc_layers = 0;
	(*absorbers)->exc_layers = NULL;
	(*absorbers)->n_det_layers = 0;
	(*absorbers)->det_layers = NULL;


	subnode = node->children;

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "excitation_path")) {
			subsubnode = subnode->children;
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "layer")) {
					(*absorbers)->exc_layers = realloc((*absorbers)->exc_layers,sizeof(struct xmi_layer)*++(*absorbers)->n_exc_layers);
					if (readLayerXML(doc, subsubnode, (*absorbers)->exc_layers+(*absorbers)->n_exc_layers-1) == 0) {
						return 0;
					}
				}
				subsubnode = subsubnode->next;
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "detector_path")) {
			subsubnode = subnode->children;
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "layer")) {
					(*absorbers)->det_layers = realloc((*absorbers)->det_layers,sizeof(struct xmi_layer)*++(*absorbers)->n_det_layers);
					if (readLayerXML(doc, subsubnode, (*absorbers)->det_layers+(*absorbers)->n_det_layers-1) == 0) {
						return 0;
					}
				}
				subsubnode = subsubnode->next;
			}
		}
	
		subnode = subnode->next;
	}

	return 1;
}



static int readDetectorXML(xmlDocPtr doc, xmlNodePtr node, struct xmi_detector **detector) {
	xmlNodePtr subnode,subsubnode;
	xmlChar *txt;


	
	*detector= (struct xmi_detector *) malloc(sizeof(struct xmi_detector));

	(*detector)->n_crystal_layers = 0;
	(*detector)->crystal_layers = NULL;
	(*detector)->nchannels = 2048;

	subnode = node->children;

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "detector_type")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
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
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*detector)->live_time)) != 1) {
				fprintf(stderr,"error reading in live_time of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "pulse_width")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*detector)->pulse_width)) != 1) {
				fprintf(stderr,"error reading in pulse_width of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "gain")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*detector)->gain)) != 1) {
				fprintf(stderr,"error reading in gain of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "zero")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*detector)->zero)) != 1) {
				fprintf(stderr,"error reading in zero of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "fano")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*detector)->fano)) != 1) {
				fprintf(stderr,"error reading in fano of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "noise")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&((*detector)->noise)) != 1) {
				fprintf(stderr,"error reading in noise of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "nchannels")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%i",&((*detector)->nchannels)) != 1) {
				fprintf(stderr,"error reading in nchannels of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "crystal")) {
			subsubnode = subnode->children;
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "layer")) {
					(*detector)->crystal_layers = realloc((*detector)->crystal_layers,sizeof(struct xmi_layer)*++(*detector)->n_crystal_layers);
					if (readLayerXML(doc, subsubnode, (*detector)->crystal_layers+(*detector)->n_crystal_layers-1) == 0) {
						return 0;
					}
				}
				subsubnode = subsubnode->next;
			}
	

		}
		subnode= subnode->next;
	}

	return 1;
}






static int readLayerXML(xmlDocPtr doc, xmlNodePtr node, struct xmi_layer *layer) {
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


	subnode = node->children;

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "element")) {
			n_elements++;
			Z = (int *) realloc(Z,sizeof(int)*n_elements);
			weight = (double *) realloc(weight,sizeof(double)*n_elements);
			subsubnode = subnode->children;
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "atomic_number")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%i",Z+n_elements-1) != 1) {
						fprintf(stderr,"error reading in atomic_number of xml file\n");
						return 0;
					}
					xmlFree(txt);
					
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "weight_fraction")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lg",weight+n_elements-1) != 1) {
						fprintf(stderr,"error reading in weight_fraction of xml file\n");
						return 0;
					}
					//special case: if weight is equal to zero, skip the element
					//negative values will get caught by xmi_validate
					//normalization will be performed by xmi_input_C2F
					if (fabs(weight[n_elements-1]) < 1E-20) {
						xrlFree(txt);
						Z = realloc(Z, sizeof(int)*--n_elements);
						weight = realloc(weight, sizeof(double)*n_elements);
						break;
					}
					//divide weight by 100 to get rid of the percentages
					weight[n_elements-1] /= 100.0;
					xmlFree(txt);
				}
				subsubnode = subsubnode->next;
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "density")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&(layer->density)) != 1) {
				fprintf(stderr,"error reading in density of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "thickness")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lg",&(layer->thickness)) != 1) {
				fprintf(stderr,"error reading in thickness of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		subnode = subnode->next;

	}

	//sort!
	sorted_Z_ind = xmi_sort_idl_int(Z, n_elements);
	layer->n_elements = n_elements;
	layer->Z = (int *) malloc(sizeof(int)*n_elements);
	layer->weight = (double*) malloc(sizeof(double)*n_elements);
	for (i = 0 ; i < n_elements ; i++) {
		layer->Z[i] = Z[sorted_Z_ind[i]];
		layer->weight[i] = weight[sorted_Z_ind[i]];
	}

	free(sorted_Z_ind);
	free(Z);
	free(weight);

	return 1;
}


int xmi_read_input_xml (char *xmlfile, struct xmi_input **input) {

	xmlDocPtr doc;
	xmlNodePtr root, subroot;
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

	subroot= root->children;

	//allocate memory for input
	*input = (struct xmi_input *) malloc(sizeof(struct xmi_input));


	if (xmi_read_input_xml_body(doc, subroot, *input) == 0)
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
	xmlTextWriterPtr writer;
	xmlDocPtr doc;
	char version[100];
	int i,j;
	char buffer[1024];
	int xmlstringlength;



	LIBXML_TEST_VERSION


	if ((writer = xmlNewTextWriterDoc(&doc,0)) == NULL) {
		fprintf(stderr,"Error calling xmlNewTextWriterDoc\n");
		return 0;
	}
	xmlTextWriterSetIndent(writer,2);
	if (xmlTextWriterStartDocument(writer, NULL, NULL, NULL) < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartDocument\n");
		return 0;
	}
	if (xmlTextWriterStartDTD(writer,BAD_CAST  "xmimsim", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd") < 0 ) {
		fprintf(stderr,"Error starting DTD\n");
		return 0;
	}

	if (xmlTextWriterEndDTD(writer) < 0) {
		fprintf(stderr,"Error ending DTD\n");
		return 0;
	}

	if (xmlTextWriterStartElement(writer,BAD_CAST "xmimsim") < 0) {
		fprintf(stderr,"Error writing xmimsimtag\n");
		return 0;
	}

	if (xmi_write_input_xml_body(writer, input) == 0)
		return 0;
	
	
	//end it
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending xmimsim\n");
		return 0;
	}

	if (xmlTextWriterEndDocument(writer) < 0) {
		fprintf(stderr,"Error ending document\n");
		return 0;
	}

	xmlFreeTextWriter(writer);
	xmlDocDumpMemory(doc,(xmlChar **) xmlstring, &xmlstringlength);
	xmlFreeDoc(doc);

	return 1;

}

int xmi_write_input_xml(char *xmlfile, struct xmi_input *input) {

	xmlTextWriterPtr writer;
	xmlDocPtr doc;
	char version[100];
	int i,j;
	char buffer[1024];



	LIBXML_TEST_VERSION


	if ((writer = xmlNewTextWriterDoc(&doc,0)) == NULL) {
		fprintf(stderr,"Error calling xmlNewTextWriterDoc\n");
		return 0;
	}
	xmlTextWriterSetIndent(writer,2);
	if (xmlTextWriterStartDocument(writer, NULL, NULL, NULL) < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartDocument\n");
		return 0;
	}
	if (xmlTextWriterStartDTD(writer,BAD_CAST  "xmimsim", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd") < 0 ) {
		fprintf(stderr,"Error starting DTD\n");
		return 0;
	}

	if (xmlTextWriterEndDTD(writer) < 0) {
		fprintf(stderr,"Error ending DTD\n");
		return 0;
	}

	if (xmi_write_default_comments(writer) == 0) {
		return 0;
	}

	if (xmlTextWriterStartElement(writer,BAD_CAST "xmimsim") < 0) {
		fprintf(stderr,"Error writing xmimsimtag\n");
		return 0;
	}

	if (xmi_write_input_xml_body(writer, input) == 0)
		return 0;
	
	
	//end it
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending xmimsim\n");
		return 0;
	}

	if (xmlTextWriterEndDocument(writer) < 0) {
		fprintf(stderr,"Error ending document\n");
		return 0;
	}

	xmlFreeTextWriter(writer);
	if (xmlSaveFileEnc(xmlfile,doc,NULL) == -1) {
		fprintf(stderr,"Could not write to %s\n",xmlfile);
		return 0;
	}
	xmlFreeDoc(doc);

	return 1;

}


static int xmi_write_output_xml_body(xmlTextWriterPtr writer, struct xmi_output *output, int step1, int step2, int with_svg) {

	char detector_type[20];
	int i,j,k;
	char buffer[1024];
	double *maxima;
	double gl_conv_max;
	double gl_unconv_max;
	double counts_sum, counts_temp;



	LIBXML_TEST_VERSION





	if (xmlTextWriterStartElement(writer,BAD_CAST "xmimsim-results") < 0) {
		fprintf(stderr,"Error writing xmimsim-results tag\n");
		return 0;
	}

	if (xmlTextWriterWriteFormatAttribute(writer, BAD_CAST "version","%.1f",output->input->general->version) < 0) {
		fprintf(stderr,"Error at xmlTextWriterWriteFormatAttribute\n");
		return 0;
	}

	if (step1 != -1 && step2 != -1) {
		if (xmlTextWriterWriteFormatAttribute(writer, BAD_CAST "step1","%i", step1) < 0) {
			fprintf(stderr,"Error at xmlTextWriterWriteFormatAttribute\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatAttribute(writer, BAD_CAST "step2","%i", step2) < 0) {
			fprintf(stderr,"Error at xmlTextWriterWriteFormatAttribute\n");
			return 0;
		}
	}

	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "inputfile","%s",output->inputfile) < 0) {
		fprintf(stderr,"Error writing inputfile\n");
		return 0;
	}

	//spectrum convoluted
	if (xmlTextWriterStartElement(writer, BAD_CAST "spectrum_conv") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement spectrum_conv\n");
		return 0;
	}

	for (j = 0 ; j < output->input->detector->nchannels ; j++) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "channel") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement channel\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "channelnr","%i",j) < 0) {
			fprintf(stderr,"Error writing channelnr\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "energy","%g",output->input->detector->gain*j+output->input->detector->zero) < 0) {
			fprintf(stderr,"Error writing energy\n");
			return 0;
		}

		for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {

			if (xmlTextWriterStartElement(writer, BAD_CAST "counts") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement counts\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "interaction_number","%i",i) < 0) {
				fprintf(stderr,"Error writing interaction_number attribute\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatString(writer,"%g",output->channels_conv[i][j]) < 0) {
				fprintf(stderr,"Error writing counts\n");
				return 0;
			}

			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error ending counts\n");
				return 0;
			}
		}
		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error ending channel\n");
			return 0;
		}
	}	


	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending spectrum_conv\n");
		return 0;
	}

	//spectrum unconvoluted
	if (xmlTextWriterStartElement(writer, BAD_CAST "spectrum_unconv") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement spectrum_unconv\n");
		return 0;
	}

	for (j = 0 ; j < output->input->detector->nchannels ; j++) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "channel") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement channel\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "channelnr","%i",j) < 0) {
			fprintf(stderr,"Error writing channelnr\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "energy","%g",output->input->detector->gain*j+output->input->detector->zero) < 0) {
			fprintf(stderr,"Error writing energy\n");
			return 0;
		}
		for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {
			if (xmlTextWriterStartElement(writer, BAD_CAST "counts") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement counts\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "interaction_number","%i",i) < 0) {
				fprintf(stderr,"Error writing interaction_number attribute\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatString(writer,"%g",output->channels_unconv[i][j]) < 0) {
				fprintf(stderr,"Error writing counts\n");
				return 0;
			}

			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error ending counts\n");
				return 0;
			}
		}
		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error ending channel\n");
			return 0;
		}
	}	


	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending spectrum_unconv\n");
		return 0;
	}

	if (xmlTextWriterStartElement(writer, BAD_CAST "brute_force_history") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement brute_force_history\n");
		return 0;
	}

	char *symbol;
	//Z loop
	for (i = 0 ; i < output->nbrute_force_history; i++) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "fluorescence_line_counts") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement fluorescence_line_counts\n");
			return 0;
		}
		//two attributes: atomic_number and element
		if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "atomic_number","%i",output->brute_force_history[i].atomic_number) < 0) {
			fprintf(stderr,"Error writing atomic_number\n");
			return 0;
		}
		symbol = AtomicNumberToSymbol(output->brute_force_history[i].atomic_number);
		if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "symbol","%s",symbol) < 0) {
       			fprintf(stderr,"Error writing symbol\n");                     
			return 0;
		} 
		xrlFree(symbol);
		if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "total_counts","%g", output->brute_force_history[i].total_counts) < 0) {
       			fprintf(stderr,"Error writing total_counts\n");                     
			return 0;
		} 
		//start with the different lines
		//line loop
		for (j = 0 ; j < output->brute_force_history[i].n_lines ; j++) {
			if (xmlTextWriterStartElement(writer, BAD_CAST "fluorescence_line") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement fluorescence_line\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "type","%s", output->brute_force_history[i].lines[j].line_type) < 0) {
				fprintf(stderr,"Error writing type\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "energy","%g",output->brute_force_history[i].lines[j].energy) < 0) {
				fprintf(stderr,"Error writing energy\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "total_counts","%g",output->brute_force_history[i].lines[j].total_counts) < 0) {
       				fprintf(stderr,"Error writing total_counts\n");                     
				return 0;
			} 
			//interactions loop
			for (k = 0 ; k < output->brute_force_history[i].lines[j].n_interactions ; k++) {
				if (xmlTextWriterStartElement(writer, BAD_CAST "counts") < 0) {
					fprintf(stderr,"Error at xmlTextWriterStartElement counts\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "interaction_number","%i",output->brute_force_history[i].lines[j].interactions[k].interaction_number) < 0) {
					fprintf(stderr,"Error writing interaction_number\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatString(writer,"%g",output->brute_force_history[i].lines[j].interactions[k].counts) < 0) {
					fprintf(stderr,"Error writing counts\n");
					return 0;
				}
				if (xmlTextWriterEndElement(writer) < 0) {
					fprintf(stderr,"Error ending counts\n");
					return 0;
				}
			}//interactions loop
			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error ending fluorescence_line\n");
				return 0;
			}
	
		}//line loop
	
		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error ending fluorescence_line_counts\n");
			return 0;
		}
	
	}//Z loop
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending brute_force_history\n");
		return 0;
	}


	//variance reduction history
	if (xmlTextWriterStartElement(writer, BAD_CAST "variance_reduction_history") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement variance_reduction_history\n");
		return 0;
		}

	if (output->var_red_history == NULL)
		goto after_var_red_history;

	//Z loop
	for (i = 0 ; i < output->nvar_red_history; i++) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "fluorescence_line_counts") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement fluorescence_line_counts\n");
			return 0;
		}
		//two attributes: atomic_number and element
		if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "atomic_number","%i",output->var_red_history[i].atomic_number) < 0) {
			fprintf(stderr,"Error writing atomic_number\n");
			return 0;
		}
		symbol = AtomicNumberToSymbol(output->var_red_history[i].atomic_number);
		if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "symbol","%s", symbol) < 0) {
       			fprintf(stderr,"Error writing symbol\n");                     
			return 0;
		} 
		xrlFree(symbol);
		if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "total_counts","%g", output->var_red_history[i].total_counts) < 0) {
       			fprintf(stderr,"Error writing total_counts\n");                     
			return 0;
		} 
		//start with the different lines
		//line loop
		for (j = 0 ; j < output->var_red_history[i].n_lines ; j++) {
			if (xmlTextWriterStartElement(writer, BAD_CAST "fluorescence_line") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement fluorescence_line\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "type","%s", output->var_red_history[i].lines[j].line_type) < 0) {
				fprintf(stderr,"Error writing type\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "energy","%g",output->var_red_history[i].lines[j].energy) < 0) {
				fprintf(stderr,"Error writing energy\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "total_counts","%g",output->var_red_history[i].lines[j].total_counts) < 0) {
       				fprintf(stderr,"Error writing total_counts\n");                     
				return 0;
			} 
			//interactions loop
			for (k = 0 ; k < output->var_red_history[i].lines[j].n_interactions ; k++) {
				if (xmlTextWriterStartElement(writer, BAD_CAST "counts") < 0) {
					fprintf(stderr,"Error at xmlTextWriterStartElement counts\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "interaction_number","%i",output->var_red_history[i].lines[j].interactions[k].interaction_number) < 0) {
					fprintf(stderr,"Error writing interaction_number\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatString(writer,"%g",output->var_red_history[i].lines[j].interactions[k].counts) < 0) {
					fprintf(stderr,"Error writing counts\n");
					return 0;
				}
				if (xmlTextWriterEndElement(writer) < 0) {
					fprintf(stderr,"Error ending counts\n");
					return 0;
				}
			}//interactions loop
			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error ending fluorescence_line\n");
				return 0;
			}
	
		}//line loop
	
		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error ending fluorescence_line_counts\n");
			return 0;
		}
	
	}//Z loop

after_var_red_history:
	
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending variance_reduction_history\n");
		return 0;
	}

	//write inputfile
	if (xmlTextWriterStartElement(writer,BAD_CAST "xmimsim-input") < 0) {
		fprintf(stderr,"Error writing xmimsim tag\n");
		return 0;
	}

	if (xmi_write_input_xml_body(writer, output->input) == 0)
		return 0;

	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending xmimsim-input\n");
		return 0;
	}
	if (with_svg == 0) {
		goto after_svg;
	}
	


	//write svg stuff
	if (xmlTextWriterStartElement(writer, BAD_CAST "svg_graphs") < 0) {
		fprintf(stderr,"Error writing svg_graphs tag\n");
		return 0;
	}

        // calculate global channel max for conv
	maxima = (double *) malloc(sizeof(double)*(output->input->general->n_interactions_trajectory+1));
	maxima[0]=0.0;
	for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {
		maxima[i]=xmi_maxval_double(output->channels_conv[i], output->input->detector->nchannels);
	}
	gl_conv_max = xmi_maxval_double(maxima,output->input->general->n_interactions_trajectory+1);
        free(maxima); maxima = NULL;

        // calculate global channel max for unconv
	maxima = (double *) malloc(sizeof(double)*(output->input->general->n_interactions_trajectory+1));
	maxima[0]=0.0;
	for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {
		maxima[i]=xmi_maxval_double(output->channels_unconv[i], output->input->detector->nchannels);
	}
	gl_unconv_max = xmi_maxval_double(maxima,output->input->general->n_interactions_trajectory+1);
	free(maxima); maxima = NULL;


        //write svg_graph lines
	for (i = (output->use_zero_interactions == 1 ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {

		//convoluted first
	
		if (xmi_write_input_xml_svg(writer, output->input, "convoluted", i, output->channels_conv[i], gl_conv_max) == 0) {
			fprintf(stderr,"Error in xmi_write_input_xml_svg\n");
			return 0;
		}

		//unconvoluted second

		if (xmi_write_input_xml_svg(writer, output->input, "unconvoluted", i, output->channels_unconv[i], gl_unconv_max ) == 0) {
			fprintf(stderr,"Error in xmi_write_input_xml_svg\n");
			return 0;
		}

	}

	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending svg_graphs tag\n");
		return 0;
	}

	//end it
after_svg:
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending xmimsim-results\n");
		return 0;
	}

	return 1;
}

int xmi_write_output_xml(char *xmlfile, struct xmi_output *output) {
	xmlDocPtr doc;
	xmlTextWriterPtr writer;


	LIBXML_TEST_VERSION

	if ((writer = xmlNewTextWriterDoc(&doc,0)) == NULL) {
		fprintf(stderr,"Error calling xmlNewTextWriterDoc\n");
		return 0;
	}
	xmlTextWriterSetIndent(writer,2);
	if (xmlTextWriterStartDocument(writer, NULL, NULL, NULL) < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartDocument\n");
		return 0;
	}
	if (xmlTextWriterStartDTD(writer,BAD_CAST  "xmimsim-results", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd") < 0 ) {
		fprintf(stderr,"Error starting DTD\n");
		return 0;
	}

	if (xmlTextWriterEndDTD(writer) < 0) {
		fprintf(stderr,"Error ending DTD\n");
		return 0;
	}
	
	if (xmi_write_default_comments(writer) == 0) {
		return 0;
	}

	if(xmi_write_output_xml_body(writer, output, -1, -1, 1) == 0){
		return 0;
	}

	if (xmlTextWriterEndDocument(writer) < 0) {
		fprintf(stderr,"Error ending document\n");
		return 0;
	}

	xmlFreeTextWriter(writer);

	if (xmlSaveFileEnc(xmlfile,doc,NULL) == -1) {
		fprintf(stderr,"Could not write to %s\n",xmlfile);
		return 0;
	}
	xmlFreeDoc(doc);

	return 1;

}


static int xmi_write_input_xml_body(xmlTextWriterPtr writer, struct xmi_input *input) {
	int i,j;
	char detector_type[20];


	//general
	if (xmlTextWriterStartElement(writer, BAD_CAST "general") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}

	if (xmlTextWriterWriteFormatAttribute(writer, BAD_CAST "version", "%.1f",input->general->version) < 0) {
		fprintf(stderr,"Error at xmlTextWriterWriteAttribute\n");
		return 0;
	}

	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "outputfile","%s",(xmlChar *) input->general->outputfile) < 0) {
		fprintf(stderr,"Error writing outputfile\n");
		return 0;
	}

	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "n_photons_interval","%li",input->general->n_photons_interval) < 0) {
		fprintf(stderr,"Error writing n_photons_interval\n");
		return 0;
	}

	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "n_photons_line","%li",input->general->n_photons_line) < 0) {
		fprintf(stderr,"Error writing n_photons_line\n");
		return 0;
	}

	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "n_interactions_trajectory","%i",input->general->n_interactions_trajectory) < 0) {
		fprintf(stderr,"Error writing n_interactions_trajectory\n");
		return 0;
	}

	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "comments","%s",(xmlChar *) input->general->comments) < 0) {
		fprintf(stderr,"Error writing comments\n");
		return 0;
	}

	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error calling xmlTextWriterEndElement for general\n");
		return 0;
	}

	//composition
	if (xmlTextWriterStartElement(writer, BAD_CAST "composition") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}

	if (xmi_write_layer_xml_body(writer, input->composition->layers, input->composition->n_layers) == 0)
		return 0;

	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "reference_layer","%i",input->composition->reference_layer) < 0) {
		fprintf(stderr,"Error writing reference_layer\n");
		return 0;
	}

	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error calling xmlTextWriterEndElement for composition\n");
		return 0;
	}

	//geometry
	if (xmlTextWriterStartElement(writer, BAD_CAST "geometry") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "d_sample_source","%g",input->geometry->d_sample_source) < 0) {
		fprintf(stderr,"Error writing d_sample_source\n");
		return 0;
	}
	if (xmlTextWriterStartElement(writer, BAD_CAST "n_sample_orientation") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "x","%g",input->geometry->n_sample_orientation[0]) < 0) {
		fprintf(stderr,"Error writing n_sample_orientation[0]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "y","%g",input->geometry->n_sample_orientation[1]) < 0) {
		fprintf(stderr,"Error writing n_sample_orientation[1]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "z","%g",input->geometry->n_sample_orientation[2]) < 0) {
		fprintf(stderr,"Error writing n_sample_orientation[2]\n");
		return 0;
	}
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error calling xmlTextWriterEndElement for n_sample_orientation\n");
		return 0;
	}
	if (xmlTextWriterStartElement(writer, BAD_CAST "p_detector_window") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "x","%g",input->geometry->p_detector_window[0]) < 0) {
		fprintf(stderr,"Error writing p_detector_window[0]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "y","%g",input->geometry->p_detector_window[1]) < 0) {
		fprintf(stderr,"Error writing p_detector_window[1]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "z","%g",input->geometry->p_detector_window[2]) < 0) {
		fprintf(stderr,"Error writing p_detector_window[2]\n");
		return 0;
	}
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error calling xmlTextWriterEndElement for p_detector_window\n");
		return 0;
	}
	if (xmlTextWriterStartElement(writer, BAD_CAST "n_detector_orientation") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "x","%g",input->geometry->n_detector_orientation[0]) < 0) {
		fprintf(stderr,"Error writing n_detector_orientation[0]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "y","%g",input->geometry->n_detector_orientation[1]) < 0) {
		fprintf(stderr,"Error writing n_detector_orientation[1]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "z","%g",input->geometry->n_detector_orientation[2]) < 0) {
		fprintf(stderr,"Error writing n_detector_orientation[2]\n");
		return 0;
	}
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error calling xmlTextWriterEndElement for n_detector_orientation\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "area_detector","%g",input->geometry->area_detector) < 0) {
		fprintf(stderr,"Error writing area_detector\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "collimator_height","%g",input->geometry->collimator_height) < 0) {
		fprintf(stderr,"Error writing collimator_height\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "collimator_diameter","%g",input->geometry->collimator_diameter) < 0) {
		fprintf(stderr,"Error writing collimator_diameter\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "d_source_slit","%g",input->geometry->d_source_slit) < 0) {
		fprintf(stderr,"Error writing d_source_slit\n");
		return 0;
	}
	if (xmlTextWriterStartElement(writer, BAD_CAST "slit_size") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "slit_size_x","%g",input->geometry->slit_size_x) < 0) {
		fprintf(stderr,"Error writing slit_size_x\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "slit_size_y","%g",input->geometry->slit_size_y) < 0) {
		fprintf(stderr,"Error writing slit_size_y\n");
		return 0;
	}
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error calling xmlTextWriterEndElement for slit_size\n");
		return 0;
	}

	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error calling xmlTextWriterEndElement for geometry\n");
		return 0;
	}

	//excitation
	if (xmlTextWriterStartElement(writer, BAD_CAST "excitation") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	if (input->excitation->n_discrete > 0) {
		for (i = 0 ; i < input->excitation->n_discrete ; i++) { 
			if (xmlTextWriterStartElement(writer, BAD_CAST "discrete") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "energy","%g",input->excitation->discrete[i].energy) < 0) {
				fprintf(stderr,"Error writing energy\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "horizontal_intensity","%g",input->excitation->discrete[i].horizontal_intensity) < 0) {
				fprintf(stderr,"Error writing horizontal_intensity\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "vertical_intensity","%g",input->excitation->discrete[i].vertical_intensity) < 0) {
				fprintf(stderr,"Error writing vertical_intensity\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_x","%g",input->excitation->discrete[i].sigma_x) < 0) {
				fprintf(stderr,"Error writing sigma_x\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_xp","%g",input->excitation->discrete[i].sigma_xp) < 0) {
				fprintf(stderr,"Error writing sigma_xp\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_y","%g",input->excitation->discrete[i].sigma_y) < 0) {
				fprintf(stderr,"Error writing sigma_y\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_yp","%g",input->excitation->discrete[i].sigma_yp) < 0) {
				fprintf(stderr,"Error writing sigma_yp\n");
				return 0;
			}
			//only write scale_parameter if distribution type is not monochromatic
			//this is done to keep the file backwards compatible
			if (input->excitation->discrete[i].distribution_type != XMI_DISCRETE_MONOCHROMATIC) {
				if (xmlTextWriterStartElement(writer, BAD_CAST "scale_parameter") < 0) {
					fprintf(stderr,"Error at xmlTextWriterStartElement\n");
					return 0;
				}
				if (input->excitation->discrete[i].distribution_type == XMI_DISCRETE_GAUSSIAN) {
					if (xmlTextWriterWriteAttribute(writer, BAD_CAST "distribution_type", BAD_CAST "gaussian") <0) {
						fprintf(stderr,"Error at xmlTextWriterWriteAttribute\n");
						return 0;
					}
				}
				else if (input->excitation->discrete[i].distribution_type == XMI_DISCRETE_LORENTZIAN) {
					if (xmlTextWriterWriteAttribute(writer, BAD_CAST "distribution_type", BAD_CAST "lorentzian") <0) {
						fprintf(stderr,"Error at xmlTextWriterWriteAttribute\n");
						return 0;
					}
				}
				if (xmlTextWriterWriteFormatString(writer, "%g", input->excitation->discrete[i].scale_parameter) < 0) {
					fprintf(stderr,"Error at xmlTextWriterWriteFormatString\n");
					return 0;
				
				}	
				if (xmlTextWriterEndElement(writer) < 0) {
					fprintf(stderr,"Error calling xmlTextWriterEndElement for scale_parameter\n");
					return 0;
				}
			}
			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error calling xmlTextWriterEndElement for discrete\n");
				return 0;
			}
		}
	}
	if (input->excitation->n_continuous > 0) {
		for (i = 0 ; i < input->excitation->n_continuous ; i++) { 
			if (xmlTextWriterStartElement(writer, BAD_CAST "continuous") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "energy","%g",input->excitation->continuous[i].energy) < 0) {
				fprintf(stderr,"Error writing energy\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "horizontal_intensity","%g",input->excitation->continuous[i].horizontal_intensity) < 0) {
				fprintf(stderr,"Error writing horizontal_intensity\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "vertical_intensity","%g",input->excitation->continuous[i].vertical_intensity) < 0) {
				fprintf(stderr,"Error writing vertical_intensity\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_x","%g",input->excitation->continuous[i].sigma_x) < 0) {
				fprintf(stderr,"Error writing sigma_x\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_xp","%g",input->excitation->continuous[i].sigma_xp) < 0) {
				fprintf(stderr,"Error writing sigma_xp\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_y","%g",input->excitation->continuous[i].sigma_y) < 0) {
				fprintf(stderr,"Error writing sigma_y\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_yp","%g",input->excitation->continuous[i].sigma_yp) < 0) {
				fprintf(stderr,"Error writing sigma_yp\n");
				return 0;
			}
			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error calling xmlTextWriterEndElement for continuous\n");
				return 0;
			}
		}
	}

	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending excitation\n");
		return 0;
	}
	//absorbers
	if (xmlTextWriterStartElement(writer, BAD_CAST "absorbers") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}

	if (input->absorbers->n_exc_layers > 0) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "excitation_path") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement\n");
			return 0;
		}
		if (xmi_write_layer_xml_body(writer, input->absorbers->exc_layers, input->absorbers->n_exc_layers) == 0) {
			return 0;
		}
		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error calling xmlTextWriterEndElement for excitation_path\n");
			return 0;
		}
		
	}
	if (input->absorbers->n_det_layers > 0) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "detector_path") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement\n");
			return 0;
		}
		if (xmi_write_layer_xml_body(writer, input->absorbers->det_layers, input->absorbers->n_det_layers) == 0) {
			return 0;
		}
		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error calling xmlTextWriterEndElement for detector_path\n");
			return 0;
		}
		
	}
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending absorbers\n");
		return 0;
	}

	//detector
	if (xmlTextWriterStartElement(writer, BAD_CAST "detector") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	if (input->detector->detector_type == XMI_DETECTOR_SILI)
		strcpy(detector_type,"SiLi");
	else if (input->detector->detector_type == XMI_DETECTOR_GE)
		strcpy(detector_type,"Ge");
	else if (input->detector->detector_type == XMI_DETECTOR_SI_SDD)
		strcpy(detector_type,"Si_SDD");


	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "detector_type","%s",detector_type) < 0) {
		fprintf(stderr,"Error writing detector_type\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "live_time","%g",input->detector->live_time) < 0) {
		fprintf(stderr,"Error writing live_time\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "pulse_width","%g",input->detector->pulse_width) < 0) {
		fprintf(stderr,"Error writing pulse_width\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "nchannels","%i",input->detector->nchannels) < 0) {
		fprintf(stderr,"Error writing nchannels\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "gain","%g",input->detector->gain) < 0) {
		fprintf(stderr,"Error writing gain\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "zero","%g",input->detector->zero) < 0) {
		fprintf(stderr,"Error writing zero\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "fano","%g",input->detector->fano) < 0) {
		fprintf(stderr,"Error writing fano\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "noise","%g",input->detector->noise) < 0) {
		fprintf(stderr,"Error writing noise\n");
		return 0;
	}
	if (xmlTextWriterStartElement(writer, BAD_CAST "crystal") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	if (xmi_write_layer_xml_body(writer, input->detector->crystal_layers, input->detector->n_crystal_layers) == 0) {
		return 0;
	}
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending crystal\n");
		return 0;
	}
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending detector\n");
		return 0;
	}
	
	return 1;
}


int xmi_xmlfile_to_string(char *xmlfile, char **xmlstring, int *xmlstringlength) {

	xmlDocPtr doc;
	xmlNodePtr root, subroot;
	xmlParserCtxtPtr ctx;
	int buffersize;


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

	subroot= root->children;

	//allocate memory for input
	*input = (struct xmi_input *) malloc(sizeof(struct xmi_input));




	while (subroot != NULL) {
		if (!xmlStrcmp(subroot->name,(const xmlChar *) "general")) {
			if (readGeneralXML(doc, subroot, &((**input).general)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "composition")) {
			if (readCompositionXML(doc, subroot, &((**input).composition)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "geometry")) {
			if (readGeometryXML(doc, subroot, &((**input).geometry)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "excitation")) {
			if (readExcitationXML(doc, subroot, &((**input).excitation)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "absorbers")) {
			if (readAbsorbersXML(doc, subroot, &((**input).absorbers)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "detector")) {
			if (readDetectorXML(doc, subroot, &((**input).detector)) == 0) {
				xmlFreeParserCtxt(ctx);
				xmlFreeDoc(doc);
				return 0;
			}	
		}

		subroot = subroot->next;
	}

	xmlFreeParserCtxt(ctx);
	xmlFreeDoc(doc);
	return 1;


}

static int xmi_write_input_xml_svg(xmlTextWriterPtr writer, struct xmi_input *input, char *name, int interaction, 
double *channels, double maximum2 ) {
	
	double minimum;
	double maximum;
	double minimum_log, maximum_log;
	double *energies;
	int i;
	double energy;
	double intensity;
	int intensity_int;
	int energy_pos;
	int error = 0;
	int width = SVG_DEFAULT_BOX_WIDTH;
	int height = SVG_DEFAULT_BOX_HEIGHT;
 	int energy_step = 5.0;
	int max_channel;
	char line[100];




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
	if(!error) error = write_start_element(writer, "graphic");

       	if(!error) error = write_start_element(writer,  "id");

	if(!error) error = write_char_element(writer, "name", name);
	if(!error) error = write_value_element_int(writer,  "interaction", interaction);

       	if(!error) error = write_end_element(writer,  "id");

        //create box
        if(!error) error = write_start_element(writer, "rect");
	
       	if(!error) error = write_start_element(writer, "view");
       	if(!error) error = write_end_element(writer, "view");


       	if(!error) error = write_start_element(writer, "size");
		if(!error) error = write_value_element(writer, "width", (int) width);
		if(!error) error = write_value_element(writer, "height", (int) height);
		if(!error) error = write_value_element(writer, "min_energy", energies[0]);
		if(!error) error = write_value_element(writer, "max_energy", energies[max_channel-1]);
       	if(!error) error = write_end_element(writer, "size");

	// x-axis
      	if(!error) error = write_start_element(writer, "x-axis");
        if(!error) error = write_char_element(writer, "name", "Energy (keV)");
	//for now print energy every 5 keV on X-axis
	energy = 0.0;
	while (energy <= energies[max_channel]) {
		if(!error) error = write_start_element(writer, "index");
		if(!error) error = write_value_element(writer,  "value", e2c(energy, channels, energies, max_channel));
                sprintf(line, "%.1f", energy);
		if(!error) error = write_char_element(writer,  "name", line);
		if(!error) error = write_end_element(writer,"index");
	energy += energy_step;
	} 
	if(!error) error = write_end_element(writer,"x-axis");
	// end x-axis

        // start y-axis
       	if(!error) error = write_start_element(writer, "y-axis");
 	if(!error) error = write_char_element(writer, "name", "Intensity (counts)");

	if (minimum_log < 0.0) {
		minimum_log = 0.0;
	}

	intensity = 1.0;	
	while (intensity <= maximum) {
		if (intensity < minimum) {
			intensity *= 10.0;
			continue;
		}

		if(!error) error = write_start_element(writer, "index");
		if(!error) error = write_value_element(writer,  "value", i2c(intensity, maximum_log, minimum_log));
		sprintf(line, "%.0f", intensity);
		if(!error) error = write_char_element(writer,  "name", line);
		if(!error) error = write_end_element(writer,"index");

		intensity *= 10.0;
	}      	
       	if(!error) error = write_end_element(writer, "y-axis");
	// end y-axis

       	if(!error) error = write_end_element(writer, "rect");
	// end create box
       	
        //loop grafic
	if(!error) error = write_start_element(writer,  "points");
  	if(!error) error = write_char_element(writer, "color", "blue");

	//loop point			
	for (i = 0 ; i <= max_channel ; i++) {
		if(!error) error = write_start_element(writer,  "point");
		if(!error) error = write_value_element(writer,  "x", e2c(energies[i], channels, energies, max_channel));
		if(!error) error = write_value_element(writer,  "y", i2c(channels[i], maximum_log, minimum_log));
		if(!error) error = write_end_element(writer,  "point");	       
		}
	//end loop point

       	if(!error) error = write_end_element(writer,  "points");		

       	if(!error) error = write_end_element(writer,  "graphic");
        //end loop

	free(energies);

        if(error)
		return 0;
	else
		return 1;


} 


static int write_start_element(xmlTextWriterPtr writer, char *element){
 	int error;
	 error = 0;

 	if (xmlTextWriterStartElement(writer, BAD_CAST element) < 0) {
		fprintf(stderr,"Error starting svg element %s \n", element);
		error = 1;
		}
         
	return error;
}

static int write_end_element(xmlTextWriterPtr writer, char *element){
 	int error;
	 error = 0;

 	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error starting svg element %s \n", element);
		error = 1;
		}
         
	return error;
}

static int write_value_element(xmlTextWriterPtr writer, char *element, float parm){
 	int error;
	error = 0;

        if (xmlTextWriterStartElement(writer, BAD_CAST element) < 0) {error = 1;}		
        if (!error){if (xmlTextWriterWriteFormatString(writer,"%g", parm ) < 0 ) {error = 1;}}
        if (!error){if (xmlTextWriterEndElement(writer) < 0) {error = 1;}}

        if (error) fprintf(stderr,"Error parameter svg element %s %g \n", element, parm);         
	return error;
}

static int write_value_element_int(xmlTextWriterPtr writer, char *element, int parm){
 	int error;
	error = 0;

        if (xmlTextWriterStartElement(writer, BAD_CAST element) < 0) {error = 1;}		
        if (!error){if (xmlTextWriterWriteFormatString(writer,"%i", parm ) < 0 ) {error = 1;}}
        if (!error){if (xmlTextWriterEndElement(writer) < 0) {error = 1;}}

        if (error) fprintf(stderr,"Error parameter svg element %s %i \n", element, parm);         
	return error;
}

static int write_char_element(xmlTextWriterPtr writer, char *element, char *parm){
 	int error;
	error = 0;

        if (xmlTextWriterStartElement(writer, BAD_CAST element) < 0) {error = 1;}		
        if (!error){if (xmlTextWriterWriteFormatString(writer,"%s", parm ) < 0 ) {error = 1;}}
        if (!error){if (xmlTextWriterEndElement(writer) < 0) {error = 1;}}

        if (error)fprintf(stderr,"Error parameter svg element %s %s \n", element, parm);
	return error;
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
	struct xmi_output *op = malloc(sizeof(struct xmi_output));

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

static int xmi_read_input_xml_body(xmlDocPtr doc, xmlNodePtr subroot, struct xmi_input *input) {

	while (subroot != NULL) {
		if (!xmlStrcmp(subroot->name,(const xmlChar *) "general")) {
			if (readGeneralXML(doc, subroot, &(input->general)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "composition")) {
			if (readCompositionXML(doc, subroot, &(input->composition)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "geometry")) {
			if (readGeometryXML(doc, subroot, &(input->geometry)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "excitation")) {
			if (readExcitationXML(doc, subroot, &(input->excitation)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "absorbers")) {
			if (readAbsorbersXML(doc, subroot, &(input->absorbers)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "detector")) {
			if (readDetectorXML(doc, subroot, &(input->detector)) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}	
		}

		subroot = subroot->next;
	}
	return 1;
}

static int xmi_write_default_comments(xmlTextWriterPtr writer) {
	gchar *timestring;

	//start off with some comments about user, time, date, hostname...
	if (xmlTextWriterStartComment(writer) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}
	
	if (xmlTextWriterWriteFormatElement(writer, BAD_CAST "Creator","%s (%s)",g_get_real_name(),g_get_user_name()) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}

#if GLIB_MAJOR_VERSION == 2 && GLIB_MINOR_VERSION >= 26
	GDateTime *time = g_date_time_new_now_local();
	timestring = g_date_time_format(time,"%F %H:%M:%S (%Z)");
	g_date_time_unref(time);
#else
	GTimeVal time;
	g_get_current_time(&time);
        timestring = g_time_val_to_iso8601(&time);

#endif
	if (xmlTextWriterWriteFormatElement(writer, BAD_CAST "Timestamp","%s",timestring) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}
	g_free(timestring);

	
	if (xmlTextWriterWriteFormatElement(writer, BAD_CAST "Hostname","%s",g_get_host_name()) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}

	if (xmlTextWriterEndComment(writer) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}

	if (xmlTextWriterWriteComment(writer, BAD_CAST "DO NOT MODIFY THIS FILE UNLESS YOU KNOW WHAT YOU ARE DOING!") < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}


	return 1;
}
	
static int xmi_read_output_xml_body(xmlDocPtr doc, xmlNodePtr root, struct xmi_output *op, int *step1, int *step2) {
	xmlNodePtr subroot, subsubroot;
	xmlChar *txt;

	//read step1 and step2 if necessary
	if (step1 != NULL || step2 != NULL) {
		xmlAttrPtr attr = root->properties;
		while (attr != NULL) {
			if (xmlStrcmp(attr->name, BAD_CAST "step1") == 0) {
				txt = xmlNodeListGetString(doc, attr->children, 1);
				if(sscanf((const char *)txt,"%i",step1) != 1) {
					fprintf(stderr,"error reading in step1 attribute of xml file\n");
					return 0;
				}
				xmlFree(txt);
			}
			else if (xmlStrcmp(attr->name, BAD_CAST "step2") == 0 && step2 != NULL) {
				txt = xmlNodeListGetString(doc, attr->children, 1);
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

	op->input = (struct xmi_input *) malloc(sizeof(struct xmi_input));

	if (xmi_read_input_xml_body(doc, xpathObj->nodesetval->nodeTab[0]->children, op->input) == 0)
		return 0;

	if (xmi_validate_input(op->input) != 0) {
		xmlFreeDoc(doc);
		fprintf(stderr, "Error validating input data\n");
		return 0;
	} 
	op->ninteractions = op->input->general->n_interactions_trajectory;

	xmlXPathFreeObject(xpathObj);
	xmlXPathFreeContext(xpathCtx);

	subroot = root->children;

	while (subroot != NULL) {
		if (!xmlStrcmp(subroot->name,(const xmlChar *) "inputfile")) {
			//inputfile
			txt = xmlNodeListGetString(doc,subroot->children,1);	
			op->inputfile = (char *) strdup((char *) txt);
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "spectrum_conv")) {
			//convoluted spectrum
			if (readSpectrumXML(doc,subroot, op, 1) == 0) {
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
			if (readSpectrumXML(doc,subroot, op, 0) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "brute_force_history")) {
			if (readHistoryXML(doc,subroot, &op->brute_force_history, &op->nbrute_force_history) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "variance_reduction_history")) {
			if (readHistoryXML(doc,subroot, &op->var_red_history, &op->nvar_red_history) == 0) {
				xmlFreeDoc(doc);
				return 0;
			}
		}
		subroot=subroot->next;
	}
	return 1;
}

int xmi_read_archive_xml(char *xmsafile, struct xmi_archive **archive) {

	xmlDocPtr doc;
	xmlNodePtr root;
	xmlParserCtxtPtr ctx;
	struct xmi_archive *ar = malloc(sizeof(struct xmi_archive));
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

	xmlNodePtr subroot, subsubroot;
	xmlChar *txt;

	subroot = root->children;

	ar->input = NULL;
	ar->output = NULL;
	ar->inputfiles = NULL;
	ar->outputfiles = NULL;
	ar->nsteps2 = 0;
	ar->xpath2 = NULL;

	while (subroot != NULL) {
		if (!xmlStrcmp(subroot->name,(const xmlChar *) "start_value1")) {
			txt = xmlNodeListGetString(doc,subroot->children,1);	
			if (sscanf((const char*) txt, "%lg", &ar->start_value1) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read start_value1\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "end_value1")) {
			txt = xmlNodeListGetString(doc,subroot->children,1);	
			if (sscanf((const char*) txt, "%lg", &ar->end_value1) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read end_value1\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "nsteps1")) {
			txt = xmlNodeListGetString(doc,subroot->children,1);	
			if (sscanf((const char*) txt, "%i", &ar->nsteps1) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read nsteps1\n");
				return 0;
			}
			xmlFree(txt);
			//allocate memory
			ar->output = malloc(sizeof(struct xmi_output**)*(ar->nsteps1+1));	
			ar->input = malloc(sizeof(struct xmi_input**)*(ar->nsteps1+1));	
			ar->inputfiles = malloc(sizeof(char**)*(ar->nsteps1+1));	
			ar->outputfiles = malloc(sizeof(char**)*(ar->nsteps1+1));	
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "xpath1")) {
			txt = xmlNodeListGetString(doc,subroot->children,1);	
			ar->xpath1 = strdup((char*) txt);
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "start_value2")) {
			txt = xmlNodeListGetString(doc,subroot->children,1);	
			if (sscanf((const char*) txt, "%lg", &ar->start_value2) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read start_value2\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "end_value2")) {
			txt = xmlNodeListGetString(doc,subroot->children,1);	
			if (sscanf((const char*) txt, "%lg", &ar->end_value2) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read end_value2\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "nsteps2")) {
			txt = xmlNodeListGetString(doc,subroot->children,1);	
			if (sscanf((const char*) txt, "%i", &ar->nsteps2) !=1) {
				fprintf(stderr,"xmi_read_archive_xml: could not read nsteps2\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "xpath2")) {
			txt = xmlNodeListGetString(doc,subroot->children,1);	
			ar->xpath2 = strdup((char*) txt);
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "xmimsim-results")) {
			struct xmi_output *output = malloc(sizeof(struct xmi_output));
			step1 = step2 = 0;
			if (xmi_read_output_xml_body(doc, subroot, output, &step1, &step2) == 0) {
				return 0;
			}
			if (step2 == 0) {
				ar->output[step1] = malloc(sizeof(struct xmi_output*)*(ar->nsteps2+1));
				ar->input[step1] = malloc(sizeof(struct xmi_input*)*(ar->nsteps2+1));
				ar->inputfiles[step1] = malloc(sizeof(char*)*(ar->nsteps2+1));
				ar->outputfiles[step1] = malloc(sizeof(char*)*(ar->nsteps2+1));
			}
			ar->output[step1][step2] = output;
			//fix links
			ar->input[step1][step2] = ar->output[step1][step2]->input;
			ar->inputfiles[step1][step2] = ar->output[step1][step2]->inputfile;
			ar->outputfiles[step1][step2] = ar->input[step1][step2]->general->outputfile;
			files_read++;
		}
		subroot=subroot->next;
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
	xmlDocPtr doc;
	xmlTextWriterPtr writer;


	LIBXML_TEST_VERSION

	if ((writer = xmlNewTextWriterDoc(&doc,9)) == NULL) {
		fprintf(stderr,"Error calling xmlNewTextWriterDoc\n");
		return 0;
	}
	xmlTextWriterSetIndent(writer,2);
	if (xmlTextWriterStartDocument(writer, NULL, NULL, NULL) < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartDocument\n");
		return 0;
	}
	if (xmlTextWriterStartDTD(writer,BAD_CAST  "xmimsim-archive", NULL, BAD_CAST "http://www.xmi.UGent.be/xml/xmimsim-1.0.dtd") < 0 ) {
		fprintf(stderr,"Error starting DTD\n");
		return 0;
	}

	if (xmlTextWriterEndDTD(writer) < 0) {
		fprintf(stderr,"Error ending DTD\n");
		return 0;
	}
	
	if (xmi_write_default_comments(writer) == 0) {
		return 0;
	}

	//body
	if (xmlTextWriterStartElement(writer,BAD_CAST "xmimsim-archive") < 0) {
		fprintf(stderr,"Error writing xmimsim-archive tag\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "start_value1","%g", archive->start_value1) < 0) {
		fprintf(stderr,"Error writing start_value1\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "end_value1","%g", archive->end_value1) < 0) {
		fprintf(stderr,"Error writing end_value1\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "nsteps1","%i", archive->nsteps1) < 0) {
		fprintf(stderr,"Error writing nsteps1\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "xpath1","%s", archive->xpath1) < 0) {
		fprintf(stderr,"Error writing xpath1\n");
		return 0;
	}
	if (archive->nsteps2 > 0) {
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "start_value2","%g", archive->start_value2) < 0) {
			fprintf(stderr,"Error writing start_value2\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "end_value2","%g", archive->end_value2) < 0) {
			fprintf(stderr,"Error writing end_value2\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "nsteps2","%i", archive->nsteps2) < 0) {
				fprintf(stderr,"Error writing nsteps2\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "xpath2","%s", archive->xpath2) < 0) {
			fprintf(stderr,"Error writing xpath2\n");
			return 0;
		}
	}
	int i,j;

	for (i = 0 ; i <= archive->nsteps1 ; i++) {
		for (j = 0 ; j <= archive->nsteps2 ; j++) {
			if (xmi_write_output_xml_body(writer, archive->output[i][j], i, j, 0) == 0) {
				return 0;
			}
		}
	}


	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending xmimsim-archive\n");
		return 0;
	}
	//endbody

	if (xmlTextWriterEndDocument(writer) < 0) {
		fprintf(stderr,"Error ending document\n");
		return 0;
	}

	xmlFreeTextWriter(writer);

	if (xmlSaveFileEnc(xmlfile,doc,NULL) == -1) {
		fprintf(stderr,"Could not write to %s\n",xmlfile);
		return 0;
	}
	xmlFreeDoc(doc);

	return 1;
}

static int xmi_write_layer_xml_body(xmlTextWriterPtr writer, struct xmi_layer *layers, int n_layers) {
	int i, j;

	for (i = 0 ; i < n_layers ; i++) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "layer") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement\n");
			return 0;
		}
		for (j = 0 ; j < layers[i].n_elements ; j++) {
			//skip if weight fraction is equal to zero
			if (fabs(layers[i].weight[j]) < 1E-20)
				continue;
			if (xmlTextWriterStartElement(writer, BAD_CAST "element") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "atomic_number","%i",layers[i].Z[j]) < 0) {
				fprintf(stderr,"Error writing atomic_number\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "weight_fraction","%g",layers[i].weight[j]*100.0) < 0) {
				fprintf(stderr,"Error writing weight_number\n");
				return 0;
			}
			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error calling xmlTextWriterEndElement for element\n");
				return 0;
			}
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "density","%g",layers[i].density) < 0) {
			fprintf(stderr,"Error writing density\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "thickness","%g",layers[i].thickness) < 0) {
			fprintf(stderr,"Error writing thickness\n");
			return 0;
		}
	
		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error calling xmlTextWriterEndElement for layer\n");
			return 0;
		}
	}

	return 1;
}
