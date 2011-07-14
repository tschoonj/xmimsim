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
static int xmi_cmp_struct_xmi_energy(const void *a, const void *b);
static int xmi_write_input_xml_body(xmlTextWriterPtr writer, struct xmi_input *input); 
static int xmi_write_input_xml_svg(xmlTextWriterPtr writer, struct xmi_input *input, char *name, int interaction,  double *channels, int nchannels, double maximum); 
static int xmi_write_output_doc(xmlDocPtr *doc, struct xmi_input *input, double *brute_history, double *var_red_history,double **channels_conv, double *channels_unconv, int nchannels, char *inputfile, int use_zero_interactions );

extern int xmlLoadExtDtdDefaultValue;

static int write_start_element(xmlTextWriterPtr writer, char *element);
static int write_end_element(xmlTextWriterPtr writer, char *element);
static int write_value_element(xmlTextWriterPtr writer, char *element, float parm);
static int write_value_element_int(xmlTextWriterPtr writer, char *element, int parm);
static int write_char_element(xmlTextWriterPtr writer, char *element, char *parm);
static float e2c(double energy, double * channels, double * energies, int nchannels );
static float i2c(double intensity, double maximum_log, double minimum_log);



#ifdef _WIN32
#include "xmi_registry_win.h"

int xmi_xmlLoadCatalog() {
	char *catalog;
	int rv;
	if (xmi_registry_win_query(XMI_REGISTRY_WIN_CATALOG,&catalog) == 0)
		return 0;


	if (xmlLoadCatalog((gchar *) catalog) != 0) {
		fprintf(stderr,"Could not load %s\n",(gchar *) catalog);
		rv=0;
	}
	else {
		rv=1;
		free(catalog);
	}

	return rv;

}

#else
int xmi_xmlLoadCatalog() {
	char catalog[] = XMI_CATALOG;
	int rv;

	if (xmlLoadCatalog(catalog) != 0) {
		fprintf(stderr,"Could not load %s\n",catalog);
		rv=0;
	}
	else
		rv=1;
	
	return rv;
}
#endif




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
				(*general)->outputfile = (char *) xmlStrdup(txt); 
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
				(*general)->comments = (char *) xmlStrdup(txt); 
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
				fprintf(stdout,"invalid reference_layer value detected\n");
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
	double norm;
	

	//allocate memory
	*geometry= (struct xmi_geometry *) malloc(sizeof(struct xmi_geometry));

	subnode = node->children;

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar *) "d_sample_source")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*geometry)->d_sample_source)) != 1) {
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
					if(sscanf((const char *)txt,"%lf",&((*geometry)->n_sample_orientation[0])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*geometry)->n_sample_orientation[1])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*geometry)->n_sample_orientation[2])) != 1) {
						fprintf(stderr,"error reading in n_sample_orientation y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = subsubnode->next;
			}
			//normalize
			norm = sqrt((*geometry)->n_sample_orientation[0]*(*geometry)->n_sample_orientation[0] + 
			(*geometry)->n_sample_orientation[1]*(*geometry)->n_sample_orientation[1]+
			(*geometry)->n_sample_orientation[2]*(*geometry)->n_sample_orientation[2]
			);
			(*geometry)->n_sample_orientation[0] /= norm;
			(*geometry)->n_sample_orientation[1] /= norm;
			(*geometry)->n_sample_orientation[2] /= norm;
			//make sure that the Z component is positive!
			//weird things will happen btw if Z is equal to zero...
			if ((*geometry)->n_sample_orientation[2] < 0.0) {
				(*geometry)->n_sample_orientation[0] *= -1.0;
				(*geometry)->n_sample_orientation[1] *= -1.0;
				(*geometry)->n_sample_orientation[2] *= -1.0;
			}

		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "p_detector_window")) {
			subsubnode = subnode->children;	
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "x")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*geometry)->p_detector_window[0])) != 1) {
						fprintf(stderr,"error reading in p_detector_window x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*geometry)->p_detector_window[1])) != 1) {
						fprintf(stderr,"error reading in p_detector_window y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*geometry)->p_detector_window[2])) != 1) {
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
					if(sscanf((const char *)txt,"%lf",&((*geometry)->n_detector_orientation[0])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*geometry)->n_detector_orientation[1])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "z")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*geometry)->n_detector_orientation[2])) != 1) {
						fprintf(stderr,"error reading in n_detector_orientation z of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = subsubnode->next;
			}
			//normalize
			norm = sqrt((*geometry)->n_detector_orientation[0]*(*geometry)->n_detector_orientation[0] + 
			(*geometry)->n_detector_orientation[1]*(*geometry)->n_detector_orientation[1]+
			(*geometry)->n_detector_orientation[2]*(*geometry)->n_detector_orientation[2]
			);
			(*geometry)->n_detector_orientation[0] /= norm;
			(*geometry)->n_detector_orientation[1] /= norm;
			(*geometry)->n_detector_orientation[2] /= norm;
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "area_detector")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*geometry)->area_detector)) != 1) {
				fprintf(stderr,"error reading in area_detector of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "collimator_height")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*geometry)->collimator_height)) != 1) {
				fprintf(stderr,"error reading in collimator_height of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "collimator_diameter")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*geometry)->collimator_diameter)) != 1) {
				fprintf(stderr,"error reading in collimator_diameter of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "d_source_slit")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*geometry)->d_source_slit)) != 1) {
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
					if(sscanf((const char *)txt,"%lf",&((*geometry)->slit_size_x)) != 1) {
						fprintf(stderr,"error reading in slit_size_x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "slit_size_y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*geometry)->slit_size_y)) != 1) {
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
	
	*excitation = (struct xmi_excitation *) malloc(sizeof(struct xmi_excitation));

	(*excitation)->n_discrete = 0;
	(*excitation)->discrete = NULL;
	(*excitation)->n_continuous = 0;
	(*excitation)->continuous = NULL;


	subnode = node->children;

	while (subnode != NULL) {
		if (!xmlStrcmp(subnode->name,(const xmlChar*) "discrete")) {
			(*excitation)->discrete = (struct xmi_energy *) realloc((*excitation)->discrete,++((*excitation)->n_discrete)*sizeof(struct xmi_energy));
			subsubnode = subnode->children;
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "energy")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->discrete[(*excitation)->n_discrete-1].energy)) != 1) {
						fprintf(stderr,"error reading in discrete energy of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "horizontal_intensity")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->discrete[(*excitation)->n_discrete-1].horizontal_intensity)) != 1) {
						fprintf(stderr,"error reading in discrete horizontal_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "vertical_intensity")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->discrete[(*excitation)->n_discrete-1].vertical_intensity)) != 1) {
						fprintf(stderr,"error reading in discrete vertical_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_x")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->discrete[(*excitation)->n_discrete-1].sigma_x)) != 1) {
						fprintf(stderr,"error reading in sigma_x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_xp")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->discrete[(*excitation)->n_discrete-1].sigma_xp)) != 1) {
						fprintf(stderr,"error reading in sigma_xp of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->discrete[(*excitation)->n_discrete-1].sigma_y)) != 1) {
						fprintf(stderr,"error reading in sigma_y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_yp")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->discrete[(*excitation)->n_discrete-1].sigma_yp)) != 1) {
						fprintf(stderr,"error reading in sigma_yp of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = subsubnode->next;
			}
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "continuous")) {
			(*excitation)->continuous = (struct xmi_energy *) realloc((*excitation)->continuous,++((*excitation)->n_continuous)*sizeof(struct xmi_energy));
			subsubnode = subnode->children;
			while (subsubnode != NULL) {
				if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "energy")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->continuous[(*excitation)->n_continuous-1].energy)) != 1) {
						fprintf(stderr,"error reading in continuous energy of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "horizontal_intensity")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->continuous[(*excitation)->n_continuous-1].horizontal_intensity)) != 1) {
						fprintf(stderr,"error reading in continuous horizontal_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar*) "vertical_intensity")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->continuous[(*excitation)->n_continuous-1].vertical_intensity)) != 1) {
						fprintf(stderr,"error reading in continuous vertical_intensity of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_x")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->continuous[(*excitation)->n_continuous-1].sigma_x)) != 1) {
						fprintf(stderr,"error reading in sigma_x of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_xp")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->continuous[(*excitation)->n_continuous-1].sigma_xp)) != 1) {
						fprintf(stderr,"error reading in sigma_xp of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_y")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->continuous[(*excitation)->n_continuous-1].sigma_y)) != 1) {
						fprintf(stderr,"error reading in sigma_y of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				else if (!xmlStrcmp(subsubnode->name,(const xmlChar *) "sigma_yp")) {
					txt = xmlNodeListGetString(doc,subsubnode->children,1);
					if(sscanf((const char *)txt,"%lf",&((*excitation)->continuous[(*excitation)->n_continuous-1].sigma_yp)) != 1) {
						fprintf(stderr,"error reading in sigma_yp of xml file\n");
						return 0;
					}
					xmlFree(txt);
				}
				subsubnode = subsubnode->next;
			}
		}
		subnode = subnode->next;
	}

	if ((*excitation)->n_continuous == 0 && (*excitation)->n_discrete == 0) {
		fprintf(stderr,"Found no discrete or continuous energies in xml file\n");
		return 0;
	}

	//sort!
	if ((*excitation)->n_continuous != 0) {
		qsort((*excitation)->continuous,(*excitation)->n_continuous,sizeof(struct xmi_energy),xmi_cmp_struct_xmi_energy);
	}
	if ((*excitation)->n_discrete != 0) {
		qsort((*excitation)->discrete,(*excitation)->n_discrete,sizeof(struct xmi_energy),xmi_cmp_struct_xmi_energy);
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
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "live_time")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*detector)->live_time)) != 1) {
				fprintf(stderr,"error reading in live_time of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "pulse_width")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*detector)->pulse_width)) != 1) {
				fprintf(stderr,"error reading in pulse_width of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "gain")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*detector)->gain)) != 1) {
				fprintf(stderr,"error reading in gain of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "zero")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*detector)->zero)) != 1) {
				fprintf(stderr,"error reading in zero of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "fano")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*detector)->fano)) != 1) {
				fprintf(stderr,"error reading in fano of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "noise")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*detector)->noise)) != 1) {
				fprintf(stderr,"error reading in noise of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar*) "max_convolution_energy")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*detector)->max_convolution_energy)) != 1) {
				fprintf(stderr,"error reading in max_convolution_energy of xml file\n");
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
					if(sscanf((const char *)txt,"%lf",weight+n_elements-1) != 1) {
						fprintf(stderr,"error reading in weight_fraction of xml file\n");
						return 0;
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
			if(sscanf((const char *)txt,"%lf",&(layer->density)) != 1) {
				fprintf(stderr,"error reading in density of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "thickness")) {
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&(layer->thickness)) != 1) {
				fprintf(stderr,"error reading in thickness of xml file\n");
				return 0;
			}
			xmlFree(txt);
		}
		subnode = subnode->next;

	}
	//normalize weights...
	xmi_scale_double(weight,n_elements,xmi_sum_double(weight,n_elements));	

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

	if (xmlStrcmp(root->name,(const xmlChar*) "xmimsim")) {
		fprintf(stderr,"XML document is %s of wrong type, expected xmimsim\n",xmlfile);
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
		fprintf(stderr,"Error writing xmimsim tag\n");
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
	gchar *timestring;
	GTimeVal time;



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
	if (xmlTextWriterWriteFormatElement(writer, BAD_CAST "Timestamp","%s",g_date_time_format(g_date_time_new_now_local(),"%F %H:%M:%S (%Z)")) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}
#else
	g_get_current_time(&time);
        timestring = g_time_val_to_iso8601(&time);

	if (xmlTextWriterWriteFormatElement(writer, BAD_CAST "Timestamp","%s",timestring) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}

	g_free(timestring);
#endif
	
	if (xmlTextWriterWriteFormatElement(writer, BAD_CAST "Hostname","%s",g_get_host_name()) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}

	if (xmlTextWriterEndComment(writer) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}
	if (xmlTextWriterStartElement(writer,BAD_CAST "xmimsim") < 0) {
		fprintf(stderr,"Error writing xmimsim tag\n");
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
	xmlSaveFileEnc(xmlfile,doc,NULL);
	xmlFreeDoc(doc);

	return 1;

}
static int xmi_cmp_struct_xmi_energy(const void *a, const void *b) {
	double diff;

	diff = ((struct xmi_energy *)a)->energy - ((struct xmi_energy *)b)->energy;
	
	if (diff > 0.0)
		return 1;
	else if (diff < 0.0)
		return -1;
	return 0;

}


static int xmi_write_output_doc(xmlDocPtr *doc, struct xmi_input *input, double *brute_history, double *var_red_history,double **channels_conv, double *channels_unconv, int nchannels, char *inputfile, int use_zero_interactions ) {

	char version[100];
	char detector_type[20];
	int i,j,k;
	char buffer[1024];
	int *uniqZ = NULL;
	int nuniqZ = 1;
	int found;
	double *maxima;
	double gl_conv_max;
	double gl_unconv_max;
	gchar *timestring;
	GTimeVal time;
	xmlTextWriterPtr writer;


//	fprintf(stdout, "pbro tells : xmi_write_output_doc \n"); 

	LIBXML_TEST_VERSION

	//get unique Z's

/*
       uniqZ = [layers(1)%Z(1)]
       DO i=1,SIZE(layers)
	       DO j=1,SIZE(layers(i)%Z)
		       IF (.NOT. ANY(layers(i)%Z(j) == uniqZ)) uniqZ = &
		               [uniqZ,layers(i)%Z(j)]
	       ENDDO
       ENDDO
       CALL qsort(C_LOC(uniqZ),SIZE(uniqZ,KIND=C_SIZE_T),&
	     INT(KIND(uniqZ),KIND=C_SIZE_T),C_FUNLOC(C_INT_CMP))
*/

#if DEBUG == 1
	fprintf(stdout,"Before uniqZ\n");
#endif
	uniqZ = (int *) realloc(uniqZ, sizeof(int));
	uniqZ[0] = input->composition->layers[0].Z[0];
	for (i = 0 ; i < input->composition->n_layers ; i++) {
		for (j = 0 ; j < input->composition->layers[i].n_elements ; j++) {
			found = 0;
			for (k = 0 ; k < nuniqZ ; k++) {
				if (uniqZ[k] == input->composition->layers[i].Z[j]) {
					found = 1;
					break;
				}
			}
			if (found == 0) {
				//enlarge uniqZ
				uniqZ = (int *) realloc(uniqZ, sizeof(int)*++nuniqZ);
				uniqZ[nuniqZ-1] = input->composition->layers[i].Z[j]; 
			}
		}
	}
#if DEBUG == 1
	fprintf(stdout,"After uniqZ\n");
#endif
	qsort(uniqZ, nuniqZ, sizeof(int),xmi_cmp_int);
#if DEBUG == 1
	for (i = 0 ; i < nuniqZ ; i++)
		fprintf(stdout,"Z: %i\n",uniqZ[i]);
#endif



	if ((writer = xmlNewTextWriterDoc(doc,0)) == NULL) {
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
	if (xmlTextWriterWriteFormatElement(writer, BAD_CAST "Timestamp","%s",g_date_time_format(g_date_time_new_now_local(),"%F %H:%M:%S (%Z)")) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}
#else
	g_get_current_time(&time);
        timestring = g_time_val_to_iso8601(&time);

	if (xmlTextWriterWriteFormatElement(writer, BAD_CAST "Timestamp","%s",timestring) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}

	g_free(timestring);
#endif
	
	if (xmlTextWriterWriteFormatElement(writer, BAD_CAST "Hostname","%s",g_get_host_name()) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}

	if (xmlTextWriterEndComment(writer) < 0) {
		fprintf(stderr,"Error writing comment\n");
		return 0;
	}
	if (xmlTextWriterStartElement(writer,BAD_CAST "xmimsim-results") < 0) {
		fprintf(stderr,"Error writing xmimsim-results tag\n");
		return 0;
	}

	sprintf(version,"%.1f",input->general->version);
	if (xmlTextWriterWriteFormatAttribute(writer, BAD_CAST "version","%.1f",input->general->version) < 0) {
		fprintf(stderr,"Error at xmlTextWriterWriteFormatAttribute\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "inputfile","%s",inputfile) < 0) {
		fprintf(stderr,"Error writing inputfile\n");
		return 0;
	}

	//spectrum convoluted
	if (xmlTextWriterStartElement(writer, BAD_CAST "spectrum_conv") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement spectrum_conv\n");
		return 0;
	}

#define ARRAY2D_FORTRAN(array,i,j,Ni,Nj) (array[Nj*(i)+(j-1)])
	for (j = 0 ; j < nchannels ; j++) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "channel") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement channel\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "channelnr","%i",j+1) < 0) {
			fprintf(stderr,"Error writing channelnr\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "energy","%lf",input->detector->gain*(j+1)+input->detector->zero) < 0) {
			fprintf(stderr,"Error writing energy\n");
			return 0;
		}
/*		if (xmlTextWriterWriteFormatElement(writer, "counts","%lf",channels_conv[i]) < 0) {
			fprintf(stderr,"Error writing counts\n");
			return 0;
		}*/

		for (i = (use_zero_interactions == 1 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {

			if (xmlTextWriterStartElement(writer, BAD_CAST "counts") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement counts\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "interaction_number","%i",i) < 0) {
				fprintf(stderr,"Error writing interaction_number attribute\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatString(writer,"%lf",channels_conv[i][j]) < 0) {
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

	for (j = 1 ; j <= nchannels ; j++) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "channel") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement channel\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "channelnr","%i",j) < 0) {
			fprintf(stderr,"Error writing channelnr\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "energy","%lf",input->detector->gain*j+input->detector->zero) < 0) {
			fprintf(stderr,"Error writing energy\n");
			return 0;
		}
/*		if (xmlTextWriterWriteFormatElement(writer, "counts","%lf",channels_unconv[i]) < 0) {
			fprintf(stderr,"Error writing counts\n");
			return 0;
		}*/
		for (i = (use_zero_interactions == 1 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
			if (xmlTextWriterStartElement(writer, BAD_CAST "counts") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement counts\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "interaction_number","%i",i) < 0) {
				fprintf(stderr,"Error writing interaction_number attribute\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatString(writer,"%lf",ARRAY2D_FORTRAN(channels_unconv,i,j,input->general->n_interactions_trajectory+1,nchannels)) < 0) {
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

	//brute_history.. only print the relevant elements...
#define ARRAY3D_FORTRAN(array,i,j,k,Ni,Nj,Nk) (array[Nj*Nk*(i-1)+Nk*(j-1)+(k-1)])

	if (xmlTextWriterStartElement(writer, BAD_CAST "brute_force_history") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement brute_force_history\n");
		return 0;
	}

	for (i = 0 ; i < nuniqZ ; i++) {
#if DEBUG == 1
		fprintf(stdout,"Element: %i\n",uniqZ[i]);
#endif
		for (j = 1 ; j <= 385 ; j++) {
			for (k = 1 ; k <= input->general->n_interactions_trajectory ; k++) {
				if (ARRAY3D_FORTRAN(brute_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory) <= 0.0)
					continue;
				if (xmlTextWriterStartElement(writer, BAD_CAST "fluorescence_line_counts") < 0) {
					fprintf(stderr,"Error at xmlTextWriterStartElement fluorescence_line_counts\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "atomic_number","%i",uniqZ[i]) < 0) {
					fprintf(stderr,"Error writing atomic_number\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "line_type","%s",xmi_lines[j]) < 0) {
					fprintf(stderr,"Error writing line_type\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "interaction_number","%i",k) < 0) {
					fprintf(stderr,"Error writing interaction_number\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "energy","%lf",LineEnergy(uniqZ[i],-1*j)) < 0) {
					fprintf(stderr,"Error writing energy\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "symbol","%s",AtomicNumberToSymbol(uniqZ[i])) < 0) {
       					fprintf(stderr,"Error writing symbol\n");                     
					return 0;
				} 

				if (xmlTextWriterWriteFormatString(writer,"%lg",ARRAY3D_FORTRAN(brute_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory)) < 0) {
					fprintf(stderr,"Error writing counts\n");
					return 0;
				}
				if (xmlTextWriterEndElement(writer) < 0) {
					fprintf(stderr,"Error ending fluorescence_line_counts\n");
					return 0;
				}
			}
		}
	}

	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending brute_force_history\n");
		return 0;
	}

	if (var_red_history != NULL) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "variance_reduction_history") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement variance_reduction_history\n");
			return 0;
		}

		for (i = 0 ; i < nuniqZ ; i++) {
#if DEBUG == 1
			fprintf(stdout,"Element: %i\n",uniqZ[i]);
#endif
			for (j = 1 ; j <= 385 ; j++) {
				for (k = 1 ; k <= input->general->n_interactions_trajectory ; k++) {
					if (ARRAY3D_FORTRAN(var_red_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory) <= 0.0)
						continue;
					if (xmlTextWriterStartElement(writer, BAD_CAST "fluorescence_line_counts") < 0) {
						fprintf(stderr,"Error at xmlTextWriterStartElement fluorescence_line_counts\n");
						return 0;
					}
					if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "atomic_number","%i",uniqZ[i]) < 0) {
						fprintf(stderr,"Error writing atomic_number\n");
						return 0;
					}
					if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "line_type","%s",xmi_lines[j]) < 0) {
						fprintf(stderr,"Error writing line_type\n");
						return 0;
					}
					if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "interaction_number","%i",k) < 0) {
						fprintf(stderr,"Error writing interaction_number\n");
						return 0;
					}
					if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "energy","%lf",LineEnergy(uniqZ[i],-1*j)) < 0) {
						fprintf(stderr,"Error writing energy\n");
						return 0;
					}
					if (xmlTextWriterWriteFormatAttribute(writer,BAD_CAST "symbol","%s",AtomicNumberToSymbol(uniqZ[i])) < 0) {
       						fprintf(stderr,"Error writing symbol\n");                     
						return 0;
					} 

					if (xmlTextWriterWriteFormatString(writer,"%lg",ARRAY3D_FORTRAN(var_red_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory)) < 0) {
						fprintf(stderr,"Error writing counts\n");
						return 0;
					}
					if (xmlTextWriterEndElement(writer) < 0) {
						fprintf(stderr,"Error ending fluorescence_line_counts\n");
						return 0;
					}
				}
			}
		}

		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error ending variance_reduction_history\n");
			return 0;
		}
	}
	//write inputfile
	if (xmlTextWriterStartElement(writer,BAD_CAST "xmimsim-input") < 0) {
		fprintf(stderr,"Error writing xmimsim tag\n");
		return 0;
	}

	if (xmi_write_input_xml_body(writer, input) == 0)
		return 0;

	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending xmimsim-input\n");
		return 0;
	}

	//write svg stuff
	if (xmlTextWriterStartElement(writer, BAD_CAST "svg_graphs") < 0) {
		fprintf(stderr,"Error writing svg_graphs tag\n");
		return 0;
	}

        // calculate global channel max for conv
	maxima = (double *) malloc(sizeof(double)*(input->general->n_interactions_trajectory+1));
	maxima[0]=0.0;
	for (i = (use_zero_interactions == 1 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
		maxima[i]=xmi_maxval_double(channels_conv[i],nchannels);
	}
	gl_conv_max = xmi_maxval_double(maxima,input->general->n_interactions_trajectory+1);
        free(maxima); maxima = NULL;

        // calculate global channel max for unconv
	maxima = (double *) malloc(sizeof(double)*(input->general->n_interactions_trajectory+1));
	maxima[0]=0.0;
	for (i = (use_zero_interactions == 1 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
		maxima[i]=xmi_maxval_double(channels_unconv+i*nchannels,nchannels);
	}
	gl_unconv_max = xmi_maxval_double(maxima,input->general->n_interactions_trajectory+1);
	free(maxima); maxima = NULL;


        //write svg_graph lines
	for (i = (use_zero_interactions == 1 ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {

		//convoluted first
	
		if (xmi_write_input_xml_svg(writer, input, "convoluted", i, channels_conv[i], nchannels, gl_conv_max) == 0) {
			fprintf(stderr,"Error in xmi_write_input_xml_svg\n");
			return 0;
		}

		//unconvoluted second

		if (xmi_write_input_xml_svg(writer, input, "unconvoluted", i,  channels_unconv+i*nchannels, nchannels, gl_unconv_max ) == 0) {
			fprintf(stderr,"Error in xmi_write_input_xml_svg\n");
			return 0;
		}

	}

	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending svg_graphs tag\n");
		return 0;
	}

	//end it
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error ending xmimsim-results\n");
		return 0;
	}
	if (xmlTextWriterEndDocument(writer) < 0) {
		fprintf(stderr,"Error ending document\n");
		return 0;
	}

	xmlFreeTextWriter(writer);

	return 1;
}

int xmi_write_output_xml(char *xmlfile, struct xmi_input *input, double *brute_history, double *var_red_history,double **channels_conv, double *channels_unconv, int nchannels, char *inputfile, int use_zero_interactions ) {


	xmlDocPtr doc;

//	fprintf(stdout," pbro tells: xmi_write_output_xml \n " );

	LIBXML_TEST_VERSION

	if(xmi_write_output_doc(&doc, input, brute_history, var_red_history, channels_conv, channels_unconv, nchannels, inputfile, use_zero_interactions) == 0){	//pbro return 0;
}


	xmlSaveFileEnc(xmlfile,doc,NULL);
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

	for (i = 0 ; i < input->composition->n_layers ; i++) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "layer") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement\n");
			return 0;
		}
		for (j = 0 ; j < input->composition->layers[i].n_elements ; j++) {
			if (xmlTextWriterStartElement(writer, BAD_CAST "element") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "atomic_number","%i",input->composition->layers[i].Z[j]) < 0) {
				fprintf(stderr,"Error writing atomic_number\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "weight_fraction","%lg",input->composition->layers[i].weight[j]*100.0) < 0) {
				fprintf(stderr,"Error writing weight_number\n");
				return 0;
			}
			
		
			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error calling xmlTextWriterEndElement for element\n");
				return 0;
			}
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "density","%lg",input->composition->layers[i].density) < 0) {
			fprintf(stderr,"Error writing density\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "thickness","%lg",input->composition->layers[i].thickness) < 0) {
			fprintf(stderr,"Error writing thickness\n");
			return 0;
		}
	
		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error calling xmlTextWriterEndElement for layer\n");
			return 0;
		}
	}
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
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "d_sample_source","%lf",input->geometry->d_sample_source) < 0) {
		fprintf(stderr,"Error writing d_sample_source\n");
		return 0;
	}
	if (xmlTextWriterStartElement(writer, BAD_CAST "n_sample_orientation") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "x","%lf",input->geometry->n_sample_orientation[0]) < 0) {
		fprintf(stderr,"Error writing n_sample_orientation[0]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "y","%lf",input->geometry->n_sample_orientation[1]) < 0) {
		fprintf(stderr,"Error writing n_sample_orientation[1]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "z","%lf",input->geometry->n_sample_orientation[2]) < 0) {
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
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "x","%lf",input->geometry->p_detector_window[0]) < 0) {
		fprintf(stderr,"Error writing p_detector_window[0]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "y","%lf",input->geometry->p_detector_window[1]) < 0) {
		fprintf(stderr,"Error writing p_detector_window[1]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "z","%lf",input->geometry->p_detector_window[2]) < 0) {
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
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "x","%lf",input->geometry->n_detector_orientation[0]) < 0) {
		fprintf(stderr,"Error writing n_detector_orientation[0]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "y","%lf",input->geometry->n_detector_orientation[1]) < 0) {
		fprintf(stderr,"Error writing n_detector_orientation[1]\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "z","%lf",input->geometry->n_detector_orientation[2]) < 0) {
		fprintf(stderr,"Error writing n_detector_orientation[2]\n");
		return 0;
	}
	if (xmlTextWriterEndElement(writer) < 0) {
		fprintf(stderr,"Error calling xmlTextWriterEndElement for n_detector_orientation\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "area_detector","%lf",input->geometry->area_detector) < 0) {
		fprintf(stderr,"Error writing area_detector\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "collimator_height","%lf",input->geometry->collimator_height) < 0) {
		fprintf(stderr,"Error writing collimator_height\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "collimator_diameter","%lf",input->geometry->collimator_diameter) < 0) {
		fprintf(stderr,"Error writing collimator_diameter\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "d_source_slit","%lf",input->geometry->d_source_slit) < 0) {
		fprintf(stderr,"Error writing d_source_slit\n");
		return 0;
	}
	if (xmlTextWriterStartElement(writer, BAD_CAST "slit_size") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "slit_size_x","%lf",input->geometry->slit_size_x) < 0) {
		fprintf(stderr,"Error writing slit_size_x\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "slit_size_y","%lf",input->geometry->slit_size_y) < 0) {
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
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "energy","%lf",input->excitation->discrete[i].energy) < 0) {
				fprintf(stderr,"Error writing energy\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "horizontal_intensity","%lg",input->excitation->discrete[i].horizontal_intensity) < 0) {
				fprintf(stderr,"Error writing horizontal_intensity\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "vertical_intensity","%lg",input->excitation->discrete[i].vertical_intensity) < 0) {
				fprintf(stderr,"Error writing vertical_intensity\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_x","%lf",input->excitation->discrete[i].sigma_x) < 0) {
				fprintf(stderr,"Error writing sigma_x\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_xp","%lf",input->excitation->discrete[i].sigma_xp) < 0) {
				fprintf(stderr,"Error writing sigma_xp\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_y","%lf",input->excitation->discrete[i].sigma_y) < 0) {
				fprintf(stderr,"Error writing sigma_y\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_yp","%lf",input->excitation->discrete[i].sigma_yp) < 0) {
				fprintf(stderr,"Error writing sigma_yp\n");
				return 0;
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
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "energy","%lf",input->excitation->continuous[i].energy) < 0) {
				fprintf(stderr,"Error writing energy\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "horizontal_intensity","%lg",input->excitation->continuous[i].horizontal_intensity) < 0) {
				fprintf(stderr,"Error writing horizontal_intensity\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "vertical_intensity","%lg",input->excitation->continuous[i].vertical_intensity) < 0) {
				fprintf(stderr,"Error writing vertical_intensity\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_x","%lf",input->excitation->continuous[i].sigma_x) < 0) {
				fprintf(stderr,"Error writing sigma_x\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_xp","%lf",input->excitation->continuous[i].sigma_xp) < 0) {
				fprintf(stderr,"Error writing sigma_xp\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_y","%lf",input->excitation->continuous[i].sigma_y) < 0) {
				fprintf(stderr,"Error writing sigma_y\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "sigma_yp","%lf",input->excitation->continuous[i].sigma_yp) < 0) {
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
		for (i = 0 ; i < input->absorbers->n_exc_layers ; i++) {
			if (xmlTextWriterStartElement(writer, BAD_CAST "layer") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement\n");
				return 0;
			}
			for (j = 0 ; j < input->absorbers->exc_layers[i].n_elements ; j++) {
				if (xmlTextWriterStartElement(writer, BAD_CAST "element") < 0) {
					fprintf(stderr,"Error at xmlTextWriterStartElement\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "atomic_number","%i",input->absorbers->exc_layers[i].Z[j]) < 0) {
					fprintf(stderr,"Error writing atomic_number\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "weight_fraction","%lg",input->absorbers->exc_layers[i].weight[j]*100.0) < 0) {
					fprintf(stderr,"Error writing weight_number\n");
					return 0;
				}
			
		
				if (xmlTextWriterEndElement(writer) < 0) {
					fprintf(stderr,"Error calling xmlTextWriterEndElement for element\n");
					return 0;
				}
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "density","%lg",input->absorbers->exc_layers[i].density) < 0) {
				fprintf(stderr,"Error writing density\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "thickness","%lg",input->absorbers->exc_layers[i].thickness) < 0) {
				fprintf(stderr,"Error writing thickness\n");
				return 0;
			}
	
			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error calling xmlTextWriterEndElement for layer\n");
				return 0;
			}
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
		for (i = 0 ; i < input->absorbers->n_det_layers ; i++) {
			if (xmlTextWriterStartElement(writer, BAD_CAST "layer") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement\n");
				return 0;
			}
			for (j = 0 ; j < input->absorbers->det_layers[i].n_elements ; j++) {
				if (xmlTextWriterStartElement(writer, BAD_CAST "element") < 0) {
					fprintf(stderr,"Error at xmlTextWriterStartElement\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "atomic_number","%i",input->absorbers->det_layers[i].Z[j]) < 0) {
					fprintf(stderr,"Error writing atomic_number\n");
					return 0;
				}
				if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "weight_fraction","%lg",input->absorbers->det_layers[i].weight[j]*100.0) < 0) {
					fprintf(stderr,"Error writing weight_number\n");
					return 0;
				}
			
		
				if (xmlTextWriterEndElement(writer) < 0) {
					fprintf(stderr,"Error calling xmlTextWriterEndElement for element\n");
					return 0;
				}
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "density","%lg",input->absorbers->det_layers[i].density) < 0) {
				fprintf(stderr,"Error writing density\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "thickness","%lg",input->absorbers->det_layers[i].thickness) < 0) {
				fprintf(stderr,"Error writing thickness\n");
				return 0;
			}
	
			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error calling xmlTextWriterEndElement for layer\n");
				return 0;
			}
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
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "live_time","%lf",input->detector->live_time) < 0) {
		fprintf(stderr,"Error writing live_time\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "pulse_width","%lg",input->detector->pulse_width) < 0) {
		fprintf(stderr,"Error writing pulse_width\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "gain","%lg",input->detector->gain) < 0) {
		fprintf(stderr,"Error writing gain\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "zero","%lg",input->detector->zero) < 0) {
		fprintf(stderr,"Error writing zero\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "fano","%lg",input->detector->fano) < 0) {
		fprintf(stderr,"Error writing fano\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "noise","%lg",input->detector->noise) < 0) {
		fprintf(stderr,"Error writing noise\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "max_convolution_energy","%lg",input->detector->max_convolution_energy) < 0) {
		fprintf(stderr,"Error writing max_convolution_energy\n");
		return 0;
	}
	if (xmlTextWriterStartElement(writer, BAD_CAST "crystal") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}
	for (i = 0 ; i < input->detector->n_crystal_layers ; i++) {
		if (xmlTextWriterStartElement(writer, BAD_CAST "layer") < 0) {
			fprintf(stderr,"Error at xmlTextWriterStartElement\n");
			return 0;
		}
		for (j = 0 ; j < input->detector->crystal_layers[i].n_elements ; j++) {
			if (xmlTextWriterStartElement(writer, BAD_CAST "element") < 0) {
				fprintf(stderr,"Error at xmlTextWriterStartElement\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "atomic_number","%i",input->detector->crystal_layers[i].Z[j]) < 0) {
				fprintf(stderr,"Error writing atomic_number\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "weight_fraction","%lg",input->detector->crystal_layers[i].weight[j]*100.0) < 0) {
				fprintf(stderr,"Error writing weight_number\n");
				return 0;
			}
			
		
			if (xmlTextWriterEndElement(writer) < 0) {
				fprintf(stderr,"Error calling xmlTextWriterEndElement for element\n");
				return 0;
			}
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "density","%lg",input->detector->crystal_layers[i].density) < 0) {
			fprintf(stderr,"Error writing density\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "thickness","%lg",input->detector->crystal_layers[i].thickness) < 0) {
			fprintf(stderr,"Error writing thickness\n");
			return 0;
		}
	
		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error calling xmlTextWriterEndElement for layer\n");
			return 0;
		}
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
double *channels, int nchannels, double maximum2 ) {
	
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
	for (i = nchannels-1 ; i >= 0 ; i--) {
		if (channels[i] >= 1) {
		max_channel = i;
		break;
		}
	}

	energies = xmi_dindgen(nchannels);
	xmi_add_val_to_array_double(energies, nchannels, 1.0);
	xmi_scale_double(energies, nchannels, input->detector->gain);
	xmi_add_val_to_array_double(energies, nchannels, input->detector->zero);

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
        if (!error){if (xmlTextWriterWriteFormatString(writer,"%f", parm ) < 0 ) {error = 1;}}
        if (!error){if (xmlTextWriterEndElement(writer) < 0) {error = 1;}}

        if (error) fprintf(stderr,"Error parameter svg element %s %i \n", element, parm);         
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

	if(out_of_range > 0)fprintf(stderr, "svg x-values out of range \n");

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

//int xmi_read_output_xml()

