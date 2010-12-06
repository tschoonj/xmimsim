#include "xmi_xml.h"
#include "xmi_aux.h"
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xmlwriter.h>
#include <string.h>
#include <math.h>



static int readLayerXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_layer *layer);
static int readGeneralXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_general **general);
static int readCompositionXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_composition **composition);
static int readGeometryXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_geometry **geometry);
static int readExcitationXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_excitation **excitation);
static int readAbsorbersXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_absorbers **absorbers);
static int readDetectorXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_detector **detector);
static int xmi_cmp_struct_xmi_energy(const void *a, const void *b);




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
			(*general)->outputfile = (char *) xmlStrdup(txt); 
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
						fprintf(stderr,"error reading in p_detector_window y of xml file\n");
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
						fprintf(stderr,"error reading in n_detector_orientation y of xml file\n");
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
		else if (!xmlStrcmp(subnode->name,(const xmlChar *) "acceptance_detector")){
			txt = xmlNodeListGetString(doc,subnode->children,1);
			if(sscanf((const char *)txt,"%lf",&((*geometry)->acceptance_detector)) != 1) {
				fprintf(stderr,"error reading in acceptance_detector of xml file\n");
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
	//char catalog[] = XMI_CATALOG;

	LIBXML_TEST_VERSION

	//catalog code
/*	if (xmlLoadCatalog(catalog) != 0) {
		fprintf(stderr,"Could not load xmi_catalog.xml\n");
		return 0;
	}
*/


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

int xmi_write_input_xml(char *xmlfile, struct xmi_input *input) {

	xmlTextWriterPtr writer;
	xmlDocPtr doc;
	char version[100];
	char detector_type[20];
	int i,j;



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

	//general
	if (xmlTextWriterStartElement(writer, BAD_CAST "general") < 0) {
		fprintf(stderr,"Error at xmlTextWriterStartElement\n");
		return 0;
	}

	sprintf(version,"%.1f",input->general->version);
	if (xmlTextWriterWriteAttribute(writer, BAD_CAST "version", BAD_CAST version) < 0) {
		fprintf(stderr,"Error at xmlTextWriterWriteAttribute\n");
		return 0;
	}

	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "outputfile","%s",input->general->outputfile) < 0) {
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
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "density","%lf",input->composition->layers[i].density) < 0) {
			fprintf(stderr,"Error writing density\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "thickness","%lf",input->composition->layers[i].thickness) < 0) {
			fprintf(stderr,"Error writing thickness\n");
			return 0;
		}
	
		if (xmlTextWriterEndElement(writer) < 0) {
			fprintf(stderr,"Error calling xmlTextWriterEndElement for layer\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "reference_layer","%i",input->composition->reference_layer) < 0) {
			fprintf(stderr,"Error writing reference_layer\n");
			return 0;
		}
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
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "acceptance_detector","%lf",input->geometry->acceptance_detector) < 0) {
		fprintf(stderr,"Error writing acceptance_detector\n");
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
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "density","%lf",input->absorbers->exc_layers[i].density) < 0) {
				fprintf(stderr,"Error writing density\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "thickness","%lf",input->absorbers->exc_layers[i].thickness) < 0) {
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
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "density","%lf",input->absorbers->det_layers[i].density) < 0) {
				fprintf(stderr,"Error writing density\n");
				return 0;
			}
			if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "thickness","%lf",input->absorbers->det_layers[i].thickness) < 0) {
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
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "gain","%lf",input->detector->gain) < 0) {
		fprintf(stderr,"Error writing gain\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "zero","%lf",input->detector->zero) < 0) {
		fprintf(stderr,"Error writing zero\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "fano","%lf",input->detector->fano) < 0) {
		fprintf(stderr,"Error writing fano\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "noise","%lf",input->detector->noise) < 0) {
		fprintf(stderr,"Error writing noise\n");
		return 0;
	}
	if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "max_convolution_energy","%lf",input->detector->max_convolution_energy) < 0) {
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
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "density","%lf",input->detector->crystal_layers[i].density) < 0) {
			fprintf(stderr,"Error writing density\n");
			return 0;
		}
		if (xmlTextWriterWriteFormatElement(writer,BAD_CAST "thickness","%lf",input->detector->crystal_layers[i].thickness) < 0) {
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
