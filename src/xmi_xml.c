#include "xmi_xml.h"
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>


static int readLayerXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_layer **layer);
static int readGeneralXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_general **general);
static int readCompositionXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_composition **composition);
static int readGeometryXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_geometry **geometry);
static int readExcitationXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_excitation **excitation);
static int readAbsorbersXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_absorbers **absorbers);
static int readDetectorXML(xmlDocPtr doc, xmlNodePtr nodePtr, struct xmi_detector **detector);




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
			if(sscanf((const char *)txt,"%i",&((*general)->version)) != 1) {
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
/*		else if (!xmlStrcmp(subroot->name,(const xmlChar *) "composition")) {
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
		}*/

		subroot = subroot->next;
	}

	xmlFreeParserCtxt(ctx);
	xmlFreeDoc(doc);
	return 1;

}
