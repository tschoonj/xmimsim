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
#include "xmi_xslt.h"
#include "xmi_xml.h"
#include <libxslt/xslt.h>
#include <libxslt/xsltInternals.h>
#include <libxml/xmlmemory.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>
#include <glib.h>
#include <string.h>

#ifdef G_OS_WIN32
#include "xmi_registry_win.h"
#elif defined(MAC_INTEGRATION)
#include "xmi_resources_mac.h"
#endif

static void update_xpath_nodes(xmlNodeSetPtr nodes, const xmlChar* value) {
	int size;
	int i;
	   
	size = (nodes) ? nodes->nodeNr : 0;
	for(i = size - 1; i >= 0; i--) {
		xmlNodeSetContent(nodes->nodeTab[i], value);

		/*if (nodes->nodeTab[i]->type != XML_NAMESPACE_DECL)
	    		nodes->nodeTab[i] = NULL;*/
	}
}		


int xmi_xmso_to_xmsi_xslt(char *xmsofile, char *xmsifile , char *outputfile  ) {

	xsltStylesheetPtr cur = NULL;
	xmlDocPtr doc, res;
	xmlParserCtxtPtr ctx;
	const char *params[1] = {NULL};
	xmlXPathContextPtr xpathCtx; 
	xmlXPathObjectPtr xpathObj;
	const gchar *env = g_getenv("XMI_XMSO2XMSI_XSLT");
	xmlChar *xsltfile = NULL;

	if (env == NULL) {
#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_XMSO2XMSI,(char **) &xsltfile) == 0)
			return 0;
#elif defined(MAC_INTEGRATION)
		if (xmi_resources_mac_query(XMI_RESOURCES_MAC_XMSO2XMSI,(char **) &xsltfile) == 0)
			return 0;
#else
		xsltfile = strdup(XMI_XMSO2XMSI_XSLT);
#endif
	}
	else {
		xsltfile = strdup(env);
	}

	xsltInit();

	cur = xsltParseStylesheetFile(xsltfile);
	if (cur == NULL) {
		fprintf(stderr, "Could not parse stylesheet %s\n", xsltfile);
		return 0;
	}

	free(xsltfile);

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

	res = xsltApplyStylesheet(cur, doc, params);
	if (res == NULL) {
		fprintf(stderr, "Could not apply stylesheet %s to %s\n", xsltfile, xmsofile);
		return 0;
	}

	if (outputfile != NULL) {
    		xpathCtx = xmlXPathNewContext(res);
        	if(xpathCtx == NULL) {
	        	fprintf(stderr,"Error: unable to create new XPath context\n");
		        return 0;
		}
		xpathObj = xmlXPathEvalExpression((const xmlChar *) "/xmimsim/general/outputfile", xpathCtx);
		if(xpathObj == NULL) {
		        fprintf(stderr,"Error: unable to evaluate xpath expression \"%s\"\n","xmimsim/general/outputfile" );
			xmlXPathFreeContext(xpathCtx); 
			return 0;
		}
		update_xpath_nodes(xpathObj->nodesetval, (const xmlChar *) outputfile);
		xmlXPathFreeObject(xpathObj);
	        xmlXPathFreeContext(xpathCtx);
	}


	xsltSaveResultToFilename(xmsifile, res, cur, 0);

	xsltFreeStylesheet(cur);
	xmlFreeDoc(res);
	xmlFreeDoc(doc);

        xsltCleanupGlobals();

	return 1;
}


int xmi_xmso_to_svg_xslt(char *xmsofile, char *xmsifile, unsigned convoluted) {

	xsltStylesheetPtr cur = NULL;
	xmlDocPtr doc, res;
	xmlParserCtxtPtr ctx;
	const char *params[3];

	char parm_name[] = "type1";
        char s_convoluted[] = "'convoluted'";
        char s_unconvoluted[] = "'unconvoluted'";

	const gchar *env = g_getenv("XMI_XMSO2SVG_XSLT");
	xmlChar *xsltfile = NULL;

	if (env == NULL) {
#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_XMSO2SVG,(char **) &xsltfile) == 0)
			return 0;
#elif defined(MAC_INTEGRATION)
		if (xmi_resources_mac_query(XMI_RESOURCES_MAC_XMSO2SVG,(char **) &xsltfile) == 0)
			return 0;
#else
		xsltfile = strdup(XMI_XMSO2SVG_XSLT);
#endif
	}
	else {
		xsltfile = strdup(env);
	}

	xsltInit();

	params[0] = parm_name;
        if ( convoluted )
         params[1] = s_convoluted;
        else
         params[1] = s_unconvoluted;
        params[2] = NULL;

       	//fprintf(stdout, "parm 0 = %s \n", params[0] ); 
	//fprintf(stdout, "parm 1 = %s \n", params[1] );
	//fprintf(stdout, "parm 2 = %s \n", params[2] );  

	cur = xsltParseStylesheetFile(xsltfile);
	if (cur == NULL) {
		fprintf(stderr, "Could not parse stylesheet %s\n", xsltfile);
		return 0;
	}

	free(xsltfile);

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

	res = xsltApplyStylesheet(cur, doc, params);
	if (res == NULL) {
		fprintf(stderr, "Could not apply stylesheet %s to %s\n", xsltfile, xmsofile);
		return 0;
	}

	xsltSaveResultToFilename(xmsifile, res, cur, 0);

	xsltFreeStylesheet(cur);
	xmlFreeDoc(res);
	xmlFreeDoc(doc);

        xsltCleanupGlobals();

	return 1;

}

int xmi_xmso_to_spe_xslt(char *xmsofile, char *spefile, unsigned convoluted, int interaction_number) {

	xsltStylesheetPtr cur = NULL;
	xmlDocPtr doc, res;
	xmlParserCtxtPtr ctx;
	const char *params[5];

	char parm_name1[] = "type";
	char parm_name2[] = "interaction";
        char s_convoluted[] = "'spectrum_conv'";
        char s_unconvoluted[] = "'spectrum_unconv'";
	char interaction[10];

	const gchar *env = g_getenv("XMI_XMSO2SPE_XSLT");
	xmlChar *xsltfile = NULL;

	if (env == NULL) {
#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_XMSO2SPE,(char **) &xsltfile) == 0)
			return 0;
#elif defined(MAC_INTEGRATION)
		if (xmi_resources_mac_query(XMI_RESOURCES_MAC_XMSO2SPE,(char **) &xsltfile) == 0)
			return 0;
#else
		xsltfile = strdup(XMI_XMSO2SPE_XSLT);
#endif
	}
	else {
		xsltfile = strdup(env);
	}

	xsltInit();

	params[0] = parm_name1;
        if ( convoluted )
         params[1] = s_convoluted;
        else
         params[1] = s_unconvoluted;
	params[2] = parm_name2;
	sprintf(interaction,"'%i'",interaction_number);
	params[3] = interaction;
        params[4] = NULL;

       	//fprintf(stdout, "parm 0 = %s \n", params[0] ); 
	//fprintf(stdout, "parm 1 = %s \n", params[1] );
	//fprintf(stdout, "parm 2 = %s \n", params[2] );  

	cur = xsltParseStylesheetFile(xsltfile);
	if (cur == NULL) {
		fprintf(stderr, "Could not parse stylesheet %s\n", xsltfile);
		return 0;
	}

	free(xsltfile);

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

	res = xsltApplyStylesheet(cur, doc, params);
	if (res == NULL) {
		fprintf(stderr, "Could not apply stylesheet %s to %s\n", xsltfile, xmsofile);
		return 0;
	}

	xsltSaveResultToFilename(spefile, res, cur, 0);

	xsltFreeStylesheet(cur);
	xmlFreeDoc(res);
	xmlFreeDoc(doc);

        xsltCleanupGlobals();

	return 1;

}

int xmi_xmso_to_csv_xslt(char *xmsofile, char *csvfile, unsigned convoluted) {


	xsltStylesheetPtr cur = NULL;
	xmlDocPtr doc, res;
	xmlParserCtxtPtr ctx;
	const char *params[3];

	char parm_name[] = "type";
        char s_convoluted[] = "'spectrum_conv'";
        char s_unconvoluted[] = "'spectrum_unconv'";

	const gchar *env = g_getenv("XMI_XMSO2CSV_XSLT");
	xmlChar *xsltfile = NULL;


	if (env == NULL) {
#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_XMSO2CSV,(char **) &xsltfile) == 0)
			return 0;
#elif defined(MAC_INTEGRATION)
		if (xmi_resources_mac_query(XMI_RESOURCES_MAC_XMSO2CSV,(char **) &xsltfile) == 0)
			return 0;
#else
		xsltfile = strdup(XMI_XMSO2CSV_XSLT);
#endif
	}
	else {
		xsltfile = strdup(env);
	}

	xsltInit();

	params[0] = parm_name;
        if (convoluted)
         params[1] = s_convoluted;
        else
         params[1] = s_unconvoluted;
        params[2] = NULL;

       	//fprintf(stdout, "parm 0 = %s \n", params[0] ); 
	//fprintf(stdout, "parm 1 = %s \n", params[1] );
	//fprintf(stdout, "parm 2 = %s \n", params[2] );  

	cur = xsltParseStylesheetFile(xsltfile);
	if (cur == NULL) {
		fprintf(stderr, "Could not parse stylesheet %s\n", xsltfile);
		return 0;
	}

	free(xsltfile);

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

	res = xsltApplyStylesheet(cur, doc, params);
	if (res == NULL) {
		fprintf(stderr, "Could not apply stylesheet %s to %s\n", xsltfile, xmsofile);
		return 0;
	}

	xsltSaveResultToFilename(csvfile, res, cur, 0);

	xsltFreeStylesheet(cur);
	xmlFreeDoc(res);
	xmlFreeDoc(doc);

        xsltCleanupGlobals();

	return 1;

}

int xmi_xmso_to_htm_xslt(char *xmsofile, char *xmsifile, unsigned convoluted) {

	xsltStylesheetPtr cur = NULL;
	xmlDocPtr doc, res;
	xmlParserCtxtPtr ctx;
	const char *params[3];

	char parm_name[] = "type1";
        char s_convoluted[] = "'convoluted'";
        char s_unconvoluted[] = "'unconvoluted'";

	const gchar *env = g_getenv("XMI_XMSO2HTM_XSLT");
	xmlChar *xsltfile = NULL;

	if (env == NULL) {
#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_XMSO2HTM,(char **) &xsltfile) == 0)
			return 0;
#elif defined(MAC_INTEGRATION)
		if (xmi_resources_mac_query(XMI_RESOURCES_MAC_XMSO2HTM,(char **) &xsltfile) == 0)
			return 0;
#else
		xsltfile = strdup(XMI_XMSO2HTM_XSLT);
#endif
	}
	else {
		xsltfile = strdup(env);
	}

	xsltInit();

	params[0] = parm_name;

        if (convoluted)
         	params[1] = s_convoluted;
        else
         	params[1] = s_unconvoluted;

        params[2] = NULL;

       	//fprintf(stdout, "parm 0 = %s \n", params[0] ); 
	//fprintf(stdout, "parm 1 = %s \n", params[1] );
	//fprintf(stdout, "parm 2 = %s \n", params[2] );  

	cur = xsltParseStylesheetFile(xsltfile);
	if (cur == NULL) {
		fprintf(stderr, "Could not parse stylesheet %s\n", xsltfile);
		return 0;
	}

	free(xsltfile);

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

	res = xsltApplyStylesheet(cur, doc, params);
	if (res == NULL) {
		fprintf(stderr, "Could not apply stylesheet %s to %s\n", xsltfile, xmsofile);
		return 0;
	}

	xsltSaveResultToFilename(xmsifile, res, cur, 0);

	xsltFreeStylesheet(cur);
	xmlFreeDoc(res);
	xmlFreeDoc(doc);

        xsltCleanupGlobals();

	return 1;

}

int xmi_xmsa_to_xmso_xslt(char *xmsafile, char *xmsofile, int step1, int step2) {
	xsltStylesheetPtr cur = NULL;
	xmlDocPtr doc, res;
	xmlParserCtxtPtr ctx;
	const gchar **params;
	xmlXPathContextPtr xpathCtx; 
	xmlXPathObjectPtr xpathObj;
	gchar *xmsofilenew;

	const gchar *env = g_getenv("XMI_XMSA2XMSO_XSLT");
	xmlChar *xsltfile = NULL;

	if (env == NULL) {
#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_XMSA2XMSO,(char **) &xsltfile) == 0)
			return 0;
#elif defined(MAC_INTEGRATION)
		if (xmi_resources_mac_query(XMI_RESOURCES_MAC_XMSA2XMSO,(char **) &xsltfile) == 0)
			return 0;
#else
		xsltfile = strdup(XMI_XMSA2XMSO_XSLT);
#endif
	}
	else {
		xsltfile = strdup(env);
	}

	xsltInit();

       	//fprintf(stdout, "parm 0 = %s \n", params[0] ); 
	//fprintf(stdout, "parm 1 = %s \n", params[1] );
	//fprintf(stdout, "parm 2 = %s \n", params[2] );  

	cur = xsltParseStylesheetFile(xsltfile);
	if (cur == NULL) {
		fprintf(stderr, "Could not parse stylesheet %s\n", xsltfile);
		return 0;
	}

	free(xsltfile);

	if ((ctx=xmlNewParserCtxt()) == NULL) {
		fprintf(stderr,"xmlNewParserCtxt error\n");
		xsltFreeStylesheet(cur);
		return 0;
	}

	if ((doc = xmlCtxtReadFile(ctx,xmsafile,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		fprintf(stderr,"xmlCtxtReadFile error for %s\n",xmsafile);
		xmlFreeParserCtxt(ctx);
		xsltFreeStylesheet(cur);
		return 0;
	}	

	if (ctx->valid == 0) {
		fprintf(stderr,"Error validating %s\n",xmsafile);
		xmlFreeParserCtxt(ctx);
		xmlFreeDoc(doc);
		xsltFreeStylesheet(cur);
		return 0;
	}
	xmlFreeParserCtxt(ctx);

	//check if step1 and step2 are valid values
	int nsteps1 = 0, nsteps2 = 0;
	xmlChar *nsteps1_string = NULL, *nsteps2_string = NULL;

    	xpathCtx = xmlXPathNewContext(doc);
       	if(xpathCtx == NULL) {
        	fprintf(stderr,"Error: unable to create new XPath context\n");
		xsltFreeStylesheet(cur);
		xmlFreeDoc(doc);
	        return 0;
	}
	xpathObj = xmlXPathEvalExpression((const xmlChar *) "/xmimsim-archive/nsteps1", xpathCtx);
	//these two checks are actually not necessary because the DTD has been checked already...
	if(xpathObj == NULL) {
	        fprintf(stderr,"Error: unable to evaluate xpath expression \"%s\"\n","/xmimsim-archive/nsteps1");
		xmlXPathFreeContext(xpathCtx); 
		xsltFreeStylesheet(cur);
		xmlFreeDoc(doc);
		return 0;
	}
	if (xmlXPathNodeSetIsEmpty(xpathObj->nodesetval)) {
		fprintf(stderr, "Error: XPath /xmimsim-archive/nsteps1 node not found\n");
		xmlXPathFreeObject(xpathObj);
		xsltFreeStylesheet(cur);
		xmlFreeDoc(doc);
		return 0;
	}
	nsteps1_string = xmlNodeListGetString(doc, xpathObj->nodesetval->nodeTab[0]->children, 1);
	xmlXPathFreeObject(xpathObj);
	sscanf((const char *) nsteps1_string, "%i", &nsteps1);
	xmlFree(nsteps1_string);

	if (step1 > nsteps1) {
		fprintf(stderr, "Error: step1 cannot be greater than %i in this archive\n", nsteps1);
		xsltFreeStylesheet(cur);
		xmlXPathFreeContext(xpathCtx); 
		xmlFreeDoc(doc);
		return 0;
	}

	xpathObj = xmlXPathEvalExpression((const xmlChar *) "/xmimsim-archive/nsteps2", xpathCtx);
	if(xpathObj == NULL) {
	        fprintf(stderr,"Error: unable to evaluate xpath expression \"%s\"\n","/xmimsim-archive/nsteps2");
		xmlXPathFreeContext(xpathCtx); 
		xsltFreeStylesheet(cur);
		xmlFreeDoc(doc);
		return 0;
	}
	if (xmlXPathNodeSetIsEmpty(xpathObj->nodesetval)) {
		nsteps2 = 0;
		if (step2 > 0) {
			fprintf(stderr, "Error: step2 cannot be greater than 0 in an archive with one XPath parameter\n");
			xmlXPathFreeObject(xpathObj);
			xmlXPathFreeContext(xpathCtx); 
			xsltFreeStylesheet(cur);
			xmlFreeDoc(doc);
			return 0;
		}
	}
	else {
		nsteps2_string = xmlNodeListGetString(doc, xpathObj->nodesetval->nodeTab[0]->children, 1);
		sscanf((const char *) nsteps2_string, "%i", &nsteps2);
		xmlFree(nsteps2_string);
		if (step2 > nsteps2) {
			fprintf(stderr, "Error: step2 cannot be greater than %i in this archive\n", nsteps2);
			xmlXPathFreeObject(xpathObj);
			xmlXPathFreeContext(xpathCtx); 
			xsltFreeStylesheet(cur);
			xmlFreeDoc(doc);
			return 0;
		}
	}
	xmlXPathFreeObject(xpathObj);
	xmlXPathFreeContext(xpathCtx); 
	

	if (step1 == -1 && step2 == -1) {
		//lets's speed things up a bit here...
		//also, let's hope that libxslt and libxml2 are threadsafe...
		//needs to be tested on Windows
		//look into LIBXML_THREAD_ENABLED from xmlversion.h if necessary
#ifdef _OPENMP
#pragma omp parallel for collapse(2), default(shared), private(step1, step2, xmsofilenew, params, res)
#endif
		for (step1 = 0 ; step1 <= nsteps1 ; step1++) {
			for (step2 = 0 ; step2 <= nsteps2 ; step2++) {
				params = g_malloc(sizeof(gchar *)*5);
				params[0] = g_strdup("step1");
        			params[1] = g_strdup_printf("'%i'", step1);
				params[2] = g_strdup("step2");
        			params[3] = g_strdup_printf("'%i'", step2);
        			params[4] = NULL;

				res = xsltApplyStylesheet(cur, doc, params);
				g_strfreev((gchar **) params);
				if (res == NULL) {
					fprintf(stderr, "Could not apply stylesheet %s to %s\n", xsltfile, xmsafile);
					//xsltFreeStylesheet(cur);
					//xmlFreeDoc(doc);
					//return 0;
					continue;
				}

				if (nsteps2 == 0)
					xmsofilenew = g_strdup_printf("%s_%i.xmso", xmsofile, step1);
				else
					xmsofilenew = g_strdup_printf("%s_%i_%i.xmso", xmsofile, step1, step2);

				xsltSaveResultToFilename(xmsofilenew, res, cur, 0);
				g_free(xmsofilenew);
				xmlFreeDoc(res);
			}
		}
	}
	else {
		params = g_malloc(sizeof(gchar *)*5);
		params[0] = g_strdup("step1");
        	params[1] = g_strdup_printf("'%i'", step1);
		params[2] = g_strdup("step2");
        	params[3] = g_strdup_printf("'%i'", step2);
        	params[4] = NULL;

		res = xsltApplyStylesheet(cur, doc, params);
		g_strfreev((gchar **) params);
		if (res == NULL) {
			fprintf(stderr, "Could not apply stylesheet %s to %s\n", xsltfile, xmsafile);
			xsltFreeStylesheet(cur);
			xmlFreeDoc(doc);
			return 0;
		}
		xsltSaveResultToFilename(xmsofile, res, cur, 0);
		xmlFreeDoc(res);
	}

	xsltFreeStylesheet(cur);
	xmlFreeDoc(doc);

        xsltCleanupGlobals();

	return 1;
}
