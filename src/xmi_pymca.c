#include "xmi_pymca.h"
#include <glib.h>
#include <stdio.h>
#include <xraylib-parser.h>
#include <stdlib.h>
#include <string.h>



int get_composition(GKeyFile *pymcaFile, char *compositionString, struct xmi_layer **layer) {
	int rv = 0;
	gchar *predefGroup;


	//remove any leading spaces
	while (*compositionString == ' ')
		compositionString++;

#if DEBUG == 1
	fprintf(stdout,"composition without leading space: x%sx\n",compositionString);
#endif


	predefGroup = (gchar *) malloc(sizeof(gchar)*(strlen("result.config.materials.")+strlen(compositionString)+1));
	strcpy(predefGroup,"result.config.materials.");
	strcat(predefGroup,compositionString);

#if DEBUG == 1
	fprintf(stdout,"predefGroup: %s\n",predefGroup);
#endif


	//First check if it's not one of the predefined layers...
	if (g_key_file_has_group(pymcaFile, predefGroup) == TRUE) {
#if DEBUG == 1
		fprintf(stdout,"Found predefGroup\n");
#endif
	}
	



	rv = 1;
	return rv;
}



int read_atmosphere_composition(GKeyFile *pymcaFile, struct xmi_layer **atmosphere_layer ) {
	int rv=0;
	char **strings = NULL;
	gsize length = 0;
	int i;

	if ( (strings = g_key_file_get_string_list(pymcaFile, "result.config.attenuators", "atmosphere", &length, NULL)) == NULL) {
		fprintf(stdout,"No atmosphere found in model\n");
		return 1;
	}

#if DEBUG == 1
	for (i=0 ; i < length ; i++)
		fprintf(stdout,"x%sx\n",strings[i]);
#endif
	get_composition(pymcaFile, strings[1], atmosphere_layer);	


	rv = 1;
	return rv;
}



int xmi_read_input_pymca(char *pymca_file, struct xmi_input **input) {
	int rv = 0;
	GKeyFile *pymcaFile;
	GError *error=NULL;
	struct xmi_layer *atmosphere_layer = NULL;


	//read the file...
	pymcaFile = g_key_file_new();
	if (g_key_file_load_from_file(pymcaFile, pymca_file, G_KEY_FILE_NONE, &error) == FALSE) {
		fprintf(stderr,"Could not parse file %s\n",pymca_file);
		fprintf(stderr,"Error message: %s\n",error->message);
		return rv;
	}

	g_key_file_set_list_separator(pymcaFile, ',');

	//read atmosphere composition
	if (read_atmosphere_composition(pymcaFile, &atmosphere_layer) == 0)
		return rv;
	





	rv = 1;

	return rv;
}
