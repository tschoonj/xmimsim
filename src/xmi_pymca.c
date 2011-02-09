#include "xmi_pymca.h"
#include "xmi_aux.h"
#include <glib.h>
#include <stdio.h>
#include <xraylib-parser.h>
#include <stdlib.h>
#include <string.h>



int get_composition(GKeyFile *pymcaFile, char *compositionString, struct xmi_layer **layer) {
	int rv = 0;
	gchar *predefGroup;
	gchar **strings=NULL;
	gchar **compoundlist=NULL;
	gsize lengthfractions, lengthlist;
	GError *error = NULL;
	gdouble *compoundfractions;
	struct xmi_layer *temp1,*temp2;
	struct compoundData *cd1, *cd2, *cd_sum;
	int i;


	//remove any leading spaces
	g_strchug(compositionString);

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
		//don't get density and thickness here! Just the composition...
		//CompoundFraction -> doubles
		compoundfractions = g_key_file_get_double_list(pymcaFile, predefGroup, "CompoundFraction", &lengthfractions, &error);
		if (compoundfractions == NULL) {
			fprintf(stderr,"Error parsing compoundfractions\n");
			fprintf(stderr,"GLib error message: %s\n",error->message);
			return rv;
		}
		//CompoundList -> strings
		compoundlist = g_key_file_get_string_list(pymcaFile, predefGroup, "CompoundList", &lengthlist, &error);
		if (compoundlist == NULL) {
			fprintf(stderr,"Error parsing compoundlist\n");
			fprintf(stderr,"GLib error message: %s\n",error->message);
			return rv;
		}
		if (lengthlist != lengthfractions) {
			fprintf(stderr,"CompoundList and CompoundFractions have different lengths\n");
			return rv;
		}

		if (lengthlist == 1) {
			//only one compound
			cd1 = (struct compoundData *) malloc(sizeof(struct compoundData));
			if (CompoundParser(g_strchug(compoundlist[0]),cd1) == 0) {
				fprintf(stderr,"Could not parse compound %s\n",compoundlist[0]);
				return rv;
			}
			*layer = compoundData2xmi_layer (cd1);
			FREE_COMPOUND_DATA(*cd1);
			free(cd1);
		}
		else {
			//more than one compound...
			//start with the first compound
			cd1 = (struct compoundData *) malloc(sizeof(struct compoundData));
			if (CompoundParser(g_strchug(compoundlist[0]),cd1) == 0) {
				fprintf(stderr,"Could not parse compound %s\n",compoundlist[0]);
				return rv;
			}
			for (i = 1 ; i < lengthlist ; i++) {
				cd2 = (struct compoundData *) malloc(sizeof(struct compoundData));
				if (CompoundParser(g_strchug(compoundlist[i]),cd2) == 0) {
					fprintf(stderr,"Could not parse compound %s\n",compoundlist[i]);
					return rv;
				}
				//sum up cd1 and cd2
				cd_sum = add_compound_data(*cd1,i==1 ? compoundfractions[0] : 1.0,*cd2, compoundfractions[i]);
				FREE_COMPOUND_DATA(*cd1);
				free(cd1);
				cd1 = cd_sum;
				FREE_COMPOUND_DATA(*cd2);
				free(cd2);
			}
			*layer = compoundData2xmi_layer (cd_sum);
			FREE_COMPOUND_DATA(*cd_sum);
			free(cd_sum);
		}
		g_strfreev(compoundlist);
		g_free(compoundfractions);

	}
	else {
		//not predefined... feed it directly to xraylib
		cd1 = (struct compoundData *) malloc(sizeof(struct compoundData));
		if (CompoundParser(compositionString,cd1) == 0) {
			fprintf(stderr,"Could not parse compound %s\n",compositionString);
			return rv;
		}
		*layer = compoundData2xmi_layer (cd1);
		FREE_COMPOUND_DATA(*cd1);
		free(cd1);
	}

#if DEBUG == 1
	for (i = 0 ; i < (*layer)->n_elements ; i++) {
		fprintf(stdout,"Element: %i\n",(*layer)->Z[i]);
		fprintf(stdout,"Weight: %lf\n",(*layer)->weight[i]);
	}
#endif



	rv = 1;
	return rv;
}



int read_atmosphere_composition(GKeyFile *pymcaFile, struct xmi_layer **atmosphere_layer ) {
	int rv=0;
	char **strings = NULL;
	gsize length = 0;
	int i;
	gint active ;

	if ( (strings = g_key_file_get_string_list(pymcaFile, "result.config.attenuators", "atmosphere", &length, NULL)) == NULL) {
		fprintf(stdout,"No atmosphere reference found in model... looks suspicious\n");
		return 1;
	}

	//see if it's activated
	active = atoi(strings[0]);
	
	if (active != 1) {
		fprintf(stdout,"Atmosphere layer inactive\n");
		return 1;
	}


#if DEBUG == 1
	for (i=0 ; i < length ; i++)
		fprintf(stdout,"x%sx\n",strings[i]);
#endif
	get_composition(pymcaFile, strings[1], atmosphere_layer);	

	//get density and thickness
	(*atmosphere_layer)->density = g_ascii_strtod(strings[2],NULL);
	(*atmosphere_layer)->thickness= g_ascii_strtod(strings[3],NULL);

	g_strfreev(strings);


	rv = 1;
	return rv;
}

int read_multilayer_composition(GKeyFile *pymcaFile, struct xmi_layer **multilayer_layers, int *n_multilayer_layers) {



}

//int get_peak_areas

int xmi_read_input_pymca(char *pymca_file, struct xmi_input **input, struct xmi_pymca **pymca_input) {
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
