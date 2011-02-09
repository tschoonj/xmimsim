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

int get_peak_areas(GKeyFile *pymcaFile, struct xmi_pymca **pymca_input) {
	int rv = 0;
	gchar **elements, **lines;
	gsize n_elements, n_lines;
	GError *error;
	int i, Z, j;
	int K_found, Ka_found, Kb_found, L_found, L1_found, L2_found, L3_found;
	int use_K, use_L;
	gchar buffer[128];


	//first examine result.config.peaks...
	elements = g_key_file_get_keys(pymcaFile, "result.config.peaks", &n_elements, &error);
	if (elements == NULL || n_elements == 0) {
		fprintf(stderr,"No peaks were configured for the fit: aborting\n");
		return rv;
	}
	
	//malloc memory...
	*pymca_input = (struct xmi_pymca *) malloc(sizeof(struct xmi_pymca)); 
	(*pymca_input)->n_peaks = n_elements;
	(*pymca_input)->z_arr = (int *) malloc(sizeof(int)*n_elements);
	(*pymca_input)->k_alpha = (double *) malloc(sizeof(double)*n_elements);
	(*pymca_input)->l_alpha = (double *) malloc(sizeof(double)*n_elements);

	for (i = 0 ; i < n_elements ; i++) {
#if DEBUG == 1
		fprintf(stdout,"Examining peaks of %s\n",elements[i]);
#endif
		Z = SymbolToAtomicNumber(g_strstrip(elements[i]));
		(*pymca_input)->z_arr[i] = Z;

		//check the lines 
		K_found = Ka_found = Kb_found = L_found = L1_found = L2_found = L3_found = 0;

		lines = g_key_file_get_string_list(pymcaFile, "result.config.peaks",elements[i], &n_lines, &error);
		
		if (lines == NULL || n_lines == 0) {
			fprintf(stderr,"Element %s appears to have no peaks configured although mentioned in result.config.peaks\n",elements[i]);
			return rv;
		}

		//check lines
		for (j = 0 ; j < n_lines ; j++) {
			if (strcmp("K",g_strstrip(lines[j])) == 0) {
				K_found = 1;
			}
			else if (strcmp("Ka",g_strstrip(lines[j])) == 0) {
				Ka_found = 1;
			}
			else if (strcmp("Kb",g_strstrip(lines[j])) == 0) {
				Kb_found = 1;
			}
			else if (strcmp("L",g_strstrip(lines[j])) == 0) {
				L_found = 1;
			}
			else if (strcmp("L1",g_strstrip(lines[j])) == 0) {
				L1_found = 1;
			}
			else if (strcmp("L2",g_strstrip(lines[j])) == 0) {
				L2_found = 1;
			}
			else if (strcmp("L3",g_strstrip(lines[j])) == 0) {
				L3_found = 1;
			}
			else if (strcmp("M",g_strstrip(lines[j])) == 0) {
				fprintf(stdout,"M-lines are not yet supported...\n");
			}
		}

		g_strfreev(lines);

                //K-line
		if ( (K_found && (Ka_found || Kb_found))) {
			fprintf(stderr,"Invalid combination of K-lines detected for element %s\n",elements[i]);
			return rv;
		}
		else if (!K_found && Ka_found && !Kb_found) {
			fprintf(stderr,"Invalid combination of K-lines detected for element %s\n",elements[i]);
			return rv;
		}
		else if (!K_found && !Ka_found && Kb_found) {
			fprintf(stderr,"Invalid combination of K-lines detected for element %s\n",elements[i]);
			return rv;
		}
		else if (K_found)
			use_K=TRUE;
		else
			use_K=FALSE;
		//L-line
		if ( (L_found && (L1_found || L2_found || L3_found))) {
			fprintf(stderr,"Invalid combination of L-lines detected for element %s\n",elements[i]);
			return rv;
		}
		else if (!L_found && !L1_found && !L2_found && L3_found) {
			fprintf(stderr,"Invalid combination of L-lines detected for element %s\n",elements[i]);
			return rv;
		}
		else if (!L_found && !L1_found && L2_found && !L3_found) {
			fprintf(stderr,"Invalid combination of L-lines detected for element %s\n",elements[i]);
			return rv;
		}
		else if (!L_found && !L1_found && L2_found && L3_found) {
			fprintf(stderr,"Invalid combination of L-lines detected for element %s\n",elements[i]);
			return rv;
		}
		else if (!L_found && L1_found && !L2_found && L3_found) {
			fprintf(stderr,"Invalid combination of L-lines detected for element %s\n",elements[i]);
			return rv;
		}
		else if (!L_found && L1_found && L2_found && !L3_found) {
			fprintf(stderr,"Invalid combination of L-lines detected for element %s\n",elements[i]);
			return rv;
		}
		else if (!L_found && L1_found && !L2_found && !L3_found) {
			fprintf(stderr,"Invalid combination of L-lines detected for element %s\n",elements[i]);
			return rv;
		}
		else if (L_found)
			use_L=TRUE;
		else
			use_L=FALSE;


		(*pymca_input)->k_alpha[i] = 0.0;
		(*pymca_input)->l_alpha[i] = 0.0;

		//check all keys
		sprintf(buffer,"result.%s %s", elements[i],use_K ? "K.KL3" : "Ka.KL3a");
		(*pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KL3esc", elements[i],use_K ? "K.KL3" : "Ka.KL3a");
		(*pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KM3esc", elements[i],use_K ? "K.KL3" : "Ka.KL3a");
		(*pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s", elements[i],use_K ? "K.KL2" : "Ka.KL2a");
		(*pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KL3esc", elements[i],use_K ? "K.KL2" : "Ka.KL2a");
		(*pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KM3esc", elements[i],use_K ? "K.KL2" : "Ka.KL2a");
		(*pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s", elements[i],use_L ? "L.L3M5*" : "L3.L3M5");
		(*pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KL3esc", elements[i],use_L ? "L.L3M5*" : "L3.L3M5");
		(*pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KM3esc", elements[i],use_L ? "L.L3M5*" : "L3.L3M5");
		(*pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s", elements[i],use_L ? "L.L3M4*" : "L3.L3M4");
		(*pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KL3esc", elements[i],use_L ? "L.L3M4*" : "L3.L3M4");
		(*pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KM3esc", elements[i],use_L ? "L.L3M4*" : "L3.L3M4");
		(*pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);


#if DEBUG == 1
		fprintf(stdout,"k_alpha: %lf\n",(*pymca_input)->k_alpha[i]);
		fprintf(stdout,"l_alpha: %lf\n",(*pymca_input)->l_alpha[i]);
#endif

	}

	g_strfreev(elements);

	rv = 1;
	return rv;
}

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
	

	//read multilayer
	
	//get_peak_areas
	if (get_peak_areas(pymcaFile, pymca_input) == 0)
		return rv;



	rv = 1;

	return rv;
}
