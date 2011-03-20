#include "xmi_pymca.h"
#include "xmi_aux.h"
#include <glib.h>
#include <stdio.h>
#include <xraylib-parser.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>



int read_scatter_intensity(GKeyFile *pymcaFile, struct xmi_pymca *pymca_input) {

	int rv = 0;
	gchar **strings, **peaks, **escapepeaks;
	gsize length, npeaks, nescapepeaks;
	int i,j;
	double scatter_energy = 0.0;
	double scatter_intensity = 0.0;
	gchar buffer[128];


	pymca_input->scatter_energy = 0.0;
	pymca_input->scatter_intensity = 0.0;
	
	//check parameters
	strings = g_key_file_get_string_list(pymcaFile, "result","parameters",&length, NULL);

	if (strings == NULL) {
		/*
		*/
		fprintf(stderr,"Could not find parameters tag in pymca fit file\nAborting\n");
		rv = 0;
		return rv;
	}

	for (i = 0 ; i < length ; i++) {
		//remove leading spaces
		g_strchug(strings[i]);
		if (strncmp(strings[i],"Scatter Peak",12) != 0) 
			continue;

		//scatter peak found
		sprintf(buffer,"result.%s",strings[i]);
		peaks = g_key_file_get_string_list(pymcaFile, buffer,"peaks",&npeaks,NULL);
		if (peaks == NULL) {
			fprintf(stderr,"Scatter Peak found but no peaks were defined\nAborting\n");
			rv = 0;
			return rv;
		}
		scatter_energy = 0.0;
		scatter_intensity = 0.0;

		//there should be only one peak... if there are more, ignore the rest
		g_strchug(peaks[0]);
		sprintf(buffer,"result.%s.%s",strings[i],peaks[0]);
		scatter_energy = g_key_file_get_double(pymcaFile, buffer, "energy",NULL);
		scatter_intensity += g_key_file_get_double(pymcaFile, buffer, "fitarea",NULL);

		g_strfreev(peaks);

		//escape peaks should be added
		sprintf(buffer,"result.%s",strings[i]);
		escapepeaks = g_key_file_get_string_list(pymcaFile, buffer,"escapepeaks",&nescapepeaks,NULL);
		if (escapepeaks != NULL) {
			for (j = 0 ; j < nescapepeaks ; j++) {
				g_strchug(escapepeaks[j]);
				sprintf(buffer,"result.%s.%sesc",strings[i],escapepeaks[j]);
#if DEBUG == 2
				fprintf(stdout,"buffer: %s\n",buffer);
#endif
				scatter_intensity += g_key_file_get_double(pymcaFile, buffer, "fitarea",NULL);

			}
			g_strfreev(escapepeaks);
		}

		if (scatter_intensity > pymca_input->scatter_intensity) {
			pymca_input->scatter_energy = scatter_energy;
			pymca_input->scatter_intensity = scatter_intensity;
		}

	}	

#if DEBUG == 1
	fprintf(stdout,"scatter_energy: %lf\n",pymca_input->scatter_energy);
	fprintf(stdout,"scatter_intensity: %lf\n",pymca_input->scatter_intensity);
#endif

	g_strfreev(strings);

	rv = 1;

	return rv;
}



int get_composition(GKeyFile *pymcaFile, char *compositionString, struct xmi_layer **layer, int alloc) {
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
	double *weight;
	int *sorted_Z_ind;
	int *Z;


	//remove any leading spaces
	g_strchug(compositionString);

#if DEBUG == 2
	fprintf(stdout,"composition without leading space: x%sx\n",compositionString);
#endif


	predefGroup = (gchar *) malloc(sizeof(gchar)*(strlen("result.config.materials.")+strlen(compositionString)+1));
	strcpy(predefGroup,"result.config.materials.");
	strcat(predefGroup,compositionString);

#if DEBUG == 2
	fprintf(stdout,"predefGroup: %s\n",predefGroup);
#endif


	//First check if it's not one of the predefined layers...
	if (g_key_file_has_group(pymcaFile, predefGroup) == TRUE) {
#if DEBUG == 2
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
			if (alloc == TRUE) {
				*layer = compoundData2xmi_layer (cd1);
			}
			else if (alloc == FALSE) {
				//this creates a memory leak... but one we can live with...
				xmi_copy_layer2(compoundData2xmi_layer (cd1), *layer);
			}
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
			if (alloc == TRUE) {
				*layer = compoundData2xmi_layer (cd_sum);
			}
			else if (alloc == FALSE) {
				//this creates a memory leak... but one we can live with...
				xmi_copy_layer2(compoundData2xmi_layer (cd_sum), *layer);
			}
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
		if (alloc == TRUE) {
			*layer = compoundData2xmi_layer (cd1);
		}
		else if (alloc == FALSE) {
			//this creates a memory leak... but one we can live with...
			xmi_copy_layer2(compoundData2xmi_layer (cd1), *layer);
		}
		FREE_COMPOUND_DATA(*cd1);
		free(cd1);
	}

#if DEBUG == 2
	for (i = 0 ; i < (*layer)->n_elements ; i++) {
		fprintf(stdout,"Element: %i\n",(*layer)->Z[i]);
		fprintf(stdout,"Weight: %lf\n",(*layer)->weight[i]);
	}
#endif

	//sort
	Z = (int *) xmi_memdup((*layer)->Z, sizeof(int)*(*layer)->n_elements);
	weight = (double *) xmi_memdup((*layer)->weight, sizeof(double)*(*layer)->n_elements);
	sorted_Z_ind = xmi_sort_idl_int(Z,(*layer)->n_elements);

	for (i = 0 ; i < (*layer)->n_elements ; i++) {
		(*layer)->Z[i] = Z[sorted_Z_ind[i]];
		(*layer)->weight[i] = weight[sorted_Z_ind[i]];
	}


	free(Z);
	free(weight);
	free(sorted_Z_ind);


	rv = 1;
	return rv;
}


int read_detector_params(GKeyFile *pymcaFile, struct xmi_detector **detector) {
	int rv;
	gchar *type;

	rv = 0;

	*detector = (struct xmi_detector *) malloc(sizeof(struct xmi_detector));

	(*detector)->gain = g_key_file_get_double(pymcaFile, "result.config.detector","gain", NULL);
	(*detector)->zero = g_key_file_get_double(pymcaFile, "result.config.detector","zero", NULL);
	(*detector)->fano= g_key_file_get_double(pymcaFile, "result.config.detector","fano", NULL);
	(*detector)->noise= g_key_file_get_double(pymcaFile, "result.config.detector","noise", NULL);

	type = g_key_file_get_string(pymcaFile, "result.config.detector", "detele", NULL);

	if (strcmp("Si",type) == 0) {
		(*detector)->detector_type = XMI_DETECTOR_SILI;
	}
	else if (strcmp("Ge",type) == 0) {
		(*detector)->detector_type = XMI_DETECTOR_GE;
	}
	else {
		fprintf(stderr,"Unsupported detector element detected. Choose either Si or Ge... Fatal error\n");
		return rv;
	}

	//this should be calculated based on the maximum energy...
	(*detector)->max_convolution_energy = 100.0;



	rv = 1;
	return rv;
}


enum {
	ABSORBER_BEAM,
	ABSORBER_DETECTOR,
	ABSORBER_CRYSTAL
};


int read_absorbers (GKeyFile *pymcaFile, struct xmi_layer **layers, int *n_layers, int kind) {
	char *exc_names[]={"BeamFilter0","BeamFilter1", NULL};
	char *det_names[]={"deadlayer","absorber","window","contact","Filter 6",
	                 "Filter 7", NULL};
	char *crystal_names[]={"Detector", NULL};

	char **names;

	char **strings = NULL;
	gsize length = 0;
	gint active;
	int i;
	struct xmi_layer *temp;

	int rv = 0;

	if (kind == ABSORBER_BEAM)
		names = exc_names;
	else if (kind == ABSORBER_DETECTOR)
		names = det_names;
	else if (kind == ABSORBER_CRYSTAL)
		names = crystal_names;
	else {
		fprintf(stderr,"Invalid kind detected in read_absorbers... Fatal error\n");
		return rv;
	}


	
	for (i = 0 ; names[i] !=NULL ; i++) {
#if DEBUG == 2
		fprintf(stdout,"names: %s\n",names[i]);
#endif
		if ((strings = g_key_file_get_string_list(pymcaFile, "result.config.attenuators", names[i], &length, NULL)) == NULL) {
			fprintf(stderr,"Could not find reference for %s... Looks suspicious\n",names[i]);
			continue;
		}
		//check activity
		active = atoi(strings[0]);
		if (active != 1) {
			continue;
		}
		//ok... allocate memory
		*(layers) = (struct xmi_layer *) realloc(*(layers), sizeof(struct xmi_layer)*++(*n_layers));
		temp = *layers+*n_layers-1;
		if (get_composition(pymcaFile, strings[1], &temp, FALSE) == 0)
			return rv;
		//density and thickness
		(*layers+*n_layers-1)->density = g_ascii_strtod(strings[2],NULL);
		(*layers+*n_layers-1)->thickness= g_ascii_strtod(strings[3],NULL);
		g_strfreev(strings);

	}



	rv = 1;
	return rv;
}

int read_geometry(GKeyFile *pymcaFile, struct xmi_geometry **geometry) {
	int rv = 0;
	double alpha, beta;
	gchar **strings;
	gsize length;
	double det_dist;

	/*
	 *	Armando uses mm, but we use cm!
	 *
	 */


	//allocate memory
	*geometry = (struct xmi_geometry *) malloc(sizeof(struct xmi_geometry));


	//calculate sample normal using Matrix angles alpha and beta
	strings = g_key_file_get_string_list(pymcaFile, "result.config.attenuators", "Matrix", &length, NULL);
	//no need to do checking here... it has been done before
	alpha = 90.0 - g_ascii_strtod(strings[4], NULL);
	beta = 90.0 - g_ascii_strtod(strings[5], NULL);
  	alpha = alpha/180.*M_PI;
  	beta  = beta/180.*M_PI;

	//detector area	
	(*geometry)->area_detector = g_key_file_get_double(pymcaFile, "result.config.concentrations", "area",NULL)/100.0; 
	if ((*geometry)->area_detector <= 0.0) {
		fprintf(stderr,"Detector area must be positive... Fatal error\n");
		return rv;
	}

	//sample_source_distance
	(*geometry)->d_sample_source = g_key_file_get_double(pymcaFile, "xrfmc.setup", "source_sample_distance",NULL)/10.0; 
	if ((*geometry)->d_sample_source <= 0.0) {
		fprintf(stderr,"Sample-source distance must be positive... Fatal error\n");
		return rv;
	}
	
	//collimator_height
	(*geometry)->collimator_height = g_key_file_get_double(pymcaFile, "xrfmc.setup", "collimator_height",NULL); 
	if ((*geometry)->collimator_height < 0.0) {
		fprintf(stderr,"Collimator height must be greater or equal to zero... Fatal error\n");
		return rv;
	}
	
	//collimator_diameter
	(*geometry)->collimator_diameter = g_key_file_get_double(pymcaFile, "xrfmc.setup", "collimator_diameter",NULL); 
	if ((*geometry)->collimator_diameter < 0.0) {
		fprintf(stderr,"Collimator opening diameter must be greater or equal to zero... Fatal error\n");
		return rv;
	}
	
	//detector-sample distance
	det_dist = g_key_file_get_double(pymcaFile, "result.config.concentrations", "distance",NULL)/10.0; 
	if (det_dist <= 0.0) {
		fprintf(stderr,"Detector-sample distance must be positive... Fatal error\n");
		return rv;
	}



	(*geometry)->p_detector_window[0] = 0.0;
	(*geometry)->p_detector_window[1] = -1.0*det_dist*sin(alpha+beta);
	(*geometry)->p_detector_window[2] = (*geometry)->d_sample_source - det_dist*cos(alpha+beta) ;
	
	(*geometry)->n_detector_orientation[0] = 0.0;
	(*geometry)->n_detector_orientation[1] = sin(alpha+beta);
	(*geometry)->n_detector_orientation[2] = cos(alpha+beta);

	(*geometry)->n_sample_orientation[0] = 0.0;
	(*geometry)->n_sample_orientation[1] = sin(alpha);
	(*geometry)->n_sample_orientation[2] = cos(alpha);

	//simplify slits by using default values!!!
	(*geometry)->d_source_slit = (*geometry)->d_sample_source;
	(*geometry)->slit_size_x = 10E-4;
	(*geometry)->slit_size_y = 10E-4;

	g_strfreev(strings);

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


#if DEBUG == 2
	for (i=0 ; i < length ; i++)
		fprintf(stdout,"x%sx\n",strings[i]);
#endif
	if (get_composition(pymcaFile, strings[1], atmosphere_layer, TRUE) == 0)
		return rv;

	//get density and thickness
	(*atmosphere_layer)->density = g_ascii_strtod(strings[2],NULL);
	(*atmosphere_layer)->thickness= g_ascii_strtod(strings[3],NULL);

	g_strfreev(strings);


	rv = 1;
	return rv;
}

int read_multilayer_composition(GKeyFile *pymcaFile, struct xmi_layer **multilayer_layers, int *n_multilayer_layers, int flags[100], int ilay_pymca) {
	int rv = 0;
	gint active;
	gsize length, length2;
	gchar **strings, **strings2;
	gchar buffer[128];
	int i, j, k;
	struct xmi_layer *temp;

	//see it Matrix is toggled -> absolute requirement!
	if ( (strings = g_key_file_get_string_list(pymcaFile, "result.config.attenuators", "Matrix", &length, NULL)) == NULL) {
		fprintf(stderr,"No Matrix reference found in model... Fatal error\n");
		return rv;
	}
	
	active = atoi(strings[0]);

	if (active != 1) {
		fprintf(stderr,"Matrix layer inactive... Fatal error\n");
		return rv;
	}

	//see if we're dealing with a multilayer here or a single layer Matrix
	if (strcmp(g_strstrip(strings[1]),"MULTILAYER") == 0) {
		//Multilayer found
		for (i = 0 ; i < 10 ; i++) {
			sprintf(buffer,"Layer%i",i);
			if ((strings2 = g_key_file_get_string_list(pymcaFile, "result.config.multilayer", buffer ,&length2, NULL)) == NULL) {
				fprintf(stderr,"Couldn't find key %s in result.config.multilayer... Fatal error",buffer);
				return rv;

			}
			//check if it's active
			active = atoi(strings2[0]);
			if (active != 1) {
				if (i == 0) {
					fprintf(stderr,"Multilayer was selected but the first individual layer is inactive... Fatal error\n");
					return rv;
				}
				break;
			}
			//ok... allocate memory
			*(multilayer_layers) = (struct xmi_layer *) realloc(*(multilayer_layers), sizeof(struct xmi_layer)*++(*n_multilayer_layers));
			temp = *multilayer_layers+i;
			if (get_composition(pymcaFile, strings2[1], &temp, FALSE) == 0)
				return rv;
			//density and thickness
			(*multilayer_layers+i)->density = g_ascii_strtod(strings2[2],NULL);
			(*multilayer_layers+i)->thickness= g_ascii_strtod(strings2[3],NULL);

			g_strfreev(strings2);
		}
	}
	else {
		//single layer
		if (get_composition(pymcaFile, strings[1], multilayer_layers, TRUE) == 0)
			return rv;
	
		*n_multilayer_layers = 1;

		//get density and thickness
		(*multilayer_layers)->density = g_ascii_strtod(strings[2],NULL);
		(*multilayer_layers)->thickness= g_ascii_strtod(strings[3],NULL);
	}

	if (ilay_pymca > *n_multilayer_layers) {
		fprintf(stderr,"Invalid value for xrfmc.setup.layer found\nMust be less or equal than the number of layers in Matrix\n");
		return rv;
	}

	for (i = 0 ; i < (*multilayer_layers+ilay_pymca-1)->n_elements ; i++) {
			flags[(*multilayer_layers+ilay_pymca-1)->Z[i]] = 1;
	}

	g_strfreev(strings);

#if DEBUG == 2
	for (i = 1 ; i < 100 ; i++)
		if (flags[i] == 1) 
			fprintf(stdout,"Element flagged: %s\n",AtomicNumberToSymbol(i));

#endif

	rv = 1;
	return rv;
}

int get_peak_areas(GKeyFile *pymcaFile, struct xmi_pymca *pymca_input) {
	int rv = 0;
	gchar **elements, **lines;
	gsize n_elements, n_lines;
	GError *error;
	int i, Z, j;
	int K_found, Ka_found, Kb_found, L_found, L1_found, L2_found, L3_found;
	int use_K, use_L;
	gchar buffer[128];

	//sort variables
	int *z_arr, *sorted_Z_ind;
	double *k_alpha, *l_alpha;



	//first examine result.config.peaks...
	elements = g_key_file_get_keys(pymcaFile, "result.config.peaks", &n_elements, &error);
	if (elements == NULL || n_elements == 0) {
		fprintf(stderr,"No peaks were configured for the fit: aborting\n");
		return rv;
	}
	
	//malloc memory...
	(pymca_input)->n_peaks = n_elements;
	(pymca_input)->z_arr = (int *) malloc(sizeof(int)*n_elements);
	(pymca_input)->k_alpha = (double *) malloc(sizeof(double)*n_elements);
	(pymca_input)->l_alpha = (double *) malloc(sizeof(double)*n_elements);

	for (i = 0 ; i < n_elements ; i++) {
#if DEBUG == 2
		fprintf(stdout,"Examining peaks of %s\n",elements[i]);
#endif
		Z = SymbolToAtomicNumber(g_strstrip(elements[i]));
		(pymca_input)->z_arr[i] = Z;

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


		(pymca_input)->k_alpha[i] = 0.0;
		(pymca_input)->l_alpha[i] = 0.0;

		//check all keys
		sprintf(buffer,"result.%s %s", elements[i],use_K ? "K.KL3" : "Ka.KL3a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KL3esc", elements[i],use_K ? "K.KL3" : "Ka.KL3a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KM3esc", elements[i],use_K ? "K.KL3" : "Ka.KL3a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s", elements[i],use_K ? "K.KL2" : "Ka.KL2a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KL3esc", elements[i],use_K ? "K.KL2" : "Ka.KL2a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KM3esc", elements[i],use_K ? "K.KL2" : "Ka.KL2a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s", elements[i],use_L ? "L.L3M5*" : "L3.L3M5");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KL3esc", elements[i],use_L ? "L.L3M5*" : "L3.L3M5");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KM3esc", elements[i],use_L ? "L.L3M5*" : "L3.L3M5");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s", elements[i],use_L ? "L.L3M4*" : "L3.L3M4");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KL3esc", elements[i],use_L ? "L.L3M4*" : "L3.L3M4");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);

		sprintf(buffer,"result.%s %s Si_KM3esc", elements[i],use_L ? "L.L3M4*" : "L3.L3M4");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);


#if DEBUG == 2
		fprintf(stdout,"k_alpha: %lf\n",(pymca_input)->k_alpha[i]);
		fprintf(stdout,"l_alpha: %lf\n",(pymca_input)->l_alpha[i]);
#endif

	}

	//sort
	z_arr = (int *) xmi_memdup(pymca_input->z_arr, sizeof(int)*n_elements);
	k_alpha = (double *) xmi_memdup(pymca_input->k_alpha, sizeof(double)*n_elements);
	l_alpha = (double *) xmi_memdup(pymca_input->l_alpha, sizeof(double)*n_elements);
	sorted_Z_ind = xmi_sort_idl_int(z_arr, n_elements);	
	
	for (i = 0 ; i < n_elements ; i++) {
		pymca_input->z_arr[i] = z_arr[sorted_Z_ind[i]];
		pymca_input->k_alpha[i] = k_alpha[sorted_Z_ind[i]];
		pymca_input->l_alpha[i] = l_alpha[sorted_Z_ind[i]];
#if DEBUG == 1
		fprintf(stdout,"pymca_input->z_arr[%i]: %i\n",i,pymca_input->z_arr[i]);
		fprintf(stdout,"pymca_input->k_alpha[%i]: %lf\n",i,pymca_input->k_alpha[i]);
#endif
	}

	free(z_arr);
	free(k_alpha);
	free(l_alpha);
	free(sorted_Z_ind);
	g_strfreev(elements);

	rv = 1;
	return rv;
}

int read_excitation_spectrum(GKeyFile *pymcaFile, struct xmi_excitation **excitation) {
	gchar **energy = NULL;
	gdouble *energyweight = NULL;
	gint *energyflag = NULL;
	gsize n_energy, n_energyweight, n_energyflag;
	int rv = 0;
	gint i;
	double pdeg;
	double flux;
	double livetime;

	//get degree of polarization;
	pdeg = g_key_file_get_double(pymcaFile, "xrfmc.setup", "p_polarisation", NULL);
	flux = g_key_file_get_double(pymcaFile, "result.config.concentrations", "flux", NULL);
	livetime = g_key_file_get_double(pymcaFile, "result.config.concentrations", "time", NULL);

	if (flux <= 0.0) {
		fprintf(stderr,"Flux must be a positive value... Fatal error\n");
		return rv;
	}
	if (livetime <= 0.0) {
		fprintf(stderr,"Livetime must be a positive value... Fatal error\n");
		return rv;
	}


	energy = g_key_file_get_string_list(pymcaFile, "result.config.fit","energy",&n_energy, NULL);
	energyweight = g_key_file_get_double_list(pymcaFile, "result.config.fit","energyweight", &n_energyweight, NULL);
	energyflag = g_key_file_get_integer_list(pymcaFile, "result.config.fit","energyflag", &n_energyflag, NULL);

#if DEBUG == 2
	fprintf(stdout,"n_energy: %i\n",n_energy);
	fprintf(stdout,"n_energyweight: %i\n",n_energyweight);
	fprintf(stdout,"n_energyflag: %i\n",n_energyflag);
#endif


	if (n_energyflag * n_energyweight * n_energy != n_energyflag * n_energyflag * n_energyflag) {
		fprintf(stderr,"Inconsistency detected in energy arrays length... Fatal error\n");
		return rv;
	}
	
	//look at the flags -> assume that all lines are discrete!!!
	(*excitation) = (struct xmi_excitation *) malloc(sizeof(struct xmi_excitation));
	(*excitation)->n_discrete = 0;
	(*excitation)->discrete = NULL;
	(*excitation)->n_continuous = 0;
	(*excitation)->continuous = NULL;
	
	for (i = 0 ; i < n_energy ; i++) {
		//check flag
		if (energyflag[i] == FALSE)
			continue;

		(*excitation)->discrete = (struct xmi_energy *) realloc((*excitation)->discrete, ++((*excitation)->n_discrete)*sizeof(struct xmi_energy));
		(*excitation)->discrete[((*excitation)->n_discrete)-1].energy = g_ascii_strtod(energy[i],NULL);
		if ((*excitation)->discrete[((*excitation)->n_discrete)-1].energy <= 0.0) {
			fprintf(stderr,"A flagged energy turned out to be negative or zero... Fatal error\n");
			return rv;
		}
		(*excitation)->discrete[((*excitation)->n_discrete)-1].horizontal_intensity = energyweight[i]*flux*livetime*(1.0+pdeg)/2.0;
		(*excitation)->discrete[((*excitation)->n_discrete)-1].vertical_intensity = energyweight[i]*flux*livetime*(1.0-pdeg)/2.0;
		//assume point source
		(*excitation)->discrete[((*excitation)->n_discrete)-1].sigma_x = 0.0;
		(*excitation)->discrete[((*excitation)->n_discrete)-1].sigma_y = 0.0;
		(*excitation)->discrete[((*excitation)->n_discrete)-1].sigma_xp = 0.0;
		(*excitation)->discrete[((*excitation)->n_discrete)-1].sigma_yp = 0.0;
	
	}	



	g_strfreev(energy);
	g_free(energyweight);
	g_free(energyflag);

	rv = 1;
	return rv;
}


int xmi_read_input_pymca(char *pymca_file, struct xmi_input **input, struct xmi_pymca **pymca_input) {
	int rv = 0;
	GKeyFile *pymcaFile;
	GError *error=NULL;
	struct xmi_layer *atmosphere_layer = NULL;
	struct xmi_layer *multilayer_layers = NULL;
	int n_multilayer_layers=0;
	struct xmi_layer *det_layers = NULL;
	int n_det_layers = 0;
	struct xmi_layer *exc_layers = NULL;
	int n_exc_layers = 0;
	struct xmi_layer *crystal_layers = NULL;
	int n_crystal_layers = 0;
	int i,j,k;
	int found;
	struct xmi_geometry *geometry = NULL;
	struct xmi_excitation *excitation = NULL;
	struct xmi_detector *detector = NULL;
	struct xmi_general *general = NULL;
	gchar **strings, *energy_string;

	//read the file...
	pymcaFile = g_key_file_new();
	if (g_key_file_load_from_file(pymcaFile, pymca_file, G_KEY_FILE_NONE, &error) == FALSE) {
		fprintf(stderr,"Could not parse file %s\n",pymca_file);
		fprintf(stderr,"Error message: %s\n",error->message);
		return rv;
	}

	g_key_file_set_list_separator(pymcaFile, ',');

	//allocate input
	*input = (struct xmi_input *) malloc(sizeof(struct xmi_input));
	*pymca_input = (struct xmi_pymca *) malloc(sizeof(struct xmi_pymca));


	//get layer that will be quantified...
	//1 means first layer of MULTILAYER Matrix
	//2 means second layer of MULTILAYER Matrix etc...
	(*pymca_input)->ilay_pymca = g_key_file_get_integer(pymcaFile, "xrfmc.setup","layer", &error); 
	if((*pymca_input)->ilay_pymca < 1) {
		fprintf(stdout,"Found invalid value for xrfmc.setup:layer\nMust be greater or equal to 1\n");
		return rv;
	}

#if DEBUG == 2
	fprintf(stdout,"ilay_pymca: %i\n",(*pymca_input)->ilay_pymca);
#endif


	//read atmosphere composition
	if (read_atmosphere_composition(pymcaFile, &atmosphere_layer) == 0)
		return rv;
	

	//read multilayer
	//set flags to zero
	for (i = 0 ; i < 100 ; i++)
		(*pymca_input)->flags[i] = 0;

	if (read_multilayer_composition(pymcaFile, &multilayer_layers, &n_multilayer_layers, (*pymca_input)->flags, (*pymca_input)->ilay_pymca) == 0)
		return rv;
	
#if DEBUG == 2
	fprintf(stdout,"Before get_peak_areas\n");
#endif

	//get_peak_areas
	if (get_peak_areas(pymcaFile, *pymca_input) == 0)
		return rv;

#if DEBUG == 2
	fprintf(stdout,"After get_peak_areas\n");
#endif
	//determine elements that will actually be quantified (non-matrix)
	//1st condition: there has to be a (positive) net-line intensity available
	//2nd condition: the element may not be part of the matrix composition
	//3rd condition: the element may not be part of any of the other layers
	(*pymca_input)->z_arr_quant = NULL;
	(*pymca_input)->n_z_arr_quant = 0;
	for (i = 0 ; i < (*pymca_input)->n_peaks ; i++) {
#if DEBUG == 2
		fprintf(stderr,"Element %s\n",AtomicNumberToSymbol((*pymca_input)->z_arr[i]));
#endif
		found = 0;
		//check multilayer
		for (j = 0 ; j < n_multilayer_layers ; j++) {
			for (k = 0 ; k < multilayer_layers[j].n_elements ; k++) {
				if (multilayer_layers[j].Z[k] == (*pymca_input)->z_arr[i]) {
					found = 1;
					break;
				}
			}
			if (found == 1)
				break;
		}
		if (found == 0 && atmosphere_layer != NULL) {
			//check atmosphere
			for (k = 0 ; k < atmosphere_layer->n_elements ; k++) {
				if (atmosphere_layer->Z[k] == (*pymca_input)->z_arr[i]) {
					found = 1;
					break;
				}
			}
			
		}
		if (found == 0) {
			//found
			(*pymca_input)->z_arr_quant = (int *) realloc((*pymca_input)->z_arr_quant,sizeof(int)*++((*pymca_input)->n_z_arr_quant) );
			(*pymca_input)->z_arr_quant[((*pymca_input)->n_z_arr_quant)-1] = (*pymca_input)->z_arr[i];
#if DEBUG == 2
			fprintf(stdout,"Element to be quantified: %s\n",AtomicNumberToSymbol((*pymca_input)->z_arr_quant[((*pymca_input)->n_z_arr_quant)-1]));
#endif
		}
	}

	//sort
	qsort((*pymca_input)->z_arr_quant, (*pymca_input)->n_z_arr_quant, sizeof(int),xmi_cmp_int  );



	//absorbers and  crystal
	if (read_absorbers(pymcaFile, &det_layers, &n_det_layers, ABSORBER_DETECTOR) == 0)
		return rv;

	if (read_absorbers(pymcaFile, &exc_layers, &n_exc_layers, ABSORBER_BEAM) == 0)
		return rv;

	if (read_absorbers(pymcaFile, &crystal_layers, &n_crystal_layers, ABSORBER_CRYSTAL) == 0)
		return rv;

	if (n_crystal_layers == 0) {
		fprintf(stderr,"Detector contains no detector crystal. Define a Detector layer in PyMCA\n");
		return rv;
	}


	//geometry
	if (read_geometry(pymcaFile, &geometry) == 0)
		return rv;
	
	//energy
	if (read_excitation_spectrum(pymcaFile, &excitation) == 0)
		return rv;

	//detector parameters
	if (read_detector_params(pymcaFile, &detector) == 0)
		return rv;

	//general
	general = (struct xmi_general *) malloc(sizeof(struct xmi_general));
	general->outputfile = strdup("");
	general->n_photons_interval = 100000;
	general->n_photons_line = 100000;

	general->n_interactions_trajectory = g_key_file_get_integer(pymcaFile, "xrfmc.setup","nmax_interaction",NULL);
	if (general->n_interactions_trajectory < 1) {
		fprintf(stderr,"Maximum number of interactions must be at least one... Fatal error\n");
		return rv;
	}

	//put it all together
	(*input)->general = general;
	//allocate composition
	(*input)->composition = (struct xmi_composition *) malloc(sizeof(struct xmi_composition));
	//if an atmosphere layer is present... some advanced tricks are required...
	if (atmosphere_layer != NULL) {
		//atmosphere is present
		(*input)->composition->n_layers = n_multilayer_layers + 2;
		(*input)->composition->layers = (struct xmi_layer *) realloc(multilayer_layers, (*input)->composition->n_layers*sizeof(struct xmi_layer));
		(*input)->composition->layers[(*input)->composition->n_layers-1] = *atmosphere_layer; 	
		for (i =(*input)->composition->n_layers-2 ; i >= 1 ; i--) {
			(*input)->composition->layers[i] = (*input)->composition->layers[i-1];
		}
		(*input)->composition->layers[0] = *atmosphere_layer; 	
		(*input)->composition->reference_layer = 2;	
	}
	else {
		//no atmosphere
		(*input)->composition->n_layers = n_multilayer_layers;
		(*input)->composition->layers = multilayer_layers;
		(*input)->composition->reference_layer = 1;	
	}

	(*input)->geometry = geometry;
	(*input)->excitation = excitation;
	(*input)->absorbers = (struct xmi_absorbers *) malloc(sizeof(struct xmi_absorbers));
	(*input)->absorbers->n_exc_layers = n_exc_layers;
	(*input)->absorbers->exc_layers = exc_layers;
	(*input)->absorbers->n_det_layers = n_det_layers;
	(*input)->absorbers->det_layers = det_layers;
	(*input)->detector = detector;
	detector->n_crystal_layers = n_crystal_layers;
	detector->crystal_layers = crystal_layers;

	//adjust ilay_pymca if necessary
	if (atmosphere_layer == NULL)
		(*pymca_input)->ilay_pymca = 0;			

#if DEBUG == 2
	fprintf(stdout,"ilay_pymca: %i\n",(*pymca_input)->ilay_pymca);
#endif
	//nchannels
	energy_string = g_key_file_get_string(pymcaFile, "result", "energy", NULL);

	strings = g_strsplit(energy_string," ",100000);
	(*pymca_input)->nchannels = (int) g_strv_length(strings)-2;

#if DEBUG == 2
	fprintf(stdout,"nchannels: %i\n",(*pymca_input)->nchannels);
	fprintf(stdout,"channel[0]: %s\n",strings[0]);
	fprintf(stdout,"channel[last]: %s\n",strings[(*pymca_input)->nchannels-1]);
#endif
	g_strfreev(strings);
	g_free(energy_string);

	//see if we find a nice scatter peak, which can be used for adjusting the beam intensity afterwards
	if (read_scatter_intensity(pymcaFile, *pymca_input) == 0) {
		rv = 0;
		return rv;
	}



	rv = 1;

	return rv;
}

struct xmi_layer xmi_ilay_composition_pymca(struct xmi_layer *matrix, struct xmi_pymca *pymca_aux , double *weights_arr_quant) {
	
	struct xmi_layer rv;
	double sum_quant, sum_matrix;
	int i;
	double *weight;
	int *sorted_Z_ind;
	int *Z;

	rv.Z = (int *) malloc(sizeof(int)*(matrix->n_elements+pymca_aux->n_z_arr_quant));
	rv.weight = (double *) malloc(sizeof(double)*(matrix->n_elements+pymca_aux->n_z_arr_quant));
	rv.density = matrix->density;
	rv.thickness = matrix->thickness;


	//calculate sum 
	sum_quant = xmi_sum_double(weights_arr_quant, pymca_aux->n_z_arr_quant);
	sum_matrix = xmi_sum_double(matrix->weight, matrix->n_elements);
	xmi_scale_double(matrix->weight, matrix->n_elements, 1.0/sum_matrix);


	for (i = 0 ; i < matrix->n_elements ; i++) {
		rv.Z[i] = matrix->Z[i];
		rv.weight[i] = matrix->weight[i]*(1.0-sum_quant);
	}

	for (i = 0 ; i < pymca_aux->n_z_arr_quant ; i++) {
		rv.Z[i+matrix->n_elements] = pymca_aux->z_arr_quant[i];
		rv.weight[i+matrix->n_elements] = weights_arr_quant[i];
	}



	rv.n_elements = matrix->n_elements+pymca_aux->n_z_arr_quant;

	//sort
	Z = (int *) xmi_memdup(rv.Z, sizeof(int)*rv.n_elements);
	weight = (double *) xmi_memdup(rv.weight, sizeof(double)*rv.n_elements);
	sorted_Z_ind = xmi_sort_idl_int(Z,rv.n_elements);

	for (i = 0 ; i < rv.n_elements ; i++) {
#if DEBUG == 2
		fprintf(stdout,"indices sorted: %i\n",sorted_Z_ind[i]);
		fprintf(stdout,"Z sorted: %i\n",Z[sorted_Z_ind[i]]);
#endif
		rv.Z[i] = Z[sorted_Z_ind[i]];
		rv.weight[i] = weight[sorted_Z_ind[i]];
	}


	free(Z);
	free(weight);
	free(sorted_Z_ind);
	
	
	rv.density = matrix->density;
	rv.thickness= matrix->thickness;



	return rv;
}
