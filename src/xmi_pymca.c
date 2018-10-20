/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

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
#include "xmi_pymca.h"
#include "xmi_aux.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <stdio.h>
#include <xraylib.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

static int read_pymca_concentrations(GKeyFile *pymcaFile, xmi_pymca *pymca_input) {

	//this function will read the concentrations calculated by PyMCA, of course only if they were calculated in the first place
	int rv = 0;
	gchar **elements;
	gsize nelements;
	gint i;
	gchar *temp_string;
	gdouble temp_double;



	pymca_input->z_arr_pymca_conc = NULL;
	pymca_input->weight_arr_pymca_conc = NULL;
	pymca_input->n_z_arr_pymca_conc = 0;

	if (g_key_file_has_group(pymcaFile, "concentrations") == FALSE) {
		//concentrations not found
		g_fprintf(stdout,"No concentrations were calculated by PyMca... Using default values\n");
		rv = 1;
		return rv;
	}

	//check for all elements in "elements"
	elements = g_key_file_get_string_list(pymcaFile,"concentrations", "elements", &nelements,NULL);

	if (elements == NULL) {
		g_fprintf(stderr,"No elements key found in concentrations\nAborting\n");
		return rv;
	}

	for (i = 0 ; i < nelements ; i++) {
		//check for K, Ka, L and L3
		g_strchug(elements[i]);

		temp_string = g_strdup_printf("%s K",elements[i]);
		temp_double = g_key_file_get_double(pymcaFile, "concentrations.mass fraction",temp_string, NULL);
		g_free(temp_string);
		if (temp_double > 0.0) {
			pymca_input->n_z_arr_pymca_conc++;
			pymca_input->z_arr_pymca_conc = (int *) g_realloc(pymca_input->z_arr_pymca_conc,sizeof(int)*pymca_input->n_z_arr_pymca_conc);
			pymca_input->weight_arr_pymca_conc = (double *) g_realloc(pymca_input->weight_arr_pymca_conc,sizeof(double)*pymca_input->n_z_arr_pymca_conc);
			pymca_input->z_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1] = SymbolToAtomicNumber(elements[i]);
			pymca_input->weight_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1] = temp_double;
#if DEBUG == 1
			fprintf(stdout,"pymca conc: %i: %lf\n",pymca_input->z_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1],pymca_input->weight_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1]);
#endif
			continue;
		}

		temp_string = g_strdup_printf(temp_string,"%s Ka",elements[i]);
		temp_double = g_key_file_get_double(pymcaFile, "concentrations.mass fraction",temp_string, NULL);
		g_free(temp_string);
		if (temp_double > 0.0) {
			pymca_input->n_z_arr_pymca_conc++;
			pymca_input->z_arr_pymca_conc = (int *) g_realloc(pymca_input->z_arr_pymca_conc,sizeof(int)*pymca_input->n_z_arr_pymca_conc);
			pymca_input->weight_arr_pymca_conc = (double *) g_realloc(pymca_input->weight_arr_pymca_conc,sizeof(double)*pymca_input->n_z_arr_pymca_conc);
			pymca_input->z_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1] = SymbolToAtomicNumber(elements[i]);
			pymca_input->weight_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1] = temp_double;
#if DEBUG == 1
			fprintf(stdout,"pymca conc: %i: %lf\n",pymca_input->z_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1],pymca_input->weight_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1]);
#endif
			continue;
		}

		temp_string = g_strdup_printf("%s L",elements[i]);
		temp_double = g_key_file_get_double(pymcaFile, "concentrations.mass fraction",temp_string, NULL);
		g_free(temp_string);
		if (temp_double > 0.0) {
			pymca_input->n_z_arr_pymca_conc++;
			pymca_input->z_arr_pymca_conc = (int *) g_realloc(pymca_input->z_arr_pymca_conc,sizeof(int)*pymca_input->n_z_arr_pymca_conc);
			pymca_input->weight_arr_pymca_conc = (double *) g_realloc(pymca_input->weight_arr_pymca_conc,sizeof(double)*pymca_input->n_z_arr_pymca_conc);
			pymca_input->z_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1] = SymbolToAtomicNumber(elements[i]);
			pymca_input->weight_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1] = temp_double;
#if DEBUG == 1
			fprintf(stdout,"pymca conc: %i: %lf\n",pymca_input->z_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1],pymca_input->weight_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1]);
#endif
			continue;
		}

		temp_string = g_strdup_printf("%s La",elements[i]);
		temp_double = g_key_file_get_double(pymcaFile, "concentrations.mass fraction",temp_string, NULL);
		g_free(temp_string);
		if (temp_double > 0.0) {
			pymca_input->n_z_arr_pymca_conc++;
			pymca_input->z_arr_pymca_conc = (int *) g_realloc(pymca_input->z_arr_pymca_conc,sizeof(int)*pymca_input->n_z_arr_pymca_conc);
			pymca_input->weight_arr_pymca_conc = (double *) g_realloc(pymca_input->weight_arr_pymca_conc,sizeof(double)*pymca_input->n_z_arr_pymca_conc);
			pymca_input->z_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1] = SymbolToAtomicNumber(elements[i]);
			pymca_input->weight_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1] = temp_double;
#if DEBUG == 1
			fprintf(stdout,"pymca conc: %i: %lf\n",pymca_input->z_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1],pymca_input->weight_arr_pymca_conc[pymca_input->n_z_arr_pymca_conc-1]);
#endif
			continue;
		}

	}

	g_strfreev(elements);

	rv = 1;
	return rv;

}



static int read_scatter_intensity(GKeyFile *pymcaFile, xmi_pymca *pymca_input) {

	int rv = 0;
	gchar **strings, **peaks, **escapepeaks;
	gsize length, npeaks, nescapepeaks;
	int i,j;
	double scatter_energy = 0.0;
	double scatter_intensity = 0.0;
	gchar *buffer;


	pymca_input->scatter_energy = 0.0;
	pymca_input->scatter_intensity = 0.0;

	//check parameters
	strings = g_key_file_get_string_list(pymcaFile, "result","parameters",&length, NULL);

	if (strings == NULL) {
		/*
		*/
		g_fprintf(stderr,"Could not find parameters tag in pymca fit file\nAborting\n");
		rv = 0;
		return rv;
	}

	for (i = 0 ; i < length ; i++) {
		//remove leading spaces
		g_strchug(strings[i]);
		if (strncmp(strings[i],"Scatter Peak",12) != 0)
			continue;

		//scatter peak found
		buffer = g_strdup_printf("result.%s",strings[i]);
		peaks = g_key_file_get_string_list(pymcaFile, buffer,"peaks",&npeaks,NULL);
		g_free(buffer);
		if (peaks == NULL) {
			g_fprintf(stderr,"Scatter Peak found but no peaks were defined\nAborting\n");
			rv = 0;
			return rv;
		}
		scatter_energy = 0.0;
		scatter_intensity = 0.0;

		//there should be only one peak... if there are more, ignore the rest
		g_strchug(peaks[0]);
		buffer = g_strdup_printf("result.%s.%s",strings[i],peaks[0]);
		scatter_energy = g_key_file_get_double(pymcaFile, buffer, "energy",NULL);
		scatter_intensity += g_key_file_get_double(pymcaFile, buffer, "fitarea",NULL);
		g_free(buffer);

		g_strfreev(peaks);

		//escape peaks should be added
		buffer = g_strdup_printf("result.%s",strings[i]);
		escapepeaks = g_key_file_get_string_list(pymcaFile, buffer,"escapepeaks",&nescapepeaks,NULL);
		g_free(buffer);
		if (escapepeaks != NULL) {
			for (j = 0 ; j < nescapepeaks ; j++) {
				g_strchug(escapepeaks[j]);
				buffer = g_strdup_printf("result.%s.%sesc",strings[i],escapepeaks[j]);
#if DEBUG == 2
				fprintf(stdout,"buffer: %s\n",buffer);
#endif
				scatter_intensity += g_key_file_get_double(pymcaFile, buffer, "fitarea",NULL);
				g_free(buffer);
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



static int get_composition(GKeyFile *pymcaFile, char *compositionString, xmi_layer **layer, int alloc) {
	int rv = 0;
	gchar *predefGroup;
	gchar **compoundlist=NULL;
	gsize lengthfractions, lengthlist;
	GError *error = NULL;
	gdouble *compoundfractions;
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



	predefGroup = g_strdup_printf("result.config.materials.%s",compositionString);
	if (g_key_file_has_group(pymcaFile, predefGroup) == FALSE) {
		g_free(predefGroup);
		predefGroup = g_strdup_printf("materials.%s",compositionString);
		if (g_key_file_has_group(pymcaFile, predefGroup) == FALSE) {
			g_free(predefGroup);
			predefGroup = NULL;
		}
	}

#if DEBUG == 2
	fprintf(stdout,"predefGroup: %s\n",predefGroup);
#endif


	//First check if it's not one of the predefined layers...
	if (predefGroup != NULL) {
#if DEBUG == 2
		fprintf(stdout,"Found predefGroup\n");
#endif
		//don't get density and thickness here! Just the composition...
		//CompoundFraction -> doubles
		compoundfractions = g_key_file_get_double_list(pymcaFile, predefGroup, "CompoundFraction", &lengthfractions, &error);
		if (compoundfractions == NULL) {
			g_fprintf(stderr,"Error parsing compoundfractions\n");
			g_fprintf(stderr,"GLib error message: %s\n",error->message);
			return rv;
		}
		//CompoundList -> strings
		compoundlist = g_key_file_get_string_list(pymcaFile, predefGroup, "CompoundList", &lengthlist, &error);
		if (compoundlist == NULL) {
			g_fprintf(stderr,"Error parsing compoundlist\n");
			g_fprintf(stderr,"GLib error message: %s\n",error->message);
			return rv;
		}
		if (lengthlist != lengthfractions) {
			g_fprintf(stderr,"CompoundList and CompoundFractions have different lengths\n");
			return rv;
		}

		if (lengthlist == 1) {
			//only one compound
			if ((cd1 = CompoundParser(g_strchug(compoundlist[0]))) == NULL) {
				fprintf(stderr,"Could not parse compound %s\n",compoundlist[0]);
				return rv;
			}
			if (alloc == TRUE) {
				*layer = compoundData2xmi_layer (cd1);
			}
			else if (alloc == FALSE) {
				//this creates a memory leak... but one we can live with...
				xmi_layer_copy2(compoundData2xmi_layer (cd1), *layer);
			}
			FreeCompoundData(cd1);
		}
		else {
			//more than one compound...
			//start with the first compound
			if ((cd1 = CompoundParser(g_strchug(compoundlist[0]))) == NULL) {
				fprintf(stderr,"Could not parse compound %s\n",compoundlist[0]);
				return rv;
			}
			for (i = 1 ; i < lengthlist ; i++) {
				if ((cd2 = CompoundParser(g_strchug(compoundlist[i]))) == NULL) {
					fprintf(stderr,"Could not parse compound %s\n",compoundlist[i]);
					return rv;
				}
				//sum up cd1 and cd2
				cd_sum = add_compound_data(*cd1,i==1 ? compoundfractions[0] : 1.0,*cd2, compoundfractions[i]);
				FreeCompoundData(cd1);
				cd1 = cd_sum;
				FreeCompoundData(cd2);
			}
			if (alloc == TRUE) {
				*layer = compoundData2xmi_layer (cd_sum);
			}
			else if (alloc == FALSE) {
				//this creates a memory leak... but one we can live with...
				xmi_layer_copy2(compoundData2xmi_layer (cd_sum), *layer);
			}
			FreeCompoundData(cd_sum);
		}
		g_strfreev(compoundlist);
		g_free(compoundfractions);

	}
	else {
		//not predefined... feed it directly to xraylib
		if ((cd1 = CompoundParser(compositionString)) == NULL) {
			fprintf(stderr,"Could not parse compound %s\n",compositionString);
			return rv;
		}
		if (alloc == TRUE) {
			*layer = compoundData2xmi_layer (cd1);
		}
		else if (alloc == FALSE) {
			//this creates a memory leak... but one we can live with...
			xmi_layer_copy2(compoundData2xmi_layer (cd1), *layer);
		}
		FreeCompoundData(cd1);
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


	g_free(Z);
	g_free(weight);
	g_free(sorted_Z_ind);

	if (predefGroup != NULL)
		g_free(predefGroup);

	rv = 1;
	return rv;
}

static int read_detector_params(GKeyFile *pymcaFile, xmi_detector **detector) {
	int rv;
	gchar *type;
	gchar **params;
	gsize nparams;
	GError *error = NULL;

	rv = 0;

	*detector = (xmi_detector *) g_malloc(sizeof(xmi_detector));

	//get parameters from result, if available
	params = g_key_file_get_string_list(pymcaFile, "result", "fittedpar", &nparams, NULL);

	if (params != NULL && nparams > 4) {
		(*detector)->gain = g_ascii_strtod(params[1], NULL);
		(*detector)->zero = g_ascii_strtod(params[0], NULL);
		(*detector)->fano= g_ascii_strtod(params[3], NULL);
		(*detector)->noise = g_ascii_strtod(params[2], NULL);
		g_strfreev(params);
	}
	else {
		(*detector)->gain = g_key_file_get_double(pymcaFile, "result.config.detector","gain", &error);
		if (error != NULL) {
			g_clear_error(&error);
			(*detector)->gain = g_key_file_get_double(pymcaFile, "detector","gain", &error);
			if (error != NULL) {
				g_fprintf(stderr,"Could not find gain key in inputfile... Aborting\n");
				return rv;
			}
		}
		(*detector)->zero = g_key_file_get_double(pymcaFile, "result.config.detector","zero", &error);
		if (error != NULL) {
			g_clear_error(&error);
			(*detector)->zero = g_key_file_get_double(pymcaFile, "detector","zero", &error);
			if (error != NULL) {
				g_fprintf(stderr,"Could not find zero key in inputfile... Aborting\n");
				return rv;
			}
		}
		(*detector)->fano= g_key_file_get_double(pymcaFile, "result.config.detector","fano", &error);
		if (error != NULL) {
			g_clear_error(&error);
			(*detector)->fano = g_key_file_get_double(pymcaFile, "detector","fano", &error);
			if (error != NULL) {
				g_fprintf(stderr,"Could not find fano key in inputfile... Aborting\n");
				return rv;
			}
		}
		(*detector)->noise= g_key_file_get_double(pymcaFile, "result.config.detector","noise", &error);
		if (error != NULL) {
			g_clear_error(&error);
			(*detector)->noise = g_key_file_get_double(pymcaFile, "detector","noise", &error);
			if (error != NULL) {
				g_fprintf(stderr,"Could not find noise key in inputfile... Aborting\n");
				return rv;
			}
		}
	}
	(*detector)->pulse_width= g_key_file_get_double(pymcaFile, "xrfmc.setup","pulse_width", NULL);
	type = g_key_file_get_string(pymcaFile, "result.config.detector", "detele", &error);
	if (error != NULL) {
		g_clear_error(&error);
		type = g_key_file_get_string(pymcaFile, "detector", "detele", &error);
		if (error != NULL) {
			g_fprintf(stderr,"Could not find detele key in inputfile... Aborting\n");
			return rv;
		}
	}


	if (g_strcmp0("Si",type) == 0) {
		(*detector)->detector_type = XMI_DETECTOR_SILI;
	}
	else if (g_strcmp0("Ge",type) == 0) {
		(*detector)->detector_type = XMI_DETECTOR_GE;
	}
	else {
		fprintf(stderr,"Unsupported detector element detected. Choose either Si or Ge... Fatal error\n");
		return rv;
	}


	rv = 1;
	return rv;
}

enum {
	ABSORBER_BEAM,
	ABSORBER_DETECTOR,
	ABSORBER_CRYSTAL
};

static int read_absorbers (GKeyFile *pymcaFile, xmi_layer **layers, int *n_layers, int kind) {
	gchar *exc_names[]={"BeamFilter0","BeamFilter1", NULL};
	gchar **det_names;
	gchar *crystal_names[]={"Detector", NULL};

	gchar **names;

	char **strings = NULL;
	gsize length = 0;
	gint active;
	int i;
	xmi_layer *temp;

	int rv = 0;
	GError *error = NULL;

	if (kind == ABSORBER_BEAM)
		names = exc_names;
	else if (kind == ABSORBER_DETECTOR) {
		det_names = g_key_file_get_keys(pymcaFile,"result.config.attenuators", NULL, &error );
		if (error != NULL) {
			g_clear_error(&error);
			det_names = g_key_file_get_keys(pymcaFile,"attenuators", NULL, &error );
			if (error != NULL) {
				g_fprintf(stderr, "Could not find attenuators in inputfile... Aborting\n");
				return rv;
			}
		}
		length = 0;
		names = NULL;
		i = 0;
		while (det_names[i] != NULL) {
			if (g_strcmp0(det_names[i], "Matrix") == 0 ||
				g_strcmp0(det_names[i], "Detector") == 0 ||
				g_strcmp0(det_names[i], "BeamFilter0") == 0 ||
				g_strcmp0(det_names[i], "BeamFilter1") == 0
			) {
				//do nothing
			}
			else {
				names = (gchar**) g_realloc(names, sizeof(gchar *)*(++length+1));
				names[length-1] = g_strdup(det_names[i]);
				names[length] = NULL;
			}
			i++;
		}

		g_strfreev(det_names);
	}
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
		if ((strings = g_key_file_get_string_list(pymcaFile, "result.config.attenuators", names[i], &length, NULL)) == NULL && (strings = g_key_file_get_string_list(pymcaFile, "attenuators", names[i], &length, NULL)) == NULL) {

			g_fprintf(stderr,"Could not find reference for %s... Looks suspicious\n",names[i]);
			continue;
		}
		//check activity
		active = atoi(strings[0]);
		if (active != 1) {
			continue;
		}
		//ok... allocate memory
		*(layers) = (xmi_layer *) g_realloc(*(layers), sizeof(xmi_layer)*++(*n_layers));
		temp = *layers+*n_layers-1;
		if (get_composition(pymcaFile, strings[1], &temp, FALSE) == 0)
			return rv;
		//density and thickness
		(*layers+*n_layers-1)->density = g_ascii_strtod(strings[2],NULL);
		(*layers+*n_layers-1)->thickness= g_ascii_strtod(strings[3],NULL);
		g_strfreev(strings);

	}

	if (kind == ABSORBER_DETECTOR)
		g_strfreev(names);


	rv = 1;
	return rv;
}

static int read_geometry(GKeyFile *pymcaFile, xmi_geometry **geometry) {
	int rv = 0;
	double alpha, beta;
	gchar **strings;
	gsize length;
	double det_dist;
	GError *error = NULL;



	//allocate memory
	*geometry = (xmi_geometry *) g_malloc(sizeof(xmi_geometry));


	//calculate sample normal using Matrix angles alpha and beta
	if ((strings = g_key_file_get_string_list(pymcaFile, "result.config.attenuators", "Matrix", &length, NULL)) == NULL && (strings = g_key_file_get_string_list(pymcaFile, "attenuators", "Matrix", &length, NULL)) == NULL) {
		g_fprintf(stderr,"Could not find reference for %s... Aborting\n", "Matrix");
		return rv;
	}

	//no need to do checking here... it has been done before
	alpha = g_ascii_strtod(strings[4], NULL);
	beta = g_ascii_strtod(strings[5], NULL);
  	alpha = alpha*M_PI/180.0;
  	beta  = beta*M_PI/180.0;

	//detector area
	(*geometry)->area_detector = g_key_file_get_double(pymcaFile, "result.config.concentrations", "area", &error);
	if (error != NULL) {
		g_clear_error(&error);
		(*geometry)->area_detector = g_key_file_get_double(pymcaFile, "concentrations", "area", &error);
		if (error != NULL) {
			g_fprintf(stderr,"Could not find reference for %s... Aborting\n", "area");
			return rv;
		}
	}
	if ((*geometry)->area_detector <= 0.0) {
		fprintf(stderr,"Detector area must be positive... Fatal error\n");
		return rv;
	}

	//sample_source_distance
	(*geometry)->d_sample_source = g_key_file_get_double(pymcaFile, "xrfmc.setup", "source_sample_distance",NULL);
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
	det_dist = g_key_file_get_double(pymcaFile, "result.config.concentrations", "distance", &error);
	if (error != NULL) {
		g_clear_error(&error);
		det_dist = g_key_file_get_double(pymcaFile, "concentrations", "distance", &error);
		if (error != NULL) {
			g_fprintf(stderr,"Could not find reference for %s... Aborting\n", "distance");
			return rv;
		}
	}


	if (det_dist <= 0.0) {
		fprintf(stderr,"Detector-sample distance must be positive... Fatal error\n");
		return rv;
	}
#if DEBUG == 1
	fprintf(stdout,"det_dist: %lf\n",det_dist);
	fprintf(stdout,"alpha: %lf\n",alpha);
	fprintf(stdout,"beta: %lf\n",beta);
#endif

	if (alpha < 0.0 || beta < 0.0) {
		g_fprintf(stderr, "The incoming and outgoing angles must be positive... Fatal error\n");
		return rv;
	}


	(*geometry)->p_detector_window[0] = 0.0;
	(*geometry)->p_detector_window[1] = -1.0*det_dist*sin(alpha+beta);
	(*geometry)->p_detector_window[2] = (*geometry)->d_sample_source + det_dist*cos(alpha+beta) ;

	(*geometry)->n_detector_orientation[0] = 0.0;
	(*geometry)->n_detector_orientation[1] = sin(alpha+beta);
	(*geometry)->n_detector_orientation[2] = -1.0*cos(alpha+beta);

	(*geometry)->n_sample_orientation[0] = 0.0;
	(*geometry)->n_sample_orientation[1] = cos(alpha);
	(*geometry)->n_sample_orientation[2] = sin(alpha);

	//make sure that the Z component is positive!
	//weird things will happen btw if Z is equal to zero...
	if ((*geometry)->n_sample_orientation[2] < 0.0) {
		(*geometry)->n_sample_orientation[0] *= -1.0;
		(*geometry)->n_sample_orientation[1] *= -1.0;
		(*geometry)->n_sample_orientation[2] *= -1.0;
	}

#if DEBUG == 1
	fprintf(stdout,"(*geometry)->n_sample_orientation[1]: %lf\n",(*geometry)->n_sample_orientation[1]);
	fprintf(stdout,"(*geometry)->n_sample_orientation[2]: %lf\n",(*geometry)->n_sample_orientation[2]);
#endif

	//slits
	(*geometry)->d_source_slit = g_key_file_get_double(pymcaFile, "xrfmc.setup","slit_distance", NULL);
	(*geometry)->slit_size_x = g_key_file_get_double(pymcaFile, "xrfmc.setup","slit_width_x", NULL);
	(*geometry)->slit_size_y = g_key_file_get_double(pymcaFile, "xrfmc.setup","slit_width_y", NULL);

	g_strfreev(strings);




	rv = 1;
	return rv;
}

static int read_multilayer_composition(GKeyFile *pymcaFile, xmi_layer **multilayer_layers, int *n_multilayer_layers, int flags[100], int ilay_pymca, int use_single_run, int *reference_layer) {
	int rv = 0;
	gint active;
	gsize length, length2;
	gchar **strings, **strings2;
	gchar *buffer;
	int i;
	xmi_layer *temp;

	//see it Matrix is toggled -> absolute requirement!
	if ((strings = g_key_file_get_string_list(pymcaFile, "result.config.attenuators", "Matrix", &length, NULL)) == NULL && (strings = g_key_file_get_string_list(pymcaFile, "attenuators", "Matrix", &length, NULL)) == NULL) {
		fprintf(stderr,"No Matrix reference found in model... Fatal error\n");
		return rv;
	}

	active = atoi(strings[0]);

	if (active != 1) {
		g_fprintf(stderr,"Matrix layer inactive... Fatal error\n");
		return rv;
	}


	//see if we're dealing with a multilayer here or a single layer Matrix
	if (g_strcmp0(g_strstrip(strings[1]),"MULTILAYER") == 0) {
		//Multilayer found
		//look for the reference layer
		gchar *reference_layer_str = g_key_file_get_string(pymcaFile, "result.config.multilayer", "ReferenceLayer", NULL);
		if (reference_layer_str == NULL) {
			reference_layer_str = g_key_file_get_string(pymcaFile, "multilayer", "ReferenceLayer", NULL);
		}
		gboolean reference_layer_match = FALSE;
		if (reference_layer_str == NULL) {
			g_fprintf(stderr, "Could not find reference for ReferenceLayer... Assuming first layer\n");
			*reference_layer = 1;
			reference_layer_match = TRUE;
		}
		for (i = 0 ; i < 10 ; i++) {
			buffer = g_strdup_printf("Layer%i",i);
			if ((strings2 = g_key_file_get_string_list(pymcaFile, "result.config.multilayer", buffer ,&length2, NULL)) == NULL && (strings2 = g_key_file_get_string_list(pymcaFile, "multilayer", buffer ,&length2, NULL)) == NULL) {
				g_fprintf(stderr,"Could not find key %s in multilayer... Fatal error",buffer);
				return rv;

			}
			//check if it's active
			active = atoi(strings2[0]);
			if (active != 1) {
				continue;
			}
			if (reference_layer_match == FALSE && g_strcmp0(reference_layer_str, buffer) == 0) {
				reference_layer_match = TRUE;
				*reference_layer = *n_multilayer_layers;
				g_free(reference_layer_str);
			}

			//ok... allocate memory
			*(multilayer_layers) = (xmi_layer *) g_realloc(*(multilayer_layers), sizeof(xmi_layer)*++(*n_multilayer_layers));
			temp = *multilayer_layers+*n_multilayer_layers-1;
			if (get_composition(pymcaFile, strings2[1], &temp, FALSE) == 0)
				return rv;
			//density and thickness
			(*multilayer_layers+*n_multilayer_layers-1)->density = g_ascii_strtod(strings2[2],NULL);
			(*multilayer_layers+*n_multilayer_layers-1)->thickness= g_ascii_strtod(strings2[3],NULL);

			g_strfreev(strings2);
			g_free(buffer);
		}
		if (reference_layer_match == FALSE) {
			g_fprintf(stderr,"ReferenceLayer was not active... Assuming first layer\n");
			*reference_layer = 1;
		}
	}
	else {
		//single layer
		if (get_composition(pymcaFile, strings[1], multilayer_layers, TRUE) == 0)
			return rv;

		*n_multilayer_layers = 1;
		*reference_layer = 1;

		//get density and thickness
		(*multilayer_layers)->density = g_ascii_strtod(strings[2],NULL);
		(*multilayer_layers)->thickness= g_ascii_strtod(strings[3],NULL);
	}

	if (!use_single_run) {
		if (ilay_pymca > *n_multilayer_layers) {
			fprintf(stderr,"Invalid value for xrfmc.setup.layer found\nMust be less or equal than the number of layers in Matrix\n");
			return rv;
		}

		for (i = 0 ; i < (*multilayer_layers+ilay_pymca-1)->n_elements ; i++) {
			flags[(*multilayer_layers+ilay_pymca-1)->Z[i]] = 1;
		}


#if DEBUG == 2
		for (i = 1 ; i < 100 ; i++)
			if (flags[i] == 1)
				fprintf(stdout,"Element flagged: %s\n",AtomicNumberToSymbol(i));

#endif
	}
	g_strfreev(strings);

	rv = 1;
	return rv;
}

static int get_peak_areas(GKeyFile *pymcaFile, xmi_pymca *pymca_input) {
	int rv = 0;
	gchar **elements, **lines;
	gsize n_elements, n_lines;
	GError *error = NULL;
	int i, Z, j;
	int K_found, Ka_found, Kb_found, L_found, L1_found, L2_found, L3_found;
	int use_K, use_L;
	gchar *buffer;

	//sort variables
	int *z_arr, *sorted_Z_ind;
	double *k_alpha, *l_alpha;



	//first examine result.config.peaks...
	elements = g_key_file_get_keys(pymcaFile, "result.config.peaks", &n_elements, &error);
	if (elements == NULL || n_elements == 0) {
		fprintf(stderr,"No peaks were configured for the fit: aborting\n");
		return rv;
	}

	//g_malloc memory...
	(pymca_input)->n_peaks = n_elements;
	(pymca_input)->z_arr = (int *) g_malloc(sizeof(int)*n_elements);
	(pymca_input)->k_alpha = (double *) g_malloc(sizeof(double)*n_elements);
	(pymca_input)->l_alpha = (double *) g_malloc(sizeof(double)*n_elements);

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
			if (g_strcmp0("K",g_strstrip(lines[j])) == 0) {
				K_found = 1;
			}
			else if (g_strcmp0("Ka",g_strstrip(lines[j])) == 0) {
				Ka_found = 1;
			}
			else if (g_strcmp0("Kb",g_strstrip(lines[j])) == 0) {
				Kb_found = 1;
			}
			else if (g_strcmp0("L",g_strstrip(lines[j])) == 0) {
				L_found = 1;
			}
			else if (g_strcmp0("L1",g_strstrip(lines[j])) == 0) {
				L1_found = 1;
			}
			else if (g_strcmp0("L2",g_strstrip(lines[j])) == 0) {
				L2_found = 1;
			}
			else if (g_strcmp0("L3",g_strstrip(lines[j])) == 0) {
				L3_found = 1;
			}
			else if (g_strcmp0("M",g_strstrip(lines[j])) == 0) {
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
		buffer = g_strdup_printf("result.%s %s", elements[i],use_K ? "K.KL3" : "Ka.KL3a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s Si_KL3esc", elements[i],use_K ? "K.KL3" : "Ka.KL3a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s Si_KM3esc", elements[i],use_K ? "K.KL3" : "Ka.KL3a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s", elements[i],use_K ? "K.KL2" : "Ka.KL2a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s Si_KL3esc", elements[i],use_K ? "K.KL2" : "Ka.KL2a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s Si_KM3esc", elements[i],use_K ? "K.KL2" : "Ka.KL2a");
		(pymca_input)->k_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s", elements[i],use_L ? "L.L3M5*" : "L3.L3M5");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s Si_KL3esc", elements[i],use_L ? "L.L3M5*" : "L3.L3M5");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s Si_KM3esc", elements[i],use_L ? "L.L3M5*" : "L3.L3M5");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s", elements[i],use_L ? "L.L3M4*" : "L3.L3M4");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s Si_KL3esc", elements[i],use_L ? "L.L3M4*" : "L3.L3M4");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);

		buffer = g_strdup_printf("result.%s %s Si_KM3esc", elements[i],use_L ? "L.L3M4*" : "L3.L3M4");
		(pymca_input)->l_alpha[i] += g_key_file_get_double(pymcaFile, buffer, "fitarea", NULL);
		g_free(buffer);


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
		fprintf(stdout,"pymca_input->l_alpha[%i]: %lf\n",i,pymca_input->l_alpha[i]);
#endif
	}

	g_free(z_arr);
	g_free(k_alpha);
	g_free(l_alpha);
	g_free(sorted_Z_ind);
	g_strfreev(elements);

	rv = 1;
	return rv;
}

static int read_excitation_spectrum(GKeyFile *pymcaFile, xmi_excitation **excitation, xmi_detector *detector) {
	gchar **energy = NULL;
	gdouble *energyweight = NULL;
	gint *energyflag = NULL;
	gsize n_energy, n_energyweight, n_energyflag;
	int rv = 0;
	gint i;
	double pdeg;
	double flux;
	double livetime;
	GError *error = NULL;

	//get degree of polarization;
	pdeg = g_key_file_get_double(pymcaFile, "xrfmc.setup", "p_polarisation", NULL);
	flux = g_key_file_get_double(pymcaFile, "result.config.concentrations", "flux", &error);
	if (error != NULL) {
		g_clear_error(&error);
		flux = g_key_file_get_double(pymcaFile, "concentrations", "flux", &error);
		if (error != NULL) {
			g_fprintf(stderr,"Could not find reference for %s... Aborting\n", "flux");
			return rv;
		}
	}

	livetime = g_key_file_get_double(pymcaFile, "result.config.concentrations", "time", &error);
	if (error != NULL) {
		g_clear_error(&error);
		livetime = g_key_file_get_double(pymcaFile, "concentrations", "time", &error);
		if (error != NULL) {
			g_fprintf(stderr,"Could not find reference for %s... Aborting\n", "time");
			return rv;
		}
	}

	if (flux <= 0.0) {
		fprintf(stderr,"Flux must be a positive value... Fatal error\n");
		return rv;
	}
	if (livetime <= 0.0) {
		fprintf(stderr,"Livetime must be a positive value... Fatal error\n");
		return rv;
	}

	detector->live_time = livetime;

	if ((energy = g_key_file_get_string_list(pymcaFile, "result.config.fit","energy",&n_energy, NULL)) == NULL && (energy = g_key_file_get_string_list(pymcaFile, "fit","energy",&n_energy, NULL)) == NULL) {
		g_fprintf(stderr,"Could not find reference for %s... Aborting\n", "energy");
		return rv;
	}
	if ((energyweight = g_key_file_get_double_list(pymcaFile, "result.config.fit","energyweight", &n_energyweight, NULL)) == NULL && (energyweight = g_key_file_get_double_list(pymcaFile, "fit","energyweight", &n_energyweight, NULL)) == NULL) {
		g_fprintf(stderr,"Could not find reference for %s... Aborting\n", "energyweight");
		return rv;
	}
	if ((energyflag = g_key_file_get_integer_list(pymcaFile, "result.config.fit","energyflag", &n_energyflag, NULL)) == NULL && (energyflag = g_key_file_get_integer_list(pymcaFile, "fit","energyflag", &n_energyflag, NULL)) == NULL) {
		g_fprintf(stderr,"Could not find reference for %s... Aborting\n", "energyflag");
		return rv;
	}

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
	(*excitation) = (xmi_excitation *) g_malloc(sizeof(xmi_excitation));
	(*excitation)->n_discrete = 0;
	(*excitation)->discrete = NULL;
	(*excitation)->n_continuous = 0;
	(*excitation)->continuous = NULL;

	gdouble weights_sum = 0.0;

	for (i = 0 ; i < n_energy ; i++) {
		if (energyflag[i] == FALSE)
			continue;
		else
			weights_sum += energyweight[i];
	}

	if (weights_sum <= 0.0) {
		g_fprintf(stderr,"Sum of flagged energy weights must be strictly positive... Fatal error\n");
		return rv;
	}


	for (i = 0 ; i < n_energy ; i++) {
		//check flag
		if (energyflag[i] == FALSE)
			continue;

		(*excitation)->discrete = (xmi_energy_discrete *) g_realloc((*excitation)->discrete, ++((*excitation)->n_discrete)*sizeof(xmi_energy_discrete));
		(*excitation)->discrete[((*excitation)->n_discrete)-1].energy = g_ascii_strtod(energy[i],NULL);
		if ((*excitation)->discrete[((*excitation)->n_discrete)-1].energy <= 0.0) {
			fprintf(stderr,"A flagged energy turned out to be negative or zero... Fatal error\n");
			return rv;
		}
		(*excitation)->discrete[((*excitation)->n_discrete)-1].horizontal_intensity = energyweight[i]*flux*(1.0+pdeg)/2.0/weights_sum;
		(*excitation)->discrete[((*excitation)->n_discrete)-1].vertical_intensity = energyweight[i]*flux*(1.0-pdeg)/2.0/weights_sum;
		(*excitation)->discrete[((*excitation)->n_discrete)-1].sigma_x = g_key_file_get_double(pymcaFile, "xrfmc.setup","source_size_x", NULL);
		(*excitation)->discrete[((*excitation)->n_discrete)-1].sigma_y = g_key_file_get_double(pymcaFile, "xrfmc.setup","source_size_y", NULL);
		(*excitation)->discrete[((*excitation)->n_discrete)-1].sigma_xp = g_key_file_get_double(pymcaFile, "xrfmc.setup","source_diverg_x", NULL);
		(*excitation)->discrete[((*excitation)->n_discrete)-1].sigma_yp = g_key_file_get_double(pymcaFile, "xrfmc.setup","source_diverg_y", NULL);
		(*excitation)->discrete[((*excitation)->n_discrete)-1].distribution_type = XMI_DISCRETE_MONOCHROMATIC;
		(*excitation)->discrete[((*excitation)->n_discrete)-1].scale_parameter = 0.0;

	}

	qsort((*excitation)->discrete,(*excitation)->n_discrete,sizeof(xmi_energy_discrete),xmi_cmp_struct_xmi_energy_discrete);


	g_strfreev(energy);
	g_free(energyweight);
	g_free(energyflag);

	rv = 1;
	return rv;
}


int xmi_read_input_pymca(char *pymca_file, xmi_input **input, xmi_pymca **pymca_input, int use_matrix_override, int use_roi_normalization, int use_single_run) {
	int rv = 0;
	GKeyFile *pymcaFile;
	GError *error=NULL;
	xmi_layer *multilayer_layers = NULL;
	int n_multilayer_layers=0;
	xmi_layer *det_layers = NULL;
	int n_det_layers = 0;
	xmi_layer *exc_layers = NULL;
	int n_exc_layers = 0;
	xmi_layer *crystal_layers = NULL;
	int n_crystal_layers = 0;
	int i,j,k;
	int found;
	xmi_geometry *geometry = NULL;
	xmi_excitation *excitation = NULL;
	xmi_detector *detector = NULL;
	xmi_general *general = NULL;
	gchar **strings, *ydata_string;
	int override_required = 0;

	//read the file...
	pymcaFile = g_key_file_new();
	if (g_key_file_load_from_file(pymcaFile, pymca_file, G_KEY_FILE_NONE, &error) == FALSE) {
		fprintf(stderr,"Could not parse file %s\n",pymca_file);
		fprintf(stderr,"Error message: %s\n",error->message);
		return rv;
	}

	g_key_file_set_list_separator(pymcaFile, ',');

	//allocate input
	*input = (xmi_input *) g_malloc(sizeof(xmi_input));
	*pymca_input = (xmi_pymca *) g_malloc(sizeof(xmi_pymca));


	if (!use_single_run) {
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
	}

	//read multilayer
	//set flags to zero
	for (i = 0 ; i < 100 ; i++)
		(*pymca_input)->flags[i] = 0;

	int reference_layer = 1;

	if (read_multilayer_composition(pymcaFile, &multilayer_layers, &n_multilayer_layers, (*pymca_input)->flags, (*pymca_input)->ilay_pymca, use_single_run, &reference_layer) == 0)
		return rv;

#if DEBUG == 2
	fprintf(stdout,"Before get_peak_areas\n");
#endif

	if (!use_single_run) {
	//get_peak_areas
		if (get_peak_areas(pymcaFile, *pymca_input) == 0)
			return rv;
	}
#if DEBUG == 2
	fprintf(stdout,"After get_peak_areas\n");
#endif


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

	//detector parameters
	if (read_detector_params(pymcaFile, &detector) == 0)
		return rv;

	//energy
	if (read_excitation_spectrum(pymcaFile, &excitation, detector) == 0)
		return rv;

	//elements to be ignored
	gchar **ignore_elements = g_key_file_get_string_list(pymcaFile, "xrfmc.setup", "ignore_elements", NULL, NULL);
	(*pymca_input)->n_ignore_elements = 0;
	(*pymca_input)->ignore_elements = NULL;
	if (ignore_elements) {
		int element;
		int atomic_number;
		for (element = 0 ; ignore_elements[element] != NULL ; element++) {
			atomic_number = SymbolToAtomicNumber(ignore_elements[element]);
			if (atomic_number < 1) {
				g_fprintf(stderr,"Invalid chemical symbol %s found in ignore_elements\n", ignore_elements[element]);
				return rv;
			}
			(*pymca_input)->ignore_elements = g_realloc((*pymca_input)->ignore_elements, sizeof(int)*++(*pymca_input)->n_ignore_elements);
			(*pymca_input)->ignore_elements[(*pymca_input)->n_ignore_elements-1] = atomic_number;
		}
	}


	if (!use_single_run) {

		(*pymca_input)->usematrix = g_key_file_get_integer(pymcaFile, "result.config.concentrations", "usematrix", NULL);

		if (use_roi_normalization && !(*pymca_input)->usematrix) {
			g_fprintf(stderr, "Inconsistency detected: --enable-roi-normalization requires the option \"From matrix composition\" to be selected in PyMca\n");
			return rv;
		}

		gchar *reference = g_key_file_get_string(pymcaFile, "result.config.concentrations", "reference", NULL);

		if (reference != NULL && (*pymca_input)->usematrix == 1) {
			if (g_strcmp0(reference, "Auto") == 0) {
				g_fprintf(stderr, "When using ROI normalization, the \"The matrix reference element\" has to be different from Auto\n");
				return rv;
			}
			else {
				(*pymca_input)->reference = SymbolToAtomicNumber(reference);
				if ((*pymca_input)->reference == 0) {
					g_fprintf(stderr,"Invalid Matrix reference element detected.\n");
					return rv;
				}
				//check if the reference element is part of the layer of interest
				found = 0;
				for (k = 0 ; k < multilayer_layers[(*pymca_input)->ilay_pymca-1].n_elements ; k++) {
					if (multilayer_layers[(*pymca_input)->ilay_pymca-1].Z[k] == (*pymca_input)->reference) {
						found = 1;
						break;
					}
				}
				if (!found) {
					g_fprintf(stderr,"Reference element %s is not part of the matrix composition\n",AtomicNumberToSymbol((*pymca_input)->reference));
					(*pymca_input)->reference = 0;
					return rv;
				}
				if (multilayer_layers[(*pymca_input)->ilay_pymca-1].n_elements < 2){
					g_fprintf(stderr,"Matrix layer of interest must consist of at least two elements when --enable-roi-normalization is used\n");
					return rv;
				}
			}
			g_free(reference);
		}

		//determine elements that will actually be quantified (non-matrix)
		//1st condition: there has to be a (positive) net-line intensity available
		//2nd condition: the element may not be part of the matrix composition unless matrix is overridden
		//3rd condition: the element may not be part of any of the other layers
		//4th condition: the element may not be the reference element if --enable-roi-integration is active
		//5th condition: the element may not be a part of the ignore_elements list
		(*pymca_input)->z_arr_quant = NULL;
		(*pymca_input)->n_z_arr_quant = 0;
		for (i = 0 ; i < (*pymca_input)->n_peaks ; i++) {
#if DEBUG == 2
			fprintf(stderr,"Element %s\n",AtomicNumberToSymbol((*pymca_input)->z_arr[i]));
#endif

			if ((*pymca_input)->k_alpha[i] + (*pymca_input)->l_alpha[i] <= 0.0)
				continue;

			if ((*pymca_input)->usematrix && (*pymca_input)->z_arr[i] == (*pymca_input)->reference)
				continue;

			if ((*pymca_input)->n_ignore_elements > 0) {
				found = 0;
				for (j = 0 ; j < (*pymca_input)->n_ignore_elements ; j++) {
					if ((*pymca_input)->ignore_elements[j] == (*pymca_input)->z_arr[i]) {
						found = 1;
						break;
					}
				}
				if (found) {
					//element ignored
					continue;
				}
			}

			found = 0;
			//check multilayer
			for (j = 0 ; j < n_multilayer_layers ; j++) {
				for (k = 0 ; k < multilayer_layers[j].n_elements ; k++) {
					if (multilayer_layers[j].Z[k] == (*pymca_input)->z_arr[i]) {
						if (use_matrix_override == 1 && j == (*pymca_input)->ilay_pymca-1) {
							found = 0;
							override_required = 1;
						}
						else {
							found = 1;
							override_required = 0;
							break;
						}
					}
				}
				if (found == 1)
					break;
			}
			if (found == 0) {
				//found
				(*pymca_input)->z_arr_quant = (int *) g_realloc((*pymca_input)->z_arr_quant,sizeof(int)*++((*pymca_input)->n_z_arr_quant) );
				(*pymca_input)->z_arr_quant[((*pymca_input)->n_z_arr_quant)-1] = (*pymca_input)->z_arr[i];
#if DEBUG == 2
				fprintf(stdout,"Element to be quantified: %s\n",AtomicNumberToSymbol((*pymca_input)->z_arr_quant[((*pymca_input)->n_z_arr_quant)-1]));
#endif
			}
		}

		//sort
		qsort((*pymca_input)->z_arr_quant, (*pymca_input)->n_z_arr_quant, sizeof(int),xmi_cmp_int  );

	}


	//general
	general = (xmi_general *) g_malloc(sizeof(xmi_general));
	general->outputfile = g_strdup("");
	general->comments = g_strdup("");
	general->n_photons_interval = 100000;
	general->n_photons_line = 100000;
	general->version = 1.0;


	int n_photons_line = g_key_file_get_integer(pymcaFile, "xrfmc.setup","histories",&error);

	if (error != NULL) {
		//key not found: use default value
		g_clear_error(&error);
	}
	else if (n_photons_line < 1) {
		g_fprintf(stderr, "histories key in xrfmc.setup must be a strictly positive integer... Fatal error\n");
		return rv;
	}
	else
		general->n_photons_line = n_photons_line;

	general->n_interactions_trajectory = g_key_file_get_integer(pymcaFile, "xrfmc.setup","nmax_interaction",NULL);
	if (general->n_interactions_trajectory < 1) {
		fprintf(stderr,"Maximum number of interactions must be at least one... Fatal error\n");
		return rv;
	}

	//put it all together
	(*input)->general = general;
	//allocate composition
	(*input)->composition = (xmi_composition *) g_malloc(sizeof(xmi_composition));
	(*input)->composition->n_layers = n_multilayer_layers;
	(*input)->composition->layers = multilayer_layers;
	//check ReferenceLayer
	(*input)->composition->reference_layer = 1;

	(*input)->geometry = geometry;
	(*input)->excitation = excitation;
	(*input)->absorbers = (xmi_absorbers *) g_malloc(sizeof(xmi_absorbers));
	(*input)->absorbers->n_exc_layers = n_exc_layers;
	(*input)->absorbers->exc_layers = exc_layers;
	(*input)->absorbers->n_det_layers = n_det_layers;
	(*input)->absorbers->det_layers = det_layers;
	(*input)->detector = detector;
	detector->n_crystal_layers = n_crystal_layers;
	detector->crystal_layers = crystal_layers;

	//nchannels
	//get max from energies and from xmax
	int nchannels_energy = (int) (1.10*((excitation)->discrete[((excitation)->n_discrete)-1].energy - detector->zero)/detector->gain);
	int nchannels_xmax;


	nchannels_xmax = (int) (g_key_file_get_double(pymcaFile, "result.config.fit", "xmax", &error)*1.10);
	if (error != NULL) {
		g_clear_error(&error);
		nchannels_xmax = (int) (g_key_file_get_double(pymcaFile, "fit", "xmax", &error)*1.10);
		if (error != NULL) {
			nchannels_xmax = 0;
			return rv;
		}
	}
	(*input)->detector->nchannels = MAX(nchannels_xmax, nchannels_energy);
#if DEBUG == 1
	fprintf(stdout,"nchannels: %i\n",(*pymca_input)->nchannels);
	fprintf(stdout,"nchannels_xmax: %i\n", nchannels_xmax);
	fprintf(stdout,"nchannels_energy: %i\n", nchannels_energy);
#endif


	if (use_single_run) {
		rv = 1;
		return rv;
	}

	(*pymca_input)->xmin = (int) g_key_file_get_integer(pymcaFile, "result.config.fit", "xmin", NULL);
	(*pymca_input)->xmax = (int) g_key_file_get_integer(pymcaFile, "result.config.fit", "xmax", NULL);
	//integrate between xmin and xmax -> sum_xmin_xmax
	//get ydata
	ydata_string = g_key_file_get_string(pymcaFile, "result", "ydata", NULL);
	strings = g_strsplit(ydata_string," ",100000);

	for (i = 1 ; i <= (*pymca_input)->xmax-(*pymca_input)->xmin+1 ; i++ ) {
		(*pymca_input)->sum_xmin_xmax += (double) g_ascii_strtod(strings[i], NULL);
	}

	g_strfreev(strings);
	g_free(ydata_string);


	//see if we find a nice scatter peak, which can be used for adjusting the beam intensity afterwards
	if (read_scatter_intensity(pymcaFile, *pymca_input) == 0) {
		rv = 0;
		return rv;
	}

	if (read_pymca_concentrations(pymcaFile, *pymca_input) == 0) {
		rv = 0;
		return rv;
	}


	//adjust ilay_pymca if necessary
	(*pymca_input)->ilay_pymca--;

#if DEBUG == 2
	fprintf(stdout,"ilay_pymca: %i\n",(*pymca_input)->ilay_pymca);
#endif

	if (override_required) {
		//replace all quantifiable elements with one element, determined based on the weighted average
		int *Z_or = NULL;
		int n_elements_or = 0;
		double *weight_or = NULL;

		int *Z_orig = NULL;
		int n_elements_orig = 0;
		double *weight_orig = NULL;

		found = 0;

		for (i = 0 ; i < (*input)->composition->layers[(*pymca_input)->ilay_pymca].n_elements ; i++) {
			found = 0;
			for (j = 0 ; j < (*pymca_input)->n_z_arr_quant ; j++) {
				if ((*input)->composition->layers[(*pymca_input)->ilay_pymca].Z[i] ==
					(*pymca_input)->z_arr_quant[j]) {
					n_elements_or++;
					Z_or = (int *) g_realloc(Z_or, sizeof(int)*n_elements_or);
					weight_or = (double *) g_realloc(weight_or, sizeof(double)*n_elements_or);
					Z_or[n_elements_or-1] = (*input)->composition->layers[(*pymca_input)->ilay_pymca].Z[i];
					weight_or[n_elements_or-1] = (*input)->composition->layers[(*pymca_input)->ilay_pymca].weight[i];
					found = 1;
					break;
				}
			}
			if (! found) {
				n_elements_orig++;
				Z_orig = (int *) g_realloc(Z_orig, sizeof(int)*n_elements_orig);
				weight_orig = (double *) g_realloc(weight_orig, sizeof(double)*n_elements_orig);
				Z_orig[n_elements_orig-1] = (*input)->composition->layers[(*pymca_input)->ilay_pymca].Z[i];
				weight_orig[n_elements_orig-1] = (*input)->composition->layers[(*pymca_input)->ilay_pymca].weight[i];
			}
		}



		if (n_elements_or == 0) {
			fprintf(stderr,"Error in matrix override function: no elements were found that could be overridden\n");
			rv = 0;
			return rv;
		}

		//calculate sum of weights
		double weights_sum_or = xmi_sum_double(weight_or, n_elements_or);
		xmi_scale_double(weight_or, n_elements_or, 1.0/weights_sum_or);
		double matrix_el_dbl = 0.0;
		for (i = 0 ; i < n_elements_or ; i++)
			matrix_el_dbl += weight_or[i]*Z_or[i];

		int matrix_el_int = (int) lround(matrix_el_dbl);

		fprintf(stdout,"Matrix element start value: %i\n",matrix_el_int);

		int found1, found2;
/*		int found12 = 0;
 		for (i = 1 ; i < MIN(5, abs(matrix_el_int - 10)) ; i++) {
			found1 = found2 = 0;
			for (j = 0 ; j < (*pymca_input)->n_z_arr_quant ; j++) {
				if (matrix_el_int - i == (*pymca_input)->z_arr_quant[j]) {
					found1 = 1;
					break;
				}
			}
			if (found1)
				continue;
			for (j = 0 ; j < (*pymca_input)->n_z_arr_quant ; j++) {
				if (matrix_el_int + i == (*pymca_input)->z_arr_quant[j]) {
					found2 = 1;
					break;
				}
			}
			if (!found1 && !found2) {
				found12 = 1;
				break;
			}
		}

		if (found12) {
			//found two suitable matrix elements -> add them to the array
			g_free((*input)->composition->layers[(*pymca_input)->ilay_pymca].Z);
			g_free((*input)->composition->layers[(*pymca_input)->ilay_pymca].weight);
			(*input)->composition->layers[(*pymca_input)->ilay_pymca].n_elements = n_elements_orig+2;
			(*input)->composition->layers[(*pymca_input)->ilay_pymca].Z = (int *) g_realloc(Z_orig, sizeof(int)*(n_elements_orig+2));
			(*input)->composition->layers[(*pymca_input)->ilay_pymca].weight = (double *) g_realloc(weight_orig,sizeof(double)*(n_elements_orig+2));
			(*input)->composition->layers[(*pymca_input)->ilay_pymca].Z[n_elements_orig] = matrix_el_int-i;
			(*input)->composition->layers[(*pymca_input)->ilay_pymca].Z[n_elements_orig+1] = matrix_el_int+i;
			(*input)->composition->layers[(*pymca_input)->ilay_pymca].weight[n_elements_orig] = 0.5*weights_sum_or;
			(*input)->composition->layers[(*pymca_input)->ilay_pymca].weight[n_elements_orig+1] = 0.5*weights_sum_or;
			fprintf(stdout,"Matrix adjusted with two elements\n");
		}
		else {*/
			//try with one element
			//for (j = 0 ; j < (*pymca_input)->n_z_arr_quant ; j++) {
			//	fprintf(stdout,"z_arr_quant: %i\n",(*pymca_input)->z_arr_quant[j]);
			//}

			for (i = 1 ; i < MIN(7, abs(matrix_el_int - 10)) ; i++) {
				found1 = found2 = 0;
				for (j = 0 ; j < (*pymca_input)->n_z_arr_quant ; j++) {
					if (matrix_el_int + i == (*pymca_input)->z_arr_quant[j]) {
						found1 = 1;
						break;
					}
				}
				if (!found1) {
					break;
				}
				for (j = 0 ; j < (*pymca_input)->n_z_arr_quant ; j++) {
					if (matrix_el_int - i == (*pymca_input)->z_arr_quant[j]) {
						found2 = 1;
						break;
					}
				}
				if (!found2) {
					break;
				}
			}
			if (!found1 || !found2) {
				//found two suitable matrix elements -> add them to the array
				g_free((*input)->composition->layers[(*pymca_input)->ilay_pymca].Z);
				g_free((*input)->composition->layers[(*pymca_input)->ilay_pymca].weight);
				(*input)->composition->layers[(*pymca_input)->ilay_pymca].n_elements = n_elements_orig+1;
				(*input)->composition->layers[(*pymca_input)->ilay_pymca].Z = (int *) g_realloc(Z_orig, sizeof(int)*(n_elements_orig+1));
				(*input)->composition->layers[(*pymca_input)->ilay_pymca].weight = (double *) g_realloc(weight_orig,sizeof(double)*(n_elements_orig+1));
				if (!found1)
					(*input)->composition->layers[(*pymca_input)->ilay_pymca].Z[n_elements_orig] = matrix_el_int+i;
				else
					(*input)->composition->layers[(*pymca_input)->ilay_pymca].Z[n_elements_orig] = matrix_el_int-i;
				(*input)->composition->layers[(*pymca_input)->ilay_pymca].weight[n_elements_orig] = 0.5*weights_sum_or;
				fprintf(stdout,"Matrix adjusted with one element: %i\n",(*input)->composition->layers[(*pymca_input)->ilay_pymca].Z[n_elements_orig]);

			}
			else {
				fprintf(stderr,"Matrix override: search for alternative matrix elements produced no results\n");
				rv = 0;
				return rv;
			}


		//}
	}

	rv = 1;

	return rv;
}

xmi_layer xmi_ilay_composition_pymca(xmi_layer *matrix, xmi_pymca *pymca_aux , double *weights_arr_quant) {

	xmi_layer rv;
	double sum_quant, sum_matrix, sum_above;
	int i,j;
	double *weight;
	int *sorted_Z_ind;
	int *Z;
	int maxloc;
	double maxval;
	double max_net_intensity;
	double reference_weight = 0.0;

	rv.Z = (int *) g_malloc(sizeof(int)*(matrix->n_elements+pymca_aux->n_z_arr_quant));
	rv.weight = (double *) g_malloc(sizeof(double)*(matrix->n_elements+pymca_aux->n_z_arr_quant));
	rv.density = matrix->density;
	rv.thickness = matrix->thickness;

	if (pymca_aux->usematrix) {
		for (i = 0 ; i < matrix->n_elements ; i++) {
			if (matrix->Z[i] == pymca_aux->reference) {
				reference_weight = matrix->weight[i];
				break;
			}
		}
	}
	else
		reference_weight = 0.0;




	//calculate sum
	sum_quant = xmi_sum_double(weights_arr_quant, pymca_aux->n_z_arr_quant);
	sum_matrix = xmi_sum_double(matrix->weight, matrix->n_elements);
	xmi_scale_double(matrix->weight, matrix->n_elements, 1.0/sum_matrix);

	if (sum_quant + reference_weight > 1.0) {
		g_fprintf(stdout,"weights sum of quantifiable elements > 1.0\nRescaling...\n");
		//look up maximum in array
		//if value is > 1.0... abort...
		//else keep this value and scale the others accordingly. Matrix will be set to zero
		maxval = xmi_maxval_double(weights_arr_quant, pymca_aux->n_z_arr_quant);
		maxloc = xmi_maxloc_double(weights_arr_quant, pymca_aux->n_z_arr_quant);

		//check if there are multiple values with this maxval
		//if so use the one with the highest pymca net-line intensity
		for (i = 0 ; i < pymca_aux->n_peaks ; i++) {
			if (pymca_aux->z_arr[i] == pymca_aux->z_arr_quant[maxloc]) {
				if (pymca_aux->k_alpha[i] > 0.0) {
					max_net_intensity=pymca_aux->k_alpha[i];
				}
				else if (pymca_aux->l_alpha[i] > 0.0) {
					max_net_intensity=pymca_aux->l_alpha[i];
				}
				break;
			}
		}
#if DEBUG == 1
		fprintf(stdout,"initial max_net_intensity: %lf\n",max_net_intensity);
#endif

		for (i = 0 ; i < pymca_aux->n_z_arr_quant ; i++) {
			if (weights_arr_quant[i] == maxval && i != maxloc) {
				//another maximum found
				//check pymca net-line intensity
				for (j = 0 ; j < pymca_aux->n_peaks ; j++) {
					if (pymca_aux->z_arr[j] == pymca_aux->z_arr_quant[i]) {
						if (pymca_aux->k_alpha[j] > max_net_intensity) {
							max_net_intensity=pymca_aux->k_alpha[j];
							maxloc = i;
						}
						else if (pymca_aux->l_alpha[i] > max_net_intensity) {
							max_net_intensity=pymca_aux->l_alpha[i];
							maxloc = i;
						}
						break;
					}
				}
			}
		}

#if DEBUG == 1
		fprintf(stdout,"initial max_net_intensity: %lf\n",max_net_intensity);
#endif


#if DEBUG == 1
		for (i = 0 ; i < pymca_aux->n_z_arr_quant ; i++) {
			fprintf(stdout,"Element %i: %lf %%\n",pymca_aux->z_arr_quant[i],weights_arr_quant[i]*100.0);
		}
		fprintf(stdout,"maxval: %lf\n",maxval);
		fprintf(stdout,"maxloc: %i\n",maxloc);
#endif
		if (maxval > 1.0) {
			g_fprintf(stderr,"Maximum weight is above 100 %%: element %i (%lf %%)\nAborting",pymca_aux->z_arr_quant[maxloc],maxval*100.0);
			exit(1);
		}
		//calculate sum of elements before the one with maximal line intensity
		sum_above = xmi_sum_double(weights_arr_quant, maxloc+1);
		xmi_scale_double(weights_arr_quant, maxloc+1, (1.0-(sum_quant-sum_above)-reference_weight)/(sum_above));
		//weights_arr_quant[maxloc] = maxval;
		sum_quant = 1.0-reference_weight;
	}

#if DEBUG == 1
	for (i = 0 ; i < pymca_aux->n_z_arr_quant ; i++) {
		fprintf(stdout,"Element %i: %lf %%\n",pymca_aux->z_arr_quant[i],weights_arr_quant[i]*100.0);
	}
#endif

	if (pymca_aux->usematrix) {
		if (sum_quant + reference_weight > 1.0) {
			g_fprintf(stderr, "Sum of quantifiable element weight fractions plus reference element is greater than 1.0\n");
			exit(1);
		}
	}

	for (i = 0 ; i < matrix->n_elements ; i++) {
		rv.Z[i] = matrix->Z[i];
		if (pymca_aux->usematrix && matrix->Z[i] == pymca_aux->reference) {
			rv.weight[i] = reference_weight;
		}
		else {
			rv.weight[i] = matrix->weight[i]*(1.0-sum_quant-reference_weight)/(1.0-reference_weight);
		}
#if DEBUG == 1
		g_fprintf(stdout, "Matrix: %i -> %lf\n", rv.Z[i], rv.weight[i]);
#endif
	}

	for (i = 0 ; i < pymca_aux->n_z_arr_quant ; i++) {
		rv.Z[i+matrix->n_elements] = pymca_aux->z_arr_quant[i];
		rv.weight[i+matrix->n_elements] = weights_arr_quant[i];
		if (weights_arr_quant[i] < 0.0 ) {
			g_fprintf(stderr,"Negative weight fraction detected in xmi_ilay_composition_pymca\nUsually indicates that an element was fitted that should be omitted\n");
			exit(1);
		}
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
		if (weight[sorted_Z_ind[i]] < 0.0 ) {
			g_fprintf(stderr,"Negative weight fraction detected in xmi_ilay_composition_pymca\nUsually indicates that an element was fitted that should be omitted\n");
			exit(1);
		}
	}

#if DEBUG == 1
	g_fprintf(stdout, "total weights sum: %lf\n", xmi_sum_double(rv.weight,rv.n_elements));
#endif


	g_free(Z);
	g_free(weight);
	g_free(sorted_Z_ind);


	rv.density = matrix->density;
	rv.thickness= matrix->thickness;



	return rv;
}
