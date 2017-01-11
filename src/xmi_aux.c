/*
Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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
#include "xmi_msim.h"
#include "xmi_aux.h"
#include "xmi_data_structs.h"
#include <stdio.h>
#include <glib/gstdio.h>
#include <hdf5.h>
#include "xmi_private.h"

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)


void *xmi_memdup(const void *mem, size_t bytes) {
	return g_memdup(mem, bytes);
}

int *xmi_sort_idl_int(int *array,int n_elements) {
	int *rv;
	int *array_copy;
	int i,j;

	if (n_elements < 1) {
		rv = (int *) g_malloc(sizeof(int));
		rv[0] = -1;
		return rv;
	}

	array_copy = (int *) xmi_memdup(array,n_elements*sizeof(int));
	qsort(array_copy,(size_t) n_elements, sizeof(int),xmi_cmp_int);

#if DEBUG == 2
	fprintf(stdout,"xmi_sort_idl_int check\n");
	for (i=0 ; i < n_elements ; i++)
		fprintf(stdout,"%i\n",array_copy[i]);
#endif




	rv = (int *) g_malloc(sizeof(int)*n_elements);

	for (i = 0 ; i < n_elements ; i++) {
		for (j = 0 ; j < n_elements ; j++)
			if (array_copy[i] == array[j]) {
				rv[i] = j;
				break;
			}
	}

	g_free(array_copy);

	return rv;
}

int xmi_cmp_int(const void *a, const void *b) {
	return *((int *) a) - *((int *) b);
}

struct compoundData *xmi_layer2compoundData(struct xmi_layer *xl) {
	struct compoundData *rv;

	rv = (struct compoundData *) g_malloc(sizeof(struct compoundData));

	if (xl != NULL) {
		rv->nElements = xl->n_elements;
		rv->Elements = (int *) xmi_memdup(xl->Z, sizeof(int)*xl->n_elements);
		rv->massFractions= (double *) xmi_memdup(xl->weight, sizeof(double)*xl->n_elements);
		// TODO: remove when bumping minimum xraylib version
#if XRAYLIB_MAJOR == 3 && XRAYLIB_MINOR > 2
		rv->nAtoms = (double *) g_malloc(sizeof(int));
#endif
	}
	else {
		rv->nElements = 0;
	}
	return rv;
}

struct xmi_layer *compoundDataNIST2xmi_layer( struct compoundDataNIST *cd) {
	struct xmi_layer *rv;

	rv = (struct xmi_layer *) g_malloc(sizeof(struct xmi_layer));

		rv->n_elements = cd->nElements;
		rv->Z = (int *) xmi_memdup(cd->Elements, sizeof(int)*cd->nElements);
		rv->weight = (double *) xmi_memdup(cd->massFractions, sizeof(double)*cd->nElements);
		rv->density = cd->density;
	return rv;
}
struct xmi_layer *compoundData2xmi_layer( struct compoundData *cd) {
	struct xmi_layer *rv;

	rv = (struct xmi_layer *) g_malloc(sizeof(struct xmi_layer));

		rv->n_elements = cd->nElements;
		rv->Z = (int *) xmi_memdup(cd->Elements, sizeof(int)*cd->nElements);
		rv->weight = (double *) xmi_memdup(cd->massFractions, sizeof(double)*cd->nElements);
	return rv;
}

double *xmi_dindgen(int n) {
	double *rv;
	int i;

	if (n < 1) {
		fprintf(stderr,"xmi_dindgen requires a strictly positive argument\n");
		return NULL;
	}

	rv = (double *) g_malloc(sizeof(double)*n);

	for (i = 0 ; i < n ; i++) {
		rv[i] = (double )i;
	}

	return rv;
}

void xmi_print_progress(char *string, int progress) {
	if (progress == -1) {
		fprintf(stdout,"%s\n",string);
	}
	else {
		fprintf(stdout,"%s %3i %%\n",string,progress);
	}
}

int xmi_cmp_struct_xmi_energy_discrete(const void *a, const void *b) {
	double diff;

	diff = ((struct xmi_energy_discrete *)a)->energy - ((struct xmi_energy_discrete *)b)->energy;

	if (diff > 0.000000001)
		return 1;
	else if (diff < -0.000000001)
		return -1;
	return 0;
}

int xmi_cmp_struct_xmi_energy_continuous(const void *a, const void *b) {
	double diff;

	diff = ((struct xmi_energy_continuous *)a)->energy - ((struct xmi_energy_continuous *)b)->energy;

	if (diff > 0.000000001)
		return 1;
	else if (diff < -0.000000001)
		return -1;
	return 0;
}
#include <xraylib.h>
#ifdef HAVE_GUI
#include <gtk/gtk.h>
  #ifdef HAVE_CXX
   #include <gtkmm-plplotconfig.h>
   #include <gtkmmconfig.h>
  #else
    #include <gtkextra/gtkextra.h>
  #endif
#endif
#include <hdf5.h>
#include <libxml/xmlversion.h>
#include <libxslt/xsltconfig.h>
#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
#include <curl/curl.h>
#include <json-glib/json-glib.h>
#endif

char *xmi_version_string() {
	GString *string = g_string_sized_new(512);

	g_string_append_printf(string, "XMI-MSIM %s\n\n", VERSION);
	g_string_append(string,"Compiled with ");
	//xraylib
	g_string_append_printf(string, "xraylib %i.%i, ", XRAYLIB_MAJOR, XRAYLIB_MINOR);
	//glib
	g_string_append_printf(string, "glib %i.%i.%i, ", GLIB_MAJOR_VERSION, GLIB_MINOR_VERSION, GLIB_MICRO_VERSION);
	//gtk2
#ifdef HAVE_GUI
	g_string_append_printf(string, "gtk+ %i.%i.%i, ", GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
  #ifdef HAVE_CXX
	g_string_append_printf(string, "gtkmm %i.%i.%i, ", GTKMM_MAJOR_VERSION, GTKMM_MINOR_VERSION, GTKMM_MICRO_VERSION);
  #endif
#endif
	//hdf5
	g_string_append_printf(string, "HDF5 %i.%i.%i, ", H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
	//libxml2
	g_string_append_printf(string, "libxml2 %s,\n", LIBXML_DOTTED_VERSION);
	//libxslt
	g_string_append_printf(string, "libxslt %s, ", LIBXSLT_DOTTED_VERSION);
	//fgsl
#ifdef FGSL_VERSION
	g_string_append_printf(string, "fgsl %s, ", TOSTRING(FGSL_VERSION));
#endif
#ifdef EASYRNG_VERSION
	g_string_append_printf(string, "easyRNG %s, ", TOSTRING(EASYRNG_VERSION));
#endif
#ifdef HAVE_GUI
  #ifdef HAVE_CXX
	//gtkextra
	g_string_append_printf(string, "gtkmm-plplot %i.%i", GTKMM_PLPLOT_MAJOR_VERSION, GTKMM_PLPLOT_MINOR_VERSION);
  #else
	//gtkextra
	g_string_append_printf(string, "gtkextra %i.%i.%i", GTKEXTRA_MAJOR_VERSION, GTKEXTRA_MINOR_VERSION, GTKEXTRA_MICRO_VERSION);
  #endif
#endif

#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
	g_string_append_printf(string, ", curl %i.%i.%i", LIBCURL_VERSION_MAJOR, LIBCURL_VERSION_MINOR, LIBCURL_VERSION_PATCH);
	g_string_append_printf(string, ", json-glib %i.%i.%i", JSON_MAJOR_VERSION, JSON_MINOR_VERSION, JSON_MICRO_VERSION);
#endif
	g_string_append(string, "\n\n");
	g_string_append(string,
"Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze\n"
"\n"
"This program is free software: you can redistribute it and/or modify\n"
"it under the terms of the GNU General Public License as published by\n"
"the Free Software Foundation, either version 3 of the License, or\n"
"(at your option) any later version.\n"
"\n"
"This program is distributed in the hope that it will be useful,\n"
"but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
"GNU General Public License for more details.\n"
"\n"
"You should have received a copy of the GNU General Public License\n"
"along with this program.  If not, see <http://www.gnu.org/licenses/>.\n"
);

	return g_string_free(string, FALSE);
}



/* getdelim.c --- Implementation of replacement getdelim function.
   Copyright (C) 1994, 1996-1998, 2001, 2003, 2005-2013 Free Software
   Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.  */

/* Ported from glibc by Simon Josefsson. */

/* Don't use __attribute__ __nonnull__ in this compilation unit.  Otherwise gcc
   optimizes away the lineptr == NULL || n == NULL || fp == NULL tests below.  */
#define _GL_ARG_NONNULL(params)

#include <stdio.h>

#include <limits.h>
#include <stdint.h>
#include <errno.h>

#ifndef HAVE_GETDELIM

#ifndef SSIZE_MAX
# define SSIZE_MAX ((ssize_t) (SIZE_MAX / 2))
#endif

#if USE_UNLOCKED_IO
# include "unlocked-io.h"
# define getc_maybe_unlocked(fp)        getc(fp)
#elif !HAVE_FLOCKFILE || !HAVE_FUNLOCKFILE || !HAVE_DECL_GETC_UNLOCKED
# undef flockfile
# undef funlockfile
# define flockfile(x) ((void) 0)
# define funlockfile(x) ((void) 0)
# define getc_maybe_unlocked(fp)        getc(fp)
#else
# define getc_maybe_unlocked(fp)        getc_unlocked(fp)
#endif

/* Read up to (and including) a DELIMITER from FP into *LINEPTR (and
   NUL-terminate it).  *LINEPTR is a pointer returned from malloc (or
   NULL), pointing to *N characters of space.  It is realloc'ed as
   necessary.  Returns the number of characters read (not including
   the null terminator), or -1 on error or EOF.  */

/* Ugly hack to get around MinGW not defining EOVERFLOW
 * Should be future proof though
 */

#ifndef EOVERFLOW
# define EOVERFLOW 132
#endif

ssize_t
getdelim (char **lineptr, size_t *n, int delimiter, FILE *fp)
{
  ssize_t result;
  size_t cur_len = 0;

  if (lineptr == NULL || n == NULL || fp == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  flockfile (fp);

  if (*lineptr == NULL || *n == 0)
    {
      char *new_lineptr;
      *n = 120;
      new_lineptr = (char *) g_realloc (*lineptr, *n);
      if (new_lineptr == NULL)
        {
          result = -1;
          goto unlock_return;
        }
      *lineptr = new_lineptr;
    }

  for (;;)
    {
      int i;

      i = getc_maybe_unlocked (fp);
      if (i == EOF)
        {
          result = -1;
          break;
        }

      /* Make enough space for len+1 (for final NUL) bytes.  */
      if (cur_len + 1 >= *n)
        {
          size_t needed_max =
            SSIZE_MAX < SIZE_MAX ? (size_t) SSIZE_MAX + 1 : SIZE_MAX;
          size_t needed = 2 * *n + 1;   /* Be generous. */
          char *new_lineptr;

          if (needed_max < needed)
            needed = needed_max;
          if (cur_len + 1 >= needed)
            {
              result = -1;
              errno = EOVERFLOW;
              goto unlock_return;
            }

          new_lineptr = (char *) g_realloc (*lineptr, needed);
          if (new_lineptr == NULL)
            {
              result = -1;
              goto unlock_return;
            }

          *lineptr = new_lineptr;
          *n = needed;
        }

      (*lineptr)[cur_len] = i;
      cur_len++;

      if (i == delimiter)
        break;
    }
  (*lineptr)[cur_len] = '\0';
  result = cur_len ? cur_len : result;

 unlock_return:
  funlockfile (fp); /* doesn't set errno */

  return result;
}
#endif

/* getline.c --- Implementation of replacement getline function.
   Copyright (C) 2005-2007, 2009-2013 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.  */

/* Written by Simon Josefsson. */

#ifndef HAVE_GETLINE
ssize_t
getline (char **lineptr, size_t *n, FILE *stream)
{
  return getdelim (lineptr, n, '\n', stream);
}
#endif


void xmi_free(void *ptr) {
	g_free(ptr);
}

void *xmi_malloc(size_t size) {
	return g_malloc(size);
}

void *xmi_realloc(void *ptr, size_t size){
	return g_realloc(ptr, size);
}


#if LIBXML_VERSION < 20901
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
//taken from the libxml2 source code...
int xmlXPathSetContextNode(xmlNodePtr node, xmlXPathContextPtr ctx) {
    if ((node == NULL) || (ctx == NULL))
        return(-1);

    if (node->doc == ctx->doc) {
        ctx->node = node;
        return(0);
    }
    return(-1);
}

xmlXPathObjectPtr xmlXPathNodeEval(xmlNodePtr node, const xmlChar *str, xmlXPathContextPtr ctx) {
    if (str == NULL)
        return(NULL);
    if (xmlXPathSetContextNode(node, ctx) < 0)
        return(NULL);
    return(xmlXPathEval(str, ctx));
}

#endif

static gboolean xmi_init_hdf5_done = FALSE;

void xmi_init_hdf5(void) {
	//this is not thread-safe but I don't expect problems

	if (xmi_init_hdf5_done)
		return;

	xmi_init_hdf5_done = TRUE;

	if (H5open() < 0) {
		g_fprintf(stderr, "Could not initialize HDF5!\n");
		exit(1);
	}

	//disable HDF5's error messages -> could be a bad idea!
	H5Eset_auto(H5E_DEFAULT, NULL, NULL);

	XMI_H5T_NATIVE_DOUBLE = H5T_NATIVE_DOUBLE;
	XMI_H5T_NATIVE_INT = H5T_NATIVE_INT;

}

struct copy_hdf5_data {
	int kind;
	hid_t file_from_id;
	hid_t file_to_id;
	char *file_from;
	char *file_to;
	char **groups;
	int force;
	struct xmi_input *temp_input;
	int ncopied;
};

static herr_t xmi_read_single_hdf5_group2(hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {
	struct copy_hdf5_data *chd = (struct copy_hdf5_data *) op_data;

	//read in this particular group
	hid_t group_id;
	hid_t dset_id, dspace_id;
	hsize_t dims_string[1];
	char *xmi_input_string;

	group_id = H5Gopen(g_id,name, H5P_DEFAULT);
	if (group_id < 0) {
		fprintf(stderr,"Error opening group %s\n",name);
		return -1;
	}

	//open xmi_input_string
	dset_id = H5Dopen(group_id, "xmi_input_string", H5P_DEFAULT);
	dspace_id = H5Dget_space(dset_id);
	H5Sget_simple_extent_dims(dspace_id, dims_string, NULL);
	xmi_input_string = (char *) g_malloc(sizeof(char)*dims_string[0]);
	H5Dread(dset_id, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, xmi_input_string);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);
	H5Gclose(group_id);

	struct xmi_input *temp_input;

	if (xmi_read_input_xml_from_string(xmi_input_string, &temp_input) == 0)
		return -1;

	herr_t rv = 0;
	if (chd->kind == XMI_HDF5_ESCAPE_RATIOS && xmi_check_escape_ratios_match(temp_input, chd->temp_input) == 1) {
		rv = 1;
	}
	else if (chd->kind == XMI_HDF5_SOLID_ANGLES && xmi_check_solid_angle_match(temp_input, chd->temp_input) == 1) {
		rv = 1;
	}
	xmi_free_input(temp_input);
	return rv;
}

static herr_t xmi_read_single_hdf5_group(hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {
	//we'll assume that based on previous checks the groups are actually groups!
	//first thing to is check if name is one of the allowed groups (if relevant)
	struct copy_hdf5_data *chd = (struct copy_hdf5_data *) op_data;
	int i;

	if (chd->groups != NULL) {
		int found = 0;
		for (i = 0 ; chd->groups[i] != NULL ; i++) {
			if (g_strcmp0(name, chd->groups[i]) == 0) {
				found = 1;
				break;
			}
		}
		if (found == 0) {
			//go to next one
			return 0;
		}
	}

	if (chd->force) {
		//forced copy
		//since this copy will have the most recent creation date, it should be picked up over older groups
		//consider deleting the old one with H5Ldelete
		if (H5Ocopy(g_id, name, chd->file_to_id, name, H5P_DEFAULT, H5P_DEFAULT) < 0) {
			g_fprintf(stderr,"Could not perform forced copy of %s from %s to %s\n", name, chd->file_from, chd->file_to);
			return -1;
		}
		else {
			chd->ncopied++;
			return 0;
		}
	}

	//read in this particular group
	hid_t group_id;
	hid_t dset_id, dspace_id;
	hsize_t dims_string[1];
	char *xmi_input_string;

	group_id = H5Gopen(g_id,name, H5P_DEFAULT);
	if (group_id < 0) {
		fprintf(stderr,"Error opening group %s\n",name);
		return -1;
	}

	//open xmi_input_string
	dset_id = H5Dopen(group_id, "xmi_input_string", H5P_DEFAULT);
	dspace_id = H5Dget_space(dset_id);
	H5Sget_simple_extent_dims(dspace_id, dims_string, NULL);
	xmi_input_string = (char *) g_malloc(sizeof(char)*dims_string[0]);
	H5Dread(dset_id, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, xmi_input_string);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);
	H5Gclose(group_id);

	struct xmi_input *temp_input;

	if (xmi_read_input_xml_from_string(xmi_input_string, &temp_input) == 0)
		return -1;

	chd->temp_input = temp_input;

	herr_t iterate_rv = H5Literate(chd->file_to_id, H5_INDEX_NAME, H5_ITER_INC, NULL, xmi_read_single_hdf5_group2, (void *) chd);

	xmi_free_input(temp_input);

	if (iterate_rv < 0)
		return -1;
	else if (iterate_rv == 0) {
		//no match found -> copy!
		if (H5Ocopy(g_id, name, chd->file_to_id, name, H5P_DEFAULT, H5P_DEFAULT) < 0) {
			g_fprintf(stderr,"Could not perform copy of %s from %s to %s\n", name, chd->file_from, chd->file_to);
			return -1;
		}
		chd->ncopied++;
	}

	return 0;
}

int xmi_copy_between_hdf5_files(int kind, char *file_from, char *file_to, char **groups, int force) {
	hid_t file_from_id;
	hid_t file_to_id;

	if (kind < XMI_HDF5_DATA || kind > XMI_HDF5_ESCAPE_RATIOS) {
		g_fprintf(stderr, "Invalid kind selected in xmi_copy_between_hdf5_files\n");
		return -1;
	}

	//for now XMI_HDF5_DATA is not available
	if (kind == XMI_HDF5_DATA) {
		return -1;
	}

	//open files
	file_from_id = H5Fopen(file_from, H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_from_id < 0) {
		g_fprintf(stderr,"Cannot open XMI-MSIM HDF5 file %s for reading\n", file_from);
		return -1;
	}
	//check if kind and version are correct
	hid_t attribute_id;
	attribute_id = H5Aopen(file_from_id, "kind", H5P_DEFAULT);
	if (attribute_id < 0) {
		g_fprintf(stderr,"XMI-MSIM HDF5 file %s does not contain the kind attribute\n", file_from);
		H5Fclose(file_from_id);
		return -1;
	}
	hid_t atype = H5Aget_type(attribute_id);
	hid_t atype_mem = H5Tget_native_type(atype, H5T_DIR_ASCEND);
	char xmi_kind[512];
	H5Aread(attribute_id, atype_mem, xmi_kind);
	H5Tclose(atype_mem);
	H5Tclose(atype);
	H5Aclose(attribute_id);

	if (kind == XMI_HDF5_SOLID_ANGLES && g_strcmp0(xmi_kind, "XMI_HDF5_SOLID_ANGLES") != 0) {
		g_fprintf(stderr,"XMI-MSIM HDF5 file %s has wrong kind attribute\n", file_from);
		g_fprintf(stderr, "Expected XMI_HDF5_SOLID_ANGLES but found %s\n", xmi_kind);
		H5Fclose(file_from_id);
		return -1;
	}
	else if (kind == XMI_HDF5_ESCAPE_RATIOS && g_strcmp0(xmi_kind, "XMI_HDF5_ESCAPE_RATIOS") != 0) {
		g_fprintf(stderr,"XMI-MSIM HDF5 file %s has wrong kind attribute\n", file_from);
		g_fprintf(stderr, "Expected XMI_HDF5_ESCAPE_RATIOS but found %s\n", xmi_kind);
		H5Fclose(file_from_id);
		return -1;
	}
	else if (kind == XMI_HDF5_DATA && g_strcmp0(xmi_kind, "XMI_HDF5_DATA") != 0) {
		g_fprintf(stderr,"XMI-MSIM HDF5 file %s has wrong kind attribute\n", file_from);
		g_fprintf(stderr, "Expected XMI_HDF5_DATA but found %s\n", xmi_kind);
		H5Fclose(file_from_id);
		return -1;
	}
	attribute_id = H5Aopen(file_from_id, "version", H5P_DEFAULT);
	if (attribute_id < 0) {
		g_fprintf(stderr,"XMI-MSIM HDF5 file %s does not contain the version attribute\n", file_from);
		H5Fclose(file_from_id);
		return -1;
	}
	double version;
	H5Aread(attribute_id, H5T_NATIVE_DOUBLE, &version);
	H5Aclose(attribute_id);

	if (kind == XMI_HDF5_SOLID_ANGLES && version < XMI_SOLID_ANGLES_MIN_VERSION) {
		g_fprintf(stderr, "Solid angles file %s is not compatible with this version of XMI-MSIM\n", file_from);
		H5Fclose(file_from_id);
		return -1;
	}
	else if (kind == XMI_HDF5_ESCAPE_RATIOS && version < XMI_ESCAPE_RATIOS_MIN_VERSION) {
		g_fprintf(stderr, "Escape ratios file %s is not compatible with this version of XMI-MSIM\n", file_from);
		H5Fclose(file_from_id);
		return -1;
	}
	else if (kind == XMI_HDF5_DATA && version < XMI_DATA_MIN_VERSION) {
		g_fprintf(stderr, "Data file %s is not compatible with this version of XMI-MSIM\n", file_from);
		H5Fclose(file_from_id);
		return -1;
	}
	//if we get here then file_from is ok to work with

	file_to_id = H5Fopen(file_to, H5F_ACC_RDWR, H5P_DEFAULT);
	if (file_to_id < 0) {
		//if it doesnt exist, try to create it
		g_fprintf(stderr,"Cannot open XMI-MSIM HDF5 file %s for writing\n", file_to);
		H5Fclose(file_from_id);
		return -1;
	}
	//check if kind and version are correct
	attribute_id = H5Aopen(file_to_id, "kind", H5P_DEFAULT);
	if (attribute_id < 0) {
		g_fprintf(stderr,"XMI-MSIM HDF5 file %s does not contain the kind attribute\n", file_to);
		H5Fclose(file_to_id);
		return -1;
	}
	atype = H5Aget_type(attribute_id);
	atype_mem = H5Tget_native_type(atype, H5T_DIR_ASCEND);
	H5Aread(attribute_id, atype_mem, xmi_kind);
	H5Tclose(atype_mem);
	H5Tclose(atype);
	H5Aclose(attribute_id);

	if (kind == XMI_HDF5_SOLID_ANGLES && g_strcmp0(xmi_kind, "XMI_HDF5_SOLID_ANGLES") != 0) {
		g_fprintf(stderr,"XMI-MSIM HDF5 file %s has wrong kind attribute\n", file_to);
		g_fprintf(stderr, "Expected XMI_HDF5_SOLID_ANGLES but found %s\n", xmi_kind);
		H5Fclose(file_to_id);
		return -1;
	}
	else if (kind == XMI_HDF5_ESCAPE_RATIOS && g_strcmp0(xmi_kind, "XMI_HDF5_ESCAPE_RATIOS") != 0) {
		g_fprintf(stderr,"XMI-MSIM HDF5 file %s has wrong kind attribute\n", file_to);
		g_fprintf(stderr, "Expected XMI_HDF5_ESCAPE_RATIOS but found %s\n", xmi_kind);
		H5Fclose(file_to_id);
		return -1;
	}
	else if (kind == XMI_HDF5_DATA && g_strcmp0(xmi_kind, "XMI_HDF5_DATA") != 0) {
		g_fprintf(stderr,"XMI-MSIM HDF5 file %s has wrong kind attribute\n", file_to);
		g_fprintf(stderr, "Expected XMI_HDF5_DATA but found %s\n", xmi_kind);
		H5Fclose(file_to_id);
		return -1;
	}
	attribute_id = H5Aopen(file_to_id, "version", H5P_DEFAULT);
	if (attribute_id < 0) {
		g_fprintf(stderr,"XMI-MSIM HDF5 file %s does not contain the version attribute\n", file_to);
		H5Fclose(file_to_id);
		return -1;
	}
	H5Aread(attribute_id, H5T_NATIVE_DOUBLE, &version);
	H5Aclose(attribute_id);

	if (kind == XMI_HDF5_SOLID_ANGLES && version < XMI_SOLID_ANGLES_MIN_VERSION) {
		g_fprintf(stderr, "Solid angles file %s is not compatible with this version of XMI-MSIM\n", file_to);
		H5Fclose(file_to_id);
		return -1;
	}
	else if (kind == XMI_HDF5_ESCAPE_RATIOS && version < XMI_ESCAPE_RATIOS_MIN_VERSION) {
		g_fprintf(stderr, "Escape ratios file %s is not compatible with this version of XMI-MSIM\n", file_to);
		H5Fclose(file_to_id);
		return -1;
	}
	else if (kind == XMI_HDF5_DATA && version < XMI_DATA_MIN_VERSION) {
		g_fprintf(stderr, "Data file %s is not compatible with this version of XMI-MSIM\n", file_to);
		H5Fclose(file_to_id);
		return -1;
	}

	struct copy_hdf5_data chd;
	chd.kind = kind;
	chd.file_from_id = file_from_id;
	chd.file_to_id = file_to_id;
	chd.file_from = file_from;
	chd.file_to = file_to;
	chd.groups = groups;
	chd.force = force;
	chd.ncopied = 0;

	herr_t iterate_rv = H5Literate(file_from_id, H5_INDEX_NAME, H5_ITER_INC, NULL, xmi_read_single_hdf5_group, (void *) &chd);

	//cleanup
	H5Fclose(file_to_id);
	H5Fclose(file_from_id);

	if (iterate_rv < 0)
		return -1;

	return chd.ncopied;
}

int xmi_get_hdf5_kind(char *name) {
	//open files
	hid_t file_id = H5Fopen(name, H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_id < 0) {
		return XMI_HDF5_INVALID;
	}
	//check if kind and version are correct
	hid_t attribute_id;
	attribute_id = H5Aopen(file_id, "kind", H5P_DEFAULT);
	if (attribute_id < 0) {
		H5Fclose(file_id);
		return XMI_HDF5_INVALID;
	}
	hid_t atype = H5Aget_type(attribute_id);
	hid_t atype_mem = H5Tget_native_type(atype, H5T_DIR_ASCEND);
	char xmi_kind[512];
	H5Aread(attribute_id, atype_mem, xmi_kind);
	H5Tclose(atype_mem);
	H5Tclose(atype);
	H5Aclose(attribute_id);

	int rv = XMI_HDF5_INVALID;

	if (g_strcmp0(xmi_kind, "XMI_HDF5_SOLID_ANGLES") == 0) {
		rv = XMI_HDF5_SOLID_ANGLES;
	}
	else if (g_strcmp0(xmi_kind, "XMI_HDF5_ESCAPE_RATIOS") == 0) {
		rv = XMI_HDF5_ESCAPE_RATIOS;
	}
	else if (g_strcmp0(xmi_kind, "XMI_HDF5_DATA") == 0) {
		rv = XMI_HDF5_DATA;
	}
	else {
		H5Fclose(file_id);
		return XMI_HDF5_INVALID;
	}
	attribute_id = H5Aopen(file_id, "version", H5P_DEFAULT);
	if (attribute_id < 0) {
		H5Fclose(file_id);
		return XMI_HDF5_INVALID;
	}
	double version;
	H5Aread(attribute_id, H5T_NATIVE_DOUBLE, &version);
	H5Aclose(attribute_id);

	if (rv == XMI_HDF5_SOLID_ANGLES && version < XMI_SOLID_ANGLES_MIN_VERSION) {
		H5Fclose(file_id);
		return XMI_HDF5_INVALID;
	}
	else if (rv == XMI_HDF5_ESCAPE_RATIOS && version < XMI_ESCAPE_RATIOS_MIN_VERSION) {
		H5Fclose(file_id);
		return XMI_HDF5_INVALID;
	}
	else if (rv == XMI_HDF5_DATA && version < XMI_DATA_MIN_VERSION) {
		H5Fclose(file_id);
		return XMI_HDF5_INVALID;
	}
	H5Fclose(file_id);
	return  rv;
}
int compare_string(const void *a, const void *b) {
   return g_strcmp0(*(char **)a, *(char **)b);
}
