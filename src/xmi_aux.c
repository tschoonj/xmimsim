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
#include <stdlib.h>
#include <string.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_permutation.h>
#include <gsl/gsl_linalg.h>
#include <xraylib-parser.h>




void *xmi_memdup(const void *mem, size_t bytes) {
	void *temp;

	if (mem == NULL || bytes == 0) {
#if DEBUG == 1
		fprintf(stdout,"Warning: xmi_memdup returns NULL\n");
#endif
		return NULL;
	}
	
	temp = (void*) malloc(bytes);
	if (temp == NULL) {
#if DEBUG == 1
		fprintf(stdout,"Warning: xmi_memdup returns NULL\n");
#endif
		return NULL;
	}
	memcpy(temp,mem,bytes);

	return temp;
}

int *xmi_sort_idl_int(int *array,int n_elements) {
	int *rv;
	int *array_copy;
	int i,j;
	int *res;

	if (n_elements < 1) {
		rv = (int *) malloc(sizeof(int));
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




	rv = (int *) malloc(sizeof(int)*n_elements);

	for (i = 0 ; i < n_elements ; i++) {
		for (j = 0 ; j < n_elements ; j++)
			if (array_copy[i] == array[j]) {
				rv[i] = j;
				break;
			}
	}

	free(array_copy);

	return rv;
} 



int xmi_cmp_int(const void *a, const void *b) {
	return *((int *) a) - *((int *) b);
}


void xmi_inverse_matrix(double x[3], double y[3], double z[3], double **inverseF) {

	gsl_matrix *m = gsl_matrix_alloc(3,3);
	gsl_matrix *inverse = gsl_matrix_alloc(3,3);
	gsl_permutation *p = gsl_permutation_calloc(3);
	int signum;
	double *rv;
	int i,j,k;


	gsl_matrix_set(m,0,0, x[0]);
	gsl_matrix_set(m,1,0, x[1]);
	gsl_matrix_set(m,2,0, x[2]);

	gsl_matrix_set(m,0,1, y[0]);
	gsl_matrix_set(m,1,1, y[1]);
	gsl_matrix_set(m,2,1, y[2]);

	gsl_matrix_set(m,0,2, z[0]);
	gsl_matrix_set(m,1,2, z[1]);
	gsl_matrix_set(m,2,2, z[2]);

#if DEBUG == 2
	fprintf(stdout,"input matrix\n");
	gsl_matrix_fprintf(stdout,m , "%g");
#endif

	//invert the sucker
	gsl_linalg_LU_decomp(m,p,&signum);
	gsl_linalg_LU_invert(m,p,inverse);
#if DEBUG == 2
	fprintf(stdout,"inverted matrix\n");
	gsl_matrix_fprintf(stdout,inverse , "%g");
#endif

	gsl_matrix_transpose(inverse);
#if DEBUG == 2
	fprintf(stdout,"transposed inverted matrix\n");
	gsl_matrix_fprintf(stdout,inverse , "%g");
#endif

	rv = (double *) malloc(9*sizeof(double));

	k = 0;

	for (i = 0 ; i < 3 ; i++)
		for (j = 0 ; j < 3 ; j++) 
			rv[k++] = gsl_matrix_get(inverse, i,j);

	gsl_matrix_free(m);
	gsl_matrix_free(inverse);
	gsl_permutation_free(p);

	*inverseF = rv;
}

void xmi_determinant_matrix(double x[3], double y[3], double z[3]) {
	gsl_matrix *m = gsl_matrix_alloc(3,3);
	gsl_permutation *p = gsl_permutation_calloc(3);
	int signum;
	int i,j,k;
	double det;
	gsl_matrix_set(m,0,0, x[0]);
	gsl_matrix_set(m,1,0, x[1]);
	gsl_matrix_set(m,2,0, x[2]);

	gsl_matrix_set(m,0,1, y[0]);
	gsl_matrix_set(m,1,1, y[1]);
	gsl_matrix_set(m,2,1, y[2]);

	gsl_matrix_set(m,0,2, z[0]);
	gsl_matrix_set(m,1,2, z[1]);
	gsl_matrix_set(m,2,2, z[2]);
	gsl_linalg_LU_decomp(m,p,&signum);
	det=gsl_linalg_LU_det(m,signum);

	fprintf(stdout,"Determinant is: %lf\n",det);
	return;

}
struct compoundData *xmi_layer2compoundData(struct xmi_layer *xl) {
	struct compoundData *rv;

	rv = (struct compoundData *) malloc(sizeof(struct compoundData));

	if (xl != NULL) {
		rv->nElements = xl->n_elements;
		rv->Elements = (int *) xmi_memdup(xl->Z, sizeof(int)*xl->n_elements);
		rv->massFractions= (double *) xmi_memdup(xl->weight, sizeof(double)*xl->n_elements);
	}
	else {
		rv->nElements = 0; 
	}
	return rv;
}

struct xmi_layer *compoundData2xmi_layer( struct compoundData *cd) {
	struct xmi_layer *rv;

	rv = (struct xmi_layer *) malloc(sizeof(struct xmi_layer));

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

	rv = (double *) malloc(sizeof(double)*n);
	
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
#include <gtk/gtk.h>
#include <gsl/gsl_version.h>
#include <hdf5.h>
#include <libxml/xmlversion.h>
#include <libxslt/xsltconfig.h>
#include <gtkextra/gtkextra.h>
#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
#include <curl/curl.h>
#include <json-glib/json-glib.h>
#endif

gchar *xmi_version_string() {
	gchar *string = g_malloc(sizeof(gchar)*5*1024);
	gchar *temp;

	temp = g_strdup_printf("XMI-MSIM %i.%i\n\n", XMI_MSIM_VERSION_MAJOR, XMI_MSIM_VERSION_MINOR);
	strcat(string,temp);
	g_free(temp);
	strcat(string,"Compiled with ");
	//xraylib
	temp = g_strdup_printf("xraylib %i.%i, ", XRAYLIB_MAJOR, XRAYLIB_MINOR);
	strcat(string,temp);
	g_free(temp);
	//glib
	temp = g_strdup_printf("glib %i.%i.%i, ", GLIB_MAJOR_VERSION, GLIB_MINOR_VERSION, GLIB_MICRO_VERSION);
	strcat(string,temp);
	g_free(temp);
	//gtk2
	temp = g_strdup_printf("gtk+ %i.%i.%i, ", GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
	strcat(string,temp);
	g_free(temp);
	//GSL
	temp = g_strdup_printf("gsl %i.%i, ", GSL_MAJOR_VERSION, GSL_MINOR_VERSION);
	strcat(string,temp);
	g_free(temp);
	//hdf5
	temp = g_strdup_printf("HDF5 %i.%i.%i, ", H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
	strcat(string,temp);
	g_free(temp);
	//libxml2
	temp = g_strdup_printf("libxml2 %s,\n", LIBXML_DOTTED_VERSION);
	strcat(string,temp);
	g_free(temp);
	//libxslt
	temp = g_strdup_printf("libxslt %s, ", LIBXSLT_DOTTED_VERSION);
	strcat(string,temp);
	g_free(temp);
	//fgsl
	temp = g_strdup_printf("fgsl 0.9.4, ");
	strcat(string,temp);
	g_free(temp);
	//gtkextra
	temp = g_strdup_printf("gtkextra %i.%i.%i", GTKEXTRA_MAJOR_VERSION, GTKEXTRA_MINOR_VERSION, GTKEXTRA_MICRO_VERSION);
	strcat(string,temp);
	g_free(temp);

#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
	temp = g_strdup_printf(", curl %i.%i.%i", LIBCURL_VERSION_MAJOR, LIBCURL_VERSION_MINOR, LIBCURL_VERSION_PATCH);
	strcat(string,temp);
	g_free(temp);
	temp = g_strdup_printf(", json-glib %i.%i.%i", JSON_MAJOR_VERSION, JSON_MINOR_VERSION, JSON_MICRO_VERSION);
	strcat(string,temp);
	g_free(temp);
#endif
	strcat(string,"\n\n");
	strcat(string,
"Copyright (C) 2010-2013 Tom Schoonjans and Laszlo Vincze\n"
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

	return string;
}


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
#include <stdlib.h>
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
      new_lineptr = (char *) realloc (*lineptr, *n);
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

          new_lineptr = (char *) realloc (*lineptr, needed);
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
