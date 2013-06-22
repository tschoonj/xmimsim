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

#ifndef XMI_AUX_X
#define XMI_AUX_X

#include <config.h>
#include <stddef.h>
#include "xmi_data_structs.h"
#include <glib.h>
#include <stdio.h>
#include <xraylib.h>



#ifndef HAVE_GETLINE
ssize_t getline (char **lineptr, size_t *n, FILE *stream);
#endif



//returns NULL on error
void *xmi_memdup(const void *mem, size_t bytes);

//returns an array with at least one element. If an error occurred, this element will have a value of -1
//works similar to the IDL function carrying the same name... the array that is returned contains indices!!
int *xmi_sort_idl_int(int *array,int n_elements);

//inverse will be column-major!! intended to be used from fortran only
void xmi_inverse_matrix(double x[3], double y[3], double z[3], double **inverse);

//calculates the sum of array containing n elements
double xmi_sum_double(double *array, int n); 

//multiplies each element of array with scale_factor
void xmi_scale_double(double *array, int n, double scale_factor);


//calculates the norm of the vector array
double xmi_norm_double(double *array, int n);

//returns the maximum value of the vector array
double xmi_maxval_double(double *array, int n);

//returns the location of the maximum in vector array
int xmi_maxloc_double(double *array, int n);

//normalizes the vector array
void xmi_normalize_vector_double(double *array, int n);

//returns an IDL style dindgen array
double *xmi_dindgen(int n);

//returns minimum value in array
double xmi_minval_double(double *array, int n);

//increase array with value (can also be negative of course)
void xmi_add_val_to_array_double(double *array, int n, double increment);


//to be used in qsort or bsearch for comparing integers
int xmi_cmp_int(const void *a, const void *b);

struct compoundData *xmi_layer2compoundData(struct xmi_layer *xl);

struct xmi_layer *compoundData2xmi_layer( struct compoundData *cd);
struct xmi_layer *compoundDataNIST2xmi_layer( struct compoundDataNIST *cd);

int xmi_cmp_struct_xmi_energy_discrete(const void *a, const void *b);
int xmi_cmp_struct_xmi_energy_continuous(const void *a, const void *b);

#ifdef G_OS_WIN32
  #include <windows.h>
  #define XMI_ARGC_ORIG argc_orig
  #define XMI_ARGV_ORIG argv_orig
  #define XMI_ARGC argc
  #define XMI_ARGV argv
  #define XMI_MAIN int main(int argc_orig, char *argv_orig[]) {\
	int argc;\
	char **argv;\
	int argc_counter;\
	LPWSTR WinCommandLine = GetCommandLineW();\
	gunichar2 **WinArgv = CommandLineToArgvW(WinCommandLine,&argc);\
	argv = (char **) g_malloc(sizeof(char *)*argc);\
	for (argc_counter = 0 ; argc_counter < argc ; argc_counter++)\
	  	argv[argc_counter] = g_utf16_to_utf8(WinArgv[argc_counter],-1, NULL, NULL, NULL);\
	LocalFree(WinArgv);
	
#else
  #define XMI_ARGC_ORIG argc
  #define XMI_ARGV_ORIG argv
  #define XMI_ARGC argc
  #define XMI_ARGV argv
  #define XMI_MAIN int main(int argc, char *argv[]) {

#endif


//following defines and typedefs are required in order to support gtk-mac-integration version 2.0+
#ifdef HAVE_GTK_MAC_INTEGRATION_2

#define GTK_TYPE_OSX_APPLICATION GTKOSX_TYPE_APPLICATION
#define GTK_OSX_APPLICATION GTKOSX_APPLICATION
#define GTK_IS_OSX_APPLICATION GTKOSX_IS_APPLICATION
#define GTK_OSX_APPLICATION_CLASS GTKOSX_APPLICATION_CLASS
#define GTK_IS_OSX_APPLICATION_CLASS GTKOSX_IS_APPLICATION_CLASS
#define GTK_OSX_APPLICATION_GET_CLASS GTKOSX_APPLICATION_GET_CLASS

#define GtkOSXApplication GtkosxApplication
#define GtkOSXApplicationPrivate GtkosxApplicationPrivate
#define GtkOSXApplicationClass GtkosxApplicationClass
#define GtkOSXApplicationMenuGroup GtkosxApplicationMenuGroup

#define gtk_osxapplication_get_type gtkosx_application_get_type
#define gtk_osxapplication_ready gtkosx_application_ready

#define gtk_osxapplication_set_use_quartz_accelerators gtkosx_application_set_use_quartz_accelerators
#define gtk_osxapplication_use_quartz_accelerators gtkosx_application_use_quartz_accelerators

#define gtk_osxapplication_set_menu_bar gtkosx_application_set_menu_bar
#define gtk_osxapplication_sync_menubar gtkosx_application_sync_menubar
#define gtk_osxapplication_insert_app_menu_item gtkosx_application_insert_app_menu_item
#define gtk_osxapplication_set_window_menu gtkosx_application_set_window_menu
#define gtk_osxapplication_set_help_menu gtkosx_application_set_help_menu

#define gtk_osxapplication_set_dock_menu gtkosx_application_set_dock_menu
#define gtk_osxapplication_set_dock_icon_pixbuf gtkosx_application_set_dock_icon_pixbuf
#define gtk_osxapplication_set_dock_icon_resource gtkosx_application_set_dock_icon_resource

#define gtk_osxapplication_attention_request gtkosx_application_attention_request
#define gtk_osxapplication_cancel_attention_request gtkosx_application_cancel_attention_request

#define quartz_application_get_bundle_path gtkosx_application_get_bundle_path
#define quartz_application_get_resource_path gtkosx_application_get_resource_path
#define quartz_application_get_executable_path gtkosx_application_get_executable_path
#define quartz_application_get_bundle_id gtkosx_application_get_bundle_id
#define quartz_application_get_bundle_info gtkosx_application_get_bundle_info

#endif

gchar *xmi_version_string();

#endif
