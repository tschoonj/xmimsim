/*
Copyright (C) 2010-2018 Tom Schoonjans and Laszlo Vincze

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


#ifndef XMI_PRIVATE_H
#define XMI_PRIVATE_H

#include <config.h>
#include <glib.h>
#include <stdint.h>
#ifndef __GI_SCANNER__
#if defined(HAVE_JSONGLIB) || defined(HAVE_GOOGLE_ANALYTICS)
#include <libsoup/soup.h>
#endif
#endif
#include "xmi_solid_angle.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _xmi_history_element_line xmi_history_element_line;
/**
 * xmi_history_element_line:
 * @energy: line energy, expressed in keV.
 * @total_counts: total number of counts for this line, for all interactions.
 * @interactions: (element-type double): array containing counts per interaction for this line.
 */
struct _xmi_history_element_line {
	//char *line_type;
	double energy;
	double total_counts;
	//int n_interactions;
	//xmi_counts *interactions;
	GArray *interactions;
	gint ref_count;
};

typedef struct _xmi_history_element xmi_history_element;
/**
 * xmi_history_element:
 * @total_counts: total number of counts for this element, for all lines, for all interactions.
 * @lines: (element-type utf8 XmiMsim.HistoryElementLine): hash containing line specific information.
 */
struct _xmi_history_element {
	//int atomic_number;
	double total_counts;
	//int n_lines;
	GHashTable *lines;
	gint ref_count;
};

void xmi_history_element_line_free(xmi_history_element_line *line);
void xmi_history_element_free(xmi_history_element *element);

#ifndef __GI_SCANNER__

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
	LPWSTR *WinArgv = CommandLineToArgvW(WinCommandLine,&argc);\
	argv = (char **) g_malloc(sizeof(char *)*argc);\
	for (argc_counter = 0 ; argc_counter < argc ; argc_counter++) {\
		int size_needed = WideCharToMultiByte(CP_UTF8, 0, WinArgv[argc_counter], -1, NULL, 0, NULL, NULL); \
		char *argvSingle = (char *) g_malloc(sizeof(char) * size_needed); \
		WideCharToMultiByte(CP_UTF8, 0 , WinArgv[argc_counter], -1, argvSingle, size_needed, NULL, NULL); \
		argv[argc_counter] = argvSingle; \
	} \
	LocalFree(WinArgv);

#else
  #define XMI_ARGC_ORIG argc
  #define XMI_ARGV_ORIG argv
  #define XMI_ARGC argc
  #define XMI_ARGV argv
  #define XMI_MAIN int main(int argc, char *argv[]) {

#endif

extern int64_t XMI_H5T_NATIVE_DOUBLE;
extern int64_t XMI_H5T_NATIVE_INT;

#endif

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
int xmi_solid_angle_calculation_cl(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *xmo);
#endif

#if defined(HAVE_METAL)
int xmi_solid_angle_calculation_metal(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *xmo);
#endif

int xmi_solid_angle_calculation_f(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *xmo);

#ifndef __GI_SCANNER__
#if defined(HAVE_JSONGLIB) || defined(HAVE_GOOGLE_ANALYTICS)
SoupSession* xmi_soup_session_new(const gchar *user_agent);
#endif
#endif

#ifdef __cplusplus
}
#endif

#endif
