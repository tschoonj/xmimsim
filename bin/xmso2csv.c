/*
Copyright (C) 2010-2017 Tom Schoonjans, Laszlo Vincze

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

#include "xmi_xslt.h"
#include "xmi_xml.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <stdio.h>
#include "xmi_aux.h"
#include "xmi_private.h"




int main(int argc, char **argv) {
	GError *error = NULL;
    unsigned type=1;
	static int use_unconvoluted=0;
	static int version = 0;
	static gchar **filenames = NULL;


	GOptionContext *context;
	static GOptionEntry entries[] = {
       	{"unconvoluted", 'u', 0, G_OPTION_ARG_NONE, &(use_unconvoluted), "Use unconvoluted data", NULL},
		{"version", 0, 0, G_OPTION_ARG_NONE, &version, "Display version information", NULL},
		{G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &filenames, "", NULL},
		{NULL}
	};

#ifdef G_OS_WIN32
	argv = g_win32_get_command_line();
#else
	argv = g_strdupv(argv);
#endif

	//parse options
	context = g_option_context_new("XMSO_file CSV_file");
	g_option_context_add_main_entries(context, entries, NULL);
	g_option_context_set_summary(context, "xmso2csv: a utility for the conversion of the spectral data contained within an XMSO file to a CSV file\n");
	if (!g_option_context_parse_strv(context, &argv, &error)) {
		g_print("option parsing failed: %s\n", error->message);
		return 1;
	}

	g_strfreev(argv);

	if (version) {
		g_fprintf(stdout, "%s", xmi_version_string());
		return 0;
	}


	if (filenames == NULL || g_strv_length(filenames) != 2) {
		g_fprintf(stderr, "At least two arguments are required\n");
		g_fprintf(stderr, "%s", g_option_context_get_help(context, FALSE, NULL ));
		return 1;
	}


	//load xml catalog
	if (xmi_xmlLoadCatalog(&error) == 0) {
		fprintf(stderr, "Could not load XML catalog: %s\n", error->message);
		return 1;
	}

    if(use_unconvoluted == 1)
		type = 0;

    // type = 0 is convoluted, type = 1 is unconvoluted
	if (xmi_xmso_to_csv_xslt(filenames[0], filenames[1], type) == 0) {
		return 1;
	}


	return 0;
}

