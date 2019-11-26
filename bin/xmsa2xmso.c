/*
Copyright (C) 2015-2017 Tom Schoonjans and Laszlo Vincze

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
	static int version = 0;
	static int step1 = 0, step2 = 0;
	static gboolean all = FALSE;
	static gchar **filenames = NULL;

	GOptionContext *context;
	static GOptionEntry entries[] = {
		{"step1"  , '1', 0, G_OPTION_ARG_INT,  &step1,   "Extract data for parameter 1 after N1 steps",        "N1"},
		{"step2"  , '2', 0, G_OPTION_ARG_INT,  &step2,   "Extract data for parameter 2 after N2 steps",        "N2"},
		{"all"    , 'a', 0, G_OPTION_ARG_NONE, &all,     "Extract all data. XMSO_file will be used as prefix", NULL},
		{"version",  0,  0, G_OPTION_ARG_NONE, &version, "Display version information",                        NULL },
		{G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &filenames, "", NULL},
		{NULL}
	};

#ifdef G_OS_WIN32
	argv = g_win32_get_command_line();
#else
	argv = g_strdupv(argv);
#endif

	//parse options
	context = g_option_context_new("XMSA_file XMSO_file");
	g_option_context_add_main_entries(context, entries, NULL);
	g_option_context_set_summary(context, "xmsa2xmso: a utility for the extraction of XMSO output-files from an XMSA archive file\n\nUse either the all option or the step1 and step2 options to select which simulation(s) are required. Default: step1 = step2 = 0!");
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
		g_fprintf(stderr,"Two arguments are required\n");
		g_fprintf(stderr,"%s", g_option_context_get_help(context, FALSE, NULL));
		return 1;
	}

	g_option_context_free(context);

	if (step1 < 0 || step2 < 0) {
		g_fprintf(stderr, "step1 and step2 must be greater or equal to zero\n");
		return 1;
	}

	if (all) {
		step1 = step2 = -1;
	}

	//load xml catalog
	if (xmi_xmlLoadCatalog(&error) == 0) {
		g_fprintf(stderr, "Could not load XML catalog: %s\n", error->message);
		return 1;
	}

	if (xmi_xmsa_to_xmso_xslt(filenames[0], filenames[1], step1, step2) == 0) {
		return 1;
	}

	return 0;
}

