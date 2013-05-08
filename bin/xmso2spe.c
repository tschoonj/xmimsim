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

#include "xmi_xslt.h"
#include "xmi_xml.h"
#include <glib.h>
#include <stdio.h>
#include "xmi_aux.h"



XMI_MAIN
	GError *error = NULL;
        unsigned type=1;
	static int use_unconvoluted=0;
	static int interaction_number=1;
	static int version = 0;


	GOptionContext *context;
	static GOptionEntry entries[] = {
           	{ "unconvoluted", 'u', 0, G_OPTION_ARG_NONE, &use_unconvoluted, "Use unconvoluted data", NULL },
		{ "interaction-number", 'i', 0, G_OPTION_ARG_INT, &interaction_number, "Spectrum after n interactions"},
		{ "version", 0, 0, G_OPTION_ARG_NONE, &version, "display version information", NULL },
		{NULL}
	};

	//parse options
	context = g_option_context_new ("XMSO_file SPE_file");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmso2spe: a utility for the extraction of SPE files from an XMSO file\n");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		return 1;
	}

	if (version) {
		g_fprintf(stdout,"%s",xmi_version_string());	
		return 0;
	}

	if (argc < 3) {
		fprintf(stderr,"At least two arguments are required\n");
		fprintf(stderr,"%s",  g_option_context_get_help(context, FALSE, NULL ));
		return 1;
	}

	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		return 1;
	}

        if(use_unconvoluted == 1) type = 0;

        // type = 0 is convoluted, type = 1 is unconvoluted
	if (xmi_xmso_to_spe_xslt(argv[1], argv[2], type, interaction_number) == 0) {
		return 1;
	}


	return 0;
}
