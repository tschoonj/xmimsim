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

#include "xmi_data.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <stdlib.h>
#include <glib/gstdio.h>

int main(int argc, char **argv) {
	int rv;
	GOptionContext *context;
	static gchar **filenames = NULL;
	static int version = 0;
	static GOptionEntry entries[] = {
		{"version", 0, 0, G_OPTION_ARG_NONE, &version, "Display version information", NULL},
		{G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &filenames, "", NULL},
		NULL
	};

#ifdef G_OS_WIN32
	argv = g_win32_get_command_line();
#else
	argv = g_strdupv(argv);
#endif

	context = g_option_context_new("[data file]");
	g_option_context_add_main_entries(context, entries, NULL);
	g_option_context_set_summary(context, "xmimsim-db: generates the database necessary to run XMI-MSIM simulations");

	GError *error = NULL;
	if (!g_option_context_parse_strv(context, &argv, &error)) {
		g_fprintf(stderr, "option parsing failed: %s\n", error->message);
		return 1;
	}

	g_strfreev(argv);

	if (version) {
		g_fprintf(stdout, "%s", xmi_version_string());
		return 0;
	}

	xmi_init_hdf5();

	const int nZ = 94;
	int *Zs = g_malloc(sizeof(int) * nZ);
	int i;
	for (i = 0 ; i < nZ ; i++)
		Zs[i] = i + 1;

	if (filenames == NULL || g_strv_length(filenames) == 0) {
		rv = xmi_db("xmimsimdata.h5", Zs, nZ);
	}
	else {
		rv = xmi_db(filenames[0], Zs, nZ);
	}

	if (rv == 1)
		rv = 0;
	else if (rv == 0)
		rv = 1;

	g_free(Zs);

	return rv;
}
