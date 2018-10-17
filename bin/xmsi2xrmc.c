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

#include <config.h>
#include "xmi_private.h"
#include "xmi_msim.h"
#include <stdio.h>
#include <math.h>
#include <glib.h>

XMI_MAIN
	xmi_main_options *options = xmi_main_options_new();

	xmi_input *input;
	int version = 0;
	int rv;

	GOptionContext *context;
	gchar *input_file = NULL;
	gchar *source_file = NULL;
	gchar *sample_file = NULL;
	gchar *composition_file = NULL;
	gchar *spectrum_file = NULL;
	gchar *geom3d_file = NULL;
	gchar *quadric_file = NULL;
	gchar *detector_file = NULL;
	gchar *convoluted_file = NULL;
	gchar *unconvoluted_file = NULL;
	double rotate_angle_z = 0.0;

	GArray *entries = g_array_sized_new(TRUE, FALSE, sizeof(GOptionEntry), 16);
#define ADD_OPTION(long_name, short_name, flags, arg, arg_data, description, arg_description) \
	{ \
		GOptionEntry entry = {long_name, short_name, flags, arg, arg_data, description, arg_description}; \
		g_array_append_val(entries, entry); \
	}

	ADD_OPTION("input-file",0,0,G_OPTION_ARG_FILENAME,&input_file, "Path to XRMC input file", "FILE");
	ADD_OPTION("composition-file",0,0,G_OPTION_ARG_FILENAME,&composition_file, "Path to XRMC composition file", "FILE");
	ADD_OPTION("source-file",0,0,G_OPTION_ARG_FILENAME,&source_file, "Path to XRMC source file", "FILE");
	ADD_OPTION("sample-file",0,0,G_OPTION_ARG_FILENAME,&sample_file, "Path to XRMC sample file", "FILE");
	ADD_OPTION("spectrum-file",0,0,G_OPTION_ARG_FILENAME,&spectrum_file, "Path to XRMC spectrum file", "FILE");
	ADD_OPTION("geom3d-file",0,0,G_OPTION_ARG_FILENAME,&geom3d_file, "Path to XRMC geom3d file", "FILE");
	ADD_OPTION("quadric-file",0,0,G_OPTION_ARG_FILENAME,&quadric_file, "Path to XRMC quadric file", "FILE");
	ADD_OPTION("detector-file",0,0,G_OPTION_ARG_FILENAME,&detector_file, "Path to XRMC detector file", "FILE");
	ADD_OPTION("convoluted-file",0,0,G_OPTION_ARG_FILENAME,&convoluted_file, "Path to XRMC convoluted spectra output", "FILE");
	ADD_OPTION("unconvoluted-file",0,0,G_OPTION_ARG_FILENAME,&unconvoluted_file, "Path to XRMC unconvoluted spectra output", "FILE");
	ADD_OPTION("enable-poisson", 0, 0, G_OPTION_ARG_NONE, &options->use_poisson, "Generate Poisson noise in the spectra", NULL);
	ADD_OPTION("disable-poisson", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_poisson, "Disable the generating of spectral Poisson noise (default)", NULL );
	ADD_OPTION("enable-pile-up", 0, 0, G_OPTION_ARG_NONE, &options->use_sum_peaks, "Enable pile-up", NULL );
	ADD_OPTION("disable-pile-up", 0, G_OPTION_FLAG_REVERSE, G_OPTION_ARG_NONE, &options->use_sum_peaks, "Disable pile-up (default)", NULL );
	ADD_OPTION("rotate-angle-z", 0, 0, G_OPTION_ARG_DOUBLE, &rotate_angle_z, "Rotate all objects, source and detector around XRMC's Z-axis by ANGLE degrees", "ANGLE");
	ADD_OPTION("version", 0, 0, G_OPTION_ARG_NONE, &version, "Display version information", NULL );
	
	//parse options

	context = g_option_context_new ("XMSI_file");
	GError *error = NULL;
	g_option_context_add_main_entries(context, (const GOptionEntry *) entries->data, NULL);
	g_option_context_set_summary(context, "xmsi2xrmc: a utility for the conversion of an XMI-MSIM input-file to the corresponding XRMC input-files.\n");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		return 1;
	}

	if (version) {
		fprintf(stdout,"%s",xmi_version_string());
		return 0;
	}

	if (argc != 2) {
		fprintf(stderr,"%s\n",g_option_context_get_help(context, TRUE, NULL));
		return 1;
	}

	g_option_context_free(context);

	g_array_free(entries, TRUE);

	//ok time, to fill up the paths with defaults
	gchar *cwd = g_get_current_dir();
	if (!input_file)
		input_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", cwd, "input.dat");

	if (!source_file)
		source_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", cwd, "source.dat");

	if (!sample_file)
		sample_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", cwd, "sample.dat");

	if (!composition_file)
		composition_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", cwd, "composition.dat");

	if (!spectrum_file)
		spectrum_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", cwd, "spectrum.dat");

	if (!geom3d_file)
		geom3d_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", cwd, "geom3d.dat");

	if (!quadric_file)
		quadric_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", cwd, "quadric.dat");

	if (!detector_file)
		detector_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", cwd, "detector.dat");

	if (!convoluted_file)
		convoluted_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", cwd, "convoluted_spectra.dat");


	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		return 1;
	}

	//read in the inputfile
	rv = xmi_read_input_xml(argv[1],&input, NULL);

	if (rv != 1) {
		return 1;
	}

	if (xmi_copy_input_to_xrmc(input,
				input_file,
				composition_file,
				detector_file,
				geom3d_file,
				quadric_file,
				sample_file,
				source_file,
				spectrum_file,
				convoluted_file,
				unconvoluted_file,
				/*xmi_layer *collimator*/ NULL,
				options,
				rotate_angle_z
				) == 0)
		return 1;

	return 0;
}
