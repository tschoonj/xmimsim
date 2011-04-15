#include "xmi_xslt.h"
#include <glib.h>
#include <stdio.h>



int main(int argc, char *argv[]) {

	static gchar *outputfile=NULL;
	GError *error = NULL;


	GOptionContext *context;
	static GOptionEntry entries[] = {
		{"outputfile",'o',0,G_OPTION_ARG_FILENAME,&outputfile,"XMSI outputfile",NULL},
		{NULL}
	};


	//parse options
	context = g_option_context_new ("XMSO_file XMSI_file");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmso2xmsi: a utility for the extraction of the XMSI inputfile from an XMSO file\n");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		return 1;
	}


	if (argc != 3) {
		fprintf(stderr,"Two arguments are required\n");
		fprintf(stderr,"%s",  g_option_context_get_help(context, FALSE, NULL ));
		return 1;
	}

	if (xmi_xmso_to_xmsi_xslt(argv[1], argv[2], outputfile) == 0) {
		return 1;
	}


	return 0;
}
