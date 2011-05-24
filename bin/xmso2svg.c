#include "xmi_xslt.h"
#include <glib.h>
#include <stdio.h>



int main(int argc, char *argv[]) {

	GError *error = NULL;
        unsigned type;
	static int use_unconvoluted;


	GOptionContext *context;
	static GOptionEntry entries[] = {
           	{ "u", 0, 0, G_OPTION_ARG_NONE, &(use_unconvoluted), "Create unconvoluted graphs", NULL },
		{NULL}
	};


	//parse options
	context = g_option_context_new ("XMSO_file SVG_file");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmso2svg: a utility for the extraction of the SVG file from an XMSO file\n");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		return 1;
	}


	if (argc < 3) {
		fprintf(stderr,"At least two arguments are required\n");
		fprintf(stderr,"%s",  g_option_context_get_help(context, FALSE, NULL ));
		return 1;
	}


        if (argv[3] != NULL) type = atoi(argv[3]); 
 	else type = 0;

 	//fprintf(stdout,"use_unconvoluted: %i\n",use_unconvoluted);
        
        if(use_unconvoluted == 1) type = 1;

        // type = 0 is convoluted, type = 1 is unconvoluted
	if (xmi_xmso_to_svg_xslt(argv[1], argv[2], type) == 0) {
		return 1;
	}


	return 0;
}
