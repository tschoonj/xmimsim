/*
Copyright (C) 2015 Tom Schoonjans and Laszlo Vincze

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

#include <xmi_config.h>
#include <math.h>
#include <unistd.h>

#ifndef XMI_MSIM_HAVE_LIBSOUP
#error "libsoup is required to run the testsuite!"
#endif

#include "libxmimsim-test.h"
#include <libsoup/soup.h>
#include <stdio.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <stdlib.h>
#include <assert.h>
#include <libxml/xpath.h>

#if !GLIB_CHECK_VERSION (2, 35, 3)
#include <glib-object.h>
#endif

int test_init (void) {
	LIBXML_TEST_VERSION

	xmlInitParser();

	// load our xml catalog
	assert(xmi_xmlLoadCatalog(NULL));

	return 1;
}

int test_download_file(char *url) {
	FILE *fp;
	int rv = 0;

	GFile *url_file = g_file_new_for_uri(url);
	gchar *outputfile = g_file_get_basename(url_file);

	// check if this file is present in the xmimsim.wiki git repo locally first
	gchar *local_outputfilename = g_build_filename(ABS_TOP_SRCDIR, "..", "xmimsim.wiki", outputfile, NULL);
	g_debug("local_outputfilename: %s", local_outputfilename);
	GFile *local_outputfile = g_file_new_for_path(local_outputfilename);
	g_free(local_outputfilename);
	GFile *local_outputfile_copy = g_file_new_for_path(outputfile);

	if (!g_file_copy(local_outputfile, local_outputfile_copy, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, NULL)) {
		g_debug("local copying failed!");
		SoupSession *session = soup_session_new();
		SoupMessage *msg = soup_message_new ("GET", url);
		soup_session_send_message (session, msg); // blocking!

		if (SOUP_STATUS_IS_SUCCESSFUL(msg->status_code)) {
			fp = g_fopen(outputfile, "wb");
			if (!fp) {
				g_warning("Could not open %s for writing!", outputfile);
			}
			else {
				fwrite (msg->response_body->data,
					1,
					msg->response_body->length,
					fp);
				fclose (fp);
				rv = 1;
			}
		}
		else {
			g_warning("libsoup error message: %s", msg->reason_phrase);
		}

		g_object_unref(msg);
		g_object_unref(session);
	}
	else {
		rv = 1;
	}

	g_free(outputfile);
	g_object_unref(url_file);
	g_object_unref(local_outputfile);
	g_object_unref(local_outputfile_copy);
	return rv;
}

struct spe_data * read_spe(const char *filename) {
	FILE *spePtr = NULL;
	struct spe_data *rv = NULL;
	char buffer[1024];

	spePtr = g_fopen(filename, "r");
	if (spePtr == NULL) {
		fprintf(stderr, "Could not open %s for reading\n", filename);
		return NULL;
	}

	rv = g_malloc(sizeof(struct spe_data));

	do {
		if (fgets(buffer, 1024, spePtr) == NULL) {
			fprintf(stderr,"An error occurred while reading %s...\nAre you sure this is an SPE file?\n",filename);
			return NULL;
		}
	} while (strstr(buffer, "$DATA:") == NULL);

	fscanf(spePtr,"%u %u",&(rv->channel_first),&(rv->channel_last));

	rv->nchannels = rv->channel_last - rv->channel_first + 1;

	rv->data = g_malloc(sizeof(double) * rv->nchannels);

	unsigned int i;

	for (i = 0 ; i < rv->nchannels ; i++) {
		if (fscanf(spePtr, "%lf", rv->data + i) != 1) {
			fprintf(stderr,"Error reading DATA segment of %s at position %i\n",filename,i);
			return NULL;
		}
	}

	fclose(spePtr);

	return rv;
}

void free_spe_data(struct spe_data *sd) {
	g_assert(sd != NULL);
	g_assert(sd->data != NULL);
	g_free(sd->data);
	g_free(sd);

	return;
}

extern long hits_per_single;

double CS_Total_Layer(xmi_layer *layer, double E) {
	int i;
	double rv = 0.0;
	for (i = 0 ; i < layer->n_elements ; i++) {
		rv += layer->weight[i] * CS_Total(layer->Z[i], E, NULL);
	}
	return rv;
}

double chi(double E0, double E1, xmi_layer *layer, double alpha, double beta) {
	double mu0 = CS_Total_Layer(layer, E0);
	double mu1 = CS_Total_Layer(layer, E1);
	return mu0/sin(alpha) + mu1/sin(beta);
}

double fpm(xmi_layer *layer, int Z, int line, double w, double E0, double I0, double D, double Adet, double alpha, double beta) {
	double rv = I0;
	double theta = atan(sqrt(Adet/M_PI)/D);
	double Omega = 2.0 * M_PI * (1.0 - cos(theta));
	double G = Omega / 4.0 / M_PI / sin(alpha);
	rv *= G;
	rv *= w;
	rv *= CS_FluorLine_Kissel(Z, line, E0, NULL);
	double my_chi = chi(E0, LineEnergy(Z, line, NULL), layer, alpha, beta);
	rv *= (1.0 - exp(-1.0 * my_chi * layer->density * layer->thickness)) / my_chi;

	return rv;
}

xmi_output* run_main(const char *compound) {
	xmi_solid_angle *solid_angle_def = NULL;
	double *channels;
	double *brute_history;
	double *var_red_history;
	int i;
	xmi_main_options *options = xmi_main_options_new();
	options->use_escape_peaks = 0;
	options->verbose = 1;

	// init test
	g_assert(test_init() == 1);
	
	xmi_init_hdf5();

	// read compound
	struct compoundData *cd = CompoundParser(compound, NULL);
	g_assert(cd != NULL);

	// generate appropriate xmimsimdata.h5 file
	gchar *data_file = g_strdup_printf("xmimsimdata-%s.h5", compound);
	//if (!g_file_test(data_file, G_FILE_TEST_EXISTS))
		g_assert(xmi_db(data_file, cd->Elements, cd->nElements));

	// get default input-file
	xmi_input *input = xmi_input_init_empty();
	// simulate 1M photons
	input->general->n_photons_line = 1000000;

	// calculate first interaction only
	input->general->n_interactions_trajectory = 1;

	// add compound to composition
	xmi_layer *layer = compoundData2xmi_layer(cd);
	layer->thickness = 1.0;
	layer->density = 1.0;
	input->composition->n_layers = 1;
	input->composition->layers = layer;
	input->composition->reference_layer = 1;
	
	// increase sample-detector distance to have more reliable fundamental parameter calculation
	input->geometry->p_detector_window[1] = 10.0;

	// decrease energy to 10.0 keV
	input->excitation->discrete[0].energy = 10.0;

	// copy input to fortran variable
	xmi_inputFPtr inputFPtr;
	xmi_input_C2F(input, &inputFPtr);
	
	// initialization
	g_assert(xmi_init_input(&inputFPtr) == 1);
	
	// read data file
	xmi_hdf5FPtr hdf5FPtr;
	g_assert(xmi_init_from_hdf5(data_file, inputFPtr, &hdf5FPtr, options) == 1);
	xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);

	// start random number acquisition
	g_assert(xmi_start_random_acquisition() == 1);

	// reduce hits_per_single to shorten calculation time
	hits_per_single = 1000;

	// run the actual calculation
	xmi_solid_angle_calculation(inputFPtr, &solid_angle_def, NULL, options);

	// check solid_angle_def is not NULL
	g_assert(solid_angle_def != NULL);
	
	// run simulation
	g_assert(xmi_main_msim(inputFPtr, hdf5FPtr, 1, &channels, options, &brute_history, &var_red_history, solid_angle_def) == 1);

	// apply detector response function
	double **channels_conv = (double **) g_malloc(sizeof(double *)*(input->general->n_interactions_trajectory+1));
	double **channels_def_ptrs = g_malloc(sizeof(double *) * (input->general->n_interactions_trajectory+1));
	for (i = 0 ; i <= input->general->n_interactions_trajectory ; i++)
		channels_def_ptrs[i] = channels+i*input->detector->nchannels;
	
	xmi_detector_convolute_all(inputFPtr, channels_def_ptrs, channels_conv, brute_history, var_red_history, options, NULL, input->general->n_interactions_trajectory, 0);

	// convert to xmi_output
	xmi_output *output = xmi_output_raw2struct(input, brute_history, var_red_history, channels_conv, channels, "test.xmso", 0);

	// cleanup
	for (i = 1 ; i <= input->general->n_interactions_trajectory ; i++)
		g_free(channels_conv[i] );
	g_free(channels_conv);
	g_free(channels);
	g_free(channels_def_ptrs);

	// check solid_angle_def is still not NULL
	g_assert(solid_angle_def != NULL);
	
	xmi_free_solid_angle(solid_angle_def);
	g_unlink(data_file);
	g_free(data_file);
	FreeCompoundData(cd);
	xmi_input_free(input);
	xmi_free_input_F(&inputFPtr);

	g_assert(xmi_end_random_acquisition() == 1);
	return output;
}

int replace_xml_tag(const char *filename_old, const char *filename_new,  const char *xpath_expression, const char *new_value) {

	xmlDocPtr doc;
	xmlParserCtxtPtr ctx;
	xmlNodePtr root;

	LIBXML_TEST_VERSION

	if ((ctx = xmlNewParserCtxt()) == NULL) {
		return 0;
	}

	if ((doc = xmlCtxtReadFile(ctx, filename_old, NULL, XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		xmlFreeParserCtxt(ctx);
		return 0;
	}

	if (ctx->valid == 0) {
		xmlFreeDoc(doc);
		return 0;
	}
	xmlFreeParserCtxt(ctx);

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		xmlFreeDoc(doc);
		return 0;
	}

	xmlXPathContextPtr xpathCtx;
	xmlXPathObjectPtr xpathObj;
	xpathCtx = xmlXPathNewContext(doc);
	if(xpathCtx == NULL) {
		xmlFreeDoc(doc);
        	return 0;
	}
	xpathObj = xmlXPathNodeEval(root, BAD_CAST xpath_expression, xpathCtx);
	if(xpathObj == NULL || xpathObj->nodesetval->nodeNr == 0) {
		xmlXPathFreeContext(xpathCtx);
		xmlFreeDoc(doc);
		return 0;
	}

	xmlNodePtr node = xpathObj->nodesetval->nodeTab[0];
	
	xmlXPathFreeObject(xpathObj);
	xmlXPathFreeContext(xpathCtx);

	xmlNodeSetContent(node, BAD_CAST new_value);

	xmlThrDefIndentTreeOutput(2);

	if (xmlSaveFormatFileEnc(filename_new, doc, NULL, 1) == -1) {
		return 0;
	}
	xmlFreeDoc(doc);
	return 1;
}

int remove_xml_tags(const char *filename_old, const char *filename_new,  const char *xpath_expression) {

	xmlDocPtr doc;
	xmlParserCtxtPtr ctx;
	xmlNodePtr root;

	LIBXML_TEST_VERSION

	if ((ctx = xmlNewParserCtxt()) == NULL) {
		return 0;
	}

	if ((doc = xmlCtxtReadFile(ctx, filename_old, NULL, XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		xmlFreeParserCtxt(ctx);
		return 0;
	}

	if (ctx->valid == 0) {
		xmlFreeDoc(doc);
		return 0;
	}
	xmlFreeParserCtxt(ctx);

	if ((root = xmlDocGetRootElement(doc)) == NULL) {
		xmlFreeDoc(doc);
		return 0;
	}

	xmlXPathContextPtr xpathCtx;
	xmlXPathObjectPtr xpathObj;
	xpathCtx = xmlXPathNewContext(doc);
	if(xpathCtx == NULL) {
		xmlFreeDoc(doc);
        	return 0;
	}
	xpathObj = xmlXPathNodeEval(root, BAD_CAST xpath_expression, xpathCtx);
	if(xpathObj == NULL || xpathObj->nodesetval->nodeNr == 0) {
		xmlXPathFreeContext(xpathCtx);
		xmlFreeDoc(doc);
		return 0;
	}

	int i;
	for (i = 0 ; i < xpathObj->nodesetval->nodeNr ; i++) {
		xmlNodePtr node = xpathObj->nodesetval->nodeTab[i];
		xmlUnlinkNode(node);	
	}
	
	xmlXPathFreeObject(xpathObj);
	xmlXPathFreeContext(xpathCtx);

	xmlThrDefIndentTreeOutput(2);

	if (xmlSaveFormatFileEnc(filename_new, doc, NULL, 1) == -1) {
		return 0;
	}
	xmlFreeDoc(doc);
	return 1;
}

gboolean test_log_fatal_false(const gchar *log_domain, GLogLevelFlags log_level, const gchar *message, gpointer user_data) {
	return FALSE;
}

void test_compare_channels_and_csv(int nchannels, double **channels, const gchar *csv_file) {

	GFile *file = g_file_new_for_path(csv_file);
	GFileInputStream *file_stream = g_file_read(file, NULL, NULL);
	g_object_unref(file);
  	GDataInputStream *data_stream = g_data_input_stream_new(G_INPUT_STREAM(file_stream));
  	g_data_input_stream_set_newline_type(data_stream, G_DATA_STREAM_NEWLINE_TYPE_ANY);

	char *line = (char *) 1;
  	int nlines = 0;

  	while (line) {
  		gsize linelen;
    	GError *tmp_error = NULL;
    	line = g_data_input_stream_read_line(data_stream, &linelen, NULL, &tmp_error);
    	g_assert_null(tmp_error);
    	if (line == NULL)
	 		break;
    	if (linelen == 0 || strlen(g_strstrip(line)) == 0) {
      		continue;
    	}
    	gchar **splitted = g_strsplit(line, ",", 0);

    	int i;
 	   	for (i = 2 ; i < g_strv_length(splitted) ; i++) {
    		double csv_value = g_ascii_strtod(splitted[i], NULL);
      		g_assert_cmpfloat(channels[i-1][nlines], ==, csv_value);
    	}

    	g_free(line);
    	g_strfreev(splitted);
    	nlines++;
  	}
	g_assert_cmpint(nchannels, ==, nlines);
  	g_object_unref(file_stream);
  	g_object_unref(data_stream);
}

void test_compare_channels_and_spe(int nchannels, double *channels, const gchar *spe_file) {
	struct spe_data *sd = read_spe(spe_file);
    g_assert_nonnull(sd);
    g_assert_cmpint(nchannels, ==, sd->nchannels);
    //compare channel contents
    int j;
    for (j = 0 ; j < sd->nchannels ; j++) {
      g_assert_cmpfloat(channels[j], ==, sd->data[j]);
    }
    free_spe_data(sd);
}