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

#include <config.h>

#ifndef HAVE_LIBCURL
#error "CURL is required to run the testsuite!"
#endif

#include "libxmimsim-test.h"
#include <curl/curl.h>
#include <stdio.h>
#include <glib.h>
#include <stdlib.h>
#include "libxml/catalog.h"


int test_init () {
	char uriStartString[] = "http://www.xmi.UGent.be/xml/";
	char rewritePrefix[] = CATALOGPATH;

	if (xmlCatalogAdd(BAD_CAST "catalog",NULL,NULL) == -1) {
		fprintf(stderr, "xmlCatalogAdd error: catalog\n");
		return 0;
	}
	if (xmlCatalogAdd(BAD_CAST "rewriteURI", BAD_CAST uriStartString, BAD_CAST rewritePrefix) == -1) {
		fprintf(stderr, "xmlCatalogAdd error: rewriteURI\n");
		return 0;
	}
	return 1;
}

int test_download_file(char *url) {
	FILE *fp;
	CURLcode res;
	char curlerrors[CURL_ERROR_SIZE];

	gchar *outputfile = g_path_get_basename(url);

	CURL *curl = curl_easy_init();

	if (curl) {
		fp = fopen(outputfile, "wb");
		if (fp == NULL)
			return 0;
		curl_easy_setopt(curl, CURLOPT_URL, url);
        	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
        	curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
		curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curlerrors);
		curl_easy_setopt(curl, CURLOPT_FAILONERROR, 1);
		curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION , 1);
		curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 4L);
		curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);

        	res = curl_easy_perform(curl);
		if (res != CURLE_OK) {
			fprintf(stderr, "CURL error: %s\n", curlerrors);
			return 0;
		}
        	curl_easy_cleanup(curl);
        	fclose(fp);
	}
	else
		return 0;

	g_free(outputfile);
	return 1;
}

struct spe_data * read_spe(const char *filename) {
	FILE *spePtr = NULL;
	struct spe_data *rv = NULL;
	char buffer[1024];

	spePtr = fopen(filename, "r");
	if (spePtr == NULL) {
		fprintf(stderr, "Could not open %s for reading\n", filename);
		return NULL;
	}

	rv = malloc(sizeof(struct spe_data));

	do {
		if (fgets(buffer, 1024, spePtr) == NULL) {
			fprintf(stderr,"An error occurred while reading %s...\nAre you sure this is an SPE file?\n",filename);
			return NULL;
		}
	} while (strstr(buffer,"$DATA:") == NULL);

	fscanf(spePtr,"%u %u",&(rv->channel_first),&(rv->channel_last));

	rv->nchannels = rv->channel_last - rv->channel_first + 1;

	rv->data = malloc(sizeof(double) * rv->nchannels);

	unsigned int i;

	for (i = 0 ; i < rv->nchannels ; i++) {
    if (fscanf(spePtr, "%lf", rv->data + i) != 1) {
      fprintf(stderr,"Error reading DATA segment of %s at position %i\n",filename,i);
      return NULL;
    }
  }

	return rv;
}

void free_spe_data(struct spe_data *sd) {
	g_assert(sd != NULL);
	g_assert(sd->data != NULL);
	free(sd->data);
	free(sd);

	return;
}
