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
#include <config.h>
#include "xmimsim-gui-updater.h"
#include "xmimsim-gui-layer.h"
#include <curl/curl.h>
#include <json-glib/json-glib.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <math.h>
#ifdef MAC_INTEGRATION
	#import <Foundation/Foundation.h>
#endif

/*
 *
 * This code allows for the checking of available updates by parsing the JSON output obtained
 * through requesting the tags from the GitHub repo of XMI-MSIM
 *
 *
 *
 */


#define GTK_RESPONSE_QUIT 7



#define XMIMSIM_GITHUB_TAGS_LOCATION "https://api.github.com/repos/tschoonj/xmimsim/git/refs/tags"
#define XMIMSIM_GITHUB_DOWNLOADS_LOCATION "https://api.github.com/repos/tschoonj/xmimsim/downloads"


struct MemoryStruct {
 	char *memory;
	size_t size;
};

struct DownloadVars {
	gboolean started;
	GtkWidget *progressbar;
	GtkWidget *expander;
	GtkWidget *update_dialog;
	GtkWidget *button;
	GtkWidget *label;
	gulong downloadG;
	gulong stopG;
	gulong exitG;
	CURL *curl;
	FILE *fp;
	gchar *download_location;
};

static size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp) {
	size_t realsize = size * nmemb;
	struct MemoryStruct *mem = (struct MemoryStruct *)userp;
     
	mem->memory = realloc(mem->memory, mem->size + realsize + 1);
		        
	memcpy(&(mem->memory[mem->size]), contents, realsize);
	mem->size += realsize;
	mem->memory[mem->size] = 0;
			       
	return realsize;
}


static size_t WriteData(void *ptr, size_t size, size_t nmemb, FILE *stream) {
	size_t written;
	written = fwrite(ptr, size, nmemb, stream);
	return written;

}

static void stop_button_clicked_cb(GtkButton *button, struct DownloadVars *dv) {
	dv->started=FALSE;

	return;
}

static void exit_button_clicked_cb(GtkButton *button, struct DownloadVars *dv) {
	gtk_dialog_response(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_QUIT);
	fprintf(stdout,"exit clicked\n");

	return;
}
static void download_button_clicked_cb(GtkButton *button, struct DownloadVars *dv) {
	FILE *fp;
	CURLcode res;
	char curlerrors[CURL_ERROR_SIZE];

	fp = fopen(dv->download_location, "wb");
	dv->fp = fp;
	if (fp == NULL) {
		fprintf(stderr,"download_updates: Could not open %s for writing\n",dv->download_location);
		//set buttons ok
		

		return;
	}

	curl_easy_setopt(dv->curl, CURLOPT_WRITEDATA, fp);
	gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, FALSE);
	gtk_button_set_use_stock(GTK_BUTTON(dv->button), TRUE);
	gtk_button_set_label(GTK_BUTTON(dv->button), GTK_STOCK_STOP);
	g_signal_handler_disconnect(dv->button, dv->downloadG);
	dv->stopG = g_signal_connect(button, "clicked", G_CALLBACK(stop_button_clicked_cb), dv);

	dv->started=TRUE;
	gtk_expander_set_expanded(GTK_EXPANDER(dv->expander), TRUE);
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"0 %");
	while(gtk_events_pending())
	    gtk_main_iteration();
	
	res = curl_easy_perform(dv->curl);
	if (res != 0) {
		//download aborted
		fclose(fp);
		unlink(dv->download_location);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"Download aborted");
		while(gtk_events_pending())
		    gtk_main_iteration();
		curl_easy_cleanup(dv->curl);
		gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, TRUE);
		gtk_widget_set_sensitive(dv->button, FALSE);
		gtk_label_set_text(GTK_LABEL(dv->label),"Download aborted.\nTry again at a later time.");


	}
	else {
		dv->started=FALSE;
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"Download finished");
		while(gtk_events_pending())
		    gtk_main_iteration();
		curl_easy_cleanup(dv->curl);
		g_signal_handler_disconnect(dv->button, dv->stopG);
		gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, TRUE);
		gchar *buffer = g_strdup_printf("The new version of XMI-MSIM was downloaded as\n%s\nPress Quit to terminate XMI-MSIM.",dv->download_location);
		gtk_button_set_label(GTK_BUTTON(dv->button), GTK_STOCK_QUIT);
		gtk_label_set_text(GTK_LABEL(dv->label), buffer);
		dv->exitG = g_signal_connect(button, "clicked", G_CALLBACK(exit_button_clicked_cb), dv);
	}
	return;
} 



struct UrlStruct {
	char *filename;
	char *url;
};

static void check_download_url(JsonArray *array, guint index, JsonNode *node, struct UrlStruct *us) {
	JsonObject *object = json_node_get_object(node);
	if (!json_object_has_member(object,"html_url")) {
		return;
	}
	
	const gchar *html_url_string = json_object_get_string_member(object, "html_url");
	char *filename = strrchr(html_url_string,'/')+1;
	if (strcmp(filename, us->filename) == 0) {
		us->url = strdup(html_url_string);
	}

	return;
}

static void check_version_of_tag(JsonArray *array, guint index, JsonNode *node, char **max_version) {
	JsonObject *object = json_node_get_object(node);
	if (!json_object_has_member(object,"ref")) {
		return;
	}
	
	const gchar *ref_string = json_object_get_string_member(object, "ref");

	//discard old tag...
	if (strncmp(ref_string,"refs/tags/XMI-MSIM-",strlen("refs/tags/XMI-MSIM-")) != 0)
		return;

	char *tag_version_str = strrchr(ref_string,'-')+1;
	gdouble tag_version = g_ascii_strtod(tag_version_str,NULL);
	if (tag_version > g_ascii_strtod(*max_version,NULL)) {
		free(*max_version);
		*max_version = strdup(tag_version_str);
	}
		
	return;
}

static int DownloadProgress(void *dvvoid, double dltotal, double dlnow, double ultotal, double ulnow) {
	struct DownloadVars *dv = (struct DownloadVars *) dvvoid;
	if (dv->started == FALSE)
		return -1;
	

	if (dltotal < 1000)
		return 0;

	double ratio = dlnow/dltotal;
	gchar buffer[10];
	if (floor(ratio*100.0)/100.0 > floor(gtk_progress_bar_get_fraction(GTK_PROGRESS_BAR(dv->progressbar))*100.0)/100.0) {
		//update progress bar
		g_sprintf(buffer,"%i %%",(int) floor(ratio*100.0));
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),buffer);
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(dv->progressbar),ratio);
		while(gtk_events_pending())
		    gtk_main_iteration();
	}





	return 0;
}


int check_for_updates(char **max_version_rv) {
	GError *error = NULL;
	JsonParser *parser;
	char curlerrors[CURL_ERROR_SIZE];


	CURL *curl;
	CURLcode res;
	struct MemoryStruct chunk;

	chunk.memory = malloc(1);
	chunk.size = 0;

	fprintf(stdout,"checking for updates...\n");


	curl = curl_easy_init();
	if (!curl) {
		fprintf(stderr,"Could not initialize cURL\n");
		return XMIMSIM_UPDATES_ERROR;
	} 

	curl_easy_setopt(curl, CURLOPT_URL,XMIMSIM_GITHUB_TAGS_LOCATION);
	curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);
	curl_easy_setopt(curl, CURLOPT_USERAGENT, "libcurl-agent/1.0");
	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curlerrors);
	curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 4L);
	res = curl_easy_perform(curl);
	if (res != 0) {
		fprintf(stderr,"check_for_updates: %s\n",curlerrors);
		return XMIMSIM_UPDATES_ERROR;
	}
	curl_easy_cleanup(curl);




	parser = json_parser_new();
	if (json_parser_load_from_data(parser, chunk.memory, -1,&error) ==  FALSE) {
		if (error) {
			fprintf(stderr,"check_for_updates: %s\n",error->message);
			return XMIMSIM_UPDATES_ERROR;
		}
	}
	JsonNode *rootNode = json_parser_get_root(parser);
	if(json_node_get_node_type(rootNode) != JSON_NODE_ARRAY) {
		fprintf(stderr,"check_for_updates: rootNode is not an Array\n");
		return XMIMSIM_UPDATES_ERROR;
	}
	JsonArray *rootArray = json_node_get_array(rootNode);
	char *max_version = g_strdup(PACKAGE_VERSION);
	char *current_version = g_strdup(max_version);
	json_array_foreach_element(rootArray, (JsonArrayForeach) check_version_of_tag, &max_version);

	int rv;
	if (g_ascii_strtod(max_version, NULL) > g_ascii_strtod(current_version, NULL))
		rv = XMIMSIM_UPDATES_AVAILABLE;
	else
		rv = XMIMSIM_UPDATES_NONE;

	*max_version_rv = strdup(g_strstrip(max_version));	

	g_object_unref(parser);
	fprintf(stdout,"done checking for updates\n");

	return rv;
}



int download_updates(GtkWidget *window, char *max_version) {

	//should only be called when there is actually a new version available...
	//so call check_for_updates before
	
	GError *error = NULL;
	JsonParser *parser;
	char curlerrors[CURL_ERROR_SIZE];


	CURL *curl;
	CURLcode res;
	struct MemoryStruct chunk;

#ifdef MAC_INTEGRATION
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc]init];
#endif
	chunk.memory = malloc(1);
	chunk.size = 0;

	curl = curl_easy_init();
	if (!curl) {
		fprintf(stderr,"Could not initialize cURL\n");
		return 0;
	} 

	//construct filename -> platform dependent!!!
	gchar *filename;
	const gchar *downloadfolder;


#if defined(MAC_INTEGRATION)
	//Mac OS X
	filename = g_strdup_printf("XMI-MSIM-%s.dmg",max_version);
	//filename = g_strdup_printf("XMI-MSIM.dmg");
	NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDownloadsDirectory, NSUserDomainMask,TRUE);
	NSString *documentsDirectory = [paths objectAtIndex:0];
	downloadfolder = [documentsDirectory cStringUsingEncoding:NSUTF8StringEncoding];
	
#elif defined(G_OS_WIN32)
	//Win 32
	filename = g_strdup_printf("XMI-MSIM-%s-win32.exe",max_version);
	downloadfolder = g_get_user_special_dir(G_USER_DIRECTORY_DOWNLOAD);
	
#else
	//Linux??
	filename = g_strdup_printf("xmimsim-%s.tar.gz",max_version);
	downloadfolder = g_get_user_special_dir(G_USER_DIRECTORY_DOWNLOAD);
	
#endif

	//check if file exists!	
	


	res = curl_easy_setopt(curl, CURLOPT_URL,XMIMSIM_GITHUB_DOWNLOADS_LOCATION);
	res = curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
	res = curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
	res = curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);
	res = curl_easy_setopt(curl, CURLOPT_USERAGENT, "libcurl-agent/1.0");
	res = curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curlerrors);
	res = curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 4L);

	res = curl_easy_perform(curl);
	if (res != 0) {
		fprintf(stderr,"download_updates: %s\n",curlerrors);
		return 0;
	}
	curl_easy_cleanup(curl);

	parser = json_parser_new();
	if (json_parser_load_from_data(parser, chunk.memory, -1,&error) ==  FALSE) {
		if (error) {
			fprintf(stderr,"download_updates: %s\n",error->message);
			return 0;
		}
	}
	JsonNode *rootNode = json_parser_get_root(parser);
	if(json_node_get_node_type(rootNode) != JSON_NODE_ARRAY) {
		fprintf(stderr,"check_for_updates: rootNode is not an Array\n");
		return 0;
	}
	JsonArray *rootArray = json_node_get_array(rootNode);
	struct UrlStruct *us = (struct UrlStruct *) malloc(sizeof(struct UrlStruct));
	us->filename = filename;
	us->url = NULL;

	json_array_foreach_element(rootArray, (JsonArrayForeach) check_download_url, us);
	if (us->url == NULL)
		return 0;

	//spawn dialog
	//write your own code for this
	GtkWidget *update_dialog = gtk_dialog_new_with_buttons("XMI-MSIM updater",window != NULL ? GTK_WINDOW(window):NULL,
		window != NULL ? GTK_DIALOG_MODAL : 0, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,NULL);

	struct DownloadVars dv;

	gtk_window_set_resizable(GTK_WINDOW(update_dialog), FALSE);
	GtkWidget *update_content = gtk_dialog_get_content_area(GTK_DIALOG(update_dialog));
	GtkWidget *label_and_button_hbox = gtk_hbox_new(FALSE,5);
	gchar *label_text = g_strdup_printf("A new version of XMI-MSIM (%s) is available.\nYou are currently running version %s.",max_version, PACKAGE_VERSION);
	GtkWidget *label = gtk_label_new(label_text);
	GtkWidget *button = gtk_button_new_with_label("Download");
	gtk_box_pack_start(GTK_BOX(label_and_button_hbox), label, TRUE, TRUE, 1);
	gtk_box_pack_end(GTK_BOX(label_and_button_hbox), button, FALSE, TRUE, 1);
	gtk_container_set_border_width(GTK_CONTAINER(label_and_button_hbox), 8);
	gtk_box_pack_start(GTK_BOX(update_content),label_and_button_hbox, TRUE, FALSE, 2);
	dv.label = label;

	GtkWidget *expander = gtk_expander_new("Download progress");
	gtk_container_set_border_width(GTK_CONTAINER(expander), 8);
	gtk_expander_set_expanded(GTK_EXPANDER(expander),FALSE);
	gtk_expander_set_spacing(GTK_EXPANDER(expander),3);
	GtkWidget *progressbar = gtk_progress_bar_new();
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar), "Download not started");
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar), 0.0);

	gtk_container_add(GTK_CONTAINER(expander), progressbar);

	gtk_box_pack_start(GTK_BOX(update_content), expander, TRUE, FALSE, 2);
	gtk_container_set_border_width(GTK_CONTAINER(update_content), 8);

	gtk_widget_show_all(update_content);

	dv.started = FALSE;
	dv.progressbar = progressbar;
	dv.button = button;
	dv.expander = expander;
	dv.update_dialog = update_dialog;
	
	dv.downloadG = g_signal_connect(button, "clicked", G_CALLBACK(download_button_clicked_cb), &dv);
	
	curl = curl_easy_init();
	if (!curl) {
		fprintf(stderr,"Could not initialize cURL\n");
		return 0;
	} 
	dv.curl = curl;
	curl_easy_setopt(curl, CURLOPT_URL,us->url);
	curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteData);
	curl_easy_setopt(curl, CURLOPT_USERAGENT, "libcurl-agent/1.0");
	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curlerrors);
	curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0L);
	curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, DownloadProgress);
	curl_easy_setopt(curl, CURLOPT_PROGRESSDATA, &dv);
	curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION , 1);
	curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 4L);
	//construct download location
	gchar *download_location = g_strdup_printf("%s%s%s",downloadfolder,G_DIR_SEPARATOR_S,filename);
	dv.download_location = download_location;


	gint result = gtk_dialog_run(GTK_DIALOG(update_dialog));
	int rv;

	if (result == GTK_RESPONSE_QUIT) {
		//quit XMI-MSIM
		rv = 1;	
	}
	else
		rv = 0;
	
	gtk_widget_destroy(update_dialog);
#ifdef MAC_INTEGRATION
	[pool drain];
#endif
	

	return rv;
}


