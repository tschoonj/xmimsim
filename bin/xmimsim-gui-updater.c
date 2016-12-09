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
#include "xmimsim-gui.h"
#include "xmimsim-gui-updater.h"
#include "xmimsim-gui-prefs.h"
#include "xmi_aux.h"
#include <curl/curl.h>
#include <json-glib/json-glib.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <math.h>
#ifdef MAC_INTEGRATION
	#import <Foundation/Foundation.h>
	#include <gtkosxapplication.h>
#endif


#ifdef G_OS_WIN32
	#include <Shellapi.h>
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
#define XMIMSIM_DOWNLOADS_LOCATION "http://lvserver.ugent.be/xmi-msim"


struct MemoryStruct {
 	char *memory;
	size_t size;
};

struct DownloadVars {
	gboolean started;
	GtkWidget *progressbar;
	GtkWidget *update_dialog;
	GtkWidget *button;
	GtkWidget *label;
	gulong downloadG;
	gulong stopG;
	gulong exitG;
	CURL *curl;
	FILE *fp;
	gchar *download_location;
	gchar *filename;
	GChecksum *md5sum_comp;
	char md5sum_tag[32+1];

};

static void my_gtk_text_buffer_insert_at_cursor_with_tags_updater(GtkTextBuffer *buffer, const gchar *text, gint len, GtkTextTag *first_tag, ...) {
	GtkTextIter iter, start;
	va_list args;
	GtkTextTag *tag;
	gint start_offset;

	g_return_if_fail(GTK_IS_TEXT_BUFFER(buffer));
	g_return_if_fail(text != NULL);

	gchar *to_print = g_strdup_printf("%s\n", text);

	gtk_text_buffer_get_end_iter(buffer, &iter);

	start_offset = gtk_text_iter_get_offset(&iter);
	gtk_text_buffer_insert(buffer, &iter, to_print,len);

	g_free(to_print);

	if (first_tag == NULL) {
		gtk_text_buffer_get_end_iter(buffer, &iter);
		gtk_text_buffer_get_insert(buffer);
		gtk_text_buffer_place_cursor(buffer,&iter);
		return;
	}

	gtk_text_buffer_get_iter_at_offset (buffer, &start, start_offset);

	va_start(args, first_tag);
	tag = first_tag;
	while (tag) {
		gtk_text_buffer_apply_tag(buffer, tag, &start, &iter);
		tag = va_arg(args, GtkTextTag*);
	}
	va_end(args);

	gtk_text_buffer_get_end_iter(buffer, &iter);
	gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_place_cursor(buffer,&iter);

	return;
}

static void update_check_toggled_cb(GtkToggleButton *checkbutton, GtkWidget *window) {

	union xmimsim_prefs_val prefs;

	prefs.b = gtk_toggle_button_get_active(checkbutton);

	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, prefs) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(window),
			GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR , GTK_BUTTONS_CLOSE, "A serious error occurred while checking\nthe preferences file.\nThe program will abort.");
		gtk_dialog_run(GTK_DIALOG(dialog));
	        gtk_widget_destroy(dialog);
		exit(1);
	}


}


static size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp) {
	size_t realsize = size * nmemb;
	struct MemoryStruct *mem = (struct MemoryStruct *)userp;

	mem->memory = (char *) realloc(mem->memory, mem->size + realsize + 1);

	memcpy(&(mem->memory[mem->size]), contents, realsize);
	mem->size += realsize;
	mem->memory[mem->size] = 0;

	return realsize;
}


static size_t WriteData(void *ptr, size_t size, size_t nmemb, struct DownloadVars *dv) {
	size_t written;
	written = fwrite(ptr, size, nmemb, dv->fp);
	if (dv->md5sum_comp != NULL) {
		g_checksum_update(dv->md5sum_comp, (const guchar *) ptr, size*nmemb);
	}
	return written;

}

static void stop_button_clicked_cb(GtkButton *button, struct DownloadVars *dv) {
	dv->started=FALSE;

	return;
}

static void exit_button_clicked_cb(GtkButton *button, struct DownloadVars *dv) {
	gtk_dialog_response(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_QUIT);
	fprintf(stdout,"exit clicked\n");

#ifdef MAC_INTEGRATION
	gchar *file = g_strdup_printf("file://%s",dv->download_location);
	CFURLRef url = CFURLCreateWithBytes (
      	NULL,
      	(UInt8*)file,
      	strlen(file),
      	kCFStringEncodingUTF8,
      	NULL
    	);
  	LSOpenCFURLRef(url,NULL);
  	CFRelease(url);

	quit_program_cb((GtkosxApplication *) g_object_new(GTKOSX_TYPE_APPLICATION,NULL), gtk_window_get_transient_for(GTK_WINDOW(dv->update_dialog)));
#elif defined(G_OS_WIN32)
	ShellExecute(NULL, "runas", dv->download_location, NULL, NULL, SW_SHOWNORMAL);
#endif
	return;
}

static void download_button_clicked_cb(GtkButton *button, struct DownloadVars *dv) {
	FILE *fp;
	CURLcode res;

	dv->started=TRUE;
	gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, FALSE);
	gtk_button_set_use_stock(GTK_BUTTON(dv->button), TRUE);
	gtk_button_set_label(GTK_BUTTON(dv->button), GTK_STOCK_STOP);
	g_signal_handler_disconnect(dv->button, dv->downloadG);
	dv->stopG = g_signal_connect(button, "clicked", G_CALLBACK(stop_button_clicked_cb), dv);

	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"0.00 %%");
	while(gtk_events_pending())
	    gtk_main_iteration();

	//get download locations
	union xmimsim_prefs_val prefs;
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS, &prefs) == 0) {
		fprintf(stderr,"Get preferences error\n");
		return;
	}

	unsigned int i;
	int rv;
	for (i = 0 ; i < g_strv_length(prefs.ss) ; i++) {
		gchar *url = g_strdup_printf("%s/%s", prefs.ss[i],dv->filename);
		curl_easy_setopt(dv->curl, CURLOPT_URL,url);
		fprintf(stdout,"Trying url %s\n",url);

		fp = fopen(dv->download_location, "wb");
		dv->fp = fp;
		if (fp == NULL) {
			fprintf(stderr,"download_updates: Could not open %s for writing\n",dv->download_location);
			//set buttons ok


			return;
		}

		res = curl_easy_perform(dv->curl);
		gchar *buffer = NULL;

		switch (res) {
		case CURLE_OK:
			dv->started=FALSE;
			fclose(fp);
			//checksum check
			if (dv->md5sum_comp != NULL && strcmp(g_checksum_get_string(dv->md5sum_comp), dv->md5sum_tag) != 0) {
				//checksums don't match!
				unlink(dv->download_location);
				gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"Checksum error");
				gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, TRUE);
				gtk_widget_set_sensitive(dv->button, FALSE);
				gtk_label_set_text(GTK_LABEL(dv->label),"Checksum error.\nTry again at a later time.");
				rv = 0;
				g_checksum_free(dv->md5sum_comp);
				break;
			}
			else if (dv->md5sum_comp != NULL) {
				g_checksum_free(dv->md5sum_comp);
			}
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"Download finished");
			g_signal_handler_disconnect(dv->button, dv->stopG);
			gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, TRUE);
			buffer = g_strdup_printf("The new version of XMI-MSIM was downloaded as\n%s\nPress Quit to terminate XMI-MSIM.",dv->download_location);
			gtk_button_set_label(GTK_BUTTON(dv->button), GTK_STOCK_QUIT);
			gtk_label_set_text(GTK_LABEL(dv->label), buffer);
			dv->exitG = g_signal_connect(button, "clicked", G_CALLBACK(exit_button_clicked_cb), dv);
			rv = 1;
			break;
		case CURLE_ABORTED_BY_CALLBACK:
			//aborted
			fclose(fp);
			unlink(dv->download_location);
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"Download aborted");
			gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, TRUE);
			gtk_widget_set_sensitive(dv->button, FALSE);
			gtk_label_set_text(GTK_LABEL(dv->label),"Download aborted.\nTry again at a later time.");
			rv = 0;
			break;
		default:
			fprintf(stdout,"curl_easy_perform error code: %i\n", res);
			fclose(fp);
			unlink(dv->download_location);
			rv = -1;

		}
		if (rv == 1 || rv == 0)
			break;
	}

	//if update_progressbar is still running somehow -> kill it
	/*GSource *source = g_main_context_find_source_by_id(NULL, source_id);
	if (source != NULL && !g_source_is_destroyed(source)) {
		g_source_destroy(source);
	}
	*/

	if (rv == -1) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"Download failed");
		gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, TRUE);
		gtk_widget_set_sensitive(dv->button, FALSE);
		gchar *text = g_strdup_printf("Download failed with error message: %s.", curl_easy_strerror(res));
		gtk_label_set_text(GTK_LABEL(dv->label), text);
		g_free(text);
	}

	g_strfreev(prefs.ss);
	while(gtk_events_pending())
	    gtk_main_iteration();
	curl_easy_cleanup(dv->curl);


	return;
}



struct json_loop_data {
	char *max_version;
	char *url;
};


static void check_version_of_tag(JsonArray *array, guint index, JsonNode *node, struct json_loop_data *jld) {
	JsonObject *object = json_node_get_object(node);
	if (!json_object_has_member(object,"ref")) {
		return;
	}

	const gchar *ref_string = json_object_get_string_member(object, "ref");

	//discard old tag...
	if (strncmp(ref_string,"refs/tags/XMI-MSIM-",strlen("refs/tags/XMI-MSIM-")) != 0)
		return;

	char *tag_version_str = (char *) strrchr(ref_string,'-')+1;
	gdouble tag_version = g_ascii_strtod(tag_version_str,NULL);
	if (tag_version > g_ascii_strtod(jld->max_version,NULL)) {
		g_free(jld->max_version);
		jld->max_version = g_strdup(tag_version_str);
		g_free(jld->url);
		JsonObject *urlObject = json_object_get_object_member(object, "object");
		jld->url = g_strdup(json_object_get_string_member(urlObject, "url"));
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
	gchar buffer[15];
	if (floor(ratio*10000.0)/10000.0 > floor(gtk_progress_bar_get_fraction(GTK_PROGRESS_BAR(dv->progressbar))*10000.0)/10000.0) {
		//update progress bar
		g_sprintf(buffer,"%.2f %%",ratio*100.0);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),buffer);
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(dv->progressbar),ratio);
		while(gtk_events_pending())
		    gtk_main_iteration();
	}





	return 0;
}

int check_for_updates(char **max_version_rv, char **message) {
	GError *error = NULL;
	JsonParser *parser;
	char curlerrors[CURL_ERROR_SIZE];


	CURL *curl;
	CURLcode res;
	struct MemoryStruct chunk;

	chunk.memory = (char *) malloc(1);
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
	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curlerrors);
	curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 4L);
	char *user_agent = g_strdup_printf("XMI-MSIM %s updater using curl %i.%i.%i", PACKAGE_VERSION, LIBCURL_VERSION_MAJOR, LIBCURL_VERSION_MINOR, LIBCURL_VERSION_PATCH);
	curl_easy_setopt(curl, CURLOPT_USERAGENT, user_agent);
	g_free(user_agent);
	res = curl_easy_perform(curl);
	if (res != 0) {
		fprintf(stderr,"check_for_updates: %s\n",curlerrors);
		return XMIMSIM_UPDATES_ERROR;
	}




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
	struct json_loop_data *jld = (struct json_loop_data *) g_malloc(sizeof(struct json_loop_data));
	jld->max_version = max_version;
	jld->url = NULL;
	json_array_foreach_element(rootArray, (JsonArrayForeach) check_version_of_tag, jld);

	free(chunk.memory);
	int rv;
	if (g_ascii_strtod(jld->max_version, NULL) > g_ascii_strtod(current_version, NULL)) {
		rv = XMIMSIM_UPDATES_AVAILABLE;
		//get tag message
		g_object_unref(parser);
		chunk.memory = (char *) malloc(1);
		chunk.size = 0;
		curl_easy_setopt(curl, CURLOPT_URL, jld->url);
		res = curl_easy_perform(curl);
		if (res != 0) {
			fprintf(stderr,"check_for_updates: %s\n",curlerrors);
			return XMIMSIM_UPDATES_ERROR;
		}
		parser = json_parser_new();
		if (json_parser_load_from_data(parser, chunk.memory, -1,&error) ==  FALSE) {
			if (error) {
				fprintf(stderr,"check_for_updates: %s\n",error->message);
				return XMIMSIM_UPDATES_ERROR;
			}
		}
		rootNode = json_parser_get_root(parser);
		JsonObject *object = json_node_get_object(rootNode);
		if (!json_object_has_member(object,"message")) {
			return XMIMSIM_UPDATES_ERROR;
		}

		*message = g_strdup(json_object_get_string_member(object, "message"));
		free(chunk.memory);

	}
	else {
		rv = XMIMSIM_UPDATES_NONE;
	}

	*max_version_rv = strdup(g_strstrip(jld->max_version));
	//g_fprintf(stdout, "tag url: %s\n", jld->url);
	g_free(jld->max_version);
	g_free(jld->url);
	g_free(jld);

	g_object_unref(parser);
	curl_easy_cleanup(curl);
	//fprintf(stdout, "message: %s\n", *message);
	fprintf(stdout,"done checking for updates\n");

	return rv;
}



int download_updates(GtkWidget *window, char *max_version, char *message) {

	//should only be called when there is actually a new version available...
	//so call check_for_updates before

	char curlerrors[CURL_ERROR_SIZE];
	CURL *curl;

#ifdef MAC_INTEGRATION
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc]init];
#endif




	//spawn dialog
	//write your own code for this
	GtkWidget *update_dialog = gtk_dialog_new_with_buttons("XMI-MSIM updater",window != NULL ? GTK_WINDOW(window):NULL,
		window != NULL ? GTK_DIALOG_MODAL : (GtkDialogFlags) 0, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,NULL);


	struct DownloadVars dv;

	//gtk_window_set_resizable(GTK_WINDOW(update_dialog), FALSE);
	GtkWidget *update_content = gtk_dialog_get_content_area(GTK_DIALOG(update_dialog));
	GtkWidget *label_and_button_hbox = gtk_hbox_new(FALSE,5);
	gchar *label_text = g_strdup_printf("A new version of XMI-MSIM (%s) is available.\nYou are currently running version %s.",max_version, PACKAGE_VERSION);
	GtkWidget *label = gtk_label_new(label_text);
	GtkWidget *button = gtk_button_new_with_label("Download");
	gtk_box_pack_start(GTK_BOX(label_and_button_hbox), label, TRUE, TRUE, 1);
	gtk_box_pack_end(GTK_BOX(label_and_button_hbox), button, FALSE, TRUE, 1);
	gtk_box_pack_start(GTK_BOX(update_content),label_and_button_hbox, TRUE, FALSE, 5);
	gtk_box_pack_start(GTK_BOX(update_content),gtk_hseparator_new(), TRUE, FALSE, 5);
	dv.label = label;

	GtkWidget *messageBox = gtk_text_view_new();
	gtk_widget_set_size_request(messageBox,300,150);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(messageBox),GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(messageBox),3);
	GtkTextBuffer *textBuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(messageBox));
	gtk_text_view_set_editable(GTK_TEXT_VIEW(messageBox),FALSE);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(messageBox),FALSE);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), messageBox);
	//gtk_text_buffer_set_text(textBuffer, message, -1);
	gtk_box_pack_start(GTK_BOX(update_content),scrolled_window, TRUE, FALSE, 5);
	gtk_box_pack_start(GTK_BOX(update_content),gtk_hseparator_new(), TRUE, FALSE, 5);
	//GtkTextIter begin, end;
	//gtk_text_buffer_get_iter_at_line(textBuffer, &begin, 0);
	//gtk_text_buffer_get_iter_at_line(textBuffer, &end, 1);
	GtkTextTag *tag = gtk_text_buffer_create_tag(textBuffer, NULL, "font", "20", NULL);
	GtkTextTag *tag2 = gtk_text_buffer_create_tag(textBuffer, NULL, "font", "16", NULL);
	//gtk_text_buffer_apply_tag(textBuffer, tag, &begin, &end);
	int i;

	gchar **splitted = g_strsplit_set(message, "\n", -1);
	my_gtk_text_buffer_insert_at_cursor_with_tags_updater(textBuffer, splitted[0], -1, tag, NULL);

	for (i = 1 ; splitted[i] != NULL ; i++) {
		if (g_ascii_strncasecmp(splitted[i], "Changes", strlen("Changes")) == 0 ||
		   g_ascii_strncasecmp(splitted[i], "Bugfixes", strlen("Bugfixes")) == 0 ||
		   g_ascii_strncasecmp(splitted[i], "New", strlen("New")) == 0 ||
		   g_ascii_strncasecmp(splitted[i], "Checksum", strlen("Checksum")) == 0 ||
		   g_ascii_strncasecmp(splitted[i], "Important", strlen("Important")) == 0 ||
		   g_ascii_strncasecmp(splitted[i], "Note", strlen("Note")) == 0// ||
		   //g_ascii_strncasecmp(splitted[i], "-----BEGIN PGP SIGNATURE-----", strlen("-----BEGIN PGP SIGNATURE-----")) == 0
		) {
			my_gtk_text_buffer_insert_at_cursor_with_tags_updater(textBuffer, splitted[i], -1, tag2, NULL);
		}
		else {
			my_gtk_text_buffer_insert_at_cursor_with_tags_updater(textBuffer, splitted[i], -1, NULL);
		}
	}


	label = gtk_label_new("Download progress");
	gtk_box_pack_start(GTK_BOX(update_content), label, TRUE, FALSE, 2);
	GtkWidget *progressbar = gtk_progress_bar_new();
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar), "Download not started");
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar), 0.0);

	gtk_box_pack_start(GTK_BOX(update_content), progressbar, TRUE, FALSE, 5);
	gtk_box_pack_start(GTK_BOX(update_content),gtk_hseparator_new(), TRUE, FALSE, 5);

	//Check for updates on startup preference
	GtkWidget *checkbutton = gtk_check_button_new_with_label("Check for updates on startup?");
	gtk_box_pack_start(GTK_BOX(update_content), checkbutton, TRUE, FALSE, 5);
	//get value from preferences
	union xmimsim_prefs_val prefs;
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, &prefs) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new(window != NULL ? GTK_WINDOW(window): NULL,
			window != NULL ? GTK_DIALOG_MODAL : (GtkDialogFlags) 0, GTK_MESSAGE_ERROR , GTK_BUTTONS_CLOSE, "A serious error occurred while checking\nthe preferences file.\nThe program will abort.");
		gtk_dialog_run(GTK_DIALOG(dialog));
	        gtk_widget_destroy(dialog);
		exit(1);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton),prefs.b);
	g_signal_connect(G_OBJECT(checkbutton), "toggled", G_CALLBACK(update_check_toggled_cb), window);



	//signal


	gtk_container_set_border_width(GTK_CONTAINER(update_dialog), 10);

	gtk_widget_show_all(update_content);

	dv.started = FALSE;
	dv.progressbar = progressbar;
	dv.button = button;
	dv.update_dialog = update_dialog;

	dv.downloadG = g_signal_connect(button, "clicked", G_CALLBACK(download_button_clicked_cb), &dv);

	curl = curl_easy_init();
	if (!curl) {
		fprintf(stderr,"Could not initialize cURL\n");
		return 0;
	}
	dv.curl = curl;
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
#ifdef _WIN64
	filename = g_strdup_printf("XMI-MSIM-%s-win64.exe",max_version);
#elif defined(_WIN32)
	filename = g_strdup_printf("XMI-MSIM-%s-win32.exe",max_version);
#else
#error Unknown Windows architecture detected
#endif

	downloadfolder = g_get_user_special_dir(G_USER_DIRECTORY_DOWNLOAD);

#else
	//Linux??
	filename = g_strdup_printf("xmimsim-%s.tar.gz",max_version);
	downloadfolder = g_get_user_special_dir(G_USER_DIRECTORY_DOWNLOAD);

#endif

	strcpy(dv.md5sum_tag, "none");
	char *pattern = g_strdup_printf("MD5 (%s) = %%32s", filename);

	for (i = 1 ; splitted[i] != NULL ; i++) {
		if (sscanf(splitted[i], pattern, dv.md5sum_tag) > 0) {
			break;
		}

	}
	g_strfreev(splitted);
	g_free(pattern);

	if (strcmp(dv.md5sum_tag, "none") != 0) {
		dv.md5sum_comp = g_checksum_new(G_CHECKSUM_MD5);
	}
	else {
		dv.md5sum_comp = NULL;
	}

	curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteData);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *) &dv);
	char *user_agent = g_strdup_printf("XMI-MSIM %s updater using curl %i.%i.%i", PACKAGE_VERSION, LIBCURL_VERSION_MAJOR, LIBCURL_VERSION_MINOR, LIBCURL_VERSION_PATCH);
	curl_easy_setopt(curl, CURLOPT_USERAGENT, user_agent);
	g_free(user_agent);
	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curlerrors);
	curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0L);
	curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, DownloadProgress);
	curl_easy_setopt(curl, CURLOPT_PROGRESSDATA, &dv);
	curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION , 1);
	curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 4L);
	curl_easy_setopt(curl, CURLOPT_FAILONERROR, 1);
	//construct download location
	gchar *download_location = g_strdup_printf("%s%s%s",downloadfolder,G_DIR_SEPARATOR_S,filename);
	dv.download_location = download_location;
	dv.filename = filename;


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


