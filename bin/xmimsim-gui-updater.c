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
#include "xmimsim-gui-updater.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <libsoup/soup.h>
#include <json-glib/json-glib.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <glib/gstdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#ifdef MAC_INTEGRATION
	#include "xmi_resources_mac.h"
	#include <CoreFoundation/CoreFoundation.h>
	#include <CoreServices/CoreServices.h>
#endif


#ifdef G_OS_WIN32
	#include <windows.h>
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


struct DownloadVars {
	SoupSession *session;
	SoupMessage *msg;
	GMainLoop *loop;
	goffset content_length;
	goffset content_downloaded;
	GtkWidget *progressbar;
	GtkWidget *update_dialog;
	GtkWidget *button;
	GtkWidget *label;
	gulong downloadG;
	gulong stopG;
	gulong exitG;
	gchar *download_location;
	gchar *filename;
	GChecksum *md5sum_comp;
	char *md5sum_tag;
	guint status_code;
	gchar *reason_phrase;
	GFileOutputStream *stream;
};

static void update_check_toggled_cb(GtkToggleButton *checkbutton, GtkWidget *update_dialog) {

	GValue prefs = G_VALUE_INIT;

	g_value_init(&prefs, G_TYPE_BOOLEAN);
	g_value_set_boolean(&prefs, gtk_toggle_button_get_active(checkbutton));

	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, &prefs) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(update_dialog),
			GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR , GTK_BUTTONS_CLOSE, "A serious error occurred while checking\nthe preferences file.\nThe program will abort.");
		gtk_dialog_run(GTK_DIALOG(dialog));
	        gtk_widget_destroy(dialog);
		exit(1);
	}
	g_value_unset(&prefs);
}

static void stop_button_clicked_cb(GtkButton *button, struct DownloadVars *dv) {
	soup_session_cancel_message(dv->session, dv->msg, SOUP_STATUS_CANCELLED);
}

static void exit_button_clicked_cb(GtkButton *button, struct DownloadVars *dv) {
	gtk_dialog_response(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_QUIT);

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

	// TODO: need to carefully check what to do here...
	//quit_program_cb((GtkosxApplication *) g_object_new(GTKOSX_TYPE_APPLICATION,NULL), gtk_window_get_transient_for(GTK_WINDOW(dv->update_dialog)));
#elif defined(G_OS_WIN32)
	// TODO: use ShellExecuteW to deal with unicode download_location...
	ShellExecute(NULL, "runas", dv->download_location, NULL, NULL, SW_SHOWNORMAL);
#endif
	return;
}

static void download_got_headers(SoupMessage *msg, struct DownloadVars *dv) {
	dv->content_length = soup_message_headers_get_content_length(msg->response_headers);
}

static gboolean download_update_progressbar(struct DownloadVars *dv) {

	double ratio = (double) dv->content_downloaded / (double) dv->content_length;
	gchar *buffer = g_strdup_printf("%.2f %%",ratio*100.0);
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar), buffer);
	g_free(buffer);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(dv->progressbar), ratio);
	
	return FALSE;
}

static void download_got_chunk (SoupMessage *msg, SoupBuffer *chunk, struct DownloadVars *dv) {
	GError *error = NULL;
	GBytes *bytes = soup_buffer_get_as_bytes (chunk);

	g_output_stream_write_bytes(G_OUTPUT_STREAM(dv->stream), bytes, NULL, &error);

	if (error) {
		dv->reason_phrase = g_strdup(error->message);
		soup_session_cancel_message(dv->session, msg, SOUP_STATUS_IO_ERROR);
	}

	dv->content_downloaded += g_bytes_get_size(bytes);

	if (dv->md5sum_comp != NULL) {
		g_checksum_update(dv->md5sum_comp, g_bytes_get_data(bytes, NULL), g_bytes_get_size(bytes));
	}
	// update progressbar!
	gdk_threads_add_idle((GSourceFunc) download_update_progressbar, dv);
}

static void download_finished(SoupSession *session, SoupMessage *msg, struct DownloadVars *dv) {
	dv->status_code = msg->status_code;
	dv->reason_phrase = g_strdup(msg->reason_phrase);
	g_main_loop_quit(dv->loop);
}

static void download_button_clicked_cb(GtkButton *button, struct DownloadVars *dv) {
	GError *error = NULL;

	unsigned int i;
	int rv;

	GFile *gfile = g_file_new_for_path(dv->download_location);

	gchar *user_agent = g_strdup_printf("XMI-MSIM " PACKAGE_VERSION " updater using libsoup %d.%d.%d", SOUP_MAJOR_VERSION, SOUP_MINOR_VERSION, SOUP_MICRO_VERSION);
	SoupSession *session = xmi_soup_session_new(user_agent);
	g_free(user_agent);
	dv->session = session;
	dv->content_downloaded = 0;

	gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, FALSE);
	gtk_button_set_label(GTK_BUTTON(dv->button), "_Stop");
	g_signal_handler_disconnect(dv->button, dv->downloadG);
	dv->stopG = g_signal_connect(button, "clicked", G_CALLBACK(stop_button_clicked_cb), dv);

	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"0.00 %");

	//get download locations
	GValue prefs = G_VALUE_INIT;
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS, &prefs) == 0) {
		g_warning("Get preferences error\n");
		// show error dialog
		return;
	}

	gchar **download_locations = g_value_dup_boxed(&prefs);
	g_value_unset(&prefs);

	for (i = 0 ; i < g_strv_length(download_locations) ; i++) {
		gchar *url = g_strdup_printf("%s/%s", download_locations[i], dv->filename);
		g_debug("Trying url %s\n",url);

		GFileOutputStream *stream = g_file_replace(gfile, NULL, FALSE, G_FILE_CREATE_NONE, NULL, &error);

		if (!stream) {
			rv = 0;
			break;
		}
		dv->stream = stream;
		SoupMessage *msg = soup_message_new("GET", url);
		dv->msg = msg;
		soup_message_body_set_accumulate(msg->response_body, FALSE);
		g_signal_connect (msg, "got-headers", G_CALLBACK(download_got_headers), dv);
		g_signal_connect (msg, "got-chunk", G_CALLBACK(download_got_chunk), dv);

		soup_session_queue_message(session, msg, (SoupSessionCallback) download_finished, dv);
		dv->loop = g_main_loop_new(NULL, TRUE);
		g_main_loop_run(dv->loop);

		if (!g_output_stream_close(G_OUTPUT_STREAM(dv->stream), NULL, &error)) {
			rv = 0;
			break;
		}
		g_object_unref(dv->stream);

		if (SOUP_STATUS_IS_SUCCESSFUL(dv->status_code)) {
			//checksum check
			if (dv->md5sum_comp != NULL && strcmp(g_checksum_get_string(dv->md5sum_comp), dv->md5sum_tag) != 0) {
				//checksums don't match!
				g_unlink(dv->download_location);
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
			g_signal_handler_disconnect(dv->button, dv->stopG);
			gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, TRUE);
			gchar *buffer = g_strdup_printf("The new version of XMI-MSIM was downloaded as\n%s\nPress Quit to terminate XMI-MSIM.",dv->download_location);
			gtk_button_set_label(GTK_BUTTON(dv->button), "Quit");
			gtk_label_set_text(GTK_LABEL(dv->label), buffer);
			g_free(buffer);
			dv->exitG = g_signal_connect(button, "clicked", G_CALLBACK(exit_button_clicked_cb), dv);
			rv = 1;
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"Download finished");
			break;
		}
		else if (dv->status_code == SOUP_STATUS_CANCELLED) {
			//aborted
			g_unlink(dv->download_location);
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"Download aborted");
			gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, TRUE);
			gtk_widget_set_sensitive(dv->button, FALSE);
			gtk_label_set_text(GTK_LABEL(dv->label),"Download aborted.\nTry again at a later time.");
			rv = 0;
			break;
		}
		else {
			g_unlink(dv->download_location);
			g_debug("%s could not be downloaded: %s", url, dv->reason_phrase);
			rv = -1;
		}
		// cleanup should happen here...
	}

	g_strfreev(download_locations);

	if (rv == -1) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(dv->progressbar),"Download failed");
		gtk_dialog_set_response_sensitive(GTK_DIALOG(dv->update_dialog), GTK_RESPONSE_REJECT, TRUE);
		gtk_widget_set_sensitive(dv->button, FALSE);
		gchar *text = g_strdup_printf("Last attempt failed with error message: %s.", dv->reason_phrase);
		gtk_label_set_text(GTK_LABEL(dv->label), text);
		g_free(text);
	}
}



struct json_loop_data {
	gchar *max_version;
	gchar *url;
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

	gchar *tag_version_str = strrchr(ref_string,'-')+1;
	gdouble tag_version = g_ascii_strtod(tag_version_str,NULL);
	if (tag_version > g_ascii_strtod(jld->max_version,NULL)) {
		g_free(jld->max_version);
		jld->max_version = g_strdup(tag_version_str);
		g_free(jld->url);
		JsonObject *urlObject = json_object_get_object_member(object, "object");
		jld->url = g_strdup(json_object_get_string_member(urlObject, "url"));
	}
}

struct update_data {
	gchar *max_version;
	gchar *message;
};

static void check_for_updates_thread(GTask *task, gpointer source_object, gpointer task_data, GCancellable *cancellable) {
	GError *error = NULL;
	gchar *user_agent = g_strdup_printf("XMI-MSIM " PACKAGE_VERSION " updater using libsoup %d.%d.%d", SOUP_MAJOR_VERSION, SOUP_MINOR_VERSION, SOUP_MICRO_VERSION);
	SoupSession *session = soup_session_new_with_options(
		SOUP_SESSION_USER_AGENT, user_agent,
		SOUP_SESSION_TIMEOUT, 5u,
		NULL);
	g_free(user_agent);

	SoupRequestHTTP *request = soup_session_request_http(
		session,
		"GET",
		XMIMSIM_GITHUB_TAGS_LOCATION,
		&error);
	if (!request) {
		g_object_unref(session);
		g_task_return_error(task, error);
		return;
	}

	GInputStream *stream = soup_request_send(SOUP_REQUEST(request), NULL, &error);
	if (!stream) {
		g_object_unref(request);
		g_object_unref(session);
		g_task_return_error(task, error);
		return;
	}

	g_object_unref(request);

	JsonParser *parser = json_parser_new();
	if (!json_parser_load_from_stream(parser, stream, NULL, &error)) {
		g_object_unref(session);
		g_object_unref(parser);
		g_input_stream_close(stream, NULL, NULL);
		g_task_return_error(task, error);
		return;
	}

	if(!g_input_stream_close(stream, NULL, &error)) {
		g_object_unref(parser);
		g_object_unref(session);
		g_task_return_error(task, error);
		return;
	}
	g_object_unref(stream);

	JsonNode *rootNode = json_parser_get_root(parser);
	if(json_node_get_node_type(rootNode) != JSON_NODE_ARRAY) {
		g_object_unref(session);
		g_object_unref(parser);
		g_task_return_new_error(
			task,
			XMI_MSIM_GUI_UPDATER_ERROR,
			XMI_MSIM_GUI_UPDATER_JSON_TYPE_MISMATCH,
			"json stream root node is not an array"
			);
		return;
	}
	JsonArray *rootArray = json_node_get_array(rootNode);
	gchar *max_version = g_strdup(PACKAGE_VERSION);
	gchar *current_version = g_strdup(max_version);
	struct json_loop_data *jld = g_malloc(sizeof(struct json_loop_data));
	jld->max_version = max_version;
	jld->url = NULL;
	json_array_foreach_element(rootArray, (JsonArrayForeach) check_version_of_tag, jld);

	g_object_unref(parser);

	struct update_data *ud = NULL;

	if (g_ascii_strtod(jld->max_version, NULL) > g_ascii_strtod(current_version, NULL)) {
		ud = g_malloc(sizeof(struct update_data));
		//get tag message
		request = soup_session_request_http(
			session,
			"GET",
			jld->url,
			&error);

		if (!request) {
			g_object_unref(session);
			g_task_return_error(task, error);
			return;
		}

		stream = soup_request_send(SOUP_REQUEST(request), NULL, &error);
		if (!stream) {
			g_object_unref(request);
			g_object_unref(session);
			g_task_return_error(task, error);
			return;
		}

		g_object_unref(request);

		parser = json_parser_new();
		if (!json_parser_load_from_stream(parser, stream, NULL, &error)) {
			g_object_unref(session);
			g_object_unref(parser);
			g_input_stream_close(stream, NULL, NULL);
			g_task_return_error(task, error);
			return;
		}

		if (!g_input_stream_close(stream, NULL, &error)) {
			g_object_unref(parser);
			g_object_unref(session);
			g_task_return_error(task, error);
			return;
		}
		g_object_unref(stream);

		rootNode = json_parser_get_root(parser);
		JsonObject *object = json_node_get_object(rootNode);
		if (!json_object_has_member(object,"message")) {
			g_object_unref(session);
			g_object_unref(parser);
			g_task_return_new_error(
				task,
				XMI_MSIM_GUI_UPDATER_ERROR,
				XMI_MSIM_GUI_UPDATER_JSON_MISSING_MEMBER,
				"tag root object has no member called message"
				);
			return;
		}

		ud->message = g_strdup(json_object_get_string_member(object, "message"));
		ud->max_version = g_strdup(g_strstrip(jld->max_version));
		g_object_unref(parser);
	}

	g_free(jld->max_version);
	g_free(jld->url);
	g_free(jld);

	g_debug("done checking for updates\n");

	g_object_unref(session);

	g_task_return_pointer(task, ud, g_free);
}

void xmi_msim_gui_updater_check_for_updates_async(XmiMsimGuiApplication *app, GAsyncReadyCallback callback, gpointer user_data) {

	g_debug("checking for updates...\n");

	GTask *task = g_task_new(app, NULL, callback, user_data);
	g_task_run_in_thread(task, check_for_updates_thread);
	g_object_unref(task);
}

XmiMsimGuiUpdaterCheck xmi_msim_gui_updater_check_for_updates_finish(XmiMsimGuiApplication *app, GAsyncResult *result, gchar **max_version, gchar **message, GError **error) {
	struct update_data *ud = g_task_propagate_pointer(G_TASK(result), error);
	if (g_task_had_error(G_TASK(result)))
		return XMI_MSIM_UPDATES_ERROR;
	if (ud) {
		*max_version = ud->max_version;
		*message = ud->message;
		g_free(ud);
		return XMI_MSIM_UPDATES_AVAILABLE;
	}
	return XMI_MSIM_UPDATES_NONE;
}

int xmi_msim_gui_updater_download_updates_dialog(XmiMsimGuiApplication *app, gchar *max_version, gchar *message) {

	//should only be called when there is actually a new version available...
	//so call check_for_updates before

	//spawn dialog
	//write your own code for this
	GtkWindow *active_window = gtk_application_get_active_window(GTK_APPLICATION(app));
	GtkWidget *update_dialog = gtk_dialog_new_with_buttons("XMI-MSIM updater", active_window,
		active_window != NULL ? GTK_DIALOG_MODAL : 0, "_Cancel", GTK_RESPONSE_REJECT, NULL);
	gtk_window_set_application(GTK_WINDOW(update_dialog), GTK_APPLICATION(app));

	gtk_window_set_default_size(GTK_WINDOW(update_dialog), 600, 600);

	struct DownloadVars *dv = g_malloc(sizeof(struct DownloadVars));

	GtkWidget *update_content = gtk_dialog_get_content_area(GTK_DIALOG(update_dialog));
	GtkWidget *label_and_button_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gchar *label_text = g_strdup_printf("<span weight=\"bold\">XMI-MSIM %s is now available.\nYou are currently running version %s.\nPlease update in order to take advantage of bugfixes and new features.</span>",max_version, PACKAGE_VERSION);
	GtkWidget *label = gtk_label_new(label_text);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
	GtkWidget *button = gtk_button_new_with_label("Download");
	gtk_box_pack_start(GTK_BOX(label_and_button_hbox), label, TRUE, TRUE, 1);
	gtk_box_pack_end(GTK_BOX(label_and_button_hbox), button, FALSE, TRUE, 1);
	gtk_box_pack_start(GTK_BOX(update_content),label_and_button_hbox, FALSE, FALSE, 5);
	gtk_box_pack_start(GTK_BOX(update_content), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 5);
	dv->label = label;

	GtkWidget *messageBox = gtk_text_view_new();
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(messageBox),GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(messageBox),3);
	GtkTextBuffer *textBuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(messageBox));
	gtk_text_view_set_editable(GTK_TEXT_VIEW(messageBox),FALSE);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(messageBox),FALSE);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), messageBox);
	gtk_box_pack_start(GTK_BOX(update_content),scrolled_window, TRUE, TRUE, 5);
	gtk_box_pack_start(GTK_BOX(update_content), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 5);
	GtkTextTag *tag = gtk_text_buffer_create_tag(textBuffer, NULL, "font", "20", NULL);
	GtkTextTag *tag2 = gtk_text_buffer_create_tag(textBuffer, NULL, "font", "16", NULL);
	int i;

	gchar **splitted = g_strsplit_set(message, "\n", -1);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(NULL, NULL, textBuffer, splitted[0], -1, tag, NULL);

	for (i = 1 ; splitted[i] != NULL ; i++) {
		if (g_ascii_strncasecmp(splitted[i], "Changes", strlen("Changes")) == 0 ||
		   g_ascii_strncasecmp(splitted[i], "Bugfixes", strlen("Bugfixes")) == 0 ||
		   g_ascii_strncasecmp(splitted[i], "New", strlen("New")) == 0 ||
		   g_ascii_strncasecmp(splitted[i], "Checksum", strlen("Checksum")) == 0 ||
		   g_ascii_strncasecmp(splitted[i], "Important", strlen("Important")) == 0 ||
		   g_ascii_strncasecmp(splitted[i], "Note", strlen("Note")) == 0// ||
		   //g_ascii_strncasecmp(splitted[i], "-----BEGIN PGP SIGNATURE-----", strlen("-----BEGIN PGP SIGNATURE-----")) == 0
		) {
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(NULL, NULL, textBuffer, splitted[i], -1, tag2, NULL);
		}
		else {
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(NULL, NULL, textBuffer, splitted[i], -1, NULL);
		}
	}


	label = gtk_label_new("Download progress");
	gtk_box_pack_start(GTK_BOX(update_content), label, FALSE, FALSE, 2);
	GtkWidget *progressbar = gtk_progress_bar_new();
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar), "Download has not started yet");
	gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(progressbar), TRUE);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar), 0.0);

	gtk_box_pack_start(GTK_BOX(update_content), progressbar, FALSE, FALSE, 5);
	gtk_box_pack_start(GTK_BOX(update_content), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 5);

	//Check for updates on startup preference
	GtkWidget *checkbutton = gtk_check_button_new_with_label("Check for updates on startup?");
	gtk_box_pack_start(GTK_BOX(update_content), checkbutton, FALSE, FALSE, 5);
	//get value from preferences
	GValue prefs = G_VALUE_INIT;
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, &prefs) == 0) {
		GtkWidget *dialog = gtk_message_dialog_new(active_window,
			active_window != NULL ? GTK_DIALOG_MODAL : 0, GTK_MESSAGE_ERROR , GTK_BUTTONS_CLOSE, "A serious error occurred while checking\nthe preferences file.\nThe program will abort.");
		gtk_window_set_application(GTK_WINDOW(dialog), GTK_APPLICATION(app));
		gtk_dialog_run(GTK_DIALOG(dialog));
	        gtk_widget_destroy(dialog);
		GAction *action = g_action_map_lookup_action(G_ACTION_MAP(g_application_get_default()), "quit");
		g_action_activate(action, NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton), g_value_get_boolean(&prefs));
	g_value_unset(&prefs);
	g_signal_connect(G_OBJECT(checkbutton), "toggled", G_CALLBACK(update_check_toggled_cb), update_dialog);

	gtk_container_set_border_width(GTK_CONTAINER(update_dialog), 10);

	gtk_widget_show_all(update_content);

	dv->progressbar = progressbar;
	dv->button = button;
	dv->update_dialog = update_dialog;

	dv->downloadG = g_signal_connect(button, "clicked", G_CALLBACK(download_button_clicked_cb), dv);

	//construct filename -> platform dependent!!!
	gchar *filename;
	const gchar *downloadfolder;


#if defined(MAC_INTEGRATION)
	//Mac OS X
	filename = g_strdup_printf("XMI-MSIM-%s.dmg",max_version);
	//filename = g_strdup_printf("XMI-MSIM.dmg");
	downloadfolder = xmi_resources_mac_get_user_downloads_dir();

#elif defined(G_OS_WIN32)
	//Win 32
  #ifdef _WIN64
	filename = g_strdup_printf("XMI-MSIM-%s-win64.exe",max_version);
  #else
    #error UnsupportedWindows architecture detected
  #endif
	downloadfolder = g_get_user_special_dir(G_USER_DIRECTORY_DOWNLOAD);
#else
	//Linux??
	filename = g_strdup_printf("xmimsim-%s.tar.gz",max_version);
	downloadfolder = g_get_user_special_dir(G_USER_DIRECTORY_DOWNLOAD);
#endif

	dv->md5sum_tag = g_strdup("none");
	dv->md5sum_tag = g_realloc(dv->md5sum_tag, (32+1) * sizeof(gchar));
	gchar *pattern = g_strdup_printf("MD5 (%s) = %%32s", filename);

	for (i = 1 ; splitted[i] != NULL ; i++) {
		if (sscanf(splitted[i], pattern, dv->md5sum_tag) > 0) {
			break;
		}

	}
	g_strfreev(splitted);
	g_free(pattern);

	if (strcmp(dv->md5sum_tag, "none") != 0) {
		dv->md5sum_comp = g_checksum_new(G_CHECKSUM_MD5);
	}
	else {
		dv->md5sum_comp = NULL;
	}

	//construct download location
	gchar *download_location = g_strdup_printf("%s%s%s",downloadfolder,G_DIR_SEPARATOR_S,filename);
	dv->download_location = download_location;
	dv->filename = filename;

	gint result = gtk_dialog_run(GTK_DIALOG(update_dialog));
	int rv;

	if (result == GTK_RESPONSE_QUIT) {
		//quit XMI-MSIM
		rv = 1;
	}
	else
		rv = 0;

	gtk_widget_destroy(update_dialog);

	return rv;
}

static void download_url_cb(SoupSession *session, SoupMessage *msg, gpointer user_data) {
	GTask *task = user_data;
	gboolean rv = SOUP_STATUS_IS_SUCCESSFUL(msg->status_code);
	if (!rv) {
		g_warning("soup failure: %s", msg->reason_phrase);
	}

	g_object_unref(session);
	g_task_return_boolean(task, rv);
	g_object_unref(task);
}

void xmi_msim_gui_updater_check_download_url_async(GtkListStore *store, gchar *download_url, GAsyncReadyCallback callback, gpointer user_data) {
	gchar *url = NULL;
#if defined(MAC_INTEGRATION)
	//Mac OS X
	url = g_strdup_printf("%s/XMI-MSIM-%s.dmg", download_url, VERSION);
#elif defined(G_OS_WIN32)
	url = g_strdup_printf("%s/XMI-MSIM-%s-win64.exe", download_url, VERSION);
#else
	//Linux??
	url = g_strdup_printf("%s/xmimsim-%s.tar.gz", download_url, VERSION);
#endif
	g_debug("download_url: %s", url);
	GTask *task = g_task_new(store, NULL, callback, user_data);

	gchar *user_agent = g_strdup_printf("XMI-MSIM " PACKAGE_VERSION " updater using libsoup %d.%d.%d", SOUP_MAJOR_VERSION, SOUP_MINOR_VERSION, SOUP_MICRO_VERSION);
	SoupSession *session = soup_session_new_with_options(
		SOUP_SESSION_USER_AGENT, user_agent,
		SOUP_SESSION_TIMEOUT, 10u,
		NULL);
	g_free(user_agent);
	SoupMessage *msg = soup_message_new("HEAD", url);
	soup_session_queue_message (session, msg, download_url_cb, task); // non-blocking!

}

gboolean xmi_msim_gui_updater_check_download_url_finish(GtkListStore *store, GAsyncResult *result) {
	g_return_val_if_fail(g_task_is_valid(result, store), FALSE);
	return g_task_propagate_boolean(G_TASK(result), NULL);
}

GQuark xmi_msim_gui_updater_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-gui-updater-error-quark");
}

