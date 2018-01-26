/*
Copyright (C) 2016-2017 Tom Schoonjans and Laszlo Vincze

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

#include "xmimsim-gui-utils.h"
#ifdef HAVE_LIBSOUP
#include <libsoup/soup.h>
#endif
#include <math.h>
#include <xraylib.h>
#include <string.h>

#ifdef G_OS_WIN32
#include <windows.h>
#include <Shellapi.h>
#elif defined(MAC_INTEGRATION)
#include <CoreFoundation/CFBundle.h>
#include <ApplicationServices/ApplicationServices.h>
#else
#include <unistd.h>
#endif

double xmi_msim_gui_utils_get_solid_angle_from_slits(struct xmi_geometry *geometry) {
	//calculate solid angle based on slits
	double solid_angle = 4.0 * atan(geometry->slit_size_x * geometry->slit_size_y/(2.0*geometry->d_source_slit*sqrt(4.0 * geometry->d_source_slit * geometry->d_source_slit + geometry->slit_size_x * geometry->slit_size_x + geometry->slit_size_y * geometry->slit_size_y)));

	return solid_angle;
}

void xmi_msim_gui_utils_update_button_text(GtkWidget *button, const gchar *text) {
	//this function is a hack and may not work on Gtk3
	GList *children = gtk_container_get_children(GTK_CONTAINER(button));
	GtkWidget *temp = (GtkWidget *) g_list_nth_data(children, 0);
	g_list_free(children);
	children = gtk_container_get_children(GTK_CONTAINER(temp));
	temp = (GtkWidget *) g_list_nth_data(children, 0);
	g_list_free(children);
	children = gtk_container_get_children(GTK_CONTAINER(temp));
	gtk_label_set_text(GTK_LABEL((GtkWidget *) g_list_nth_data(children,1)), text);
	g_list_free(children);
	return;
}

GtkWidget *xmi_msim_gui_utils_long_job_dialog(GtkWidget *parent, const gchar *message_with_markup) {
	GtkWidget *dialog = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_decorated(GTK_WINDOW(dialog), FALSE);
	gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(parent));
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
	gtk_window_set_position (GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
	GtkWidget *main_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_set_homogeneous(GTK_BOX(main_vbox), FALSE);
	GtkWidget *label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), message_with_markup);
	gtk_box_pack_start(GTK_BOX(main_vbox), label, TRUE, FALSE, 10);
	label = gtk_label_new("This may take a while...");
	gtk_box_pack_start(GTK_BOX(main_vbox), label, FALSE, FALSE, 10);
	gtk_container_add(GTK_CONTAINER(dialog), main_vbox);
	gtk_container_set_border_width(GTK_CONTAINER(dialog),5);
	gtk_window_set_default_size(GTK_WINDOW(dialog),200,50);
	g_signal_connect(G_OBJECT(dialog), "delete-event", G_CALLBACK(gtk_true), NULL);

	return dialog;
}

gpointer xmi_msim_gui_utils_read_xmsa_thread(struct read_xmsa_data *rxd) {
	return GINT_TO_POINTER(xmi_read_archive_xml(rxd->filename, rxd->archive, NULL));
}

void xmi_msim_gui_utils_open_email(const char *address) {
	char *link;

	link = g_strdup_printf("mailto:%s",address);

#ifdef MAC_INTEGRATION
	CFURLRef url = CFURLCreateWithBytes (
      	NULL,
      	(UInt8*)link,
      	strlen(link),
      	kCFStringEncodingASCII,
      	NULL
    	);
  	LSOpenCFURLRef(url,NULL);
  	CFRelease(url);
#elif defined(G_OS_WIN32)
	ShellExecute(NULL, "open", link, NULL, NULL, SW_SHOWNORMAL);
#else
	pid_t pid;
	char * const argv[] = {(char *) "xdg-email", link, NULL};
	//argv[0] = "xdg-email";
	//argv[1] = link;
	//argv[2] = NULL;

	pid = fork();
	if (!pid)
		execvp(argv[0], argv);
#endif
	g_free(link);
	return;

}

void xmi_msim_gui_utils_open_url(const char *link) {
#ifdef MAC_INTEGRATION
	CFURLRef url = CFURLCreateWithBytes (
      	NULL,
      	(UInt8*)link,
      	strlen(link),
      	kCFStringEncodingASCII,
      	NULL
    	);
  	LSOpenCFURLRef(url,NULL);
  	CFRelease(url);
#elif defined(G_OS_WIN32)
	ShellExecute(NULL, "open", link, NULL, NULL, SW_SHOWNORMAL);
#else
	pid_t pid;
	char * const argv[] = {(char *) "xdg-open", (char *) link, NULL};
	//argv[0] = "xdg-open";
	//argv[1] = link;
	//argv[2] = NULL;

	pid = fork();
	if (!pid)
		execvp(argv[0], argv);
#endif
	return;
}

#ifdef HAVE_LIBSOUP
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

void xmi_msim_gui_utils_check_download_url_async(GtkListStore *store, gchar *download_url, GAsyncReadyCallback callback, gpointer user_data) {
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

gboolean xmi_msim_gui_utils_check_download_url_finish(GtkListStore *store, GAsyncResult *result) {
	g_return_val_if_fail(g_task_is_valid(result, store), FALSE);
	return g_task_propagate_boolean(G_TASK(result), NULL);
}
#endif

void xmi_msim_gui_utils_ensure_extension(gchar **filename, const gchar *extension) {
	GString *string = g_string_new(*filename);
	g_free(*filename);
	if (g_ascii_strcasecmp(string->str + string->len - strlen(extension), extension) != 0) {
		g_string_append(string, extension);
	}
	*filename = g_string_free(string, FALSE);
}

gchar* xmi_msim_gui_utils_get_layer_element_string(struct xmi_layer *layer) {
	GString *rv = g_string_new(NULL);
	int j;

	for (j = 0 ; j < layer->n_elements ; j++) {
		char *symbol = AtomicNumberToSymbol(layer->Z[j]);
		g_string_append(rv, symbol);
		xrlFree(symbol);
		if (j != layer->n_elements-1) {
			g_string_append(rv, ", ");
		}
	}
	return g_string_free(rv, FALSE);
}

void xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(GtkWidget *controlsLogW, GTimer *timer, GtkTextBuffer *buffer, const gchar *text, gint len, GtkTextTag *first_tag, ...) {
	GtkTextIter iter, start;
	va_list args;
	GtkTextTag *tag;
	GtkTextMark *insert_mark;
	gint start_offset;
	gchar *to_print;

	g_return_if_fail(GTK_IS_TEXT_BUFFER(buffer));
	g_return_if_fail(text != NULL);

	if (timer != NULL) {
		glong time_elapsed = (glong) g_timer_elapsed(timer,NULL);
		glong hours = time_elapsed / 3600;
		time_elapsed = time_elapsed % 3600;
		glong minutes = time_elapsed / 60;
		glong seconds = time_elapsed % 60;

		to_print = g_strdup_printf("%02ld:%02ld:%02ld %s", hours, minutes, seconds, text);
	}
	else {
		to_print = g_strdup_printf("%s\n", text);
	}

	gtk_text_buffer_get_end_iter(buffer, &iter);

	start_offset = gtk_text_iter_get_offset(&iter);
	gtk_text_buffer_insert(buffer, &iter, to_print,len);

	g_free(to_print);

	if (first_tag == NULL) {
		gtk_text_buffer_get_end_iter(buffer, &iter);
		insert_mark = gtk_text_buffer_get_insert(buffer);
		gtk_text_buffer_place_cursor(buffer,&iter);
		if (controlsLogW)
        		gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW(controlsLogW), insert_mark, 0.0, FALSE, 0, 1.0);
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
	insert_mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_place_cursor(buffer,&iter);
	if (controlsLogW)
       		gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (controlsLogW), insert_mark, 0.0, FALSE, 0, 1.0);

}

GArray* xmi_msim_gui_utils_tree_view_get_selected_indices(GtkTreeView *tree) {
	GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	GList *list = gtk_tree_selection_get_selected_rows(selection, NULL);

	guint n_selected = g_list_length(list);
	guint i;

	GArray *rv = g_array_sized_new(FALSE, FALSE, sizeof(int), n_selected);

	for (i = 0 ; i < n_selected ; i++) {
		GtkTreePath *path = (GtkTreePath *) g_list_nth_data(list, i);
		gint *indices = gtk_tree_path_get_indices(path);
		g_array_append_val(rv, indices[0]);
	}

	g_list_free_full (list, (GDestroyNotify) gtk_tree_path_free);

	return rv;
}

