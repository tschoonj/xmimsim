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
#include <math.h>
#include <xraylib.h>
#include <string.h>

#ifdef G_OS_WIN32
#include <windows.h>
#include <Shellapi.h>
#elif defined(__APPLE__)
#include <CoreFoundation/CFBundle.h>
#include <ApplicationServices/ApplicationServices.h>
#else
#include <unistd.h>
#endif

double xmi_msim_gui_utils_get_solid_angle_from_slits(xmi_geometry *geometry) {
	//calculate solid angle based on slits
	double solid_angle = 4.0 * atan(geometry->slit_size_x * geometry->slit_size_y/(2.0*geometry->d_source_slit*sqrt(4.0 * geometry->d_source_slit * geometry->d_source_slit + geometry->slit_size_x * geometry->slit_size_x + geometry->slit_size_y * geometry->slit_size_y)));

	return solid_angle;
}

static void read_xmsa_thread(GTask *task, gpointer source_object, gpointer task_data, GCancellable *cancellable) {
	GError *error = NULL;
	xmi_archive *archive = NULL;
	if (archive = xmi_archive_read_from_xml_file(task_data, &error)) {
		g_task_return_error(task, error);
		g_object_unref(task);
		return;
	}
	g_task_return_pointer(task, archive, (GDestroyNotify) xmi_archive_free);
}

void xmi_msim_gui_utils_read_xmsa_async(GtkWidget *dialog, const gchar *filename, GAsyncReadyCallback callback, gpointer user_data) {
	GTask *task = g_task_new(dialog, NULL, callback, user_data);
	g_task_set_task_data(task, g_strdup(filename), g_free);
	g_task_run_in_thread(task, read_xmsa_thread);
	g_object_unref(task);
}

xmi_archive* xmi_msim_gui_utils_read_xmsa_finish(GtkWidget *dialog, GAsyncResult *result, GError **error) {
	return g_task_propagate_pointer(G_TASK(result), error);
}

void xmi_msim_gui_utils_open_email(const char *address) {
	char *link;

	link = g_strdup_printf("mailto:%s",address);

#ifdef __APPLE__
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
#ifdef __APPLE__
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

void xmi_msim_gui_utils_ensure_extension(gchar **filename, const gchar *extension) {
	if (extension == NULL)
		return;
	GString *string = g_string_new(*filename);
	g_free(*filename);
	if (g_ascii_strcasecmp(string->str + string->len - strlen(extension), extension) != 0) {
		g_string_append(string, extension);
	}
	*filename = g_string_free(string, FALSE);
}

gchar* xmi_msim_gui_utils_get_layer_element_string(xmi_layer *layer) {
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

