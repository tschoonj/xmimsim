/*
Copyright (C) 2019 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-xmso-results-application-window.h"
#include "xmimsim-gui-xmso-results-scrolled-window.h"

#ifdef __APPLE__
  #include "xmimsim-gui-osx.h"
#endif

#ifdef __APPLE__
  #define XMIMSIM_TITLE_PREFIX ""
#else
  #define XMIMSIM_TITLE_PREFIX "XMI-MSIM: "
#endif

struct _XmiMsimGuiXmsoResultsApplicationWindow {
	GtkApplicationWindow parent_instance;
	GtkWidget *scrolled_window;
};

struct _XmiMsimGuiXmsoResultsApplicationWindowClass {
	GtkApplicationWindowClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiXmsoResultsApplicationWindow, xmi_msim_gui_xmso_results_application_window, GTK_TYPE_APPLICATION_WINDOW)

static void xmi_msim_gui_xmso_results_application_window_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_xmso_results_application_window_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_xmso_results_application_window_finalize(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_xmso_results_application_window_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_xmso_results_application_window_class_init(XmiMsimGuiXmsoResultsApplicationWindowClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_xmso_results_application_window_dispose;
	object_class->finalize = xmi_msim_gui_xmso_results_application_window_finalize;
}

static void xmso_close_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	gtk_widget_destroy(GTK_WIDGET(user_data));
}

static void minimize_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	gtk_window_iconify(GTK_WINDOW(user_data));
}

static GActionEntry xmso_win_entries[] = {
	{"close", xmso_close_activated, NULL, NULL, NULL},
	{"minimize", minimize_activated, NULL, NULL, NULL},
};

static void viewer_cb(XmiMsimGuiXmsoResultsApplicationWindow *viewer, gpointer data) {
	// get filename from child
	XmiMsimGuiXmsoResultsScrolledWindow *window = XMI_MSIM_GUI_XMSO_RESULTS_SCROLLED_WINDOW(gtk_bin_get_child(GTK_BIN(viewer)));
	const gchar *filename = xmi_msim_gui_xmso_results_scrolled_window_get_filename(window);
	gchar *basename = g_path_get_basename(filename);
	gchar *xmimsim_title_xmsa = g_strdup_printf(XMIMSIM_TITLE_PREFIX "%s", basename);
	g_free(basename);
	gtk_window_set_title(GTK_WINDOW(viewer), xmimsim_title_xmsa);
	g_free(xmimsim_title_xmsa);
#ifdef __APPLE__
	xmi_msim_gui_osx_window_set_file(GTK_WIDGET(viewer), filename);
#endif
}

static void xmi_msim_gui_xmso_results_application_window_init(XmiMsimGuiXmsoResultsApplicationWindow *self) {
	g_action_map_add_action_entries(G_ACTION_MAP(self), xmso_win_entries, G_N_ELEMENTS(xmso_win_entries), self);
	gtk_window_set_default_size(GTK_WINDOW(self), 900, 900);
	gtk_window_set_position(GTK_WINDOW(self), GTK_WIN_POS_CENTER);
	gtk_container_set_border_width(GTK_CONTAINER(self), 10);
	self->scrolled_window = xmi_msim_gui_xmso_results_scrolled_window_new();
	gtk_container_add(GTK_CONTAINER(self), self->scrolled_window);
	g_signal_connect(self, "realize", G_CALLBACK(viewer_cb), NULL);
}

/**
 * xmi_msim_gui_xmso_results_application_window_new: (constructor)
 * @app: (nullable): an instance of #XmiMsimGuiApplication.
 * @filename: (nullable): the name of a file that the window should immediately load.
 *
 * Returns: a fresly initialized #XmiMsimGuiXmsoResultsApplicationWindow
 */
GtkWidget* xmi_msim_gui_xmso_results_application_window_new(XmiMsimGuiApplication* app, gchar *filename, GError **error) {

	XmiMsimGuiXmsoResultsApplicationWindow *rv = 
		g_object_new(
			XMI_MSIM_GUI_TYPE_XMSO_RESULTS_APPLICATION_WINDOW,
			"application", app,
			NULL);

	if (filename && xmi_msim_gui_xmso_results_scrolled_window_load_from_file(XMI_MSIM_GUI_XMSO_RESULTS_SCROLLED_WINDOW(rv->scrolled_window), filename, error) == FALSE) {
		gtk_widget_destroy(GTK_WIDGET(rv));
		return NULL;
	}
	return GTK_WIDGET(rv);
}
