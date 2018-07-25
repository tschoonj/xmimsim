/*
Copyright (C) 2018 Tom Schoonjans and Laszlo Vincze

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
#include <stdio.h>
#include <string.h>
#include <xraylib.h>
#include "xmimsim-gui-application.h"
#include "xmimsim-gui-application-window.h"
#include "xmimsim-gui-colors.h"
#include "xmimsim-gui-source-abstract.h"
#include "xmimsim-gui-source-module.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-private.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-tools.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-undo-manager.h"
#include "xmimsim-gui-xmso-results-scrolled-window.h"
#include "xmimsim-gui-long-task-window.h"
#include "xmimsim-gui-xmsa-viewer-window.h"
#include "xmi_aux.h"
#include "xmi_xml.h"

#ifdef HAVE_GOOGLE_ANALYTICS
  #include "xmimsim-gui-google-analytics.h"
#endif

#ifdef MAC_INTEGRATION
  #include "xmi_resources_mac.h"
#endif

#ifdef __APPLE__
  #include "xmimsim-gui-osx.h"
#endif

#ifdef G_OS_WIN32
  #include <locale.h>
  #include "xmi_registry_win.h"
#endif

#if defined(HAVE_LIBSOUP) && defined(HAVE_JSONGLIB)
  #include "xmimsim-gui-updater.h"
#endif

#include "xmimsim-gui-prefs.h"

#ifdef __APPLE__
  #define XMIMSIM_TITLE_PREFIX ""
#else
  #define XMIMSIM_TITLE_PREFIX "XMI-MSIM: "
#endif

struct _XmiMsimGuiApplication {
	GtkApplication parent_instance;
};

struct _XmiMsimGuiApplicationClass {
	GtkApplicationClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiApplication, xmi_msim_gui_application, GTK_TYPE_APPLICATION)

static void xmi_msim_gui_application_finalize(GObject *gobject) {
	XmiMsimGuiApplication *self = XMI_MSIM_GUI_APPLICATION(gobject);

	G_OBJECT_CLASS(xmi_msim_gui_application_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_application_dispose(GObject *gobject) {
	XmiMsimGuiApplication *self = XMI_MSIM_GUI_APPLICATION(gobject);

	G_OBJECT_CLASS(xmi_msim_gui_application_parent_class)->dispose(gobject);
}

static GOptionEntry option_entries[] = {
	{ "version", 0, 0, G_OPTION_ARG_NONE, NULL, "display version information", NULL },
	{NULL}
};

static gint app_handle_local_options(GApplication *app, GVariantDict *options) {
	if (g_variant_dict_contains(options, "version")) {
		fprintf(stdout, "%s", xmi_version_string());
		return 0;
	}
	return -1;
}

static void preferences_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	GtkApplication *app = GTK_APPLICATION(user_data);
	xmimsim_gui_launch_preferences(gtk_application_get_active_window(app));
}

#ifdef XMIMSIM_GUI_UPDATER_H
static void check_for_updates_on_click_cb(XmiMsimGuiApplication *app);

static void check_for_updates_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	XmiMsimGuiApplication *app = XMI_MSIM_GUI_APPLICATION(user_data);
	check_for_updates_on_click_cb(app);
}
#endif

static void about_activate_link(GtkAboutDialog *about, const gchar *url, gpointer data) {
	if (strncmp(url, "https", 5) == 0) {
		xmi_msim_gui_utils_open_url(url);
	}
	else {
		xmi_msim_gui_utils_open_email(url);
	}
	return;
}

static void about_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {

	GtkApplication *app = GTK_APPLICATION(user_data);

	static const gchar *authors[] = {
		"Tom Schoonjans <Tom.Schoonjans@diamond.ac.uk>",
		"Laszlo Vincze <Laszlo.Vincze@UGent.be>",
		"Vicente Armando Sol\303\251 <sole@esrf.fr>",
		"Philip Brondeel <Philip.Brondeel@UGent.be>",
		"Manuel Sanchez del Rio <srio@esrf.fr>",
		"Claudio Ferrero <ferrero@esrf.fr>",
		NULL
	};

	static const gchar *artists[] = {
		"Jan Garrevoet <Jan.Garrevoet@desy.de>",
		NULL
	};

	static const gchar copyright[] = "Copyright \xc2\xa9 2010-2018 Tom Schoonjans, Philip Brondeel and Laszlo Vincze";

	static const gchar comments[] = "A tool for predicing the spectral response of ED-XRF spectrometers using Monte Carlo simulations\n\n\nPlease read carefully the License section and the links therein.";

	GtkWidget *about_dialog = gtk_about_dialog_new();
	gtk_window_set_transient_for(GTK_WINDOW(about_dialog), gtk_application_get_active_window(app));
	gtk_window_set_modal(GTK_WINDOW(about_dialog), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(about_dialog), TRUE);
	gtk_about_dialog_set_program_name(GTK_ABOUT_DIALOG(about_dialog), "XMI-MSIM");
	gtk_about_dialog_set_authors(GTK_ABOUT_DIALOG(about_dialog), authors);
	gtk_about_dialog_set_comments(GTK_ABOUT_DIALOG(about_dialog), comments);
	gtk_about_dialog_set_copyright(GTK_ABOUT_DIALOG(about_dialog), copyright);
	gtk_about_dialog_set_license(GTK_ABOUT_DIALOG(about_dialog), "This program comes with ABSOLUTELY NO WARRANTY. It is made available under the terms and conditions specified by version 3 of the GNU General Public License. For details, visit http://www.gnu.org/licenses/gpl.html\n\nPlease refer to our paper \"A general Monte Carlo simulation of energy-dispersive X-ray fluorescence spectrometers - Part 5. Polarized radiation, stratified samples, cascade effects, M-lines\" (http://dx.doi.org/10.1016/j.sab.2012.03.011 ) in your manuscripts when using this tool.\n\nWhen using XMI-MSIM through the PyMca quantification interface, please refer to our paper \"A general Monte Carlo simulation of energy-dispersive X-ray fluorescence spectrometers - Part 6. Quantification through iterative simulations\" (http://dx.doi.org/10.1016/j.sab.2012.12.011 ) in your manuscripts.");
	gtk_about_dialog_set_logo_icon_name(GTK_ABOUT_DIALOG(about_dialog), "Logo_xmi_msim");
	gtk_about_dialog_set_artists(GTK_ABOUT_DIALOG(about_dialog), artists);
	gtk_about_dialog_set_version(GTK_ABOUT_DIALOG(about_dialog), VERSION);
	gtk_about_dialog_set_website(GTK_ABOUT_DIALOG(about_dialog), "https://github.com/tschoonj/xmimsim");
	gtk_about_dialog_set_website_label(GTK_ABOUT_DIALOG(about_dialog), "https://github.com/tschoonj/xmimsim");
	gtk_about_dialog_set_wrap_license(GTK_ABOUT_DIALOG(about_dialog), TRUE);
	g_signal_connect(about_dialog, "activate-link", G_CALLBACK(about_activate_link), NULL);

	gtk_dialog_run(GTK_DIALOG(about_dialog));
	gtk_widget_destroy(about_dialog);
}

static void quit_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling quit_activated");

}

static void new_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling new_activated");
	
	g_application_activate(G_APPLICATION(user_data));
}

static void open_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling open_activated");
	
	// launch file chooser and feed selected file(s) to g_application_open
	XmiMsimGuiFileChooserDialog *dialog = NULL;
	GtkFileFilter *filter1, *filter2, *filter3, *filterAll;

	filterAll = gtk_file_filter_new();
	gtk_file_filter_set_name(filterAll, "XMI-MSIM files");

	filter1 = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter1, "*.xmsi");
	gtk_file_filter_add_pattern(filter1, "*.XMSI");
	gtk_file_filter_add_pattern(filterAll, "*.xmsi");
	gtk_file_filter_add_pattern(filterAll, "*.XMSI");
	gtk_file_filter_set_name(filter1, "XMI-MSIM inputfiles");
	filter2 = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter2,"*.xmso");
	gtk_file_filter_add_pattern(filter2,"*.XMSO");
	gtk_file_filter_add_pattern(filterAll, "*.xmso");
	gtk_file_filter_add_pattern(filterAll, "*.XMSO");
	gtk_file_filter_set_name(filter2,"XMI-MSIM outputfiles");
	filter3 = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter3,"*.xmsa");
	gtk_file_filter_add_pattern(filter3,"*.XMSA");
	gtk_file_filter_add_pattern(filterAll, "*.xmsa");
	gtk_file_filter_add_pattern(filterAll, "*.XMSA");
	gtk_file_filter_set_name(filter3,"XMI-MSIM archives");
	dialog = xmi_msim_gui_file_chooser_dialog_new("Open XMI-MSIM files",
		gtk_application_get_active_window(GTK_APPLICATION(user_data)),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		"_Open",
		"_Cancel");
	gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filterAll);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter1);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter2);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter3);

	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		GSList *file_list = gtk_file_chooser_get_files(GTK_FILE_CHOOSER(dialog));
		GPtrArray *file_array = g_ptr_array_new_with_free_func(g_object_unref);
		const GSList *iter;
		for (iter = file_list ; iter; iter = iter->next)
			g_ptr_array_add(file_array, iter->data);
		g_slist_free(file_list);
		GtkWindow *window = gtk_application_get_active_window(GTK_APPLICATION(user_data));
		
		if (XMI_MSIM_GUI_IS_APPLICATION_WINDOW(window)) {
			g_application_open(G_APPLICATION(user_data), (GFile **) file_array->pdata, file_array->len, gtk_widget_get_name(GTK_WIDGET(window)));
		}
		else {
			g_application_open(G_APPLICATION(user_data), (GFile **) file_array->pdata, file_array->len, "");
		}
		g_ptr_array_free(file_array, TRUE);
	}
}

static void batch_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling batch_activated");
}

static void help_url_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling help_url_activated with %s", g_variant_get_string(parameter, NULL));
	xmi_msim_gui_utils_open_url(g_variant_get_string(parameter, NULL));
}

static void help_email_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling help_email_activated with %s", g_variant_get_string(parameter, NULL));
	xmi_msim_gui_utils_open_email(g_variant_get_string(parameter, NULL));
}

static GActionEntry app_entries[] = {
	{"new", new_activated, NULL, NULL, NULL},
	{"open", open_activated, NULL, NULL, NULL},
	{"batch", batch_activated, NULL, NULL, NULL},
	{"preferences", preferences_activated, NULL, NULL, NULL},
#ifdef XMIMSIM_GUI_UPDATER_H
	{"check-for-updates", check_for_updates_activated, NULL, NULL, NULL},
#endif
	{"about", about_activated, NULL, NULL, NULL},
	{"xmsi2xrmc", xmsi2xrmc_activated, NULL, NULL, NULL},
	{"xmso2xmsi", xmso2xmsi_activated, NULL, NULL, NULL},
	{"xmso2csv", xmso2csv_activated, NULL, NULL, NULL},
	{"xmso2spe", xmso2spe_activated, NULL, NULL, NULL},
	{"xmso2html", xmso2html_activated, NULL, NULL, NULL},
	{"xmso2svg", xmso2svg_activated, NULL, NULL, NULL},
	{"xmsa2xmso", xmsa2xmso_activated, NULL, NULL, NULL},
	{"quit", quit_activated, NULL, NULL, NULL},
	{"help-url", help_url_activated, "s", NULL, NULL},
	{"help-email", help_email_activated, "s", NULL, NULL},
};

static void add_accelerator(GtkApplication *app, const gchar *action_name, const gchar *accel) {
#if GTK_CHECK_VERSION(3, 12, 0)
	const gchar *vaccels[] = {accel, NULL};
	gtk_application_set_accels_for_action(app, action_name, vaccels);
#else
	gtk_application_add_accelerator(app, accel, action_name, NULL);
#endif
}

#ifdef XMIMSIM_GUI_UPDATER_H

static gboolean check_for_updates_on_init_cb(XmiMsimGuiApplication *app);

static void check_for_updates_callback(XmiMsimGuiApplication *app, GAsyncResult *result, gpointer user_data) {
	gchar *max_version = NULL;
	gchar *message = NULL;
	GError *error = NULL;
	int rv = 0;

	XmiMsimGuiUpdaterCheck check = xmi_msim_gui_updater_check_for_updates_finish(app, result, &max_version, &message, &error);

	if (check == XMI_MSIM_UPDATES_AVAILABLE) {
		rv = xmi_msim_gui_updater_download_updates_dialog(app, max_version, message);

	}
	else if (user_data == check_for_updates_on_click_cb){
		if (check == XMI_MSIM_UPDATES_ERROR) {
			GtkWindow *active_window = gtk_application_get_active_window(GTK_APPLICATION(app));
			GtkWidget *dialog = gtk_message_dialog_new(active_window,
				GTK_DIALOG_DESTROY_WITH_PARENT,
		       		GTK_MESSAGE_ERROR,
		       		GTK_BUTTONS_CLOSE,
		       		"Could not check for updates"
	                	);
			gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
			g_error_free(error);
	     		gtk_dialog_run (GTK_DIALOG (dialog));
			gtk_widget_destroy(dialog);
		}
		else if (check == XMI_MSIM_UPDATES_NONE) {
			GtkWindow *active_window = gtk_application_get_active_window(GTK_APPLICATION(app));
			GtkWidget *dialog = gtk_message_dialog_new(active_window,
				GTK_DIALOG_DESTROY_WITH_PARENT,
		       		GTK_MESSAGE_INFO,
		       		GTK_BUTTONS_CLOSE,
		       		"No updates available at this moment..."
	                	);
			gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "Please check again later");
	     		gtk_dialog_run(GTK_DIALOG (dialog));
			gtk_widget_destroy(dialog);

		}
	}

	GAction *action = g_action_map_lookup_action(G_ACTION_MAP(app), "check-for-updates");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), TRUE);

	if (rv == 1) {
		//exit XMI-MSIM
		g_application_quit(G_APPLICATION(app));
	}
}

static gboolean check_for_updates_on_init_cb(XmiMsimGuiApplication *app) {
	//do this only if it is allowed by the preferences
	GValue prefs = G_VALUE_INIT;
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, &prefs) == 0) {
		GtkWindow *active_window = gtk_application_get_active_window(GTK_APPLICATION(app));
		GtkWidget *dialog = gtk_message_dialog_new(active_window, GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR , GTK_BUTTONS_CLOSE, "A serious error occurred while checking\nthe preferences file.\nThe program will abort.");
		gtk_dialog_run(GTK_DIALOG(dialog));
	        gtk_widget_destroy(dialog);
		g_application_quit(G_APPLICATION(app));
	}
	if (g_value_get_boolean(&prefs) == FALSE) {
		g_value_unset(&prefs);
		return FALSE;
	}
	g_value_unset(&prefs);

	GAction *action = g_action_map_lookup_action(G_ACTION_MAP(app), "check-for-updates");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), FALSE);
	xmi_msim_gui_updater_check_for_updates_async(app, (GAsyncReadyCallback) check_for_updates_callback, (gpointer) check_for_updates_on_init_cb);

	return FALSE;
}



static void check_for_updates_on_click_cb(XmiMsimGuiApplication *app) {
	GAction *action = g_action_map_lookup_action(G_ACTION_MAP(app), "check-for-updates");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), FALSE);
	xmi_msim_gui_updater_check_for_updates_async(app, (GAsyncReadyCallback) check_for_updates_callback, (gpointer) check_for_updates_on_click_cb);
}
#endif

static void query_source_modules_dir(gchar *dirname) {
	GError *error = NULL;
	GDir *dir = g_dir_open(dirname, 0, &error);

	if (dir == NULL) {
		g_warning("Could not open %s: %s\n", dirname, error->message);
		return;
	}

	// dir exists, let's start creating modules out of these files and load them if possible
	const gchar *file = NULL;
	while ((file = g_dir_read_name(dir)) != NULL) {
		if (!g_str_has_suffix(file, G_MODULE_SUFFIX))
			continue;
		if (!g_str_has_prefix(file, "xmimsim-gui-source-"))
			continue;
		gchar *full_file = g_build_filename(dirname, file, NULL);
		XmiMsimGuiSourceModule *module = xmi_msim_gui_source_module_new(full_file);
		g_free(full_file);
		if (module != NULL && g_type_module_use(G_TYPE_MODULE(module)) == FALSE)
			g_type_module_unuse(G_TYPE_MODULE(module));
	}

	g_dir_close(dir);
}

static void query_source_modules(void) {
	gchar *sources_dir;

	// first add the system-wide modules
	g_debug("Querying system-wide installed modules");
#ifdef G_OS_WIN32
	if (xmi_registry_win_query(XMI_REGISTRY_WIN_SOURCES, &sources_dir) == 0) {
		g_critical("Could not get sources location in registry!");
		return;
	}
#elif defined(MAC_INTEGRATION)
	if (xmi_resources_mac_query(XMI_RESOURCES_MAC_SOURCES, &sources_dir) == 0) {
		g_critical("Could not get sources location in resources!");
		return;
	}
#else
	sources_dir = g_strdup(XMIMSIM_SOURCES_DEFAULT);
#endif
	query_source_modules_dir(sources_dir);
	g_free(sources_dir);

#ifdef MAC_INTEGRATION
        const gchar *config_dir = xmi_resources_mac_get_user_data_dir();
#else
        const gchar *config_dir = g_get_user_config_dir();
#endif

	//first check if the preferences file exists!
	sources_dir = g_strdup_printf("%s" G_DIR_SEPARATOR_S "XMI-MSIM" G_DIR_SEPARATOR_S "sources", config_dir);
	
	g_debug("Querying locally installed modules");
	query_source_modules_dir(sources_dir);
	g_free(sources_dir);

	// get kids
	guint ntypes;
	GType *source_types = g_type_children(XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT, &ntypes);
	guint i;
	for (i = 0 ; i < ntypes ; i++)
		g_debug("source %s found\n", g_type_name(source_types[i]));
}

#ifdef HAVE_GOOGLE_ANALYTICS

static gboolean launch_google_analytics(XmiMsimGuiApplication *app) {
	g_debug("Launching Google Analytics support");

	GValue xpv = G_VALUE_INIT;
	
	xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_GOOGLE_ANALYTICS_SHOW_STARTUP_DIALOG, &xpv);
	gboolean show_startup_dialog = g_value_get_boolean(&xpv);
	g_value_unset(&xpv);

	if (show_startup_dialog) {
		GtkWindow *active_window = gtk_application_get_active_window(GTK_APPLICATION(app));
		GtkWidget *dialog = gtk_message_dialog_new(active_window,
			GTK_DIALOG_DESTROY_WITH_PARENT,
			GTK_MESSAGE_INFO,
			GTK_BUTTONS_CLOSE,
			"XMI-MSIM 7.0 introduced Google Analytics event tracking to give the developers some insight into how often this package is used as well as which of its features are popular. This information may be used in the future to decide where to focus further development on.\n\nCurrently the following events are tracked: XMI-MSIM startup, Simulation start, Batch mode start, Geometry dialog open, Catalog dialog open and Sources dialog Update spectrum button clicks. The developers will also have access to the approximate geographical location (the name of the city) of where the XMI-MSIM session was started, but not the IP address.\n\nDisabling all Google Analytics event tracking is only possible by recompiling XMI-MSIM from source with the --disable-google-analytics option of the configure script.");
		gtk_window_set_title(GTK_WINDOW(dialog), "Google Analytics Event Tracking");
		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		// update prefs
		g_value_init(&xpv, G_TYPE_BOOLEAN);
		g_value_set_boolean(&xpv, FALSE);
		xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_GOOGLE_ANALYTICS_SHOW_STARTUP_DIALOG, &xpv);
		g_value_unset(&xpv);
	}

	xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_GOOGLE_ANALYTICS_UUID, &xpv);
	const gchar *uuid = g_value_get_string(&xpv);
	xmi_msim_gui_google_analytics_tracker_create_global(uuid);
	g_value_unset(&xpv);
	const XmiMsimGuiGoogleAnalyticsTracker *tracker = xmi_msim_gui_google_analytics_tracker_get_global();

#ifdef __APPLE__
	const gchar os[] = "macOS";
#elif defined(__WIN64)
	const gchar os[] = "Windows 64-bit";
#elif defined(__WIN32)
	const gchar os[] = "Windows 32-bit";
#elif defined(__linux)
	const gchar os[] = "Linux";
#else
	const gchar os[] = "Unknown";
#endif

	gchar *event_label = g_strdup_printf("OS: %s VERSION: %s", os, PACKAGE_VERSION);

	xmi_msim_gui_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "LAUNCH", event_label, NULL);
	g_free(event_label);
	return FALSE;
}
#endif

static void app_startup(GApplication *app) {
	// invoke parent method
	G_APPLICATION_CLASS(xmi_msim_gui_application_parent_class)->startup(app);

	// no xraylib error messages!
	SetErrorMessages(0);

	// init hdf5 constants and error messages
	xmi_init_hdf5();

	// init colors and css stuff
	xmi_msim_gui_utils_init_colors();

#ifdef __APPLE__ 
	xmi_msim_gui_osx_app_disable_tabbing();
#endif

	gtk_window_set_default_icon_name("Logo_xmi_msim");

#if defined(G_OS_WIN32)
	setlocale(LC_ALL,"English_United States");
	gchar *installation_dir = g_win32_get_package_installation_directory_of_module(NULL);
	gchar *path_to_plplot = g_build_filename(installation_dir, "Share", "plplot", NULL);
	g_setenv("PLPLOT_LIB", path_to_plplot, TRUE);
	g_free(installation_dir);
	g_free(path_to_plplot);
#else
	g_setenv("LANG","en_US",TRUE);
#endif

#ifdef MAC_INTEGRATION
	char *resource_path = xmi_application_get_resource_path();
	char *gtls_system_ca_file = g_strdup_printf("%s/share/curl/curl-ca-bundle.crt", resource_path);
	g_setenv("GTLS_SYSTEM_CA_FILE", gtls_system_ca_file, TRUE);
	g_free(resource_path);
	g_free(gtls_system_ca_file);
#endif

	setbuf(stdout, NULL);

	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		g_warning("Could not load XML catalog!!!");
		return;
	}
	// populate action map
	g_action_map_add_action_entries(G_ACTION_MAP(app), app_entries, G_N_ELEMENTS(app_entries), app);

	// set accelerators
	add_accelerator(GTK_APPLICATION(app), "app.new", "<Primary>N");
	add_accelerator(GTK_APPLICATION(app), "app.quit", "<Primary>Q");
	add_accelerator(GTK_APPLICATION(app), "app.open", "<Primary>O");
	add_accelerator(GTK_APPLICATION(app), "win.save", "<Primary>S");
	add_accelerator(GTK_APPLICATION(app), "win.save-as", "<Primary><Shift>S");
	add_accelerator(GTK_APPLICATION(app), "win.close", "<Primary>W");
	add_accelerator(GTK_APPLICATION(app), "win.undo", "<Primary>Z");
	add_accelerator(GTK_APPLICATION(app), "win.redo", "<Primary><Shift>Z");
	add_accelerator(GTK_APPLICATION(app), "win.cut", "<Primary>X");
	add_accelerator(GTK_APPLICATION(app), "win.copy", "<Primary>C");
	add_accelerator(GTK_APPLICATION(app), "win.paste", "<Primary>V");

#ifdef __APPLE__
	GtkBuilder *builder = gtk_builder_new_from_resource("/com/github/tschoonj/xmimsim/gui/gtk/menus-appmenu.ui");
	GMenuModel *app_menu = G_MENU_MODEL(gtk_builder_get_object(builder, "app-menu"));
	gtk_application_set_app_menu(GTK_APPLICATION(app), app_menu);
	g_object_unref(builder);
#endif

	// import sources
	query_source_modules();

#ifdef XMIMSIM_GUI_UPDATER_H
	g_idle_add((GSourceFunc) check_for_updates_on_init_cb, app);
#endif
#ifdef HAVE_GOOGLE_ANALYTICS
	g_idle_add((GSourceFunc) launch_google_analytics, app);
#endif

}

static void app_activate(GApplication *app) {
	g_debug("Calling app_activate");
	GtkWidget *window = xmi_msim_gui_application_window_new(XMI_MSIM_GUI_APPLICATION(app));
	gtk_widget_show(window);
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

static void viewer_cb(GtkWidget *viewer, gchar *filename) {
	gchar *basename = g_path_get_basename(filename);
	gchar *xmimsim_title_xmsa = g_strdup_printf(XMIMSIM_TITLE_PREFIX "%s", basename);
	g_free(basename);
	gtk_window_set_title(GTK_WINDOW(viewer), xmimsim_title_xmsa);
	g_free(xmimsim_title_xmsa);
#ifdef __APPLE__
	xmi_msim_gui_osx_nswindow_set_file(viewer, filename);
#endif
	g_free(filename);
}

static void read_xmsa_callback(GtkWidget *task_window, GAsyncResult *result, GtkWidget *active_window) {

	GError *error = NULL;
	struct xmi_archive *archive = xmi_msim_gui_utils_read_xmsa_finish(task_window, result, &error);
	GTask *task = G_TASK(result);

	gdk_window_set_cursor(gtk_widget_get_window(task_window), NULL);
	gtk_widget_destroy(task_window);

	if (!archive) {
		GtkWidget *message_dialog = gtk_message_dialog_new(
			GTK_WINDOW(active_window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
			GTK_MESSAGE_ERROR,
			GTK_BUTTONS_CLOSE,
			"Could not read file %s", (char *) g_task_get_task_data(task)
		);
		gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(message_dialog), "%s", error->message);
	    	gtk_dialog_run(GTK_DIALOG(message_dialog));
		gtk_widget_destroy(message_dialog);
		g_application_release(g_application_get_default());
		return ;
	}
	GtkWidget *viewer = xmi_msim_gui_xmsa_viewer_window_new(XMI_MSIM_GUI_APPLICATION(g_application_get_default()), archive);
	g_signal_connect(viewer, "realize", G_CALLBACK(viewer_cb), g_strdup((const gchar*) g_task_get_task_data(task)));
	gtk_widget_show(viewer);
	g_application_release(g_application_get_default());
}

static void app_open(GApplication *app, GFile **files, gint n_files, const gchar *hint) {
	g_debug("Calling app_open with hint %s and %d files", hint, n_files);

	if (n_files == 1 && strlen(hint) > 0) {
		// in this case the file needs to be either xmsi or xmso
		// get window
		GList *windows = gtk_application_get_windows(GTK_APPLICATION(app));
		XmiMsimGuiApplicationWindow *window = NULL;
		GList *temp;
		for (temp = windows ; temp != NULL ; temp = temp->next) {
			g_debug("app_open: window name is %s", gtk_widget_get_name(GTK_WIDGET(temp->data)));
			if (XMI_MSIM_GUI_IS_APPLICATION_WINDOW(temp->data) && g_strcmp0(hint, gtk_widget_get_name(GTK_WIDGET(temp->data))) == 0) {
				window = XMI_MSIM_GUI_APPLICATION_WINDOW(temp->data);
				break;
			}	
		}
		if (window == NULL) {
			g_warning("app_open: hint refers to non-existent window!");
			return;
		}

		GError *error = NULL;
		gchar *filename = g_file_get_path(files[0]);
		if (g_ascii_strcasecmp(filename + strlen(filename) - 5, ".xmsi") == 0) {
			if (xmi_msim_gui_undo_manager_get_status(window->undo_manager) == XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_DEFAULT) {
				// use current window
				xmi_msim_gui_application_window_load_file(window, filename, &error);
			}
			else {
				// use new window
				GtkWidget *new_window = xmi_msim_gui_application_window_new(XMI_MSIM_GUI_APPLICATION(app));
				gtk_window_present(GTK_WINDOW(new_window));
				if (!xmi_msim_gui_application_window_load_file(XMI_MSIM_GUI_APPLICATION_WINDOW(new_window), filename, &error)) {
					gtk_widget_destroy(new_window);
				}
			}
		}
		else if (g_ascii_strcasecmp(filename + strlen(filename) - 5, ".xmso") == 0) {
			xmi_msim_gui_xmso_results_scrolled_window_load_from_file(XMI_MSIM_GUI_XMSO_RESULTS_SCROLLED_WINDOW(window->results_page), filename, &error);
		}
		if (error) {
			GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
		       		GTK_MESSAGE_ERROR,
		       		GTK_BUTTONS_CLOSE,
		       		"Could not load %s",
				filename
	                	);
			gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
			g_error_free(error);
	     		gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
		}
		g_free(filename);
		return;
	}

	int i;

	for (i = 0 ; i < n_files ; i++) {
		GFile *file = files[i];
		gchar *filename = g_file_get_path(file);
		GError *error = NULL;
		GtkWidget *error_dialog = NULL;
		GtkWindow *active_window = gtk_application_get_active_window(GTK_APPLICATION(app));

		if (g_ascii_strcasecmp(filename + strlen(filename) - 5, ".xmsi") == 0) {
			GtkWidget *window = xmi_msim_gui_application_window_new(XMI_MSIM_GUI_APPLICATION(app));
			gtk_window_present(GTK_WINDOW(window));
			if (xmi_msim_gui_application_window_load_file(XMI_MSIM_GUI_APPLICATION_WINDOW(window), filename, &error) == FALSE) {
				gtk_widget_destroy(window);
				error_dialog = gtk_message_dialog_new(active_window,
					GTK_DIALOG_DESTROY_WITH_PARENT,
					GTK_MESSAGE_ERROR,
					GTK_BUTTONS_CLOSE,
					"Could not load %s:", filename
				);
				gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(error_dialog), "%s", error->message);
				g_error_free(error);
			}
		}
		else if (g_ascii_strcasecmp(filename + strlen(filename) - 5, ".xmso") == 0) {
			GtkWidget *window = gtk_application_window_new(GTK_APPLICATION(app));
			g_action_map_add_action_entries(G_ACTION_MAP(window), xmso_win_entries, G_N_ELEMENTS(xmso_win_entries), window);
			gtk_window_set_default_size(GTK_WINDOW(window), 900, 900);
			gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
			gtk_container_set_border_width(GTK_CONTAINER(window), 10);
			GtkWidget *xmso_sw = xmi_msim_gui_xmso_results_scrolled_window_new();
			gtk_container_add(GTK_CONTAINER(window), xmso_sw);
			g_signal_connect(window, "realize", G_CALLBACK(viewer_cb), g_strdup((const gchar*) filename));
			if (xmi_msim_gui_xmso_results_scrolled_window_load_from_file(XMI_MSIM_GUI_XMSO_RESULTS_SCROLLED_WINDOW(xmso_sw), filename, &error) == FALSE) {
				gtk_widget_destroy(window);
				error_dialog = gtk_message_dialog_new(active_window,
					GTK_DIALOG_DESTROY_WITH_PARENT,
					GTK_MESSAGE_ERROR,
					GTK_BUTTONS_CLOSE,
					"Could not load %s:", filename
				);
				gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(error_dialog), "%s", error->message);
				g_error_free(error);
			}
			gtk_widget_show_all(window);
		}
		else if (g_ascii_strcasecmp(filename + strlen(filename) - 5, ".xmsa") == 0) {
			GtkWidget *task_window = xmi_msim_gui_long_task_window_new(active_window); // disable modal???
			//gtk_window_set_application(GTK_WINDOW(task_window), GTK_APPLICATION(app));
			g_application_hold(app);
			xmi_msim_gui_long_task_window_set_text(XMI_MSIM_GUI_LONG_TASK_WINDOW(task_window), "<b>Reading XMSA file</b>");
			gtk_widget_show(task_window);
			GdkCursor *watch_cursor = gdk_cursor_new_for_display(gdk_display_get_default(), GDK_WATCH);
			gdk_window_set_cursor(gtk_widget_get_window(task_window), watch_cursor);

			xmi_msim_gui_utils_read_xmsa_async(task_window, filename, (GAsyncReadyCallback) read_xmsa_callback, active_window);
			filename = NULL;
		}
		else {
			error_dialog = gtk_message_dialog_new(active_window,
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Could not load %s:", filename
			);
			gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(error_dialog), "Unsupported filetype");
		}

		if (error_dialog) {
			gtk_dialog_run(GTK_DIALOG(error_dialog));
			gtk_widget_destroy(error_dialog);
		}
		g_free(filename);
	}
}

static void app_shutdown(GApplication *app) {
	g_debug("Calling app_shutdown");
	// invoke parent method
	G_APPLICATION_CLASS(xmi_msim_gui_application_parent_class)->shutdown(app);

}

static void xmi_msim_gui_application_class_init(XmiMsimGuiApplicationClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);
	GApplicationClass *app_class = G_APPLICATION_CLASS(klass);

	object_class->dispose = xmi_msim_gui_application_dispose;
	object_class->finalize = xmi_msim_gui_application_finalize;

	app_class->handle_local_options = app_handle_local_options;
	app_class->startup = app_startup;
	app_class->activate = app_activate;
	app_class->open = app_open;
	app_class->shutdown = app_shutdown;
}

static void xmi_msim_gui_application_init(XmiMsimGuiApplication *self) {
	g_set_application_name("XMI-MSIM");
	g_application_add_main_option_entries(G_APPLICATION(self), option_entries);
#if GLIB_CHECK_VERSION(2, 56, 0)
	g_application_set_option_context_parameter_string(G_APPLICATION(self), "XMSI/XMSO/XMSA file(s)");
	g_application_set_option_context_summary(G_APPLICATION(self), "xmimsim-gui: a graphical user interface for creating XMSI files and visualizing XMSO files\n");
#endif

}

XmiMsimGuiApplication* xmi_msim_gui_application_new(void) {
	return g_object_new(XMI_MSIM_GUI_TYPE_APPLICATION, "application-id", "com.github.tschoonj.xmimsim.gui", "flags", G_APPLICATION_HANDLES_OPEN, NULL);
}
