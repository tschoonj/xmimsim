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

// This file is heavily influenced by GeditPluginsEngine

#include <config.h>
#include "xmimsim-gui-plugins-engine.h"
#include <libpeas/peas.h>

#ifdef MAC_INTEGRATION
  #include "xmi_resources_mac.h"
#endif

#ifdef G_OS_WIN32
  #include <windows.h>
  #include "xmi_registry_win.h"
#endif

struct _XmiMsimGuiPluginsEngine {
	PeasEngine parent_instance;
};

struct _XmiMsimGuiPluginsEngineClass {
	PeasEngineClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiPluginsEngine, xmi_msim_gui_plugins_engine, PEAS_TYPE_ENGINE)

static XmiMsimGuiPluginsEngine *default_engine = NULL;

struct gir_pair {
	gchar *namespace;
	gchar *version;
};

static struct gir_pair gir_pairs[] = {
	{"Peas", "1.0"},
	{"XmiMsim", "1.0"},
	{"XmiMsimGui", "1.0"},
};

static void plugin_load(PeasPluginInfo *info, XmiMsimGuiPluginsEngine *engine) {
	if (!peas_engine_load_plugin(PEAS_ENGINE(engine), info)) {
		g_warning("Could not load plugin %s", peas_plugin_info_get_name(info));
	}
}

#ifdef G_OS_WIN32
// inspired by gobject-introspection...

static HMODULE libxmimsimgui_dll = NULL;

#ifdef DLL_EXPORT

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved);

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved) {
	if (fdwReason == DLL_PROCESS_ATTACH)
		libxmimsimgui_dll = hinstDLL;
	return TRUE;
}
#endif
#endif

static void xmi_msim_gui_plugins_engine_init(XmiMsimGuiPluginsEngine *engine) {
	GError *error = NULL;
	unsigned int i;

	peas_engine_enable_loader(PEAS_ENGINE(engine), "python3");

	/* Require XMI-MSIM's typelibs. */
	// TODO FIXME for Windows/macOS, this will be complicated and will require fooling around with g_irepository_prepend_search_path (https://developer.gnome.org/gi/stable/GIRepository.html#g-irepository-prepend-search-path)
#ifdef G_OS_WIN32
	gchar *installation_dir = g_win32_get_package_installation_directory_of_module(libxmimsimgui_dll);
	gchar *xmimsim_typelib_dir = g_build_filename(installation_dir, "lib", "girepository-1.0", NULL);
	g_irepository_prepend_search_path(xmimsim_typelib_dir);
	g_free(xmimsim_typelib_dir);
	g_free(installation_dir);
#elif defined(MAC_INTEGRATION)
	gchar *resource_path = xmi_application_get_resource_path();
	gchar *xmimsim_typelib_dir = g_build_filename(resource_path, "lib", "girepository-1.0", NULL);
	g_irepository_prepend_search_path(xmimsim_typelib_dir);
	g_free(xmimsim_typelib_dir);
	g_free(resource_path);
#endif

	for (i = 0 ; i < G_N_ELEMENTS(gir_pairs) ; i++) {
		g_debug("Loading %s-%s", gir_pairs[i].namespace, gir_pairs[i].version);
		if (!g_irepository_require(g_irepository_get_default (), gir_pairs[i].namespace, gir_pairs[i].version, 0, &error)) {
			g_warning("Could not load %s repository: %s", gir_pairs[i].namespace, error->message);
			g_error_free(error);
			error = NULL;
			break;
		}
	}

	// embedded plugins
	peas_engine_add_search_path(PEAS_ENGINE(engine), "resource:///com/github/tschoonj/xmimsim/gui/sources", NULL);

	gchar *sources_dir = NULL;

	// system-wide plugins
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
	peas_engine_add_search_path(PEAS_ENGINE(engine), sources_dir, NULL);
	g_free(sources_dir);

        const gchar *docs_dir = g_get_user_special_dir(G_USER_DIRECTORY_DOCUMENTS);

	sources_dir = g_build_filename(docs_dir, "XMI-MSIM", "sources", NULL);
	peas_engine_add_search_path(PEAS_ENGINE(engine), sources_dir, NULL);
	g_free(sources_dir);

	const GList* plugin_list = peas_engine_get_plugin_list(PEAS_ENGINE(engine));

	g_list_foreach((GList *) plugin_list, (GFunc) plugin_load, engine);
}

static void xmi_msim_gui_plugins_engine_dispose(GObject *object) {
	XmiMsimGuiPluginsEngine *engine = XMI_MSIM_GUI_PLUGINS_ENGINE (object);

	G_OBJECT_CLASS(xmi_msim_gui_plugins_engine_parent_class)->dispose (object);
}

static void xmi_msim_gui_plugins_engine_class_init(XmiMsimGuiPluginsEngineClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->dispose = xmi_msim_gui_plugins_engine_dispose;
}

XmiMsimGuiPluginsEngine* xmi_msim_gui_plugins_engine_get_default(void) {
	if (default_engine == NULL) {
		default_engine = XMI_MSIM_GUI_PLUGINS_ENGINE(g_object_new(XMI_MSIM_GUI_TYPE_PLUGINS_ENGINE, NULL));

		g_object_add_weak_pointer(G_OBJECT(default_engine), (gpointer) &default_engine);
	}

	return default_engine;
}
