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

#include <config.h>
#include "xmimsim-gui-source-module.h"
#include <string.h>


G_DEFINE_TYPE(XmiMsimGuiSourceModule, xmi_msim_gui_source_module, G_TYPE_TYPE_MODULE)


static gboolean xmi_msim_gui_source_module_real_load(GTypeModule *module);

static void xmi_msim_gui_source_module_real_unload(GTypeModule *module);

static void xmi_msim_gui_source_module_finalize(GObject *object);

static void xmi_msim_gui_source_module_class_init(XmiMsimGuiSourceModuleClass *klass) {
  GTypeModuleClass *type_module_class = G_TYPE_MODULE_CLASS(klass);
  GObjectClass *object_class = G_OBJECT_CLASS(klass);

  type_module_class->load = xmi_msim_gui_source_module_real_load;
  type_module_class->unload = xmi_msim_gui_source_module_real_unload;

  object_class->finalize = xmi_msim_gui_source_module_finalize;
}

static void xmi_msim_gui_source_module_init(XmiMsimGuiSourceModule *module) {
	module->filename = NULL;
	module->library = NULL;
	module->initialized = FALSE;
}

XmiMsimGuiSourceModule *xmi_msim_gui_source_module_new (const gchar *filename) {
	XmiMsimGuiSourceModule *module;

	g_return_val_if_fail(filename != NULL, NULL);

	module = (XmiMsimGuiSourceModule *) g_object_new(XMI_MSIM_GUI_TYPE_SOURCE_MODULE, NULL);
 	module->filename = g_strdup(filename);

	return module;
}

void xmi_msim_gui_source_module_finalize(GObject *object) {
	XmiMsimGuiSourceModule *module = XMI_MSIM_GUI_SOURCE_MODULE(object);

	g_free(module->filename);

	G_OBJECT_CLASS(xmi_msim_gui_source_module_parent_class)->finalize(object);
}

gboolean xmi_msim_gui_source_module_real_load(GTypeModule *gmodule) {
	XmiMsimGuiSourceModule *module = XMI_MSIM_GUI_SOURCE_MODULE(gmodule);

	g_return_val_if_fail(module->filename, FALSE);

	module->library = g_module_open (module->filename, (GModuleFlags) (G_MODULE_BIND_LAZY | G_MODULE_BIND_LOCAL));

	if (!module->library) {
		g_warning("%s\n", g_module_error ());
		return FALSE;
	}

	/* get the load and unload functions */
	GError *error = NULL;
	GRegex *regex = g_regex_new("xmimsim-gui-source-(.+)." G_MODULE_SUFFIX, (GRegexCompileFlags) 0, (GRegexMatchFlags) 0, &error);
	if (regex == NULL) {
		g_warning("regex compile error: %s\n", error->message);
		return FALSE;
	}
	GMatchInfo *match_info;
	if (g_regex_match(regex, module->filename, (GRegexMatchFlags) 0, &match_info) == FALSE) {
		g_warning("regex: no match\n");
		return FALSE;
	}
	gchar *class_name_short = g_match_info_fetch(match_info, 1);

	/* replace dashes with underscores */
	int i;
	for (i = 0 ; i < strlen(class_name_short) ; i++) {
		if (class_name_short[i] == '-')
			class_name_short[i] = '_';
	}
	
	gchar *load_name = g_strdup_printf("xmi_msim_gui_source_%s_load", class_name_short);
	gchar *unload_name = g_strdup_printf("xmi_msim_gui_source_%s_unload", class_name_short);
	g_free(class_name_short);
	/* Make sure that the loaded library contains the required methods */
	if (!g_module_symbol(module->library,
		load_name,
		(gpointer *) &module->load) ||
      	    !g_module_symbol(module->library,
		unload_name,
		(gpointer *) &module->unload)) {

      		g_warning("%s\n", g_module_error());
		g_module_close(module->library);
		return FALSE;
	}
	g_free(load_name);
	g_free(unload_name);

	/* Initialize the loaded module */
	module->load(G_TYPE_MODULE(module));
	module->initialized = TRUE;


	return TRUE;
}

static void xmi_msim_gui_source_module_real_unload (GTypeModule *gmodule) {
  XmiMsimGuiSourceModule *module = XMI_MSIM_GUI_SOURCE_MODULE(gmodule);

  module->unload(gmodule);

  g_module_close(module->library);
  module->library = NULL;

  module->load   = NULL;
  module->unload = NULL;
}
