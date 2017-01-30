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

#include <glib.h>
#include <gmodule.h>
#include <glib-object.h>

#ifndef XMI_MSIM_GUI_SOURCE_MODULE_H
#define XMI_MSIM_GUI_SOURCE_MODULE_H

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_SOURCE_MODULE                  (xmi_msim_gui_source_module_get_type ())
#define XMI_MSIM_GUI_SOURCE_MODULE(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_SOURCE_MODULE, XmiMsimGuiSourceModule))
#define XMI_MSIM_GUI_SOURCE_MODULE_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_SOURCE_MODULE, XmiMsimGuiSourceModuleClass))
#define XMI_MSIM_GUI_IS_SOURCE_MODULE(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_SOURCE_MODULE))
#define XMI_MSIM_GUI_IS_SOURCE_MODULE_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_SOURCE_MODULE))
#define XMI_MSIM_GUI_SOURCE_MODULE_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_SOURCE_MODULE, XmiMsimGuiSourceModuleClass))

typedef struct _XmiMsimGuiSourceModule        XmiMsimGuiSourceModule;
typedef struct _XmiMsimGuiSourceModuleClass   XmiMsimGuiSourceModuleClass;

struct _XmiMsimGuiSourceModule {
	GTypeModule parent;
	gchar *filename;
	GModule *library;
	gboolean initialized;
	void (*load)   (GTypeModule *module);
	void (*unload) (GTypeModule *module);
};

struct _XmiMsimGuiSourceModuleClass {
	GTypeModuleClass parent_class;
};

GType xmi_msim_gui_source_module_get_type(void) G_GNUC_CONST;

XmiMsimGuiSourceModule* xmi_msim_gui_source_module_new(const gchar *filename);

typedef enum {
	XMI_MSIM_GUI_SOURCE_MODULE_ERROR_METHOD_UNDEFINED,
} XmiMsimGuiSourceModuleError;

#define XMI_MSIM_GUI_SOURCE_MODULE_ERROR (xmi_msim_gui_source_module_error_quark())

GQuark xmi_msim_gui_source_module_error_quark(void);

G_END_DECLS

#endif
