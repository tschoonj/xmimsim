/*
Copyright (C) 2016 Tom Schoonjans and Laszlo Vincze

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

#include <gtk/gtk.h>
#include "xmi_data_structs.h"

#ifndef XMI_MSIM_GUI_SOURCE_ABSTRACT_H
#define XMI_MSIM_GUI_SOURCE_ABSTRACT_H

G_BEGIN_DECLS


#define XMI_MSIM_GUI_DEFINE_DYNAMIC_SOURCE_TYPE(a, b, c) \
	G_DEFINE_DYNAMIC_TYPE(a, b, c) \
	G_MODULE_EXPORT void b ## _load(GTypeModule *module) { \
		b ## _register_type(module); \
	} \
	G_MODULE_EXPORT void b ## _unload(GTypeModule *module) { \
	}


#define XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT                  (xmi_msim_gui_source_abstract_get_type ())
#define XMI_MSIM_GUI_SOURCE_ABSTRACT(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT, XmiMsimGuiSourceAbstract))
#define XMI_MSIM_GUI_SOURCE_ABSTRACT_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT, XmiMsimGuiSourceAbstractClass))
#define XMI_MSIM_GUI_IS_SOURCE_ABSTRACT(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT))
#define XMI_MSIM_GUI_IS_SOURCE_ABSTRACT_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT))
#define XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT, XmiMsimGuiSourceAbstractClass))

typedef struct _XmiMsimGuiSourceAbstract        XmiMsimGuiSourceAbstract;
typedef struct _XmiMsimGuiSourceAbstractClass   XmiMsimGuiSourceAbstractClass;

struct _XmiMsimGuiSourceAbstract
{
  GtkVBox parent_instance;
  GArray *x;
  GArray *y;
  struct xmi_excitation *raw_data;
  struct xmi_input *current;
};

struct _XmiMsimGuiSourceAbstractClass
{
  GtkVBoxClass parent_class;

  gboolean (* generate)  (XmiMsimGuiSourceAbstract *source, GError **error);

  gboolean (* save)      (XmiMsimGuiSourceAbstract *source, const char *filename, GError **error);
  
  const gchar* (*get_name) (XmiMsimGuiSourceAbstract *source);

  const gchar* (*get_about_text) (XmiMsimGuiSourceAbstract *source);

  gchar* (*energy_discrete_printf) (XmiMsimGuiSourceAbstract *source, struct xmi_energy_discrete *energy);

  gchar* (*energy_continuous_printf) (XmiMsimGuiSourceAbstract *source, struct xmi_energy_continuous *energy);
};

GType xmi_msim_gui_source_abstract_get_type(void) G_GNUC_CONST;

gboolean xmi_msim_gui_source_abstract_generate(XmiMsimGuiSourceAbstract *source, GError **error);

gboolean xmi_msim_gui_source_abstract_save(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error);

void xmi_msim_gui_source_abstract_get_plot_data(XmiMsimGuiSourceAbstract *source, GArray **x, GArray**y);

const gchar *xmi_msim_gui_source_abstract_get_name(XmiMsimGuiSourceAbstract *source);

const gchar *xmi_msim_gui_source_abstract_get_about_text(XmiMsimGuiSourceAbstract *source);

struct xmi_excitation *xmi_msim_gui_source_abstract_get_raw_data(XmiMsimGuiSourceAbstract *source);

typedef enum {
	XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_METHOD_UNDEFINED,
	XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_INVALID_FILENAME,
	XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_NO_RAW_DATA,
} XmiMsimGuiSourceAbstractError;

#define XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR (xmi_msim_gui_source_abstract_error_quark())

GQuark xmi_msim_gui_source_abstract_error_quark(void);

double xmi_msim_gui_source_abstract_get_solid_angle_from_slits(struct xmi_geometry *geometry);

G_END_DECLS
#endif
