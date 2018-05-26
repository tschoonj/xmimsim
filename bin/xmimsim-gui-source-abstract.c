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

#include "xmimsim-gui-source-abstract.h"
#include <math.h>
#include <string.h>
#include "xmimsim-gui-marshal.h"
#include "xmi_gobject.h"

G_DEFINE_ABSTRACT_TYPE(XmiMsimGuiSourceAbstract, xmi_msim_gui_source_abstract, GTK_TYPE_BOX)

static void xmi_msim_gui_source_abstract_real_generate(XmiMsimGuiSourceAbstract *source);

static gboolean xmi_msim_gui_source_abstract_real_save(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error);

static const gchar *xmi_msim_gui_source_abstract_real_get_name(XmiMsimGuiSourceAbstract *source);

static const gchar *xmi_msim_gui_source_abstract_real_get_about_text(XmiMsimGuiSourceAbstract *source);

static gchar *xmi_msim_gui_source_abstract_real_energy_discrete_printf(XmiMsimGuiSourceAbstract *source, struct xmi_energy_discrete *energy);

static gchar *xmi_msim_gui_source_abstract_real_energy_continuous_printf(XmiMsimGuiSourceAbstract *source, struct xmi_energy_continuous *energy);

static gboolean xmi_msim_gui_source_abstract_real_save_parameters(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error);

static gboolean xmi_msim_gui_source_abstract_real_load_parameters(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error);

static void xmi_msim_gui_source_abstract_dispose(GObject *object);

static void xmi_msim_gui_source_abstract_finalize(GObject *object);

static void xmi_msim_gui_source_abstract_set_property (GObject          *object,
                                                guint             prop_id,
                                                const GValue     *value,
                                                GParamSpec       *pspec);

enum {
	AFTER_GENERATE,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

static void xmi_msim_gui_source_abstract_class_init(XmiMsimGuiSourceAbstractClass *klass) {

	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_source_abstract_dispose;
	object_class->finalize = xmi_msim_gui_source_abstract_finalize;
	object_class->set_property = xmi_msim_gui_source_abstract_set_property;

	klass->generate = xmi_msim_gui_source_abstract_real_generate;
	klass->save = xmi_msim_gui_source_abstract_real_save;
	klass->get_name = xmi_msim_gui_source_abstract_real_get_name;
	klass->get_about_text = xmi_msim_gui_source_abstract_real_get_about_text;
	klass->energy_discrete_printf = xmi_msim_gui_source_abstract_real_energy_discrete_printf;
	klass->energy_continuous_printf = xmi_msim_gui_source_abstract_real_energy_continuous_printf;
	klass->save_parameters = xmi_msim_gui_source_abstract_real_save_parameters;
	klass->load_parameters = xmi_msim_gui_source_abstract_real_load_parameters;

	g_object_class_install_property(object_class,
		1,
		g_param_spec_boxed(
			"xmi-input-current",
			"Current struct xmi_input",
			"Current struct xmi_input",
			XMI_MSIM_TYPE_INPUT,
    			(GParamFlags) (G_PARAM_WRITABLE | G_PARAM_CONSTRUCT)
		)
	);

	signals[AFTER_GENERATE] = g_signal_new(
		"after-generate",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__POINTER,
		G_TYPE_NONE,
		1,
		G_TYPE_POINTER // GError *
	);
}

static void xmi_msim_gui_source_abstract_init(XmiMsimGuiSourceAbstract *source) {

	source->x = NULL;
	source->y = NULL;
	source->raw_data = NULL;
	source->current = NULL;

	g_object_set(
		(gpointer) source,
		"spacing", 2,
		"homogeneous", FALSE,
		"expand", FALSE,
		"orientation", GTK_ORIENTATION_VERTICAL,
		NULL
	);
	
}

void xmi_msim_gui_source_abstract_get_plot_data(XmiMsimGuiSourceAbstract *source, GArray **x, GArray **y) {
	*x = source->x;
	*y = source->y;
}

struct xmi_excitation *xmi_msim_gui_source_abstract_get_raw_data(XmiMsimGuiSourceAbstract *source) {
	return source->raw_data;
}

static void xmi_msim_gui_source_abstract_dispose(GObject *object) {
	// deriving methods should store the data in the preferences file now

	G_OBJECT_CLASS(xmi_msim_gui_source_abstract_parent_class)->dispose(object);
}

static void xmi_msim_gui_source_abstract_finalize(GObject *object) {
	XmiMsimGuiSourceAbstract *source = XMI_MSIM_GUI_SOURCE_ABSTRACT(object);
	if (source->raw_data)
		xmi_free_excitation(source->raw_data);
	if (source->x)
		g_array_free(source->x, TRUE);
	if (source->y)
		g_array_free(source->y, TRUE);

	g_boxed_free(XMI_MSIM_TYPE_INPUT, source->current);

	G_OBJECT_CLASS(xmi_msim_gui_source_abstract_parent_class)->finalize(object);
}

static void xmi_msim_gui_source_abstract_real_generate(XmiMsimGuiSourceAbstract *source) {
	g_warning("XmiMsimGuiSourceAbstract::generate not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	GError *error = g_error_new(XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_METHOD_UNDEFINED, "XmiMsimGuiSourceAbstract::generate not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));

	g_signal_emit((gpointer) source, signals[AFTER_GENERATE], 0, error);
	g_error_free(error);

	return;
}

static gboolean xmi_msim_gui_source_abstract_real_save_parameters(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error) {
	g_warning("XmiMsimGuiSourceAbstract::save_parameters not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	g_set_error(error, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_METHOD_UNDEFINED, "XmiMsimGuiSourceAbstract::save_parameters not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	return FALSE;
}

static gboolean xmi_msim_gui_source_abstract_real_load_parameters(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error) {
	g_warning("XmiMsimGuiSourceAbstract::load_parameters not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	g_set_error(error, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_METHOD_UNDEFINED, "XmiMsimGuiSourceAbstract::load_parameters not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	return FALSE;
}

static gboolean xmi_msim_gui_source_abstract_real_save(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error) {
	// check if we have data to save
	if (source->raw_data == NULL || (source->raw_data->n_discrete == 0 && source->raw_data->n_continuous == 0)) {
		g_set_error(error, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_NO_RAW_DATA, "No data available for saving. Ensure the model data is valid and click \"Generate spectrum\"");
		return FALSE;

	}

	GFile *file = g_file_new_for_path(filename);
	GFileType type = g_file_query_file_type(file, G_FILE_QUERY_INFO_NONE, NULL);
	if (type == G_FILE_TYPE_REGULAR) {
		// file exists -> delete it
		if (g_file_delete(file, NULL, error) == FALSE) {
			g_object_unref(file);
			return FALSE;
		}
	}
	else if (type != G_FILE_TYPE_UNKNOWN) {
		// file exists but is not a regular file -> throw error
		g_set_error(error, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_INVALID_FILENAME, "Could not save to %s. It appears to exist already but it is not a regular file!", filename);
		g_object_unref(file);
		return FALSE;
	}

	GFileOutputStream *stream;

	if ((stream = g_file_create(file, G_FILE_CREATE_PRIVATE, NULL, error)) == NULL) {
		g_object_unref(file);
		return FALSE;
	}

	int i;
	gsize bytes_written = 0;
	for (i = 0 ; i < source->raw_data->n_continuous ; i++) {
		gchar *line = XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->energy_continuous_printf(source, &source->raw_data->continuous[i]);
		if (g_output_stream_write_all(G_OUTPUT_STREAM(stream), (const void*) line, strlen(line), &bytes_written, NULL, error) == FALSE) {
			g_object_unref(stream);
			g_object_unref(file);
			return FALSE;
		}
		g_free(line);
	}
	for (i = 0 ; i < source->raw_data->n_discrete ; i++) {
		gchar *line = XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->energy_discrete_printf(source, &source->raw_data->discrete[i]);
		if (g_output_stream_write_all(G_OUTPUT_STREAM(stream), (const void*) line, strlen(line), &bytes_written, NULL, error) == FALSE) {
			g_object_unref(stream);
			g_object_unref(file);
			return FALSE;
		}
		g_free(line);
	}

	if (g_output_stream_close(G_OUTPUT_STREAM(stream), NULL, error) == FALSE) {
		g_object_unref(stream);
		g_object_unref(file);
		return FALSE;
	}

	g_object_unref(stream);
	g_object_unref(file);
	return TRUE;
}

static const gchar *xmi_msim_gui_source_abstract_real_get_name(XmiMsimGuiSourceAbstract *source) {
	g_warning("XmiMsimGuiSourceAbstract::get_name not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	return NULL;
}

static const gchar *xmi_msim_gui_source_abstract_real_get_about_text(XmiMsimGuiSourceAbstract *source) {
	g_warning("XmiMsimGuiSourceAbstract::get_about_text not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	return NULL;
}

GQuark xmi_msim_gui_source_abstract_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-gui-source-abstract-error-quark");
}

void xmi_msim_gui_source_abstract_generate(XmiMsimGuiSourceAbstract *source) {
	XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->generate(source);
	return;
}

gboolean xmi_msim_gui_source_abstract_save(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error) {
	return XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->save(source, filename, error);
}

gboolean xmi_msim_gui_source_abstract_save_parameters(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error) {
	return XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->save_parameters(source, filename, error);
}

gboolean xmi_msim_gui_source_abstract_load_parameters(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error) {
	return XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->load_parameters(source, filename, error);
}

const gchar *xmi_msim_gui_source_abstract_get_name(XmiMsimGuiSourceAbstract *source) {
	return XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->get_name(source);
}

const gchar *xmi_msim_gui_source_abstract_get_about_text(XmiMsimGuiSourceAbstract *source) {
	return XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->get_about_text(source);
}

static gchar *xmi_msim_gui_source_abstract_real_energy_discrete_printf(XmiMsimGuiSourceAbstract *source, struct xmi_energy_discrete *energy) {
	return g_strdup_printf("%g     %g\n", energy->energy, energy->horizontal_intensity + energy->vertical_intensity);
}

static gchar *xmi_msim_gui_source_abstract_real_energy_continuous_printf(XmiMsimGuiSourceAbstract *source, struct xmi_energy_continuous *energy) {
	return g_strdup_printf("%g     %g\n", energy->energy, energy->horizontal_intensity + energy->vertical_intensity);
}

static void xmi_msim_gui_source_abstract_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiSourceAbstract *source = XMI_MSIM_GUI_SOURCE_ABSTRACT(object);

  switch (prop_id) {
    case 1:
      source->current = (struct xmi_input *) g_value_dup_boxed(value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }

}

