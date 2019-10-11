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
#include "xmimsim-gui-sources-dialog.h"
#include "xmimsim-gui-private.h"
#include <math.h>
#include <string.h>
#include <libpeas/peas.h>
#include "xmi_gobject.h"

struct _XmiMsimGuiSourceAbstractPrivate
{
  GArray *x;
  GArray *y;
  xmi_excitation *raw_data;
  xmi_input *current;
  XmiMsimGuiSourcesDialog *dialog;
};

static void peas_activatable_iface_init (PeasActivatableInterface *iface);

G_DEFINE_ABSTRACT_TYPE_WITH_CODE(
	XmiMsimGuiSourceAbstract,
	xmi_msim_gui_source_abstract,
	GTK_TYPE_BOX,
	G_ADD_PRIVATE(XmiMsimGuiSourceAbstract)
	G_IMPLEMENT_INTERFACE(PEAS_TYPE_ACTIVATABLE, peas_activatable_iface_init))

static void xmi_msim_gui_source_abstract_real_generate(XmiMsimGuiSourceAbstract *source);

static gboolean xmi_msim_gui_source_abstract_real_save(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error);

static const gchar *xmi_msim_gui_source_abstract_real_get_name(XmiMsimGuiSourceAbstract *source);

static const gchar *xmi_msim_gui_source_abstract_real_get_about_text(XmiMsimGuiSourceAbstract *source);

static gchar *xmi_msim_gui_source_abstract_real_energy_discrete_printf(XmiMsimGuiSourceAbstract *source, xmi_energy_discrete *energy);

static gchar *xmi_msim_gui_source_abstract_real_energy_continuous_printf(XmiMsimGuiSourceAbstract *source, xmi_energy_continuous *energy);

static gboolean xmi_msim_gui_source_abstract_real_save_parameters(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error);

static gboolean xmi_msim_gui_source_abstract_real_load_parameters(XmiMsimGuiSourceAbstract *source, const char *filename, GError **error);

static void xmi_msim_gui_source_abstract_dispose(GObject *object);

static void xmi_msim_gui_source_abstract_finalize(GObject *object);

static void xmi_msim_gui_source_abstract_set_property (GObject          *object,
                                                guint             prop_id,
                                                const GValue     *value,
                                                GParamSpec       *pspec);

static void xmi_msim_gui_source_abstract_get_property (GObject          *object,
                                                guint             prop_id,
                                                GValue     *value,
                                                GParamSpec       *pspec);

enum {
	AFTER_GENERATE,
	LAST_SIGNAL
};

enum {
	PROP_0,
	PROP_CURRENT_INPUT,
	PROP_RAW_DATA,
	PROP_ARRAY_X,
	PROP_ARRAY_Y,
	N_PROPERTIES,
	PROP_DIALOG,
};

static guint signals[LAST_SIGNAL];

static GParamSpec *props[N_PROPERTIES] = {NULL, };

static void after_generate_default_cb(XmiMsimGuiSourceAbstract *source, GError *error, gpointer data) {
	g_debug("Calling after_generate_default_cb");
	if (error == NULL)
		return;

	// if an error occurred, set everything to NULL
	g_object_set(source, "raw_data", NULL, "x", NULL, "y", NULL, NULL);
}

static void xmi_msim_gui_source_abstract_class_init(XmiMsimGuiSourceAbstractClass *klass) {

	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_source_abstract_dispose;
	object_class->finalize = xmi_msim_gui_source_abstract_finalize;
	object_class->set_property = xmi_msim_gui_source_abstract_set_property;
	object_class->get_property = xmi_msim_gui_source_abstract_get_property;

	klass->generate = xmi_msim_gui_source_abstract_real_generate;
	klass->save = xmi_msim_gui_source_abstract_real_save;
	klass->get_source_name = xmi_msim_gui_source_abstract_real_get_name;
	klass->get_about_text = xmi_msim_gui_source_abstract_real_get_about_text;
	klass->energy_discrete_printf = xmi_msim_gui_source_abstract_real_energy_discrete_printf;
	klass->energy_continuous_printf = xmi_msim_gui_source_abstract_real_energy_continuous_printf;
	klass->save_parameters = xmi_msim_gui_source_abstract_real_save_parameters;
	klass->load_parameters = xmi_msim_gui_source_abstract_real_load_parameters;

	props[PROP_CURRENT_INPUT] = g_param_spec_boxed(
		"xmi-input-current",
		"Current xmi_input",
		"Current xmi_input",
		XMI_MSIM_TYPE_INPUT,
    		G_PARAM_READWRITE | G_PARAM_CONSTRUCT
	);

	/**
	 * XmiMsimGuiSourceAbstract:raw-data:
	 */
	props[PROP_RAW_DATA] = g_param_spec_boxed(
		"raw-data",
		"Raw data",
		"Raw data",
		XMI_MSIM_TYPE_EXCITATION,
    		G_PARAM_READWRITE
	);

	/**
	 * XmiMsimGuiSourceAbstract:x: (type GLib.Array<gdouble>):
	 */
	props[PROP_ARRAY_X] = g_param_spec_boxed(
		"x",
		"Array x",
		"Array x",
		G_TYPE_ARRAY,
    		G_PARAM_READWRITE
	);

	/**
	 * XmiMsimGuiSourceAbstract:y: (type GLib.Array<gdouble>):
	 */
	props[PROP_ARRAY_Y] = g_param_spec_boxed(
		"y",
		"Array y",
		"Array y",
		G_TYPE_ARRAY,
    		G_PARAM_READWRITE
	);

	g_object_class_install_properties(object_class, N_PROPERTIES, props);

	g_object_class_override_property(object_class, PROP_DIALOG, "object");

	GType param_types[1] = {G_TYPE_ERROR /* GError* */};
	GClosure *default_handler = g_cclosure_new(G_CALLBACK(after_generate_default_cb), NULL, NULL);

	signals[AFTER_GENERATE] = g_signal_newv(
		"after-generate",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_FIRST,
		default_handler,
		NULL,
		NULL,
		NULL,
		G_TYPE_NONE,
		1,
		param_types
	);
}

static void xmi_msim_gui_source_abstract_init(XmiMsimGuiSourceAbstract *source) {

	source->priv = xmi_msim_gui_source_abstract_get_instance_private(source);

	g_object_ref_sink(source); // Otherwise we get into trouble with libpeas' python loader!

	g_object_set(
		source,
		"spacing", 2,
		"homogeneous", FALSE,
		"expand", FALSE,
		"orientation", GTK_ORIENTATION_VERTICAL,
		NULL
	);
	
}

/**
 * xmi_msim_gui_source_abstract_get_plot_data:
 * @source: an #XmiMsimGuiSourceAbstract instance
 * @x: (element-type double) (out): the X-data.
 * @y: (element-type double) (out): the Y-data.
 *
 * Gets the current plot data as two arrays x and y.
 */
void xmi_msim_gui_source_abstract_get_plot_data(XmiMsimGuiSourceAbstract *source, GArray **x, GArray **y) {
	g_return_if_fail(XMI_MSIM_GUI_IS_SOURCE_ABSTRACT(source));
	g_return_if_fail(x != NULL && y != NULL);

	if (source->priv->x)
		*x = g_array_ref(source->priv->x);
	else
		*x = NULL;
	if (source->priv->y)
		*y = g_array_ref(source->priv->y);
	else
		*y = NULL;
}

/**
 * xmi_msim_gui_source_abstract_get_raw_data:
 * @source: an #XmiMsimGuiSourceAbstract instance
 *
 * Returns: (transfer full): the current raw excitation spectrum as a freshly allocated #XmiMsimExcitation struct, or %NULL.
 */
xmi_excitation *xmi_msim_gui_source_abstract_get_raw_data(XmiMsimGuiSourceAbstract *source) {
	xmi_excitation *raw_data = NULL;
	g_object_get(source, "raw-data", &raw_data, NULL);
	return raw_data;
}

static void xmi_msim_gui_source_abstract_dispose(GObject *object) {
	// deriving methods should store the data in the preferences file now
	XmiMsimGuiSourceAbstract *source = XMI_MSIM_GUI_SOURCE_ABSTRACT(object);

	g_clear_object(&source->priv->dialog);

	G_OBJECT_CLASS(xmi_msim_gui_source_abstract_parent_class)->dispose(object);
}

static void xmi_msim_gui_source_abstract_finalize(GObject *object) {
	g_debug("Calling xmi_msim_gui_source_abstract_finalize");
	XmiMsimGuiSourceAbstract *source = XMI_MSIM_GUI_SOURCE_ABSTRACT(object);
	xmi_excitation_free(source->priv->raw_data);
	if (source->priv->x)
		g_array_unref(source->priv->x);
	if (source->priv->y)
		g_array_unref(source->priv->y);

	g_boxed_free(XMI_MSIM_TYPE_INPUT, source->priv->current);

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
	if (source->priv->raw_data == NULL || (source->priv->raw_data->n_discrete == 0 && source->priv->raw_data->n_continuous == 0)) {
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
	for (i = 0 ; i < source->priv->raw_data->n_continuous ; i++) {
		gchar *line = XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->energy_continuous_printf(source, &source->priv->raw_data->continuous[i]);
		if (g_output_stream_write_all(G_OUTPUT_STREAM(stream), (const void*) line, strlen(line), &bytes_written, NULL, error) == FALSE) {
			g_object_unref(stream);
			g_object_unref(file);
			return FALSE;
		}
		g_free(line);
	}
	for (i = 0 ; i < source->priv->raw_data->n_discrete ; i++) {
		gchar *line = XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->energy_discrete_printf(source, &source->priv->raw_data->discrete[i]);
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
	return XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->get_source_name(source);
}

const gchar *xmi_msim_gui_source_abstract_get_about_text(XmiMsimGuiSourceAbstract *source) {
	return XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->get_about_text(source);
}

static gchar *xmi_msim_gui_source_abstract_real_energy_discrete_printf(XmiMsimGuiSourceAbstract *source, xmi_energy_discrete *energy) {
	return g_strdup_printf("%g     %g\n", energy->energy, energy->horizontal_intensity + energy->vertical_intensity);
}

static gchar *xmi_msim_gui_source_abstract_real_energy_continuous_printf(XmiMsimGuiSourceAbstract *source, xmi_energy_continuous *energy) {
	return g_strdup_printf("%g     %g\n", energy->energy, energy->horizontal_intensity + energy->vertical_intensity);
}

static void xmi_msim_gui_source_abstract_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiSourceAbstract *source = XMI_MSIM_GUI_SOURCE_ABSTRACT(object);

  switch (prop_id) {
    case PROP_CURRENT_INPUT:
      source->priv->current = (xmi_input *) g_value_dup_boxed(value);
      break;
    case PROP_RAW_DATA:
      xmi_excitation_free(source->priv->raw_data);
      source->priv->raw_data = (xmi_excitation *) g_value_dup_boxed(value);
      break;
    case PROP_ARRAY_X:
      if (source->priv->x)
	      g_array_unref(source->priv->x);
      source->priv->x = (GArray *) g_value_dup_boxed(value);
      break;
    case PROP_ARRAY_Y:
      if (source->priv->y)
	      g_array_unref(source->priv->y);
      source->priv->y = (GArray *) g_value_dup_boxed(value);
      break;
    case PROP_DIALOG:
      source->priv->dialog = g_value_dup_object(value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }

}

static void xmi_msim_gui_source_abstract_get_property(GObject *object, guint prop_id, GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiSourceAbstract *source = XMI_MSIM_GUI_SOURCE_ABSTRACT(object);

  switch (prop_id) {
    case PROP_CURRENT_INPUT:
      g_value_set_boxed(value, source->priv->current);
      break;
    case PROP_RAW_DATA:
      g_value_set_boxed(value, source->priv->raw_data);
      break;
    case PROP_ARRAY_X:
      g_value_set_boxed(value, source->priv->x);
      break;
    case PROP_ARRAY_Y:
      g_value_set_boxed(value, source->priv->y);
      break;
    case PROP_DIALOG:
      g_value_set_object(value, source->priv->dialog);
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

/**
 * xmi_msim_gui_source_abstract_set_data:
 * @source: #XmiMsimGuiSourceAbstract instance
 * @raw_data: #XmiMsimExcitation instance
 * @x: (element-type double): plot data x-axis
 * @y: (element-type double): plot data y-axis
 *
 * Update the raw and plot data. Should be used only from within the generate method, before emitting the after-generate signal.
 */
void xmi_msim_gui_source_abstract_set_data(XmiMsimGuiSourceAbstract *source, xmi_excitation *raw_data, GArray *x, GArray *y) {
	g_return_if_fail(source != NULL && raw_data != NULL && x != NULL && y != NULL);

	g_object_set(source, "x", x, "y", y, "raw-data", raw_data, NULL);
}

static void xmi_msim_gui_source_abstract_activate(PeasActivatable *activatable) {
	g_debug("Calling xmi_msim_gui_source_abstract_activate");
	XmiMsimGuiSourceAbstract *source = XMI_MSIM_GUI_SOURCE_ABSTRACT(activatable);
	gtk_widget_show_all(GTK_WIDGET(source));

	GtkWidget *label = gtk_label_new(xmi_msim_gui_source_abstract_get_name(source));
	gtk_notebook_append_page(xmi_msim_gui_sources_dialog_get_notebook(source->priv->dialog), GTK_WIDGET(source), label);
	g_debug("Source ref count after append: %d", G_OBJECT(source)->ref_count);
	
	xmi_msim_gui_source_abstract_generate(source);

	g_signal_connect(G_OBJECT(source), "after-generate", G_CALLBACK(after_generate_cb), source->priv->dialog);
}

static void xmi_msim_gui_source_abstract_deactivate(PeasActivatable *activatable) {
	g_debug("Calling xmi_msim_gui_source_abstract_deactivate");
	g_debug("Source ref count after deactivate: %d", G_OBJECT(activatable)->ref_count);
}

static void peas_activatable_iface_init(PeasActivatableInterface *iface) {
	iface->activate = xmi_msim_gui_source_abstract_activate;
	iface->deactivate = xmi_msim_gui_source_abstract_deactivate;
}
