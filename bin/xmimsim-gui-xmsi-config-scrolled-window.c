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
#include <string.h>
#include "xmimsim-gui-xmsi-config-scrolled-window.h"
#include "xmimsim-gui-layer-box.h"
#include "xmimsim-gui-energies-box.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-private.h"

#ifdef HAVE_GOOGLE_ANALYTICS
#include "xmimsim-gui-google-analytics.h"
#endif

struct _XmiMsimGuiXmsiConfigScrolledWindowClass {
	GtkScrolledWindowClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiXmsiConfigScrolledWindow, xmi_msim_gui_xmsi_config_scrolled_window, GTK_TYPE_SCROLLED_WINDOW)

static void xmi_msim_gui_xmsi_config_scrolled_window_finalize(GObject *gobject) {
	XmiMsimGuiXmsiConfigScrolledWindow *self = XMI_MSIM_GUI_XMSI_CONFIG_SCROLLED_WINDOW(gobject);

	g_hash_table_unref(self->table);

	G_OBJECT_CLASS(xmi_msim_gui_xmsi_config_scrolled_window_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_xmsi_config_scrolled_window_dispose(GObject *gobject) {
	XmiMsimGuiXmsiConfigScrolledWindow *self = XMI_MSIM_GUI_XMSI_CONFIG_SCROLLED_WINDOW(gobject);

	g_clear_object(&self->clipboard_manager);
	g_clear_object(&self->undo_manager);
	g_clear_object(&self->orig_pixbuf);
	g_clear_object(&self->current_pixbuf);

	G_OBJECT_CLASS(xmi_msim_gui_xmsi_config_scrolled_window_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_xmsi_config_scrolled_window_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiXmsiConfigScrolledWindow *self = XMI_MSIM_GUI_XMSI_CONFIG_SCROLLED_WINDOW(object);

  switch (prop_id) {
    case 1:
      self->undo_manager = XMI_MSIM_GUI_UNDO_MANAGER(g_value_dup_object(value));
      break;
    case 2:
      self->clipboard_manager = XMI_MSIM_GUI_CLIPBOARD_MANAGER(g_value_dup_object(value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
  }
}

static void outputfile_writer(GValue *value, const struct xmi_input *input) {
	g_value_init(value, G_TYPE_STRING);
	g_value_set_string(value, input->general->outputfile);
}

static void outputfile_reader(const GValue *value, struct xmi_input *input) {
	g_free(input->general->outputfile);
	input->general->outputfile = g_value_dup_string(value);
}

static XmiMsimGuiUndoManagerValueValidatorResult outputfile_validator(GtkWidget *widget, struct xmi_input *current_input, GValue *value) {
	const gchar *text = gtk_entry_get_text(GTK_ENTRY(widget));

	if (g_strcmp0(text, current_input->general->outputfile) == 0)
		return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_EQUAL;

	g_value_init(value, G_TYPE_STRING);
	g_value_set_string(value, text);

	return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_VALID;
}

#define CREATE_ENTRY_UNDO_MANAGER_METHODS_INT(widget_name, data_name, min_value, max_value) \
static void widget_name ## _writer(GValue *value, const struct xmi_input *input) { \
	g_value_init(value, G_TYPE_INT); \
	g_value_set_int(value, data_name); \
} \
\
static void widget_name ## _reader(const GValue *value, struct xmi_input *input) { \
	data_name = g_value_get_int(value); \
} \
\
static XmiMsimGuiUndoManagerValueValidatorResult widget_name ## _validator(GtkWidget *widget, struct xmi_input *input, GValue *value) { \
	const gchar *text = gtk_entry_get_text(GTK_ENTRY(widget)); \
	gchar *endptr; \
\
	gint64 val = g_ascii_strtoll(text, &endptr, 10); \
	if (val < min_value || val > max_value || endptr != text + strlen(text)) { \
		return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_INVALID; \
	} \
\
	if (val == data_name) \
		return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_EQUAL; \
\
	g_value_init(value, G_TYPE_INT); \
	g_value_set_int(value, (int) val); \
\
	return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_VALID; \
}

#define CREATE_ENTRY_UNDO_MANAGER_METHODS_LONG(widget_name, data_name, min_value, max_value) \
static void widget_name ## _writer(GValue *value, const struct xmi_input *input) { \
	g_value_init(value, G_TYPE_LONG); \
	g_value_set_long(value, data_name); \
} \
\
static void widget_name ## _reader(const GValue *value, struct xmi_input *input) { \
	data_name = g_value_get_long(value); \
} \
\
static XmiMsimGuiUndoManagerValueValidatorResult widget_name ## _validator(GtkWidget *widget, struct xmi_input *input, GValue *value) { \
	const gchar *text = gtk_entry_get_text(GTK_ENTRY(widget)); \
	gchar *endptr; \
\
	gint64 val = g_ascii_strtoll(text, &endptr, 10); \
	if (val < min_value || val > max_value || endptr != text + strlen(text)) { \
		return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_INVALID; \
	} \
\
	if (val == data_name) \
		return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_EQUAL; \
\
	g_value_init(value, G_TYPE_LONG); \
	g_value_set_long(value, val); \
\
	return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_VALID; \
}

#define CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(widget_name, data_name, min_value, min_value_inclusive, max_value, max_value_inclusive) \
static void widget_name ## _writer(GValue *value, const struct xmi_input *input) { \
	g_value_init(value, G_TYPE_DOUBLE); \
	g_value_set_double(value, data_name); \
} \
\
static void widget_name ## _reader(const GValue *value, struct xmi_input *input) { \
	data_name = g_value_get_double(value); \
} \
\
static XmiMsimGuiUndoManagerValueValidatorResult widget_name ## _validator(GtkWidget *widget, struct xmi_input *input, GValue *value) { \
	gchar *text = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(widget)))); \
	gchar *endptr; \
\
	gdouble val = g_ascii_strtod(text, &endptr); \
	if ((min_value_inclusive && val < min_value) || (!min_value_inclusive && val <= min_value) || (max_value_inclusive && val > max_value) || (!max_value_inclusive && val >= max_value) || endptr != text + strlen(text) || strlen(text) == 0) { \
		g_free(text); \
		return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_INVALID; \
	} \
\
	g_free(text); \
\
	if (val == data_name) \
		return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_EQUAL; \
\
	g_value_init(value, G_TYPE_DOUBLE); \
	g_value_set_double(value, val); \
\
	return XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_VALID; \
}

CREATE_ENTRY_UNDO_MANAGER_METHODS_LONG(n_photons_interval, input->general->n_photons_interval, 1, G_MAXLONG);
CREATE_ENTRY_UNDO_MANAGER_METHODS_LONG(n_photons_line, input->general->n_photons_line, 1, G_MAXLONG);
CREATE_ENTRY_UNDO_MANAGER_METHODS_INT(n_interactions_trajectory, input->general->n_interactions_trajectory, 1, 10);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(d_sample_source, input->geometry->d_sample_source, 0.0, FALSE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(n_sample_orientation_x, input->geometry->n_sample_orientation[0], G_MINDOUBLE, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(n_sample_orientation_y, input->geometry->n_sample_orientation[1], G_MINDOUBLE, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(n_sample_orientation_z, input->geometry->n_sample_orientation[2], 0.0, FALSE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(p_detector_window_x, input->geometry->p_detector_window[0], G_MINDOUBLE, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(p_detector_window_y, input->geometry->p_detector_window[1], G_MINDOUBLE, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(p_detector_window_z, input->geometry->p_detector_window[2], G_MINDOUBLE, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(n_detector_orientation_x, input->geometry->n_detector_orientation[0], G_MINDOUBLE, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(n_detector_orientation_y, input->geometry->n_detector_orientation[1], G_MINDOUBLE, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(n_detector_orientation_z, input->geometry->n_detector_orientation[2], G_MINDOUBLE, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(area_detector, input->geometry->area_detector, 0.0, FALSE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(collimator_height, input->geometry->collimator_height, 0.0, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(collimator_diameter, input->geometry->collimator_diameter, 0.0, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(d_source_slit, input->geometry->d_source_slit, 0.0, FALSE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(slit_size_x, input->geometry->slit_size_x, 0.0, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(slit_size_y, input->geometry->slit_size_y, 0.0, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(detector_gain, input->detector->gain, 0.0, FALSE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(detector_live_time, input->detector->live_time, 0.0, FALSE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(detector_pulse_width, input->detector->pulse_width, 0.0, FALSE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(detector_zero, input->detector->zero, G_MINDOUBLE, TRUE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(detector_fano, input->detector->fano, 0.0, FALSE, G_MAXDOUBLE, TRUE);
CREATE_ENTRY_UNDO_MANAGER_METHODS_DOUBLE(detector_noise, input->detector->noise, 0.0, FALSE, G_MAXDOUBLE, TRUE);

static void comments_writer(GValue *value, const struct xmi_input *input) {
	g_value_init(value, G_TYPE_STRING);
	g_value_set_string(value, input->general->comments);
}

static void comments_reader(const GValue *value, struct xmi_input *input) {
	const gchar *all_text = NULL;
	if (G_VALUE_HOLDS(value, XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_INSERT_DATA)) {
		all_text = xmi_msim_gui_undo_manager_text_view_insert_data_get_all_text((XmiMsimGuiUndoManagerTextViewInsertData *) g_value_get_boxed(value));
	}
	else if (G_VALUE_HOLDS(value, XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_DELETE_DATA)) {
		all_text = xmi_msim_gui_undo_manager_text_view_delete_data_get_all_text((XmiMsimGuiUndoManagerTextViewDeleteData *) g_value_get_boxed(value));
	}
	else {
		g_warning("comments reader: unknown type %s detected", G_VALUE_TYPE_NAME(value));
		return;
	}
	g_debug("all_text: %s", all_text);
	g_free(input->general->comments);
	input->general->comments = g_strdup(all_text);	
}

static void detector_type_writer(GValue *value, const struct xmi_input *input) {
	g_value_init(value, G_TYPE_INT);
	g_value_set_int(value, input->detector->detector_type);
}

static void detector_type_reader(const GValue *value, struct xmi_input *input) {
	input->detector->detector_type = g_value_get_int(value);
}

static void detector_nchannels_writer(GValue *value, const struct xmi_input *input) {
	g_value_init(value, G_TYPE_DOUBLE);
	g_value_set_double(value, (double) input->detector->nchannels);
}

static void detector_nchannels_reader(const GValue *value, struct xmi_input *input) {
	input->detector->nchannels = (int) g_value_get_double(value);
}

static void xmi_msim_gui_xmsi_config_scrolled_window_constructed(GObject *obj) {
	// here comes all the stuff that relies on the undo_manager and clipboard_manager being set
	XmiMsimGuiXmsiConfigScrolledWindow *self = XMI_MSIM_GUI_XMSI_CONFIG_SCROLLED_WINDOW(obj);

	// general
	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->outputfileW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->outputfileW), "output-file changed", outputfile_writer, outputfile_reader, outputfile_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->n_photons_intervalW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->n_photons_intervalW), "number of simulated photons per interval changed", n_photons_interval_writer, n_photons_interval_reader, n_photons_interval_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->n_photons_lineW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->n_photons_lineW), "number of simulated photons per line changed", n_photons_line_writer, n_photons_line_reader, n_photons_line_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->n_interactions_trajectoryW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->n_interactions_trajectoryW), "number of simulated interactions per trajectory changed", n_interactions_trajectory_writer, n_interactions_trajectory_reader, n_interactions_trajectory_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->commentsW);
	xmi_msim_gui_undo_manager_register_text_view(self->undo_manager, GTK_TEXT_VIEW(self->commentsW), comments_writer, comments_reader);

	// composition
	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->compositionW);
	xmi_msim_gui_undo_manager_register_layer_box(self->undo_manager, XMI_MSIM_GUI_LAYER_BOX(self->compositionW));

	// geometry
	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->d_sample_sourceW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->d_sample_sourceW), "distance between source and sample changed", d_sample_source_writer, d_sample_source_reader, d_sample_source_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->n_sample_orientation_xW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->n_sample_orientation_xW), "sample orientation vector x changed", n_sample_orientation_x_writer, n_sample_orientation_x_reader, n_sample_orientation_x_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->n_sample_orientation_yW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->n_sample_orientation_yW), "sample orientation vector y changed", n_sample_orientation_y_writer, n_sample_orientation_y_reader, n_sample_orientation_y_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->n_sample_orientation_zW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->n_sample_orientation_zW), "sample orientation vector z changed", n_sample_orientation_z_writer, n_sample_orientation_z_reader, n_sample_orientation_z_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->p_detector_window_xW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->p_detector_window_xW), "detector window position x changed", p_detector_window_x_writer, p_detector_window_x_reader, p_detector_window_x_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->p_detector_window_yW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->p_detector_window_yW), "detector window position y changed", p_detector_window_y_writer, p_detector_window_y_reader, p_detector_window_y_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->p_detector_window_zW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->p_detector_window_zW), "detector window position z changed", p_detector_window_z_writer, p_detector_window_z_reader, p_detector_window_z_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->n_detector_orientation_xW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->n_detector_orientation_xW), "detector orientation vector x changed", n_detector_orientation_x_writer, n_detector_orientation_x_reader, n_detector_orientation_x_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->n_detector_orientation_yW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->n_detector_orientation_yW), "detector orientation vector y changed", n_detector_orientation_y_writer, n_detector_orientation_y_reader, n_detector_orientation_y_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->n_detector_orientation_zW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->n_detector_orientation_zW), "detector orientation vector z changed", n_detector_orientation_z_writer, n_detector_orientation_z_reader, n_detector_orientation_z_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->area_detectorW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->area_detectorW), "detector area changed", area_detector_writer, area_detector_reader, area_detector_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->collimator_heightW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->collimator_heightW), "collimator height changed", collimator_height_writer, collimator_height_reader, collimator_height_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->collimator_diameterW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->collimator_diameterW), "collimator diamater changed", collimator_diameter_writer, collimator_diameter_reader, collimator_diameter_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->d_source_slitW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->d_source_slitW), "distance source to slits changed", d_source_slit_writer, d_source_slit_reader, d_source_slit_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->slit_size_xW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->slit_size_xW), "slit size x changed", slit_size_x_writer, slit_size_x_reader, slit_size_x_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->slit_size_yW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->slit_size_yW), "slit size y changed", slit_size_y_writer, slit_size_y_reader, slit_size_y_validator);

	// energies
	xmi_msim_gui_undo_manager_register_energies_box(self->undo_manager, XMI_MSIM_GUI_ENERGIES_BOX(self->energiesW));

	// absorbers
	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->exc_compositionW);
	xmi_msim_gui_undo_manager_register_layer_box(self->undo_manager, XMI_MSIM_GUI_LAYER_BOX(self->exc_compositionW));

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->det_compositionW);
	xmi_msim_gui_undo_manager_register_layer_box(self->undo_manager, XMI_MSIM_GUI_LAYER_BOX(self->det_compositionW));

	// detector
	xmi_msim_gui_undo_manager_register_combo_box_text(self->undo_manager, GTK_COMBO_BOX_TEXT(self->detector_typeW), "detector type changed", detector_type_writer, detector_type_reader);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->detector_gainW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->detector_gainW), "detector gain changed", detector_gain_writer, detector_gain_reader, detector_gain_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->detector_live_timeW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->detector_live_timeW), "live time changed", detector_live_time_writer, detector_live_time_reader, detector_live_time_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->detector_pulse_widthW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->detector_pulse_widthW), "pulse width changed", detector_pulse_width_writer, detector_pulse_width_reader, detector_pulse_width_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->detector_zeroW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->detector_zeroW), "detector zero changed", detector_zero_writer, detector_zero_reader, detector_zero_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->detector_fanoW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->detector_fanoW), "detector fano factor changed", detector_fano_writer, detector_fano_reader, detector_fano_validator);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->detector_noiseW);
	xmi_msim_gui_undo_manager_register_entry(self->undo_manager, GTK_ENTRY(self->detector_noiseW), "detector electronic noise changed", detector_noise_writer, detector_noise_reader, detector_noise_validator);

	xmi_msim_gui_undo_manager_register_spin_button(self->undo_manager, GTK_SPIN_BUTTON(self->detector_nchannelsW), "number of channels changed", detector_nchannels_writer, detector_nchannels_reader);

	xmi_msim_gui_clipboard_manager_register_widget(self->clipboard_manager, self->crystal_compositionW);
	xmi_msim_gui_undo_manager_register_layer_box(self->undo_manager, XMI_MSIM_GUI_LAYER_BOX(self->crystal_compositionW));

}

static void xmi_msim_gui_xmsi_config_scrolled_window_class_init(XmiMsimGuiXmsiConfigScrolledWindowClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_xmsi_config_scrolled_window_dispose;
	object_class->finalize = xmi_msim_gui_xmsi_config_scrolled_window_finalize;
	object_class->set_property = xmi_msim_gui_xmsi_config_scrolled_window_set_property;
	object_class->constructed = xmi_msim_gui_xmsi_config_scrolled_window_constructed;

	g_object_class_install_property(object_class,
		1,
		g_param_spec_object(
			"undo-manager",
			"Undo Manager",
			"Undo Manager",
			XMI_MSIM_GUI_TYPE_UNDO_MANAGER,
    			(GParamFlags) (G_PARAM_WRITABLE | G_PARAM_CONSTRUCT)
		)
	);
	g_object_class_install_property(object_class,
		2,
		g_param_spec_object(
			"clipboard-manager",
			"Clipboard Manager",
			"Clipboard Manager",
			XMI_MSIM_GUI_TYPE_CLIPBOARD_MANAGER,
    			(GParamFlags) (G_PARAM_WRITABLE | G_PARAM_CONSTRUCT)
		)
	);
}

#define GEOMETRY_HELP_SCALE_FACTOR_DEFAULT 8.0

static double sample_source_distance_coords[2][2] = {{530, 498}, {652, 584}};
static double sample_orientation_coords[2][2] = {{1232, 200}, {1385, 290}};
static double detector_window_position_coords[2][2] = {{1417, 592}, {1573, 683}};
static double detector_window_normal_coords[2][2] = {{1604, 663}, {1744, 725}};
static double active_detector_area_coords[2][2] = {{1505, 497}, {1607, 564}};
static double collimator_height_coords[2][2] = {{1263, 647}, {1381, 780}};
static double collimator_diameter_coords[2][2] = {{1358, 450}, {1460, 540}};
static double source_slit_distance_coords[2][2] = {{374, 671}, {455, 750}};
static double slits_size_coords[2][2] = {{706, 510}, {981, 846}};

static void draw_box(double coords[2][2], cairo_t *cr, XmiMsimGuiXmsiConfigScrolledWindow *self) {
	cairo_set_source_rgb(cr, 0, 0, 0);
	cairo_set_line_width (cr, 1.0);

	cairo_move_to(cr, coords[0][0]/self->geometry_help_scale_factor, coords[0][1]/self->geometry_help_scale_factor);
	cairo_line_to(cr, coords[0][0]/self->geometry_help_scale_factor, coords[1][1]/self->geometry_help_scale_factor);
	cairo_move_to(cr, coords[0][0]/self->geometry_help_scale_factor, coords[1][1]/self->geometry_help_scale_factor);
	cairo_line_to(cr, coords[1][0]/self->geometry_help_scale_factor, coords[1][1]/self->geometry_help_scale_factor);
	cairo_move_to(cr, coords[1][0]/self->geometry_help_scale_factor, coords[1][1]/self->geometry_help_scale_factor);
	cairo_line_to(cr, coords[1][0]/self->geometry_help_scale_factor, coords[0][1]/self->geometry_help_scale_factor);
	cairo_move_to(cr, coords[1][0]/self->geometry_help_scale_factor, coords[0][1]/self->geometry_help_scale_factor);
	cairo_line_to(cr, coords[0][0]/self->geometry_help_scale_factor, coords[0][1]/self->geometry_help_scale_factor);
	cairo_stroke(cr);
}

static gboolean geometry_eb_enter_cb(GtkWidget *event_box, GdkEvent *event, XmiMsimGuiXmsiConfigScrolledWindow *self) {
	//entering the eventbox!
	//1) turn it chartreuse_green
	//2) draw the box

	GtkStyleContext *style_context = gtk_widget_get_style_context(event_box);
	gtk_style_context_add_class(style_context, "chartreuse");

	self->current_coords = g_hash_table_lookup(self->table, event_box);

	gtk_widget_queue_draw(self->cs_window);

	return FALSE;
}

static gboolean geometry_eb_leave_cb(GtkWidget *event_box, GdkEvent *event, XmiMsimGuiXmsiConfigScrolledWindow *self) {
	//leaving the eventbox!
	//1) turn it back to its normal color
	//2) remove the box

	GtkStyleContext *style_context = gtk_widget_get_style_context(event_box);
	gtk_style_context_remove_class(style_context, "chartreuse");

	self->current_coords = NULL;
	gtk_widget_queue_draw(self->cs_window);

	return FALSE;
}

static gboolean coords_check(double coords[2][2], double x, double y) {
	return x > coords[0][0] && x < coords[1][0] && y > coords[0][1] && y < coords[1][1];
}

static gboolean coordinate_system_motion_cb(GtkWidget *event_box, GdkEvent *event, XmiMsimGuiXmsiConfigScrolledWindow *self) {
	gdouble x = event->motion.x * self->geometry_help_scale_factor;
	gdouble y = event->motion.y * self->geometry_help_scale_factor;

	GHashTableIter iter;
	gpointer key, value;

	g_hash_table_iter_init (&iter, self->table);
	while (g_hash_table_iter_next (&iter, &key, &value)) {
		if (coords_check((double (*)[2]) value, x, y)) {
			GtkStyleContext *style_context = gtk_widget_get_style_context(GTK_WIDGET(key));
			gtk_style_context_add_class(style_context, "chartreuse");
			self->current_coords = (double *) value;
			gtk_widget_queue_draw(self->cs_window);
			return FALSE;
		}
	}

	g_hash_table_iter_init (&iter, self->table);
	while (g_hash_table_iter_next (&iter, &key, &value)) {
		GtkWidget *w = GTK_WIDGET(key);
		GtkStyleContext *style_context = gtk_widget_get_style_context(w);
		if (gtk_style_context_has_class(style_context, "chartreuse")) {
			gtk_style_context_remove_class(style_context, "chartreuse");
			break;
		}
	}

	self->current_coords = NULL;
	gtk_widget_queue_draw(self->cs_window);

	return FALSE;
}

static gboolean cs_window_delete_event(XmiMsimGuiXmsiConfigScrolledWindow *self) {
	g_signal_handler_block(G_OBJECT(self->geometry_helpW), self->geometry_helpG);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->geometry_helpW), FALSE);
	g_signal_handler_unblock(G_OBJECT(self->geometry_helpW), self->geometry_helpG);
	gtk_button_set_label(GTK_BUTTON(self->geometry_helpW), "Show geometry help");

	//deactivate all eventbox callbacks
	GHashTableIter iter;
	gpointer key;

	g_hash_table_iter_init (&iter, self->table);
	while (g_hash_table_iter_next (&iter, &key, NULL)) {
		g_signal_handlers_disconnect_by_data(key, self);
	}

	return FALSE;
}

static gboolean image_draw_event(GtkWidget *image, cairo_t *cr, XmiMsimGuiXmsiConfigScrolledWindow *self) {
	gint new_width = gtk_widget_get_allocated_width(image);
	gint new_height = gtk_widget_get_allocated_height(image);

	g_debug("image_draw_event width: %d", new_width);
	g_debug("image_draw_event height: %d", new_height);
	g_debug("orig_pixbuf width: %d", gdk_pixbuf_get_width(self->orig_pixbuf));

	if (new_width != self->old_width || new_height != self->old_height) {
		self->old_width = new_width;
		self->old_height = new_height;
		g_clear_object(&self->current_pixbuf);
		self->current_pixbuf = gdk_pixbuf_scale_simple(self->orig_pixbuf, new_width, new_height, GDK_INTERP_HYPER);
		self->geometry_help_scale_factor = (double) gdk_pixbuf_get_width(self->orig_pixbuf)/ (double) gdk_pixbuf_get_width(self->current_pixbuf);
	}
	
	gdk_cairo_set_source_pixbuf(cr, self->current_pixbuf, 0, 0);
	cairo_paint(cr);

	if (self->current_coords)
		draw_box((double (*)[2]) self->current_coords, cr, self);

	return TRUE;
}

static void geometry_help_clicked_cb(XmiMsimGuiXmsiConfigScrolledWindow *self) {

	GError *error = NULL;
	if (self->orig_pixbuf == NULL) {
		self->orig_pixbuf = gdk_pixbuf_new_from_resource("/com/github/tschoonj/xmimsim/gui/data/coordinate_system.png", &error);

		if (error != NULL) {
			g_warning("Could not open coordinate system file: %s\n", error->message);
			g_error_free(error);
			return;
		}
	}

	if (!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->geometry_helpW))) {
		cs_window_delete_event(self);
		gtk_widget_destroy(self->cs_window);
		return;
	}
	self->geometry_help_scale_factor = GEOMETRY_HELP_SCALE_FACTOR_DEFAULT;
	gtk_button_set_label(GTK_BUTTON(self->geometry_helpW), "Hide geometry help");
	self->cs_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_container_set_border_width(GTK_CONTAINER(self->cs_window), 5);
	gtk_window_set_title(GTK_WINDOW(self->cs_window), "Geometry coordinate system");
	gtk_window_set_position(GTK_WINDOW(self->cs_window), GTK_WIN_POS_NONE);
	g_signal_connect_swapped(G_OBJECT(self->cs_window), "delete-event", G_CALLBACK(cs_window_delete_event), self);

	GtkWidget *event_box = gtk_event_box_new();
	g_signal_connect(G_OBJECT(event_box), "motion-notify-event", G_CALLBACK(coordinate_system_motion_cb), self);

	GtkWidget *coordinate_system_image = gtk_drawing_area_new();
	gtk_widget_set_events(event_box, gtk_widget_get_events(event_box) | GDK_EXPOSURE_MASK | GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);
	gtk_container_add(GTK_CONTAINER(event_box), coordinate_system_image);
	gtk_container_add(GTK_CONTAINER(self->cs_window), event_box);
	self->old_width = 0;
	self->old_height = 0;

	GdkGeometry geometry;
	geometry.min_aspect = geometry.max_aspect = (double) gdk_pixbuf_get_width(self->orig_pixbuf)/(double) gdk_pixbuf_get_height(self->orig_pixbuf);

	gtk_window_set_geometry_hints(GTK_WINDOW(self->cs_window), NULL, &geometry, GDK_HINT_ASPECT);
	g_signal_connect(G_OBJECT(coordinate_system_image), "draw", G_CALLBACK(image_draw_event), self);

	//activate all signals for *_ebW
	GHashTableIter iter;
	gpointer key;

	g_hash_table_iter_init (&iter, self->table);
	while (g_hash_table_iter_next (&iter, &key, NULL)) {
		g_signal_connect(G_OBJECT(key), "enter-notify-event", G_CALLBACK(geometry_eb_enter_cb), self);
		g_signal_connect(G_OBJECT(key), "leave-notify-event", G_CALLBACK(geometry_eb_leave_cb), self);
	}

	gtk_window_set_default_size(GTK_WINDOW(self->cs_window), 1000, gdk_pixbuf_get_height(self->orig_pixbuf) * 1000.0 / gdk_pixbuf_get_width(self->orig_pixbuf));
	gtk_widget_show_all(self->cs_window);

#ifdef HAVE_GOOGLE_ANALYTICS
	const XmiMsimGuiGoogleAnalyticsTracker *tracker = xmi_msim_gui_google_analytics_tracker_get_global();
	xmi_msim_gui_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "SHOW-GEOMETRY-HELP", NULL, NULL);
#endif
}

static void select_outputfile_cb(GtkButton *button, XmiMsimGuiXmsiConfigScrolledWindow *self) {
	XmiMsimGuiFileChooserDialog *dialog;
	GtkFileFilter *filter;
	char *filename;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmso");
	gtk_file_filter_add_pattern(filter,"*.XMSO");
	gtk_file_filter_set_name(filter,"XMI-MSIM outputfiles");

	dialog = xmi_msim_gui_file_chooser_dialog_new("Select the outputfile for the simulation",
		GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(self))),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		"_Save",
		"_Cancel"
	);

	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	struct xmi_input *current = xmi_msim_gui_undo_manager_get_current_input(self->undo_manager);

	if (current == NULL || current->general->outputfile == NULL || strlen(current->general->outputfile) == 0) {
		union xmimsim_prefs_val prefs;
		if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_DEFAULT_SAVE_FOLDER, &prefs) == 0) {
			gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(dialog), g_get_user_special_dir(G_USER_DIRECTORY_DOCUMENTS));
		}
		else {
			gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(dialog), prefs.s);
			g_free(prefs.s);
		}
		gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(dialog), "Untitled.xmso");
	}
	else {
		gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(dialog), current->general->outputfile);
		gchar *name = g_path_get_basename(current->general->outputfile);
		gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(dialog), name);
		g_free(name);
	}
	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER(dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		//check extension
		xmi_msim_gui_utils_ensure_extension(&filename, ".xmso");
		gtk_entry_set_text(GTK_ENTRY(self->outputfileW), filename);
		g_free (filename);
	}
	xmi_free_input(current);

	xmi_msim_gui_file_chooser_dialog_destroy(dialog);

	return;
}

static void xmi_msim_gui_xmsi_config_scrolled_window_init(XmiMsimGuiXmsiConfigScrolledWindow *self) {
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);

	GtkWidget *frame, *superframe, *vbox_notebook, *hbox_text_label;
	GtkWidget *label;

	frame = gtk_frame_new("General");
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.5, 0.5);
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">General</span>");

	//Append general
	superframe = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	vbox_notebook = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook), 10);

	//outputfile
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Outputfile");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	label = gtk_button_new_with_label("Save");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(label), "clicked", G_CALLBACK(select_outputfile_cb), self);
	self->outputfileW = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->outputfileW), FALSE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->outputfileW, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(self->outputfileW), "aux-widget", label);

	//n_photons_interval
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of photons per interval");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->n_photons_intervalW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->n_photons_intervalW, FALSE, FALSE, 0);

	//n_photons_line
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of photons per discrete line");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->n_photons_lineW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->n_photons_lineW, FALSE, FALSE, 0);

	//n_interactions_trajectory
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of interactions per trajectory");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->n_interactions_trajectoryW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->n_interactions_trajectoryW, FALSE, FALSE, 0);

	//comments
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Comments");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->commentsW = gtk_text_view_new();
	gtk_container_set_border_width(GTK_CONTAINER(self->commentsW), 2);

	GtkWidget *comments_scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(comments_scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(comments_scrolled_window), self->commentsW);
	gtk_widget_set_size_request(comments_scrolled_window, 700, 100);
	GtkWidget *comments_frameW = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(comments_frameW), comments_scrolled_window);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), comments_frameW, TRUE, TRUE, 0);

	gtk_container_add(GTK_CONTAINER(frame), vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe), frame, FALSE, FALSE, 5);

	//composition
	self->compositionW = xmi_msim_gui_layer_box_new(XMI_MSIM_GUI_LAYER_BOX_TYPE_SAMPLE_COMPOSITION);
	gtk_container_set_border_width(GTK_CONTAINER(self->compositionW), 10);
	frame = gtk_frame_new("Composition");
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.5, 0.5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Composition</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	gtk_container_add(GTK_CONTAINER(frame), self->compositionW);
	gtk_box_pack_start(GTK_BOX(superframe), frame, FALSE, FALSE, 5);

	//geometry
	//d_sample_source
	vbox_notebook = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook), 5);
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	label = gtk_label_new("Sample-source distance (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->d_sample_sourceW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->d_sample_sourceW, FALSE, FALSE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	GtkWidget *d_sample_source_ebW = gtk_event_box_new();
	gtk_widget_set_name(d_sample_source_ebW, "color_event_box");
	gtk_container_add(GTK_CONTAINER(d_sample_source_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), d_sample_source_ebW, TRUE, FALSE, 3);

	//n_sample_orientation
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	GtkWidget *n_sample_orientation_ebW = gtk_event_box_new();
	gtk_widget_set_name(n_sample_orientation_ebW, "color_event_box");
	gtk_container_add(GTK_CONTAINER(n_sample_orientation_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), n_sample_orientation_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Sample orientation vector");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->n_sample_orientation_zW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(self->n_sample_orientation_zW), 10);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->n_sample_orientation_zW, FALSE, FALSE, 0);
	label = gtk_label_new("z:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->n_sample_orientation_yW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(self->n_sample_orientation_yW), 10);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->n_sample_orientation_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->n_sample_orientation_xW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(self->n_sample_orientation_xW), 10);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->n_sample_orientation_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	//p_detector_window
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	GtkWidget *p_detector_window_ebW = gtk_event_box_new();
	gtk_widget_set_name(p_detector_window_ebW, "color_event_box");
	gtk_container_add(GTK_CONTAINER(p_detector_window_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), p_detector_window_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Detector window position (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->p_detector_window_zW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(self->p_detector_window_zW), 10);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->p_detector_window_zW, FALSE, FALSE, 0);
	label = gtk_label_new("z:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->p_detector_window_yW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(self->p_detector_window_yW), 10);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->p_detector_window_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->p_detector_window_xW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(self->p_detector_window_xW), 10);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->p_detector_window_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	//n_detector_orientation
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	GtkWidget *n_detector_orientation_ebW = gtk_event_box_new();
	gtk_widget_set_name(n_detector_orientation_ebW, "color_event_box");
	gtk_container_add(GTK_CONTAINER(n_detector_orientation_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), n_detector_orientation_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Detector window normal vector");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->n_detector_orientation_zW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(self->n_detector_orientation_zW), 10);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->n_detector_orientation_zW, FALSE, FALSE, 0);
	label = gtk_label_new("z:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->n_detector_orientation_yW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(self->n_detector_orientation_yW), 10);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->n_detector_orientation_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->n_detector_orientation_xW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(self->n_detector_orientation_xW), 10);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->n_detector_orientation_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	//area detector
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	GtkWidget *area_detector_ebW = gtk_event_box_new();
	gtk_widget_set_name(area_detector_ebW, "color_event_box");
	gtk_container_add(GTK_CONTAINER(area_detector_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), area_detector_ebW, TRUE, FALSE, 3);
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label),"Active detector area (cm<sup>2</sup>)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->area_detectorW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->area_detectorW, FALSE, FALSE, 0);

	//collimator_height
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	GtkWidget *collimator_height_ebW = gtk_event_box_new();
	gtk_widget_set_name(collimator_height_ebW, "color_event_box");
	gtk_container_add(GTK_CONTAINER(collimator_height_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), collimator_height_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Collimator height (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->collimator_heightW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->collimator_heightW, FALSE, FALSE, 0);

	//collimator_diameter
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL ,5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	GtkWidget *collimator_diameter_ebW = gtk_event_box_new();
	gtk_widget_set_name(collimator_diameter_ebW, "color_event_box");
	gtk_container_add(GTK_CONTAINER(collimator_diameter_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), collimator_diameter_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Collimator diameter (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->collimator_diameterW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->collimator_diameterW, FALSE, FALSE, 0);

	//d_source_slit
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	GtkWidget *d_source_slit_ebW = gtk_event_box_new();
	gtk_widget_set_name(d_source_slit_ebW, "color_event_box");
	gtk_container_add(GTK_CONTAINER(d_source_slit_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), d_source_slit_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Source-slits distance (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->d_source_slitW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->d_source_slitW, FALSE, FALSE, 0);

	//slit sizes
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(hbox_text_label), 5);
	GtkWidget *slit_size_ebW = gtk_event_box_new();
	gtk_widget_set_name(slit_size_ebW, "color_event_box");
	gtk_container_add(GTK_CONTAINER(slit_size_ebW), hbox_text_label);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), slit_size_ebW, TRUE, FALSE, 3);
	label = gtk_label_new("Slits size (cm)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->slit_size_yW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->slit_size_yW, FALSE, FALSE, 0);
	label = gtk_label_new("y:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->slit_size_xW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->slit_size_xW, FALSE, FALSE, 0);
	label = gtk_label_new("x:");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);

	self->geometry_helpW = gtk_toggle_button_new_with_label("Show geometry help");
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(hbox_text_label), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox_text_label), self->geometry_helpW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	self->geometry_helpG = g_signal_connect_swapped(G_OBJECT(self->geometry_helpW), "toggled", G_CALLBACK(geometry_help_clicked_cb), self);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->geometry_helpW), FALSE);

	frame = gtk_frame_new("Geometry");

	gtk_frame_set_label_align(GTK_FRAME(frame), 0.5, 0.5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Geometry</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	gtk_container_add(GTK_CONTAINER(frame), vbox_notebook);
	gtk_box_pack_start(GTK_BOX(superframe), frame, FALSE, FALSE, 5);

	self->table = g_hash_table_new(g_direct_hash, g_direct_equal);
	g_assert_true(g_hash_table_insert(self->table, d_sample_source_ebW, sample_source_distance_coords));
	g_assert_true(g_hash_table_insert(self->table, n_sample_orientation_ebW, sample_orientation_coords));
	g_assert_true(g_hash_table_insert(self->table, p_detector_window_ebW, detector_window_position_coords));
	g_assert_true(g_hash_table_insert(self->table, n_detector_orientation_ebW, detector_window_normal_coords));
	g_assert_true(g_hash_table_insert(self->table, area_detector_ebW, active_detector_area_coords));
	g_assert_true(g_hash_table_insert(self->table, collimator_height_ebW, collimator_height_coords));
	g_assert_true(g_hash_table_insert(self->table, collimator_diameter_ebW, collimator_diameter_coords));
	g_assert_true(g_hash_table_insert(self->table, d_source_slit_ebW, source_slit_distance_coords));
	g_assert_true(g_hash_table_insert(self->table, slit_size_ebW, slits_size_coords));

	//energies

	frame = gtk_frame_new("Energies");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Excitation</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	self->energiesW = xmi_msim_gui_energies_box_new();
	gtk_container_add(GTK_CONTAINER(frame), self->energiesW);
	gtk_box_pack_start(GTK_BOX(superframe), frame, FALSE, FALSE, 5);


	//absorbers
	self->exc_compositionW = xmi_msim_gui_layer_box_new(XMI_MSIM_GUI_LAYER_BOX_TYPE_EXCITATION_ABSORBERS);
	gtk_container_set_border_width(GTK_CONTAINER(self->exc_compositionW), 10);

	frame = gtk_frame_new("Beam absorbers");
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.5, 0.5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Beam absorbers</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	gtk_container_add(GTK_CONTAINER(frame), self->exc_compositionW);
	gtk_box_pack_start(GTK_BOX(superframe), frame, FALSE, FALSE, 5);

	self->det_compositionW = xmi_msim_gui_layer_box_new(XMI_MSIM_GUI_LAYER_BOX_TYPE_DETECTOR_ABSORBERS);
	gtk_container_set_border_width(GTK_CONTAINER(self->det_compositionW), 10);

	frame = gtk_frame_new("Detection absorbers");
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.5, 0.5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Detection absorbers</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	gtk_container_add(GTK_CONTAINER(frame), self->det_compositionW);
	gtk_box_pack_start(GTK_BOX(superframe), frame, FALSE, FALSE, 5);

	//detector
	//detector type
	vbox_notebook = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook), 10);
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector type");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->detector_typeW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(self->detector_typeW), "Si(Li)");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(self->detector_typeW), "Ge");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(self->detector_typeW), "SDD");
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->detector_typeW, FALSE, FALSE, 0);

	//channels
	GtkAdjustment *spinner_adj = GTK_ADJUSTMENT(gtk_adjustment_new(2048.0, 10.0, 100000.0, 1.0, 10.0, 0.0));
	self->detector_nchannelsW = gtk_spin_button_new(spinner_adj, 1, 0);
	gtk_editable_set_editable(GTK_EDITABLE(self->detector_nchannelsW), TRUE);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(self->detector_nchannelsW), GTK_UPDATE_IF_VALID);
	gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(self->detector_nchannelsW), TRUE);
	gtk_entry_set_max_length(GTK_ENTRY(self->detector_nchannelsW), 7);
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	label = gtk_label_new("Number of spectrum channels");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->detector_nchannelsW, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);

	//gain
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector gain (keV/channel)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->detector_gainW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->detector_gainW, FALSE, FALSE, 0);

	//zero
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector zero (keV)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->detector_zeroW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->detector_zeroW, FALSE, FALSE, 0);

	//fano
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector Fano factor");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->detector_fanoW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->detector_fanoW, FALSE, FALSE, 0);

	//noise
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Detector electronic noise (keV)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->detector_noiseW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->detector_noiseW, FALSE, FALSE, 0);

	//live time
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Live time (s)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->detector_live_timeW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->detector_live_timeW, FALSE, FALSE, 0);

	//pulse_width
	hbox_text_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Pulse width (s)");
	gtk_box_pack_start(GTK_BOX(hbox_text_label), label, FALSE, FALSE, 0);
	self->detector_pulse_widthW = gtk_entry_new();
	gtk_box_pack_end(GTK_BOX(hbox_text_label), self->detector_pulse_widthW, FALSE, FALSE, 0);

	//crystal
	self->crystal_compositionW = xmi_msim_gui_layer_box_new(XMI_MSIM_GUI_LAYER_BOX_TYPE_CRYSTAL_COMPOSITION);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), self->crystal_compositionW, TRUE, FALSE, 3);

	frame = gtk_frame_new("Detector settings");

	gtk_frame_set_label_align(GTK_FRAME(frame), 0.5, 0.5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Detector settings</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	gtk_container_add(GTK_CONTAINER(frame), vbox_notebook);
	gtk_box_pack_start(GTK_BOX(superframe), frame, FALSE, FALSE, 5);
	gtk_widget_show_all(superframe);
	gtk_container_add(GTK_CONTAINER(self), superframe);
}

GtkWidget* xmi_msim_gui_xmsi_config_scrolled_window_new(XmiMsimGuiUndoManager *undo_manager, XmiMsimGuiClipboardManager *clipboard_manager) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(undo_manager), NULL);
	g_return_val_if_fail(XMI_MSIM_GUI_IS_CLIPBOARD_MANAGER(clipboard_manager), NULL);

	XmiMsimGuiXmsiConfigScrolledWindow *scrolled_window = XMI_MSIM_GUI_XMSI_CONFIG_SCROLLED_WINDOW(g_object_new(XMI_MSIM_GUI_TYPE_XMSI_CONFIG_SCROLLED_WINDOW, "undo-manager", undo_manager, "clipboard-manager", clipboard_manager, NULL));

	return GTK_WIDGET(scrolled_window);
}
