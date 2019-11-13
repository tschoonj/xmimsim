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

#include "xmimsim-gui-batch-archive-settings-box.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-private.h"
#include "xmi_xml.h"
#include <libxml/xpath.h>
#include <string.h>

// these defines will be used to set and get data in XmiMsimGuiXmsiSelectionXPathData objects
static G_DEFINE_QUARK("grandfather-node", grandfather_node)
#define DATA_NODE_GRANDFATHER grandfather_node_quark()

G_DEFINE_QUARK("common-weight-fraction-data", common_weight_fraction_data)

static G_DEFINE_QUARK("start-entry", start_entry)
#define DATA_START_ENTRY start_entry_quark()

static G_DEFINE_QUARK("end-entry", end_entry)
#define DATA_END_ENTRY end_entry_quark()

static G_DEFINE_QUARK("nsteps-entry", nsteps_entry)
#define DATA_NSTEPS_ENTRY nsteps_entry_quark()

static G_DEFINE_QUARK("status", status)
#define ENTRY_STATUS status_quark()

static G_DEFINE_QUARK("xpath-data", xpath_data)
#define ENTRY_XPATH_DATA xpath_data_quark()

static G_DEFINE_QUARK("type", type)
#define ENTRY_TYPE type_quark()

enum {
	ENTRY_TYPE_START,
	ENTRY_TYPE_END,
	ENTRY_TYPE_NSTEPS,
};

static void common_weight_fraction_data_free(gpointer _data) {
	struct common_weight_fraction_data *data = _data;

	if (data->all_xpath_data)
		g_ptr_array_unref(data->all_xpath_data);

	g_free(data);
}


struct _XmiMsimGuiBatchArchiveSettingsBox {
	GtkBox parent_instance;
	GPtrArray *xpath_expressions;
	GPtrArray *single_batch_data;
	gchar *archive_file;
	gchar *input_file;
	xmi_input *input;
	GtkWidget *archive_entry;
};

struct _XmiMsimGuiBatchArchiveSettingsBoxClass {
	GtkBoxClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiBatchArchiveSettingsBox, xmi_msim_gui_batch_archive_settings_box, GTK_TYPE_BOX)

static void xmi_msim_gui_batch_archive_settings_box_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_batch_archive_settings_box_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_batch_archive_settings_box_finalize(GObject *gobject) {
	XmiMsimGuiBatchArchiveSettingsBox *self = XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX(gobject);

	if (self->xpath_expressions)
		g_ptr_array_unref(self->xpath_expressions);
	if (self->single_batch_data)
		g_ptr_array_unref(self->single_batch_data);

	g_free(self->archive_file);
	g_free(self->input_file);

	xmi_input_free(self->input);

	G_OBJECT_CLASS(xmi_msim_gui_batch_archive_settings_box_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_batch_archive_settings_box_constructed(GObject *gobject);

enum {
	PROP_0,
	PROP_ARCHIVE_FILE,
	PROP_INPUT_FILE,
	PROP_XPATH_EXPRESSIONS,
	PROP_SINGLE_BATCH_DATA,
	N_PROPERTIES
};

static GParamSpec *props[N_PROPERTIES] = {NULL, };

static void xmi_msim_gui_batch_archive_settings_box_set_property(GObject          *object,
                                                guint             prop_id,
                                                const GValue     *value,
                                                GParamSpec       *pspec);

static void xmi_msim_gui_batch_archive_settings_box_get_property(GObject          *object,
                                                guint             prop_id,
                                                GValue     *value,
                                                GParamSpec       *pspec);

static void xmi_msim_gui_batch_archive_settings_box_class_init(XmiMsimGuiBatchArchiveSettingsBoxClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_batch_archive_settings_box_dispose;
	object_class->finalize = xmi_msim_gui_batch_archive_settings_box_finalize;
	object_class->constructed = xmi_msim_gui_batch_archive_settings_box_constructed;
	object_class->set_property = xmi_msim_gui_batch_archive_settings_box_set_property;
	object_class->get_property = xmi_msim_gui_batch_archive_settings_box_get_property;

	props[PROP_ARCHIVE_FILE] = g_param_spec_string(
		"archive-file",
		"archive-file",
		"archive-file",
		NULL,
    		G_PARAM_READABLE
	);

	props[PROP_INPUT_FILE] = g_param_spec_string(
		"input-file",
		"input-file",
		"input-file",
		NULL,
    		G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY
	);

	props[PROP_XPATH_EXPRESSIONS] = g_param_spec_boxed(
		"xpath-expressions",
		"xpath-expressions",
		"xpath-expressions",
		G_TYPE_PTR_ARRAY,
    		G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY
	);

	props[PROP_SINGLE_BATCH_DATA] = g_param_spec_boxed(
		"single-batch-data",
		"single-batch-data",
		"single-batch-data",
		G_TYPE_PTR_ARRAY,
    		G_PARAM_READABLE
	);

	g_object_class_install_properties(object_class, N_PROPERTIES, props);
}

static void xmi_msim_gui_batch_archive_settings_box_init(XmiMsimGuiBatchArchiveSettingsBox *self) {

}

static void xmi_msim_gui_batch_archive_settings_box_set_property(GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec) {
  XmiMsimGuiBatchArchiveSettingsBox *self = XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX(object);

  switch (prop_id) {
    case PROP_INPUT_FILE:
      g_free(self->input_file);
      self->input_file = g_value_dup_string(value);
      break;
    case PROP_XPATH_EXPRESSIONS:
      if (self->xpath_expressions)
	g_ptr_array_unref(self->xpath_expressions);
      self->xpath_expressions = g_value_dup_boxed(value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static void xmi_msim_gui_batch_archive_settings_box_get_property(GObject *object, guint prop_id, GValue *value, GParamSpec *pspec) {
  XmiMsimGuiBatchArchiveSettingsBox *self = XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX(object);

  switch (prop_id) {
    case PROP_ARCHIVE_FILE:
      g_value_set_string(value, self->archive_file);
      break;
    case PROP_INPUT_FILE:
      g_value_set_string(value, self->input_file);
      break;
    case PROP_XPATH_EXPRESSIONS:
      g_value_set_boxed(value, self->xpath_expressions);
      break;
    case PROP_SINGLE_BATCH_DATA:
      g_value_set_boxed(value, self->single_batch_data);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static void archivesaveButton_clicked_cb(GtkButton *saveButton, XmiMsimGuiBatchArchiveSettingsBox *self) {
	XmiMsimGuiFileChooserDialog *dialog  = xmi_msim_gui_file_chooser_dialog_new(
		"Select the filename of the XMSA file",
		GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(self))),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		"_Save",
		"_Cancel");
	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);
	GtkFileFilter *filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsa");
	gtk_file_filter_add_pattern(filter,"*.XMSA");
	gtk_file_filter_set_name(filter,"XMI-MSIM archive files");
	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(dialog), self->archive_file);
	gchar *filename;

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		xmi_msim_gui_utils_ensure_extension(&filename, ".xmsa");
		gtk_entry_set_text(GTK_ENTRY(self->archive_entry), filename);
		self->archive_file = filename;
		g_object_notify_by_pspec(G_OBJECT(self), props[PROP_ARCHIVE_FILE]);
	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);
}

static gboolean entry_get_text_as_double(GtkEntry *entry, double *value) {
	g_assert(value != NULL);

	gchar *textPtr = g_strdup(gtk_entry_get_text(entry));
	gchar *textTrimPtr = g_strstrip(textPtr);
	gchar *endPtr = NULL;
	*value = strtod(textTrimPtr, &endPtr);
	gchar *lastPtr = textTrimPtr + strlen(textTrimPtr);
	gboolean rv = TRUE;

	if (strlen(textTrimPtr) == 0 || lastPtr != endPtr) {
		rv = FALSE;
	}

	g_free(textPtr);

	return rv;
}

static gboolean entry_get_text_as_long(GtkEntry *entry, long *value) {
	g_assert(value != NULL);

	gchar *textPtr = g_strdup(gtk_entry_get_text(entry));
	gchar *textTrimPtr = g_strstrip(textPtr);
	gchar *endPtr = NULL;
	*value = strtol(textTrimPtr, &endPtr, 10);
	gchar *lastPtr = textTrimPtr + strlen(textTrimPtr);
	gboolean rv = TRUE;

	if (strlen(textTrimPtr) == 0 || lastPtr != endPtr) {
		rv = FALSE;
	}

	g_free(textPtr);

	return rv;
}

static void update_single_batch_data(XmiMsimGuiBatchArchiveSettingsBox *self) {

	if (self->single_batch_data) {
		g_ptr_array_unref(self->single_batch_data);
		self->single_batch_data = NULL;
	}

	// check if everything is valid first
	gboolean all_valid = TRUE;
	unsigned int i;
	for (i = 0 ; i < self->xpath_expressions->len ; i++) {
		XmiMsimGuiXmsiSelectionXPathData *xpath_data = g_ptr_array_index(self->xpath_expressions, i);
		{
			GtkWidget *start_entry = g_object_get_qdata(G_OBJECT(xpath_data), DATA_START_ENTRY);
			if (GPOINTER_TO_INT(g_object_get_qdata(G_OBJECT(start_entry), ENTRY_STATUS)) == 0) {
				all_valid = FALSE;
				break;
			}
		}

		{
			GtkWidget *end_entry = g_object_get_qdata(G_OBJECT(xpath_data), DATA_END_ENTRY);
			if (GPOINTER_TO_INT(g_object_get_qdata(G_OBJECT(end_entry), ENTRY_STATUS)) == 0) {
				all_valid = FALSE;
				break;
			}
		}

		{
			GtkWidget *nsteps_entry = g_object_get_qdata(G_OBJECT(xpath_data), DATA_NSTEPS_ENTRY);
			if (GPOINTER_TO_INT(g_object_get_qdata(G_OBJECT(nsteps_entry), ENTRY_STATUS)) == 0) {
				all_valid = FALSE;
				break;
			}
		}
	}

	if (!all_valid) {
		g_object_notify_by_pspec(G_OBJECT(self), props[PROP_SINGLE_BATCH_DATA]);
		return;
	}

	self->single_batch_data = g_ptr_array_new_full(self->xpath_expressions->len, (GDestroyNotify) xmi_batch_single_data_free);

	for (i = 0 ; i < self->xpath_expressions->len ; i++) {
		XmiMsimGuiXmsiSelectionXPathData *xpath_data = g_ptr_array_index(self->xpath_expressions, i);

		xmi_batch_single_data *sdata = g_malloc0(sizeof(xmi_batch_single_data));
		sdata->xpath = xmi_msim_gui_xmsi_selection_xpath_data_get_string(xpath_data);

		{
			GtkWidget *start_entry = g_object_get_qdata(G_OBJECT(xpath_data), DATA_START_ENTRY);
			double value = 0.0;
			gboolean valid = entry_get_text_as_double(GTK_ENTRY(start_entry), &value);
			g_assert(valid == TRUE);
			sdata->start = value;
		}

		{
			GtkWidget *end_entry = g_object_get_qdata(G_OBJECT(xpath_data), DATA_END_ENTRY);
			double value = 0.0;
			gboolean valid = entry_get_text_as_double(GTK_ENTRY(end_entry), &value);
			g_assert(valid == TRUE);
			sdata->end = value;
		}

		{
			GtkWidget *nsteps_entry = g_object_get_qdata(G_OBJECT(xpath_data), DATA_NSTEPS_ENTRY);
			long value = 0;
			gboolean valid = entry_get_text_as_long(GTK_ENTRY(nsteps_entry), &value);
			g_assert(valid == TRUE);
			sdata->nsteps = value;
		}

		g_ptr_array_add(self->single_batch_data, sdata);
	}
		
	g_object_notify_by_pspec(G_OBJECT(self), props[PROP_SINGLE_BATCH_DATA]);
}

static void common_weight_fraction_data_set_status(struct common_weight_fraction_data *common_data, GQuark entry_type, int status) {
	unsigned int i;

	for (i = 0 ; i < common_data->all_xpath_data->len ; i++) {
		XmiMsimGuiXmsiSelectionXPathData *xpath_data = g_ptr_array_index(common_data->all_xpath_data, i);
		GtkEditable *entry = g_object_get_qdata(G_OBJECT(xpath_data), entry_type);
		GtkStyleContext *style_context = gtk_widget_get_style_context(GTK_WIDGET(entry));
		g_object_set_qdata(G_OBJECT(entry), ENTRY_STATUS, GINT_TO_POINTER(status));

		if (status) {
      			gtk_style_context_remove_class(style_context, "red");
		}
		else {
      			gtk_style_context_add_class(style_context, "red");
		}
	}
}

static gboolean common_weight_fraction_data_get_sum(struct common_weight_fraction_data *common_data, GQuark entry_type, double *value_sum) {
	gboolean all_valid = TRUE;
	unsigned int i;
	*value_sum = 0.0;

	for (i = 0 ; i < common_data->all_xpath_data->len ; i++) {
		XmiMsimGuiXmsiSelectionXPathData *xpath_data = g_ptr_array_index(common_data->all_xpath_data, i);
		GtkEditable *entry = g_object_get_qdata(G_OBJECT(xpath_data), entry_type);

		double value = 0.0;
		gboolean proper_value = entry_get_text_as_double(GTK_ENTRY(entry), &value);
		if (!proper_value || value < 0.0) {
			all_valid = FALSE;
			break;
		}
		*value_sum += value;
	}

	return all_valid;
}

static gboolean common_weight_fraction_data_check_per_xpath_data(struct common_weight_fraction_data *common_data) {
	gboolean all_valid = TRUE;
	unsigned int i;

	for (i = 0 ; i < common_data->all_xpath_data->len ; i++) {
		XmiMsimGuiXmsiSelectionXPathData *xpath_data = g_ptr_array_index(common_data->all_xpath_data, i);
		GtkEditable *start_entry = g_object_get_qdata(G_OBJECT(xpath_data), DATA_START_ENTRY);
		GtkEditable *end_entry = g_object_get_qdata(G_OBJECT(xpath_data), DATA_END_ENTRY);

		double start_value = 0.0;
		gboolean proper_value_start = entry_get_text_as_double(GTK_ENTRY(start_entry), &start_value);
		if (!proper_value_start) {
			all_valid = FALSE;
			break;
		}
		double end_value = 0.0;
		gboolean proper_value_end = entry_get_text_as_double(GTK_ENTRY(end_entry), &end_value);
		if (!proper_value_end) {
			all_valid = FALSE;
			break;
		}

		if (end_value <= start_value) {
			all_valid = FALSE;
			break;
		}
	}

	return all_valid;
}

static void xpath_entry_changed_cb(XmiMsimGuiBatchArchiveSettingsBox *self, GtkEditable *entry) {

	// get type of entry
	guint entry_type = GPOINTER_TO_INT(g_object_get_qdata(G_OBJECT(entry), ENTRY_TYPE));

	// get style context
	GtkStyleContext *style_context = gtk_widget_get_style_context(GTK_WIDGET(entry));

	if (entry_type == ENTRY_TYPE_NSTEPS) {
		// this is the easiest case
		long nsteps;
		gboolean valid = entry_get_text_as_long(GTK_ENTRY(entry), &nsteps);

		if (!valid || nsteps < 1) {
      			gtk_style_context_add_class(style_context, "red");
			g_object_set_qdata(G_OBJECT(entry), ENTRY_STATUS, GINT_TO_POINTER(0));
		}
		else {
      			gtk_style_context_remove_class(style_context, "red");
			g_object_set_qdata(G_OBJECT(entry), ENTRY_STATUS, GINT_TO_POINTER(1));
		}

		update_single_batch_data(self);

		return;
	}

	// get xpath data
	XmiMsimGuiXmsiSelectionXPathData *xpath_data = g_object_get_qdata(G_OBJECT(entry), ENTRY_XPATH_DATA);
	g_assert(xpath_data != NULL);

	// get xpath data flags
	XmiMsimGuiXmsiSelectionXPathFlags flags = xmi_msim_gui_xmsi_selection_xpath_data_get_flags(xpath_data);

	if (entry_type == ENTRY_TYPE_START) {
		double value;
		gboolean proper_value;
		struct common_weight_fraction_data *common_data = g_object_get_qdata(G_OBJECT(xpath_data), DATA_COMMON_WEIGHT_FRACTION);

		if ((proper_value = entry_get_text_as_double(GTK_ENTRY(entry), &value)) == TRUE) {
			// we have a proper value but is it within the accepted range?
			if (flags & XMI_MSIM_GUI_XMSI_SELECTION_XPATH_STRICT_POSITIVE && value <= 0.0) {
				proper_value = FALSE;
			}
			else if (flags & XMI_MSIM_GUI_XMSI_SELECTION_XPATH_POSITIVE && value < 0.0) {
				proper_value = FALSE;
			}
			else if (flags & XMI_MSIM_GUI_XMSI_SELECTION_XPATH_WEIGHT_FRACTION) {

				if (common_data != NULL) {
					// check sum of all start entry values -> must be less than 100!
					double value_sum = 0.0;
					gboolean all_valid = common_weight_fraction_data_get_sum(common_data, DATA_START_ENTRY, &value_sum);

					if (!all_valid) {
						proper_value = FALSE;
					}
					else if (value_sum >= 100.0) {
						// all start entries from the common_weight_fraction_data struct should go red
						proper_value = FALSE;
					}
				}
				else if (value < 0.0 || value >= 100.0) {
					proper_value = FALSE;
				}
			}
		}

		if (proper_value) {
			if (common_data) {
				// in case of coupled weight fractions -> ensure red is removed everywhere
				common_weight_fraction_data_set_status(common_data, DATA_START_ENTRY, 1);

				// next: check what needs to be done about the corresponding endEntries!
				double value_sum_start = 0.0;
				gboolean all_valid_start = common_weight_fraction_data_get_sum(common_data, DATA_START_ENTRY, &value_sum_start);
				g_assert(all_valid_start == TRUE);

				double value_sum_end = 0.0;
				gboolean all_valid_end = common_weight_fraction_data_get_sum(common_data, DATA_END_ENTRY, &value_sum_end);
				if (all_valid_end) {
					if (value_sum_start < value_sum_end && value_sum_end <= 100.0 && common_weight_fraction_data_check_per_xpath_data(common_data)) {
						common_weight_fraction_data_set_status(common_data, DATA_END_ENTRY, 1);
					}
					else {
						common_weight_fraction_data_set_status(common_data, DATA_END_ENTRY, 0);
					}	
				}
			}
			else {
	      			gtk_style_context_remove_class(style_context, "red");
				g_object_set_qdata(G_OBJECT(entry), ENTRY_STATUS, GINT_TO_POINTER(1));

				// next: check what needs to be done about the corresponding endEntry!
				GtkEditable *end_entry = g_object_get_qdata(G_OBJECT(xpath_data), DATA_END_ENTRY);
				GtkStyleContext *style_context2 = gtk_widget_get_style_context(GTK_WIDGET(end_entry));
				// if status is 1, then the value is guaranteed to be in the acceptible range
				int status = GPOINTER_TO_INT(g_object_get_qdata(G_OBJECT(end_entry), ENTRY_STATUS));

				double value2;
				gboolean proper_value2 = entry_get_text_as_double(GTK_ENTRY(end_entry), &value2);

				if (status) {
					g_assert(proper_value2 == TRUE);

					// value2 is less than value! -> bad
					if (value >= value2) {
      						gtk_style_context_add_class(style_context2, "red");
						g_object_set_qdata(G_OBJECT(end_entry), ENTRY_STATUS, GINT_TO_POINTER(0));
					}
				}
				else {
					// in this case the current value in end_entry is invalid, but it may have become valid now that start_entry has been updated...
					if (proper_value2) {
						proper_value2 = FALSE;
						if (flags & XMI_MSIM_GUI_XMSI_SELECTION_XPATH_WEIGHT_FRACTION && value < value2 && value2 <= 100.0) {
							proper_value2 = TRUE;
						}
						else if (value2 > value) {
							proper_value2 = TRUE;
						}

						if (proper_value2) {
      							gtk_style_context_remove_class(style_context2, "red");
							g_object_set_qdata(G_OBJECT(end_entry), ENTRY_STATUS, GINT_TO_POINTER(1));
						}
					}
				}
			}
		}
		else {
			if (common_data) {
				common_weight_fraction_data_set_status(common_data, DATA_START_ENTRY, 0);
			}
			else {
      				gtk_style_context_add_class(style_context, "red");
				g_object_set_qdata(G_OBJECT(entry), ENTRY_STATUS, GINT_TO_POINTER(0));
			}
		}
	}
	else if (entry_type == ENTRY_TYPE_END) {
		double value;
		gboolean proper_value;
		struct common_weight_fraction_data *common_data = g_object_get_qdata(G_OBJECT(xpath_data), DATA_COMMON_WEIGHT_FRACTION);

		if ((proper_value = entry_get_text_as_double(GTK_ENTRY(entry), &value)) == TRUE) {
			// we have a proper value but is it within the accepted range?
			if (flags & XMI_MSIM_GUI_XMSI_SELECTION_XPATH_STRICT_POSITIVE && value < 0.0) {
				proper_value = FALSE;
			}
			else if (flags & XMI_MSIM_GUI_XMSI_SELECTION_XPATH_POSITIVE && value < 0.0) {
				proper_value = FALSE;
			}
			else if (flags & XMI_MSIM_GUI_XMSI_SELECTION_XPATH_WEIGHT_FRACTION) {
				if (common_data != NULL) {
					// check sum of all end entry values -> must be less than 100!
					double value_sum = 0.0;
					gboolean all_valid = common_weight_fraction_data_get_sum(common_data, DATA_END_ENTRY, &value_sum);

					if (!all_valid) {
						proper_value = FALSE;
					}
					else if (value_sum > 100.0) {
						// all start entries from the common_weight_fraction_data struct should go red
						proper_value = FALSE;
					}
				}
				else if (value < 0.0 || value > 100.0) {
					proper_value = FALSE;
				}
			}
		}

		if (proper_value) {
			if (common_data) {
				// to make everything go green, we need to compare against the start entries, both an a per XPath base as well as their sums
				double value_sum_start = 0.0;
				gboolean all_valid_start = common_weight_fraction_data_get_sum(common_data, DATA_START_ENTRY, &value_sum_start);

				double value_sum_end = 0.0;
				gboolean all_valid_end = common_weight_fraction_data_get_sum(common_data, DATA_END_ENTRY, &value_sum_end);
				g_assert(all_valid_end == TRUE);

				if (all_valid_start && value_sum_start < value_sum_end && common_weight_fraction_data_check_per_xpath_data(common_data)) {
					common_weight_fraction_data_set_status(common_data, DATA_END_ENTRY, 1);
				}
				else {
					common_weight_fraction_data_set_status(common_data, DATA_END_ENTRY, 0);
				}
				// we do not change the start entries here!
			}
			else {
				GtkEditable *start_entry = g_object_get_qdata(G_OBJECT(xpath_data), DATA_START_ENTRY);
				double start_value = 0.0;
				gboolean proper_value_start = entry_get_text_as_double(GTK_ENTRY(start_entry), &start_value);
				if (proper_value_start && start_value < value) {
	      				gtk_style_context_remove_class(style_context, "red");
					g_object_set_qdata(G_OBJECT(entry), ENTRY_STATUS, GINT_TO_POINTER(1));
				}
				else {
	      				gtk_style_context_add_class(style_context, "red");
					g_object_set_qdata(G_OBJECT(entry), ENTRY_STATUS, GINT_TO_POINTER(0));
				}
			}
		}
		else {
			if (common_data) {
				common_weight_fraction_data_set_status(common_data, DATA_END_ENTRY, 0);
			}
			else {
      				gtk_style_context_add_class(style_context, "red");
				g_object_set_qdata(G_OBJECT(entry), ENTRY_STATUS, GINT_TO_POINTER(0));
			}
		}
	}

	update_single_batch_data(self);
}


static void xmi_msim_gui_batch_archive_settings_box_constructed(GObject *gobject) {
	XmiMsimGuiBatchArchiveSettingsBox *self = XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX(gobject);

	gtk_orientable_set_orientation(GTK_ORIENTABLE(gobject), GTK_ORIENTATION_VERTICAL);
	gtk_box_set_spacing(GTK_BOX(self), 2);
	gtk_box_set_homogeneous(GTK_BOX(self), FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(gobject), 3);

	// read the input-file into its XML structure, for XPath manipulation
	xmlDocPtr doc = xmlReadFile(self->input_file, NULL, XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR);
	g_assert(doc != NULL);
	
	// read the input-file into our XMI-MSIM struct
	self->input = xmi_input_read_from_xml_file(self->input_file, NULL);
	g_assert(self->input != NULL);

	if (self->xpath_expressions->len > 1) {
		// now the hard part... determine which xpath strings are weight fractions, and if they belong to the same layer...
		// first determine the grandfathers
		xmlXPathContextPtr context = xmlXPathNewContext(doc);
		g_assert(context != NULL);

		unsigned int i = 0;
		for (i = 0 ; i < self->xpath_expressions->len ; i++) {
			XmiMsimGuiXmsiSelectionXPathData *xpath_data = g_ptr_array_index(self->xpath_expressions, i);
			XmiMsimGuiXmsiSelectionXPathFlags flags = xmi_msim_gui_xmsi_selection_xpath_data_get_flags(xpath_data);
			if (!(flags & XMI_MSIM_GUI_XMSI_SELECTION_XPATH_WEIGHT_FRACTION))
				continue;

			gchar *xpath = xmi_msim_gui_xmsi_selection_xpath_data_get_string(xpath_data);
			xmlXPathObjectPtr result = xmlXPathEvalExpression(BAD_CAST xpath, context);
			g_free(xpath);
			g_assert(result != NULL);
			g_assert(!xmlXPathNodeSetIsEmpty(result->nodesetval));
			
			xmlNodeSetPtr nodeset = result->nodesetval;
			g_assert(nodeset->nodeNr == 1);

			g_object_set_qdata(G_OBJECT(xpath_data), DATA_NODE_GRANDFATHER, nodeset->nodeTab[0]->parent->parent);
			xmlXPathFreeObject(result);
		}

		// grandfathers have been determined, so now let's look at if and where they are shared
		for (i = 0 ; i < self->xpath_expressions->len - 1 ; i++) {
			XmiMsimGuiXmsiSelectionXPathData *xpath_data1 = g_ptr_array_index(self->xpath_expressions, i);
			struct common_weight_fraction_data *old_data = g_object_get_qdata(G_OBJECT(xpath_data1), DATA_COMMON_WEIGHT_FRACTION);
			if (old_data != NULL)
				continue;

			xmlNodePtr grandfather1 = g_object_get_qdata(G_OBJECT(xpath_data1), DATA_NODE_GRANDFATHER);
			if (grandfather1 == NULL)
				continue;

			unsigned int count = 1;
			unsigned int j = 0;

			// count how often grandfather1 occurs in the list
			for (j = i + 1 ; j < self->xpath_expressions->len ; j++) {
				XmiMsimGuiXmsiSelectionXPathData *xpath_data2 = g_ptr_array_index(self->xpath_expressions, j);
				xmlNodePtr grandfather2 = g_object_get_qdata(G_OBJECT(xpath_data2), DATA_NODE_GRANDFATHER);
				if (grandfather1 == grandfather2)
					count++;
			}

			if (count > 1) {
				g_assert(count + 2 < xmlChildElementCount(grandfather1));

				struct common_weight_fraction_data *data = g_malloc0(sizeof(struct common_weight_fraction_data));
				data->all_xpath_data = g_ptr_array_sized_new(count);
				g_ptr_array_add(data->all_xpath_data, xpath_data1);
				g_object_set_qdata_full(G_OBJECT(xpath_data1), DATA_COMMON_WEIGHT_FRACTION, data, common_weight_fraction_data_free);

				gchar *value1 = xmi_msim_gui_xmsi_selection_xpath_data_get_value(xpath_data1);
				data->weights_sum = g_ascii_strtod(value1, NULL);
				data->count = count;
				// TODO: check if weights are percentages or normalized!!!!

				for (j = i + 1 ; j < self->xpath_expressions->len ; j++) {
					XmiMsimGuiXmsiSelectionXPathData *xpath_data2 = g_ptr_array_index(self->xpath_expressions, j);
					xmlNodePtr grandfather2 = g_object_get_qdata(G_OBJECT(xpath_data2), DATA_NODE_GRANDFATHER);
					if (grandfather1 == grandfather2) {
						g_object_set_qdata(G_OBJECT(xpath_data2), DATA_COMMON_WEIGHT_FRACTION, data);
						gchar *value2 = xmi_msim_gui_xmsi_selection_xpath_data_get_value(xpath_data2);
						data->weights_sum += g_ascii_strtod(value2, NULL);
						g_ptr_array_add(data->all_xpath_data, xpath_data2);
					}
				}

				data->inc = 1.0;
				while (data->weights_sum + data->count * data->inc >= 100.0) {
					data->inc /= 10.0;
				}

			}
		}


		xmlXPathFreeContext(context);
	}

	// now add the widgets
	unsigned int i = 0;
	for (i = 0 ; i < self->xpath_expressions->len ; i++) {
		XmiMsimGuiXmsiSelectionXPathData *xpath_data = g_ptr_array_index(self->xpath_expressions, i);
		gchar *xpath_string = xmi_msim_gui_xmsi_selection_xpath_data_get_string(xpath_data);
		GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
		GtkWidget *label = gtk_label_new(NULL);
		gchar *buffer = g_strdup_printf("<b>XPath parameter %d: %s</b>", i + 1, xpath_string);
		gtk_label_set_markup(GTK_LABEL(label), buffer);
		g_free(buffer);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_widget_show_all(hbox);
		gtk_box_pack_start(GTK_BOX(self), hbox, FALSE, FALSE, 10);

		hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);

		label = gtk_label_new("Start");
		GtkWidget *startEntry = gtk_entry_new();
		gtk_widget_set_name(startEntry, "color_entry");
		gtk_editable_set_editable(GTK_EDITABLE(startEntry), TRUE);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(hbox), startEntry, TRUE, TRUE, 1);
		g_object_set_qdata(G_OBJECT(xpath_data), DATA_START_ENTRY, startEntry);

		label = gtk_label_new("End");
		GtkWidget *endEntry = gtk_entry_new();
		gtk_widget_set_name(endEntry, "color_entry");
		gtk_editable_set_editable(GTK_EDITABLE(endEntry), TRUE);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(hbox), endEntry, TRUE, TRUE, 1);
		g_object_set_qdata(G_OBJECT(xpath_data), DATA_END_ENTRY, endEntry);

		label = gtk_label_new("#Steps");
		GtkWidget *nstepsEntry = gtk_entry_new();
		gtk_widget_set_name(nstepsEntry, "color_entry");
		gtk_editable_set_editable(GTK_EDITABLE(nstepsEntry), TRUE);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(hbox), nstepsEntry, TRUE, TRUE, 1);
		g_object_set_qdata(G_OBJECT(xpath_data), DATA_NSTEPS_ENTRY, nstepsEntry);

		gtk_widget_show_all(hbox);
		gtk_box_pack_start(GTK_BOX(self), hbox, FALSE, FALSE, 2);

		gchar *xpath_value = xmi_msim_gui_xmsi_selection_xpath_data_get_value(xpath_data);

		// check if we are dealing with a weight fraction parameter that is linked to another one
		struct common_weight_fraction_data *weight_data = g_object_get_qdata(G_OBJECT(xpath_data), DATA_COMMON_WEIGHT_FRACTION);
		double start = g_ascii_strtod(xpath_value, NULL);
		gchar *start_text = xpath_value;
		gchar *end_text = NULL;

		if (weight_data) {
			double end = start + weight_data->inc;
			
			end_text = g_strdup_printf("%g", end);
		}
		else {
			XmiMsimGuiXmsiSelectionXPathFlags flags = xmi_msim_gui_xmsi_selection_xpath_data_get_flags(xpath_data);
			double end;
			if (flags & XMI_MSIM_GUI_XMSI_SELECTION_XPATH_WEIGHT_FRACTION) {
				// we are dealing with a lonely weight fraction...
				double inc = 1.0;
				while (start + inc >= 100.0) {
					inc /= 10.0;
				}
				end = start + inc;
			}
			else {
				end = start + 1.0;

			}
			end_text = g_strdup_printf("%g", end);
		}

		gtk_entry_set_text(GTK_ENTRY(startEntry), start_text);
		gtk_entry_set_text(GTK_ENTRY(endEntry), end_text);
		gtk_entry_set_text(GTK_ENTRY(nstepsEntry), "10");

		g_object_set_qdata(G_OBJECT(startEntry), ENTRY_XPATH_DATA, xpath_data);
		g_object_set_qdata(G_OBJECT(endEntry), ENTRY_XPATH_DATA, xpath_data);
		g_object_set_qdata(G_OBJECT(nstepsEntry), ENTRY_XPATH_DATA, xpath_data);

		g_object_set_qdata(G_OBJECT(startEntry), ENTRY_TYPE, GINT_TO_POINTER(ENTRY_TYPE_START));
		g_object_set_qdata(G_OBJECT(endEntry), ENTRY_TYPE, GINT_TO_POINTER(ENTRY_TYPE_END));
		g_object_set_qdata(G_OBJECT(nstepsEntry), ENTRY_TYPE, GINT_TO_POINTER(ENTRY_TYPE_NSTEPS));

		g_object_set_qdata(G_OBJECT(startEntry), ENTRY_STATUS, GINT_TO_POINTER(1));
		g_object_set_qdata(G_OBJECT(endEntry), ENTRY_STATUS, GINT_TO_POINTER(1));
		g_object_set_qdata(G_OBJECT(nstepsEntry), ENTRY_STATUS, GINT_TO_POINTER(1));
	
		g_signal_connect_swapped(G_OBJECT(startEntry), "changed", G_CALLBACK(xpath_entry_changed_cb), self);
		g_signal_connect_swapped(G_OBJECT(endEntry), "changed", G_CALLBACK(xpath_entry_changed_cb), self);
		g_signal_connect_swapped(G_OBJECT(nstepsEntry), "changed", G_CALLBACK(xpath_entry_changed_cb), self);

		g_free(end_text);
		g_free(xpath_value);
		g_free(xpath_string);
	}

	// now the archive file name box	
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	GtkWidget *label = gtk_label_new("XMSA file");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	GtkWidget *archiveEntry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(archiveEntry), FALSE);
	gchar *xmsafile = g_strdup(self->input_file);
	xmsafile[strlen(xmsafile)-1] = 'a';
	gtk_entry_set_text(GTK_ENTRY(archiveEntry), xmsafile);
	self->archive_file = xmsafile;
	gtk_box_pack_start(GTK_BOX(hbox), archiveEntry, TRUE, TRUE, 2);
	GtkWidget *archivesaveButton = gtk_button_new_with_mnemonic("_Save As");
	self->archive_entry = archiveEntry;
	g_signal_connect(G_OBJECT(archivesaveButton), "clicked", G_CALLBACK(archivesaveButton_clicked_cb), self);
	gtk_box_pack_start(GTK_BOX(hbox), archivesaveButton, FALSE, FALSE, 2);
	gtk_widget_show_all(hbox);
	gtk_box_pack_end(GTK_BOX(self), hbox, FALSE, FALSE, 1);
	
	xmlFreeDoc(doc);

	update_single_batch_data(self);

	G_OBJECT_CLASS(xmi_msim_gui_batch_archive_settings_box_parent_class)->constructed(gobject);
}

GtkWidget* xmi_msim_gui_batch_archive_settings_box_new(GPtrArray *xpath_expressions, const gchar *input_file) {
	g_return_val_if_fail(xpath_expressions != NULL && xpath_expressions->len > 0, NULL);
	g_return_val_if_fail(input_file != NULL, NULL);

	return g_object_new(XMI_MSIM_GUI_TYPE_BATCH_ARCHIVE_SETTINGS_BOX, "xpath-expressions", xpath_expressions, "input-file", input_file, NULL);
}

GPtrArray* xmi_msim_gui_batch_archive_settings_box_get_data(XmiMsimGuiBatchArchiveSettingsBox* self) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_BATCH_ARCHIVE_SETTINGS_BOX(self), NULL);

	GPtrArray *rv = NULL;

	g_object_get(self, "single-batch-data", &rv, NULL);

	return rv;
}

gchar* xmi_msim_gui_batch_archive_settings_box_get_archive_name(XmiMsimGuiBatchArchiveSettingsBox *self) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_BATCH_ARCHIVE_SETTINGS_BOX(self), NULL);

	gchar *rv = NULL;

	g_object_get(self, "archive-file", &rv, NULL);

	return rv;
}
