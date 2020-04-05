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

#include "xmimsim-gui-batch-assistant.h"
#include "xmimsim-gui-options-box.h"
#include "xmimsim-gui-xmsi-selection-scrolled-window.h"
#include "xmimsim-gui-batch-archive-settings-box.h"
#include "xmimsim-gui-batch-controls-box.h"
#include "xmimsim-gui-private.h"
#include "xmimsim-gui-long-task-window.h"
#include "xmimsim-gui-xmsa-viewer-window.h"
#include "xmimsim-gui-batch-multi-selection-type-grid.h"

#include "xmi_xml.h"
#include "xmi_aux.h"

#include <unistd.h>
#include <glib/gstdio.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include <string.h>

static G_DEFINE_QUARK("options-box", options_box)
#define OPTIONS_BOX_QUARK options_box_quark()

struct _XmiMsimGuiBatchAssistant {
	GtkAssistant parent_instance;
	GPtrArray *xmsi_files;
	GPtrArray *options_pages;
	GtkWidget *multi_options_question_page;
	GtkWidget *xmsi_selection_page;
	GtkWidget *archive_settings_page;
	GtkWidget *batch_controls_page;
	XmiMsimBatchAbstract *batch_data;
};

struct _XmiMsimGuiBatchAssistantClass {
	GtkAssistantClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiBatchAssistant, xmi_msim_gui_batch_assistant, GTK_TYPE_ASSISTANT)

static void xmi_msim_gui_batch_assistant_dispose(GObject *gobject) {
	XmiMsimGuiBatchAssistant *self = XMI_MSIM_GUI_BATCH_ASSISTANT(gobject);

	if (self->batch_data)
		g_clear_object(&self->batch_data);

	G_OBJECT_CLASS(xmi_msim_gui_batch_assistant_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_batch_assistant_finalize(GObject *gobject) {
	XmiMsimGuiBatchAssistant *self = XMI_MSIM_GUI_BATCH_ASSISTANT(gobject);

	if (self->xmsi_files)
		g_ptr_array_unref(self->xmsi_files);
	if (self->options_pages)
		g_ptr_array_unref(self->options_pages);

	G_OBJECT_CLASS(xmi_msim_gui_batch_assistant_parent_class)->finalize(gobject);
}

struct batch_killer_data {
	GtkWidget *dialog;
	XmiMsimBatchAbstract *batch_data;
};

static gboolean batch_killer(struct batch_killer_data *data) {

	if (data->batch_data && xmi_msim_batch_abstract_is_running(data->batch_data)) {
		return G_SOURCE_CONTINUE;
	}
	gtk_dialog_response(GTK_DIALOG(data->dialog), GTK_RESPONSE_CLOSE);

	return G_SOURCE_REMOVE;
}

static gboolean timeout_killer(GtkWidget *self) {
	gtk_widget_destroy(self);

	return G_SOURCE_REMOVE;
}

static void xmi_msim_gui_batch_assistant_shutdown(XmiMsimGuiBatchAssistant *self) {
	if (!self->batch_data || !xmi_msim_batch_abstract_is_running(self->batch_data)) {
		gtk_widget_destroy(GTK_WIDGET(self));
		return;
	}

	XmiMsimBatchAbstract *batch_data = g_object_ref(self->batch_data);
	g_debug("batch is running!");
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Closing Window...",
		GTK_WINDOW(self),
		GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
		"_Kill batch and close", GTK_RESPONSE_OK,
		"_Cancel", GTK_RESPONSE_CANCEL,
		NULL);
	GtkWidget *content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *label = gtk_label_new("The window you are trying to close still has a batch running!");
	gtk_widget_show(label);
	gtk_box_pack_start(GTK_BOX(content),label, FALSE, FALSE, 3);
	struct batch_killer_data data = {.dialog = dialog, .batch_data = batch_data};
	guint source_id = g_timeout_add_seconds(1, (GSourceFunc) batch_killer, &data);
	switch (gtk_dialog_run(GTK_DIALOG(dialog))) {
		case GTK_RESPONSE_CANCEL:
		case GTK_RESPONSE_DELETE_EVENT:
			g_source_remove(source_id);
			gtk_widget_destroy(dialog);
			break;
		case GTK_RESPONSE_OK:
			g_source_remove(source_id);
		case GTK_RESPONSE_CLOSE: // from batch_killer!
		{
			gtk_widget_destroy(dialog);
			if (xmi_msim_batch_abstract_is_running(batch_data)) {
				xmi_msim_batch_abstract_kill(batch_data, NULL);
			}
			g_timeout_add_seconds(1, (GSourceFunc) timeout_killer, self);
		}
	}
	g_object_unref(batch_data);
}

static void xmi_msim_gui_batch_assistant_prepare(GtkAssistant *assistant, GtkWidget *page) {
	g_debug("Entering xmi_msim_gui_batch_assistant_prepare");
}

static void xmi_msim_gui_batch_assistant_apply(GtkAssistant *assistant) {
	g_debug("Entering xmi_msim_gui_batch_assistant_apply");
	xmi_msim_gui_batch_assistant_shutdown(XMI_MSIM_GUI_BATCH_ASSISTANT(assistant));
}

static void xmi_msim_gui_batch_assistant_close(GtkAssistant *assistant) {
	g_debug("Entering xmi_msim_gui_batch_assistant_close");
	xmi_msim_gui_batch_assistant_shutdown(XMI_MSIM_GUI_BATCH_ASSISTANT(assistant));
}

static void xmi_msim_gui_batch_assistant_cancel(GtkAssistant *assistant) {
	g_debug("Entering xmi_msim_gui_batch_assistant_cancel");
	xmi_msim_gui_batch_assistant_shutdown(XMI_MSIM_GUI_BATCH_ASSISTANT(assistant));
}

static void xmi_msim_gui_batch_assistant_class_init(XmiMsimGuiBatchAssistantClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);
	GtkAssistantClass *assistant_class = GTK_ASSISTANT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_batch_assistant_dispose;
	object_class->finalize = xmi_msim_gui_batch_assistant_finalize;

	assistant_class->prepare = xmi_msim_gui_batch_assistant_prepare;
	assistant_class->apply = xmi_msim_gui_batch_assistant_apply;
	assistant_class->close = xmi_msim_gui_batch_assistant_close;
	assistant_class->cancel = xmi_msim_gui_batch_assistant_cancel;
}

static gint xmi_msim_gui_batch_assistant_get_page(XmiMsimGuiBatchAssistant *self, GtkWidget *page) {
	gint i = 0;
	for (i = 0 ; i < gtk_assistant_get_n_pages(GTK_ASSISTANT(self)) ; i++) {
		if (gtk_assistant_get_nth_page(GTK_ASSISTANT(self), i) == page)
			return i;
	}

	return -1;
}

static void filter_filename(gpointer data, gpointer user_data) {
	gchar *filename = data;
	XmiMsimGuiBatchAssistant *self = user_data;

	if (g_file_test(filename, G_FILE_TEST_IS_REGULAR) && g_access(filename, R_OK) == 0) {
		g_ptr_array_add(self->xmsi_files, g_strdup(filename));
		g_debug("Adding filename: %s", filename);
	}
}

static void update_weight_fractions(xmlNodeSetPtr selected_nodes, const gchar *xpath_base, xmlXPathContextPtr context, double weights_sum_orig) {

	// this set will contain all nodes
	xmlNodeSetPtr all_nodes = NULL;

	// construct XPath expression for all nodes
	gchar *xpath_close_bracket = strrchr(xpath_base, ']');
	gchar *xpath_open_bracket = strrchr(xpath_base, '[');

	GString *xpath_all_nodes = g_string_new_len(xpath_base, xpath_open_bracket - xpath_base + 1);
	g_string_append_c(xpath_all_nodes, '*');
	g_string_append(xpath_all_nodes, xpath_close_bracket);

	xmlXPathObjectPtr result = xmlXPathEvalExpression(BAD_CAST xpath_all_nodes->str, context);
	g_string_free(xpath_all_nodes, TRUE);

	g_assert(result != NULL);
	g_assert(!xmlXPathNodeSetIsEmpty(result->nodesetval));

	all_nodes = result->nodesetval;
	xmlXPathFreeNodeSetList(result);
	g_assert(all_nodes->nodeNr > selected_nodes->nodeNr);

	// get the difference of both setlists
	xmlNodeSetPtr unselected_nodes = xmlXPathDifference(all_nodes, selected_nodes);
	g_assert(unselected_nodes->nodeNr >= 1);

	xmlXPathFreeNodeSet(all_nodes);

	// get the sum of the selected_nodes values 
	double selected_sum = 0.0;

	unsigned int k;
	for (k = 0 ; k < selected_nodes->nodeNr ; k++) {
		xmlNodePtr node = selected_nodes->nodeTab[k];
		xmlChar *txt = xmlNodeGetContent(node);
		g_assert(txt != NULL);
		double value = g_ascii_strtod((gchar *) txt, NULL);
		g_assert(value >= 0.0);
		selected_sum += value;
		xmlFree(txt);
	}

	g_debug("selected_sum: %g", selected_sum);
	g_debug("weights_sum_orig: %g", weights_sum_orig);
	g_debug("unselected_nodes->nodeNr: %d", unselected_nodes->nodeNr);

	// now last thing is to update the unselected nodes
	for (k = 0 ; k < unselected_nodes->nodeNr ; k++) {
		xmlNodePtr node = unselected_nodes->nodeTab[k];
		xmlChar *txt = xmlNodeGetContent(node);
		g_assert(txt != NULL);
		double old_value = g_ascii_strtod((gchar *) txt, NULL);
		g_assert(old_value > 0.0);
		xmlFree(txt);
		double new_value = old_value * (100.0 - selected_sum) / (100.0 - weights_sum_orig);
		txt = BAD_CAST g_strdup_printf("%g", new_value);
		g_debug("old_value: %g", old_value);
		g_debug("new_value: %g", new_value);
		xmlNodeSetContent(node, txt);
		g_free(txt);
	}

	xmlXPathFreeNodeSet(unselected_nodes);
}

static GPtrArray* get_xmsi_data(XmiMsimGuiBatchAssistant *self, GPtrArray *single_data_arr) {

	GArray *dims = g_array_sized_new(FALSE, FALSE, sizeof(int), single_data_arr->len);
	unsigned int i;
	int dims_prod = 1;

	for (i = 0 ; i < single_data_arr->len ; i++) {
		xmi_batch_single_data *data = g_ptr_array_index(single_data_arr, i);
		int dim = data->nsteps + 1;
		g_array_append_val(dims, dim);
		dims_prod *= dim;
	}

	GPtrArray *xmsi_data = g_ptr_array_new_full(dims_prod, (GDestroyNotify) xmi_input_free);
	g_ptr_array_set_size(xmsi_data, dims_prod);

	gchar *filename = g_ptr_array_index(self->xmsi_files, 0);

	xmlDocPtr orig_doc;
	if ((orig_doc = xmlReadFile(filename, NULL, XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		g_error("xmlReadFile error for %s", filename);
	}


	for (i = 0 ; i < dims_prod ; i++) {
		xmlDocPtr doc = xmlCopyDoc(orig_doc, 1);
		xmlNodePtr root = xmlDocGetRootElement(doc);
		g_assert(root != NULL);

		xmlXPathContextPtr xpathCtx;
		xpathCtx = xmlXPathNewContext(doc);

		GArray *indices = xmi_row_major_array_get_indices(dims, i);

		g_debug("offset: %d", i);

		unsigned int j;

		for (j = 0 ; j < single_data_arr->len ; j++) {
			xmi_batch_single_data *data = g_ptr_array_index(single_data_arr, j);
			int _index = g_array_index(indices, int, j);
			
			double value = data->start + _index * (data->end - data->start) / data->nsteps;

			xmlXPathObjectPtr xpathObj = xmlXPathNodeEval(root, BAD_CAST data->xpath, xpathCtx);
			g_assert(xpathObj != NULL && xpathObj->nodesetval->nodeNr == 1);

			xmlNodePtr node = xpathObj->nodesetval->nodeTab[0];
	
			xmlXPathFreeObject(xpathObj);

			gchar *new_value = g_strdup_printf("%g", value);

			xmlNodeSetContent(node, BAD_CAST new_value);

			g_debug("index: %d XPath: %s new_value: %s", _index, data->xpath, new_value);

			g_free(new_value);
		}

		g_array_unref(indices);

		GPtrArray *common_data_array = g_ptr_array_new();

		// when dealing with common_weight_fraction_data, make adjustments now
		GPtrArray *xpath_arr = xmi_msim_gui_xmsi_selection_scrolled_window_get_xpath_expressions(XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW(self->xmsi_selection_page));

		for (j = 0 ; j < xpath_arr->len ; j++) {
			XmiMsimGuiXmsiSelectionXPathData *xpath_data = g_ptr_array_index(xpath_arr, j);
			struct common_weight_fraction_data *common_data = g_object_get_qdata(G_OBJECT(xpath_data), DATA_COMMON_WEIGHT_FRACTION);

			// no common data
			if (common_data == NULL) {
				// check if xpath contains weight_fraction
				gchar *xpath = xmi_msim_gui_xmsi_selection_xpath_data_get_string(xpath_data);
				// this is a bit of an ugly manner to detect that we are dealing with a layer... may want to improve this some day...
				if (strstr(xpath, "weight_fraction") != NULL) {
					// this needs to use the old value, so use orig_doc
					xmlNodePtr orig_root = xmlDocGetRootElement(orig_doc);
					xmlXPathContextPtr context = xmlXPathNewContext(orig_doc);
					
					xmlXPathObjectPtr xpathObj = xmlXPathNodeEval(orig_root, BAD_CAST xpath, context);
					g_assert(xpathObj != NULL && xpathObj->nodesetval->nodeNr == 1);

					xmlNodePtr node = xpathObj->nodesetval->nodeTab[0];
					xmlChar *txt = xmlNodeGetContent(node);
					g_assert(txt != NULL);
					double value = g_ascii_strtod((gchar *) txt, NULL);
					g_assert(value > 0.0);
					xmlFree(txt);
					xmlXPathFreeContext(context);
					xmlXPathFreeObject(xpathObj);

					// use the doc copy for the update
					context = xmlXPathNewContext(doc);
					xpathObj = xmlXPathNodeEval(root, BAD_CAST xpath, context);
					g_assert(xpathObj != NULL && xpathObj->nodesetval->nodeNr == 1);
					update_weight_fractions(xpathObj->nodesetval, xpath, context, value);
					xmlXPathFreeContext(context);
					xmlXPathFreeObject(xpathObj);
				}

				g_free(xpath);
				continue;
			}

			// common data has already been processed
			if (g_ptr_array_find(common_data_array, common_data, NULL))
				continue;

			xmlXPathContextPtr context = xmlXPathNewContext(doc);

			// this set will contain all nodes that have been used as XPath expressions
			xmlNodeSetPtr selected_nodes = NULL;

			unsigned int k;

			for (k = 0 ; k < common_data->all_xpath_data->len ; k++) {
				XmiMsimGuiXmsiSelectionXPathData *xpath_data2 = g_ptr_array_index(common_data->all_xpath_data, k);

				gchar *xpath = xmi_msim_gui_xmsi_selection_xpath_data_get_string(xpath_data2);
				xmlXPathObjectPtr result = xmlXPathEvalExpression(BAD_CAST xpath, context);
				g_assert(result != NULL);
				g_assert(!xmlXPathNodeSetIsEmpty(result->nodesetval));
			
				xmlNodeSetPtr nodeset = result->nodesetval;
				g_assert(nodeset->nodeNr == 1);

				selected_nodes = xmlXPathNodeSetMerge(selected_nodes, nodeset);
				xmlXPathFreeObject(result);
				g_free(xpath);
			}

			gchar *xpath = xmi_msim_gui_xmsi_selection_xpath_data_get_string(xpath_data);

			update_weight_fractions(selected_nodes, xpath, context, common_data->weights_sum);

			g_free(xpath);
			xmlXPathFreeNodeSet(selected_nodes);
			xmlXPathFreeContext(context);

			g_ptr_array_add(common_data_array, common_data);
		}

		g_ptr_array_unref(xpath_arr);
		g_ptr_array_unref(common_data_array);

		xmi_input *input = g_malloc0(sizeof(xmi_input));
		gboolean rv = xmi_read_input_xml_body(doc, root, input, NULL);
		g_assert(rv == TRUE);

		g_ptr_array_index(xmsi_data, i) = input;

		xmlXPathFreeContext(xpathCtx);
		xmlFreeDoc(doc);
	}
	
	xmlFreeDoc(orig_doc);
	g_array_unref(dims);

	return xmsi_data;
}

static void batch_data_running_changed_cb(XmiMsimBatchAbstract *batch_data, GParamSpec *pspec, XmiMsimGuiBatchAssistant *self) {
	gboolean running = xmi_msim_batch_abstract_is_running(batch_data);

	if (running) {
		gtk_assistant_commit(GTK_ASSISTANT(self));
	}
}

static void save_archive_callback(GtkWidget *task_window, GAsyncResult *result, gpointer data) {
	GtkWindow *window = gtk_window_get_transient_for(GTK_WINDOW(task_window));
	gdk_window_set_cursor(gtk_widget_get_window(task_window), NULL);
	gtk_widget_destroy(task_window);

	XmiMsimGuiBatchAssistant *self = g_task_get_task_data(G_TASK(result));

	GError *error = NULL;

	gboolean rv = g_task_propagate_boolean(G_TASK(result), &error);

	if (!rv) {
		GtkWidget *dialog = gtk_message_dialog_new(window,
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An error occured while performing the conversion"
                	);
		gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
		g_error_free(error);
     		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		return;
	}

	gchar *archive_name = xmi_msim_gui_batch_archive_settings_box_get_archive_name(XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX(self->archive_settings_page));
	xmi_archive *archive = NULL;
	g_object_get(self->batch_data, "archive", &archive, NULL);
	GtkWidget *xmsa_window = xmi_msim_gui_xmsa_viewer_window_new(archive, XMI_MSIM_GUI_APPLICATION(g_application_get_default()), archive_name);
	xmi_archive_unref(archive);
	g_free(archive_name);
	gtk_window_set_transient_for(GTK_WINDOW(xmsa_window), GTK_WINDOW(window));
	gtk_window_present(GTK_WINDOW(xmsa_window));
}

static void save_archive_thread(GTask *task, gpointer source_object, gpointer task_data, GCancellable *cancellable) {
	XmiMsimGuiLongTaskWindow *task_window = source_object;
	XmiMsimGuiBatchAssistant *self = task_data;
	
	GError *error = NULL;
	
	gchar *archive_name = xmi_msim_gui_batch_archive_settings_box_get_archive_name(XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX(self->archive_settings_page));

	if (xmi_msim_batch_single_write_archive(XMI_MSIM_BATCH_SINGLE(self->batch_data),  archive_name, &error)) {
		g_task_return_boolean(task, TRUE);
	}
	else {
		g_task_return_error(task, error);
	}
	g_free(archive_name);
}

static void batch_single_data_finished_event_cb(XmiMsimBatchAbstract *batch_data, gboolean result, gchar *message, XmiMsimGuiBatchAssistant *self) {

	gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(self->batch_controls_page), TRUE);

	if (result) {
		GtkWidget *task_window = xmi_msim_gui_long_task_window_new(GTK_WINDOW(self));
		xmi_msim_gui_long_task_window_set_text(XMI_MSIM_GUI_LONG_TASK_WINDOW(task_window), "<b>Saving archive data</b>");
		gtk_widget_show(task_window);

		GdkCursor* watchCursor = gdk_cursor_new_for_display(gdk_display_get_default(), GDK_WATCH);
		gdk_window_set_cursor(gtk_widget_get_window(task_window), watchCursor);

		GTask *task = g_task_new(task_window, NULL, (GAsyncReadyCallback) save_archive_callback, self);
	
		g_task_set_task_data(task, g_object_ref(self), g_object_unref);
		g_task_run_in_thread(task, save_archive_thread);
		g_object_unref(task);
	}
}

static void single_batch_data_changed_cb(XmiMsimGuiBatchAssistant *self, GParamSpec *pspec, XmiMsimGuiBatchArchiveSettingsBox *archive_setting_page) {

	xmi_msim_gui_batch_controls_box_set_batch_data(XMI_MSIM_GUI_BATCH_CONTROLS_BOX(self->batch_controls_page), NULL);

	GPtrArray *arr = xmi_msim_gui_batch_archive_settings_box_get_data(XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX(archive_setting_page));

	if (arr) {
		g_debug("single_batch_data_changed_cb: valid data found");
		unsigned int i;
		for (i = 0 ; i < arr->len ; i++) {
			xmi_batch_single_data *data = g_ptr_array_index(arr, i);
			g_debug("single_batch_data_changed_cb: %d -> %s", i, data->xpath);
			g_debug("single_batch_data_changed_cb: %g -> %g in %d steps", data->start, data->end, data->nsteps);
		}

		// generate array with xmi_input structs -> this is not trivial and will require the XPath evaluation stuff...
		GPtrArray *xmsi_data = get_xmsi_data(self, arr);

		xmi_main_options *options = xmi_msim_gui_options_box_get_options(g_ptr_array_index(self->options_pages, 0));

		if (self->batch_data)
			g_object_unref(self->batch_data);

		self->batch_data = xmi_msim_batch_single_new(xmsi_data, arr, options);

		g_signal_connect(self->batch_data, "notify::running", G_CALLBACK(batch_data_running_changed_cb), self);
		g_signal_connect(self->batch_data, "finished-event", G_CALLBACK(batch_single_data_finished_event_cb), self);

		xmi_main_options_free(options);

		xmi_msim_gui_batch_controls_box_set_batch_data(XMI_MSIM_GUI_BATCH_CONTROLS_BOX(self->batch_controls_page), self->batch_data);

		g_ptr_array_unref(arr);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(archive_setting_page), TRUE);
	}
	else {
		g_debug("single_batch_data_changed_cb: NO valid data found");
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(archive_setting_page), FALSE);
	}


}

static void xpath_changed_cb(XmiMsimGuiBatchAssistant *self, GParamSpec *pspec, XmiMsimGuiXmsiSelectionScrolledWindow *selection_page) {

	g_debug("Entering xpath_changed_cb");

	if (self->archive_settings_page) {
		gint page_index = xmi_msim_gui_batch_assistant_get_page(self, self->archive_settings_page);
		g_assert(page_index >= 0);
		gtk_assistant_remove_page(GTK_ASSISTANT(self), page_index);
		self->archive_settings_page = NULL;
	}

	GPtrArray *arr = xmi_msim_gui_xmsi_selection_scrolled_window_get_xpath_expressions(selection_page);

	if (arr) {
		gchar *filename = g_ptr_array_index(self->xmsi_files, 0);
		GtkWidget *page = xmi_msim_gui_batch_archive_settings_box_new(arr, filename);
		g_signal_connect_swapped(page, "notify::single-batch-data", G_CALLBACK(single_batch_data_changed_cb), self);
		gtk_assistant_insert_page(GTK_ASSISTANT(self), page, 4);
		gtk_assistant_set_page_type(GTK_ASSISTANT(self), page, GTK_ASSISTANT_PAGE_CONTENT);
		gtk_assistant_set_page_title(GTK_ASSISTANT(self), page, "Archive Settings");
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), page, TRUE);
		self->archive_settings_page = page;
		gtk_widget_show(page);

		g_ptr_array_unref(arr);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(selection_page), TRUE);
		single_batch_data_changed_cb(self, NULL, XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX(page));
	}
	else {
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(selection_page), FALSE);
	}
}

static void batch_multi_data_finished_event_cb(XmiMsimBatchAbstract *batch_data, gboolean result, gchar *message, XmiMsimGuiBatchAssistant *self) {

	gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(self->batch_controls_page), TRUE);
}

static void options_changed_cb(XmiMsimGuiBatchAssistant *self, XmiMsimGuiOptionsBox *_options_box) {
	g_debug("Entering options_changed_cb");

	// whenever called, update the batch instance
	if (self->batch_data)
		g_object_unref(self->batch_data);

	// gather the options
	GPtrArray *options = g_ptr_array_new_full(self->options_pages->len, (GDestroyNotify) xmi_main_options_free);

	if (self->options_pages->len == 1) {
		g_ptr_array_add(options, xmi_msim_gui_options_box_get_options(g_ptr_array_index(self->options_pages, 0)));
	}
	else {
		unsigned int i;
		for (i = 0 ; i < self->options_pages->len ; i++) {
			GtkWidget *box = g_ptr_array_index(self->options_pages, i);
			XmiMsimGuiOptionsBox *options_box = g_object_get_qdata(G_OBJECT(box), OPTIONS_BOX_QUARK);
			g_ptr_array_add(options, xmi_msim_gui_options_box_get_options(options_box));
		}
	}

	self->batch_data = xmi_msim_batch_multi_new(self->xmsi_files, options);
	g_signal_connect(self->batch_data, "notify::running", G_CALLBACK(batch_data_running_changed_cb), self);
	g_signal_connect(self->batch_data, "finished-event", G_CALLBACK(batch_multi_data_finished_event_cb), self);

	xmi_msim_gui_batch_controls_box_set_batch_data(XMI_MSIM_GUI_BATCH_CONTROLS_BOX(self->batch_controls_page), self->batch_data);

	g_ptr_array_unref(options);
}

static void selection_type_changed_cb(XmiMsimGuiBatchAssistant *self, GParamSpec *pspec, XmiMsimGuiBatchMultiSelectionTypeGrid *multi_options_question_page) {
	g_debug("Entering selection_type_changed_cb");

	gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(multi_options_question_page), TRUE);

	XmiMsimGuiBatchMultiSelectionType type;
	g_object_get(multi_options_question_page, "selection-type", &type, NULL);

	if (self->options_pages) {
		unsigned int i;
		for (i = 0 ; i < self->options_pages->len ; i++) {
			gint page_index = xmi_msim_gui_batch_assistant_get_page(self, g_ptr_array_index(self->options_pages, i));
			g_assert(page_index >= 0);
			gtk_assistant_remove_page(GTK_ASSISTANT(self), page_index);
		}
		g_ptr_array_free(self->options_pages, TRUE);
		self->options_pages = NULL;
	}

	self->options_pages = g_ptr_array_new();

	if (type == XMI_MSIM_GUI_BATCH_MULTI_SELECTION_TYPE_SINGLE_OPTION) {
		GtkWidget *page = xmi_msim_gui_options_box_new();
		gtk_assistant_insert_page(GTK_ASSISTANT(self), page, 3);
		gtk_assistant_set_page_type(GTK_ASSISTANT(self), page, GTK_ASSISTANT_PAGE_CONTENT);
		gtk_assistant_set_page_title(GTK_ASSISTANT(self), page, "Set the Options");
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), page, TRUE);

		g_ptr_array_add(self->options_pages, page);

		g_signal_connect_swapped(page, "changed", G_CALLBACK(options_changed_cb), self);

		gtk_widget_show_all(page);
	}
	else if (type == XMI_MSIM_GUI_BATCH_MULTI_SELECTION_TYPE_MULTI_OPTION) {
		unsigned int i;
		self->options_pages = g_ptr_array_new();

		for (i = 0 ; i < self->xmsi_files->len ; i++) {
			GtkWidget *page = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
			gchar *file_name = g_ptr_array_index(self->xmsi_files, i);
			gchar *basename = g_path_get_basename(file_name);
			gchar *label_text = g_strdup_printf("<span font_weight=\"bold\" font_size=\"x-large\">%s</span>", basename);
			g_free(basename);
			GtkWidget *label = gtk_label_new(label_text);
			g_free(label_text);
			gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
			g_object_set(label, "use-markup", TRUE, NULL);
			gtk_box_pack_start(GTK_BOX(page), label, FALSE, FALSE, 0);

			GtkWidget *options_box = xmi_msim_gui_options_box_new();
			gtk_box_pack_start(GTK_BOX(page), options_box, TRUE, TRUE, 0);
			g_object_set_qdata(G_OBJECT(page), OPTIONS_BOX_QUARK, options_box);

			gtk_assistant_insert_page(GTK_ASSISTANT(self), page, 3 + i);
			gtk_assistant_set_page_type(GTK_ASSISTANT(self), page, GTK_ASSISTANT_PAGE_CONTENT);
			gchar *title = g_strdup_printf("Set the Options #%d", i + 1);
			gtk_assistant_set_page_title(GTK_ASSISTANT(self), page, title);
			g_free(title);
			gtk_assistant_set_page_complete(GTK_ASSISTANT(self), page, TRUE);

			g_ptr_array_add(self->options_pages, page);

			g_signal_connect_swapped(options_box, "changed", G_CALLBACK(options_changed_cb), self);

			gtk_widget_show_all(page);
		}
	}

	options_changed_cb(self, NULL);
}

static void file_chooser_selection_changed_cb(XmiMsimGuiBatchAssistant *self, GtkFileChooser *file_chooser) {
	g_debug("Entering file_chooser_selection_changed_cb");

	GSList *filenames = gtk_file_chooser_get_filenames(file_chooser);

	if (self->xmsi_files) {
		g_ptr_array_unref(self->xmsi_files);
	}
	self->xmsi_files = g_ptr_array_new_with_free_func(g_free);

	g_slist_foreach(filenames, filter_filename, self);

	guint nfilenames = self->xmsi_files->len;

	// start by cleaning up all pages
	if (self->options_pages) {
		unsigned int i;
		for (i = 0 ; i < self->options_pages->len ; i++) {
			gint page_index = xmi_msim_gui_batch_assistant_get_page(self, g_ptr_array_index(self->options_pages, i));
			g_assert(page_index >= 0);
			gtk_assistant_remove_page(GTK_ASSISTANT(self), page_index);
		}
		g_ptr_array_free(self->options_pages, TRUE);
		self->options_pages = NULL;
	}

	if (self->multi_options_question_page) {
		gint page_index = xmi_msim_gui_batch_assistant_get_page(self, self->multi_options_question_page);
		g_assert(page_index >= 0);
		gtk_assistant_remove_page(GTK_ASSISTANT(self), page_index);
		self->multi_options_question_page = NULL;
	}

	if (self->xmsi_selection_page) {
		gint page_index = xmi_msim_gui_batch_assistant_get_page(self, self->xmsi_selection_page);
		g_assert(page_index >= 0);
		gtk_assistant_remove_page(GTK_ASSISTANT(self), page_index);
		self->xmsi_selection_page = NULL;
	}

	if (self->archive_settings_page) {
		gint page_index = xmi_msim_gui_batch_assistant_get_page(self, self->archive_settings_page);
		g_assert(page_index >= 0);
		gtk_assistant_remove_page(GTK_ASSISTANT(self), page_index);
		self->archive_settings_page = NULL;
	}

	if (nfilenames > 1) {
		g_debug("Entering multi mode!");

		// add question page
		GtkWidget *page = xmi_msim_gui_batch_multi_selection_type_grid_new();
		gtk_assistant_insert_page(GTK_ASSISTANT(self), page, 2);
		gtk_assistant_set_page_type(GTK_ASSISTANT(self), page, GTK_ASSISTANT_PAGE_CONTENT);
		gtk_assistant_set_page_title(GTK_ASSISTANT(self), page, "Select Options Type");
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), page, FALSE);
		gtk_widget_show_all(page);

		g_signal_connect_swapped(page, "notify::selection-type", G_CALLBACK(selection_type_changed_cb), self);
		self->multi_options_question_page = page;

		selection_type_changed_cb(self, NULL, XMI_MSIM_GUI_BATCH_MULTI_SELECTION_TYPE_GRID(page));

	} else if (nfilenames == 1) {
		g_debug("Entering single mode!");
		gchar *filename = g_ptr_array_index(self->xmsi_files, 0);

		// single mode!

		// add options page
		GtkWidget *page = xmi_msim_gui_options_box_new();
		gtk_assistant_insert_page(GTK_ASSISTANT(self), page, 2);
		gtk_assistant_set_page_type(GTK_ASSISTANT(self), page, GTK_ASSISTANT_PAGE_CONTENT);
		gtk_assistant_set_page_title(GTK_ASSISTANT(self), page, "Set the Options");
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), page, TRUE);

		self->options_pages = g_ptr_array_new();
		g_ptr_array_add(self->options_pages, page);
		gtk_widget_show_all(page);

		// xmsi selection scrolled window
		xmi_input *input = xmi_input_read_from_xml_file(filename, NULL);
		g_assert(input != NULL);
		page = xmi_msim_gui_xmsi_selection_scrolled_window_new(input, FALSE);
		xmi_input_free(input);
		gtk_assistant_insert_page(GTK_ASSISTANT(self), page, 3);
		gtk_assistant_set_page_type(GTK_ASSISTANT(self), page, GTK_ASSISTANT_PAGE_CONTENT);
		gtk_assistant_set_page_title(GTK_ASSISTANT(self), page, "Parameter Selection");
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), page, FALSE);
		g_signal_connect_swapped(page, "notify::xpath-expressions", G_CALLBACK(xpath_changed_cb), self);
		self->xmsi_selection_page = page;
		gtk_widget_show_all(page);

		xpath_changed_cb(self, NULL, XMI_MSIM_GUI_XMSI_SELECTION_SCROLLED_WINDOW(page));
			

		//gtk_widget_show_all_children(GTK_WIDGET(self));
		//gtk_assistant_update_buttons_state(GTK_ASSISTANT(self));
	}


	gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(file_chooser), nfilenames ? TRUE : FALSE);

	g_slist_free_full(filenames, g_free);
}

static void xmi_msim_gui_batch_assistant_init(XmiMsimGuiBatchAssistant *self) {

	GtkWidget *intro_page = gtk_label_new(
		"<span font_weight=\"bold\" font_size=\"x-large\">Welcome to the XMI-MSIM Batch Mode!</span>\n\n"
		"<span font_size=\"x-large\">Here you will be able to either:\n"
		"1. Simulate a bunch of input-files you created already.\n"
		"2. Take a single input-file and vary one or two parameters within a range.</span>\n"
		);
	g_object_set(intro_page, "use-markup", TRUE, NULL);
	gtk_assistant_append_page(GTK_ASSISTANT(self), intro_page);
	gtk_assistant_set_page_type(GTK_ASSISTANT(self), intro_page, GTK_ASSISTANT_PAGE_INTRO);
	gtk_assistant_set_page_title(GTK_ASSISTANT(self), intro_page, "Welcome!");
	gtk_assistant_set_page_complete(GTK_ASSISTANT(self), intro_page, TRUE);

	GtkWidget *file_chooser_page = gtk_file_chooser_widget_new(GTK_FILE_CHOOSER_ACTION_OPEN);
	GtkFileFilter *filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter, "*.xmsi");
	gtk_file_filter_add_pattern(filter, "*.XMSI");
	gtk_file_filter_set_name(filter, "XMI-MSIM inputfiles");
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(file_chooser_page), filter);
	gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(file_chooser_page), TRUE);
	gtk_assistant_append_page(GTK_ASSISTANT(self), file_chooser_page);
	gtk_assistant_set_page_type(GTK_ASSISTANT(self), file_chooser_page, GTK_ASSISTANT_PAGE_CONTENT);
	gtk_assistant_set_page_title(GTK_ASSISTANT(self), file_chooser_page, "Choose your files!");
	gtk_assistant_set_page_complete(GTK_ASSISTANT(self), file_chooser_page, FALSE);
	g_signal_connect_swapped(file_chooser_page, "selection-changed", G_CALLBACK(file_chooser_selection_changed_cb), self);

	GtkWidget *batch_controls_page = xmi_msim_gui_batch_controls_box_new();
	gtk_assistant_append_page(GTK_ASSISTANT(self), batch_controls_page);
	gtk_assistant_set_page_type(GTK_ASSISTANT(self), batch_controls_page, GTK_ASSISTANT_PAGE_CONTENT);
	gtk_assistant_set_page_title(GTK_ASSISTANT(self), batch_controls_page, "Batch Controls");
	gtk_assistant_set_page_complete(GTK_ASSISTANT(self), batch_controls_page, FALSE);
	self->batch_controls_page = batch_controls_page;

	GtkWidget *summary_page = gtk_label_new("Thanks for using the XMI-MSIM Batch Mode!");
	gtk_assistant_append_page(GTK_ASSISTANT(self), summary_page);
	gtk_assistant_set_page_type(GTK_ASSISTANT(self), summary_page, GTK_ASSISTANT_PAGE_SUMMARY);
	gtk_assistant_set_page_title(GTK_ASSISTANT(self), summary_page, "Fin");
	gtk_assistant_set_page_complete(GTK_ASSISTANT(self), summary_page, TRUE);
}

GtkWidget* xmi_msim_gui_batch_assistant_new(GtkWindow *parent) {
	return GTK_WIDGET(g_object_new(XMI_MSIM_GUI_TYPE_BATCH_ASSISTANT,
		"title", "XMI-MSIM Batch Mode",
		"transient-for", parent,
		"modal", FALSE,
		"window-position", GTK_WIN_POS_CENTER_ON_PARENT,
		NULL));
}
