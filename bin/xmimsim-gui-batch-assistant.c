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

#include "xmi_xml.h"

#include <unistd.h>
#include <glib/gstdio.h>

struct _XmiMsimGuiBatchAssistant {
	GtkAssistant parent_instance;
	GPtrArray *xmsi_files;
	GPtrArray *options_pages;
	GtkWidget *multi_options_question_page;
	GtkWidget *xmsi_selection_page;
	GtkWidget *archive_settings_page;
	GtkWidget *batch_controls_page;
};

struct _XmiMsimGuiBatchAssistantClass {
	GtkAssistantClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiBatchAssistant, xmi_msim_gui_batch_assistant, GTK_TYPE_ASSISTANT)

static void xmi_msim_gui_batch_assistant_dispose(GObject *gobject) {
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

static void xmi_msim_gui_batch_assistant_shutdown(XmiMsimGuiBatchAssistant *self) {
	gtk_widget_destroy(GTK_WIDGET(self));
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

static void single_batch_data_changed_cb(XmiMsimGuiBatchAssistant *self, GParamSpec *pspec, XmiMsimGuiBatchArchiveSettingsBox *archive_setting_page) {

	GPtrArray *arr = xmi_msim_gui_batch_archive_settings_box_get_data(XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX(archive_setting_page));

	if (arr) {
		g_debug("single_batch_data_changed_cb: valid data found");
		unsigned int i;
		for (i = 0 ; i < arr->len ; i++) {
			xmi_batch_single_data *data = g_ptr_array_index(arr, i);
			g_debug("single_batch_data_changed_cb: %d -> %s", i, data->xpath);
			g_debug("single_batch_data_changed_cb: %g -> %g in %d steps", data->start, data->end, data->nsteps);
		}

		g_ptr_array_unref(arr);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(archive_setting_page), TRUE);
	}
	else {
		g_debug("single_batch_data_changed_cb: NO valid data found");
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(archive_setting_page), FALSE);
	}


}

static void xpath_changed_cb(XmiMsimGuiBatchAssistant *self, GParamSpec *pspec, XmiMsimGuiXmsiSelectionScrolledWindow *selection_page) {

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
	}
	else {
		gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(selection_page), FALSE);
	}
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

			

		//gtk_widget_show_all_children(GTK_WIDGET(self));
		//gtk_assistant_update_buttons_state(GTK_ASSISTANT(self));
	}


	gtk_assistant_set_page_complete(GTK_ASSISTANT(self), GTK_WIDGET(file_chooser), nfilenames ? TRUE : FALSE);

	g_slist_free_full(filenames, g_free);
}

static void xmi_msim_gui_batch_assistant_init(XmiMsimGuiBatchAssistant *self) {

	GtkWidget *intro_page = gtk_label_new(
		"<b>Welcome to the XMI-MSIM Batch Mode!</b>\n\n"
		"Here you will be able to either:\n"
		"1. Simulate a bunch of input-files you created already.\n"
		"2. Take an input-file and vary one or two parameters within a range.\n"
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

	// TODO: add simulation page!


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
		"modal", TRUE,
		NULL));
}
