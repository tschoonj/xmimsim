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
#include <stdlib.h>
#include <string.h>
#include "xmimsim-gui-application-window.h"
#include "xmimsim-gui-undo-manager.h"
#include "xmimsim-gui-clipboard-manager.h"
#include "xmimsim-gui-xmsi-config-scrolled-window.h"
#include "xmimsim-gui-controls-scrolled-window.h"
#include "xmimsim-gui-xmso-results-scrolled-window.h"
#include "xmimsim-gui-sources-dialog.h"
#include "xmimsim-gui-private.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-utils.h"
#include "xmi_aux.h"

#ifdef HAVE_GOOGLE_ANALYTICS
  #include "xmi_google_analytics.h"
#endif

#ifdef __APPLE__
  #include "xmimsim-gui-osx.h"
#endif

#ifdef __APPLE__
  #define XMIMSIM_TITLE_PREFIX ""
#else
  #define XMIMSIM_TITLE_PREFIX "XMI-MSIM: "
#endif

struct _XmiMsimGuiApplicationWindowClass {
	GtkApplicationWindowClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiApplicationWindow, xmi_msim_gui_application_window, GTK_TYPE_APPLICATION_WINDOW)

static void xmi_msim_gui_application_window_finalize(GObject *gobject) {
	XmiMsimGuiApplicationWindow *self = XMI_MSIM_GUI_APPLICATION_WINDOW(gobject);

	G_OBJECT_CLASS(xmi_msim_gui_application_window_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_application_window_dispose(GObject *gobject) {
	XmiMsimGuiApplicationWindow *self = XMI_MSIM_GUI_APPLICATION_WINDOW(gobject);

	g_clear_object(&self->undo_manager);
	g_clear_object(&self->clipboard_manager);

	G_OBJECT_CLASS(xmi_msim_gui_application_window_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_application_window_class_init(XmiMsimGuiApplicationWindowClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_application_window_dispose;
	object_class->finalize = xmi_msim_gui_application_window_finalize;
}

static void save_as_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling save_as_activated");

	XmiMsimGuiApplicationWindow *window = XMI_MSIM_GUI_APPLICATION_WINDOW(user_data);
	XmiMsimGuiFileChooserDialog *dialog;
	GtkFileFilter *filter;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsi");
	gtk_file_filter_add_pattern(filter,"*.XMSI");
	gtk_file_filter_set_name(filter,"XMI-MSIM inputfiles");
	dialog = xmi_msim_gui_file_chooser_dialog_new ("Save simulation inputfile",
		GTK_WINDOW(user_data),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		"_Save",
		"_Cancel"
		);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog), TRUE);

	gchar *inputfile  = g_strdup(xmi_msim_gui_undo_manager_get_output_filename(window->undo_manager));
	g_debug("inputfile: %s", inputfile);
	inputfile[strlen(inputfile)-1] = 'i';
	gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(dialog), inputfile);
	gchar *basename = g_path_get_basename(inputfile);
	g_free(inputfile);
	gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(dialog), basename);
	g_free(basename);

	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		xmi_msim_gui_utils_ensure_extension(&filename, ".xmsi");
		xmi_msim_gui_file_chooser_dialog_destroy(dialog);
		GError *error = NULL;
		if (xmi_msim_gui_undo_manager_saveas_file(window->undo_manager, filename, &error) == FALSE) {
			GtkWidget *message_dialog = gtk_message_dialog_new(
				GTK_WINDOW(window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Could not save to file %s", xmi_msim_gui_undo_manager_get_filename(window->undo_manager)
			);
			gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(message_dialog), "%s", error->message);
	    		gtk_dialog_run(GTK_DIALOG(message_dialog));
			gtk_widget_destroy(message_dialog);
			g_error_free(error);
			g_free(filename);
		}
		else {
			basename = g_path_get_basename(filename);
			gchar *xmimsim_title_xmsi = g_strdup_printf(XMIMSIM_TITLE_PREFIX "%s", basename);
			g_free(basename);
			gtk_window_set_title(GTK_WINDOW(user_data), xmimsim_title_xmsi);
			g_free(xmimsim_title_xmsi);
#ifdef __APPLE__
			xmi_msim_gui_osx_nswindow_set_file(user_data, filename);
#endif
			g_free(filename);
		}
	}
}

static void save_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling save_activated");
	/* if never saved before -> call save as
	 * if saved before -> overwrite file
	 * if saved before but no changes -> do nothing (emit warning as shouldnt be possible!)
	 */
	XmiMsimGuiApplicationWindow *window = XMI_MSIM_GUI_APPLICATION_WINDOW(user_data);
	XmiMsimGuiUndoManagerStatus status = xmi_msim_gui_undo_manager_get_status(window->undo_manager);
	switch (status) {
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_DEFAULT:
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_INVALID:
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_INVALID:
			g_warning("save_activated: trying to save invalid file!");
			return;
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_NO_CHANGES:
			g_warning("save_activated: trying to save even though there were no changes");
			return;
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_VALID:
			save_as_activated(action, parameter, user_data);
			break;
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_VALID:
		{
			GError *error = NULL;
			if (xmi_msim_gui_undo_manager_save_file(window->undo_manager, &error) == FALSE) {
				GtkWidget *message_dialog = gtk_message_dialog_new(
					GTK_WINDOW(window),
					GTK_DIALOG_DESTROY_WITH_PARENT,
					GTK_MESSAGE_ERROR,
					GTK_BUTTONS_CLOSE,
					"Could not save to file %s", xmi_msim_gui_undo_manager_get_filename(window->undo_manager)
				);
				gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(message_dialog), "%s", error->message);
	    			gtk_dialog_run(GTK_DIALOG(message_dialog));
				gtk_widget_destroy(message_dialog);
				g_error_free(error);
			}
			break;
		}
		default:
			g_warning("save_activated: unknown case detected!");
			
	}
}


static void undo_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling undo_activated");
	xmi_msim_gui_undo_manager_undo(XMI_MSIM_GUI_APPLICATION_WINDOW(user_data)->undo_manager);
}

static void redo_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling redo_activated");
	xmi_msim_gui_undo_manager_redo(XMI_MSIM_GUI_APPLICATION_WINDOW(user_data)->undo_manager);
}

static void cut_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling cut_activated");
	xmi_msim_gui_clipboard_manager_cut(XMI_MSIM_GUI_APPLICATION_WINDOW(user_data)->clipboard_manager);
}

static void copy_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling copy_activated");
	xmi_msim_gui_clipboard_manager_copy(XMI_MSIM_GUI_APPLICATION_WINDOW(user_data)->clipboard_manager);
}

static void paste_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling paste_activated");
	xmi_msim_gui_clipboard_manager_paste(XMI_MSIM_GUI_APPLICATION_WINDOW(user_data)->clipboard_manager);
}

static gboolean window_delete_event(XmiMsimGuiApplicationWindow *window, GdkEvent *event);

static void close_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	if (window_delete_event(XMI_MSIM_GUI_APPLICATION_WINDOW(user_data), NULL) == FALSE)
		gtk_widget_destroy(GTK_WIDGET(user_data));
}

static void sources_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling sources_activated");
	XmiMsimGuiApplicationWindow *self = XMI_MSIM_GUI_APPLICATION_WINDOW(user_data);

	xmi_input *current_input = xmi_msim_gui_undo_manager_get_current_input(self->undo_manager);
	GtkWidget *dialog = xmi_msim_gui_sources_dialog_new(GTK_WINDOW(self), current_input);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		// get the excitation data from the dialog
		xmi_excitation *excitation = xmi_msim_gui_sources_dialog_get_raw_data(XMI_MSIM_GUI_SOURCES_DIALOG(dialog));
		gchar *source_name = g_strdup(xmi_msim_gui_sources_dialog_get_active_source_name(XMI_MSIM_GUI_SOURCES_DIALOG(dialog)));

		gtk_widget_destroy(dialog);

		// unlikely to occur: produce an error message if it does...
		if (excitation == NULL) {
			GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(self), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "xmi_msim_gui_sources_dialog_get_raw_data returned NULL!!!");
			gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "This is a bug and should be reported to the developers");
			gtk_dialog_run(GTK_DIALOG(info_dialog));
			gtk_widget_destroy(info_dialog);
			xmi_input_free(current_input);
			g_free(source_name);
			return;	
		}

		// start new dialog: we need to know if the existing spectrum needs to be replaced or augmented with the new one
		GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(self), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "Add new source spectrum to current excitation spectrum or replace it completely?");
		gtk_dialog_add_buttons(GTK_DIALOG(info_dialog), "_Add", GTK_RESPONSE_OK, "_Replace", GTK_RESPONSE_CANCEL, NULL);
		//this may not work on all platforms -> Mac OS X
		gtk_window_set_deletable(GTK_WINDOW(info_dialog), FALSE);
		int rv;
		while ((rv = gtk_dialog_run(GTK_DIALOG(info_dialog))) == GTK_RESPONSE_DELETE_EVENT) {
		}
		gtk_widget_destroy(info_dialog);
		if (rv == GTK_RESPONSE_OK) {
			// add
			int i;
			if (current_input->excitation->n_discrete > 0) {
				for (i = 0 ; i < current_input->excitation->n_discrete ; i++) {
					if (bsearch(excitation->discrete+i, current_input->excitation->discrete, current_input->excitation->n_discrete, sizeof(xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
						GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(self), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy lines: one or more of the new energies exist already in the list of lines.");
						gtk_dialog_run(GTK_DIALOG(error_dialog));
						gtk_widget_destroy(error_dialog);
						xmi_input_free(current_input);
						xmi_excitation_free(excitation);
						g_free(source_name);
						return;
					}
				}
			}
			if (current_input->excitation->n_continuous > 0) {
				for (i = 0 ; i < current_input->excitation->n_continuous ; i++) {
					if (bsearch(excitation->continuous + i, current_input->excitation->continuous, current_input->excitation->n_continuous, sizeof(xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
						GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(self), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy intervals: one or more of the new energies exist already in the list of intervals.");
						gtk_dialog_run(GTK_DIALOG(error_dialog));
						gtk_widget_destroy(error_dialog);
						xmi_input_free(current_input);
						xmi_excitation_free(excitation);
						g_free(source_name);
						return;
					}
				}
			}
			// update current_input->excitation with excitation
			current_input->excitation->n_discrete += excitation->n_discrete;
			//realloc discrete energies
			current_input->excitation->discrete = g_realloc(current_input->excitation->discrete, sizeof(xmi_energy_discrete) * current_input->excitation->n_discrete);
			for (i = current_input->excitation->n_discrete - excitation->n_discrete ; i < current_input->excitation->n_discrete ; i++) {
				current_input->excitation->discrete[i] = excitation->discrete[i - current_input->excitation->n_discrete + excitation->n_discrete];
			}
			qsort(current_input->excitation->discrete, current_input->excitation->n_discrete, sizeof(xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete);
			current_input->excitation->n_continuous += excitation->n_continuous;
			//realloc continuous energies
			current_input->excitation->continuous = g_realloc(current_input->excitation->continuous, sizeof(xmi_energy_continuous) * current_input->excitation->n_continuous);
			for (i = current_input->excitation->n_continuous - excitation->n_continuous ; i < current_input->excitation->n_continuous ; i++) {
				current_input->excitation->continuous[i] = excitation->continuous[i - current_input->excitation->n_continuous + excitation->n_continuous];
			}
			qsort(current_input->excitation->continuous, current_input->excitation->n_continuous, sizeof(xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous);
			GtkWidget *energiesW = XMI_MSIM_GUI_XMSI_CONFIG_SCROLLED_WINDOW(self->input_page)->energiesW;
			xmi_msim_gui_energies_box_set_excitation(XMI_MSIM_GUI_ENERGIES_BOX(energiesW), current_input->excitation);
			//update_undo_buffer(SOURCE_SPECTRUM_ADD, (GtkWidget *) excitation);
			gchar *message = g_strdup_printf("adding external %s spectrum", source_name);
			g_signal_emit_by_name(energiesW, "changed", message);
			g_free(message);
		} else {
			// replace
			//update_undo_buffer(SOURCE_SPECTRUM_REPLACE, (GtkWidget *) excitation);
			GtkWidget *energiesW = XMI_MSIM_GUI_XMSI_CONFIG_SCROLLED_WINDOW(self->input_page)->energiesW;
			xmi_msim_gui_energies_box_set_excitation(XMI_MSIM_GUI_ENERGIES_BOX(energiesW), excitation);
			gchar *message = g_strdup_printf("replacing with external %s spectrum", source_name);
			g_signal_emit_by_name(energiesW, "changed", message);
			g_free(message);
		}
		xmi_input_free(current_input);
		xmi_excitation_free(excitation);
		g_free(source_name);
	}
	else {
		gtk_widget_destroy(dialog);
	}
}

static void switch_tab_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	g_debug("Calling switch_tab_activated: %s", g_variant_get_string(parameter, NULL));

	gtk_notebook_set_current_page(GTK_NOTEBOOK(XMI_MSIM_GUI_APPLICATION_WINDOW(user_data)->notebook), atoi(g_variant_get_string(parameter, NULL)));
}

static void minimize_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	gtk_window_iconify(GTK_WINDOW(user_data));
}

static GActionEntry win_entries[] = {
	{"save-as", save_as_activated, NULL, NULL, NULL},
	{"close", close_activated, NULL, NULL, NULL},
	{"save", save_activated, NULL, NULL, NULL},
	{"undo", undo_activated, NULL, NULL, NULL},
	{"redo", redo_activated, NULL, NULL, NULL},
	{"cut", cut_activated, NULL, NULL, NULL},
	{"copy", copy_activated, NULL, NULL, NULL},
	{"paste", paste_activated, NULL, NULL, NULL},
	{"sources", sources_activated, NULL, NULL, NULL},
	{"switch-tab", switch_tab_activated, "s", NULL, NULL},
	{"minimize", minimize_activated, NULL, NULL, NULL},
};

static void update_clipboard_buttons(XmiMsimGuiClipboardManager *clipboard_manager, gboolean cut_val, gboolean copy_val, gboolean paste_val, XmiMsimGuiApplicationWindow *window) {
	GAction *action = NULL;
	
	action = g_action_map_lookup_action(G_ACTION_MAP(window), "cut");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), cut_val);
	action = g_action_map_lookup_action(G_ACTION_MAP(window), "copy");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), copy_val);
	action = g_action_map_lookup_action(G_ACTION_MAP(window), "paste");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), paste_val);
}

static void update_status_buttons(XmiMsimGuiUndoManager *manager, gboolean saveas_status, gboolean save_status, gchar *undo_string, gchar *redo_string, gboolean valid_status, XmiMsimGuiApplicationWindow *window) {
	GAction *action = NULL;
	
	action = g_action_map_lookup_action(G_ACTION_MAP(window), "save-as");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), saveas_status);
	action = g_action_map_lookup_action(G_ACTION_MAP(window), "save");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), save_status);
	action = g_action_map_lookup_action(G_ACTION_MAP(window), "undo");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), undo_string != NULL);
	action = g_action_map_lookup_action(G_ACTION_MAP(window), "redo");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), redo_string != NULL);
	action = g_action_map_lookup_action(G_ACTION_MAP(window), "sources");
	g_simple_action_set_enabled(G_SIMPLE_ACTION(action), valid_status);
}

static void append_tool_button(GtkWidget *toolbar, const gchar *label, const gchar *icon_name, const gchar *action_name) {
	GtkToolItem *button = gtk_tool_button_new(NULL, label);
	gtk_tool_button_set_icon_name(GTK_TOOL_BUTTON(button), icon_name);
	gtk_actionable_set_action_name(GTK_ACTIONABLE(button), action_name);
	gtk_widget_set_can_focus(GTK_WIDGET(button), FALSE);
#if GTK_CHECK_VERSION(3, 20, 0)
	gtk_widget_set_focus_on_click(GTK_WIDGET(button), FALSE);
#endif
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), button, -1);
}

static gboolean job_killer(GtkWidget *dialog) {

	XmiMsimGuiApplicationWindow *window = XMI_MSIM_GUI_APPLICATION_WINDOW(gtk_window_get_transient_for(GTK_WINDOW(dialog)));
	XmiMsimGuiControlsScrolledWindow *controls_page = XMI_MSIM_GUI_CONTROLS_SCROLLED_WINDOW(window->controls_page);

	if (controls_page->job && xmi_msim_job_is_running(controls_page->job)) {
		return G_SOURCE_CONTINUE;
	}
	gtk_dialog_response(GTK_DIALOG(dialog), GTK_RESPONSE_CLOSE);

	return G_SOURCE_REMOVE;
}

static gboolean window_delete_event(XmiMsimGuiApplicationWindow *window, GdkEvent *event) {
	g_debug("Calling window_delete_event");
	// check if file needs saving
	// 1. if file has never been saved before, and data is not different from defaults -> close
	// 2. if file has never been saved before, but with valid data -> offer to save as
	// 3. if file has never been saved before, but with invalid data -> offer to close without saving or continue
	// 4. if file has been saved before, but with valid changes -> offer to save
	// 5. if file has been saved before, but with invalid changes -> offer to close without saving or continue
	// 6. if file has been save before, without changes -> just close
	
	XmiMsimGuiUndoManagerStatus status = xmi_msim_gui_undo_manager_get_status(window->undo_manager);
	gboolean rv;

	switch (status) {
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_DEFAULT:
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_NO_CHANGES:
			rv = FALSE;
			break;
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_INVALID:
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_INVALID:
		{
			GtkWidget *dialog = gtk_dialog_new_with_buttons("Closing Window...",
				GTK_WINDOW(window),
				GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
				"_Cancel", GTK_RESPONSE_CANCEL,
				"Close Anyway", GTK_RESPONSE_CLOSE,
				NULL);
			GtkWidget *content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
			GtkWidget *label = gtk_label_new("You are closing a window containing incomplete and/or incorrect data that cannot be saved. Clicking Cancel will allow you to resume working on the input data.");
			gtk_widget_show(label);
			gtk_box_pack_start(GTK_BOX(content),label, FALSE, FALSE, 3);
			switch (gtk_dialog_run(GTK_DIALOG(dialog))) {
				case GTK_RESPONSE_CANCEL:
				case GTK_RESPONSE_DELETE_EVENT:
					gtk_widget_destroy(dialog);
					rv = TRUE;
					break;
				case GTK_RESPONSE_CLOSE:
					gtk_widget_destroy(dialog);
					rv = FALSE;
					break;
			}
			break;
		}
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_VALID:
		{
			GtkWidget *dialog = gtk_dialog_new_with_buttons("Closing Window...",
				GTK_WINDOW(window),
				GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
				"_Save and Close", GTK_RESPONSE_OK,
				"_Cancel", GTK_RESPONSE_CANCEL,
				"Close _Without Saving", GTK_RESPONSE_CLOSE,
				NULL);
			GtkWidget *content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
			GtkWidget *label = gtk_label_new("You have made changes since your last save. This is your last chance to save them or they will be lost forever!");
			gtk_widget_show(label);
			gtk_box_pack_start(GTK_BOX(content),label, FALSE, FALSE, 3);
			switch (gtk_dialog_run(GTK_DIALOG(dialog))) {
				case GTK_RESPONSE_CANCEL:
				case GTK_RESPONSE_DELETE_EVENT:
					gtk_widget_destroy(dialog);
					rv = TRUE;
					break;
				case GTK_RESPONSE_CLOSE:
					gtk_widget_destroy(dialog);
					rv = FALSE;
					break;
				case GTK_RESPONSE_OK:
				{
					gtk_widget_destroy(dialog);
					GAction *action = g_action_map_lookup_action(G_ACTION_MAP(window), "save-as");
					g_action_activate(action, NULL);
					rv = FALSE;
					break;
				}
			}
			break;
		}
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_VALID:
		{
			GtkWidget *dialog = gtk_dialog_new_with_buttons("Closing Window...",
				GTK_WINDOW(window),
				GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
				"_Save and Close", GTK_RESPONSE_OK,
				"_Cancel", GTK_RESPONSE_CANCEL,
				"Close _Without Saving", GTK_RESPONSE_CLOSE,
				NULL);
			GtkWidget *content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
			GtkWidget *label = gtk_label_new("You have made changes since your last save. This is your last chance to save them or they will be lost forever!");
			gtk_widget_show(label);
			gtk_box_pack_start(GTK_BOX(content),label, FALSE, FALSE, 3);
			switch (gtk_dialog_run(GTK_DIALOG(dialog))) {
				case GTK_RESPONSE_CANCEL:
				case GTK_RESPONSE_DELETE_EVENT:
					gtk_widget_destroy(dialog);
					rv = TRUE;
					break;
				case GTK_RESPONSE_CLOSE:
					gtk_widget_destroy(dialog);
					rv = FALSE;
					break;
				case GTK_RESPONSE_OK:
				{
					gtk_widget_destroy(dialog);
					GAction *action = g_action_map_lookup_action(G_ACTION_MAP(window), "save");
					g_action_activate(action, NULL);
					rv = FALSE;
					break;
				}
			}
			break;
		}
		default:
			g_warning("window_delete_event: unknow status detected");
	}

	g_debug("rv: %d", rv);

	if (rv == TRUE)
		return TRUE;

	// afterwards, if user still decides to close the window -> check if there is still a job running!
	XmiMsimGuiControlsScrolledWindow *controls_page = XMI_MSIM_GUI_CONTROLS_SCROLLED_WINDOW(window->controls_page);
	if (controls_page->job && xmi_msim_job_is_running(controls_page->job)) {
		g_debug("job is running!");
		GtkWidget *dialog = gtk_dialog_new_with_buttons("Closing Window...",
			GTK_WINDOW(window),
			GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
			"_Kill job and close", GTK_RESPONSE_OK,
			"_Cancel", GTK_RESPONSE_CANCEL,
			NULL);
		GtkWidget *content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
		GtkWidget *label = gtk_label_new("The window you are trying to close still has a job running!");
		gtk_widget_show(label);
		gtk_box_pack_start(GTK_BOX(content),label, FALSE, FALSE, 3);
		guint source_id = g_timeout_add_seconds(1, (GSourceFunc) job_killer, dialog);
		switch (gtk_dialog_run(GTK_DIALOG(dialog))) {
			case GTK_RESPONSE_CANCEL:
			case GTK_RESPONSE_DELETE_EVENT:
				g_source_remove(source_id);
				gtk_widget_destroy(dialog);
				return TRUE;
			case GTK_RESPONSE_OK:
				g_source_remove(source_id);
			case GTK_RESPONSE_CLOSE: // from job_killer!
			{
				gtk_widget_destroy(dialog);
				if (controls_page->job && xmi_msim_job_is_running(controls_page->job)) {
					xmi_msim_job_kill(controls_page->job, NULL);
				}
				return FALSE;
			}
		}
	}

	g_debug("before return rv: %d", rv);
	return rv;
}

static void notebook_page_changed_cb(GtkNotebook *notebook, gpointer pageptr, guint page, XmiMsimGuiApplicationWindow *window) {
	if (page == 2) { // results page
		const gchar *filename = xmi_msim_gui_xmso_results_scrolled_window_get_filename(XMI_MSIM_GUI_XMSO_RESULTS_SCROLLED_WINDOW(window->results_page));
#ifdef __APPLE__
		xmi_msim_gui_osx_nswindow_set_file(GTK_WIDGET(window), filename);
#endif
		if (filename == NULL) {
			gtk_window_set_title(GTK_WINDOW(window), "No simulation data available");
			return;
		}
		gchar *basename = g_path_get_basename(filename);
		gchar *xmimsim_title_xmso = g_strdup_printf(XMIMSIM_TITLE_PREFIX "%s", basename);
		g_free(basename);
		gtk_window_set_title(GTK_WINDOW(window), xmimsim_title_xmso);
		g_free(xmimsim_title_xmso);
	}
	else {
		const gchar *filename = xmi_msim_gui_undo_manager_get_filename(XMI_MSIM_GUI_XMSI_CONFIG_SCROLLED_WINDOW(window->input_page)->undo_manager);
#ifdef __APPLE__
		xmi_msim_gui_osx_nswindow_set_file(GTK_WIDGET(window), filename);
#endif
		if (filename == NULL) {
			gtk_window_set_title(GTK_WINDOW(window), "New file");
			return;
		}
		gchar *basename = g_path_get_basename(filename);
		gchar *xmimsim_title_xmsi = g_strdup_printf(XMIMSIM_TITLE_PREFIX "%s", basename);
		g_free(basename);
		gtk_window_set_title(GTK_WINDOW(window), xmimsim_title_xmsi);
		g_free(xmimsim_title_xmsi);
	}
}

static void file_finished_loading_cb(gpointer loader, const gchar *filename, XmiMsimGuiApplicationWindow *self) {
	g_debug("%s finished loading", filename);
	if (loader == self->undo_manager) {
		if (gtk_notebook_get_current_page(GTK_NOTEBOOK(self->notebook)) == 0)
			notebook_page_changed_cb(GTK_NOTEBOOK(self->notebook), NULL, 0, self);
		else
			gtk_notebook_set_current_page(GTK_NOTEBOOK(self->notebook), 0);
	}
	else if (loader == self->results_page) {
		if (gtk_notebook_get_current_page(GTK_NOTEBOOK(self->notebook)) == 2)
			notebook_page_changed_cb(GTK_NOTEBOOK(self->notebook), NULL, 2, self);
		else
			gtk_notebook_set_current_page(GTK_NOTEBOOK(self->notebook), 2);
	}
	else
		g_warning("file_finished_loading_cb: unknown loader detected");
}

G_LOCK_DEFINE_STATIC(window_counter);

static void xmi_msim_gui_application_window_init(XmiMsimGuiApplicationWindow *self) {
	static int window_counter = 0;
	int window_number;

	G_LOCK(window_counter);
	window_number = window_counter++;
	G_UNLOCK(window_counter);

	gchar *name = g_strdup_printf("XmiMsimGuiApplicationWindow_%d", window_number);
	gtk_widget_set_name(GTK_WIDGET(self), name);
	g_free(name);

	// populate action map
	g_action_map_add_action_entries(G_ACTION_MAP(self), win_entries, G_N_ELEMENTS(win_entries), self);
	self->undo_manager = xmi_msim_gui_undo_manager_new();
	self->clipboard_manager = xmi_msim_gui_clipboard_manager_new();

	GtkWidget *Main_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(self), Main_vbox);

	GtkWidget *toolbar = gtk_toolbar_new();
	append_tool_button(toolbar, "New Window", "document-new", "app.new");
	append_tool_button(toolbar, "Open", "document-open", "app.open");
	append_tool_button(toolbar, "Save As", "document-save-as", "win.save-as");
	append_tool_button(toolbar, "Save", "document-save", "win.save");
	append_tool_button(toolbar, "Undo", "edit-undo", "win.undo");
	append_tool_button(toolbar, "Redo", "edit-redo", "win.redo");
	append_tool_button(toolbar, "Cut", "edit-cut", "win.cut");
	append_tool_button(toolbar, "Copy", "edit-copy", "win.copy");
	append_tool_button(toolbar, "Paste", "edit-paste", "win.paste");
	
	GtkToolItem *separator = gtk_separator_tool_item_new();
	gtk_separator_tool_item_set_draw(GTK_SEPARATOR_TOOL_ITEM(separator), FALSE);
	gtk_tool_item_set_expand(separator, TRUE);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), separator, -1);

	append_tool_button(toolbar, "Batch mode", "Logo_xmi_msim_archive", "app.batch");
	append_tool_button(toolbar, "X-ray sources", "Radiation_warning_symbol", "win.sources");
	append_tool_button(toolbar, "Preferences", "preferences-system", "app.preferences");

	gtk_box_pack_start(GTK_BOX(Main_vbox), toolbar, FALSE, FALSE, 3);
	gtk_widget_show_all(toolbar);

	//notebook
	self->notebook = gtk_notebook_new();
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(self->notebook), GTK_POS_TOP);

	GtkWidget *label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), "<span size=\"large\">Input parameters</span>");
	self->input_page = xmi_msim_gui_xmsi_config_scrolled_window_new(self->undo_manager, self->clipboard_manager);
	gtk_notebook_append_page(GTK_NOTEBOOK(self->notebook), self->input_page, label);
	gtk_container_child_set(GTK_CONTAINER(self->notebook), self->input_page, "tab-expand", TRUE, "tab-fill", TRUE, NULL);
	gtk_box_pack_start(GTK_BOX(Main_vbox), self->notebook, TRUE, TRUE, 3);
	gtk_widget_grab_focus(label);

	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), "<span size=\"large\">Simulation controls</span>");
	self->controls_page = xmi_msim_gui_controls_scrolled_window_new(self->undo_manager);
	gtk_notebook_append_page(GTK_NOTEBOOK(self->notebook), self->controls_page, label);
	gtk_container_child_set(GTK_CONTAINER(self->notebook), self->controls_page, "tab-expand", TRUE, "tab-fill", TRUE, NULL);

	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), "<span size=\"large\">Results</span>");
	self->results_page = xmi_msim_gui_xmso_results_scrolled_window_new();
	gtk_notebook_append_page(GTK_NOTEBOOK(self->notebook), self->results_page, label);
	gtk_container_child_set(GTK_CONTAINER(self->notebook), self->results_page, "tab-expand", TRUE, "tab-fill", TRUE, NULL);

	g_signal_connect(self->clipboard_manager, "update-clipboard-buttons", G_CALLBACK(update_clipboard_buttons), self);
	g_signal_connect(self->undo_manager, "update-status-buttons", G_CALLBACK(update_status_buttons), self);
	g_signal_connect(G_OBJECT(self), "delete-event", G_CALLBACK(window_delete_event), self);

	gtk_widget_show_all(self->input_page);
	gtk_widget_show(self->controls_page);
	gtk_widget_show_all(self->results_page);
	gtk_widget_show(self->notebook);
	gtk_widget_show(Main_vbox);

	gint main_height=900;
	gint main_width=950;
	gint main_temp;

	//size must depend on available height (and width too I guess)
#if GTK_CHECK_VERSION(3, 22, 0)
	GdkMonitor *monitor = gdk_display_get_primary_monitor(gdk_display_get_default());
	GdkRectangle geometry;
	gdk_monitor_get_geometry(monitor, &geometry);

	main_temp = 0.90 * (double) geometry.height;
	if (main_temp <= main_height)
		main_height = main_temp;

	main_temp = 0.95 * (double) geometry.width;
	if (main_temp <= main_width)
		main_width = main_temp;
#else
	main_temp = 0.90 * (double) gdk_screen_get_height(gdk_screen_get_default());
	if (main_temp <= main_height)
		main_height = main_temp;

	main_temp = 0.95 * (double) gdk_screen_get_width(gdk_screen_get_default());
	if (main_temp <= main_width)
		main_width = main_temp;
#endif

	gtk_window_set_default_size(GTK_WINDOW(self), main_width, main_height);

	// ensure clipboard buttons are deactivated initially
	update_clipboard_buttons(self->clipboard_manager, FALSE, FALSE, FALSE, self);

	gtk_notebook_set_current_page(GTK_NOTEBOOK(self->notebook), 0);

	g_signal_connect(G_OBJECT(self->notebook), "switch-page", G_CALLBACK(notebook_page_changed_cb), self);
	g_signal_connect(G_OBJECT(self->undo_manager), "finished-loading", G_CALLBACK(file_finished_loading_cb), self);
	g_signal_connect(G_OBJECT(self->results_page), "finished-loading", G_CALLBACK(file_finished_loading_cb), self);
	gtk_window_set_title(GTK_WINDOW(self), "New file");
}

GtkWidget* xmi_msim_gui_application_window_new(XmiMsimGuiApplication *app) {
	GObject *object = g_object_new(XMI_MSIM_GUI_TYPE_APPLICATION_WINDOW, "application", app, NULL);
	XmiMsimGuiApplicationWindow *app_window = XMI_MSIM_GUI_APPLICATION_WINDOW(object);
	xmi_msim_gui_undo_manager_create_new_file(app_window->undo_manager);

	return GTK_WIDGET(app_window);
}

gboolean xmi_msim_gui_application_window_load_file(XmiMsimGuiApplicationWindow *window, const gchar *filename, GError **error) {
	return xmi_msim_gui_undo_manager_load_file(window->undo_manager, filename, error);
}

