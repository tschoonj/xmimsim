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
#include "xmimsim-gui-undo-manager.h"
#include "xmi_data_structs.h"
#include "xmi_xml.h"

typedef struct {
	gchar *message;
	GArray *signal_ids; // gulongs!
	gboolean valid;
	XmiMsimGuiUndoManagerValueWriter writer;
	XmiMsimGuiUndoManagerValueReader reader;
	XmiMsimGuiUndoManagerValueValidator validator;
} XmiMsimGuiUndoManagerWidgetData;

static void undo_manager_widget_data_free(XmiMsimGuiUndoManagerWidgetData *widget_data) {
	g_free(widget_data->message);
	g_array_free(widget_data->signal_ids, TRUE);
	g_free(widget_data);
}

struct _XmiMsimGuiUndoManager {
	GObject parent_instance;
	GHashTable *widget_table;
	GPtrArray *undo_stack;
	gchar *inputfile;
	struct xmi_input *last_saved_input;
	gint current_index;
};

typedef struct {
	GValue value;
	struct xmi_input *input;
	GtkWidget *widget;
	gchar *message;
} XmiMsimGuiUndoManagerStackData;

static void undo_manager_stack_data_free(XmiMsimGuiUndoManagerStackData *stack_data) {
	g_free(stack_data->message);
	g_value_unset(&stack_data->value);
	xmi_free_input(stack_data->input);
}

struct _XmiMsimGuiUndoManagerClass {
	GObjectClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiUndoManager, xmi_msim_gui_undo_manager, G_TYPE_OBJECT)

static void xmi_msim_gui_undo_manager_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_undo_manager_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_undo_manager_finalize(GObject *gobject) {
	XmiMsimGuiUndoManager *manager = XMI_MSIM_GUI_UNDO_MANAGER(gobject);

	g_hash_table_destroy(manager->widget_table);
	g_ptr_array_unref(manager->undo_stack);
	g_ptr_array_free(manager->undo_stack, TRUE);
	g_free(manager->inputfile);
	xmi_free_input(manager->last_saved_input);

	G_OBJECT_CLASS(xmi_msim_gui_undo_manager_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_undo_manager_class_init(XmiMsimGuiUndoManagerClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_undo_manager_dispose;
	object_class->finalize = xmi_msim_gui_undo_manager_finalize;
	
	// several signals will need to be added: for the save/save as buttons, undo/redo buttons, etc...
	/*signals[AFTER_EVENT] = g_signal_new(
		"after-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__POINTER,
		G_TYPE_NONE,
		1,
		G_TYPE_POINTER // GError *
	);*/

}

static void xmi_msim_gui_undo_manager_init(XmiMsimGuiUndoManager *manager) {
	manager->widget_table = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, (GDestroyNotify) undo_manager_widget_data_free);
	manager->undo_stack = g_ptr_array_new_with_free_func((GDestroyNotify) undo_manager_stack_data_free);
	manager->inputfile = NULL;
	manager->current_index = -1;
	manager->last_saved_input = NULL;
	g_ptr_array_ref(manager->undo_stack);
	/* XmiMsimGuiUndoManagerStackData *stack_data = g_malloc(sizeof(XmiMsimGuiUndoManagerStackData));
	memset(&stack_data->value, 0, sizeof(GValue));
	stack_data->input = xmi_init_empty_input();
	stack_data->widget = NULL;
	stack_data->message = NULL;
	g_ptr_array_add(manager->undo_stack, stack_data);*/
}

XmiMsimGuiUndoManager* xmi_msim_gui_undo_manager_new() {
	return XMI_MSIM_GUI_UNDO_MANAGER(g_object_new(XMI_MSIM_GUI_TYPE_UNDO_MANAGER, NULL));
}

static void widget_set_sensitive_hash(GtkWidget *widget, XmiMsimGuiUndoManagerWidgetData *widget_data, gpointer data) {
	gtk_widget_set_sensitive(widget, GPOINTER_TO_INT(data));
}

static void entry_changed_cb(GtkEntry *entry, XmiMsimGuiUndoManager *manager) {
	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, entry);
	g_return_if_fail(widget_data != NULL); // ensure widget_data exists for entry
	g_return_if_fail(manager->undo_stack->len > 0); // ensure undo_stack isnt empty

	XmiMsimGuiUndoManagerStackData *stack_data_last = g_ptr_array_index(manager->undo_stack, manager->undo_stack->len - 1);

	const gchar *text = gtk_entry_get_text(entry);
	GValue value = G_VALUE_INIT;
	XmiMsimGuiUndoManagerValueValidatorResult validator_result = widget_data->validator(text, stack_data_last->input, &value);

	GtkStyleContext *style_context = gtk_widget_get_style_context(GTK_WIDGET(entry));

	if (validator_result == XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_VALID) {
		if (!widget_data->valid) {
			widget_data->valid = TRUE;
			// foreach widget set sensitive TRUE
			g_hash_table_foreach(manager->widget_table, (GHFunc) widget_set_sensitive_hash, GINT_TO_POINTER(TRUE));
			// update entry background color
			gtk_style_context_remove_class(style_context, "red");
		}

		// extend undo stack
		XmiMsimGuiUndoManagerStackData *stack_data = g_malloc(sizeof(XmiMsimGuiUndoManagerStackData));
		memset(&stack_data->value, 0, sizeof(GValue));
		g_value_init(&stack_data->value, G_VALUE_TYPE(&value));
		g_value_copy(&value, &stack_data->value);
		xmi_copy_input(stack_data_last->input, &stack_data->input);
		widget_data->reader(&value, stack_data->input);
		g_value_unset(&value);
		stack_data->widget = GTK_WIDGET(entry);
		stack_data->message = g_strdup(widget_data->message);
		// if in redo mode -> remove everything more recent than current_index
		if (manager->current_index < manager->undo_stack->len - 1)
			g_ptr_array_remove_range(manager->undo_stack, manager->current_index + 1, manager->undo_stack->len - 1 - manager->current_index);
		g_ptr_array_add(manager->undo_stack, stack_data);
		manager->current_index++;

		// emit signal for save / save as / undo redo buttons	
	}
	else if (validator_result == XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_INVALID) {
		if (widget_data->valid) {
			widget_data->valid = FALSE;
			// foreach widget set sensitive FALSE
			g_hash_table_foreach(manager->widget_table, (GHFunc) widget_set_sensitive_hash, GINT_TO_POINTER(FALSE));
			// update entry background color
			gtk_style_context_add_class(style_context, "red");
			
			// emit signal for save / save as / undo redo buttons	
		}

	}
	else if (validator_result == XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_EQUAL) {
		// take care as to what needs to happen if state was invalid!
		if (!widget_data->valid) {
			widget_data->valid = TRUE;
			// foreach widget set sensitive TRUE
			g_hash_table_foreach(manager->widget_table, (GHFunc) widget_set_sensitive_hash, GINT_TO_POINTER(TRUE));
			// update entry background color
			gtk_style_context_remove_class(style_context, "red");
			// emit signal for save / save as / undo redo buttons	
		}
	}
}

gboolean xmi_msim_gui_undo_manager_register_entry(
	XmiMsimGuiUndoManager *manager,
	GtkEntry *entry,
	const gchar *message,
	XmiMsimGuiUndoManagerValueWriter writer,
	XmiMsimGuiUndoManagerValueReader reader,
	XmiMsimGuiUndoManagerValueValidator validator) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager), FALSE);
	g_return_val_if_fail(GTK_IS_ENTRY(entry), FALSE);

	if (g_hash_table_contains(manager->widget_table, entry)) {
		g_warning("xmi_msim_gui_undo_manager_add_entry: entry already registered by manager");
		return FALSE;
	}

	// activate color support
  	gtk_widget_set_name(GTK_WIDGET(entry), "color_entry");

	XmiMsimGuiUndoManagerWidgetData *widget_data = g_malloc(sizeof(XmiMsimGuiUndoManagerWidgetData));
	widget_data->message = g_strdup(message);
	widget_data->signal_ids = g_array_new(FALSE, FALSE, sizeof(gulong));
	widget_data->valid = FALSE;
	widget_data->writer = writer;
	widget_data->reader = reader;
	widget_data->validator = validator;

	gulong signal_id = g_signal_connect(entry, "changed", G_CALLBACK(entry_changed_cb), manager);
	g_array_append_val(widget_data->signal_ids, signal_id);

	g_hash_table_insert(manager->widget_table, entry, widget_data);

	return TRUE;
}

static void widget_block_signals_hash(GtkWidget *widget, XmiMsimGuiUndoManagerWidgetData *widget_data, gpointer data) {
	gboolean block = GPOINTER_TO_INT(data);
	unsigned int i;

	for (i = 0 ; widget_data->signal_ids->len ; i++) {
		if (block)
			g_signal_handler_block(widget, g_array_index(widget_data->signal_ids, gulong, i));
		else
			g_signal_handler_unblock(widget, g_array_index(widget_data->signal_ids, gulong, i));
	}
}

static void entry_set_background_hash(GtkWidget *widget, XmiMsimGuiUndoManagerWidgetData *widget_data, gpointer data) {
	gboolean default_color = GPOINTER_TO_INT(data);

	if (GTK_IS_ENTRY(widget)) {
		widget_data->valid = default_color;
		GtkStyleContext *style_context = gtk_widget_get_style_context(widget);
		if (default_color) {
			gtk_style_context_remove_class(style_context, "red");
		}
		else {
			gtk_style_context_add_class(style_context, "red");
		}
	}	
}

static void widget_populate_hash_with_value(GtkWidget *widget, GValue *value) {
	// this will be a pretty long function

	if (GTK_IS_ENTRY(widget)) {
		gchar *buffer = NULL;
		if (G_VALUE_HOLDS_INT(value)) {
			buffer = g_strdup_printf("%d", g_value_get_int(value));
		}
		else if (G_VALUE_HOLDS_STRING(value)) {
			buffer = g_value_get_string(value) == NULL ? g_strdup_printf("") : g_value_dup_string(value);
		}
		else if (G_VALUE_HOLDS_DOUBLE(value)) {
			buffer = g_strdup_printf("%g", g_value_get_double(value));
		}
		else {
			g_warning("widget_populate_hash_with_value: widget is entry with unsupported GValue type %s", G_VALUE_TYPE_NAME(value));
		}

		if (buffer)
			gtk_entry_set_text(GTK_ENTRY(widget), buffer);

		g_free(buffer);
	}

}

static void widget_populate_hash(GtkWidget *widget, XmiMsimGuiUndoManagerWidgetData *widget_data, struct xmi_input *current_input) {

	GValue value = G_VALUE_INIT;
	widget_data->writer(&value, current_input);

	widget_populate_hash_with_value(widget, &value);

	g_value_unset(&value);
}

static void xmi_msim_gui_undo_manager_reset(XmiMsimGuiUndoManager *manager, struct xmi_input *input) {
	g_return_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager));
	g_return_if_fail(input != NULL);

	// start by blocking all signals
	g_hash_table_foreach(manager->widget_table, (GHFunc) widget_block_signals_hash, GINT_TO_POINTER(TRUE));

	// clear undo stack
	g_ptr_array_ref(manager->undo_stack);
	g_ptr_array_free(manager->undo_stack, TRUE);

	// free inputfile and set to NULL
	g_free(manager->inputfile);
	manager->inputfile = NULL;
	manager->current_index = 0;
	xmi_free_input(manager->last_saved_input);
	manager->last_saved_input = NULL;
	
	// add first stack entry
	XmiMsimGuiUndoManagerStackData *stack_data = g_malloc(sizeof(XmiMsimGuiUndoManagerStackData));
	memset(&stack_data->value, 0, sizeof(GValue));
	xmi_copy_input(input, &stack_data->input);
	stack_data->widget = NULL;
	stack_data->message = NULL;
	g_ptr_array_add(manager->undo_stack, stack_data);

	// ensure all entries have default color backgrounds
	g_hash_table_foreach(manager->widget_table, (GHFunc) entry_set_background_hash, GINT_TO_POINTER(TRUE));
	
	// repopulate all widgets with new values
	g_hash_table_foreach(manager->widget_table, (GHFunc) widget_populate_hash, stack_data->input);

	// finish by unblocking all signals
	g_hash_table_foreach(manager->widget_table, (GHFunc) widget_block_signals_hash, GINT_TO_POINTER(FALSE));

	// emit signal
}

void xmi_msim_gui_undo_manager_create_new_file(XmiMsimGuiUndoManager *manager) {
	g_return_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager));
	struct xmi_input *input = xmi_init_empty_input();
	xmi_msim_gui_undo_manager_reset(manager, input);
	xmi_free_input(input);
}

gboolean xmi_msim_gui_undo_manager_load_file(XmiMsimGuiUndoManager *manager, const gchar *filename, GError **error) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager), FALSE);
	g_return_val_if_fail(filename != NULL, FALSE);

	struct xmi_input *input = NULL;

	if (xmi_read_input_xml(filename, &input, error) == 0)
		return FALSE;

	xmi_msim_gui_undo_manager_reset(manager, input);
	manager->last_saved_input = input;
	manager->inputfile = g_strdup(filename);

	return TRUE;
}

void xmi_msim_gui_undo_manager_undo(XmiMsimGuiUndoManager *manager) {
	g_return_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager));
	g_return_if_fail(manager->current_index > 0);

	// to return to previous state, use the input in current_index - 1
	XmiMsimGuiUndoManagerStackData *previous_stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index - 1);
	struct xmi_input *previous_input = previous_stack_data->input;

	// however, the widget is in the current_index
	XmiMsimGuiUndoManagerStackData *current_stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);
	GtkWidget *current_widget = current_stack_data->widget;
	XmiMsimGuiUndoManagerWidgetData *current_widget_data = g_hash_table_lookup(manager->widget_table, current_widget);

	// freeze the signals for the widget
	widget_block_signals_hash(current_widget, current_widget_data, GINT_TO_POINTER(TRUE));
	 
	// populate widget with previous value
	widget_populate_hash(current_widget, current_widget_data, previous_input);

	// unfreeze the signals for the widget
	widget_block_signals_hash(current_widget, current_widget_data, GINT_TO_POINTER(FALSE));

	// update current_index
	manager->current_index--;

	// emit signal for save / save as / undo redo buttons	
}

void xmi_msim_gui_undo_manager_redo(XmiMsimGuiUndoManager *manager) {
	g_return_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager));
	g_return_if_fail(manager->current_index >= manager->undo_stack->len - 1);

	// to return to next state, use the input in current_index + 1
	// TODO: use value in stackdata!!!
	XmiMsimGuiUndoManagerStackData *next_stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index + 1);
	struct xmi_input *next_input = next_stack_data->input;

	// widget of interest is also in current_index + 1
	GtkWidget *next_widget = next_stack_data->widget;
	XmiMsimGuiUndoManagerWidgetData *next_widget_data = g_hash_table_lookup(manager->widget_table, next_widget);

	// freeze the signals for the widget
	widget_block_signals_hash(next_widget, next_widget_data, GINT_TO_POINTER(TRUE));
	 
	// populate widget with previous value
	widget_populate_hash_with_value(next_widget, &next_stack_data->value);

	// unfreeze the signals for the widget
	widget_block_signals_hash(next_widget, next_widget_data, GINT_TO_POINTER(FALSE));

	// update current_index
	manager->current_index++;

	// emit signal for save / save as / undo redo buttons	

}

XmiMsimGuiUndoManagerStatus xmi_msim_gui_undo_manager_get_status(XmiMsimGuiUndoManager *manager) {
	XmiMsimGuiUndoManagerStatus status;
	
	// check if input is valid
	XmiMsimGuiUndoManagerStackData *stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);
	struct xmi_input *current_input = stack_data->input;
	int validate_result = xmi_validate_input(current_input);

	if (manager->inputfile == NULL) {
		if (validate_result == 0) {
			status = XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_VALID; 
		}
		else {
			status = XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_INVALID; 
		}
	}
	else if (xmi_compare_input(manager->last_saved_input, current_input) == 0) {
		status = XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_NO_CHANGES;
	}
	else {
		if (validate_result == 0) {
			status = XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_VALID; 
		}
		else {
			status = XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_INVALID; 
		}
	}

	return status;
}

gboolean xmi_msim_gui_undo_manager_save_file(XmiMsimGuiUndoManager *manager, GError **error) {
	XmiMsimGuiUndoManagerStatus status = xmi_msim_gui_undo_manager_get_status(manager);
	if (status != XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_VALID) {
		g_set_error(error, XMI_MSIM_GUI_UNDO_MANAGER_ERROR, XMI_MSIM_GUI_UNDO_MANAGER_ERROR_FLOW, "xmi_msim_gui_undo_manager_save_file: internal error -> manager status is not XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_VALID. Actual status is %d", status);
		return FALSE;
	}

	// get current struct xmi_input
	XmiMsimGuiUndoManagerStackData *stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);
	struct xmi_input *current_input = stack_data->input;
	if (xmi_write_input_xml(manager->inputfile, current_input, error) == 0)
		return FALSE;

	// update last_saved_input
	xmi_free_input(manager->last_saved_input);
	xmi_copy_input(current_input, &manager->last_saved_input);

	// emit signal

	return TRUE;
}

// this function should only be used if the file has never been saved before
// otherwise a new ApplicationWindow should be opened
gboolean xmi_msim_gui_undo_manager_saveas_file(XmiMsimGuiUndoManager *manager, const gchar *filename, GError **error) {
	XmiMsimGuiUndoManagerStatus status = xmi_msim_gui_undo_manager_get_status(manager);
	if (status != XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_VALID) {
		g_set_error(error, XMI_MSIM_GUI_UNDO_MANAGER_ERROR, XMI_MSIM_GUI_UNDO_MANAGER_ERROR_FLOW, "xmi_msim_gui_undo_manager_save_file: internal error -> manager status is not XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_VALID. Actual status is %d", status);
		return FALSE;
	}

	// get current struct xmi_input
	XmiMsimGuiUndoManagerStackData *stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);
	struct xmi_input *current_input = stack_data->input;
	if (xmi_write_input_xml(filename, current_input, error) == 0)
		return FALSE;

	// update last_saved_input
	xmi_copy_input(current_input, &manager->last_saved_input);
	manager->inputfile = g_strdup(filename);

	// emit signal

	return TRUE;
}

GQuark xmi_msim_gui_undo_manager_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-gui-undo-manager-error-quark");
}

