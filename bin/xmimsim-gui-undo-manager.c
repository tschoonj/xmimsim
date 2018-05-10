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
#include "xmimsim-gui-marshal.h"
#include "xmi_data_structs.h"
#include "xmi_xml.h"
#include "xmi_gobject.h"
#include <string.h>

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

enum {
	UPDATE_BUTTONS,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

static void xmi_msim_gui_undo_manager_class_init(XmiMsimGuiUndoManagerClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_undo_manager_dispose;
	object_class->finalize = xmi_msim_gui_undo_manager_finalize;
	
	signals[UPDATE_BUTTONS] = g_signal_new(
		"update-status-buttons",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__BOOLEAN_BOOLEAN_STRING_STRING,
		G_TYPE_NONE,
		4,
		G_TYPE_BOOLEAN, // save-as
		G_TYPE_BOOLEAN, // save
		G_TYPE_STRING, // undo
		G_TYPE_STRING  // redo
	);
}

static void manager_emit_update_buttons(XmiMsimGuiUndoManager *manager) {
	gboolean save_as_status = FALSE;
	gboolean save_status = FALSE;
	gchar *undo_status = NULL;
	gchar *redo_status = NULL;

	g_debug("manager_emit_update_buttons: stack len -> %d", manager->undo_stack->len);
	g_debug("manager_emit_update_buttons: current_index -> %d", manager->current_index);

	// first see if all widgets are in a valid state -> if not, turn them all off
	// next save status is determined by validity of current input -> use xmi_msim_gui_undo_manager_get_status
	// undo status is on if current_index > 0
	// redo status is on if current_index < stack_len - 1
	
	GHashTableIter iter;
	XmiMsimGuiUndoManagerWidgetData *value;
	gboolean valid = TRUE;

	g_hash_table_iter_init(&iter, manager->widget_table);
	while (g_hash_table_iter_next(&iter, NULL, (gpointer *) &value)) {
		if (!value->valid) {
			valid = FALSE;
			break;
		}
	}

	if (valid) {
		// undo	
		if (manager->current_index > 0) {
			XmiMsimGuiUndoManagerStackData *stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);
			undo_status = stack_data->message;
		}
		// redo
		if (manager->current_index < manager->undo_stack->len - 1) {
			XmiMsimGuiUndoManagerStackData *stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index + 1);
			redo_status = stack_data->message;
		}
		// save
		XmiMsimGuiUndoManagerStatus status = xmi_msim_gui_undo_manager_get_status(manager);
		if (status == XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_VALID || status == XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_VALID) {
			save_status = save_as_status = TRUE;
		}
		else if (status == XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_NO_CHANGES) {
			save_as_status = TRUE;
		}
	}

	g_signal_emit(manager, signals[UPDATE_BUTTONS], 0, save_as_status, save_status, undo_status, redo_status);
}

static void xmi_msim_gui_undo_manager_init(XmiMsimGuiUndoManager *manager) {
	manager->widget_table = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, (GDestroyNotify) undo_manager_widget_data_free);
	manager->undo_stack = g_ptr_array_new_with_free_func((GDestroyNotify) undo_manager_stack_data_free);
	manager->inputfile = NULL;
	manager->current_index = -1;
	manager->last_saved_input = NULL;
	g_ptr_array_ref(manager->undo_stack);
}

XmiMsimGuiUndoManager* xmi_msim_gui_undo_manager_new(void) {
	return XMI_MSIM_GUI_UNDO_MANAGER(g_object_new(XMI_MSIM_GUI_TYPE_UNDO_MANAGER, NULL));
}

static void widget_set_sensitive_hash_true(GtkWidget *widget, XmiMsimGuiUndoManagerWidgetData *widget_data, gpointer ignored_data) {
	gtk_widget_set_sensitive(widget, TRUE);
}

static void widget_set_sensitive_hash_false(GtkWidget *widget, XmiMsimGuiUndoManagerWidgetData *widget_data, GtkWidget *keep_true_widget) {
	if (widget == keep_true_widget)
		gtk_widget_set_sensitive(widget, TRUE);
	else
		gtk_widget_set_sensitive(widget, FALSE);
}

static void extend_undo_stack(XmiMsimGuiUndoManager *manager, GtkWidget *widget, GValue *value, const gchar *message, struct xmi_input *current_input) {
	g_debug("extend_undo_stack: %s", message);

	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, widget);
	XmiMsimGuiUndoManagerStackData *stack_data = g_malloc(sizeof(XmiMsimGuiUndoManagerStackData));

	memset(&stack_data->value, 0, sizeof(GValue));
	g_value_init(&stack_data->value, G_VALUE_TYPE(value));
	g_value_copy(value, &stack_data->value);
	xmi_copy_input(current_input, &stack_data->input);
	widget_data->reader(value, stack_data->input);
	g_value_unset(value);

	stack_data->widget = widget;
	stack_data->message = g_strdup(message);
	// if in redo mode -> remove everything more recent than current_index
	if (manager->current_index < manager->undo_stack->len - 1)
		g_ptr_array_remove_range(manager->undo_stack, manager->current_index + 1, manager->undo_stack->len - 1 - manager->current_index);
	g_ptr_array_add(manager->undo_stack, stack_data);
	manager->current_index++;

	// emit signal for save / save as / undo redo buttons	
	manager_emit_update_buttons(manager);
}


static void entry_changed_cb(GtkEntry *entry, XmiMsimGuiUndoManager *manager) {
	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, entry);
	g_return_if_fail(widget_data != NULL); // ensure widget_data exists for entry
	g_return_if_fail(manager->undo_stack->len > 0); // ensure undo_stack isnt empty

	XmiMsimGuiUndoManagerStackData *current_stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);

	GValue value = G_VALUE_INIT;
	XmiMsimGuiUndoManagerValueValidatorResult validator_result = widget_data->validator(GTK_WIDGET(entry), current_stack_data->input, &value);

	GtkStyleContext *style_context = gtk_widget_get_style_context(GTK_WIDGET(entry));

	if (validator_result == XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_VALID) {
		if (!widget_data->valid) {
			widget_data->valid = TRUE;
			// foreach widget set sensitive TRUE
			g_hash_table_foreach(manager->widget_table, (GHFunc) widget_set_sensitive_hash_true, NULL);
			// update entry background color
			gtk_style_context_remove_class(style_context, "red");
		}

		// extend undo stack
		extend_undo_stack(manager, GTK_WIDGET(entry), &value, widget_data->message, current_stack_data->input);
	}
	else if (validator_result == XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_INVALID) {
		if (widget_data->valid) {
			widget_data->valid = FALSE;
			// foreach widget set sensitive FALSE
			g_hash_table_foreach(manager->widget_table, (GHFunc) widget_set_sensitive_hash_false, entry);
			// update entry background color
			gtk_style_context_add_class(style_context, "red");
			
			// emit signal for save / save as / undo redo buttons	
			manager_emit_update_buttons(manager);
		}

	}
	else if (validator_result == XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_EQUAL) {
		// take care as to what needs to happen if state was invalid!
		if (!widget_data->valid) {
			widget_data->valid = TRUE;
			// foreach widget set sensitive TRUE
			g_hash_table_foreach(manager->widget_table, (GHFunc) widget_set_sensitive_hash_true, NULL);
			// update entry background color
			gtk_style_context_remove_class(style_context, "red");
			// emit signal for save / save as / undo redo buttons	
			manager_emit_update_buttons(manager);
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
	g_return_val_if_fail(message != NULL, FALSE);

	if (g_hash_table_contains(manager->widget_table, entry)) {
		g_warning("xmi_msim_gui_undo_manager_register_entry: entry already registered by manager");
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

static void layer_box_changed_cb(XmiMsimGuiLayerBox *box, gchar *message, XmiMsimGuiUndoManager *manager) {
	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, box);
	g_return_if_fail(widget_data != NULL); // ensure widget_data exists for box
	g_return_if_fail(manager->undo_stack->len > 0); // ensure undo_stack isnt empty
	g_return_if_fail(message != NULL);

	XmiMsimGuiUndoManagerStackData *current_stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);
	
	GValue value = G_VALUE_INIT;
	g_value_init(&value, XMI_MSIM_TYPE_COMPOSITION);
	g_value_take_boxed(&value, xmi_msim_gui_layer_box_get_composition(box));

	extend_undo_stack(manager, GTK_WIDGET(box), &value, message, current_stack_data->input);
}

static void sample_composition_writer(GValue *value, const struct xmi_input *input) {
	g_value_init(value, XMI_MSIM_TYPE_COMPOSITION);
	g_value_set_boxed(value, input->composition);
}

static void sample_composition_reader(const GValue *value, struct xmi_input *input) {
	xmi_free_composition(input->composition);
	input->composition = g_value_dup_boxed(value);
}

static void excitation_absorbers_composition_writer(GValue *value, const struct xmi_input *input) {
	struct xmi_composition *composition = NULL;
	xmi_copy_abs_or_crystal2composition(input->absorbers->exc_layers, input->absorbers->n_exc_layers, &composition);

	g_value_init(value, XMI_MSIM_TYPE_COMPOSITION);
	g_value_take_boxed(value, composition);
}

static void excitation_absorbers_composition_reader(const GValue *value, struct xmi_input *input) {
	xmi_free_exc_absorbers(input->absorbers);
	struct xmi_composition *composition = g_value_get_boxed(value);
	xmi_copy_composition2abs_or_crystal(composition, &input->absorbers->exc_layers, &input->absorbers->n_exc_layers);
}

static void detector_absorbers_composition_writer(GValue *value, const struct xmi_input *input) {
	struct xmi_composition *composition = NULL;
	xmi_copy_abs_or_crystal2composition(input->absorbers->det_layers, input->absorbers->n_det_layers, &composition);

	g_value_init(value, XMI_MSIM_TYPE_COMPOSITION);
	g_value_take_boxed(value, composition);
}

static void detector_absorbers_composition_reader(const GValue *value, struct xmi_input *input) {
	xmi_free_det_absorbers(input->absorbers);
	struct xmi_composition *composition = g_value_get_boxed(value);
	xmi_copy_composition2abs_or_crystal(composition, &input->absorbers->det_layers, &input->absorbers->n_det_layers);
}

static void crystal_composition_writer(GValue *value, const struct xmi_input *input) {
	struct xmi_composition *composition = NULL;
	xmi_copy_abs_or_crystal2composition(input->detector->crystal_layers, input->detector->n_crystal_layers, &composition);

	g_value_init(value, XMI_MSIM_TYPE_COMPOSITION);
	g_value_take_boxed(value, composition);
}

static void crystal_composition_reader(const GValue *value, struct xmi_input *input) {
	int i;
	if (input->detector->n_crystal_layers > 0) {
		for (i = 0 ; i < input->detector->n_crystal_layers ; i++)
			xmi_free_layer(input->detector->crystal_layers+i);
		g_free(input->detector->crystal_layers);
	}
	struct xmi_composition *composition = g_value_get_boxed(value);
	xmi_copy_composition2abs_or_crystal(composition, &input->detector->crystal_layers, &input->detector->n_crystal_layers);
}

gboolean xmi_msim_gui_undo_manager_register_layer_box(
	XmiMsimGuiUndoManager *manager,
	XmiMsimGuiLayerBox *box
	) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager), FALSE);
	g_return_val_if_fail(XMI_MSIM_GUI_IS_LAYER_BOX(box), FALSE);

	XmiMsimGuiUndoManagerValueWriter writer;
	XmiMsimGuiUndoManagerValueReader reader;

	XmiMsimGuiLayerBoxType box_type = xmi_msim_gui_layer_box_get_layers_type(box);

	switch (box_type) {
		case XMI_MSIM_GUI_LAYER_BOX_TYPE_SAMPLE_COMPOSITION:
			writer = sample_composition_writer;
			reader = sample_composition_reader;
			break;
		case XMI_MSIM_GUI_LAYER_BOX_TYPE_EXCITATION_ABSORBERS:
			writer = excitation_absorbers_composition_writer;
			reader = excitation_absorbers_composition_reader;
			break;
		case XMI_MSIM_GUI_LAYER_BOX_TYPE_DETECTOR_ABSORBERS:
			writer = detector_absorbers_composition_writer;
			reader = detector_absorbers_composition_reader;
			break;
		case XMI_MSIM_GUI_LAYER_BOX_TYPE_CRYSTAL_COMPOSITION:
			writer = crystal_composition_writer;
			reader = crystal_composition_reader;
			break;
		default:
			g_warning("xmi_msim_gui_undo_manager_register_layer_box: unknown box type");
			return FALSE;
	}

	if (g_hash_table_contains(manager->widget_table, box)) {
		g_warning("xmi_msim_gui_undo_manager_register_layer_box: layer_box already registered by manager");
		return FALSE;
	}

	XmiMsimGuiUndoManagerWidgetData *widget_data = g_malloc(sizeof(XmiMsimGuiUndoManagerWidgetData));
	widget_data->message = NULL;
	widget_data->signal_ids = g_array_new(FALSE, FALSE, sizeof(gulong));
	widget_data->valid = TRUE; // don't think I will be using this for layer_box
	widget_data->writer = writer;
	widget_data->reader = reader;

	gulong signal_id = g_signal_connect(box, "changed", G_CALLBACK(layer_box_changed_cb), manager);
	g_array_append_val(widget_data->signal_ids, signal_id);

	g_hash_table_insert(manager->widget_table, box, widget_data);

	return TRUE;
}

static void energies_box_writer(GValue *value, const struct xmi_input *input) {
	g_value_init(value, XMI_MSIM_TYPE_EXCITATION);
	g_value_set_boxed(value, input->excitation);
}

static void energies_box_reader(const GValue *value, struct xmi_input *input) {
	xmi_free_excitation(input->excitation);
	input->excitation = g_value_dup_boxed(value);
}

static void energies_box_changed_cb(XmiMsimGuiEnergiesBox *box, gchar *message, XmiMsimGuiUndoManager *manager) {
	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, box);
	g_return_if_fail(widget_data != NULL); // ensure widget_data exists for box
	g_return_if_fail(manager->undo_stack->len > 0); // ensure undo_stack isnt empty
	g_return_if_fail(message != NULL);

	XmiMsimGuiUndoManagerStackData *current_stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);
	
	GValue value = G_VALUE_INIT;
	g_value_init(&value, XMI_MSIM_TYPE_EXCITATION);
	g_value_take_boxed(&value, xmi_msim_gui_energies_box_get_excitation(box));

	extend_undo_stack(manager, GTK_WIDGET(box), &value, message, current_stack_data->input);
}

gboolean xmi_msim_gui_undo_manager_register_energies_box(
	XmiMsimGuiUndoManager *manager,
	XmiMsimGuiEnergiesBox *box
	) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager), FALSE);
	g_return_val_if_fail(XMI_MSIM_GUI_IS_ENERGIES_BOX(box), FALSE);

	if (g_hash_table_contains(manager->widget_table, box)) {
		g_warning("xmi_msim_gui_undo_manager_register_layer_box: layer_box already registered by manager");
		return FALSE;
	}

	XmiMsimGuiUndoManagerWidgetData *widget_data = g_malloc(sizeof(XmiMsimGuiUndoManagerWidgetData));
	widget_data->message = NULL;
	widget_data->signal_ids = g_array_new(FALSE, FALSE, sizeof(gulong));
	widget_data->valid = TRUE; // don't think I will be using this for layer_box
	widget_data->writer = energies_box_writer;
	widget_data->reader = energies_box_reader;

	gulong signal_id = g_signal_connect(box, "changed", G_CALLBACK(energies_box_changed_cb), manager);
	g_array_append_val(widget_data->signal_ids, signal_id);

	g_hash_table_insert(manager->widget_table, box, widget_data);

	return TRUE;
}

struct _XmiMsimGuiUndoManagerTextViewInsertData {
	int offset;
	gchar *text;
	gchar *all_text; // not sure if this is necessary
	int length;
	gboolean mergeable;
};

static gpointer insert_data_copy(gpointer boxed) {
	XmiMsimGuiUndoManagerTextViewInsertData *insert_data = boxed;
	XmiMsimGuiUndoManagerTextViewInsertData *copy = g_memdup(insert_data, sizeof(XmiMsimGuiUndoManagerTextViewInsertData));
	copy->text = g_strdup(insert_data->text);
	copy->all_text = g_strdup(insert_data->all_text);

	return copy;
}

static void insert_data_free(gpointer boxed) {
	XmiMsimGuiUndoManagerTextViewInsertData *insert_data = boxed;
	g_free(insert_data->text);
	g_free(insert_data->all_text);
	g_free(insert_data);
}

static XmiMsimGuiUndoManagerTextViewInsertData* insert_data_new(GtkTextIter *text_iter, gchar *text, int length) {
	XmiMsimGuiUndoManagerTextViewInsertData *data = g_malloc(sizeof(XmiMsimGuiUndoManagerTextViewInsertData));
	GtkTextBuffer *text_buffer = gtk_text_iter_get_buffer(text_iter);
	data->offset = gtk_text_iter_get_offset(text_iter);
	data->text = g_strdup(text);
	data->all_text = NULL;
	data->length = length;
	if (length > 1)
		data->mergeable = FALSE;
	else
		data->mergeable = TRUE;
	return data;
} 

const gchar* xmi_msim_gui_undo_manager_text_view_insert_data_get_all_text(XmiMsimGuiUndoManagerTextViewInsertData *data) {
	return data->all_text;
}

G_DEFINE_BOXED_TYPE(XmiMsimGuiUndoManagerTextViewInsertData, xmi_msim_gui_undo_manager_text_view_insert_data, insert_data_copy, insert_data_free);

struct _XmiMsimGuiUndoManagerTextViewDeleteData {
	gchar *text;
	gchar *all_text; // not sure if this is necessary
	int start;
	int end;
	gboolean delete_key_used;
	gboolean mergeable;
};

static gpointer delete_data_copy(gpointer boxed) {
	XmiMsimGuiUndoManagerTextViewDeleteData *delete_data = boxed;
	XmiMsimGuiUndoManagerTextViewDeleteData *copy = g_memdup(delete_data, sizeof(XmiMsimGuiUndoManagerTextViewDeleteData));
	copy->text = g_strdup(delete_data->text);
	copy->all_text = g_strdup(delete_data->all_text);

	return copy;
}

static void delete_data_free(gpointer boxed) {
	XmiMsimGuiUndoManagerTextViewDeleteData *delete_data = boxed;
	g_free(delete_data->text);
	g_free(delete_data->all_text);
	g_free(delete_data);
}

static XmiMsimGuiUndoManagerTextViewDeleteData* delete_data_new(GtkTextBuffer *text_buffer, GtkTextIter *start_iter, GtkTextIter *end_iter) {
	XmiMsimGuiUndoManagerTextViewDeleteData *data = g_malloc(sizeof(XmiMsimGuiUndoManagerTextViewDeleteData));
	data->text = gtk_text_buffer_get_text(text_buffer, start_iter, end_iter, TRUE);
	data->all_text = NULL;
	data->start = gtk_text_iter_get_offset(start_iter);
	data->end = gtk_text_iter_get_offset(end_iter);

	GtkTextIter insert_iter;
	gtk_text_buffer_get_iter_at_mark(text_buffer, &insert_iter, gtk_text_buffer_get_insert(text_buffer));

	if (gtk_text_iter_get_offset(&insert_iter) <= data->start)
		data->delete_key_used = TRUE;
	else
		data->delete_key_used = FALSE;
	
	if ((data->end - data->start > 1) || (strlen(data->text) == 1 && strchr("\r\n ", data->text[0]) != NULL))
		data->mergeable = FALSE;
	else
		data->mergeable = TRUE;

	return data;
}

const gchar* xmi_msim_gui_undo_manager_text_view_delete_data_get_all_text(XmiMsimGuiUndoManagerTextViewDeleteData *data) {
	return data->all_text;
}

G_DEFINE_BOXED_TYPE(XmiMsimGuiUndoManagerTextViewDeleteData, xmi_msim_gui_undo_manager_text_view_delete_data, delete_data_copy, delete_data_free);

#define WHITESPACE "\r\n\t "

static gboolean insert_text_can_be_merged(XmiMsimGuiUndoManagerTextViewInsertData *prev, XmiMsimGuiUndoManagerTextViewInsertData *cur) {
	if (!cur->mergeable || !prev->mergeable)
		return FALSE;
	else if (cur->offset != (prev->offset + prev->length))
		return FALSE;
	else if ((strlen(cur->text) == 1 && strchr(WHITESPACE, cur->text[0]) != NULL) && !(strlen(prev->text) == 1 && strchr(WHITESPACE, prev->text[0]) != NULL))
		return FALSE;
	else if ((strlen(prev->text) == 1 && strchr(WHITESPACE, prev->text[0]) != NULL) && !(strlen(cur->text) == 1 && strchr(WHITESPACE, cur->text[0]) != NULL))
		return FALSE;
	return TRUE;
}

static void text_buffer_insert_text_cb(GtkTextBuffer *textbuffer, GtkTextIter *text_iter, gchar *text, gint length, XmiMsimGuiUndoManager *manager) {
	GtkTextView *text_view = g_object_get_data(G_OBJECT(textbuffer), "text-view");
	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, text_view);
	g_return_if_fail(widget_data != NULL); // ensure widget_data exists for box
	g_return_if_fail(manager->undo_stack->len > 0); // ensure undo_stack isnt empty

	XmiMsimGuiUndoManagerStackData *current_stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);

	XmiMsimGuiUndoManagerTextViewInsertData *insert_data = insert_data_new(text_iter, text, length);
	GValue value = G_VALUE_INIT;
	g_value_init(&value, XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_INSERT_DATA);
	g_value_take_boxed(&value, insert_data);

	if (manager->current_index == 0 || !(
			current_stack_data->widget == GTK_WIDGET(text_view) && 
			G_VALUE_HOLDS(&current_stack_data->value, XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_INSERT_DATA) &&
			insert_text_can_be_merged(g_value_get_boxed(&current_stack_data->value), insert_data))) {
		XmiMsimGuiUndoManagerStackData *stack_data = g_malloc(sizeof(XmiMsimGuiUndoManagerStackData));

		memset(&stack_data->value, 0, sizeof(GValue));
		g_value_init(&stack_data->value, G_VALUE_TYPE(&value));
		g_value_copy(&value, &stack_data->value);
		xmi_copy_input(current_stack_data->input, &stack_data->input);
		g_value_unset(&value);

		stack_data->widget = GTK_WIDGET(text_view);
		stack_data->message = g_strdup("text inserted");
		// if in redo mode -> remove everything more recent than current_index
		if (manager->current_index < manager->undo_stack->len - 1)
			g_ptr_array_remove_range(manager->undo_stack, manager->current_index + 1, manager->undo_stack->len - 1 - manager->current_index);
		g_ptr_array_add(manager->undo_stack, stack_data);
		manager->current_index++;
	}
	else {
		// do not extend undo stack -> modify current instead
		XmiMsimGuiUndoManagerTextViewInsertData *current_insert_data = g_value_get_boxed(&current_stack_data->value);
		current_insert_data->length += insert_data->length;
		gchar *temp = current_insert_data->text;
		current_insert_data->text = g_strdup_printf("%s%s", temp, insert_data->text);
		g_free(temp);
		g_value_unset(&value);

		// if in redo mode -> remove everything more recent than current_index
		if (manager->current_index < manager->undo_stack->len - 1)
			g_ptr_array_remove_range(manager->undo_stack, manager->current_index + 1, manager->undo_stack->len - 1 - manager->current_index);

	}
}

static void text_buffer_insert_text_after_cb(GtkTextBuffer *textbuffer, GtkTextIter *text_iter, gchar *text, gint length, XmiMsimGuiUndoManager *manager) {
	GtkTextView *text_view = g_object_get_data(G_OBJECT(textbuffer), "text-view");
	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, text_view);
	g_return_if_fail(widget_data != NULL); // ensure widget_data exists for box
	g_return_if_fail(manager->undo_stack->len > 0); // ensure undo_stack isnt empty

	// get the last stack data
	XmiMsimGuiUndoManagerStackData *current_stack_data = g_ptr_array_index(manager->undo_stack, manager->undo_stack->len - 1);
	XmiMsimGuiUndoManagerTextViewInsertData *current_insert_data = g_value_get_boxed(&current_stack_data->value);
	
	GtkTextIter start_iter, end_iter;
	gtk_text_buffer_get_bounds(textbuffer, &start_iter, &end_iter);
	current_insert_data->all_text = gtk_text_buffer_get_text(textbuffer, &start_iter, &end_iter, TRUE);

	widget_data->reader(&current_stack_data->value, current_stack_data->input);

	manager_emit_update_buttons(manager);
}

#undef WHITESPACE
#define WHITESPACE "\t "

static gboolean delete_text_can_be_merged(XmiMsimGuiUndoManagerTextViewDeleteData *prev, XmiMsimGuiUndoManagerTextViewDeleteData *cur) {
	if (!cur->mergeable || !prev->mergeable)
		return FALSE;
	else if (prev->delete_key_used != cur->delete_key_used)
		return FALSE;
	else if (prev->start != cur->start && prev->start != cur->end)
		return FALSE;
	else if (!(strlen(cur->text) == 1 && strchr(WHITESPACE, cur->text[0]) != NULL) && (strlen(prev->text) == 1 && strchr(WHITESPACE, prev->text[0]) != NULL))
		return FALSE;
	else if ((strlen(cur->text) == 1 && strchr(WHITESPACE, cur->text[0]) != NULL) && !(strlen(prev->text) == 1 && strchr(WHITESPACE, prev->text[0]) != NULL))
		return FALSE;
	return TRUE;
}

static void text_buffer_delete_range_cb(GtkTextBuffer *textbuffer, GtkTextIter *start, GtkTextIter *end, XmiMsimGuiUndoManager *manager) {
	GtkTextView *text_view = g_object_get_data(G_OBJECT(textbuffer), "text-view");
	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, text_view);
	g_return_if_fail(widget_data != NULL); // ensure widget_data exists for box
	g_return_if_fail(manager->undo_stack->len > 0); // ensure undo_stack isnt empty

	XmiMsimGuiUndoManagerStackData *current_stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);

	XmiMsimGuiUndoManagerTextViewDeleteData *delete_data = delete_data_new(textbuffer, start, end);
	GValue value = G_VALUE_INIT;
	g_value_init(&value, XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_DELETE_DATA);
	g_value_take_boxed(&value, delete_data);

	if (manager->current_index == 0 || !(
			current_stack_data->widget == GTK_WIDGET(text_view) && 
			G_VALUE_HOLDS(&current_stack_data->value, XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_DELETE_DATA) &&
			delete_text_can_be_merged(g_value_get_boxed(&current_stack_data->value), delete_data))) {
		XmiMsimGuiUndoManagerStackData *stack_data = g_malloc(sizeof(XmiMsimGuiUndoManagerStackData));

		memset(&stack_data->value, 0, sizeof(GValue));
		g_value_init(&stack_data->value, G_VALUE_TYPE(&value));
		g_value_copy(&value, &stack_data->value);
		xmi_copy_input(current_stack_data->input, &stack_data->input);
		g_value_unset(&value);

		stack_data->widget = GTK_WIDGET(text_view);
		stack_data->message = g_strdup("text delete");
		// if in redo mode -> remove everything more recent than current_index
		if (manager->current_index < manager->undo_stack->len - 1)
			g_ptr_array_remove_range(manager->undo_stack, manager->current_index + 1, manager->undo_stack->len - 1 - manager->current_index);
		g_ptr_array_add(manager->undo_stack, stack_data);
		manager->current_index++;
	}
	else {
		// do not extend undo stack -> modify current instead
		XmiMsimGuiUndoManagerTextViewDeleteData *current_delete_data = g_value_get_boxed(&current_stack_data->value);
		if (current_delete_data->start == delete_data->start) { // delete key used
			gchar *temp = current_delete_data->text;
			current_delete_data->text = g_strdup_printf("%s%s", temp, delete_data->text);
			g_free(temp);
			current_delete_data->end += (delete_data->end - delete_data->start);
		}
		else { // backspace used
			gchar *temp = current_delete_data->text;
			current_delete_data->text = g_strdup_printf("%s%s", delete_data->text, temp);
			g_free(temp);
			current_delete_data->start = delete_data->start;
		}
		g_value_unset(&value);

		// if in redo mode -> remove everything more recent than current_index
		if (manager->current_index < manager->undo_stack->len - 1)
			g_ptr_array_remove_range(manager->undo_stack, manager->current_index + 1, manager->undo_stack->len - 1 - manager->current_index);

	}
}

static void text_buffer_delete_range_after_cb(GtkTextBuffer *textbuffer, GtkTextIter *start, GtkTextIter *end, XmiMsimGuiUndoManager *manager) {
	GtkTextView *text_view = g_object_get_data(G_OBJECT(textbuffer), "text-view");
	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, text_view);
	g_return_if_fail(widget_data != NULL); // ensure widget_data exists for box
	g_return_if_fail(manager->undo_stack->len > 0); // ensure undo_stack isnt empty

	// get the last stack data
	XmiMsimGuiUndoManagerStackData *current_stack_data = g_ptr_array_index(manager->undo_stack, manager->undo_stack->len - 1);
	XmiMsimGuiUndoManagerTextViewDeleteData *current_delete_data = g_value_get_boxed(&current_stack_data->value);

	GtkTextIter start_iter, end_iter;
	gtk_text_buffer_get_bounds(textbuffer, &start_iter, &end_iter);
	current_delete_data->all_text = gtk_text_buffer_get_text(textbuffer, &start_iter, &end_iter, TRUE);

	widget_data->reader(&current_stack_data->value, current_stack_data->input);

	manager_emit_update_buttons(manager);
}

gboolean xmi_msim_gui_undo_manager_register_text_view(
	XmiMsimGuiUndoManager *manager,
	GtkTextView *text_view,
	XmiMsimGuiUndoManagerValueWriter writer,
	XmiMsimGuiUndoManagerValueReader reader
	) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager), FALSE);
	g_return_val_if_fail(GTK_IS_TEXT_VIEW(text_view), FALSE);

	if (g_hash_table_contains(manager->widget_table, text_view)) {
		g_warning("xmi_msim_gui_undo_manager_register_text_view: text_view already registered by manager");
		return FALSE;
	}

	XmiMsimGuiUndoManagerWidgetData *widget_data = g_malloc(sizeof(XmiMsimGuiUndoManagerWidgetData));
	widget_data->message = NULL;
	widget_data->signal_ids = g_array_new(FALSE, FALSE, sizeof(gulong));
	widget_data->valid = TRUE; // unused
	widget_data->writer = writer;
	widget_data->reader = reader;

	GtkTextBuffer *buffer = gtk_text_view_get_buffer(text_view);
	g_object_set_data(G_OBJECT(buffer), "text-view", text_view); // ensure buffer has access to the text_view it belongs to

	gulong signal_insert_id = g_signal_connect(buffer, "insert-text", G_CALLBACK(text_buffer_insert_text_cb), manager);
	g_array_append_val(widget_data->signal_ids, signal_insert_id);
	gulong signal_insert_after_id = g_signal_connect_after(buffer, "insert-text", G_CALLBACK(text_buffer_insert_text_after_cb), manager);
	g_array_append_val(widget_data->signal_ids, signal_insert_after_id);
	gulong signal_delete_id = g_signal_connect(buffer, "delete-range", G_CALLBACK(text_buffer_delete_range_cb), manager);
	g_array_append_val(widget_data->signal_ids, signal_delete_id);
	gulong signal_delete_after_id = g_signal_connect_after(buffer, "delete-range", G_CALLBACK(text_buffer_delete_range_after_cb), manager);
	g_array_append_val(widget_data->signal_ids, signal_delete_after_id);

	g_hash_table_insert(manager->widget_table, text_view, widget_data);

	return TRUE;
}

static void spin_button_value_changed_cb(GtkSpinButton *spin_button, XmiMsimGuiUndoManager *manager) {
	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, spin_button);
	g_return_if_fail(widget_data != NULL); // ensure widget_data exists for box
	g_return_if_fail(manager->undo_stack->len > 0); // ensure undo_stack isnt empty

	XmiMsimGuiUndoManagerStackData *current_stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);

	GValue value = G_VALUE_INIT;
	g_value_init(&value, G_TYPE_DOUBLE);
	g_value_set_double(&value, gtk_spin_button_get_value(spin_button));

	extend_undo_stack(manager, GTK_WIDGET(spin_button), &value, widget_data->message, current_stack_data->input);
}

gboolean xmi_msim_gui_undo_manager_register_spin_button(
	XmiMsimGuiUndoManager *manager,
	GtkSpinButton *spin_button,
	const gchar *message,
	XmiMsimGuiUndoManagerValueWriter writer,
	XmiMsimGuiUndoManagerValueReader reader
	) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager), FALSE);
	g_return_val_if_fail(GTK_IS_SPIN_BUTTON(spin_button), FALSE);
	g_return_val_if_fail(message != NULL, FALSE);

	if (g_hash_table_contains(manager->widget_table, spin_button)) {
		g_warning("xmi_msim_gui_undo_manager_register_spin_button: spin_button already registered by manager");
		return FALSE;
	}

	XmiMsimGuiUndoManagerWidgetData *widget_data = g_malloc(sizeof(XmiMsimGuiUndoManagerWidgetData));
	widget_data->message = g_strdup(message);
	widget_data->signal_ids = g_array_new(FALSE, FALSE, sizeof(gulong));
	widget_data->valid = TRUE; // unused
	widget_data->writer = writer;
	widget_data->reader = reader;

	gulong signal_id = g_signal_connect(spin_button, "value-changed", G_CALLBACK(spin_button_value_changed_cb), manager);
	g_array_append_val(widget_data->signal_ids, signal_id);

	g_hash_table_insert(manager->widget_table, spin_button, widget_data);

	return TRUE;
}

static void combo_box_text_changed_cb(GtkComboBoxText *combo_box_text, XmiMsimGuiUndoManager *manager) {
	XmiMsimGuiUndoManagerWidgetData *widget_data = g_hash_table_lookup(manager->widget_table, combo_box_text);
	g_return_if_fail(widget_data != NULL); // ensure widget_data exists for box
	g_return_if_fail(manager->undo_stack->len > 0); // ensure undo_stack isnt empty

	XmiMsimGuiUndoManagerStackData *current_stack_data = g_ptr_array_index(manager->undo_stack, manager->current_index);

	GValue value = G_VALUE_INIT;
	g_value_init(&value, G_TYPE_INT);
	g_value_set_int(&value, gtk_combo_box_get_active(GTK_COMBO_BOX(combo_box_text)));

	extend_undo_stack(manager, GTK_WIDGET(combo_box_text), &value, widget_data->message, current_stack_data->input);
}

gboolean xmi_msim_gui_undo_manager_register_combo_box_text(
	XmiMsimGuiUndoManager *manager,
	GtkComboBoxText *combo_box_text,
	const gchar *message,
	XmiMsimGuiUndoManagerValueWriter writer,
	XmiMsimGuiUndoManagerValueReader reader
	) {
	g_return_val_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager), FALSE);
	g_return_val_if_fail(GTK_IS_COMBO_BOX_TEXT(combo_box_text), FALSE);
	g_return_val_if_fail(message != NULL, FALSE);

	if (g_hash_table_contains(manager->widget_table, combo_box_text)) {
		g_warning("xmi_msim_gui_undo_manager_register_combo_box_text: combo_box_text already registered by manager");
		return FALSE;
	}

	XmiMsimGuiUndoManagerWidgetData *widget_data = g_malloc(sizeof(XmiMsimGuiUndoManagerWidgetData));
	widget_data->message = g_strdup(message);
	widget_data->signal_ids = g_array_new(FALSE, FALSE, sizeof(gulong));
	widget_data->valid = TRUE; // unused
	widget_data->writer = writer;
	widget_data->reader = reader;

	gulong signal_id = g_signal_connect(combo_box_text, "changed", G_CALLBACK(combo_box_text_changed_cb), manager);
	g_array_append_val(widget_data->signal_ids, signal_id);

	g_hash_table_insert(manager->widget_table, combo_box_text, widget_data);

	return TRUE;

}

static void widget_block_signals_hash(GtkWidget *widget, XmiMsimGuiUndoManagerWidgetData *widget_data, gpointer data) {
	gboolean block = GPOINTER_TO_INT(data);
	unsigned int i;
	GObject *object = G_OBJECT(widget);
	if (GTK_IS_TEXT_VIEW(widget))
		object = G_OBJECT(gtk_text_view_get_buffer(GTK_TEXT_VIEW(widget)));


	for (i = 0 ; i < widget_data->signal_ids->len ; i++) {
		if (block)
			g_signal_handler_block(object, g_array_index(widget_data->signal_ids, gulong, i));
		else
			g_signal_handler_unblock(object, g_array_index(widget_data->signal_ids, gulong, i));
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
		else if (G_VALUE_HOLDS_LONG(value)) {
			buffer = g_strdup_printf("%ld", g_value_get_long(value));
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
	else if (XMI_MSIM_GUI_IS_LAYER_BOX(widget)) {
		if (G_VALUE_HOLDS(value, XMI_MSIM_TYPE_COMPOSITION)){
			xmi_msim_gui_layer_box_set_composition(XMI_MSIM_GUI_LAYER_BOX(widget), g_value_get_boxed(value));
		}
		else {
			g_warning("widget_populate_hash_with_value: widget is layer_box with unsupported GValue type %s", G_VALUE_TYPE_NAME(value));
		}
	}
	else if (XMI_MSIM_GUI_IS_ENERGIES_BOX(widget)) {
		if (G_VALUE_HOLDS(value, XMI_MSIM_TYPE_EXCITATION)){
			xmi_msim_gui_energies_box_set_excitation(XMI_MSIM_GUI_ENERGIES_BOX(widget), g_value_get_boxed(value));
		}
		else {
			g_warning("widget_populate_hash_with_value: widget is energies_box with unsupported GValue type %s", G_VALUE_TYPE_NAME(value));
		}
	}
	else if (GTK_IS_TEXT_VIEW(widget)) {
		GtkTextBuffer *text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(widget));
		
		if (G_VALUE_HOLDS(value, XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_INSERT_DATA)) {
			XmiMsimGuiUndoManagerTextViewInsertData *data = g_value_get_boxed(value);
			gtk_text_buffer_set_text(text_buffer, data->all_text, -1);
		}
		else if (G_VALUE_HOLDS(value, XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_DELETE_DATA)) {
			XmiMsimGuiUndoManagerTextViewDeleteData *data = g_value_get_boxed(value);
			gtk_text_buffer_set_text(text_buffer, data->all_text, -1);
		}
		else if (G_VALUE_HOLDS_STRING(value)) {
			gtk_text_buffer_set_text(text_buffer, g_value_get_string(value), -1);
		}
		else {
			g_warning("widget_populate_hash_with_value: widget is text_view with unsupported GValue type %s", G_VALUE_TYPE_NAME(value));
		}
	}
	else if (GTK_IS_SPIN_BUTTON(widget)) {
		if (G_VALUE_HOLDS_DOUBLE(value)) {
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget), g_value_get_double(value));
		}
		else {
			g_warning("widget_populate_hash_with_value: widget is spin button with unsupported GValue type %s", G_VALUE_TYPE_NAME(value));
		}
	}
	else if (GTK_IS_COMBO_BOX_TEXT(widget)) {
		if (G_VALUE_HOLDS_INT(value)) {
			gtk_combo_box_set_active(GTK_COMBO_BOX(widget), g_value_get_int(value));
		}
		else {
			g_warning("widget_populate_hash_with_value: widget is gtk_combo_box_text with unsupported GValue type %s", G_VALUE_TYPE_NAME(value));
		}
	}
	else {
		g_warning("widget_populate_hash_with_value: unsupported widget type %s", G_OBJECT_TYPE_NAME(widget));
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

}

void xmi_msim_gui_undo_manager_create_new_file(XmiMsimGuiUndoManager *manager) {
	g_return_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager));
	struct xmi_input *input = xmi_init_empty_input();
	xmi_msim_gui_undo_manager_reset(manager, input);
	xmi_free_input(input);
	// emit signal
	manager_emit_update_buttons(manager);
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
	// emit signal
	manager_emit_update_buttons(manager);

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
	manager_emit_update_buttons(manager);
}

void xmi_msim_gui_undo_manager_redo(XmiMsimGuiUndoManager *manager) {
	g_return_if_fail(XMI_MSIM_GUI_IS_UNDO_MANAGER(manager));
	g_return_if_fail(manager->current_index <= manager->undo_stack->len - 1);

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
	manager_emit_update_buttons(manager);
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
	else if (manager->last_saved_input != NULL && xmi_compare_input(manager->last_saved_input, current_input) == 0) {
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
	manager_emit_update_buttons(manager);

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
	manager_emit_update_buttons(manager);

	return TRUE;
}

const gchar* xmi_msim_gui_undo_manager_get_filename(XmiMsimGuiUndoManager *manager) {
	XmiMsimGuiUndoManagerStatus status = xmi_msim_gui_undo_manager_get_status(manager);

	gchar* rv = NULL;

	switch (status) {
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_VALID:
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_INVALID:
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_VALID:
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_INVALID:
			break;
		case XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_NO_CHANGES:
			rv = manager->inputfile;
	}
	return rv;
}

GQuark xmi_msim_gui_undo_manager_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-gui-undo-manager-error-quark");
}

