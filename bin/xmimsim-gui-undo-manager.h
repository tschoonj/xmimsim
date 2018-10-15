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


#ifndef XMI_MSIM_GUI_UNDO_MANAGER_H
#define XMI_MSIM_GUI_UNDO_MANAGER_H

#include <gtk/gtk.h>
#include "xmi_data_structs.h"
#include "xmimsim-gui-layer-box.h"
#include "xmimsim-gui-energies-box.h"

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_UNDO_MANAGER 		      (xmi_msim_gui_undo_manager_get_type())
#define XMI_MSIM_GUI_UNDO_MANAGER(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_UNDO_MANAGER, XmiMsimGuiUndoManager))
#define XMI_MSIM_GUI_UNDO_MANAGER_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_UNDO_MANAGER, XmiMsimGuiUndoManagerClass))
#define XMI_MSIM_GUI_IS_UNDO_MANAGER(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_UNDO_MANAGER))
#define XMI_MSIM_GUI_IS_UNDO_MANAGER_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_UNDO_MANAGER))
#define XMI_MSIM_GUI_UNDO_MANAGER_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_UNDO_MANAGER, XmiMsimGuiUndoManagerClass))

typedef struct _XmiMsimGuiUndoManager		XmiMsimGuiUndoManager;
typedef struct _XmiMsimGuiUndoManagerClass   	XmiMsimGuiUndoManagerClass;

// update value with data from input
typedef void (*XmiMsimGuiUndoManagerValueWriter) (GValue *value, const xmi_input *input);
// update input with data from value
typedef void (*XmiMsimGuiUndoManagerValueReader) (const GValue *value, xmi_input *input);

typedef enum {
	XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_VALID,
	XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_INVALID,
	XMI_MSIM_GUI_UNDO_MANAGER_VALUE_VALIDATOR_RESULT_EQUAL,
} XmiMsimGuiUndoManagerValueValidatorResult;

typedef XmiMsimGuiUndoManagerValueValidatorResult (*XmiMsimGuiUndoManagerValueValidator) (GtkWidget *widget, xmi_input *current_input, GValue *value);

XmiMsimGuiUndoManager* xmi_msim_gui_undo_manager_new(void);

void xmi_msim_gui_undo_manager_create_new_file(XmiMsimGuiUndoManager *manager);

gboolean xmi_msim_gui_undo_manager_load_file(XmiMsimGuiUndoManager *manager, const gchar *filename, GError **error);

void xmi_msim_gui_undo_manager_undo(XmiMsimGuiUndoManager *manager);

void xmi_msim_gui_undo_manager_redo(XmiMsimGuiUndoManager *manager);

typedef enum {
	XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_DEFAULT,
	XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_VALID,
	XMI_MSIM_GUI_UNDO_MANAGER_STATUS_NEVER_SAVED_INVALID,
	XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_NO_CHANGES,
	XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_VALID,
	XMI_MSIM_GUI_UNDO_MANAGER_STATUS_SAVED_WITH_CHANGES_INVALID,
} XmiMsimGuiUndoManagerStatus;

XmiMsimGuiUndoManagerStatus xmi_msim_gui_undo_manager_get_status(XmiMsimGuiUndoManager *manager);

const gchar* xmi_msim_gui_undo_manager_get_filename(XmiMsimGuiUndoManager *manager);

const gchar* xmi_msim_gui_undo_manager_get_output_filename(XmiMsimGuiUndoManager *manager);

gboolean xmi_msim_gui_undo_manager_save_file(XmiMsimGuiUndoManager *manager, GError **error);

gboolean xmi_msim_gui_undo_manager_saveas_file(XmiMsimGuiUndoManager *manager, const gchar *filename, GError **error);

xmi_input* xmi_msim_gui_undo_manager_get_current_input(XmiMsimGuiUndoManager *manager);

gboolean xmi_msim_gui_undo_manager_register_entry(
	XmiMsimGuiUndoManager *manager,
	GtkEntry *entry,
	const gchar *message,
	XmiMsimGuiUndoManagerValueWriter writer,
	XmiMsimGuiUndoManagerValueReader reader,
	XmiMsimGuiUndoManagerValueValidator validator
	);

gboolean xmi_msim_gui_undo_manager_register_layer_box(
	XmiMsimGuiUndoManager *manager,
	XmiMsimGuiLayerBox *box
	);

gboolean xmi_msim_gui_undo_manager_register_energies_box(
	XmiMsimGuiUndoManager *manager,
	XmiMsimGuiEnergiesBox *box
	);

gboolean xmi_msim_gui_undo_manager_register_text_view(
	XmiMsimGuiUndoManager *manager,
	GtkTextView *text_view,
	XmiMsimGuiUndoManagerValueWriter writer,
	XmiMsimGuiUndoManagerValueReader reader
	);

gboolean xmi_msim_gui_undo_manager_register_spin_button(
	XmiMsimGuiUndoManager *manager,
	GtkSpinButton *spin_button,
	const gchar *message,
	XmiMsimGuiUndoManagerValueWriter writer,
	XmiMsimGuiUndoManagerValueReader reader
	);

gboolean xmi_msim_gui_undo_manager_register_combo_box_text(
	XmiMsimGuiUndoManager *manager,
	GtkComboBoxText *combo_box_text,
	const gchar *message,
	XmiMsimGuiUndoManagerValueWriter writer,
	XmiMsimGuiUndoManagerValueReader reader
	);

typedef struct _XmiMsimGuiUndoManagerTextViewInsertData XmiMsimGuiUndoManagerTextViewInsertData;
typedef struct _XmiMsimGuiUndoManagerTextViewDeleteData XmiMsimGuiUndoManagerTextViewDeleteData;

#define XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_INSERT_DATA (xmi_msim_gui_undo_manager_text_view_insert_data_get_type())
#define XMI_MSIM_GUI_UNDO_MANAGER_TEXT_VIEW_TYPE_DELETE_DATA (xmi_msim_gui_undo_manager_text_view_delete_data_get_type())

GType xmi_msim_gui_undo_manager_text_view_insert_data_get_type(void) G_GNUC_CONST;
GType xmi_msim_gui_undo_manager_text_view_delete_data_get_type(void) G_GNUC_CONST;

const gchar* xmi_msim_gui_undo_manager_text_view_insert_data_get_all_text(XmiMsimGuiUndoManagerTextViewInsertData *data);
const gchar* xmi_msim_gui_undo_manager_text_view_delete_data_get_all_text(XmiMsimGuiUndoManagerTextViewDeleteData *data);

GType xmi_msim_gui_undo_manager_get_type(void) G_GNUC_CONST;

typedef enum {
	XMI_MSIM_GUI_UNDO_MANAGER_ERROR_FLOW,
} XmiMsimGuiUndoManagerError;

#define XMI_MSIM_GUI_UNDO_MANAGER_ERROR (xmi_msim_gui_undo_manager_error_quark())

GQuark xmi_msim_gui_undo_manager_error_quark(void);

G_END_DECLS

#endif
