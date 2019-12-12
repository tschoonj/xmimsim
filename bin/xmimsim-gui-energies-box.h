/*
Copyright (C) 2018-2019 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_MSIM_GUI_ENERGIES_BOX_H
#define XMI_MSIM_GUI_ENERGIES_BOX_H

#include <gtk/gtk.h>
#include "xmi_data_structs.h"

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_ENERGIES_BOX 		       (xmi_msim_gui_energies_box_get_type())
#define XMI_MSIM_GUI_ENERGIES_BOX(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_ENERGIES_BOX, XmiMsimGuiEnergiesBox))
#define XMI_MSIM_GUI_ENERGIES_BOX_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_ENERGIES_BOX, XmiMsimGuiEnergiesBoxClass))
#define XMI_MSIM_GUI_IS_ENERGIES_BOX(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_ENERGIES_BOX))
#define XMI_MSIM_GUI_IS_ENERGIES_BOX_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_ENERGIES_BOX))
#define XMI_MSIM_GUI_ENERGIES_BOX_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_ENERGIES_BOX, XmiMsimGuiEnergiesBoxClass))

typedef struct _XmiMsimGuiEnergiesBox		XmiMsimGuiEnergiesBox;
typedef struct _XmiMsimGuiEnergiesBoxClass   	XmiMsimGuiEnergiesBoxClass;

GtkWidget* xmi_msim_gui_energies_box_new(void);

// this method does not emit a signal!!!
// to be used by X-ray sources dialog and others...
void xmi_msim_gui_energies_box_set_excitation(XmiMsimGuiEnergiesBox *self, xmi_excitation *excitation);

// to be used by the UndoManager
xmi_excitation* xmi_msim_gui_energies_box_get_excitation(XmiMsimGuiEnergiesBox *self);

GType xmi_msim_gui_energies_box_get_type(void) G_GNUC_CONST;

typedef enum {
	XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_DISCRETE,
	XMI_MSIM_GUI_ENERGIES_SINGLE_BOX_TYPE_CONTINUOUS,
} XmiMsimGuiEnergiesSingleBoxType;

typedef enum {
	XMI_MSIM_GUI_ENERGIES_BOX_ERROR_IMPORT_FILE,
} XmiMsimGuiEnergiesBoxError;

#define XMI_MSIM_GUI_ENERGIES_BOX_ERROR (xmi_msim_gui_energies_box_error_quark())
GQuark xmi_msim_gui_energies_box_error_quark(void);

G_END_DECLS

#endif
