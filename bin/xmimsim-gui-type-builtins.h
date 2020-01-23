/*
Copyright (C) 2016-2020 Tom Schoonjans and Laszlo Vincze

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

#ifndef __XMIMSIM_GUI_TYPE_BUILTINS_H__
#define __XMIMSIM_GUI_TYPE_BUILTINS_H__

#include <glib-object.h>
#include <glib.h>

G_BEGIN_DECLS
GType xmi_msim_gui_compound_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG_TYPE (xmi_msim_gui_compound_dialog_type_get_type())
GType xmi_msim_gui_continuous_energy_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG_TYPE (xmi_msim_gui_continuous_energy_dialog_type_get_type())
GType xmi_msim_gui_discrete_energy_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG_TYPE (xmi_msim_gui_discrete_energy_dialog_type_get_type())
GType xmi_msim_gui_layer_box_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_LAYER_BOX_TYPE (xmi_msim_gui_layer_box_type_get_type())
GType xmi_msim_gui_layer_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_LAYER_DIALOG_TYPE (xmi_msim_gui_layer_dialog_type_get_type())
GType xmi_msim_gui_source_abstract_error_get_type (void);
#define XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT_ERROR_TYPE (xmi_msim_gui_source_abstract_error_get_type())
GType xmi_msim_gui_xmsi_selection_xpath_flags_get_type (void);
#define XMI_MSIM_GUI_TYPE_XMSI_SELECTION_XPATH_FLAGS (xmi_msim_gui_xmsi_selection_xpath_flags_get_type())
GType xmi_msim_gui_batch_multi_selection_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_BATCH_MULTI_SELECTION_TYPE (xmi_msim_gui_batch_multi_selection_type_get_type())

G_END_DECLS
#endif /* __XMIMSIM_GUI_TYPE_BUILTINS_H__ */
