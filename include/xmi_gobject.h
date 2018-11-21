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


#ifndef XMI_GOBJECT_H
#define XMI_GOBJECT_H

#include <glib-object.h>

G_BEGIN_DECLS

#define XMI_MSIM_TYPE_COMPOSITION (xmi_msim_composition_get_type ())
GType xmi_msim_composition_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_EXCITATION (xmi_msim_excitation_get_type ())
GType xmi_msim_excitation_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_INPUT (xmi_msim_input_get_type ())
GType xmi_msim_input_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_ARCHIVE (xmi_msim_archive_get_type ())
GType xmi_msim_archive_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_MAIN_OPTIONS (xmi_msim_main_options_get_type ())
GType xmi_msim_main_options_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_OUTPUT (xmi_msim_output_get_type ())
GType xmi_msim_output_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_GENERAL (xmi_msim_general_get_type ())
GType xmi_msim_general_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_GEOMETRY (xmi_msim_geometry_get_type ())
GType xmi_msim_geometry_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_ABSORBERS (xmi_msim_absorbers_get_type ())
GType xmi_msim_absorbers_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_DETECTOR (xmi_msim_detector_get_type ())
GType xmi_msim_detector_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_LAYER (xmi_msim_layer_get_type ())
GType xmi_msim_layer_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_ENERGY_DISCRETE (xmi_msim_energy_discrete_get_type ())
GType xmi_msim_energy_discrete_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_ENERGY_CONTINUOUS (xmi_msim_energy_continuous_get_type ())
GType xmi_msim_energy_continuous_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_HISTORY_ELEMENT_LINE (xmi_msim_history_element_line_get_type ())
GType xmi_msim_history_element_line_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_HISTORY_ELEMENT (xmi_msim_history_element_get_type ())
GType xmi_msim_history_element_get_type(void) G_GNUC_CONST;


G_END_DECLS

#endif
