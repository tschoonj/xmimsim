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
GType xmi_msim_composition_get_type() G_GNUC_CONST;

#define XMI_MSIM_TYPE_EXCITATION (xmi_msim_excitation_get_type ())
GType xmi_msim_excitation_get_type() G_GNUC_CONST;

G_END_DECLS

#endif
