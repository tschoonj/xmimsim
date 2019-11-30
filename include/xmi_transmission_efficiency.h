/*
Copyright (C) 2017-2019 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_TRANSMISSION_EFFICIENCY_H
#define XMI_TRANSMISSION_EFFICIENCY_H

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

gboolean xmi_transmission_efficiency_read(const char *filename, size_t *nefficiencies, double **energies, double **efficiencies, GError **error);

#ifdef __cplusplus
}
#endif

#endif
