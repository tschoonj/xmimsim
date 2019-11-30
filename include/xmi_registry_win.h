/*
Copyright (C) 2010-2019 Tom Schoonjans and Laszlo Vincze

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


#ifndef _XMI_REGISTRY_WIN_H
#define _XMI_REGISTRY_WIN_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32

enum xmi_registry_win {
	XMI_REGISTRY_WIN_DATA,
	XMI_REGISTRY_WIN_SHARE,
	XMI_REGISTRY_WIN_OPENCL_LIB,
	XMI_REGISTRY_WIN_SOURCES,
};

/*
 * Queries windows registry
 * valid values for kind are defined in enum xmi_registry_win
 *
 * Returns 1 on success, 0 on failure
 *
 * On success, regcontents will point to a string containing the requested registry contents. This string should be freed by the caller after usage
 *
 */


int xmi_registry_win_query(int kind, char **regcontents);

#endif


#ifdef __cplusplus
}
#endif

#endif
