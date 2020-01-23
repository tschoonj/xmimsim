/*
Copyright (C) 2010-2020 Tom Schoonjans and Laszlo Vincze

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


#ifndef _XMI_RESOURCES_MAC_H
#define _XMI_RESOURCES_MAC_H

#ifdef __cplusplus
extern "C" {
#endif

enum xmi_resources_mac {
	XMI_RESOURCES_MAC_DATA,
	XMI_RESOURCES_MAC_XMIMSIM_EXEC,
	XMI_RESOURCES_MAC_OPENCL_LIB,
	XMI_RESOURCES_MAC_METAL_LIB,
	XMI_RESOURCES_MAC_METAL_KERNEL,
	XMI_RESOURCES_MAC_SOURCES,
};

/*
 * Queries App bundle Resources folder
 * valid values for kind are defined in enum xmi_resources_mac
 *
 * Returns 1 on success, 0 on failure
 *
 * On success, resource_file will point to a string containing the requested resource. This string should be freed by the caller after usage
 *
 */
int xmi_resources_mac_query(int kind, char **resource_file);

/*
 * Get resources path
 */
char* xmi_application_get_resource_path(void);

/*
 * Get user data dir
 */
const char* xmi_resources_mac_get_user_data_dir(void);

/*
 * Get user downloads dir
 */
const char* xmi_resources_mac_get_user_downloads_dir(void);

#ifdef __cplusplus
}
#endif

#endif

