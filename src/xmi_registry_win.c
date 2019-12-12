/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

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


#ifndef _WIN32
#error _WIN32 macro is not defined in xmi_registry_win.c
#endif

#include "config.h"
#include "xmi_registry_win.h"
#include <windows.h>
#include <stdio.h>
#include <glib.h>

int xmi_registry_win_query(int kind, char **regcontents) {

	LONG RegRV;
	DWORD QueryRV;
	LPTSTR subKey;
	HKEY key;
	GString *stringkey;


	stringkey = g_string_new("Software\\XMI-MSIM\\");
	switch (kind) {
		case XMI_REGISTRY_WIN_DATA:
			g_string_append(stringkey,"data");
			break;
		case XMI_REGISTRY_WIN_SHARE:
			g_string_append(stringkey,"share");
			break;
		case XMI_REGISTRY_WIN_OPENCL_LIB:
			g_string_append(stringkey,"opencllib");
			break;
		case XMI_REGISTRY_WIN_SOURCES:
			g_string_append(stringkey,"sources");
			break;
		default:
			fprintf(stderr,"Invalid kind in xmi_registry_win_query\n");
			return 0;
	}


	//Windows mode: query registry
	gunichar2 *tmp_unichar2 = g_utf8_to_utf16(stringkey->str, -1, NULL, NULL, NULL);
	RegRV = RegOpenKeyExW(HKEY_LOCAL_MACHINE, tmp_unichar2, 0, KEY_READ, &key);
	g_free(tmp_unichar2);
	if (RegRV != ERROR_SUCCESS) {
		fprintf(stderr, "Error opening key %s in registry\n", stringkey->str);
		g_string_free(stringkey, TRUE);
		return 0;
	}

	DWORD data_size = 0;
	QueryRV = RegQueryValueExW(key, NULL, NULL, NULL, NULL, &data_size);
	if (data_size <= 0 || QueryRV != ERROR_SUCCESS) {
		fprintf(stderr, "Could not determine value size for key %s in registry\n", stringkey->str);
		RegCloseKey(key);
		g_string_free(stringkey, TRUE);
		return 0;
	}

	tmp_unichar2 = g_malloc(data_size);
	QueryRV = RegQueryValueExW(key, NULL, NULL, NULL, (LPBYTE) tmp_unichar2, &data_size);
	if (QueryRV != ERROR_SUCCESS) {
		fprintf(stderr, "Error querying key %s in registry\n", stringkey->str);
		RegCloseKey(key);
		g_string_free(stringkey, TRUE);
		g_free(tmp_unichar2);
		return 0;
	}


	g_string_free(stringkey, TRUE);
	RegCloseKey(key);

	*regcontents = g_utf16_to_utf8(tmp_unichar2, -1, NULL, NULL, NULL);
	g_free(tmp_unichar2);
	return 1;
}
