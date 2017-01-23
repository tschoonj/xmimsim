/*
Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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

#define TOTALBYTES    8192
#define BYTEINCREMENT 4096

int xmi_registry_win_query(int kind, char **regcontents) {

	LONG RegRV;
	DWORD QueryRV;
	LPTSTR subKey;
	HKEY key;
    	DWORD BufferSize = TOTALBYTES;
        DWORD cbdata;
	PPERF_DATA_BLOCK PerfData;
	GString *stringkey;


	stringkey = g_string_new("Software\\XMI-MSIM\\");
	switch (kind) {
		case XMI_REGISTRY_WIN_DATA:
			g_string_append(stringkey,"data");
			break;
		case XMI_REGISTRY_WIN_SHARE:
			g_string_append(stringkey,"share");
			break;
		case XMI_REGISTRY_WIN_XMSO2XMSI:
			g_string_append(stringkey,"xmso2xmsi");
			break;
		case XMI_REGISTRY_WIN_XMSO2SVG:
			g_string_append(stringkey,"xmso2svg");
			break;
		case XMI_REGISTRY_WIN_XMSO2SPE:
			g_string_append(stringkey,"xmso2spe");
			break;
		case XMI_REGISTRY_WIN_XMSO2CSV:
			g_string_append(stringkey,"xmso2csv");
			break;
		case XMI_REGISTRY_WIN_XMSO2HTM:
			g_string_append(stringkey,"xmso2htm");
			break;
		case XMI_REGISTRY_WIN_OPENCL_CODE:
			g_string_append(stringkey,"openclcode");
			break;
		case XMI_REGISTRY_WIN_OPENCL_LIB:
			g_string_append(stringkey,"opencllib");
			break;
		case XMI_REGISTRY_WIN_XMSA2XMSO:
			g_string_append(stringkey,"xmsa2xmso");
			break;
		case XMI_REGISTRY_WIN_SOURCES:
			g_string_append(stringkey,"sources");
			break;
		case XMI_REGISTRY_WIN_COORDINATE_SYSTEM:
			g_string_append(stringkey,"coordinate-system");
			break;
		case XMI_REGISTRY_WIN_ICONS_DIR:
			g_string_append(stringkey,"icons-dir");
			break;
		default:
			fprintf(stderr,"Invalid kind in xmi_registry_win_query\n");
			return 0;
	}


	//Windows mode: query registry
	RegRV = RegOpenKeyEx(HKEY_LOCAL_MACHINE, TEXT(stringkey->str), 0, KEY_READ,&key);
	if (RegRV != ERROR_SUCCESS) {
		fprintf(stderr,"Error opening key %s in registry\n",stringkey->str);
		return 0;
	}
	PerfData = (PPERF_DATA_BLOCK) g_malloc(BufferSize);
	cbdata = BufferSize;


	QueryRV = RegQueryValueExA(key,TEXT(""), NULL, NULL, (LPBYTE) PerfData,&cbdata);
	while( QueryRV == ERROR_MORE_DATA ) {
	        // Get a buffer that is big enough.
		BufferSize += BYTEINCREMENT;
		PerfData = (PPERF_DATA_BLOCK) g_realloc( PerfData, BufferSize );
		cbdata = BufferSize;
		QueryRV = RegQueryValueExA(key,TEXT(""), NULL, NULL, (LPBYTE) PerfData,&cbdata);
	}
	if (QueryRV != ERROR_SUCCESS) {
		fprintf(stderr,"Error querying key %s in registry\n",stringkey->str);
		return 0;
	}


#if DEBUG == 1
		fprintf(stdout,"KeyValue: %s, bytes %i cbdata %i\n",(char *) PerfData,strlen((char *) PerfData), (int) cbdata);
#endif


	g_string_free(stringkey, TRUE);
	RegCloseKey(key);

	*regcontents = (char *) PerfData;
	return 1;
}
