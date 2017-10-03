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

#if !defined(MAC_INTEGRATION)
#error xmi_resources_mac.c should not be compiled without defining MAC_INTEGRATION
#endif

#include "config.h"
#include "xmi_resources_mac.h"
#include <Foundation/Foundation.h>
#include <glib.h>

char* xmi_application_get_resource_path() {
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	char *str = NULL;
	NSString *path = [[NSBundle mainBundle] resourcePath];
	if (!path)
		return NULL;
	str = g_strdup([path UTF8String]);
	[pool release];
	return str;
}

int xmi_resources_mac_query(int kind, char **resource_file) {
	gchar *resource_path;
	gchar *temp;

	resource_path = xmi_application_get_resource_path();
	if (resource_path == NULL) {
		fprintf(stderr, "Could not get bundle path!\n");
		return 0;
	}

	switch (kind) {
		case XMI_RESOURCES_MAC_DATA:
			temp = g_strdup_printf("%s/xmimsimdata.h5",resource_path);
			break;
		case XMI_RESOURCES_MAC_XMSO2XMSI:
			temp = g_strdup_printf("%s/xmso2xmsi.xml",resource_path);
			break;
		case XMI_RESOURCES_MAC_XMSO2SVG:
			temp = g_strdup_printf("%s/xmso2svg.xml",resource_path);
			break;
		case XMI_RESOURCES_MAC_XMSO2SPE:
			temp = g_strdup_printf("%s/xmso2spe.xml",resource_path);
			break;
		case XMI_RESOURCES_MAC_XMSO2CSV:
			temp = g_strdup_printf("%s/xmso2csv.xml",resource_path);
			break;
		case XMI_RESOURCES_MAC_XMSO2HTM:
			temp = g_strdup_printf("%s/xmso2htm.xml",resource_path);
			break;
		case XMI_RESOURCES_MAC_XMIMSIM_EXEC:
			temp = g_strdup_printf("%s/xmimsim",resource_path);
			break;
		case XMI_RESOURCES_MAC_OPENCL_CODE:
			temp = g_strdup_printf("%s/",resource_path);
			break;
		case XMI_RESOURCES_MAC_OPENCL_LIB:
			temp = g_strdup_printf("%s/",resource_path);
			break;
		case XMI_RESOURCES_MAC_XMSA2XMSO:
			temp = g_strdup_printf("%s/xmsa2xmso.xml",resource_path);
			break;
		case XMI_RESOURCES_MAC_SOURCES:
			temp = g_strdup_printf("%s/sources",resource_path);
			break;
		case XMI_RESOURCES_MAC_COORDINATE_SYSTEM:
			temp = g_strdup_printf("%s/coordinate_system.png",resource_path);
			break;
		default:
			fprintf(stderr,"Invalid kind in xmi_resources_mac_query\n");
			return 0;

	}
	
	g_free(resource_path);
	*resource_file = temp;
	return 1;
}

const char *xmi_resources_mac_get_user_data_dir() {
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

	NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask,TRUE);
	NSString *documentsDirectory = [paths objectAtIndex:0];
	const char *rv = [documentsDirectory cStringUsingEncoding:NSUTF8StringEncoding];

	[pool drain];

	return rv;
}
