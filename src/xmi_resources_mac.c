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

#if !defined(MAC_INTEGRATION) && !defined(QUICKLOOK)
#error xmi_resources_mac.c should not be compiled without defining MAC_INTEGRATION
#endif

#include "config.h"
#include "xmi_resources_mac.h"
#include "xmi_aux.h"
#include <CoreFoundation/CoreFoundation.h>
#include <glib.h>

char* xmi_application_get_bundle_path() {
	char *str = NULL;
	CFBundleRef bundle = CFBundleGetMainBundle();
	if (!bundle)
		return NULL;
	CFURLRef url = CFBundleCopyBundleURL(bundle);
	CFStringRef path = CFURLCopyFileSystemPath(url, kCFURLPOSIXPathStyle);
	CFRelease(url);
	str = g_strdup(CFStringGetCStringPtr(path, kCFStringEncodingUTF8));
	CFRelease(path);
	return str;
}

int xmi_resources_mac_query(int kind, char **resource_file) {
	gchar *bundle_path;
	gchar *temp;

	bundle_path = xmi_application_get_bundle_path();
	if (bundle_path == NULL) {
		fprintf(stderr, "Could not get bundle path!\n");
		return 0;
	}

	switch (kind) {
		case XMI_RESOURCES_MAC_DATA:
			temp = g_strdup_printf("%s/Contents/Resources/xmimsimdata.h5",bundle_path);
			break;
		case XMI_RESOURCES_MAC_XMSO2XMSI:
			temp = g_strdup_printf("%s/Contents/Resources/xmso2xmsi.xml",bundle_path);
			break;
		case XMI_RESOURCES_MAC_XMSO2SVG:
			temp = g_strdup_printf("%s/Contents/Resources/xmso2svg.xml",bundle_path);
			break;
		case XMI_RESOURCES_MAC_XMSO2SPE:
			temp = g_strdup_printf("%s/Contents/Resources/xmso2spe.xml",bundle_path);
			break;
		case XMI_RESOURCES_MAC_XMSO2CSV:
			temp = g_strdup_printf("%s/Contents/Resources/xmso2csv.xml",bundle_path);
			break;
		case XMI_RESOURCES_MAC_XMSO2HTM:
			temp = g_strdup_printf("%s/Contents/Resources/xmso2htm.xml",bundle_path);
			break;
		case XMI_RESOURCES_MAC_XMIMSIM_EXEC:
			temp = g_strdup_printf("%s/Contents/Resources/xmimsim",bundle_path);
			break;
		case XMI_RESOURCES_MAC_OPENCL_CODE:
			temp = g_strdup_printf("%s/Contents/Resources/",bundle_path);
			break;
		case XMI_RESOURCES_MAC_OPENCL_LIB:
			temp = g_strdup_printf("%s/Contents/Resources/",bundle_path);
			break;
		case XMI_RESOURCES_MAC_XMSA2XMSO:
			temp = g_strdup_printf("%s/Contents/Resources/xmsa2xmso.xml",bundle_path);
			break;
		case XMI_RESOURCES_MAC_SOURCES:
			temp = g_strdup_printf("%s/Contents/Resources/sources",bundle_path);
			break;
		case XMI_RESOURCES_MAC_COORDINATE_SYSTEM:
			temp = g_strdup_printf("%s/Contents/Resources/coordinate_system.png",bundle_path);
			break;
		default:
			fprintf(stderr,"Invalid kind in xmi_resources_mac_query\n");
			return 0;

	}
	
	g_free(bundle_path);
	*resource_file = temp;
	return 1;
}
