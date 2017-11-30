
#include <config.h>
#include "xmi_msim.h"
#include <glib.h>
#include <string.h>

int main(int argc, char **argv) {

	gchar *header_version = g_strdup_printf("%d.%d", XMI_MSIM_VERSION_MAJOR, XMI_MSIM_VERSION_MINOR);	
	if (strcmp(header_version, PACKAGE_VERSION) == 0)
		return 0;
	return 1;
}
