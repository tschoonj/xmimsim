#include <config.h>
#include "xmi_msim.h"
#include <stdio.h>

int main(int argc, char *argv[]) {
	gchar *path = xmi_application_get_resource_path();
	fprintf(stdout, "resource path: %s\n", path);

	return 0;
}
