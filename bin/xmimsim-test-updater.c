#include "xmimsim-gui-updater.h"
#include <glib-object.h>
#include <stdio.h>


int main(int argc, char *argv[]) {

	g_type_init();
	char *version;

	/*fprintf(stdout,"%i\n",check_for_updates(&version));
	fprintf(stdout,"%s\n",version);
*/

	download_updates(NULL,"1.0");

	return 0;

}
