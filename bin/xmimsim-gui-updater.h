


#ifndef XMIMSIM_GUI_UPDATER_H
#define XMIMSIM_GUI_UPDATER_H

#include <gtk/gtk.h>

enum {
	XMIMSIM_UPDATES_ERROR,
	XMIMSIM_UPDATES_AVAILABLE,
	XMIMSIM_UPDATES_NONE
};

int check_for_updates(char **max_version);

int download_updates(GtkWidget *window, char *max_version);

#endif
