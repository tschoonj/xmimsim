/*
Copyright (C) 2010-2012 Tom Schoonjans and Laszlo Vincze

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



#ifndef XMIMSIM_GUI_UPDATER_H
#define XMIMSIM_GUI_UPDATER_H

#include <gtk/gtk.h>

enum {
	XMIMSIM_UPDATES_ERROR,
	XMIMSIM_UPDATES_AVAILABLE,
	XMIMSIM_UPDATES_NONE
};



int check_for_updates(char **max_version, char **message);

int download_updates(GtkWidget *window, char *max_version, char *message);

#endif
