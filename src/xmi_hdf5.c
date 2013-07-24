/*
Copyright (C) 2010-2013 Tom Schoonjans and Laszlo Vincze

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

#include "config.h"
#include "xmi_hdf5.h"
#include <glib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <glib/gprintf.h>
#include <glib/gstdio.h>

#ifdef MAC_INTEGRATION
  #import <Foundation/Foundation.h>
  #include "xmi_resources_mac.h"
#endif

#ifdef G_OS_WIN32
  #include "xmi_registry_win.h"
#endif


int xmi_get_hdf5_data_file(char **hdf5_filePtr) {

	char *hdf5_file = *hdf5_filePtr;

	if (hdf5_file == NULL) {
		//no option detected
		//first look at default file
#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_DATA,&hdf5_file) == 0)
			return 0;


		if (g_access(hdf5_file, F_OK | R_OK) != 0) {
			g_fprintf(stderr, "HDF5 data file %s found in registry is not accessible\nTrying file in current directory instead\n", hdf5_file);
			if (g_access("xmimsimdata.h5", F_OK | R_OK) == 0) {
				//look in current folder
				hdf5_file = strdup("xmimsimdata.h5");
				*hdf5_filePtr = hdf5_file;
				return 1;
			}
		}
		else {
			*hdf5_filePtr = hdf5_file;
			return 1;
		}
#elif defined(MAC_INTEGRATION)
		if (xmi_resources_mac_query(XMI_RESOURCES_MAC_DATA,&hdf5_file) == 0)
			return 0;


		if (g_access(hdf5_file, F_OK | R_OK) != 0) {
			fprintf(stderr,"App bundle does not contain the HDF5 data file\n");
			return 0;
		}
		else {
			*hdf5_filePtr = hdf5_file;
			return 1;
		}
#else
		//UNIX mode...
		if (g_access(XMIMSIM_HDF5_DEFAULT, F_OK | R_OK) == 0)
			hdf5_file = strdup(XMIMSIM_HDF5_DEFAULT);
		else if (g_access("xmimsimdata.h5", F_OK | R_OK) == 0) {
			//look in current folder
			hdf5_file = strdup("xmimsimdata.h5");
		}
		else {
			//if not found abort...	
			g_fprintf(stderr,"Could not detect the HDF5 data file\nCheck the xmimsim installation or\nuse the --with-hdf5-data option to manually pick the file\n");
			return 0;
		}
		*hdf5_filePtr = hdf5_file;
#endif
	}


	return 1;


}
