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

#include "xmi_data.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <stdlib.h>

XMI_MAIN
	int rv;

	xmi_init_hdf5();

	const int nZ = 94;
	int *Zs = malloc(sizeof(int) * nZ);
	int i;
	for (i = 0 ; i < nZ ; i++)
		Zs[i] = i + 1;

	if (argc != 1) {
		rv = xmi_db(argv[1], Zs, nZ);
	}
	else {
		rv = xmi_db("xmimsimdata.h5", Zs, nZ);
	}
	if (rv == 1)
		rv = 0;
	else if (rv == 0)
		rv = 1;

	free(Zs);

	return rv;
}
