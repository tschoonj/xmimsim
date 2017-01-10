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

#include "xmi_random.h"
#include <stdio.h>

int main (int argc, char *argv[]) {
	int n,i;
	unsigned long int *seeds;


	if (argc != 2) {
		fprintf(stderr,"Usage: %s number\n",argv[0]);
		return 1;
	}

	n = atoi(argv[1]);

	if (xmi_start_random_acquisition() == 0) {
		return 1;
	}

	seeds = (unsigned long int *) malloc (sizeof(unsigned long int)*n);

	if (xmi_get_random_numbers(seeds,(long) n) == 0) {
		return 1;
	}

	for (i = 0 ; i < n ; i++)
		fprintf(stdout,"Seed %i: %lu\n",i,seeds[i]);


	if (xmi_end_random_acquisition() == 0) {
		return 1;
	}
	return 0;
}
