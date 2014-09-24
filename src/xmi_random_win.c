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


#ifndef _WIN32
#error _WIN32 macro is not defined in xmi_random_win.c
#endif

#define _CRT_RAND_S
#include "config.h"
#include <stdlib.h>
#include <stdio.h>
#include <glib.h>

int xmi_start_random_acquisition(void) {
	//does nothing really
	return 1;
}

int xmi_end_random_acquisition(void) {
	//does nothing really
	return 1;
}

int xmi_get_random_numbers(unsigned long int *numbers,long int n) {
	//assume numbers is already allocated!!!

	/*	
	 *
	 *This works perfect on Windows 
	 *
	 */
	long int i;
	unsigned int number;
	errno_t err;

	for (i=0 ; i < n ; i++) {
		err = rand_s(&number);
		if (err != 0) {
			fprintf(stderr,"rand_s error\n");
			return 0;
		}
		numbers[i] = number;
	}
		
	/*
	long int i;
	guint32 result;
	unsigned long int result2;
	GRand *rng;

	for (i = 0 ; i < n ; i++) {
		//size of unsigned long int is 4 bytes -> 32
		rng = g_rand_new();
		result = g_rand_int(rng);
		numbers[i] = result;
		g_rand_free(rng);
 	}*/

	return 1;
}

