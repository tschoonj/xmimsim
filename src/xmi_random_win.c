
#ifndef _WIN32
#error _WIN32 macro is not defined in xmi_random_win.c
#endif

#define _CRT_RAND_S
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
	 *
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
*/		

	long int i;
	guint32 result;
	unsigned long int result2;
	GRand *rng;

	for (i = 0 ; i < n ; i++) {
		if (sizeof(unsigned long int) == sizeof(guint32)) {
			//size of unsigned long int is 4 bytes -> 32
			rng = g_rand_new();
			result = g_rand_int(rng);
			numbers[i] = result;
			g_rand_free(rng);
		}
		else if (sizeof(unsigned long int) == 2*sizeof(guint32)) {
			rng = g_rand_new();
			result = g_rand_int(rng);
			numbers[i] = result >> 32;
			numbers[i] += g_rand_int(rng);
			g_rand_free(rng);
			
			
		}
 	}

	return 1;
}

