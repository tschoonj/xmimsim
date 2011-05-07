
#ifndef _WIN32
#error _WIN32 macro is not defined in xmi_random_win.c
#endif

#define _CRT_RAND_S
#include <stdlib.h>
#include <stdio.h>

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
		

	return 1;
}

