#include "xmi_random.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

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
