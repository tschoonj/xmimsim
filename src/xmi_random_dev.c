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

#include "config.h"
#include "xmi_random_dev.h"
#include <pthread.h>
#include <stdio.h>
#include <time.h>
#include <errno.h>
#include <glib.h>

#define SLEEP_TIME 1
#define MAX_NUMBERS 8192
#define RANDOM_DEVICE "/dev/urandom"

static unsigned long int *xmi_random_numbers;
static int xmi_numbers_in_memory;
static FILE *random_devicePtr;
static int xmi_random_active=0;
static pthread_t xmi_random_pthread_t;
static pthread_mutex_t xmi_random_mutex;


static void *xmi_random_thread(void *input) {
	unsigned long int temp_number;
	struct timespec sleep_time = {.tv_sec = 0,.tv_nsec = 100000000L};
	int rv;

#if DEBUG == 2
	fprintf(stdout,"Entering xmi_random_thread\n");
#endif

	//for loop keeps running until killed by xmi_end_random_acquisition
	for (;;) {
		//lock
#if DEBUG == 2
		fprintf(stdout,"Before lock in thread\n");
#endif
		rv=pthread_mutex_lock(&xmi_random_mutex);
#if DEBUG == 2
		fprintf(stdout,"After lock in thread: %i\n",rv);
#endif
		if (xmi_numbers_in_memory < MAX_NUMBERS) {
			rv=pthread_mutex_unlock(&xmi_random_mutex);
/*			//open the random device
			if ((random_devicePtr=fopen(RANDOM_DEVICE,"r")) == NULL) {
				fprintf(stderr,"Could not open " RANDOM_DEVICE " for reading: continuing...\n");
				nanosleep(&sleep_time,NULL);
				continue;
			}*/
#if DEBUG == 2
			fprintf(stdout,"Before fread\n");
#endif
			if (fread(&temp_number,sizeof(unsigned long int),1,random_devicePtr) == 1) {
				rv=pthread_mutex_lock(&xmi_random_mutex);
				xmi_random_numbers[xmi_numbers_in_memory++]=temp_number;
				rv=pthread_mutex_unlock(&xmi_random_mutex);
#if DEBUG == 2
				fprintf(stdout,"Found a new number: %lu\n",temp_number);
#endif
			}
//			fclose(random_devicePtr);
		}
		else {
			rv=pthread_mutex_unlock(&xmi_random_mutex);
		}


#if DEBUG == 2
		fprintf(stdout,"Before nanosleep: %i\n",rv);
#endif
		nanosleep(&sleep_time,NULL);
#if DEBUG == 2
		fprintf(stdout,"After nanosleep\n");
#endif
	}
	//this line should never be reached
	return NULL;
}









int xmi_start_random_acquisition_dev(void) {
#if DEBUG == 2
	fprintf(stdout,"Entering xmi_start_random_acquisition\n");
#endif

	if (xmi_random_active == 1) {
		fprintf(stderr,"Random number acquisition already active\n");
		return 0;
	}
	xmi_numbers_in_memory = 0;
	//allocate memory for the random_numbers
	xmi_random_numbers = (unsigned long int*) g_malloc(sizeof(unsigned long int)*MAX_NUMBERS);
	if (xmi_random_numbers == NULL) {
		fprintf(stderr,"Could not allocate memory for the random numbers\n");
		return 0;
	}


	//open random device
	if ((random_devicePtr=fopen(RANDOM_DEVICE,"r")) == NULL) {
		fprintf(stderr,"Could not open " RANDOM_DEVICE " for reading\n");
		return 0;
	}


	//initialize mutex
	pthread_mutex_init(&xmi_random_mutex,NULL);


	//start the thread
	if (pthread_create(&xmi_random_pthread_t, NULL, xmi_random_thread,NULL) != 0) {
		fprintf(stderr,"Could not create thread xmi_random_thread\n");
		return 0;
	}
	xmi_random_active = 1;
	return 1;


}
int xmi_end_random_acquisition_dev(void){
#if DEBUG == 2
	fprintf(stdout,"Entering xmi_end_random_acquisition\n");
#endif
	if (xmi_random_active == 0) {
		fprintf(stderr,"Random number acquisition inactive\n");
		return 0;
	}
	//send the thread the cancel signal
	if (pthread_cancel(xmi_random_pthread_t) != 0) {
		fprintf(stderr,"Error sending pthread_cancel request\n");
		return 0;
	}
	//join the thread
	if (pthread_join(xmi_random_pthread_t,NULL) != 0) {
		fprintf(stderr,"Error in pthread_join request\n");
		return 0;
	}
	//free memory
	g_free(xmi_random_numbers);
	xmi_numbers_in_memory=0;
	//destroy mutex
	pthread_mutex_destroy(&xmi_random_mutex);
	//close device
	fclose(random_devicePtr);


	xmi_random_active=0;

	return 1;
}
int xmi_get_random_number_dev(unsigned long int *number) {
	int rv;
#if DEBUG == 2
	fprintf(stdout,"Entering xmi_get_random_number\n");
#endif
	struct timespec sleep_time = {.tv_sec = (time_t) SLEEP_TIME*2,.tv_nsec = 0};

	if (xmi_random_active == 0) {
		fprintf(stderr,"Random number acquisition inactive\n");
		return 0;
	}
	//lock
#if DEBUG == 2
	fprintf(stdout,"Before lock\n");
#endif
	rv=pthread_mutex_lock(&xmi_random_mutex);
#if DEBUG == 2
	fprintf(stdout,"After lock\n",rv);
#endif
#if DEBUG == 2
	fprintf(stdout,"xmi_numbers_in_memory: %i\n",xmi_numbers_in_memory);
#endif
	if (xmi_numbers_in_memory > 0) {
#if DEBUG == 2
		fprintf(stdout,"Found a random in array\n");
#endif
		//a number is available -> use it
		*number = xmi_random_numbers[--xmi_numbers_in_memory];
		//unlock
		rv=pthread_mutex_unlock(&xmi_random_mutex);
	}
	else {
		//no numbers are available...
#if DEBUG == 2
		fprintf(stdout,"Found no random in array\n");
#endif
		rv=pthread_mutex_unlock(&xmi_random_mutex);
		for (;;) {
#if DEBUG == 2
			fprintf(stdout,"Found no random in array... sleeping\n");
#endif
			nanosleep(&sleep_time,NULL);
			pthread_mutex_lock(&xmi_random_mutex);
			if (xmi_numbers_in_memory > 0) {
				*number = xmi_random_numbers[--xmi_numbers_in_memory];
				rv=pthread_mutex_unlock(&xmi_random_mutex);
				break;
			}
			rv=pthread_mutex_unlock(&xmi_random_mutex);
		}

	}
#if DEBUG == 2
	fprintf(stdout,"Found number: %lu\n",*number);
#endif

	return 1;
}





