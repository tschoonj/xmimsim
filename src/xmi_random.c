#include "xmi_random.h"
#include "xmi_random_dev.h"

#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>


static long int counter;
static pthread_mutex_t counter_mutex;



int xmi_start_random_acquisition(void) {
	//two possibilities: 
	//1) if daemon is running -> do nothing
	//2) if not -> start a thread


	if (access(LOCKFILE,F_OK) == -1) {
		//daemon does not appear to be running
		return xmi_start_random_acquisition_dev();
	}
	else {
		//daemon is running...
		//initialize counter and mutex;
		pthread_mutex_init(&counter_mutex,NULL);
		counter = 0;

	}

	return 1;

}

int xmi_end_random_acquisition(void) {
	//two possibilities: 
	//1) if daemon is running -> do nothing
	//2) if not -> terminate thread


	if (access(LOCKFILE,F_OK) == -1) {
		//daemon does not appear to be running
		return xmi_end_random_acquisition_dev();
	}
	else {
		//daemon is running...
		//destroy mutex
		pthread_mutex_destroy(&counter_mutex);	
	}

	return 1;

}

int xmi_get_random_numbers(unsigned long int *numbers,long int n) {
	//two possibilities: 
	//1) if daemon is running -> query for n random numbers
	//2) if not -> get random numbers from dev

	int fdmaster,fdslave;
	long int i;
	long int counter_local;
	long int pidstuff[3]; //pid, counter and n
	char fifoslave[512];

	if ((fdmaster = open (FIFOMASTER, O_WRONLY)) == -1) {
		//daemon does not appear to be running
		for (i = 0 ; i < n ; i++) {
			if (xmi_get_random_number_dev(numbers+i) == 0) {
				return 0;
			}	
		}
	}
	else {
		//daemon is running...
		pthread_mutex_lock(&counter_mutex);
		counter_local = counter++;
		pthread_mutex_unlock(&counter_mutex);

		pidstuff[0] = (long int) getpid();
		pidstuff[1] = counter_local;
		pidstuff[2] = n;

		sprintf(fifoslave,FIFOSLAVE "%li.%li",pidstuff[0],pidstuff[1]);
        	if (mkfifo(fifoslave, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH) == -1) {
	                fprintf(stderr,"Could not create named link %s: %s",fifoslave,strerror(errno));
		        return 0;
		}
		if (write(fdmaster,pidstuff,sizeof(long int)*3) != sizeof(long int)*3) {
		        fprintf(stderr,"write error\n");
		        return 0;
		}
		close(fdmaster);
	        if ((fdslave = open(fifoslave,O_RDONLY)) == -1) {
	                fprintf(stderr,"Could not open named link %s: %s",fifoslave,strerror(errno));
	                return 0;
	        }
	        if (read(fdslave,numbers,sizeof(unsigned long int)*n) != sizeof(unsigned long int)*n) {
	                fprintf(stderr,"Error reading from %s: %s",fifoslave,strerror(errno));
	                return 0;
	        }
	        close(fdslave);
	        unlink(fifoslave);


	}

	return 1;
}

