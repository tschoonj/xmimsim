#ifndef XMI_RANDOM_DEV_H
#define XMI_RANDOM_DEV_H


#define LOCKFILE "/var/tmp/xmi_harvester.pid"
#define FIFOMASTER "/tmp/xmi_harvester.master"
#define FIFOSLAVE "/tmp/xmi_harvester.slave."


//all functions return 1 on success, 0 on error

int xmi_start_random_acquisition_dev(void);

int xmi_end_random_acquisition_dev(void);

int xmi_get_random_number_dev(unsigned long int *number);


#endif
