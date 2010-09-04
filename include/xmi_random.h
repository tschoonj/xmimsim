#ifndef XMI_RANDOM_H
#define XMI_RANDOM_H

//all functions return 1 on success, 0 on error

int xmi_start_random_acquisition(void);

int xmi_end_random_acquisition(void);

int xmi_get_random_numbers(unsigned long int *numbers,long int n);


#endif
