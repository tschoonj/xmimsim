#ifndef XMI_PYMCA_H
#define XMI_PYMCA_H



#include "xmi_data_structs.h"


//return 1 on success, 0 otherwise

//allocation of input occurs in function!
int xmi_read_input_pymca(char *pymca_file, struct xmi_input **input);

#endif
