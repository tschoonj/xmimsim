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


#ifndef XMI_DATA_H
#define XMI_DATA_H

#include <xmi_data_structs.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void* xmi_hdf5FPtr;

//returns 1 on success; 0 on error
//hdf5_filePtr must be a pointer to char *, which may be equal to NULL, in which case the function try to allocate a string containing the path to hDF5 data file of XMI-MSIM
int xmi_get_hdf5_data_file(char **hdf5_filePtr);

int xmi_db(char *filename, int *Zs, int nZ);

//Fortran function that reads in from the HDF5 data file what it needs... return 1 on success, 0 otherwise
int xmi_init_from_hdf5(char *hdf5_file, xmi_inputFPtr inputFPtr, xmi_hdf5FPtr *hdf5FPtr, xmi_main_options options);

//Fortran function that frees a Fortran xmi_hdf5 TYPE variable. The value of the pointer shall be set to NULL afterwards.
void xmi_free_hdf5_F(xmi_hdf5FPtr *hdf5FPtr);

int xmi_update_input_from_hdf5(xmi_inputFPtr inputFPtr, xmi_hdf5FPtr hdf5FPtr);

#define XMI_DATA_MIN_VERSION 5.3

#ifdef __cplusplus
}
#endif
#endif
