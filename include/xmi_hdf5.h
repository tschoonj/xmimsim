/*
Copyright (C) 2010-2013 Tom Schoonjans and Laszlo Vincze

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


#ifndef XMI_HDF5_H
#define XMI_HDF5_H



//returns 1 on success; 0 on error
//hdf5_filePtr must be a pointer to char *, which may be equal to NULL, in which case the function try to allocate a string containing the path to hDF5 data file of XMI-MSIM
int xmi_get_hdf5_data_file(char **hdf5_filePtr);


#endif
