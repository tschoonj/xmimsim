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
