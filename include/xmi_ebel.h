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

#ifndef XMI_EBEL_H
#define XMI_EBEL_H
#include "xmi_data_structs.h"

#ifdef __cplusplus
extern "C" {
#endif

int xmi_tube_ebel(struct xmi_layer *tube_anode, struct xmi_layer *tube_window,
		  struct xmi_layer *tube_filter, double tube_voltage,
		  double tube_current, double tube_angle_electron,
		  double tube_angle_xray, double tube_delta_energy,
		  double tube_solid_angle, int tube_transmission,
		  size_t tube_nefficiencies, double *tube_energies, double *tube_efficiencies,
		  struct xmi_excitation **ebel_spectrum
		  );

#ifdef __cplusplus
}
#endif
#endif
