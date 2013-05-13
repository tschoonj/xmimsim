


#include "xmi_data_structs.h"
#ifndef XMI_EBEL_H
#define XMI_EBEL_H

int xmi_tube_ebel(struct xmi_layer *tube_anode, struct xmi_layer *tube_window,
		  struct xmi_layer *tube_filter, double tube_voltage,
		  double tube_current, double tube_angle_electron,
		  double tube_angle_xray, double tube_delta_energy,
		  int tube_transmission,
		  struct xmi_energy **ebel_spectrum,
		  int *n_ebel_spectrum_cont,
		  int *n_ebel_spectrum_disc);
#endif
