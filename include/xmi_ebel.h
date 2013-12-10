


#ifndef XMI_EBEL_H
#define XMI_EBEL_H
#include "xmi_data_structs.h"

int xmi_tube_ebel(struct xmi_layer *tube_anode, struct xmi_layer *tube_window,
		  struct xmi_layer *tube_filter, double tube_voltage,
		  double tube_current, double tube_angle_electron,
		  double tube_angle_xray, double tube_delta_energy,
		  double tube_solid_angle, int tube_transmission,
		  struct xmi_excitation **ebel_spectrum
		  );
#endif
