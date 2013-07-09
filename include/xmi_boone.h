/* 
 * Heavily modified version of John M. Boone's code
 * His code mentions no license whatsoever, so will assume it is in the public domain
 * Tom Schoonjans 2013 
 */

#ifndef XMI_BOONE_H
#define XMI_BOONE_H
#include "xmi_data_structs.h"

/*
 * Based on J. Boone and J. Seibert. An accurate method for computer-generating tungsten anode x-ray spectra from 30 to 140 kv. Medical physics, 24:1661, 1997
 * Original code downloaded from http://ftp.aip.org/epaps/medical_phys/E-MPHYA-24-1661
 */

int xmi_tube_boone_1661(struct xmi_layer *tube_window, struct xmi_layer *tube_filter,
		  double tube_ripple, double tube_voltage,
		  double tube_current, double tube_solid_angle, 
		  struct xmi_excitation **boone_spectrum
		  );






#endif
