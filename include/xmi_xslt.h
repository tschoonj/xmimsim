/*
Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_XSLT_H
#define XMI_XSLT_H

#ifdef __cplusplus
extern "C" {
#endif

int xmi_xmso_to_xmsi_xslt(char *xmsofile, char *xmsifile, char *outputfile);
//passing 1 for convoluted will use the convoluted spectra, 0 the unconvoluted
int xmi_xmso_to_svg_xslt(char *xmsofile, char *svgfile, unsigned convoluted);
int xmi_xmso_to_spe_xslt(char *xmsofile, char *spefile, unsigned convoluted, int interaction_number);
int xmi_xmso_to_csv_xslt(char *xmsofile, char *csvfile, unsigned convoluted);
int xmi_xmso_to_htm_xslt(char *xmsofile, char *htmlfile, unsigned convoluted);
int xmi_xmsa_to_xmso_xslt(char *xmsafile, char *xmsifile, int step1, int step2);

#ifdef __cplusplus
}
#endif

#endif
