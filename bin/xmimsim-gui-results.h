/*
Copyright (C) 2010-2012 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMIMSIM_GUI_RESULTS_H
#define XMIMSIM_GUI_RESULTS_H

#include <gtk/gtk.h>
#include <gtkextra/gtkextra.h>
#include "xmi_xml.h"

/*
 * current results variables
 */

extern struct xmi_input *results_input;
extern struct xmi_fluorescence_line *results_brute_force_history;
extern int results_nbrute_force_history;
extern struct xmi_fluorescence_line *results_var_red_history;
extern int results_nvar_red_history;
extern double **results_channels_conv;
extern double **results_channels_unconv;
extern int results_ninteractions;
extern int results_nchannels;
extern char *results_inputfile;
extern int results_use_zero_interactions;





GtkWidget *init_results(GtkWidget *window);

int plot_spectra_from_file(char *xmsofile); 



#endif
