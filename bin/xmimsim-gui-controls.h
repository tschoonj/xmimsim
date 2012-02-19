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

#include <gtk/gtk.h> 

#ifndef XMIMSIM_GUI_CONTROLS_H
#define XMIMSIM_GUI_CONTROLS_H

struct xmi_options {
	char *executable;
	gboolean Mlines;
	gboolean rad_cascade;
	gboolean nonrad_cascade;
	gboolean variance_reduction;
	char *hdf5_data;
	char *spe_conv;
	char *spe_uconv;
	char *csv_conv;
	char *csv_uconv;
	char *html_conv;
	char *html_uconv;
	char *svg_conv;
	char *svg_uconv;
	int nchannels;
	gboolean pile_up;
};


GtkWidget *init_simulation_controls(GtkWidget *window);
extern GtkWidget *executableW;
extern GtkWidget *MlinesW;
extern GtkWidget *rad_cascadeW;
extern GtkWidget *nonrad_cascadeW;
extern GtkWidget *variance_reductionW;
extern GtkWidget *pile_upW;
extern GtkWidget *spe_convW;
extern GtkWidget *spe_uconvW;
extern GtkWidget *csv_convW;
extern GtkWidget *csv_uconvW;
extern GtkWidget *svg_convW;
extern GtkWidget *svg_uconvW;
extern GtkWidget *html_convW;
extern GtkWidget *html_uconvW;

extern GtkWidget *playButton;
extern GtkWidget *pauseButton;
extern GtkWidget *stopButton;


#endif
