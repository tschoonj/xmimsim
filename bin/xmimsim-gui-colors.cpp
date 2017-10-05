/*
Copyright (C) 2017 Tom Schoonjans and Laszlo Vincze

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

#include "xmimsim-gui-colors.h"

XmiColor white_plot;
XmiColor blue_plot;
XmiColor red_plot;
XmiColor green_plot;
XmiColor black_plot;
XmiColor purple_plot;
XmiColor yellow_plot;
XmiColor pink_plot;

#define COLOR_INIT(color) color ## _plot = new Gdk::RGBA(#color);

void xmi_msim_gui_utils_init_colors() {
	/*initialize colors*/
	COLOR_INIT(white);
	COLOR_INIT(blue);
	COLOR_INIT(red);
	COLOR_INIT(green);
	COLOR_INIT(black);
	COLOR_INIT(purple);
	COLOR_INIT(yellow);
	COLOR_INIT(pink);
}

