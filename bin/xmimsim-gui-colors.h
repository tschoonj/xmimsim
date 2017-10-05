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


#ifndef XMI_MSIM_GUI_COLORS_H
#define XMI_MSIM_GUI_COLORS_H

#include <gdkmm/rgba.h>

G_BEGIN_DECLS

#define XmiColor Gdk::RGBA *

extern XmiColor white_plot;
extern XmiColor blue_plot;
extern XmiColor red_plot;
extern XmiColor green_plot;
extern XmiColor black_plot;
extern XmiColor purple_plot;
extern XmiColor yellow_plot;
extern XmiColor pink_plot;

void xmi_msim_gui_utils_init_colors();

G_END_DECLS

#endif

