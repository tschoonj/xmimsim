/*
Copyright (C) 2017-2020 Tom Schoonjans and Laszlo Vincze

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

#include <gdk/gdk.h>

G_BEGIN_DECLS

extern GdkRGBA white_plot;
extern GdkRGBA blue_plot;
extern GdkRGBA red_plot;
extern GdkRGBA green_plot;
extern GdkRGBA black_plot;
extern GdkRGBA purple_plot;
extern GdkRGBA yellow_plot;
extern GdkRGBA pink_plot;

void xmi_msim_gui_utils_init_colors(void);

G_END_DECLS

#endif

