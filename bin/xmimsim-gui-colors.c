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
#include <gtk/gtk.h>

GdkRGBA white_plot;
GdkRGBA blue_plot;
GdkRGBA red_plot;
GdkRGBA green_plot;
GdkRGBA black_plot;
GdkRGBA purple_plot;
GdkRGBA yellow_plot;
GdkRGBA pink_plot;

#define COLOR_INIT(color) gdk_rgba_parse(&color ## _plot, #color);

void xmi_msim_gui_utils_init_colors(void) {
	/*initialize colors*/
	COLOR_INIT(white);
	COLOR_INIT(blue);
	COLOR_INIT(red);
	COLOR_INIT(green);
	COLOR_INIT(black);
	COLOR_INIT(purple);
	COLOR_INIT(yellow);
	COLOR_INIT(pink);

	// CSS stuff
	// after https://stackoverflow.com/a/39066419
	GdkScreen *screen = gdk_screen_get_default();
	GtkCssProvider *gtk_provider = gtk_css_provider_new();
	GtkStyleContext *gtk_context = gtk_style_context_new();
	gtk_style_context_add_provider_for_screen(screen, GTK_STYLE_PROVIDER(gtk_provider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
	GError *error = NULL;

	if (!gtk_css_provider_load_from_data(gtk_provider,
		"#color_entry.red { background-image: linear-gradient(red,red); }",
		-1,
		&error
	)) {
		g_warning("xmi_msim_gui_utils_init_colors -> could not load CSS data!");
		if (error) {
			g_warning("error message: %s", error->message);
			g_error_free(error);
		}
	}
}

