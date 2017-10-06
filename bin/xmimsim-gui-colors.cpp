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
#include <gdkmm/screen.h>
#include <gtkmm/cssprovider.h>
#include <gtkmm/stylecontext.h>

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


	// CSS stuff
	// after https://stackoverflow.com/a/39066419
	Glib::RefPtr<Gdk::Screen> screen = Gdk::Screen::get_default();
	Glib::RefPtr<Gtk::CssProvider> gtk_provider = Gtk::CssProvider::create();
	Glib::RefPtr<Gtk::StyleContext> gtk_context = Gtk::StyleContext::create();
	gtk_context->add_provider_for_screen(screen, gtk_provider, GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);

	if (not gtk_provider->load_from_data(
		"#color_entry.red { background-image: linear-gradient(red,red); }"
	)) {
		g_warning("xmi_msim_gui_utils_init_colors -> could not load CSS data!");
	}
	
}

