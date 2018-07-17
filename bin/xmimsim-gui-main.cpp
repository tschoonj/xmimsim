/*
Copyright (C) 2018 Tom Schoonjans and Laszlo Vincze

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

#include <config.h>
#include "xmimsim-gui-application.h"
#include <gtkmm/main.h>

int main(int argc, char *argv[]) {

	gtk_disable_setlocale();

	// for gtkmm-plplot!
	Gtk::Main::init_gtkmm_internals();

	return g_application_run(G_APPLICATION(xmi_msim_gui_application_new()), argc, argv);
}
