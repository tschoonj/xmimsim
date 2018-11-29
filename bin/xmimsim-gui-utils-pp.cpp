#include "xmimsim-gui-utils-pp.h"
#include "xmimsim-gui-colors.h"
#include <gtkmm/main.h>

void xmi_msim_gui_init(void) {
	Gtk::Main::init_gtkmm_internals();
	xmi_msim_gui_utils_init_colors();
}
