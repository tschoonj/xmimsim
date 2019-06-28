/*
Copyright (C) 2019 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-gobject.h"
#include "xmimsim-gui-xmsi-selection-scrolled-window.h"

// taken more or less from ebassi's graphene-gobject.c
#define XMI_MSIM_GUI_DEFINE_BOXED_TYPE(TypeName, type_name) \
  GType \
    xmi_msim_gui_ ## type_name ## _get_type (void) \
  { \
    static volatile gsize xmi_msim_gui_define_id__volatile = 0; \
    if (g_once_init_enter (&xmi_msim_gui_define_id__volatile)) \
      { \
        GType xmi_msim_gui_define_id = \
          g_boxed_type_register_static (g_intern_static_string (#TypeName), \
                                        (GBoxedCopyFunc) xmi_msim_gui_ ## type_name ## _copy, \
                                        (GBoxedFreeFunc) xmi_msim_gui_ ## type_name ## _free); \
        g_once_init_leave (&xmi_msim_gui_define_id__volatile, xmi_msim_gui_define_id); \
      } \
    return xmi_msim_gui_define_id__volatile; \
  }
/*
static XmiMsimGuiXmsiSelectionXPathData* xmi_msim_gui_xmsi_selection_xpath_data_copy(XmiMsimGuiXmsiSelectionXPathData *boxed) {
	if (boxed) {
		XmiMsimGuiXmsiSelectionXPathData *copy = g_memdup(boxed, sizeof(XmiMsimGuiXmsiSelectionXPathData));
		copy->xpath = g_strdup(boxed->xpath);
		return copy;
	}
	return NULL;
}

static void xmi_msim_gui_xmsi_selection_xpath_data_free(XmiMsimGuiXmsiSelectionXPathData *boxed) {
	if (boxed) {
		g_free(boxed->xpath);
		g_free(boxed);
	}
}

XMI_MSIM_GUI_DEFINE_BOXED_TYPE(XmiMsimGuiXmsiSelectionXPathData, xmsi_selection_xpath_data)
*/
