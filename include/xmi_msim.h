/*
Copyright (C) 2010-2013 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_MSIM_H
#define XMI_MSIM_H

/*
 * this file should be included
 * when you want to compile your
 * own software against XMI-MSIM
 */



#define XMI_MSIM_VERSION_MAJOR 2
#define XMI_MSIM_VERSION_MINOR 1

#include <glib.h>
#include "xmi_data_structs.h"
#include "xmi_detector.h"
#include "xmi_main.h"
#include "xmi_pymca.h"
#include "xmi_random.h"
#include "xmi_solid_angle.h"
#include "xmi_xml.h"
#include "xmi_xslt.h"
#include "xmi_data.h"
#include "xmi_ebel.h"
#include "xmi_boone.h"
#include "xmi_aux.h"

#ifdef MAC_INTEGRATION
#include "xmi_resources_mac.h"
#endif



#ifdef G_OS_WIN32
#include "xmi_registry_win.h"
#endif


#endif
