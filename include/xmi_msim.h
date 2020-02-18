/*
Copyright (C) 2010-2020 Tom Schoonjans and Laszlo Vincze

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



#define XMI_MSIM_VERSION_MAJOR 8
#define XMI_MSIM_VERSION_MINOR 1

#include "xmi_config.h"
#include "xmi_random.h"
#include "xmi_data_structs.h"
#include "xmi_xml.h"
#include "xmi_main.h"
#include "xmi_pymca.h"
#include "xmi_solid_angle.h"
#include "xmi_xslt.h"
#include "xmi_detector.h"
#include "xmi_data.h"
#include "xmi_ebel.h"
#include "xmi_aux.h"
#include "xmi_xrmc.h"
#include "xmi_error.h"
#include "xmi_spline.h"
#include "xmi_transmission_efficiency.h"
#include "xmi_gobject.h"
#include "xmi_job.h"
#include "xmi_batch.h"

#ifdef XMI_MSIM_MAC_INTEGRATION
#include "xmi_resources_mac.h"
#endif

// Windows currently only works through the installer, MSYS2 is only meant for building, not for running...
// This may change in the future though...
#ifdef _WIN32
#include "xmi_registry_win.h"
#endif

#ifdef XMI_MSIM_HAVE_GOOGLE_ANALYTICS
#include "xmi_google_analytics.h"
#endif

#endif
