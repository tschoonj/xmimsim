/*
Copyright (C) 2010-2019 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_MSIM_GUI_H
#define XMI_MSIM_GUI_H

#include "xmi_msim.h"

#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-source-abstract.h"
#include "xmimsim-gui-compound-dialog.h"
#include "xmimsim-gui-catalog-dialog.h"
#include "xmimsim-gui-layer-dialog.h"
#include "xmimsim-gui-type-builtins.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-utils-pp.h"
#include "xmimsim-gui-tools.h"
#include "xmimsim-gui-sources-dialog.h"
#include "xmimsim-gui-discrete-energy-dialog.h"
#include "xmimsim-gui-continuous-energy-dialog.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-colors.h"
#include "xmimsim-gui-xmsi-selection-scrolled-window.h"
#include "xmimsim-gui-options-box.h"
#include "xmimsim-gui-xmsa-viewer-window.h"
#include "xmimsim-gui-xmso-results-scrolled-window.h"
#include "xmimsim-gui-undo-manager.h"
#include "xmimsim-gui-energies-box.h"
#include "xmimsim-gui-layer-box.h"
#include "xmimsim-gui-clipboard-manager.h"
#include "xmimsim-gui-long-task-window.h"
#include "xmimsim-gui-controls-scrolled-window.h"
#include "xmimsim-gui-xmsi-config-scrolled-window.h"
#include "xmimsim-gui-application.h"
#include "xmimsim-gui-application-window.h"
#include "xmimsim-gui-xmso-results-application-window.h"
#include "xmimsim-gui-batch-assistant.h"
#include "xmimsim-gui-gobject.h"
#include "xmimsim-gui-batch-archive-settings-box.h"
#include "xmimsim-gui-plugins-engine.h"
#include "xmimsim-gui-batch-controls-box.h"
#include "xmimsim-gui-batch-multi-selection-type-grid.h"

#if defined(XMI_MSIM_HAVE_JSONGLIB) && defined(XMI_MSIM_HAVE_LIBSOUP)
#include "xmimsim-gui-updater.h"
#endif

#endif
