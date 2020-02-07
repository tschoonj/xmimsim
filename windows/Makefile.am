#Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

GTK3_RUNTIME_TAG = 2019-12-19
GTK3_RUNTIME_VERSION = 3.24.13
GTK3_RUNTIME_FILE = gtk3-runtime-$(GTK3_RUNTIME_VERSION)-$(GTK3_RUNTIME_TAG)-ts-win64.exe

common:
	cp ../src/libxmimsim-0.def .
	cp ../bin/libxmimsim-gui-0.def .
	unix2dos -n $(top_srcdir)/include/xmi_msim.h xmi_msim.h
	unix2dos -n $(top_builddir)/include/xmi_config.h xmi_config.h
	unix2dos -n $(top_srcdir)/include/xmi_random.h xmi_random.h
	unix2dos -n $(top_srcdir)/include/xmi_data_structs.h xmi_data_structs.h
	unix2dos -n $(top_srcdir)/include/xmi_xml.h xmi_xml.h
	unix2dos -n $(top_srcdir)/include/xmi_main.h xmi_main.h
	unix2dos -n $(top_srcdir)/include/xmi_pymca.h xmi_pymca.h
	unix2dos -n $(top_srcdir)/include/xmi_solid_angle.h xmi_solid_angle.h
	unix2dos -n $(top_srcdir)/include/xmi_xslt.h xmi_xslt.h
	unix2dos -n $(top_srcdir)/include/xmi_detector.h xmi_detector.h
	unix2dos -n $(top_srcdir)/include/xmi_data.h xmi_data.h
	unix2dos -n $(top_srcdir)/include/xmi_ebel.h xmi_ebel.h
	unix2dos -n $(top_srcdir)/include/xmi_aux.h xmi_aux.h
	unix2dos -n $(top_srcdir)/include/xmi_xrmc.h xmi_xrmc.h
	unix2dos -n $(top_srcdir)/include/xmi_error.h xmi_error.h
	unix2dos -n $(top_srcdir)/include/xmi_spline.h xmi_spline.h
	unix2dos -n $(top_srcdir)/include/xmi_transmission_efficiency.h xmi_transmission_efficiency.h
	unix2dos -n $(top_srcdir)/include/xmi_gobject.h xmi_gobject.h
	unix2dos -n $(top_srcdir)/include/xmi_job.h xmi_job.h
	unix2dos -n $(top_srcdir)/include/xmi_batch.h xmi_batch.h
	unix2dos -n $(top_srcdir)/include/xmi_registry_win.h xmi_registry_win.h
	unix2dos -n $(top_srcdir)/include/xmi_google_analytics.h xmi_google_analytics.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui.h xmimsim-gui.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-prefs.h xmimsim-gui-prefs.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-source-abstract.h xmimsim-gui-source-abstract.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-compound-dialog.h  xmimsim-gui-compound-dialog.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-catalog-dialog.h xmimsim-gui-catalog-dialog.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-layer-dialog.h xmimsim-gui-layer-dialog.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-type-builtins.h xmimsim-gui-type-builtins.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-utils.h xmimsim-gui-utils.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-utils-pp.h xmimsim-gui-utils-pp.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-export-canvas-dialog.h xmimsim-gui-export-canvas-dialog.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-tools.h xmimsim-gui-tools.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-sources-dialog.h xmimsim-gui-sources-dialog.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-discrete-energy-dialog.h xmimsim-gui-discrete-energy-dialog.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-continuous-energy-dialog.h xmimsim-gui-continuous-energy-dialog.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-compat.h xmimsim-gui-compat.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-colors.h xmimsim-gui-colors.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-xmsi-selection-scrolled-window.h xmimsim-gui-xmsi-selection-scrolled-window.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-options-box.h xmimsim-gui-options-box.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-xmsa-viewer-window.h xmimsim-gui-xmsa-viewer-window.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-xmso-results-scrolled-window.h xmimsim-gui-xmso-results-scrolled-window.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-undo-manager.h xmimsim-gui-undo-manager.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-layer-box.h xmimsim-gui-layer-box.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-clipboard-manager.h xmimsim-gui-clipboard-manager.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-long-task-window.h xmimsim-gui-long-task-window.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-controls-scrolled-window.h xmimsim-gui-controls-scrolled-window.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-xmsi-config-scrolled-window.h xmimsim-gui-xmsi-config-scrolled-window.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-application.h xmimsim-gui-application.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-application-window.h xmimsim-gui-application-window.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-xmso-results-application-window.h xmimsim-gui-xmso-results-application-window.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-batch-assistant.h xmimsim-gui-batch-assistant.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-gobject.h xmimsim-gui-gobject.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-batch-archive-settings-box.h xmimsim-gui-batch-archive-settings-box.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-plugins-engine.h xmimsim-gui-plugins-engine.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-batch-controls-box.h xmimsim-gui-batch-controls-box.h
	unix2dos -n $(top_srcdir)/bin/xmimsim-gui-batch-multi-selection-type-grid.h xmimsim-gui-batch-multi-selection-type-grid.h
	unix2dos -n $(top_srcdir)/examples/mendeljev.py mendeljev.py


$(GTK3_RUNTIME_FILE):
	wget -q https://github.com/tschoonj/GTK-for-Windows-Runtime-Environment-Installer/releases/download/$(GTK3_RUNTIME_TAG)/$(GTK3_RUNTIME_FILE)

iss: $(GTK3_RUNTIME_FILE)
	$(MAKE) common
	lib /machine:X64 /def:libxmimsim-0.def
	lib /machine:X64 /def:libxmimsim-gui-0.def
	iscc.exe "-I$(abs_top_srcdir_win)/windows" "-dabs_top_srcdir_win=$(abs_top_srcdir_win)" "-dabs_top_builddir_win=$(abs_top_builddir_win)" "-dMyAppVersion=$(VERSION)" "-dMY_HOME=$(abs_homedir_win)" "-dGTK_INSTALLER_EXE=$(GTK3_RUNTIME_FILE)" "$(srcdir)/xmimsim.iss"

EXTRA_DIST = xmimsim.iss

clean-local:
	rm -f $(GTK3_RUNTIME_FILE) dwinshs.iss
