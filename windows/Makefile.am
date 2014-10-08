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

common:
	cp ../src/libxmimsim-0.def .
	unix2dos -n $(top_srcdir)/include/xmi_random.h xmi_random.h
	unix2dos -n $(top_srcdir)/include/xmi_data_structs.h xmi_data_structs.h
	unix2dos -n $(top_srcdir)/include/xmi_xml.h xmi_xml.h
	unix2dos -n $(top_srcdir)/include/xmi_main.h xmi_main.h
	unix2dos -n $(top_srcdir)/include/xmi_pymca.h xmi_pymca.h
	unix2dos -n $(top_srcdir)/include/xmi_solid_angle.h xmi_solid_angle.h
	unix2dos -n $(top_srcdir)/include/xmi_xslt.h xmi_xslt.h
	unix2dos -n $(top_srcdir)/include/xmi_detector.h xmi_detector.h
	unix2dos -n $(top_srcdir)/include/xmi_data.h xmi_data.h
	unix2dos -n $(top_srcdir)/include/xmi_msim.h xmi_msim.h
	unix2dos -n $(top_srcdir)/include/xmi_ebel.h xmi_ebel.h
	unix2dos -n $(top_srcdir)/include/xmi_aux.h xmi_aux.h
	unix2dos -n $(top_srcdir)/include/xmi_boone.h xmi_boone.h
	unix2dos -n $(top_srcdir)/include/xmi_xrmc.h xmi_xrmc.h
	unix2dos -n $(top_srcdir)/include/xmi_registry_win.h xmi_registry_win.h


if OS_WINDOWS_32
iss:
	$(MAKE) common
	lib /machine:i386 /def:libxmimsim-0.def
	iscc.exe "-dabs_top_srcdir_win=$(abs_top_srcdir_win)" "-dabs_top_builddir_win=$(abs_top_builddir_win)" "-dXMI_MSIM32" "-dMyAppVersion=$(VERSION)" "-dMY_HOME=$(abs_homedir_win)" "$(srcdir)/xmimsim.iss"
endif

if OS_WINDOWS_64
iss:
	$(MAKE) common
	lib /machine:X64 /def:libxmimsim-0.def
	iscc.exe "-dabs_top_srcdir_win=$(abs_top_srcdir_win)" "-dabs_top_builddir_win=$(abs_top_builddir_win)" "-dXMI_MSIM64" "-dMyAppVersion=$(VERSION)" "-dMY_HOME=$(abs_homedir_win)" "$(srcdir)/xmimsim.iss"
endif

EXTRA_DIST = xmimsim.iss GPLv3.rtf
