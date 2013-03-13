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

xmimsimincludedir = ${includedir}/xmimsim

xmimsiminclude_HEADERS = xmi_random.h xmi_data_structs.h xmi_xml.h xmi_main.h xmi_pymca.h \
	xmi_solid_angle.h xmi_xslt.h xmi_detector.h xmi_msim.h

if OS_WINDOWS
xmimsiminclude_HEADERS += xmi_registry_win.h
endif

if ENABLE_MAC_APP
xmimsiminclude_HEADERS += xmi_resources_mac.h
endif
