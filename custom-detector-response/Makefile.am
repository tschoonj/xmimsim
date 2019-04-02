#Copyright (C) 2010-2014 Tom Schoonjans and Laszlo Vincze

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
xmimsimdir = $(datadir)/xmimsim

AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include -I${top_builddir}/src $(glib2_CFLAGS)

plugindir = $(libdir)/xmimsim

if ENABLE_CUSTOM_DETECTOR
plugin_LTLIBRARIES = detector-response1.la detector-response2.la
endif


#Fortran example
detector_response1_la_FCFLAGS = @OPENMP_FCFLAGS@ -I${top_builddir}/src $(xraylib_f2003_CFLAGS) $(easyRNG_CFLAGS) $(fgsl_CFLAGS)
detector_response1_la_SOURCES = detector-response1.F90
detector_response1_la_LDFLAGS = -module -avoid-version -shared -export-dynamic
if OS_WINDOWS
detector_response1_la_LIBADD = ../src/libxmimsim.la -L$(CC_DIR) -lgomp-1
detector_response1_la_LDFLAGS += -no-undefined
endif



#C example
detector_response2_la_SOURCES = detector-response2.c
detector_response2_la_CFLAGS = @OPENMP_CFLAGS@
detector_response2_la_LDFLAGS = -module -avoid-version -shared -export-dynamic
if OS_WINDOWS
detector_response2_la_LIBADD = ../src/libxmimsim.la
detector_response2_la_LDFLAGS += -no-undefined
endif

clean-local:
	rm -f xmimsim_custom_response1.mod
