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
xmimsiminclude_HEADERS = xmimsim_main.mod xmimsim_aux.mod xmimsim_solid_angle.mod xmimsim_hdf5.mod xmimsim_varred.mod xmimsim_detector.mod


AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include

lib_LTLIBRARIES = libxmimsim.la
libxmimsim_la_SOURCES =	xmi_xml.c \
			xmi_data_structs.c \
			xmi_aux.c \
			xmi_aux.h \
			xmi_aux_f.F90 \
			xmi_hdf5_f.F90 \
			xmi_hdf5.c \
			xmi_solid_angle_f.F90 \
			xmi_variance_reduction.F90 \
			xmi_detector_f.F90 \
			xmi_main.F90 \
			xmi_lines.c \
			xmi_lines.h \
			xmi_pymca.c \
			xmi_solid_angle.c \
			xmi_xslt.c \
			xmi_detector.c \
			xmi_ebel.F90

if !OS_WINDOWS
libxmimsim_la_SOURCES += xmi_random.c \
			 xmi_random_dev.c \
			 xmi_random_dev.h 
else
libxmimsim_la_SOURCES += xmi_random_win.c \
			 xmi_registry_win.c
endif

if ENABLE_MAC_APP
libxmimsim_la_SOURCES += xmi_resources_mac.c 
endif


libxmimsim_la_CFLAGS =  @H5FC_INCLUDE@ \
			@PTHREAD_CFLAGS@ \
			@xslt_CFLAGS@ \
			@gsl_CFLAGS@ \
			@glib2_CFLAGS@ \
			@xraylib_CFLAGS@ \
			@GTK_MAC_CFLAGS@ \
		 	@gtk3extra_CFLAGS@ \
			@JsonGlib_CFLAGS@ \
		      	@libcurl_CFLAGS@ \
			-DXMI_CATALOG=\"$(datadir)/xmimsim/xmimsim_cat.xml\" \
			-DXMI_XMSO2XMSI_XSLT=\"$(datadir)/xmimsim/xmso2xmsi.xml\" \
			-DXMI_XMSO2SVG_XSLT=\"$(datadir)/xmimsim/xmso2svg.xml\" \
			-DXMI_XMSO2SPE_XSLT=\"$(datadir)/xmimsim/xmso2spe.xml\" \
			-DXMI_XMSO2CSV_XSLT=\"$(datadir)/xmimsim/xmso2csv.xml\" \
			-DXMI_XMSO2HTM_XSLT=\"$(datadir)/xmimsim/xmso2htm.xml\" \
			-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\"
libxmimsim_la_FCFLAGS = @H5FC_INCLUDE@ \
			@OPENMP_FCFLAGS@ \
			@fgsl_CFLAGS@ \
			@xraylib_f2003_CFLAGS@ \
			-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\"
libxmimsim_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@ @LDFLAGS_XMIMSIM_LIB@
libxmimsim_la_LIBADD = 	@H5FC_LINKER@ \
			@PTHREAD_LIBS@ \
			@xslt_LIBS@ \
			@xraylib_f2003_LIBS@ \
			@H5FC_STATIC@ \
			@fgsl_LIBS@ \
			@glib2_LIBS@ \
			@GTK_MAC_LIBS@

if OS_WINDOWS
libxmimsim_la_FCFLAGS += -DXMI_WINDOWS 
endif
if ENABLE_MAC_APP
libxmimsim_la_CFLAGS += -xobjective-c
libxmimsim_la_LDFLAGS += -Wl,-headerpad_max_install_names
endif

xmimsim_main.mod: libxmimsim.la

xmimsim_aux.mod: libxmimsim.la

xmimsim_solid_angle.mod: libxmimsim.la

xmimsim_hdf5.mod: libxmimsim.la

xmimsim_varred.mod: libxmimsim.la

xmimsim_detector.mod: libxmimsim.la


clean-local:
	rm -f xmimsim_main.mod xmimsim_aux.mod xmimsim_solid_angle.mod xmimsim_hdf5.mod xmimsim_varred.mod xmimsim_detector.mod
