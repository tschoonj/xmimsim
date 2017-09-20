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
xmimsimdir = $(datadir)/xmimsim

nodist_xmimsiminclude_HEADERS = xmimsim_main.mod xmimsim_aux.mod xmimsim_solid_angle.mod xmimsim_data.mod xmimsim_varred.mod xmimsim_detector.mod

BUILT_SOURCES = $(nodist_xmimsiminclude_HEADERS)

if ENABLE_OPENCL
dist_xmimsim_DATA = array.h compilerfeatures.h openclfeatures.h sse.h threefry.h xmi_kernels.cl
endif

AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include

plugindir = $(libdir)/xmimsim

lib_LTLIBRARIES = libxmimsim.la
libxmimsim_la_SOURCES =	xmi_xml.c \
			xmi_data_structs.c \
			xmi_aux.c \
			xmi_aux_f.F90 \
			xmi_data_f.F90 \
			xmi_data.c \
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
			xmi_ebel.F90 \
			xmi_xrmc.c \
			xmi_private.h

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

libxmimsim_la_CFLAGS =  \
	@H5CC_INCLUDE@ \
	@PTHREAD_CFLAGS@ \
	@xslt_CFLAGS@ \
	@glib2_CFLAGS@ \
	@xraylib_CFLAGS@ \
	@GTK_MAC_CFLAGS@ \
	@gtk2_CFLAGS@ \
	@gtkmm3_CFLAGS@ \
	@JsonGlib_CFLAGS@ \
	@libcurl_CFLAGS@ \
	@gmodule_CFLAGS@ \
	@OPENMP_CFLAGS@ \
	-DXMI_CATALOG=\"$(datadir)/xmimsim/xmimsim_cat.xml\" \
	-DXMI_XMSO2XMSI_XSLT=\"$(datadir)/xmimsim/xmso2xmsi.xml\" \
	-DXMI_XMSO2SVG_XSLT=\"$(datadir)/xmimsim/xmso2svg.xml\" \
	-DXMI_XMSO2SPE_XSLT=\"$(datadir)/xmimsim/xmso2spe.xml\" \
	-DXMI_XMSO2CSV_XSLT=\"$(datadir)/xmimsim/xmso2csv.xml\" \
	-DXMI_XMSO2HTM_XSLT=\"$(datadir)/xmimsim/xmso2htm.xml\" \
	-DXMI_XMSA2XMSO_XSLT=\"$(datadir)/xmimsim/xmsa2xmso.xml\" \
	-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\" \
	-DXMI_OPENCL_CODE=\"$(datadir)/xmimsim\" \
	-DXMI_OPENCL_LIB=\"$(libdir)/xmimsim\"

libxmimsim_la_FCFLAGS = @OPENMP_FCFLAGS@ \
			@fgsl_CFLAGS@ \
			@easyRNG_CFLAGS@ \
			@xraylib_f2003_CFLAGS@ \
			-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\"

libxmimsim_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@ @LDFLAGS_XMIMSIM_LIB@
libxmimsim_la_LIBADD = 	@H5CC_LINKER@ \
			@PTHREAD_LIBS@ \
			@xslt_LIBS@ \
			@xraylib_f2003_LIBS@ \
			@H5CC_STATIC@ \
			@fgsl_LIBS@ \
			@easyRNG_LIBS@ \
			@glib2_LIBS@ \
			@gmodule_LIBS@

if ENABLE_OPENCL
plugin_LTLIBRARIES = xmimsim-cl.la
xmimsim_cl_la_CFLAGS = @CPPFLAGS_CL@ @gmodule_CFLAGS@ @glib2_CFLAGS@ @GTK_MAC_CFLAGS@
xmimsim_cl_la_CPPFLAGS = -DXMIMSIM_CL -I${top_srcdir}/include -I${top_builddir}/include -DXMI_OPENCL_CODE=\"$(datadir)/xmimsim\"
xmimsim_cl_la_LIBADD = @LIBS_CL@ libxmimsim.la
xmimsim_cl_la_SOURCES = xmi_solid_angle.c
xmimsim_cl_la_LDFLAGS = -module $(LDFLAGS_XMIMSIM_CL) -avoid-version -shared
if ENABLE_MAC_APP
xmimsim_cl_la_LDFLAGS += -Wl,-headerpad_max_install_names
endif
endif

if OS_WINDOWS
libxmimsim_la_FCFLAGS += -DXMI_WINDOWS
endif
if ENABLE_MAC_APP
libxmimsim_la_CFLAGS += -xobjective-c
libxmimsim_la_LDFLAGS += -Wl,-headerpad_max_install_names
endif

# quicklook plugin library for macOS bundle
if ENABLE_QUICKLOOK
noinst_LTLIBRARIES = libxmimsim-ql.la
libxmimsim_ql_la_SOURCES = \
	xmi_xml.c \
	xmi_data_structs.c \
	xmi_lines.h \
	xmi_lines.c
libxmimsim_ql_la_CPPFLAGS = -DQUICKLOOK=1 \
	-I${top_srcdir}/include -I${top_builddir}/include
libxmimsim_ql_la_CFLAGS = \
	@xml2_CFLAGS@ \
	@glib2_CFLAGS@ \
	@xraylib_CFLAGS@
libxmimsim_ql_la_LIBADD = \
	@xml2_LIBS@ \
	@glib2_LIBS@
libxmimsim_ql_la_LDFLAGS = \
	-shared \
	-Wl,-headerpad_max_install_names \
	-framework CoreFoundation
endif



xmimsim_main.mod: libxmimsim.la

xmimsim_aux.mod: libxmimsim.la

xmimsim_solid_angle.mod: libxmimsim.la

xmimsim_data.mod: libxmimsim.la

xmimsim_varred.mod: libxmimsim.la

xmimsim_detector.mod: libxmimsim.la

#I delete the libtool archives on installation, so this step is necessary to ensure the libraries are uninstalled
uninstall-local:
	rm -rf $(DESTDIR)$(bindir)/libxmimsim*dll
	rm -rf $(DESTDIR)$(libdir)/libxmimsim*
	rm -rf $(DESTDIR)$(plugindir)/xmimsim-cl*

clean-local:
	rm -f xmimsim_main.mod xmimsim_aux.mod xmimsim_solid_angle.mod xmimsim_data.mod xmimsim_varred.mod xmimsim_detector.mod xmimsim_ebel.mod libxmimsim-*.def
