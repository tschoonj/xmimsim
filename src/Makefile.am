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

NULL =

xmimsimincludedir = ${includedir}/xmimsim
xmimsimdir = $(datadir)/xmimsim

nodist_xmimsiminclude_HEADERS = xmimsim_main.mod xmimsim_aux.mod xmimsim_solid_angle.mod xmimsim_data.mod xmimsim_varred.mod xmimsim_detector.mod

AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include

plugindir = $(libdir)/xmimsim

lib_LTLIBRARIES = libxmimsim.la
noinst_LTLIBRARIES =
nodist_libxmimsim_la_SOURCES = xmi_resources.c xmi_resources.h
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
			xmi_private.h \
			xmi_spline.c \
			xmi_transmission_efficiency.c \
			xmi_gobject.c \
			xmi_job.c \
			xmi_type_builtins.c \
			xmi_type_builtins.h \
			xmi_batch.c \
			$(NULL)
if !OS_WINDOWS
libxmimsim_la_SOURCES += xmi_random.c \
			 xmi_random_dev.c \
			 xmi_random_dev.h
else
libxmimsim_la_SOURCES += xmi_random_win.c \
			 xmi_registry_win.c
endif

libxmimsim_la_CFLAGS =  \
	@HDF5_CFLAGS@ \
	@PTHREAD_CFLAGS@ \
	@xslt_CFLAGS@ \
	@glib2_CFLAGS@ \
	@xraylib_CFLAGS@ \
	@GTK_MAC_CFLAGS@ \
	@gtkmm3_CFLAGS@ \
	@JsonGlib_CFLAGS@ \
	@libsoup_CFLAGS@ \
	@gmodule_CFLAGS@ \
	@OPENMP_CFLAGS@ \
	-DXMI_CATALOG=\"$(datadir)/xmimsim/xmimsim_cat.xml\" \
	-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\" \
	-DXMI_OPENCL_LIB=\"$(libdir)/xmimsim\" \
	@WSTRICT_CFLAGS@

libxmimsim_la_FCFLAGS = @OPENMP_FCFLAGS@ \
			@fgsl_CFLAGS@ \
			@easyRNG_CFLAGS@ \
			@xraylib_f2003_CFLAGS@ \
			-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\"

libxmimsim_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@ @LDFLAGS_XMIMSIM_LIB@
libxmimsim_la_LIBADD = 	@HDF5_LIBS@ \
			@PTHREAD_LIBS@ \
			@xslt_LIBS@ \
			@xraylib_f2003_LIBS@ \
			@fgsl_LIBS@ \
			@easyRNG_LIBS@ \
			@glib2_LIBS@ \
			@gmodule_LIBS@

if ENABLE_OPENCL
plugin_LTLIBRARIES = xmimsim-cl.la
xmimsim_cl_la_CFLAGS = @WSTRICT_CFLAGS@
xmimsim_cl_la_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include @CPPFLAGS_CL@ @gmodule_CFLAGS@ @glib2_CFLAGS@ @GTK_MAC_CFLAGS@ 
xmimsim_cl_la_LIBADD = @LIBS_CL@ libxmimsim.la
xmimsim_cl_la_SOURCES = xmi_solid_angle_cl.c
xmimsim_cl_la_LDFLAGS = -module $(LDFLAGS_XMIMSIM_CL) -avoid-version -shared
if ENABLE_MAC_APP
xmimsim_cl_la_LDFLAGS += -Wl,-headerpad_max_install_names
endif
endif

if OS_WINDOWS
libxmimsim_la_FCFLAGS += -DXMI_WINDOWS
endif

if ENABLE_MAC_APP
libxmimsim_la_LDFLAGS += -Wl,-headerpad_max_install_names

noinst_LTLIBRARIES += libxmimsim-osx.la
libxmimsim_osx_la_SOURCES = xmi_resources_mac.m
libxmimsim_osx_la_CPPFLAGS = -DMAC_INTEGRATION $(AM_CPPFLAGS)
libxmimsim_osx_la_OBJCFLAGS =  \
	@glib2_CFLAGS@

libxmimsim_la_LIBADD +=	libxmimsim-osx.la
endif

if ENABLE_GOOGLE_ANALYTICS
noinst_LTLIBRARIES += libxmimsim-google-analytics.la
libxmimsim_google_analytics_la_SOURCES = \
		      xmi_google_analytics.c
libxmimsim_google_analytics_la_CFLAGS = \
	$(WSTRICT_CFLAGS) \
	$(NULL)
libxmimsim_google_analytics_la_CPPFLAGS = \
		$(AM_CPPFLAGS) \
		-I$(top_srcdir)/include \
		@GOOGLE_ANALYTICS_CFLAGS@
libxmimsim_google_analytics_la_LIBADD = @GOOGLE_ANALYTICS_LIBS@
libxmimsim_la_LIBADD += libxmimsim-google-analytics.la 
endif

# quicklook plugin library for macOS bundle
if ENABLE_QUICKLOOK
noinst_LTLIBRARIES += libxmimsim-ql.la
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
	@xraylib_CFLAGS@ \
	@WSTRICT_CFLAGS@
libxmimsim_ql_la_LIBADD = \
	@xml2_LIBS@ \
	@glib2_LIBS@
libxmimsim_ql_la_LDFLAGS = \
	-shared \
	-Wl,-headerpad_max_install_names \
	-framework CoreFoundation
endif

xmi_resources.c: xmi.gresource.xml $(shell $(GLIB_COMPILE_RESOURCES) --sourcedir=$(srcdir) --generate-dependencies $(srcdir)/xmi.gresource.xml)
	$(AM_V_GEN) $(GLIB_COMPILE_RESOURCES) --target=$@ --sourcedir=$(srcdir) --c-name=xmi --generate-source $(srcdir)/xmi.gresource.xml

xmi_resources.h: xmi.gresource.xml $(shell $(GLIB_COMPILE_RESOURCES) --sourcedir=$(srcdir) --generate-dependencies $(srcdir)/xmi.gresource.xml)
	$(AM_V_GEN) $(GLIB_COMPILE_RESOURCES) --target=$@ --sourcedir=$(srcdir) --c-name=xmi --generate-header $(srcdir)/xmi.gresource.xml

BUILT_SOURCES = $(nodist_libxmimsim_la_SOURCES)

xmimsim_main.mod: libxmimsim.la

xmimsim_aux.mod: libxmimsim.la

xmimsim_solid_angle.mod: libxmimsim.la

xmimsim_data.mod: libxmimsim.la

xmimsim_varred.mod: libxmimsim.la

xmimsim_detector.mod: libxmimsim.la

-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS =
INTROSPECTION_SCANNER_ARGS = \
	-I$(top_builddir) \
	--warn-all \
	--identifier-filter-cmd="python $(srcdir)/identfilter.py" \
	--symbol-filter-cmd="python $(srcdir)/symbolfilter.py" \
	--c-include=include/xmi_gobject.h \
	--c-include=include/xmi_data_structs.h \
	--accept-unprefixed \
	-v \
	$(NULL)
INTROSPECTION_COMPILER_ARGS =

if HAVE_INTROSPECTION
introspection_sources = \
	$(top_srcdir)/include/xmi_data_structs.h \
	$(top_srcdir)/include/xmi_job.h \
	$(top_srcdir)/include/xmi_gobject.h \
	xmi_type_builtins.h \
	xmi_private.h \
	$(top_srcdir)/include/xmi_xml.h \
	$(top_srcdir)/include/xmi_error.h \
	$(top_srcdir)/include/xmi_aux.h \
	$(top_srcdir)/include/xmi_data.h \
	xmi_data_structs.c \
	xmi_job.c \
	xmi_gobject.c \
	xmi_type_builtins.c \
	xmi_xml.c \
	xmi_aux.c \
	$(NULL)

if ENABLE_GOOGLE_ANALYTICS
introspection_sources += $(top_srcdir)/include/xmi_google_analytics.h xmi_google_analytics.c
endif

XmiMsim-1.0.gir: libxmimsim.la
XmiMsim_1_0_gir_INCLUDES = GObject-2.0 libxml2-2.0
XmiMsim_1_0_gir_CFLAGS = $(xraylib_CFLAGS)
XmiMsim_1_0_gir_LIBS = libxmimsim.la
XmiMsim_1_0_gir_FILES = $(introspection_sources)
INTROSPECTION_GIRS += XmiMsim-1.0.gir

girdir = $(datadir)/gir-1.0
gir_DATA = $(INTROSPECTION_GIRS)

typelibdir = $(libdir)/girepository-1.0
typelib_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

CLEANFILES = $(gir_DATA) $(typelib_DATA) $(INTROSPECTION_GIRS:.gir=.gir.files)

endif

#I delete the libtool archives on installation, so this step is necessary to ensure the libraries are uninstalled
uninstall-local:
	rm -rf $(DESTDIR)$(bindir)/libxmimsim*dll
	rm -rf $(DESTDIR)$(libdir)/libxmimsim*
	rm -rf $(DESTDIR)$(plugindir)/xmimsim-cl*

clean-local:
	rm -f xmimsim_main.mod xmimsim_aux.mod xmimsim_solid_angle.mod xmimsim_data.mod xmimsim_varred.mod xmimsim_detector.mod xmimsim_ebel.mod libxmimsim-*.def
	rm -f $(nodist_libxmimsim_la_SOURCES)
	rm -f $(CLEANFILES)

EXTRA_DIST = xmi.gresource.xml array.h compilerfeatures.h openclfeatures.h sse.h threefry.h xmi_kernels.cl identfilter.py symbolfilter.py

