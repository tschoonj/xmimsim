#Copyright (C) 2010-2018 Tom Schoonjans and Laszlo Vincze

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




AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include -I${top_srcdir}/src -I${top_builddir}/src -I${top_srcdir}/bin -DG_LOG_DOMAIN=\"XmiMsimGui\"
AM_CXXFLAGS = -std=gnu++11
if ENABLE_MAC_APP
AM_LDFLAGS = -Wl,-headerpad_max_install_names -framework Cocoa
endif

NULL=

noinst_LTLIBRARIES=$(NULL)

if ENABLE_GUI
libxmimsim_gui_includedir = ${includedir}/xmimsim-gui

libxmimsim_gui_include_HEADERS = \
	xmimsim-gui-prefs.h \
	xmimsim-gui-source-abstract.h \
	xmimsim-gui-compound-dialog.h \
	xmimsim-gui-catalog-dialog.h \
	xmimsim-gui-layer-dialog.h \
	xmimsim-gui-type-builtins.h \
	xmimsim-gui-source-module.h \
	xmimsim-gui-utils.h \
	xmimsim-gui-export-canvas-dialog.h \
	xmimsim-gui-tools.h \
	xmimsim-gui-sources-dialog.h \
	xmimsim-gui-discrete-energy-dialog.h \
	xmimsim-gui-continuous-energy-dialog.h \
	xmimsim-gui-compat.h \
	xmimsim-gui-colors.h \
	xmimsim-gui-xmsi-scrolled-window.h \
	xmimsim-gui-options-box.h \
	xmimsim-gui-xmsa-viewer-window.h \
	xmimsim-gui-xmso-results-scrolled-window.h \
	xmimsim-gui-batch.h \
	xmimsim-gui-undo-manager.h \
	xmimsim-gui-energies-box.h \
	xmimsim-gui-layer-box.h \
	xmimsim-gui-clipboard-manager.h \
	xmimsim-gui-long-task-window.h \
	xmimsim-gui-controls-scrolled-window.h \
	xmimsim-gui-xmsi-config-scrolled-window.h \
	xmimsim-gui-application.h \
	xmimsim-gui-application-window.h \
	$(NULL)

lib_LTLIBRARIES = libxmimsim-gui.la

libxmimsim_gui_la_SOURCES = \
	$(libxmimsim_gui_include_HEADERS) \
	xmimsim-gui-options-box.c \
	xmimsim-gui-prefs.c \
	xmimsim-gui-source-abstract.c \
	xmimsim-gui-colors.c \
	xmimsim-gui-compound-dialog.c \
	xmimsim-gui-catalog-dialog.c \
	xmimsim-gui-layer-dialog.c \
	xmimsim-gui-type-builtins.c \
	xmimsim-gui-source-module.c \
	xmimsim-gui-utils.c \
	xmimsim-gui-export-canvas-dialog.cpp \
	xmimsim-gui-tools.c \
	xmimsim-gui-sources-dialog.cpp \
	xmimsim-gui-marshal.h \
	xmimsim-gui-marshal.c \
	xmimsim-gui-discrete-energy-dialog.c \
	xmimsim-gui-continuous-energy-dialog.c \
	xmimsim-gui-xmsi-scrolled-window.c \
	xmimsim-gui-xmsa-viewer-window.cpp \
	xmimsim-gui-xmso-results-scrolled-window.cpp \
	xmimsim-gui-batch.c \
	xmimsim-gui-undo-manager.c \
	xmimsim-gui-energies-box.c \
	xmimsim-gui-layer-box.c \
	xmimsim-gui-clipboard-manager.c \
	xmimsim-gui-long-task-window.c \
	xmimsim-gui-controls-scrolled-window.c \
	xmimsim-gui-xmsi-config-scrolled-window.c \
	xmimsim-gui-application.c \
	xmimsim-gui-application-window.c \
	xmimsim-gui-private.h \
	$(NULL)

nodist_libxmimsim_gui_la_SOURCES = \
	xmimsim-gui-resources.c \
	$(NULL)

libxmimsim_gui_la_CFLAGS = \
	$(WSTRICT_CFLAGS) \
	$(NULL)
libxmimsim_gui_la_CPPFLAGS = \
	$(AM_CPPFLAGS) \
	@gtkmm3_CFLAGS@ \
	@GTK_MAC_CFLAGS@ \
	@xraylib_CFLAGS@ \
	@xml2_CFLAGS@ \
	@libsoup_CFLAGS@ \
	-DXMIMSIM_SOURCES_DEFAULT=\"$(plugindir)\" \
	$(NULL)

libxmimsim_gui_la_LDFLAGS = \
	@LDFLAGS_XMIMSIM_GUI_LIB@ \
	$(EXTRA_LDFLAGS) \
	$(AM_LDFLAGS) \
	$(NULL)
libxmimsim_gui_la_LIBADD = \
	@gtkmm3_LIBS@ \
	@GTK_MAC_LIBS@ \
	@xraylib_LIBS@ \
	@xml2_LIBS@ \
	@libsoup_LIBS@ \
	../src/libxmimsim.la \
	$(NULL)

plugindir = $(libdir)/xmimsim-gui/sources
plugin_LTLIBRARIES = \
	xmimsim-gui-source-tube-ebel.la \
	xmimsim-gui-source-radionuclide.la \
	$(NULL)

xmimsim_gui_source_tube_ebel_la_CFLAGS = \
	$(WSTRICT_CFLAGS) \
	$(NULL)
xmimsim_gui_source_tube_ebel_la_CPPFLAGS = \
	$(AM_CPPFLAGS) \
	@gtkmm3_CFLAGS@ \
	@xraylib_CFLAGS@ \
	@xml2_CFLAGS@ \
	$(NULL)
xmimsim_gui_source_tube_ebel_la_SOURCES = \
	xmimsim-gui-source-tube-ebel.h \
	xmimsim-gui-source-tube-ebel.c \
	$(NULL)
xmimsim_gui_source_tube_ebel_la_LDFLAGS = -module -avoid-version -shared \
	@LDFLAGS_XMIMSIM_CL@ \
	$(AM_LDFLAGS) \
	$(NULL)
xmimsim_gui_source_tube_ebel_la_LIBADD = libxmimsim-gui.la ../src/libxmimsim.la

xmimsim_gui_source_radionuclide_la_CFLAGS = \
	$(WSTRICT_CFLAGS) \
	$(NULL)
xmimsim_gui_source_radionuclide_la_CPPFLAGS = \
	$(AM_CPPFLAGS) \
	@gtkmm3_CFLAGS@ \
	@xraylib_CFLAGS@ \
	@xml2_CFLAGS@ \
	$(NULL)
xmimsim_gui_source_radionuclide_la_SOURCES = \
	xmimsim-gui-source-radionuclide.h \
	xmimsim-gui-source-radionuclide.c \
	$(NULL)
xmimsim_gui_source_radionuclide_la_LDFLAGS = -module -avoid-version -shared \
	@LDFLAGS_XMIMSIM_CL@ \
	$(AM_LDFLAGS) \
	$(NULL)
xmimsim_gui_source_radionuclide_la_LIBADD = libxmimsim-gui.la ../src/libxmimsim.la
		    
noinst_LTLIBRARIES += \
	xmimsim-gui-source-random.la
xmimsim_gui_source_random_la_CFLAGS = \
	$(WSTRICT_CFLAGS) \
	$(NULL)
xmimsim_gui_source_random_la_CPPFLAGS = \
	$(AM_CPPFLAGS) \
	@gtkmm3_CFLAGS@ \
	@xml2_CFLAGS@ \
	@fgsl_CFLAGS@ \
	@easyRNG_CFLAGS@ \
	@xraylib_CFLAGS@ \
	$(NULL)
xmimsim_gui_source_random_la_SOURCES = \
	xmimsim-gui-source-random.h \
	xmimsim-gui-source-random.c \
	$(NULL)
xmimsim_gui_source_random_la_LDFLAGS = -module -avoid-version -shared \
	@LDFLAGS_XMIMSIM_CL@ -rpath /tmp \
	$(NULL)
xmimsim_gui_source_random_la_LIBADD = libxmimsim-gui.la \
	../src/libxmimsim.la
	@fgsl_LIBS@ \
	@easyRNG_LIBS@ \
	@xraylib_LIBS@ \
	$(NULL)
endif

bin_PROGRAMS =  xmimsim \
		xmimsim-db \
		xmimsim-pymca \
		xmso2svg \
		xmso2xmsi \
		xmso2spe \
		xmso2csv \
		xmso2htm \
		xmso2xmsi \
		xmsi2xrmc \
		xmimsim-conv \
		xmsa2xmso \
		$(NULL)

noinst_PROGRAMS = $(NULL)
if ENABLE_GUI
noinst_PROGRAMS += xmimsim-gui-layer-box-test xmimsim-gui-controls-scrolled-window-test xmimsim-gui-xmsi-config-scrolled-window-test
endif
xmimsim_gui_layer_box_test_SOURCES = xmimsim-gui-layer-box-test.c
xmimsim_gui_layer_box_test_CPPFLAGS = \
	@xml2_CFLAGS@ \
	@gtkmm3_CFLAGS@ \
	-DSRM1155_XMSI=\"${top_srcdir}/examples/srm1155.xmsi\" \
	$(AM_CPPFLAGS) \
	$(NULL)
xmimsim_gui_layer_box_test_LDADD = \
	libxmimsim-gui.la \
	../src/libxmimsim.la \
	@gtkmm3_LIBS@ \
	@xml2_LIBS@ \
	$(NULL)

xmimsim_gui_controls_scrolled_window_test_SOURCES = xmimsim-gui-controls-scrolled-window-test.c
xmimsim_gui_controls_scrolled_window_test_CPPFLAGS = \
	@xml2_CFLAGS@ \
	@gtkmm3_CFLAGS@ \
	-DSRM1155_XMSI=\"${top_srcdir}/examples/srm1155.xmsi\" \
	$(AM_CPPFLAGS) \
	$(NULL)
xmimsim_gui_controls_scrolled_window_test_LDADD = \
	libxmimsim-gui.la \
	../src/libxmimsim.la \
	@gtkmm3_LIBS@ \
	@xml2_LIBS@ \
	$(NULL)

xmimsim_gui_xmsi_config_scrolled_window_test_SOURCES = xmimsim-gui-xmsi-config-scrolled-window-test.c
xmimsim_gui_xmsi_config_scrolled_window_test_CPPFLAGS = \
	@xml2_CFLAGS@ \
	@gtkmm3_CFLAGS@ \
	-DSRM1155_XMSI=\"${top_srcdir}/examples/srm1155.xmsi\" \
	$(AM_CPPFLAGS) \
	$(NULL)
xmimsim_gui_xmsi_config_scrolled_window_test_LDADD = \
	libxmimsim-gui.la \
	../src/libxmimsim.la \
	@gtkmm3_LIBS@ \
	@xml2_LIBS@ \
	$(NULL)

if ENABLE_GUI
bin_PROGRAMS += xmimsim-gui
BUILT_SOURCES = \
	$(nodist_xmimsim_gui_SOURCES) \
	$(nodist_libxmimsim_gui_la_SOURCES) \
	xmimsim-gui-type-builtins.h \
	xmimsim-gui-type-builtins.c \
	xmimsim-gui-marshal.h \
	xmimsim-gui-marshal.c \
	$(NULL)
endif

if !OS_WINDOWS
bin_PROGRAMS += xmimsim-harvester
else
bin_PROGRAMS += xmimsim-cli
endif


xmimsim_hdf5dir = $(datadir)/xmimsim

xmimsim_harvester_SOURCES = xmimsim-harvester.c
xmimsim_harvester_CFLAGS =  @glib2_CFLAGS@ @PTHREAD_CFLAGS@
xmimsim_harvester_LDADD = ../src/libxmimsim.la \
			  @glib2_LIBS@


xmimsim_gui_SOURCES = \
	xmimsim-gui-main.cpp
	$(NULL)

nodist_xmimsim_gui_SOURCES = \
	$(NULL)


mkenum_sources = \
	xmimsim-gui-compound-dialog.h \
	xmimsim-gui-layer-dialog.h \
	xmimsim-gui-discrete-energy-dialog.h \
	xmimsim-gui-continuous-energy-dialog.h \
	xmimsim-gui-layer-box.h \
	$(NULL)

marshal_input = \
	xmimsim-gui-marshal.list


xmimsim_gui_CPPFLAGS = \
	$(AM_CPPFLAGS) \
	@gtkmm3_CFLAGS@ \
	@xraylib_CFLAGS@ \
	@GTK_MAC_CFLAGS@ \
	@xml2_CFLAGS@ \
	-DXMIMSIM_ICON_DEFAULT=\"$(datadir)/xmimsim/Logo_xmi_msim.png\" \
	$(NULL)

xmimsim_gui_LDFLAGS = $(EXTRA_LDFLAGS)

if ENABLE_GUI
if OS_DARWIN
EXTRA_LDFLAGS = -stdlib=libc++

noinst_LTLIBRARIES += libxmimsim-gui-osx.la
libxmimsim_gui_osx_la_SOURCES = xmimsim-gui-osx.m xmimsim-gui-osx.h
libxmimsim_gui_osx_la_OBJCFLAGS = @gtkmm3_CFLAGS@
libxmimsim_gui_la_LIBADD += libxmimsim-gui-osx.la
endif
endif


xmimsim_gui_LDADD = libxmimsim-gui.la \
		    @gtkmm3_LIBS@ \
		    @GTK_MAC_LIBS@ \
		    @xraylib_LIBS@ \
		    @xml2_LIBS@ \
		    ../src/libxmimsim.la


if ENABLE_GUI
if ENABLE_UPDATER
libxmimsim_gui_include_HEADERS += xmimsim-gui-updater.h
libxmimsim_gui_la_SOURCES += xmimsim-gui-updater.c xmimsim-gui-updater.h
libxmimsim_gui_la_CPPFLAGS += \
	@JsonGlib_CFLAGS@ \
	$(NULL)
libxmimsim_gui_la_LIBADD += \
	@JsonGlib_LIBS@ \
	$(NULL)
endif
endif

if OS_WINDOWS
xmimsim_gui_LDFLAGS += -Wl,-subsystem,windows
xmimsim_gui_SOURCES += xmimsim-icon.rc
xmimsim_gui_LDADD += xmimsim-icon.o
xmimsim_gui_CPPFLAGS += -I$(builddir)  -D_WIN32_WINNT=0x0501
endif

#xmimsim_test_SOURCES = xmimsim-test.c
#xmimsim_test_LDADD = ../src/libxmimsim.la

#xmimsim_test_xml_SOURCES = xmimsim-test-xml.c
#xmimsim_test_xml_LDADD = ../src/libxmimsim.la

xmimsim_SOURCES = xmimsim.c
xmimsim_CFLAGS = @OPENMPI_CFLAGS@ \
		 @xml2_CFLAGS@ \
		 @glib2_CFLAGS@ \
		 @xraylib_CFLAGS@ \
		 @GTK_MAC_CFLAGS@ \
		 @OPENMP_CFLAGS@ \
		 @gmodule_CFLAGS@ \
		 -DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\"
xmimsim_LDADD = ../src/libxmimsim.la \
		@OPENMPI_LIBADD@ \
		@glib2_LIBS@ \
		@GTK_MAC_LIBS@ \
		@gmodule_LIBS@ \
		@xraylib_LIBS@
xmimsim_LDFLAGS = $(AM_LDFLAGS)
if !ENABLE_MAC_APP
xmimsim_LDFLAGS += @OPENMPI_LDFLAGS@
endif
if OS_WINDOWS
xmimsim_LDFLAGS += -Wl,-subsystem,windows
endif

xmimsim_cli_SOURCES = $(xmimsim_SOURCES)
xmimsim_cli_CFLAGS = $(xmimsim_CFLAGS)
xmimsim_cli_LDADD = $(xmimsim_LDADD)
xmimsim_cli_LDFLAGS = @OPENMPI_LDFLAGS@

xmimsim_conv_SOURCES = xmimsim-conv.c
xmimsim_conv_CFLAGS = $(xmimsim_CFLAGS)
xmimsim_conv_LDADD = $(xmimsim_LDADD)

xmimsim_pymca_SOURCES = xmimsim-pymca.c
xmimsim_pymca_LDADD = ../src/libxmimsim.la \
		      @GTK_MAC_LIBS@ \
		      @xraylib_LIBS@ \
		      @glib2_LIBS@
xmimsim_pymca_CFLAGS = @glib2_CFLAGS@ \
		       @xraylib_CFLAGS@ \
		       @GTK_MAC_CFLAGS@ \
		       @xml2_CFLAGS@ \
		       @OPENMP_CFLAGS@ \
		       -DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\"

xmsi2xrmc_SOURCES = xmsi2xrmc.c
xmsi2xrmc_LDADD = ../src/libxmimsim.la \
		  @GTK_MAC_LIBS@ \
		  @glib2_LIBS@
xmsi2xrmc_CFLAGS = @glib2_CFLAGS@ \
		   @xml2_CFLAGS@ \
		   @xraylib_CFLAGS@


xmso2xmsi_SOURCES = xmso2xmsi.c
xmso2xmsi_LDADD = ../src/libxmimsim.la \
		  @GTK_MAC_LIBS@ \
		  @glib2_LIBS@
xmso2xmsi_CFLAGS = @glib2_CFLAGS@ \
		   @xml2_CFLAGS@ \
		   @xraylib_CFLAGS@

xmso2svg_SOURCES = xmso2svg.c
xmso2svg_LDADD = ../src/libxmimsim.la \
		 @GTK_MAC_LIBS@ \
		 @glib2_LIBS@
xmso2svg_CFLAGS = @glib2_CFLAGS@ \
		  @xml2_CFLAGS@ \
		  @xraylib_CFLAGS@

xmso2spe_SOURCES = xmso2spe.c
xmso2spe_LDADD = ../src/libxmimsim.la \
		 @GTK_MAC_LIBS@ \
		 @glib2_LIBS@
xmso2spe_CFLAGS = @glib2_CFLAGS@ \
		  @xml2_CFLAGS@ \
		  @xraylib_CFLAGS@

xmso2csv_SOURCES = xmso2csv.c
xmso2csv_LDADD = ../src/libxmimsim.la \
		 @GTK_MAC_LIBS@ \
		 @glib2_LIBS@
xmso2csv_CFLAGS = @glib2_CFLAGS@ \
		  @xml2_CFLAGS@ \
		  @xraylib_CFLAGS@

xmso2htm_SOURCES = xmso2htm.c
xmso2htm_LDADD = ../src/libxmimsim.la \
		 @GTK_MAC_LIBS@ \
		 @glib2_LIBS@
xmso2htm_CFLAGS = @glib2_CFLAGS@ \
		  @xml2_CFLAGS@ \
		  @xraylib_CFLAGS@

xmimsim_db_SOURCES = xmimsim-db.c
xmimsim_db_LDADD = ../src/libxmimsim.la \
		 @GTK_MAC_LIBS@ \
		 @glib2_LIBS@
xmimsim_db_CFLAGS = @glib2_CFLAGS@ \
		    @xml2_CFLAGS@ \
		    @xraylib_CFLAGS@

xmsa2xmso_SOURCES = xmsa2xmso.c
xmsa2xmso_LDADD = ../src/libxmimsim.la \
		  @GTK_MAC_LIBS@ \
		  @glib2_LIBS@
xmsa2xmso_CFLAGS = @glib2_CFLAGS@ \
		   @xml2_CFLAGS@ \
		   @xraylib_CFLAGS@

#xmimsim_test_ebel_SOURCES = xmimsim-test-ebel.c
#xmimsim_test_ebel_LDADD = ../src/libxmimsim.la

#xmimsim_test_boone1_SOURCES = xmimsim-test-boone1.c
#xmimsim_test_boone1_LDADD = ../src/libxmimsim.la

#xmimsim_test_boone2_SOURCES = xmimsim-test-boone2.c
#xmimsim_test_boone2_LDADD = ../src/libxmimsim.la

xmimsimdir = $(datadir)/xmimsim
dist_xmimsim_DATA = coordinate_system.png

-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS =
INTROSPECTION_SCANNER_ARGS = --add-include-path=$(top_builddir)/src --warn-all
INTROSPECTION_COMPILER_ARGS = --includedir=$(top_builddir)/src 

#if ENABLE_GUI
#if HAVE_INTROSPECTION
#introspection_sources = xmimsim-gui-source-abstract.c xmimsim-gui-source-abstract.h

#XmiMsimGui-1.0.gir: libxmimsim-gui.la
#XmiMsimGui_1_0_gir_INCLUDES = GObject-2.0 XmiMsim-1.0 Gtk-3.0
#XmiMsimGui_1_0_gir_CFLAGS = $(INCLUDES) -I$(top_srcdir)/include
#XmiMsimGui_1_0_gir_LIBS = libxmimsim-gui.la
#XmiMsimGui_1_0_gir_FILES = $(introspection_sources)
#INTROSPECTION_GIRS += XmiMsimGui-1.0.gir

#girdir = $(datadir)/gir-1.0
#gir_DATA = $(INTROSPECTION_GIRS)

#typelibdir = $(libdir)/girepository-1.0
#typelib_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

#CLEANFILES = $(gir_DATA) $(typelib_DATA)
#endif
#endif

if OS_WINDOWS
xmimsim-icon.o: xmimsim-icon.rc
	$(WINDRES) -v --use-temp-file -F $(WINDRES_ARCH) \
	-DXMIMSIM_WIN_ICO=\"${top_srcdir}/icons/Logo_xmi_msim_Win7.ico\" \
	-DXMIMSIM_WIN_ICO_XMSI=\"${top_srcdir}/icons/Logo_xmi_msim_Win7_XMSI.ico\" \
	-DXMIMSIM_WIN_ICO_XMSO=\"${top_srcdir}/icons/Logo_xmi_msim_Win7_XMSO.ico\" \
	-DXMIMSIM_WIN_ICO_XMSA=\"${top_srcdir}/icons/Logo_xmi_msim_Win7_XMSA.ico\" \
	${srcdir}/xmimsim-icon.rc -O coff -o xmimsim-icon.o

endif

xmimsim-gui-type-builtins.h: $(mkenum_sources) xmimsim-gui-type-builtins.c


xmimsim-gui-type-builtins.c: $(mkenum_sources)
	( cd $(srcdir) && $(GLIB_MKENUMS) \
		--fhead "#ifndef __XMIMSIM_GUI_TYPE_BUILTINS_H__\n#define __XMIMSIM_GUI_TYPE_BUILTINS_H__\n\n#include <glib-object.h>\n#include <glib.h>\n\nG_BEGIN_DECLS\n\n" \
		--fprod "/* enumerations from \"@filename@\" */\n" \
		--vhead "GType @enum_name@_get_type (void);\n#define XMI_MSIM_GUI_TYPE_@ENUMSHORT@ (@enum_name@_get_type())\n" \
		--ftail "\nG_END_DECLS\n#endif /* __XMIMSIM_GUI_TYPE_BUILTINS_H__ */" \
		--identifier-prefix "XmiMsimGui" \
		--symbol-prefix "xmi_msim_gui" \
		$(mkenum_sources) ) > $(srcdir)/xmimsim-gui-type-builtins.h
	( cd $(srcdir) && $(GLIB_MKENUMS) \
		--fhead "#include \"xmimsim-gui-type-builtins.h\"\n#include \"xmimsim-gui-layer-box.h\"\n#include \"xmimsim-gui-compound-dialog.h\"\n#include \"xmimsim-gui-layer-dialog.h\"\n#include \"xmimsim-gui-discrete-energy-dialog.h\"\n#include \"xmimsim-gui-continuous-energy-dialog.h\"" \
		--fprod "\n/* enumerations from \"@filename@\" */" \
		--vhead "GType\n@enum_name@_get_type (void)\n{\n  static GType etype = 0;\n  if (etype == 0) {\n    static const G@Type@Value values[] = {" \
		--vprod "      { @VALUENAME@, \"@VALUENAME@\", \"@valuenick@\" }," \
		--vtail "      { 0, NULL, NULL }\n    };\n    etype = g_@type@_register_static(g_intern_static_string(\"@EnumName@\"), values);\n  }\n  return etype;\n}\n" \
		--identifier-prefix "XmiMsimGui" \
		--symbol-prefix "xmi_msim_gui" \
		$(mkenum_sources) ) > $(srcdir)/xmimsim-gui-type-builtins.c

xmimsim-gui-marshal.h: $(marshal_input) xmimsim-gui-marshal.c

xmimsim-gui-marshal.c: $(marshal_input)
	( cd $(srcdir) && $(GLIB_GENMARSHAL) \
		--prefix "xmi_msim_gui" \
		--header \
		$(marshal_input) ) > $(srcdir)/xmimsim-gui-marshal.h
	( cd $(srcdir) && $(GLIB_GENMARSHAL) \
		--prefix "xmi_msim_gui" \
		--prototypes \
		--body \
		$(marshal_input) ) > $(srcdir)/xmimsim-gui-marshal.c

xmimsim-gui-resources.c: xmimsim-gui.gresource.xml $(shell $(GLIB_COMPILE_RESOURCES) --sourcedir=$(srcdir) --generate-dependencies $(srcdir)/xmimsim-gui.gresource.xml)
	$(AM_V_GEN) $(GLIB_COMPILE_RESOURCES) --target=$@ --sourcedir=$(srcdir) --c-name=xmi_msim_gui --generate-source $(srcdir)/xmimsim-gui.gresource.xml

clean-local:
	rm -rf $(nodist_libxmimsim_gui_la_SOURCES) $(nodist_xmimsim_gui_SOURCES) libxmimsim-gui-0.def $(CLEANFILES)

EXTRA_DIST = xmimsim-gui-marshal.list xmimsim-gui.gresource.xml gtk-yes.png gtk-no.png menus-appmenu.ui menus-common.ui

