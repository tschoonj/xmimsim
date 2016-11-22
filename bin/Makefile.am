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




AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include -I${top_srcdir}/src -I${top_builddir}/src -I${top_srcdir}/bin -DG_LOG_DOMAIN=\"XmiMsimGui\"
if ENABLE_MAC_APP
AM_LDFLAGS = -headerpad_max_install_names -framework Cocoa
endif

NULL=

#TODO:
# 1) create shared lib containing abstract source code
# 2) when compiling against gtkmm3, get also gtk3 headers from configure pkg-config
# 3) ensure this shared lib is pure C, just like the plugins that will link against it
# 4) install abstract source header
if ENABLE_GUI
lib_LTLIBRARIES = libxmimsim-gui.la
libxmimsim_gui_la_SOURCES = \
	xmimsim-gui-prefs.h \
	xmimsim-gui-prefs.c \
	xmimsim-gui-source-abstract.h \
	xmimsim-gui-source-abstract.c \
	xmimsim-gui-compound-dialog.c \
	xmimsim-gui-compound-dialog.h \
	xmimsim-gui-catalog-dialog.c \
	xmimsim-gui-catalog-dialog.h \
	xmimsim-gui-layer-dialog.c \
	xmimsim-gui-layer-dialog.h \
	xmimsim-gui-type-builtins.h \
	xmimsim-gui-type-builtins.c \
	$(NULL)
libxmimsim_gui_la_CFLAGS = \
	@gtk2_CFLAGS@ \
	@gtkmm3_CFLAGS@ \
	@GTK_MAC_CFLAGS@ \
	@xraylib_CFLAGS@ \
	$(NULL)
libxmimsim_gui_la_LDFLAGS = \
	@LDFLAGS_XMIMSIM_LIB@
libxmimsim_gui_la_LIBADD = \
	@gtk2_LIBS@ \
	@gtkmm3_LIBS@ \
	@GTK_MAC_LIBS@ \
	@xraylib_LIBS@ \
	../src/libxmimsim.la \
	$(NULL)

if ENABLE_UPDATER
libxmimsim_gui_la_SOURCES += xmimsim-gui-updater.c xmimsim-gui-updater.h
libxmimsim_gui_la_CFLAGS += \
	@JsonGlib_CFLAGS@ \
	@libcurl_CFLAGS@ \
	$(NULL)
if ENABLE_UPDATER
libxmimsim_gui_la_LIBADD += \
	@JsonGlib_LIBS@ \
	@libcurl_LIBS@ \
	$(NULL)
endif
endif

if ENABLE_MAC_APP
libxmimsim_gui_la_CFLAGS += -xobjective-c
endif

plugindir = $(libdir)/xmimsim-gui/sources
plugin_LTLIBRARIES = \
	xmimsim-gui-source-tube-ebel.la \
	$(NULL)
xmimsim_gui_source_tube_ebel_la_CFLAGS = \
	@gtk2_CFLAGS@ \
	@gtkmm3_CFLAGS@ \
	@xraylib_CFLAGS@ \
	$(NULL)
xmimsim_gui_source_tube_ebel_la_SOURCES = \
	xmimsim-gui-source-tube-ebel.h \
	xmimsim-gui-source-tube-ebel.c \
	$(NULL)
xmimsim_gui_source_tube_ebel_la_LDFLAGS = -module -avoid-version -shared \
	@LDFLAGS_XMIMSIM_CL@
xmimsim_gui_source_tube_ebel_la_LIBADD = libxmimsim-gui.la ../src/libxmimsim.la
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

dist_noinst_DATA = coordinate_system.png

if ENABLE_GUI
bin_PROGRAMS += xmimsim-gui
BUILT_SOURCES = $(nodist_xmimsim_gui_SOURCES) xmimsim-gui-type-builtins.h xmimsim-gui-type-builtins.c
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
	xmimsim-gui.c \
	xmimsim-gui-energies.c \
	xmimsim-gui-energies.h \
	xmimsim-gui.h \
	xmimsim-gui-controls.c \
	xmimsim-gui-controls.h \
	xmimsim-gui-results.c \
	xmimsim-gui-results.h \
	xmimsim-gui-tools.c \
	xmimsim-gui-tools.h \
	xmimsim-gui-notifications.c \
	xmimsim-gui-notifications.h \
	xmimsim-gui-fonts.h \
	xmimsim-gui-export-canvas-dialog.c \
	xmimsim-gui-export-canvas-dialog.h \
	xmimsim-gui-sources.c \
	xmimsim-gui-sources.h \
	xmimsim-gui-batch.h \
	xmimsim-gui-batch.c \
	$(NULL)

nodist_xmimsim_gui_SOURCES = \
	xmimsim-gui-coordinate-system.h \
	$(NULL)


mkenum_sources = \
	xmimsim-gui-compound-dialog.h \
	xmimsim-gui-layer-dialog.h \
	$(NULL)

# this is a separate convenience library as it allows me to test against it test-cubic-spline
noinst_LTLIBRARIES = libxmimsim-gui-spline.la
libxmimsim_gui_spline_la_SOURCES = \
		      xmimsim-gui-spline.h \
		      xmimsim-gui-spline.c
libxmimsim_gui_spline_la_CPPFLAGS = \
		     -I$(top_srcdir)/include \
		     @xraylib_CFLAGS@
libxmimsim_gui_spline_la_LDFLAGS = -static
libxmimsim_gui_spline_la_LIBADD = ../src/libxmimsim.la



xmimsim_gui_CFLAGS = \
		     @gtk2_CFLAGS@ \
		     @gtkmm3_CFLAGS@ \
		     @xraylib_CFLAGS@ \
		     @GTK_MAC_CFLAGS@ \
		     @xml2_CFLAGS@ \
		     -DXMIMSIM_ICON_DEFAULT=\"$(datadir)/xmimsim/Logo_xmi_msim.png\" \
		     @libnotify_CFLAGS@

if ENABLE_CXX
XMICCLD = $(CXX)
if ENABLE_MAC_APP
xmimsim_gui_CFLAGS += -xobjective-c++ -std=gnu++11 -stdlib=libc++
xmimsim_gui_LDFLAGS = -stdlib=libc++
else
xmimsim_gui_CFLAGS += -xc++ -std=gnu++11
endif
else
XMICCLD = $(CCLD)
if ENABLE_MAC_APP
xmimsim_gui_CFLAGS += -xobjective-c
endif
endif


xmimsim_gui_LDADD = libxmimsim-gui-spline.la \
		    libxmimsim-gui.la \
		    @gtk2_LIBS@ \
		    @gtkmm3_LIBS@ \
		    @GTK_MAC_LIBS@ \
		    @xraylib_LIBS@ \
		    @gthread2_LIBS@ \
		    @xml2_LIBS@ \
		    @libnotify_LIBS@ \
		    ../src/libxmimsim.la




if OS_WINDOWS
xmimsim_gui_LDFLAGS = -Wl,-subsystem,windows
xmimsim_gui_SOURCES += xmimsim-icon.rc
xmimsim_gui_LDADD += xmimsim-icon.o
nodist_xmimsim_gui_SOURCES += xmimsim-gui-icons.h
xmimsim_gui_CFLAGS += -I$(builddir)  -D_WIN32_WINNT=0x0501
endif

#next line is necessary to avoid the -xobjective-c flag to be passed to the linker
xmimsim_gui_LINK = $(LIBTOOL) --tag=CC $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=link $(XMICCLD) \
	$(AM_LDFLAGS) $(LDFLAGS) \
	$(xmimsim_gui_LDFLAGS) -o $@

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


if OS_WINDOWS
xmimsim-icon.o: xmimsim-icon.rc
	$(WINDRES) -v --use-temp-file -F $(WINDRES_ARCH) \
	-DXMIMSIM_WIN_ICO=\"${top_srcdir}/icons/Logo_xmi_msim_Win7.ico\" \
	-DXMIMSIM_WIN_ICO_XMSI=\"${top_srcdir}/icons/Logo_xmi_msim_Win7_XMSI.ico\" \
	-DXMIMSIM_WIN_ICO_XMSO=\"${top_srcdir}/icons/Logo_xmi_msim_Win7_XMSO.ico\" \
	-DXMIMSIM_WIN_ICO_XMSA=\"${top_srcdir}/icons/Logo_xmi_msim_Win7_XMSA.ico\" \
	${srcdir}/xmimsim-icon.rc -O coff -o xmimsim-icon.o


xmimsim-gui.c: xmimsim-gui-icons.h xmimsim-gui-coordinate-system.h

xmimsim-gui-icons.h:
	$(GDK_PIXBUF_CSOURCE) --stream --build-list \
	"Radiation_warning_symbol_pixbuf" "$(top_srcdir)/icons/256x256/Radiation_warning_symbol.png" \
	"Logo_xmi_msim_pixbuf" "$(top_srcdir)/icons/256x256/Logo_xmi_msim.png" \
	"Logo_xmi_msim_red_pixbuf" "$(top_srcdir)/icons/256x256/Logo_xmi_msim_red.png" \
	"Logo_xmi_msim_archive_pixbuf" "$(top_srcdir)/icons/256x256/Logo_xmi_msim_archive.png" \
	> xmimsim-gui-icons.h

endif

xmimsim-gui-coordinate-system.h: $(srcdir)/coordinate_system.png
	$(GDK_PIXBUF_CSOURCE) --stream --build-list \
	"coordinate_system_pixbuf" "$(srcdir)/coordinate_system.png" \
	> xmimsim-gui-coordinate-system.h

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
		--fhead "#include \"xmimsim-gui-type-builtins.h\"\n#include \"xmimsim-gui-compound-dialog.h\"\n#include \"xmimsim-gui-layer-dialog.h\"" \
		--fprod "\n/* enumerations from \"@filename@\" */" \
		--vhead "GType\n@enum_name@_get_type (void)\n{\n  static GType etype = 0;\n  if (etype == 0) {\n    static const G@Type@Value values[] = {" \
		--vprod "      { @VALUENAME@, \"@VALUENAME@\", \"@valuenick@\" }," \
		--vtail "      { 0, NULL, NULL }\n    };\n    etype = g_@type@_register_static(g_intern_static_string(\"@EnumName@\"), values);\n  }\n  return etype;\n}\n" \
		--identifier-prefix "XmiMsimGui" \
		--symbol-prefix "xmi_msim_gui" \
		$(mkenum_sources) ) > $(srcdir)/xmimsim-gui-type-builtins.c


clean-local:
	rm -rf xmimsim-gui-icons.h $(nodist_xmimsim_gui_SOURCES)

#install-data-hook:
#	chmod a+rw  $(DESTDIR)/$(datadir)/xmimsim/xmimsim-solid-angles.h5
