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




AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include -I${top_srcdir}/src -I${top_builddir}/src -I${top_srcdir}/bin
if ENABLE_MAC_APP
AM_LDFLAGS = -headerpad_max_install_names
endif

bin_PROGRAMS =  xmimsim \
		xmimsim-db \
		xmimsim-pymca \
		xmso2svg \
		xmso2xmsi \
		xmso2spe \
		xmso2csv \
		xmso2htm \
		xmso2xmsi

bin_PROGRAMS += xmimsim-test-ebel \
		xmimsim-test-boone1 \
		xmimsim-test-boone2

if ENABLE_GUI
bin_PROGRAMS += xmimsim-gui
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


xmimsim_gui_SOURCES = xmimsim-gui.c \
		      xmimsim-gui-layer.c \
		      xmimsim-gui-layer.h \
		      xmimsim-gui-energies.c \
		      xmimsim-gui-energies.h \
		      xmimsim-gui.h \
		      xmimsim-gui-controls.c \
		      xmimsim-gui-controls.h \
		      xmimsim-gui-results.c \
		      xmimsim-gui-results.h \
		      xmimsim-gui-prefs.h \
		      xmimsim-gui-prefs.c \
		      xmimsim-gui-tools.c \
		      xmimsim-gui-tools.h \
		      xmimsim-gui-batch.h \
		      xmimsim-gui-batch.c
if ENABLE_UPDATER
xmimsim_gui_SOURCES += xmimsim-gui-updater.c xmimsim-gui-updater.h
endif

xmimsim_gui_CFLAGS = @gtk3extra_CFLAGS@ \
		     @gtk2_CFLAGS@ \
		     @xraylib_CFLAGS@ \
		     @GTK_MAC_CFLAGS@ \
		     @xml2_CFLAGS@ \
		     -DXMIMSIM_ICON_DEFAULT=\"$(datadir)/xmimsim/Logo_xmi_msim.png\"
if ENABLE_UPDATER
xmimsim_gui_CFLAGS += @JsonGlib_CFLAGS@ \
		      @libcurl_CFLAGS@
endif

xmimsim_gui_LDADD = ../src/libxmimsim.la \
		    @gtk3extra_LIBS@ \
		    @gtk2_LIBS@ \
		    @GTK_MAC_LIBS@ \
		    @xraylib_LIBS@ \
		    @gthread2_LIBS@
if ENABLE_UPDATER
xmimsim_gui_LDADD += @JsonGlib_LIBS@ \
		     @libcurl_LIBS@
endif
if OS_WINDOWS
xmimsim_gui_LDFLAGS = -Wl,-subsystem,windows
xmimsim_gui_SOURCES += xmimsim-icon.rc 
xmimsim_gui_LDADD += xmimsim-icon.o
nodist_xmimsim_gui_SOURCES = xmimsim-gui-icons.h
xmimsim_gui_CFLAGS += -I$(builddir)
endif

if ENABLE_MAC_APP
xmimsim_gui_CFLAGS += -xobjective-c
#next line is necessary to avoid the -xobjective-c flag to be passed to the linker
xmimsim_gui_LINK = $(LIBTOOL) --tag=CC $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=link $(CCLD) \
	$(AM_LDFLAGS) $(LDFLAGS) -o $@
else
xmimsim_gui_LINK = $(LIBTOOL) --tag=CC $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=link $(CCLD) \
	$(AM_LDFLAGS) $(LDFLAGS) $(xmimsim_gui_CFLAGS) \
	$(xmimsim_gui_LDFLAGS) -o $@
endif

#xmimsim_test_SOURCES = xmimsim-test.c
#xmimsim_test_LDADD = ../src/libxmimsim.la

#xmimsim_test_xml_SOURCES = xmimsim-test-xml.c
#xmimsim_test_xml_LDADD = ../src/libxmimsim.la

xmimsim_SOURCES = xmimsim.c
xmimsim_CFLAGS = @OPENMPI_CFLAGS@ \
		 @xml2_CFLAGS@ \
		 @gsl_CFLAGS@ \
		 @glib2_CFLAGS@ \
		 @xraylib_CFLAGS@ \
		 @GTK_MAC_CFLAGS@ \
		-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\" 
xmimsim_LDADD = ../src/libxmimsim.la \
		@OPENMPI_LIBADD@ \
		@glib2_LIBS@ \
		@GTK_MAC_LIBS@
xmimsim_LDFLAGS = @OPENMPI_LDFLAGS@
if OS_WINDOWS
xmimsim_LDFLAGS += -Wl,-subsystem,windows
endif

xmimsim_cli_SOURCES = $(xmimsim_SOURCES)
xmimsim_cli_CFLAGS = $(xmimsim_CFLAGS)
xmimsim_cli_LDADD = $(xmimsim_LDADD)
xmimsim_cli_LDFLAGS = @OPENMPI_LDFLAGS@

xmimsim_pymca_SOURCES = xmimsim-pymca.c
xmimsim_pymca_LDADD = ../src/libxmimsim.la \
		      @GTK_MAC_LIBS@ \
		      @xraylib_LIBS@ \
		      @glib2_LIBS@
xmimsim_pymca_CFLAGS = @glib2_CFLAGS@ \
		       @xraylib_CFLAGS@ \
		       @GTK_MAC_CFLAGS@ \
		       @xml2_CFLAGS@ \
		       -DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\" 


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
		  @xraylib_CFLAGS@ 
		
xmimsim_test_ebel_SOURCES = xmimsim-test-ebel.c
xmimsim_test_ebel_LDADD = ../src/libxmimsim.la 

xmimsim_test_boone1_SOURCES = xmimsim-test-boone1.c
xmimsim_test_boone1_LDADD = ../src/libxmimsim.la 

xmimsim_test_boone2_SOURCES = xmimsim-test-boone2.c
xmimsim_test_boone2_LDADD = ../src/libxmimsim.la 


if OS_WINDOWS
xmimsim-icon.o: xmimsim-icon.rc
	$(WINDRES) -v --use-temp-file -F pe-i386 \
	-DXMIMSIM_WIN_ICO=\"${top_srcdir}/icons/Logo_xmi_msim_Win7.ico\" \
	-DXMIMSIM_WIN_ICO_XMSI=\"${top_srcdir}/icons/Logo_xmi_msim_Win7_XMSI.ico\" \
	-DXMIMSIM_WIN_ICO_XMSO=\"${top_srcdir}/icons/Logo_xmi_msim_Win7_XMSO.ico\" \
	${srcdir}/xmimsim-icon.rc -O coff -o xmimsim-icon.o	


xmimsim-gui.c: xmimsim-gui-icons.h

xmimsim-gui-icons.h:
	$(GDK_PIXBUF_CSOURCE) --struct --build-list \
	"Radiation_warning_symbol_pixbuf" "$(top_srcdir)/icons/256x256/Radiation_warning_symbol.png" \
	"Logo_xmi_msim_pixbuf" "$(top_srcdir)/icons/256x256/Logo_xmi_msim.png" \
	> xmimsim-gui-icons.h
endif


clean-local:
	rm -rf xmimsim-gui-icons.h

#install-data-hook:
#	chmod a+rw  $(DESTDIR)/$(datadir)/xmimsim/xmimsim-solid-angles.h5



