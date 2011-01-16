AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include -I${top_srcdir}/src -I${top_builddir}/src -I${top_srcdir}/bin

bin_PROGRAMS = xmimsim-db xmimsim-harvester \
	xmimsim-test xmimsim-test-xml \
	xmimsim xmimsim-test-alloc
if ENABLE_GUI
bin_PROGRAMS += xmimsim-gui
endif


xmimsim_db_SOURCES = xmimsim-db.F90
xmimsim_db_FCFLAGS = @fgsl_CFLAGS@ @xraylib_f2003_CFLAGS@ @H5FC_INCLUDE@ @OPENMP_FCFLAGS@
#xmimsim_db_LDADD = ../src/libxmimsim.la @H5FC_LINKER@ @H5FC_STATIC@
xmimsim_db_LDADD = ../src/libxmimsim.la 
xmimsim_db_LDFLAGS = 

xmimsim_harvester_SOURCES = xmimsim-harvester.c
xmimsim_harvester_LDADD = ../src/libxmimsim.la


xmimsim_gui_SOURCES = xmimsim-gui.c xmimsim-gui-layer.c xmimsim-gui-layer.h xmimsim-gui-energies.c xmimsim-gui-energies.h xmimsim-gui.h
xmimsim_gui_CFLAGS = @gtk2_CFLAGS@ @xraylib_CFLAGS@
xmimsim_gui_LDADD = ../src/libxmimsim.la @gtk2_LIBS@


xmimsim_test_SOURCES = xmimsim-test.c
xmimsim_test_LDADD = ../src/libxmimsim.la

xmimsim_test_xml_SOURCES = xmimsim-test-xml.c
xmimsim_test_xml_LDADD = ../src/libxmimsim.la

xmimsim_SOURCES = xmimsim.c
xmimsim_CFLAGS = @H5FC_INCLUDE@  @OPENMPI_CFLAGS@ @gsl_CFLAGS@ @OPENMP_CFLAGS@ @glib2_CFLAGS@ @xraylib_CFLAGS@ \
-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\"
xmimsim_LDADD = ../src/libxmimsim.la @OPENMPI_LIBADD@ @glib2_LIBS@
xmimsim_LDFLAGS = @OPENMPI_LDFLAGS@

xmimsim_test_alloc_SOURCES = xmimsim-test-alloc.F90
xmimsim_test_alloc_FCFLAGS = @fgsl_CFLAGS@ @xraylib_f2003_CFLAGS@ @H5FC_INCLUDE@ @OPENMP_FCFLAGS@
#xmimsim_db_LDADD = ../src/libxmimsim.la @H5FC_LINKER@ @H5FC_STATIC@
xmimsim_test_alloc_LDADD = ../src/libxmimsim.la 
