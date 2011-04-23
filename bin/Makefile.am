AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include -I${top_srcdir}/src -I${top_builddir}/src -I${top_srcdir}/bin

bin_PROGRAMS = xmimsim \
	xmimsim-db \
	xmimsim-harvester \
	xmimsim-pymca \
	xmso2xmsi
if ENABLE_GUI
bin_PROGRAMS += xmimsim-gui
endif

noinst_PROGRAMS = xmimsim-init-solid-angles

xmimsim_hdf5dir = $(datadir)/xmimsim
xmimsim_hdf5_DATA = xmimsim-solid-angles.h5  


xmimsim_db_SOURCES = xmimsim-db.F90
xmimsim_db_FCFLAGS = @fgsl_CFLAGS@ @xraylib_f2003_CFLAGS@ @H5FC_INCLUDE@ @OPENMP_FCFLAGS@
#xmimsim_db_LDADD = ../src/libxmimsim.la @H5FC_LINKER@ @H5FC_STATIC@
xmimsim_db_LDADD = ../src/libxmimsim.la 
xmimsim_db_LDFLAGS = 

xmimsim_harvester_SOURCES = xmimsim-harvester.c
xmimsim_harvester_CFLAGS =  @glib2_CFLAGS@ @PTHREAD_CFLAGS@
xmimsim_harvester_LDADD = ../src/libxmimsim.la


xmimsim_gui_SOURCES = xmimsim-gui.c xmimsim-gui-layer.c xmimsim-gui-layer.h xmimsim-gui-energies.c xmimsim-gui-energies.h xmimsim-gui.h
xmimsim_gui_CFLAGS = @gtk2_CFLAGS@ @xraylib_CFLAGS@
xmimsim_gui_LDADD = ../src/libxmimsim.la @gtk2_LIBS@


#xmimsim_test_SOURCES = xmimsim-test.c
#xmimsim_test_LDADD = ../src/libxmimsim.la

#xmimsim_test_xml_SOURCES = xmimsim-test-xml.c
#xmimsim_test_xml_LDADD = ../src/libxmimsim.la

xmimsim_SOURCES = xmimsim.c
xmimsim_CFLAGS = @H5FC_INCLUDE@  @OPENMPI_CFLAGS@ @gsl_CFLAGS@ @glib2_CFLAGS@ @xraylib_CFLAGS@ \
-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\" -DXMIMSIM_HDF5_SOLID_ANGLES=\"$(datadir)/xmimsim/xmimsim-solid-angles.h5\"
xmimsim_LDADD = ../src/libxmimsim.la @OPENMPI_LIBADD@ @glib2_LIBS@
xmimsim_LDFLAGS = @OPENMPI_LDFLAGS@

#xmimsim_test_alloc_SOURCES = xmimsim-test-alloc.F90
#xmimsim_test_alloc_FCFLAGS = @fgsl_CFLAGS@ @xraylib_f2003_CFLAGS@ @H5FC_INCLUDE@ @OPENMP_FCFLAGS@
##xmimsim_db_LDADD = ../src/libxmimsim.la @H5FC_LINKER@ @H5FC_STATIC@
#xmimsim_test_alloc_LDADD = ../src/libxmimsim.la 

xmimsim_pymca_SOURCES = xmimsim-pymca.c
xmimsim_pymca_LDADD = ../src/libxmimsim.la
xmimsim_pymca_CFLAGS = @glib2_CFLAGS@ @xraylib_CFLAGS@ \
-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\" \
-DXMIMSIM_HDF5_SOLID_ANGLES=\"$(datadir)/xmimsim/xmimsim-solid-angles.h5\"

#xmimsim_test_solid_angle_SOURCES = xmimsim-test-solid-angle.c
#xmimsim_test_solid_angle_LDADD = ../src/libxmimsim.la
#xmimsim_test_solid_angle_CFLAGS = -DXMIMSIM_HDF5_SOLID_ANGLES=\"$(datadir)/xmimsim/xmimsim-solid-angles.h5\" \
#-DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\"

xmimsim_init_solid_angles_SOURCES = xmimsim-init-solid-angles.c
xmimsim_init_solid_angles_LDADD = ../src/libxmimsim.la
xmimsim_init_solid_angles_CFLAGS = -DXMIMSIM_HDF5_SOLID_ANGLES=\"$(datadir)/xmimsim/xmimsim-solid-angles.h5\" @glib2_CFLAGS@

#xmimsim_test_rayl_phi_SOURCES = xmimsim-test-rayl-phi.F90
#xmimsim_test_rayl_phi_LDADD = ../src/libxmimsim.la
#xmimsim_test_rayl_phi_FCFLAGS = @fgsl_CFLAGS@ @xraylib_f2003_CFLAGS@ @H5FC_INCLUDE@ @OPENMP_FCFLAGS@ -DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\"

xmso2xmsi_SOURCES = xmso2xmsi.c
xmso2xmsi_LDADD = ../src/libxmimsim.la
xmso2xmsi_CFLAGS = @glib2_CFLAGS@


xmimsim-solid-angles.h5:	xmimsim-init-solid-angles$(EXEEXT)
	./xmimsim-init-solid-angles$(EXEEXT)

#install-data-hook:
#	chmod a+rw  $(DESTDIR)/$(datadir)/xmimsim/xmimsim-solid-angles.h5

install-exec-hook:
	chmod u+s $(DESTDIR)/$(bindir)/xmimsim
	chmod u+s $(DESTDIR)/$(bindir)/xmimsim-pymca


clean-local:
	rm -f xmimsim-solid-angles.h5

