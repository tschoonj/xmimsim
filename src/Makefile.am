


AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include

lib_LTLIBRARIES = libxmimsim.la
libxmimsim_la_SOURCES = xmi_xml.c\
	xmi_data_structs.c xmi_aux.c xmi_aux.h xmi_aux_f.F90 xmi_hdf5.F90 xmi_solid_angle_f.F90\
	xmi_variance_reduction.F90 xmi_detector.F90 xmi_main.F90 \
	xmi_lines.c xmi_lines.h xmi_pymca.c xmi_solid_angle.c \
	xmi_xslt.c

if !OS_WINDOWS
libxmimsim_la_SOURCES += xmi_random.c xmi_random_dev.c xmi_random_dev.h 
else
libxmimsim_la_SOURCES += xmi_random_win.c 
endif

libxmimsim_la_CFLAGS = @H5FC_INCLUDE@ @PTHREAD_CFLAGS@ @xslt_CFLAGS@ @gsl_CFLAGS@ -DXMI_CATALOG=\"$(datadir)/xmimsim/xmimsim_cat.xml\" @glib2_CFLAGS@ @xraylib_CFLAGS@ -DXMI_XMSO2XMSI_XSLT=\"$(datadir)/xmimsim/xmso2xmsi.xml\"
libxmimsim_la_FCFLAGS =@H5FC_INCLUDE@ @OPENMP_FCFLAGS@ @fgsl_CFLAGS@ @xraylib_f2003_CFLAGS@ -DXMIMSIM_HDF5_DEFAULT=\"$(datadir)/xmimsim/xmimsimdata.h5\"
libxmimsim_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@ @LDFLAGS_XMIMSIM_LIB@
libxmimsim_la_LIBADD = @H5FC_LINKER@ @PTHREAD_LIBS@ @xslt_LIBS@ @xraylib_f2003_LIBS@ @H5FC_STATIC@ @fgsl_LIBS@ @glib2_LIBS@ 


clean-local:
	rm -f xmimsim_main.mod xmimsim_aux.mod xmimsim_solid_angle.mod xmimsim_hdf5.mod xmimsim_varred.mod xmimsim_detector.mod
