


AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include

lib_LTLIBRARIES = libxmimsim.la
libxmimsim_la_SOURCES = xmi_random.c xmi_random_dev.c xmi_random_dev.h xmi_xml.c\
	xmi_data_structs.c xmi_aux.c xmi_aux.h xmi_aux_f.F90 xmi_main.F90
libxmimsim_la_CFLAGS = @PTHREAD_CFLAGS@ @xml2_CFLAGS@ @gsl_CFLAGS@
libxmimsim_la_FCFLAGS =@H5FC_INCLUDE@ @OPENMP_FCFLAGS@ @fgsl_CFLAGS@ 
libxmimsim_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@
#apparently LIBADD gets angry when you try to link in static libraries... could even be a major issue on linux :-(
libxmimsim_la_LIBADD = @H5FC_LINKER@ @PTHREAD_LIBS@ @xml2_LIBS@ @xraylib_f2003_LIBS@ @H5FC_STATIC@ @fgsl_LIBS@
#libxmimsim_la_LIBADD = @H5FC_LINKER@ @PTHREAD_LIBS@ @xml2_LIBS@ @xraylib_f2003_LIBS@
