


AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include

lib_LTLIBRARIES = libxmimsim.la
libxmimsim_la_SOURCES = xmi_random.c xmi_random_dev.c xmi_random_dev.h xmi_xml.c\
	xmi_data_structs.c xmi_aux.c xmi_aux.h xmi_aux_f.F90
libxmimsim_la_CFLAGS = @PTHREAD_CFLAGS@ @xml2_CFLAGS@
libxmimsim_la_FCFLAGS = 
libxmimsim_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@
libxmimsim_la_LIBADD = @PTHREAD_LIBS@ @xml2_LIBS@ @xraylib_f2003_LIBS@
