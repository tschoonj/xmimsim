


AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include

lib_LTLIBRARIES = libxmimsim.la
libxmimsim_la_SOURCES = xmi_random.c xmi_random_dev.c xmi_random_dev.h
libxmimsim_la_CFLAGS = @PTHREAD_CFLAGS@ 
libxmimsim_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@
libxmimsim_la_LIBADD = @PTHREAD_LIBS@
