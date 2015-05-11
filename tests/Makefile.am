check_LTLIBRARIES = libxmimsim-test.la
check_PROGRAMS = test-read-xmsi$(EXEEXT)

TESTS = $(check_PROGRAMS)

libxmimsim_test_la_SOURCES = \
	libxmimsim-test.h \
	libxmimsim-test.c
libxmimsim_test_la_CFLAGS = @glib2_CFLAGS@ @libcurl_CFLAGS@ \
			    -I${top_srcdir}/include @xml2_CFLAGS@ \
			    @xraylib_CFLAGS@ \
			    -DCATALOGPATH=\"$(abs_top_srcdir)/xml/\"
libxmimsim_test_la_LIBADD = @glib2_LIBS@ @libcurl_LIBS@ ../src/libxmimsim.la

test_read_xmsi_SOURCES = test-read-xmsi.c
test_read_xmsi_CFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_read_xmsi_LDADD = libxmimsim-test.la 


clean-local:
	rm -f test.xmsi

