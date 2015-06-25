check_LTLIBRARIES = libxmimsim-test.la
check_PROGRAMS = test-read-xmsi$(EXEEXT) \
                 test-read-xmso$(EXEEXT) \
                 test-read-xmsa$(EXEEXT)

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

test_read_xmso_SOURCES = test-read-xmso.c
test_read_xmso_CFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_read_xmso_LDADD = libxmimsim-test.la 

test_read_xmsa_SOURCES = test-read-xmsa.c
test_read_xmsa_CFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_read_xmsa_LDADD = libxmimsim-test.la 


clean-local:
	rm -f test.xmsi test.xmso CaSO4_28keV_pol_1D.xmsa CaSO4_28keV_pol_2D.xmsa

