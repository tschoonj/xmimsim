check_LTLIBRARIES = libxmimsim-test.la
check_PROGRAMS = \
		 test-read-xmsi \
     test-read-xmso \
     test-read-xmsa \
		 test-write-xmsi \
     test-write-xmso \
     test-write-xmsa \
		 test-xmso2csv \
     test-xmso2htm \
		 test-xmso2spe

if ENABLE_GUI
  check_PROGRAMS += test-xmso2svg
endif

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

test_write_xmsi_SOURCES = test-write-xmsi.c
test_write_xmsi_CFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_write_xmsi_LDADD = libxmimsim-test.la

test_write_xmso_SOURCES = test-write-xmso.c
test_write_xmso_CFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_write_xmso_LDADD = libxmimsim-test.la

test_write_xmsa_SOURCES = test-write-xmsa.c
test_write_xmsa_CFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_write_xmsa_LDADD = libxmimsim-test.la

test_xmso2csv_SOURCES = test-xmso2csv.c
test_xmso2csv_CFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_xmso2csv_LDADD = libxmimsim-test.la

test_xmso2svg_SOURCES = test-xmso2svg.c
test_xmso2svg_CFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@ @gtk2_CFLAGS@
test_xmso2svg_LDADD = libxmimsim-test.la @gtk2_LIBS@

test_xmso2htm_SOURCES = test-xmso2htm.c
test_xmso2htm_CFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ @xraylib_CFLAGS@
test_xmso2htm_LDADD = libxmimsim-test.la

test_xmso2spe_SOURCES = test-xmso2spe.c
test_xmso2spe_CFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ @xraylib_CFLAGS@
test_xmso2spe_LDADD = libxmimsim-test.la


clean-local:
	rm -f test.xmsi test.xmso CaSO4_28keV_pol_1D.xmsa CaSO4_28keV_pol_2D.xmsa
