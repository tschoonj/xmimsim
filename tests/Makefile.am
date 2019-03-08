AM_CFLAGS = @WSTRICT_CFLAGS@
NULL =

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
     test-xmso2spe \
     test-xmso2xmsi \
     test-xmsa2xmso \
     test-pile-up \
     test-xmimsim-main-CaSO4 \
     test-version \
     test-cubic-spline \
     test-ebel \
     test-gobject \
     test-job \
     test-batch \
     $(NULL)

TEST_EXTENSIONS = .py
PY_LOG_COMPILER = $(PYTHON)
AM_PY_LOG_FLAGS =
AM_TESTS_ENVIRONMENT = XMI_CATALOG_PATH='$(abs_top_srcdir)/xml/' LD_LIBRARY_PATH='${top_builddir}/src/.libs:${LD_LIBRARY_PATH}' DYLD_LIBRARY_PATH='${top_builddir}/src/.libs' PATH='${PATH}:${top_builddir}/src/.libs' PYTHONPATH='${top_builddir}/python:${top_builddir}/python/.libs' GI_TYPELIB_PATH='${top_builddir}/src' ; export LD_LIBRARY_PATH DYLD_LIBRARY_PATH PATH PYTHONPATH GI_TYPELIB_PATH XMI_CATALOG_PATH ;

if OS_WINDOWS
AM_TESTS_ENVIRONMENT += XMIMSIM_EXEC='$(top_builddir)/bin/.libs/xmimsim.exe' ; export XMIMSIM_EXEC ;
AM_TESTS_ENVIRONMENT += XMIMSIM_NON_EXISTENT_EXEC='$(top_builddir)/bin/.libs/xmimsim-bad.exe' ; export XMIMSIM_NON_EXISTENT_EXEC ;
else
AM_TESTS_ENVIRONMENT += XMIMSIM_EXEC='$(top_builddir)/bin/xmimsim' ; export XMIMSIM_EXEC ;
AM_TESTS_ENVIRONMENT += XMIMSIM_NON_EXISTENT_EXEC='$(top_builddir)/bin/xmimsim-bad' ; export XMIMSIM_NON_EXISTENT_EXEC ;
endif


if HAVE_INTROSPECTION
if HAVE_PYTHON
dist_check_SCRIPTS = \
     test-gi-xmimsim.py
endif
endif     

if ENABLE_GUI
  check_PROGRAMS += test-xmso2svg
  check_PROGRAMS += test-gui-utils-get-solid-angle-from-slits
endif

if ENABLE_GOOGLE_ANALYTICS
  check_PROGRAMS += test-google-analytics-tracker
endif

if ENABLE_FGSL
  check_PROGRAMS += test-poly-solve-quadratic
endif

if ENABLE_OPENCL
  check_PROGRAMS += test-xmimsim-cl
endif

TESTS = $(dist_check_SCRIPTS) $(check_PROGRAMS)
#TESTS = test-batch

libxmimsim_test_la_SOURCES = \
	libxmimsim-test.h \
	libxmimsim-test.c
libxmimsim_test_la_CPPFLAGS = @glib2_CFLAGS@ @libsoup_CFLAGS@ \
			    -I${top_srcdir}/include @xml2_CFLAGS@ \
			    @xraylib_CFLAGS@
libxmimsim_test_la_LIBADD = @glib2_LIBS@ @libsoup_LIBS@ ../src/libxmimsim.la

test_read_xmsi_SOURCES = test-read-xmsi.c
test_read_xmsi_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_read_xmsi_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_read_xmso_SOURCES = test-read-xmso.c
test_read_xmso_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			-I$(top_srcdir)/src \
			@xraylib_CFLAGS@
test_read_xmso_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_read_xmsa_SOURCES = test-read-xmsa.c
test_read_xmsa_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_read_xmsa_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_write_xmsi_SOURCES = test-write-xmsi.c
test_write_xmsi_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_write_xmsi_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_write_xmso_SOURCES = test-write-xmso.c
test_write_xmso_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_write_xmso_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_write_xmsa_SOURCES = test-write-xmsa.c
test_write_xmsa_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_write_xmsa_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_xmso2csv_SOURCES = test-xmso2csv.c
test_xmso2csv_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/src @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_xmso2csv_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_xmso2xmsi_SOURCES = test-xmso2xmsi.c
test_xmso2xmsi_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/src @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_xmso2xmsi_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_xmsa2xmso_SOURCES = test-xmsa2xmso.c
test_xmsa2xmso_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/src @xml2_CFLAGS@ \
			@xraylib_CFLAGS@
test_xmsa2xmso_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_xmso2svg_SOURCES = test-xmso2svg.c
test_xmso2svg_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@ @gtkmm3_CFLAGS@
test_xmso2svg_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @gtkmm3_LIBS@ @glib2_LIBS@ 

test_gui_utils_get_solid_angle_from_slits_SOURCES = test-gui-utils-get-solid-angle-from-slits.c
test_gui_utils_get_solid_angle_from_slits_CPPFLAGS = -I${srcdir} -I${top_srcdir}/bin \
			-I${top_srcdir}/include @xml2_CFLAGS@ \
			@xraylib_CFLAGS@ @gtkmm3_CFLAGS@
test_gui_utils_get_solid_angle_from_slits_LDADD = ../bin/libxmimsim-gui.la libxmimsim-test.la

test_xmso2htm_SOURCES = test-xmso2htm.c
test_xmso2htm_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ @xraylib_CFLAGS@
test_xmso2htm_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_xmso2spe_SOURCES = test-xmso2spe.c
test_xmso2spe_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ @xraylib_CFLAGS@
test_xmso2spe_LDADD = @xml2_LIBS@ @xslt_LIBS@ libxmimsim-test.la @glib2_LIBS@ 

test_poly_solve_quadratic_SOURCES = test-poly-solve-quadratic.F90
test_poly_solve_quadratic_FCFLAGS = -I$(top_builddir)/src @fgsl_CFLAGS@
test_poly_solve_quadratic_LDADD = libxmimsim-test.la @glib2_LIBS@

test_cubic_spline_SOURCES = test-cubic-spline.c
test_cubic_spline_CPPFLAGS = -I$(top_srcdir)/include
test_cubic_spline_LDADD = libxmimsim-test.la

test_ebel_SOURCES = test-ebel.c
test_ebel_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ @xraylib_CFLAGS@ \
			-DTRANSMISSION_FILE=\"${srcdir}/transmission-efficiency.txt\"
test_ebel_LDADD = libxmimsim-test.la

test_gobject_SOURCES = test-gobject.c
test_gobject_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ @xraylib_CFLAGS@
test_gobject_LDADD = libxmimsim-test.la

test_xmimsim_cl_SOURCES = test-xmimsim-cl.c
test_xmimsim_cl_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ @xraylib_CFLAGS@ \
			-DXMIMSIM_CL_LIB=\"${top_builddir}/src/.libs\"
test_xmimsim_cl_LDADD = libxmimsim-test.la

test_pile_up_SOURCES = test-pile-up.c
test_pile_up_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
			-I${top_srcdir}/include @xml2_CFLAGS@ @xraylib_CFLAGS@ \
			-DSRM1155_XMSO=\"${top_srcdir}/examples/srm1155.xmso\" \
			-DSRM1155_XMSI=\"${top_srcdir}/examples/srm1155.xmsi\"
test_pile_up_LDADD = libxmimsim-test.la

test_xmimsim_main_CaSO4_SOURCES = test-xmimsim-main-CaSO4.c
test_xmimsim_main_CaSO4_CPPFLAGS = -I${srcdir} -I${top_srcdir}/include @glib2_CFLAGS@ \
					-I${top_srcdir}/include @xml2_CFLAGS@ @xraylib_CFLAGS@
test_xmimsim_main_CaSO4_LDADD = libxmimsim-test.la

test_google_analytics_tracker_SOURCES = test-google-analytics-tracker.c
test_google_analytics_tracker_CPPFLAGS = -I${top_srcdir}/include @gobject_CFLAGS@
test_google_analytics_tracker_LDADD = ../src/libxmimsim-google-analytics.la @gobject_LIBS@

test_job_SOURCES = test-job.c
test_job_CPPFLAGS = @gobject_CFLAGS@ -I$(top_srcdir)/include @xml2_CFLAGS@ @xraylib_CFLAGS@
test_job_LDADD = ../src/libxmimsim.la libxmimsim-test.la @gobject_LIBS@

test_batch_SOURCES = test-batch.c
test_batch_CPPFLAGS = @gobject_CFLAGS@ -I$(top_srcdir)/include @xml2_CFLAGS@ @xraylib_CFLAGS@
test_batch_LDADD = ../src/libxmimsim.la libxmimsim-test.la @gobject_LIBS@

test_version_SOURCES = test-version.c
test_version_CPPFLAGS = -I${top_builddir} -I${top_srcdir}/include @xml2_CFLAGS@ @glib2_CFLAGS@ @xraylib_CFLAGS@
test_version_LDADD = @glib2_LIBS@

clean-local:
	rm -f test.spe *.xmsi *.xmso *.xmsa

EXTRA_DIST = transmission-efficiency.txt
