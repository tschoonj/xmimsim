#Copyright (C) 2017 Tom Schoonjans and Laszlo Vincze

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.


if ENABLE_QUICKLOOK

all-local: ql

noinst_LTLIBRARIES = xmimsim-ql.la
xmimsim_ql_la_SOURCES = GeneratePreviewForURL.c  GenerateThumbnailForURL.c main.c
xmimsim_ql_la_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/include $(QUICKLOOK_CFLAGS)
xmimsim_ql_la_LIBADD = $(top_builddir)/src/libxmimsim-ql.la  $(QUICKLOOK_LIBS)
xmimsim_ql_la_LDFLAGS = -Wl,-headerpad_max_install_names -framework CoreFoundation -module -avoid-version -shared -rpath /tmp -shrext ""

ql: xmimsim-ql.la
	rm -rf xmi-msim.qlgenerator
	$(MKDIR_P) xmi-msim.qlgenerator/Contents
	$(MKDIR_P) xmi-msim.qlgenerator/Contents/MacOS
	$(MKDIR_P) xmi-msim.qlgenerator/Contents/Resources
	cp $(srcdir)/Info.plist xmi-msim.qlgenerator/Contents/
	cp .libs/xmimsim-ql xmi-msim.qlgenerator/Contents/MacOS/
	cp $(top_srcdir)/xml/xmimsim-1.0.dtd xmi-msim.qlgenerator/Contents/Resources/ 
	touch ql
	python $(srcdir)/finalize-quicklook.py xmi-msim.qlgenerator/Contents/MacOS/xmimsim-ql

ql-test: ql
	qlmanage -g xmi-msim.qlgenerator -c be.ugent.xmi.xmi-msim.xmso -p $(top_srcdir)/examples/srm1155.xmso
endif

EXTRA_DIST = Info.plist finalize-quicklook.py

clean-local:
	rm -f ql
	rm -rf xmi-msim.qlgenerator
