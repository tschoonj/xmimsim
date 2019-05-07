#Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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

SUBDIRS = src bin include xml etc windows icons osx custom-detector-response examples tests .
ACLOCAL_AMFLAGS = -I m4

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=libxmimsim.pc

if ENABLE_GUI
pkgconfig_DATA+=libxmimsim-gui.pc
endif

#libtool archives are evil
LT_ARCHIVES = $(DESTDIR)$(libdir)/libxmimsim.la
if ENABLE_OPENCL
LT_ARCHIVES += $(DESTDIR)$(libdir)/xmimsim/xmimsim-cl.la
endif

install-exec-hook:
	rm -f $(LT_ARCHIVES)

EXTRA_DIST = xmimsim.spec.in License.rtf Changelog
DISTCHECK_CONFIGURE_FLAGS = --enable-custom-detector-response --disable-mac-integration --enable-introspection --enable-silent-rules
