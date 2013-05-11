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

#
#
# Logo created by Jan Garrevoet (thx!)
# Radiation warning symbol taken from wikipedia: http://en.wikipedia.org/wiki/File:Radiation_warning_symbol.svg
# NOT COPYRIGHTED!
#


appdatadir = $(datadir)/applications

iconsdatadir = $(datadir)/icons/hicolor/
icons16dir = $(iconsdatadir)/16x16/apps/
icons32dir = $(iconsdatadir)/32x32/apps/
icons64dir = $(iconsdatadir)/64x64/apps/
icons128dir = $(iconsdatadir)/128x128/apps/
icons256dir = $(iconsdatadir)/256x256/apps/
iconsscalabledir = $(iconsdatadir)/scalable/apps/

dist_appdata_DATA = xmimsim.desktop
dist_icons16_DATA = 16x16/Logo_xmi_msim.png \
		    16x16/Logo_xmi_msim_Gnome_XMSI.png \
		    16x16/Logo_xmi_msim_Gnome_XMSO.png \
		    16x16/Radiation_warning_symbol.png
dist_icons32_DATA = 32x32/Logo_xmi_msim.png \
 		    32x32/Logo_xmi_msim_Gnome_XMSI.png \
		    32x32/Logo_xmi_msim_Gnome_XMSO.png \
		    32x32/Radiation_warning_symbol.png
dist_icons64_DATA = 64x64/Logo_xmi_msim.png \
		    64x64/Logo_xmi_msim_Gnome_XMSI.png \
		    64x64/Logo_xmi_msim_Gnome_XMSO.png \
		    64x64/Radiation_warning_symbol.png
dist_icons128_DATA = 128x128/Logo_xmi_msim.png \
		     128x128/Logo_xmi_msim_Gnome_XMSI.png \
		     128x128/Logo_xmi_msim_Gnome_XMSO.png \
		     128x128/Radiation_warning_symbol.png
dist_icons256_DATA = 256x256/Logo_xmi_msim.png \
		     256x256/Logo_xmi_msim_Gnome_XMSI.png \
		     256x256/Logo_xmi_msim_Gnome_XMSO.png \
		     256x256/Radiation_warning_symbol.png
dist_iconsscalable_DATA = Logo_xmi_msim.svg \
			  Logo_xmi_msim_Gnome_XMSI.svg \
			  Logo_xmi_msim_Gnome_XMSO.svg \
			  Radiation_warning_symbol.svg
xmimsimdir = $(datadir)/xmimsim
#used from within the About screen -> not working in the Mac App version starting with ML...
dist_xmimsim_DATA = Logo_xmi_msim.png

all:

convert_radiation_warning_symbol:
	convert ${srcdir}/Radiation_warning_symbol.svg -resize 16x16 ${srcdir}/16x16/Radiation_warning_symbol.png
	convert ${srcdir}/Radiation_warning_symbol.svg -resize 32x32 ${srcdir}/32x32/Radiation_warning_symbol.png
	convert ${srcdir}/Radiation_warning_symbol.svg -resize 64x64 ${srcdir}/64x64/Radiation_warning_symbol.png
	convert ${srcdir}/Radiation_warning_symbol.svg -resize 128x128 ${srcdir}/128x128/Radiation_warning_symbol.png
	convert ${srcdir}/Radiation_warning_symbol.svg -resize 256x256 ${srcdir}/256x256/Radiation_warning_symbol.png



convert_linux:
	convert ${srcdir}/Logo_xmi_msim.png -resize 16x16 ${srcdir}/16x16/Logo_xmi_msim.png
	convert ${srcdir}/Logo_xmi_msim.png -resize 32x32 ${srcdir}/32x32/Logo_xmi_msim.png
	convert ${srcdir}/Logo_xmi_msim.png -resize 64x64 ${srcdir}/64x64/Logo_xmi_msim.png
	convert ${srcdir}/Logo_xmi_msim.png -resize 128x128 ${srcdir}/128x128/Logo_xmi_msim.png
	convert ${srcdir}/Logo_xmi_msim.png -resize 256x256 ${srcdir}/256x256/Logo_xmi_msim.png
	convert ${srcdir}/Logo_xmi_msim_Gnome_XMSI.png -resize 16x16 ${srcdir}/16x16/Logo_xmi_msim_Gnome_XMSI.png
	convert ${srcdir}/Logo_xmi_msim_Gnome_XMSI.png -resize 32x32 ${srcdir}/32x32/Logo_xmi_msim_Gnome_XMSI.png
	convert ${srcdir}/Logo_xmi_msim_Gnome_XMSI.png -resize 64x64 ${srcdir}/64x64/Logo_xmi_msim_Gnome_XMSI.png
	convert ${srcdir}/Logo_xmi_msim_Gnome_XMSI.png -resize 128x128 ${srcdir}/128x128/Logo_xmi_msim_Gnome_XMSI.png
	convert ${srcdir}/Logo_xmi_msim_Gnome_XMSI.png -resize 256x256 ${srcdir}/256x256/Logo_xmi_msim_Gnome_XMSI.png
	convert ${srcdir}/Logo_xmi_msim_Gnome_XMSO.png -resize 16x16 ${srcdir}/16x16/Logo_xmi_msim_Gnome_XMSO.png
	convert ${srcdir}/Logo_xmi_msim_Gnome_XMSO.png -resize 32x32 ${srcdir}/32x32/Logo_xmi_msim_Gnome_XMSO.png
	convert ${srcdir}/Logo_xmi_msim_Gnome_XMSO.png -resize 64x64 ${srcdir}/64x64/Logo_xmi_msim_Gnome_XMSO.png
	convert ${srcdir}/Logo_xmi_msim_Gnome_XMSO.png -resize 128x128 ${srcdir}/128x128/Logo_xmi_msim_Gnome_XMSO.png
	convert ${srcdir}/Logo_xmi_msim_Gnome_XMSO.png -resize 256x256 ${srcdir}/256x256/Logo_xmi_msim_Gnome_XMSO.png


convert_windows:
	convert ${srcdir}/Logo_xmi_msim.png -resize 16x16 ${srcdir}/Logo_xmi_msim16.png
	convert ${srcdir}/Logo_xmi_msim.png -resize 24x24 ${srcdir}/Logo_xmi_msim24.png
	convert ${srcdir}/Logo_xmi_msim.png -resize 32x32 ${srcdir}/Logo_xmi_msim32.png
	convert ${srcdir}/Logo_xmi_msim.png -resize 48x48 ${srcdir}/Logo_xmi_msim48.png
	convert ${srcdir}/Logo_xmi_msim.png -resize 256x256 ${srcdir}/Logo_xmi_msim256.png
	convert ${srcdir}/Logo_xmi_msim16.png \
		${srcdir}/Logo_xmi_msim24.png \
		${srcdir}/Logo_xmi_msim32.png \
		${srcdir}/Logo_xmi_msim48.png \
		${srcdir}/Logo_xmi_msim256.png \
		${srcdir}/Logo_xmi_msim_Win7.ico
	rm -f ${srcdir}/Logo_xmi_msim16.png \
		${srcdir}/Logo_xmi_msim24.png \
		${srcdir}/Logo_xmi_msim32.png \
		${srcdir}/Logo_xmi_msim48.png \
		${srcdir}/Logo_xmi_msim256.png
	convert ${srcdir}/Logo_xmi_msim_Win7_XMSI.png -resize 16x16 ${srcdir}/Logo_xmi_msim16.png
	convert ${srcdir}/Logo_xmi_msim_Win7_XMSI.png -resize 24x24 ${srcdir}/Logo_xmi_msim24.png
	convert ${srcdir}/Logo_xmi_msim_Win7_XMSI.png -resize 32x32 ${srcdir}/Logo_xmi_msim32.png
	convert ${srcdir}/Logo_xmi_msim_Win7_XMSI.png -resize 48x48 ${srcdir}/Logo_xmi_msim48.png
	convert ${srcdir}/Logo_xmi_msim_Win7_XMSI.png -resize 256x256 ${srcdir}/Logo_xmi_msim256.png
	convert ${srcdir}/Logo_xmi_msim16.png \
		${srcdir}/Logo_xmi_msim24.png \
		${srcdir}/Logo_xmi_msim32.png \
		${srcdir}/Logo_xmi_msim48.png \
		${srcdir}/Logo_xmi_msim256.png \
		${srcdir}/Logo_xmi_msim_Win7_XMSI.ico
	rm -f ${srcdir}/Logo_xmi_msim16.png \
		${srcdir}/Logo_xmi_msim24.png \
		${srcdir}/Logo_xmi_msim32.png \
		${srcdir}/Logo_xmi_msim48.png \
		${srcdir}/Logo_xmi_msim256.png
	convert ${srcdir}/Logo_xmi_msim_Win7_XMSO.png -resize 16x16 ${srcdir}/Logo_xmi_msim16.png
	convert ${srcdir}/Logo_xmi_msim_Win7_XMSO.png -resize 24x24 ${srcdir}/Logo_xmi_msim24.png
	convert ${srcdir}/Logo_xmi_msim_Win7_XMSO.png -resize 32x32 ${srcdir}/Logo_xmi_msim32.png
	convert ${srcdir}/Logo_xmi_msim_Win7_XMSO.png -resize 48x48 ${srcdir}/Logo_xmi_msim48.png
	convert ${srcdir}/Logo_xmi_msim_Win7_XMSO.png -resize 256x256 ${srcdir}/Logo_xmi_msim256.png
	convert ${srcdir}/Logo_xmi_msim16.png \
		${srcdir}/Logo_xmi_msim24.png \
		${srcdir}/Logo_xmi_msim32.png \
		${srcdir}/Logo_xmi_msim48.png \
		${srcdir}/Logo_xmi_msim256.png \
		${srcdir}/Logo_xmi_msim_Win7_XMSO.ico
	rm -f ${srcdir}/Logo_xmi_msim16.png \
		${srcdir}/Logo_xmi_msim24.png \
		${srcdir}/Logo_xmi_msim32.png \
		${srcdir}/Logo_xmi_msim48.png \
		${srcdir}/Logo_xmi_msim256.png
	






EXTRA_DIST = Logo_xmi_msim.bmp \
	Logo_xmi_msim.eps \
	Logo_xmi_msim.pdf \
	applications-x-xmimsim.xml
