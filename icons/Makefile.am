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
#iconsscalabledir = $(iconsdatadir)/scalable/apps/

if ENABLE_GUI
dist_appdata_DATA = xmimsim.desktop
dist_icons16_DATA = 16x16/Logo_xmi_msim.png \
		    16x16/Logo_xmi_msim_red.png \
		    16x16/Logo_xmi_msim_archive.png \
		    16x16/Logo_xmi_msim_Gnome_XMSI.png \
		    16x16/Logo_xmi_msim_Gnome_XMSO.png \
		    16x16/Logo_xmi_msim_Gnome_XMSA.png \
		    16x16/Radiation_warning_symbol.png
dist_icons32_DATA = 32x32/Logo_xmi_msim.png \
		    32x32/Logo_xmi_msim_red.png \
		    32x32/Logo_xmi_msim_archive.png \
 		    32x32/Logo_xmi_msim_Gnome_XMSI.png \
		    32x32/Logo_xmi_msim_Gnome_XMSO.png \
		    32x32/Logo_xmi_msim_Gnome_XMSA.png \
		    32x32/Radiation_warning_symbol.png
dist_icons64_DATA = 64x64/Logo_xmi_msim.png \
		    64x64/Logo_xmi_msim_red.png \
		    64x64/Logo_xmi_msim_archive.png \
		    64x64/Logo_xmi_msim_Gnome_XMSI.png \
		    64x64/Logo_xmi_msim_Gnome_XMSO.png \
		    64x64/Logo_xmi_msim_Gnome_XMSA.png \
		    64x64/Radiation_warning_symbol.png
dist_icons128_DATA = 128x128/Logo_xmi_msim.png \
		     128x128/Logo_xmi_msim_red.png \
		     128x128/Logo_xmi_msim_archive.png \
		     128x128/Logo_xmi_msim_Gnome_XMSI.png \
		     128x128/Logo_xmi_msim_Gnome_XMSO.png \
		     128x128/Logo_xmi_msim_Gnome_XMSA.png \
		     128x128/Radiation_warning_symbol.png
dist_icons256_DATA = 256x256/Logo_xmi_msim.png \
		     256x256/Logo_xmi_msim_red.png \
		     256x256/Logo_xmi_msim_archive.png \
		     256x256/Logo_xmi_msim_Gnome_XMSI.png \
		     256x256/Logo_xmi_msim_Gnome_XMSO.png \
		     256x256/Logo_xmi_msim_Gnome_XMSA.png \
		     256x256/Radiation_warning_symbol.png
#if !ENABLE_MAC_APP
#dist_iconsscalable_DATA = Logo_xmi_msim.svg \
#			  Logo_xmi_msim_Gnome_XMSI.svg \
#			  Logo_xmi_msim_Gnome_XMSO.svg \
#			  Logo_xmi_msim_Gnome_XMSA.svg \
#			  Radiation_warning_symbol.svg
#endif
xmimsimdir = $(datadir)/xmimsim
#used from within the About screen -> not working in the Mac App version starting with ML...
dist_xmimsim_DATA = Logo_xmi_msim.png Logo_xmi_msim_red.png Logo_xmi_msim_archive.png
endif
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
	convert ${srcdir}/Logo_xmi_msim_red.png -resize 16x16 ${srcdir}/16x16/Logo_xmi_msim_red.png
	convert ${srcdir}/Logo_xmi_msim_red.png -resize 32x32 ${srcdir}/32x32/Logo_xmi_msim_red.png
	convert ${srcdir}/Logo_xmi_msim_red.png -resize 64x64 ${srcdir}/64x64/Logo_xmi_msim_red.png
	convert ${srcdir}/Logo_xmi_msim_red.png -resize 128x128 ${srcdir}/128x128/Logo_xmi_msim_red.png
	convert ${srcdir}/Logo_xmi_msim_red.png -resize 256x256 ${srcdir}/256x256/Logo_xmi_msim_red.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 16x16 ${srcdir}/16x16/Logo_xmi_msim_archive.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 32x32 ${srcdir}/32x32/Logo_xmi_msim_archive.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 64x64 ${srcdir}/64x64/Logo_xmi_msim_archive.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 128x128 ${srcdir}/128x128/Logo_xmi_msim_archive.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 256x256 ${srcdir}/256x256/Logo_xmi_msim_archive.png
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
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 16x16 ${srcdir}/16x16/Logo_xmi_msim_Gnome_XMSA.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 32x32 ${srcdir}/32x32/Logo_xmi_msim_Gnome_XMSA.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 64x64 ${srcdir}/64x64/Logo_xmi_msim_Gnome_XMSA.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 128x128 ${srcdir}/128x128/Logo_xmi_msim_Gnome_XMSA.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 256x256 ${srcdir}/256x256/Logo_xmi_msim_Gnome_XMSA.png

convert_mac:
	mkdir -p ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 16x16 ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset/icon_16x16.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 32x32 ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset/icon_16x16@2x.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 32x32 ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset/icon_32x32.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 64x64 ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset/icon_32x32@2x.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 128x128 ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset/icon_128x128.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 256x256 ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset/icon_128x128@2x.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 256x256 ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset/icon_256x256.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 512x512 ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset/icon_256x256@2x.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 512x512 ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset/icon_512x512.png
	cp ${srcdir}/Logo_xmi_msim_archive.png ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset/icon_512x512@2x.png
	iconutil -c icns ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset 
	rm -rf ${srcdir}/Logo_xmi_msim_MacOSX_XMSA.iconset
	mkdir -p ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSI_retina.png -resize 16x16 ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset/icon_16x16.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSI_retina.png -resize 32x32 ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset/icon_16x16@2x.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSI_retina.png -resize 32x32 ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset/icon_32x32.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSI_retina.png -resize 64x64 ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset/icon_32x32@2x.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSI_retina.png -resize 128x128 ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset/icon_128x128.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSI_retina.png -resize 256x256 ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset/icon_128x128@2x.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSI_retina.png -resize 256x256 ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset/icon_256x256.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSI_retina.png -resize 512x512 ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset/icon_256x256@2x.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSI_retina.png -resize 512x512 ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset/icon_512x512.png
	cp ${srcdir}/Logo_xmi_msim_MacOSX_XMSI_retina.png ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset/icon_512x512@2x.png
	iconutil -c icns ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset 
	rm -rf ${srcdir}/Logo_xmi_msim_MacOSX_XMSI.iconset
	mkdir -p ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSO_retina.png -resize 16x16 ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset/icon_16x16.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSO_retina.png -resize 32x32 ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset/icon_16x16@2x.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSO_retina.png -resize 32x32 ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset/icon_32x32.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSO_retina.png -resize 64x64 ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset/icon_32x32@2x.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSO_retina.png -resize 128x128 ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset/icon_128x128.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSO_retina.png -resize 256x256 ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset/icon_128x128@2x.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSO_retina.png -resize 256x256 ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset/icon_256x256.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSO_retina.png -resize 512x512 ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset/icon_256x256@2x.png
	convert ${srcdir}/Logo_xmi_msim_MacOSX_XMSO_retina.png -resize 512x512 ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset/icon_512x512.png
	cp ${srcdir}/Logo_xmi_msim_MacOSX_XMSO_retina.png ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset/icon_512x512@2x.png
	iconutil -c icns ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset 
	rm -rf ${srcdir}/Logo_xmi_msim_MacOSX_XMSO.iconset

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
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 16x16 ${srcdir}/Logo_xmi_msim16.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 24x24 ${srcdir}/Logo_xmi_msim24.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 32x32 ${srcdir}/Logo_xmi_msim32.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 48x48 ${srcdir}/Logo_xmi_msim48.png
	convert ${srcdir}/Logo_xmi_msim_archive.png -resize 256x256 ${srcdir}/Logo_xmi_msim256.png
	convert ${srcdir}/Logo_xmi_msim16.png \
		${srcdir}/Logo_xmi_msim24.png \
		${srcdir}/Logo_xmi_msim32.png \
		${srcdir}/Logo_xmi_msim48.png \
		${srcdir}/Logo_xmi_msim256.png \
		${srcdir}/Logo_xmi_msim_Win7_XMSA.ico
	rm -f ${srcdir}/Logo_xmi_msim16.png \
		${srcdir}/Logo_xmi_msim24.png \
		${srcdir}/Logo_xmi_msim32.png \
		${srcdir}/Logo_xmi_msim48.png \
		${srcdir}/Logo_xmi_msim256.png
	






EXTRA_DIST = Logo_xmi_msim.bmp \
	Logo_xmi_msim.eps \
	Logo_xmi_msim.pdf \
	applications-x-xmimsim.xml
