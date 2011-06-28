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

xmimsimdir = $(datadir)/xmimsim
dist_xmimsim_DATA = xmimsim-1.0.dtd xmso2xmsi.xml xmso2svg.xml xmso2spe.xml
xmimsim_DATA = xmimsim_cat.xml



xmimsim_cat.xml:
	$(XMLCATALOG) --noout --create xmimsim_cat.xml 
	$(XMLCATALOG) --noout --add rewriteURI \
	"http://www.xmi.UGent.be/xml/" \
	"file:///$(xmimsimdir)/" xmimsim_cat.xml

clean-local:
	rm -rf xmimsim_cat.xml
