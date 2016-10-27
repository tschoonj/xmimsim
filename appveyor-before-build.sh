#!/usr/bin/env bash

set -e

if test $RNG = "fgsl" ; then
	#install gsl
	pacman --noconfirm -Su mingw-w64-%MSYS2_ARCH%-{gsl}
	wget http://www.lrz.de/services/software/mathematik/gsl/fortran/download/fgsl-1.1.0.tar.gz
	tar xfz fgsl-1.1.0.tar.gz
	cd fgsl-1.1.0
	./configure
	make
	make install
	cd ..
elif test $RNG = "easyRNG" ; then
	wget https://easyrng.tomschoonjans.eu/easyRNG-1.0.tar.gz
	tar xfz easyRNG-1.0.tar.gz
	cd easyRNG-1.0
	./configure
	make
	make install
	cd ..
else
	exit 1
fi

if test $PLOT = "gtkextra" ; then
	pacman --noconfirm -Su mingw-w64-%MSYS2_ARCH%-{gtk2}
	wget https://downloads.sourceforge.net/project/gtkextra/3.3/gtkextra-3.3.2.tar.gz
	tar xfz gtkextra-3.3.2.tar.gz 
	cd gtkextra-3.3.2
	./configure
	make
	make install
	cd ..
elif test $PLOT = "gtkmm-plplot" ; then
	pacman --noconfirm -Su mingw-w64-%MSYS2_ARCH%-{gtkmm3,boost,cmake}
	# install plplot from master
   	git clone --depth 1 git://git.code.sf.net/p/plplot/plplot
	cd plplot
	cmake -G \"MSYS Makefiles\" -DENABLE_DYNDRIVERS=OFF -DPLD_wingcc=OFF -DCMAKE_INSTALL_PREFIX=/usr/local .
	make
	make install
	cd ..
	# install gtkmm-plplot
	wget https://gtkmm-plplot.tomschoonjans.eu/gtkmm-plplot-2.1.tar.gz
	tar xfz gtkmm-plplot-2.1.tar.gz
	cd gtkmm-plplot-2.1	
	./configure
	make
	make install
	cd ..
else
	exit 1
fi

if test $UPDATER = "true" ; then
	pacman --noconfirm -Su mingw-w64-%MSYS2_ARCH%-json-glib
fi
