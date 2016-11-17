#!/usr/bin/env bash

set -e
set -x

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig

# install xraylib
wget -q https://xraylib.tomschoonjans.eu/xraylib-3.2.0.tar.gz
tar xfz xraylib-3.2.0.tar.gz
cd xraylib-3.2.0
./configure
make
make install
cd ..

# install hdf5
wget -q https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8.12/src/hdf5-1.8.12.tar.gz
tar xfz hdf5-1.8.12.tar.gz 
cd hdf5-1.8.12
./configure --disable-hl --prefix=/usr/local CPPFLAGS=-D_GNU_SOURCE=1
# patch hdf5 -> https://tschoonj.github.io/blog/2014/01/29/building-a-64-bit-version-of-hdf5-with-mingw-w64/
echo "#ifndef H5_HAVE_WIN32_API" >> src/H5pubconf.h
echo "#ifdef WIN32 /* defined for all windows systems */" >> src/H5pubconf.h
echo "#define H5_HAVE_WIN32_API 1" >> src/H5pubconf.h
echo "#endif" >> src/H5pubconf.h
echo "#endif" >> src/H5pubconf.h
echo "#ifndef H5_HAVE_MINGW" >> src/H5pubconf.h
echo "#ifdef __MINGW32__ /*defined for all MinGW compilers */" >> src/H5pubconf.h
echo "#define H5_HAVE_MINGW 1" >> src/H5pubconf.h
echo "#endif" >> src/H5pubconf.h
echo "#endif" >> src/H5pubconf.h
echo "#define H5_BUILT_AS_DYNAMIC_LIB 1" >> src/H5pubconf.h
make
make install
cd ..



if test $RNG = "fgsl" ; then
	#install gsl
	pacman --noconfirm -Su mingw-w64-$MSYS2_ARCH-gsl
	wget -q http://www.lrz.de/services/software/mathematik/gsl/fortran/download/fgsl-1.1.0.tar.gz
	tar xfz fgsl-1.1.0.tar.gz
	cd fgsl-1.1.0
	./configure
	make
	make install
	cd ..
elif test $RNG = "easyRNG" ; then
	wget -q https://easyrng.tomschoonjans.eu/easyRNG-1.0.tar.gz
	tar xfz easyRNG-1.0.tar.gz
	cd easyRNG-1.0
	./configure
	make
	make install
	cd ..
else
	exit 1
fi

if test -z ${PLOT+x} ; then
	# do nothing
	:
elif test $PLOT = "gtkextra" ; then
	pacman --noconfirm -Su mingw-w64-$MSYS2_ARCH-gtk2
	wget -T 10 -q https://downloads.sourceforge.net/project/gtkextra/3.3/gtkextra-3.3.3.tar.gz
	tar xfz gtkextra-3.3.3.tar.gz 
	cd gtkextra-3.3.3
	# old libtool was used to generate this tarball :-(
	autoreconf -i
	./configure --disable-tests
	make
	make install
	cd ..
	if test $UPDATER = "true" ; then
		pacman --noconfirm -Su mingw-w64-$MSYS2_ARCH-json-glib
	fi
elif test $PLOT = "gtkmm-plplot" ; then
	pacman --noconfirm -Su mingw-w64-$MSYS2_ARCH-gtkmm3
	pacman --noconfirm -Su mingw-w64-$MSYS2_ARCH-boost
	pacman --noconfirm -Su mingw-w64-$MSYS2_ARCH-cmake
	# install plplot from master
	git clone -q git://git.code.sf.net/p/plplot/plplot
	cd plplot
	git checkout 3ab6396f0da8e1b7109ea3323eeb0ef81d215a8a
	cmake -G "MSYS Makefiles" -DENABLE_DYNDRIVERS=OFF -DPLD_wingcc=OFF -DCMAKE_INSTALL_PREFIX=/usr/local .
	make
	make install
	cd ..
	# install gtkmm-plplot
	wget -q https://gtkmm-plplot.tomschoonjans.eu/gtkmm-plplot-2.1.tar.gz
	tar xfz gtkmm-plplot-2.1.tar.gz
	cd gtkmm-plplot-2.1	
	export plplotcxx_CFLAGS="-I/usr/local/include/plplot"
	export plplotcxx_LIBS="-L/usr/local/lib -lplplotcxx -lplplot"
	./configure --prefix=/usr/local
	make
	make install
	cd ..
	if test $UPDATER = "true" ; then
		pacman --noconfirm -Su mingw-w64-$MSYS2_ARCH-json-glib
	fi
fi

