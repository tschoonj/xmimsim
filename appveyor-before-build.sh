#!/usr/bin/env bash

set -e
set -x

export PKG_CONFIG_PATH=$HOME/install/lib/pkgconfig
export PATH=$HOME/install/bin:$PATH
#export GTKMM_PLPLOT_BRANCH=master
export XRAYLIB_BRANCH=master

# install xraylib
if [ -n "$XRAYLIB_BRANCH" ] ; then
	git clone -b $XRAYLIB_BRANCH --single-branch --depth=1 https://github.com/tschoonj/xraylib.git
	cd xraylib
	autoreconf -i
else
	curl -L -s -O https://xraylib.tomschoonjans.eu/xraylib-3.3.0.tar.gz
	tar xfz xraylib-3.3.0.tar.gz
	cd xraylib-3.3.0
fi
./configure --prefix=$HOME/install --disable-static --enable-python --enable-python-integration
make -j2
make install
cd ..



if test $RNG = "fgsl" ; then
	#install gsl
 	pacman --ask 20 --noconfirm -Su mingw-w64-$MSYS2_ARCH-gsl
	curl -L -s -O https://doku.lrz.de/download/attachments/43321199/fgsl-1.3.0.tar.gz
	tar xfz fgsl-1.3.0.tar.gz
	cd fgsl-1.3.0
	curl -L -s -O https://github.com/reinh-bader/fgsl/commit/c306e9f936983df5bab68f8ba55006c0f88bc775.diff
	patch -p1 < c306e9f936983df5bab68f8ba55006c0f88bc775.diff
	./configure --prefix=$HOME/install --disable-static
	make
	make install
	cd ..
elif test $RNG = "easyRNG" ; then
	curl -L -s -O https://github.com/tschoonj/easyRNG/releases/download/easyRNG-1.1/easyRNG-1.1.tar.gz
	tar xfz easyRNG-1.1.tar.gz
	cd easyRNG-1.1
	./configure --prefix=$HOME/install --disable-static
	make -j2
	make install
	cd ..
else
	exit 1
fi

if test -z ${PLOT+x} ; then
	# do nothing
	:
elif test $PLOT = "gtkmm-plplot" ; then
	pacman --ask 20 --noconfirm -Su mingw-w64-$MSYS2_ARCH-gtkmm3
	pacman --ask 20 --noconfirm -Su mingw-w64-$MSYS2_ARCH-boost
	pacman --ask 20 --noconfirm -Su mingw-w64-$MSYS2_ARCH-cmake
	pacman --ask 20 --noconfirm -Su mingw-w64-$MSYS2_ARCH-libpeas
	# install plplot from master
	#git clone --depth 1 -q git@github.com:PLplot/PLplot.git
	#cd plplot
	# fix pango.pc
	#echo "Requires: gobject-2.0" >> /mingw64/lib/pkgconfig/pango.pc
	curl -L -s -O https://www.dropbox.com/s/uuh5no2qcyzxwi4/plplot-5.15.0.tar.gz
	tar xfz plplot-5.15.0.tar.gz
	cd plplot-5.15.0
	cmake -G "MSYS Makefiles" -DENABLE_fortran=OFF -DENABLE_tcl=OFF -DENABLE_tk=OFF -DENABLE_DYNDRIVERS=OFF -DPLD_wingcc=OFF -DCMAKE_INSTALL_PREFIX=$HOME/install .
	make -j2
	make install
	cd ..
	# install gtkmm-plplot
        if [ -n "$GTKMM_PLPLOT_BRANCH" ] ; then
          git clone -b $GTKMM_PLPLOT_BRANCH --single-branch --depth=1 https://github.com/tschoonj/gtkmm-plplot.git
          cd gtkmm-plplot
          autoreconf -i
        else
	  curl -L -s -O https://github.com/tschoonj/gtkmm-plplot/releases/download/gtkmm-plplot-2.3/gtkmm-plplot-2.3.tar.gz
	  tar xfz gtkmm-plplot-2.3.tar.gz
	  cd gtkmm-plplot-2.3
        fi
	./configure --prefix=$HOME/install --disable-static
	make -j2
	make install
	cd ..
	if test $UPDATER = "true" ; then
		pacman --ask 20 --noconfirm -Su mingw-w64-$MSYS2_ARCH-json-glib
	fi
fi

if test -z ${GOOGLE_ANALYTICS+x} ; then
	# install MSYS2 HDF5
	pacman --ask 20 --noconfirm -Su mingw-w64-$MSYS2_ARCH-hdf5
else
	# install hdf5
	curl -L -s -O https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8/hdf5-1.8.12/src/hdf5-1.8.12.tar.gz
	tar xfz hdf5-1.8.12.tar.gz
	cd hdf5-1.8.12
	# add support for UTF-8 filenames
	curl -L -s -O https://www.dropbox.com/s/gowzeo6vdhjpxnw/hdf5-1.8.12.diff
	patch -p1 < hdf5-1.8.12.diff
	autoreconf -i
	./configure --disable-fortran --disable-cxx --disable-hl --prefix=$HOME/install --disable-static CPPFLAGS=-D_GNU_SOURCE=1
	# patch hdf5 -> https://tschoonj.github.io/blog/2014/01/29/building-a-64-bit-version-of-hdf5-with-mingw-w64/
	echo "#ifndef H5_HAVE_WIN32_API" >> src/H5pubconf.h
	echo "#ifdef WIN32 /* defined for all windows systems */" >> src/H5pubconf.h
	echo "#define H5_HAVE_WIN32_API 1" >> src/H5pubconf.h
	echo "#endif" >> src/H5pubconf.h
	echo "#endif" >> src/H5pubconf.h
	echo "#ifndef H5_HAVE_MINGW" >> src/H5pubconf.h
	echo "#ifdef __MINGW32__ /*defined for all MinGW compilers */" >> src/H5pubconf.h
	echo "#define H5_HAVE_MINGW 1" >> src/H5pubconf.h
	echo "#define H5_HAVE_WINDOWS 1" >> src/H5pubconf.h
	echo "#endif" >> src/H5pubconf.h
	echo "#endif" >> src/H5pubconf.h
	echo "#define H5_BUILT_AS_DYNAMIC_LIB 1" >> src/H5pubconf.h
	make -j2
	make install
	cd ..

	pushd /usr/local
	curl -L -s -O https://www.dropbox.com/s/l6pw1dupx81ulzv/opencl-win64-devel.tar.gz
	tar xfz opencl-win64-devel.tar.gz
	popd
fi
