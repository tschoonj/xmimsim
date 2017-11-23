#!/usr/bin/env bash

set -e
set -x

export PKG_CONFIG_PATH=$HOME/install/lib/pkgconfig
export PATH=$HOME/install/bin:$PATH

CONFIGURE_OPTIONS=""

if test -z ${PLOT+x} ; then
	CONFIGURE_OPTIONS+="--disable-gui "
else
	CONFIGURE_OPTIONS+="--enable-gui "
	if test $UPDATER = "true" ; then
		CONFIGURE_OPTIONS+="--enable-updater "
	else
		CONFIGURE_OPTIONS+="--disable-updater "
	fi
fi

if test -z ${GOOGLE_ANALYTICS+x} ; then
	:
else
	CONFIGURE_OPTIONS+="--enable-google-analytics "
fi

if test -z ${DEPLOY+x} ; then
	:
else
	CONFIGURE_OPTIONS+="--enable-opencl "
fi

cd $APPVEYOR_BUILD_FOLDER

autoreconf -fi
export CPPFLAGS="-I/usr/local/include -I$HOME/install/include"
export CFLAGS="-Wno-deprecated -Wno-deprecated-declarations"
export CXXFLAGS="-Wno-deprecated -Wno-deprecated-declarations"
export LIBS="-L/usr/local/lib/nvidia"
./configure --prefix=$HOME/install $CONFIGURE_OPTIONS
make
if test -z ${DEPLOY+x} ; then
	make check
	make distcheck
else
	# I think the OpenCL test will block due to that stupid dialog. So don't test until glib 2.56.0 is out
	make dist
	export DO_NOT_USE_DATA=1
	cd windows
	make iss
fi
