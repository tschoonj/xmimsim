#!/usr/bin/env bash

set -e
set -x

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig

CONFIGURE_OPTIONS=""

if test -z ${PLOT+x} ; then
	CONFIGURE_OPTIONS+="--disable-gui "
else
	CONFIGURE_OPTIONS+="--enable-gui "
	if test $UPDATER = "true" ; then
		CONFIGURE_OPTIONS+="--enable-updater"
	else
		CONFIGURE_OPTIONS+="--disable-updater"
	fi
	if test $PLOT = "gtkmm-plplot" ; then
		gtkmm3_CFLAGS="-I/usr/local/include/plplot -I/usr/local/include/gtkmm-plplot-2.0 -I/usr/local/lib/gtkmm-plplot-2.0/include "
		gtkmm3_CFLAGS+=`pkg-config --cflags gtkmm-3.0`
		gtkmm3_LIBS="-L/usr/local/lib -lplplotcxx -lplplot "
		gtkmm3_LIBS+=`pkg-config --libs gtkmm-3.0`
		gtkmm3_LIBS+=" -lgtkmm-plplot-2.0"
		export gtkmm3_CFLAGS
		export gtkmm3_LIBS
	fi
fi


cd $APPVEYOR_BUILD_FOLDER
autoreconf -fi
export CPPFLAGS=-I/usr/local/include
./configure $CONFIGURE_OPTIONS
make
make check
make distcheck
