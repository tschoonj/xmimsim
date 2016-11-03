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
fi


cd $APPVEYOR_BUILD_FOLDER
autoreconf -fi
export CPPFLAGS=-I/usr/local/include
./configure $CONFIGURE_OPTIONS
make
make check
make distcheck
