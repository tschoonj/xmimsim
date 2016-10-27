#!/usr/bin/env bash

set -e

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
./configure CPPFLAGS=-I/usr/local/include $CONFIGURE_OPTIONS
make
make check
make distcheck
