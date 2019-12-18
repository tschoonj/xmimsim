#!/usr/bin/env bash

set -e
set -x

export SSL_CERT_FILE="C:/msys64/mingw64/ssl/cert.pem"
export SSL_CERT_DIR="C:/msys64/mingw64/ssl/certs"

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

if test -n "${GOOGLE_ANALYTICS}" ; then
	CONFIGURE_OPTIONS+="--enable-google-analytics "
fi

export HDF5_CFLAGS="-I$HOME/install/include"
export HDF5_LIBS="-L$HOME/install/lib -lhdf5"

if test -z ${DEPLOY+x} ; then
	:
else
	CONFIGURE_OPTIONS+="--enable-opencl "
fi

autoreconf -fi
export CPPFLAGS="-I/usr/local/include"
#export CFLAGS="-Wno-deprecated -Wno-deprecated-declarations -Werror=implicit"
#export CXXFLAGS="-Wno-deprecated -Wno-deprecated-declarations"
export LIBS="-L/usr/local/lib/nvidia"
./configure --prefix=$HOME/install $CONFIGURE_OPTIONS --disable-static --enable-introspection --enable-custom-detector-response
make
if test -z ${DEPLOY+x} ; then
	make check
else
	# I think the OpenCL test will block due to that stupid dialog. So don't test until glib 2.56.0 is out
	make dist
	export DO_NOT_USE_DATA=1
	export USE_LOCAL_XRAYLIB=1
	cd windows
	export PATH="/C/Program Files (x86)/Inno Setup 6:$HOME/install/bin:$PATH"
	make iss
	mv XMI-MSIM*exe ..
	cd ..
fi
