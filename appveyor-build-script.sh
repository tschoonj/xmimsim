#!/usr/bin/env bash

set -e

cd $APPVEYOR_BUILD_FOLDER
autoreconf -fi
./configure
make
make check
make distcheck
