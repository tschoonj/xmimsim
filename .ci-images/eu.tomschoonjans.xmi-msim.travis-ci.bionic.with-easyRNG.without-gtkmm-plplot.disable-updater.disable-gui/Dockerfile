FROM ubuntu:bionic

RUN apt-get update && apt-get upgrade -y && apt-get install -y curl gnupg2
RUN curl -sSL "http://xmi-apt.tomschoonjans.eu/xmi.packages.key" | apt-key add -
RUN echo "deb [arch=amd64] http://xmi-apt.tomschoonjans.eu/ubuntu bionic stable" | tee -a /etc/apt/sources.list > /dev/null
RUN echo "deb-src http://xmi-apt.tomschoonjans.eu/ubuntu bionic stable" | tee -a /etc/apt/sources.list > /dev/null
RUN apt-get update
RUN apt-get install -y build-essential \
	git \
	automake \
	autoconf \
	libtool \
        gfortran \
        libxslt1-dev \
        libxml2-utils \
        libhdf5-serial-dev \
        hdf5-tools \
        libxrl11-dev \
        python3-libxrl11 \
        libsoup2.4-dev \
	gobject-introspection \
	python-gobject \
	libgirepository1.0-dev \
	swig \
	python-dev \
	python-numpy \
	python3


RUN apt-get install -y libeasyrng-dev

# build xraylib from master
#WORKDIR /root
#RUN git clone --single-branch --depth=1 https://github.com/tschoonj/xraylib.git
#WORKDIR /root/xraylib
#RUN autoreconf -i
#RUN ./configure --disable-static --enable-python --enable-fortran2003
#RUN make -j2
#RUN make check
#RUN make install
#RUN make clean

WORKDIR /root

ENV PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
ENV LD_LIBRARY_PATH=/usr/local/lib

