FROM ubuntu:xenial

RUN apt-get update && apt-get upgrade -y && apt-get install -y curl
RUN curl -sSL "http://xmi-apt.tomschoonjans.eu/xmi.packages.key" | apt-key add -
RUN echo "deb [arch=amd64] http://xmi-apt.tomschoonjans.eu/ubuntu xenial stable" | tee -a /etc/apt/sources.list > /dev/null
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
        libxrl7-dev \
        python-libxrl7 \
        libsoup2.4-dev \
	gobject-introspection \
	python-gobject \
	libgirepository1.0-dev

RUN apt-get install -y libeasyrng-dev

WORKDIR /root

