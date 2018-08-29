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
        libgsl0-dev \
       	libfgsl1 \
        libfgsl1-dev \
        libgtkmm-plplot2-dev \
        gfortran \
        libxslt1-dev \
        libxml2-utils \
        libhdf5-serial-dev \
        hdf5-tools \
        libxrl7-dev \
        libsoup2.4-dev

WORKDIR /root

# build gtkmm-plplot from master
RUN git clone --single-branch --depth=1 https://github.com/tschoonj/gtkmm-plplot.git
WORKDIR /root/gtkmm-plplot
RUN autoreconf -i
RUN ./configure --disable-static
RUN make -j2
RUN make install
RUN make clean
RUN apt-get remove -y libgtkmm-plplot2
WORKDIR /root
ENV PKG_CONFIG_PATH=/usr/local/lib/pkgconfig

