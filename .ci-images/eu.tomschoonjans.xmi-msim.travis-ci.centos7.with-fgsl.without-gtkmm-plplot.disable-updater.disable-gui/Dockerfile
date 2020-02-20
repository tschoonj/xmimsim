FROM centos:7

RUN yum update -y
RUN yum install -y http://lvserver.ugent.be/yum/xmi-repo-key-7.0-1.el7.noarch.rpm
RUN yum install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
RUN yum install -y  gcc gcc-gfortran gcc-c++ \
	git \
	automake \
	autoconf \
	libtool \
	make \
	file \
	fgsl-devel \
        libxslt-devel \
        hdf5-devel \
        xraylib-devel \
        xraylib-fortran \
        xraylib-python \
        libsoup-devel \
	gobject-introspection \
	gobject-introspection-devel \
	python-gobject \
	python-gobject-base \
	swig \
	python-devel \
	numpy \
	python36

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

RUN yum clean all
