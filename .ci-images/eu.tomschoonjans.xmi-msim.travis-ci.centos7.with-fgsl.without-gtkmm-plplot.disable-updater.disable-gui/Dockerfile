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
	python-gobject-base

WORKDIR /root

RUN yum clean all
