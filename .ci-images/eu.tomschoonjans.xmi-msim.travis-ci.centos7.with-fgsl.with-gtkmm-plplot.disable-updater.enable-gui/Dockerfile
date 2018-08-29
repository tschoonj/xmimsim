FROM tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui

RUN yum install -y gtkmm-plplot-devel

WORKDIR /root

# build gtkmm-plplot from master
RUN git clone --single-branch --depth=1 https://github.com/tschoonj/gtkmm-plplot.git
WORKDIR /root/gtkmm-plplot
RUN autoreconf -i
RUN ./configure --disable-static
RUN make -j2
RUN make install
RUN make clean
RUN yum remove -y gtkmm-plplot
WORKDIR /root
ENV PKG_CONFIG_PATH=/usr/local/lib/pkgconfig

RUN yum clean all
