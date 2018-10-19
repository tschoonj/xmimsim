FROM tomschoonjans/xmimsim-travis-ci:xenial.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui

RUN apt-get install -y libgtkmm-plplot2-dev

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

