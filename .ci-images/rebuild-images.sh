set -e
set -x

# centos 7 -> always with fgsl
#docker build -t tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui eu.tomschoonjans.xmi-msim.travis-ci.centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui
#docker push tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui

#docker build -t tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi eu.tomschoonjans.xmi-msim.travis-ci.centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi
#docker push tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi

#docker build -t tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.centos7.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui
#docker push tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui

#docker build -t tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.centos7.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui
#docker push tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui

# xenial -> fgsl
docker build -t tomschoonjans/xmimsim-travis-ci:xenial.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui eu.tomschoonjans.xmi-msim.travis-ci.xenial.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui
docker push tomschoonjans/xmimsim-travis-ci:xenial.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui

docker build -t tomschoonjans/xmimsim-travis-ci:xenial.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi eu.tomschoonjans.xmi-msim.travis-ci.xenial.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi
docker push tomschoonjans/xmimsim-travis-ci:xenial.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi

docker build -t tomschoonjans/xmimsim-travis-ci:xenial.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.xenial.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui
docker push tomschoonjans/xmimsim-travis-ci:xenial.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui

docker build -t tomschoonjans/xmimsim-travis-ci:xenial.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.xenial.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui
docker push tomschoonjans/xmimsim-travis-ci:xenial.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui

# xenial -> easyRNG
docker build -t tomschoonjans/xmimsim-travis-ci:xenial.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui eu.tomschoonjans.xmi-msim.travis-ci.xenial.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui
docker push tomschoonjans/xmimsim-travis-ci:xenial.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui

docker build -t tomschoonjans/xmimsim-travis-ci:xenial.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi eu.tomschoonjans.xmi-msim.travis-ci.xenial.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi
docker push tomschoonjans/xmimsim-travis-ci:xenial.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi

docker build -t tomschoonjans/xmimsim-travis-ci:xenial.with-easyRNG.with-gtkmm-plplot.disable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.xenial.with-easyRNG.with-gtkmm-plplot.disable-updater.enable-gui
docker push tomschoonjans/xmimsim-travis-ci:xenial.with-easyRNG.with-gtkmm-plplot.disable-updater.enable-gui

docker build -t tomschoonjans/xmimsim-travis-ci:xenial.with-easyRNG.with-gtkmm-plplot.enable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.xenial.with-easyRNG.with-gtkmm-plplot.enable-updater.enable-gui
docker push tomschoonjans/xmimsim-travis-ci:xenial.with-easyRNG.with-gtkmm-plplot.enable-updater.enable-gui
