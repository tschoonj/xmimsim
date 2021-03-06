set -e
set -x

DOCKER_BUILD_OPTIONS="--pull --no-cache"
#DOCKER_BUILD_OPTIONS="--pull"

# centos 7 -> always with fgsl
#docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui eu.tomschoonjans.xmi-msim.travis-ci.centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui
#docker push tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui

#docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi eu.tomschoonjans.xmi-msim.travis-ci.centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi
#docker push tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi

#docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.centos7.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui
#docker push tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui

#docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.centos7.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui
#docker push tomschoonjans/xmimsim-travis-ci:centos7.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui

# bionic -> fgsl
#docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:bionic.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui eu.tomschoonjans.xmi-msim.travis-ci.bionic.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui
#docker push tomschoonjans/xmimsim-travis-ci:bionic.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui

#docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:bionic.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi eu.tomschoonjans.xmi-msim.travis-ci.bionic.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi
#docker push tomschoonjans/xmimsim-travis-ci:bionic.with-fgsl.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi

#docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:bionic.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.bionic.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui
#docker push tomschoonjans/xmimsim-travis-ci:bionic.with-fgsl.with-gtkmm-plplot.disable-updater.enable-gui

#docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:bionic.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.bionic.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui
#docker push tomschoonjans/xmimsim-travis-ci:bionic.with-fgsl.with-gtkmm-plplot.enable-updater.enable-gui

# bionic -> easyRNG
docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:bionic.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui eu.tomschoonjans.xmi-msim.travis-ci.bionic.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui
docker push tomschoonjans/xmimsim-travis-ci:bionic.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui

docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:bionic.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi eu.tomschoonjans.xmi-msim.travis-ci.bionic.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi
docker push tomschoonjans/xmimsim-travis-ci:bionic.with-easyRNG.without-gtkmm-plplot.disable-updater.disable-gui.with-mpi

docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:bionic.with-easyRNG.with-gtkmm-plplot.disable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.bionic.with-easyRNG.with-gtkmm-plplot.disable-updater.enable-gui
docker push tomschoonjans/xmimsim-travis-ci:bionic.with-easyRNG.with-gtkmm-plplot.disable-updater.enable-gui

docker build $DOCKER_BUILD_OPTIONS -t tomschoonjans/xmimsim-travis-ci:bionic.with-easyRNG.with-gtkmm-plplot.enable-updater.enable-gui eu.tomschoonjans.xmi-msim.travis-ci.bionic.with-easyRNG.with-gtkmm-plplot.enable-updater.enable-gui
docker push tomschoonjans/xmimsim-travis-ci:bionic.with-easyRNG.with-gtkmm-plplot.enable-updater.enable-gui
