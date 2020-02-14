#Copyright (C) 2010-2014 Tom Schoonjans and Laszlo Vincze

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

AC_INIT([xmimsim],[8.1],[Tom.Schoonjans@me.com])
AC_PREREQ([2.67])
AC_CONFIG_SRCDIR([bin/xmimsim.c])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([-Wall -Wno-portability -Werror foreign])


AC_CANONICAL_HOST
AC_GNU_SOURCE

#if platform is Mac OS X, take care of the launchd plist file
AM_CONDITIONAL(OS_DARWIN, [echo $host_os | grep '^darwin' > /dev/null])
if test "x$OS_DARWIN_TRUE" = "x" ; then
AC_SUBST(OS_DARWIN, 1)
else
AC_SUBST(OS_DARWIN, 0)
fi
AM_CONDITIONAL([OS_DARWIN],[test x$OS_DARWIN = x1])

#similar will have to happen for linux in /etc/init.d

#are we dealing with windows?
OS_WINDOWS=0
OS_WINDOWS_32=0
OS_WINDOWS_64=0
LDFLAGS_XMIMSIM_LIB=""
LDFLAGS_XMIMSIM_CL=""
case "$host" in
	*mingw*)
		OS_WINDOWS=1
		LDFLAGS_XMIMSIM_LIB+="-no-undefined -Wl,--output-def,libxmimsim-0.def"
		LDFLAGS_XMIMSIM_GUI_LIB+="-no-undefined -Wl,--output-def,libxmimsim-gui-0.def"
		LDFLAGS_XMIMSIM_CL+="-no-undefined -export-dynamic"
		;;
esac

WINDRES_ARCH=
case "$host" in
	i686-*mingw*)
		WINDRES_ARCH="pe-i386"
		OS_WINDOWS_32=1
		;;
	x86_64-*mingw*)
		WINDRES_ARCH="pe-x86-64"
		OS_WINDOWS_64=1
		;;
esac

AC_SUBST(WINDRES_ARCH)

AC_SUBST(OS_WINDOWS)
AM_CONDITIONAL([OS_WINDOWS],[test x$OS_WINDOWS = x1])
AC_SUBST(OS_WINDOWS_32)
AM_CONDITIONAL([OS_WINDOWS_32],[test x$OS_WINDOWS_32 = x1])
AC_SUBST(OS_WINDOWS_64)
AM_CONDITIONAL([OS_WINDOWS_64],[test x$OS_WINDOWS_64 = x1])




LIB_CURRENT=0
LIB_REVISION=0
LIB_AGE=0
AC_SUBST(LIB_CURRENT)
AC_SUBST(LIB_REVISION)
AC_SUBST(LIB_AGE)

#m4_pattern_allow([AS_TR_SH])
AC_CONFIG_MACRO_DIR([m4])

AC_USE_SYSTEM_EXTENSIONS
m4_ifdef([AM_PROG_AR],[AM_PROG_AR])

#libtool
LT_PREREQ([2.0.0])
LT_INIT()
AC_PROG_LIBTOOL

#
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CXX
AC_PROG_OBJC

#this next line may never be reached...
if test -z $CC ; then
        AC_MSG_ERROR([no C compiler was found on the system.])
fi

#check for GUI
AC_ARG_ENABLE([gui],[AS_HELP_STRING([--disable-gui],[build without gui])],[enable_gui=$enableval],[enable_gui=check])



#initialize pkg-config
PKG_PROG_PKG_CONFIG

#search for xraylib and other modules
PKG_CHECK_MODULES([xraylib],libxrl >= 3.99.0)
PKG_CHECK_MODULES([xraylib_f2003],libxrlf03 >= 3.99.0)
PKG_CHECK_MODULES([xml2],libxml-2.0 >= 2.9.1)
PKG_CHECK_MODULES([xslt],libxslt >= 1.1.26)
EASYRNG_FOUND=no
PKG_CHECK_MODULES([easyRNG],[easyRNG >= 1.1],
	[
	WITH_FORTRAN=`$PKG_CONFIG --variable=with_fortran easyRNG`
	if test x$WITH_FORTRAN = xyes ; then
		AC_DEFINE([HAVE_EASYRNG], [], [building with easyRNG support])
		AC_DEFINE([XMI_MSIM_HAVE_EASYRNG], [], [building with easyRNG support])
		AC_SUBST([fortran_rng], "easyRNG >= 1.1")
		AC_SUBST([RNG], "easyRNG")
		EASYRNG_VERSION=`$PKG_CONFIG --modversion easyRNG`
		AC_DEFINE_UNQUOTED([EASYRNG_VERSION], $EASYRNG_VERSION, [easyRNG version])
		EASYRNG_FOUND=yes
	else
		EASYRNG_FOUND=no
	fi
	],[
	EASYRNG_FOUND=no
	])

FGSL11_FOUND=no
FGSL10_FOUND=no
if test x$EASYRNG_FOUND = xno ; then
	PKG_CHECK_MODULES([fgsl],[fgsl >= 1.1.0],
		[
		AC_DEFINE([HAVE_FGSL11],[],[building with support for FGSL 1.1.0])
		AC_DEFINE([XMI_MSIM_HAVE_FGSL11],[],[building with support for FGSL 1.1.0])
		FGSL_VERSION=`$PKG_CONFIG --modversion fgsl`
		AC_DEFINE_UNQUOTED([FGSL_VERSION], $FGSL_VERSION, [FGSL version])
		AC_SUBST([fortran_rng], "fgsl >= 1.1.0")
		FGSL11_FOUND=yes
		AC_SUBST([RNG], "fgsl")
		], [
		FGSL11_FOUND=no
		])

	if test x$FGSL11_FOUND = xno ; then
		PKG_CHECK_MODULES([fgsl],[fgsl >= 1.0.0],
		[
		AC_DEFINE([HAVE_FGSL10],[],[building with support for FGSL 1.0.0])
		AC_DEFINE([XMI_MSIM_HAVE_FGSL10],[],[building with support for FGSL 1.0.0])
		FGSL_VERSION=`$PKG_CONFIG --modversion fgsl`
		AC_DEFINE_UNQUOTED([FGSL_VERSION], $FGSL_VERSION, [FGSL version])
		AC_SUBST([fortran_rng], "fgsl = 1.0.0")
		FGSL10_FOUND=yes
		AC_SUBST([RNG], "fgsl")
		], [
		FGSL10_FOUND=no
		])
	fi
fi
if test x$EASYRNG_FOUND = xno && test x$FGSL11_FOUND = xno && test x$FGSL10_FOUND = xno ; then
	AC_MSG_ERROR([no easyRNG or FGSL found on the system])
fi

AM_CONDITIONAL([ENABLE_FGSL],[test "x$FGSL10_FOUND" = xyes || test "x$FGSL11_FOUND" = xyes])

if test x$OS_WINDOWS = x1 ; then
PKG_CHECK_MODULES([glib2],glib-2.0 >= 2.40.0 gio-2.0 >= 2.40.0 gio-windows-2.0)
else
PKG_CHECK_MODULES([glib2],glib-2.0 >= 2.40.0 gio-2.0 >= 2.40.0 gio-unix-2.0)
fi
PKG_CHECK_MODULES([gmodule],gmodule-2.0)
PKG_CHECK_MODULES([gobject],gobject-2.0)


#libsoup is optional but is required for the automatic updater as well as to run the testsuite (make check/distcheck dependency!)
#google analytics support also relies on it
LIBSOUP_FOUND=no
PKG_CHECK_MODULES([libsoup], libsoup-2.4 >= 2.42.0,
[
 LIBSOUP_FOUND=yes
 AC_DEFINE([HAVE_LIBSOUP],[],[libsoup present])
 AC_DEFINE([XMI_MSIM_HAVE_LIBSOUP],[],[libsoup present])
],[LIBSOUP_FOUND=no])

GTK_FOUND=no
if test "x$enable_gui" != xno ; then
	PKG_CHECK_MODULES([gtkmm3], [gtkmm-plplot-2.0 >= 2.4 libpeas-1.0 >= 1.22.0],
		[
			GTK_FOUND=yes
		],[
			GTK_FOUND=no
		])
	if test "x$GTK_FOUND" = xno && test "x$enable_gui" = xyes ; then
		AC_MSG_ERROR([no gtkmm-plplot and/or libpeas found on the system.])
	elif test "x$GTK_FOUND" = xno && test "x$enable_gui" = xcheck ; then
		AC_MSG_WARN([no gtkmm-plplot and/or libpeas found on the system. The GUI will not be built.])
	else
		AC_MSG_NOTICE([Building with GUI])
		AC_DEFINE([XMI_MSIM_HAVE_GUI],[],[building with GUI])
		AC_DEFINE([HAVE_GUI],[],[building with GUI])
	fi
fi

AM_CONDITIONAL([ENABLE_GUI],[test "x$GTK_FOUND" = xyes])

JSONGLIB_FOUND=no

AC_ARG_ENABLE([updater],[AS_HELP_STRING([--disable-updater],[build without updater])],[enable_updater=$enableval],[enable_updater=check])
if test "x$GTK_FOUND" = xyes && test "x$enable_updater" != xno ; then
	PKG_CHECK_MODULES([JsonGlib],json-glib-1.0 >= 0.8,
	AC_DEFINE([XMI_MSIM_HAVE_JSONGLIB],[],[Json-Glib present])
	AC_DEFINE([HAVE_JSONGLIB],[],[Json-Glib present])
	[JSONGLIB_FOUND=yes],[JSONGLIB_FOUND=no])

	if test "x$enable_updater" = "xyes" && (test "x$JSONGLIB_FOUND" = "xno" || test "x$LIBSOUP_FOUND" = "xno") ; then
		AC_MSG_ERROR([no Json-Glib and/or no libsoup were found on the system, which are required to compile the updater routines])
	elif test "x$enable_updater" = "xcheck" && (test "x$JSONGLIB_FOUND" = "xno" || test "x$LIBSOUP_FOUND" = "xno") ; then
		AC_MSG_WARN([no Json-Glib and/or no libsoup were found on the system. The updater routines will not be compiled])
	else
		AC_SUBST(UPDATER_PC, "json-glib-1.0 >= 0.8 libsoup-2.4 >= 2.42.0")
		AC_MSG_NOTICE([Building with support for automatic updates])
	fi
fi

AM_CONDITIONAL(ENABLE_UPDATER,[test "x$JSONGLIB_FOUND" = xyes && test "x$LIBSOUP_FOUND" = xyes])

AC_ARG_ENABLE([mac-integration],[AS_HELP_STRING([--enable-mac-integration],[build without mac-integration])],[enable_mac_integration=$enableval],[enable_mac_integration=no])

GTK_MAC_INTEGRATION_FOUND=no
if test "x$GTK_FOUND" = xyes ; then
	#check for quartz backend
	_gdk_tgt=""
	_gdk_tgt_all=`$PKG_CONFIG --variable=targets gdk-3.0`
	case "$_gdk_tgt_all" in
		quartz)
			_gdk_tgt=quartz
		;;
	esac

	if test "x$_gdk_tgt" = xquartz && test "x$enable_mac_integration" = xyes ; then
		AC_DEFINE([MAC_INTEGRATION], [], [Building as macOS App])
		AC_DEFINE([XMI_MSIM_MAC_INTEGRATION], [], [Building as macOS App])
		GTK_MAC_INTEGRATION_FOUND=yes
	fi
fi
AM_CONDITIONAL([ENABLE_MAC_APP],[test "x$GTK_MAC_INTEGRATION_FOUND" = xyes])

AC_ARG_ENABLE([google-analytics],[AS_HELP_STRING([--disable-google-analytics],[build without google analytics support])],[enable_google_analytics=$enableval],[enable_google_analytics=check])

GOOGLE_ANALYTICS_FOUND=no
if test "x$enable_google_analytics" != xno ; then
	PKG_CHECK_MODULES(GOOGLE_ANALYTICS, [libsoup-2.4 >= 2.42.0 gobject-2.0 >= 2.52.0],
		[
		GOOGLE_ANALYTICS_FOUND=yes
		AC_SUBST(GOOGLE_ANALYTICS_PC, "libsoup-2.4 >= 2.42.0 gobject-2.0 >= 2.52.0")
		AC_DEFINE([HAVE_GOOGLE_ANALYTICS], [], [Google Analytics support present])
		AC_DEFINE([XMI_MSIM_HAVE_GOOGLE_ANALYTICS], [], [Google Analytics support present])
		AC_MSG_NOTICE([Building with support for Google Analytics Event tracking])
		],
		[
		if test "x$enable_google_analytics" = xyes ; then
			AC_MSG_ERROR([no libsoup and/or no gobject-2.0 >= 2.52.0 were found on the system, which are required to support Google Analytics Event tracking])
		fi
		AC_MSG_WARN([no libsoup and/or no gobject-2.0 >= 2.52.0 were found on the system, which are required to support Google Analytics Event tracking])
		])
fi
AM_CONDITIONAL([ENABLE_GOOGLE_ANALYTICS],[test "x$GOOGLE_ANALYTICS_FOUND" = xyes])

# quicklook support -> need GTKmm3 and PLplot!
QUICKLOOK_DEPS_FOUND=
if test "x$GTK_FOUND" = xyes ; then
	PKG_CHECK_MODULES(QUICKLOOK, cairo-quartz libxml-2.0 plplot,[
		QUICKLOOK_DEPS_FOUND=yes
		AC_MSG_NOTICE([Building with Quicklook support])
	], [
		QUICKLOOK_DEPS_FOUND=no
	])
fi
AM_CONDITIONAL([ENABLE_QUICKLOOK],[test "x$QUICKLOOK_DEPS_FOUND" = xyes])

#check for headers
AC_CHECK_HEADER(stdio.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(errno.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(fcntl.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(signal.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(stdlib.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(string.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(locale.h,,[AC_MSG_ERROR([Required header not found on the system])])
if test x$OS_WINDOWS = x0 ; then
AC_CHECK_HEADER(sys/resource.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(syslog.h,,[AC_MSG_ERROR([Required header not found on the system])])
else
AC_CHECK_HEADER(windows.h,,[AC_MSG_ERROR([Required header not found on the system])])
fi
AC_CHECK_HEADER(sys/stat.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(sys/types.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(time.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(unistd.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(stddef.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(math.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(search.h,,[AC_MSG_ERROR([Required header not found on the system])])

#check for functions
if test x$OS_WINDOWS = x0 ; then
AC_CHECK_FUNC(umask,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(getrlimit,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(fork,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(setsid,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(sigemptyset,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(sigaction,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(dup,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(openlog,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(syslog,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(setlogmask,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(fcntl,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(strerror,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(ftruncate,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(getpid,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(mkfifo,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(setuid,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(getuid,,[AC_MSG_ERROR([Required libc function not found])])
else
AC_CHECK_FUNCS(rand_s)

fi
AC_CHECK_FUNC(strlen,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(strrchr,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(qsort,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(bsearch,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(access,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_LIB([m], [exp], [], [AC_MSG_ERROR([math library does not contain exp])])
AC_CHECK_FUNC(expm1,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(log1p,,[AC_MSG_ERROR([Required libc function not found])])

WSTRICT_CFLAGS=
#AX_CHECK_COMPILE_FLAG([-Werror=deprecated],[WSTRICT_CFLAGS="${WSTRICT_CFLAGS} -Werror=deprecated"],,)
#AX_CHECK_COMPILE_FLAG([-Werror=deprecated-declarations],[WSTRICT_CFLAGS="${WSTRICT_CFLAGS} -Werror=deprecated-declarations"],,)
AX_CHECK_COMPILE_FLAG([-Werror=missing-declarations],[WSTRICT_CFLAGS="${WSTRICT_CFLAGS} -Werror=missing-declarations"],,)
AX_CHECK_COMPILE_FLAG([-Werror=missing-prototypes],[WSTRICT_CFLAGS="${WSTRICT_CFLAGS} -Werror=missing-prototypes"],,)
AX_CHECK_COMPILE_FLAG([-Werror=implicit],[WSTRICT_CFLAGS="${WSTRICT_CFLAGS} -Werror=implicit"],,)
AX_CHECK_COMPILE_FLAG([-Werror=shadow],[WSTRICT_CFLAGS="${WSTRICT_CFLAGS} -Werror=shadow"],,)
#AX_CHECK_COMPILE_FLAG([-Werror=unused-function],[WSTRICT_CFLAGS="${WSTRICT_CFLAGS} -Werror=unused-function"],,)
AX_CHECK_COMPILE_FLAG([-Werror=parentheses],[WSTRICT_CFLAGS="${WSTRICT_CFLAGS} -Werror=parentheses"],,)

AC_SUBST(WSTRICT_CFLAGS)

#check for types
AC_CHECK_TYPE([ptrdiff_t],,[AC_MSG_ERROR([ptrdiff_t type not found])])


#search for pthreads
ACX_PTHREAD(,
	[AC_MSG_ERROR([POSIX threads are required for building the xmimsim package])]
)

#search for xmlcatalog
AC_CHECK_PROG([XMLCATALOG],[xmlcatalog],[xmlcatalog],[fail])
if test x$XMLCATALOG = xfail ; then
	AC_MSG_ERROR([xmlcatalog was not found. This should be a part of the libxml2 installation which was detected on the system. Consider verifying your PATH variable.])
fi

#check program for icons
if test x$OS_WINDOWS = x1 ; then
  AC_CHECK_PROG([WINDRES],[windres],[windres],[fail])
  if test x$WINDRES = xfail ; then
  	AC_MSG_ERROR([windres was not found. This program is necessary to insert the icon in the windows executables])
  fi
fi
AC_CHECK_PROG([GLIB_COMPILE_RESOURCES],[glib-compile-resources],[glib-compile-resources],[fail])
if test x$GLIB_COMPILE_RESOURCES = xfail ; then
	AC_MSG_ERROR([glib-compile-resources was not found.])
fi
#search for chmod
AC_CHECK_PROG([CHMOD],[chmod],[chmod],[fail])
if test x$CHMOD = xfail ; then
	AC_MSG_ERROR([chmod was not found.])
fi

AC_ARG_VAR(HDF5_CFLAGS, [C compiler flags for HDF5])
AC_ARG_VAR(HDF5_LIBS, [linker flags for HDF5])

if test "x$HDF5_CFLAGS" = "x" && test "x$HDF5_LIBS" = "x" ; then

#search for HDF5...
AC_PATH_PROGS([H5CC],[h5cc-64 h5cc],["fail"])
if test x$H5CC = xfail ; then
	AC_MSG_ERROR([An installation of HDF5 is required, including the headers])
fi

H5CC_CC=
HDF5_CFLAGS=
HDF5_LIBS=

H5CC_PARSED=$($H5CC -show -shlib)


COUNTER="0"

AC_CHECK_PROGS([BASENAME], [gbasename basename],["fail"])

if test x"$BASENAME" = xfail ; then
	AC_MSG_ERROR([basename shell command not found])
fi

AC_CHECK_PROGS([DIRNAME], [gdirname dirname],["fail"])

if test x"$DIRNAME" = xfail ; then
	AC_MSG_ERROR([dirname shell command not found])
fi

for i in $H5CC_PARSED
do
	if test x$COUNTER = "x0" ; then
		H5CC_CC=$($BASENAME $i)
		COUNTER="1"
	elif test x${i:0:2} = "x-L" || test x${i:0:2} = "x-l" ; then
		HDF5_LIBS+="$i "
	elif test x${i:0:2} = "x-I" ; then
		HDF5_CFLAGS+="$i "
#	else
#		AC_MSG_ERROR([Unrecognized output from $H5FC: $i])
	fi
done

# h5cc sucks something awful and will not find the headers if they are not in a location that's in the preprocessor's search path
# use h5cc' location to add one more folder to the path
HDF5_CFLAGS+=" -I$($DIRNAME $($DIRNAME $H5CC))/include"


AC_SUBST(HDF5_LIBS)
AC_SUBST(HDF5_CFLAGS)

fi

#search for a fortran compiler (do not check explicitly for the 2003 features yet)
AC_PROG_FC
if test -z $FC ; then
	AC_MSG_ERROR([Fortran 2003 compiler was not found])
fi
AC_FC_SRCEXT(f90)


#search for openmp
AC_LANG_PUSH(Fortran)
AC_OPENMP
AC_LANG_POP(Fortran)
AC_LANG_PUSH(C)
AC_OPENMP
AC_LANG_POP(C)

XLINKER=""
#when we have Intel Fortran on Mac OS X, add the openmp static link option
#let's hope they won't remove it anytime soon...
if test "x${FC:0:5}" = "xifort" && test "x$OS_DARWIN" = "x1" ; then
	LDFLAGS_XMIMSIM_LIB+=" -Xlinker $OPENMP_FCFLAGS -Xlinker -threads"
	LDFLAGS_XMIMSIM_LIB+=" -Xlinker -openmp-link -Xlinker static"
elif test "x${FC:0:5}" = "xifort" ; then
	LDFLAGS_XMIMSIM_LIB+=" -Xcompiler $OPENMP_FCFLAGS -Xcompiler -threads -lpthread"
#gfortran now...
elif test "x${FC:0:8}" = "xgfortran" && test "x$OS_DARWIN" = "x1" ; then
	LDFLAGS_XMIMSIM_LIB+=" -Wc,$OPENMP_FCFLAGS"
elif test "x${FC:0:8}" = "xgfortran" ; then
	LDFLAGS_XMIMSIM_LIB+=" -Wc,$OPENMP_FCFLAGS"
fi

#LDFLAGS_XMIMSIM_LIB=
#if test "x$FC" = "xifort" ; then
#	LDFLAGS_XMIMSIM_LIB="$XLINKER -threads "
#fi

AC_SUBST(XLINKER)
AC_SUBST(LDFLAGS_XMIMSIM_LIB)
AC_SUBST(LDFLAGS_XMIMSIM_GUI_LIB)
AC_SUBST(LDFLAGS_XMIMSIM_CL)

AC_FC_LINE_LENGTH(unlimited)

#search for MPI
OPENMPI_LDFLAGS=
OPENMPI_LIBADD=
OPENMPI_CFLAGS=
AC_ARG_ENABLE([mpi],[AS_HELP_STRING([--disable-mpi],[build without MPI support])],[enable_mpi=$enableval],[enable_mpi=check])

if test x$enable_mpi = "xyes" ; then
	AC_CHECK_PROGS([MPICC],[mpicc mpiicc openmpicc],["fail"])
fi

if test x$MPICC != xfail && test x$enable_mpi = "xyes" ; then
	if test x$MPICC = xmpicc || test x$MPICC = xopenmpicc ; then
		AC_DEFINE(HAVE_OPENMPI,1,[open-mpi support detected])
		AC_DEFINE(XMI_MSIM_HAVE_OPENMPI,1,[open-mpi support detected])
		#OPENMPI_CFLAGS=$($MPICC -showme:compile)
		#OPENMPI_LINK=$($MPICC -showme:link)
		#for i in $OPENMPI_LINK
		#do
		#	if test x${i:0:2} = "x-L" || test x${i:0:2} = "x-l" ; then
		#		OPENMPI_LIBADD+="$i "
		#	else
		#		OPENMPI_LDFLAGS+="$i "
		#	fi
		#done
	elif test x$MPICC = xmpiicc ; then
		AC_DEFINE(HAVE_OPENMPI,1,[Intel MPI support detected])
		AC_DEFINE(XMI_MSIM_HAVE_OPENMPI,1,[Intel MPI support detected])
	fi
	OPENMPI_SHOW=$($MPICC -show)
	counter=0
	dont_use_next=0
	for i in $OPENMPI_SHOW
	do
		if test $counter = 0 ; then
			#maybe a compiler check wouldn't be a bad thing here...
			counter=counter+1
		elif test $dont_use_next = 1 ; then
			OPENMPI_LDFLAGS+="$i "
			dont_use_next=0
		elif test  x${i:0:2} = "x-I" || test x${i:0:2} = "x-D" ; then
			OPENMPI_CFLAGS+="$i "
		elif test x${i:0:2} = "x-L" || test x${i:0:2} = "x-l" ; then
			OPENMPI_LIBADD+="$i "
		elif test x${i:0:8} = "x-Xlinker"  ; then
			OPENMPI_LDFLAGS+="$i "
			dont_use_next=1
		elif test x${i:0:3} = "x-Wl"  ; then
			OPENMPI_LDFLAGS+="$i "
		fi
	done
elif test x$MPICC = xfail && test x$enable_mpi = "xyes" ; then
	AC_MSG_ERROR([no MPI compiler found on the system. Open-MPI and Intel MPI are supported])
fi

AC_SUBST(OPENMPI_LDFLAGS)
AC_SUBST(OPENMPI_LIBADD)
AC_SUBST(OPENMPI_CFLAGS)

#OpenCL test
no_cl=yes

AC_ARG_ENABLE([opencl],[AS_HELP_STRING([--disable-opencl],[build without OpenCL support])],[enable_opencl=$enableval],[enable_opencl=check])

if test "x$enable_opencl" != xno ; then
	AX_HAVE_OPENCL
	if test "x$no_cl" = xyes ; then
		if test "x$enable_opencl" = xcheck ; then
		AC_MSG_NOTICE([no OpenCL implementation found])
		else
		AC_MSG_ERROR([no OpenCL implementation found])
		fi
	else
		AC_MSG_NOTICE([OpenCL implementation found])
		AC_SUBST(CPPFLAGS_CL)
		AC_SUBST(LIBS_CL)
		AC_DEFINE([XMI_MSIM_HAVE_OPENCL], [], [Building with OpenCL support])
	fi
fi

AM_CONDITIONAL([ENABLE_OPENCL], [test "x$no_cl" != "xyes"])

# Metaltest
no_metal=yes

AC_ARG_ENABLE([metal],[AS_HELP_STRING([--disable-metal],[build without Metal support])],[enable_metal=$enableval],[enable_metal=check])

if test "x$enable_metal" != xno ; then
	AX_CHECK_FRAMEWORK([Metal], [
		AC_DEFINE([HAVE_METAL], [], [Building with Metal support])
		AC_DEFINE([XMI_MSIM_HAVE_METAL], [], [Building with Metal support])
		AC_MSG_NOTICE([Metal implementation found])
		LIBS_METAL="-framework Metal"
		AC_SUBST(LIBS_METAL)
		no_metal=no
	], [
		if test "x$enable_metal" = xcheck ; then
		AC_MSG_NOTICE([no Metal implementation found])
		else
		AC_MSG_ERROR([no Metal implementation found])
		fi

	])
fi

AM_CONDITIONAL([ENABLE_METAL], [test "x$no_metal" != "xyes"])

AC_ARG_ENABLE([custom-detector-response],[AS_HELP_STRING([--enable-custom-detector-response],[compile and install the custom detector response plug-in])],[enable_custom_detector=$enableval],[enable_custom_detector=check])

if test "x$enable_custom_detector" = xyes ; then
	AC_MSG_NOTICE([Building custom detector response plug-in])
	if test "x$OS_WINDOWS" = x1 ; then
		AC_MSG_CHECKING([for full path to C compiler])
		CC_FULL=`which $CC`
		CC_DIR=`AS_DIRNAME([$CC_FULL])`
		AC_SUBST(CC_DIR)
		AC_MSG_RESULT($CC_DIR)
	fi
fi

AM_CONDITIONAL([ENABLE_CUSTOM_DETECTOR], [test "x$enable_custom_detector" = "xyes"])

GOBJECT_INTROSPECTION_CHECK([1.46.0])
PYTHON_FOUND=no
if test "x$found_introspection" = xyes ; then
	AM_PATH_PYTHON([2.7],,[:])
	if test "$PYTHON" != : ; then
		PYTHON_FOUND=yes
	fi
fi
AM_CONDITIONAL([HAVE_PYTHON], [test "$PYTHON_FOUND" = yes])

AC_CONFIG_FILES([ 	Makefile
			src/Makefile
			src/Random123/Makefile
			bin/Makefile
			include/Makefile
			xml/Makefile
			etc/Makefile
			etc/be.ugent.xmi.harvester.plist
			windows/Makefile
			xmimsim.spec
			icons/Makefile
			libxmimsim.pc
			osx/Makefile
			osx/Info.plist
			osx/quicklook/Makefile
			custom-detector-response/Makefile
			examples/Makefile
			tests/Makefile
		])

if test "x$GTK_FOUND" = xyes ; then
	AC_CONFIG_FILES([
		libxmimsim-gui.pc
	])
fi

AC_CONFIG_HEADERS([config.h include/xmi_config.h])

if test x$OS_WINDOWS = "x1" ; then
	abs_top_builddir_win=`pwd -W`
	AC_SUBST(abs_top_builddir_win)
	abs_top_srcdir_win=`AS_DIRNAME([$0])`
	cd $abs_top_srcdir_win
	abs_top_srcdir_win=`pwd -W`
	cd $abs_top_builddir_win
	AC_SUBST(abs_top_srcdir_win)
	cd
	abs_homedir_win=`pwd -W`
	AC_SUBST(abs_homedir_win)
	cd $abs_top_builddir_win
fi



AC_OUTPUT
