#Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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

AC_INIT([xmimsim],[0.2],[Tom.Schoonjans@UGent.be])
AC_PREREQ([2.60])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AS_INIT

AC_CANONICAL_HOST


#if platform is Mac OS X, take care of the launchd plist file
AM_CONDITIONAL(OS_DARWIN, [echo $host_os | grep '^darwin' > /dev/null])
if test "x$OS_DARWIN_TRUE" = "x" ; then
AC_SUBST(OS_DARWIN, 1)
else
AC_SUBST(OS_DARWIN, 0)
fi

#similar will have to happen for linux in /etc/init.d

#are we dealing with windows?
OS_WINDOWS=0
case "$host" in
	*mingw*)
		OS_WINDOWS=1
		LDFLAGS+="-no-undefined"
		;;
esac

AC_SUBST(OS_WINDOWS)
AM_CONDITIONAL([OS_WINDOWS],[test x$OS_WINDOWS = x1])




LIB_CURRENT=0
LIB_REVISION=0
LIB_AGE=0
AC_SUBST(LIB_CURRENT)
AC_SUBST(LIB_REVISION)
AC_SUBST(LIB_AGE)

#m4_pattern_allow([AS_TR_SH])
AC_CONFIG_MACRO_DIR([m4])

AC_USE_SYSTEM_EXTENSIONS

#libtool
LT_PREREQ([2.0.0])
LT_INIT()
AC_PROG_LIBTOOL

#
AC_PROG_CC
#this next line may never be reached...
if test -z $CC ; then
        AC_MSG_ERROR([no C compiler was found on the system.])
fi

#check for GUI
AC_ARG_ENABLE([gui],[AS_HELP_STRING([--disable-gui],[build without gui])],[enable_gui=$enableval],[enable_gui=check])



#initialize pkg-config
PKG_PROG_PKG_CONFIG

#search for xraylib and other modules
PKG_CHECK_MODULES([xraylib],libxrl >= 2.15.0)
PKG_CHECK_MODULES([xraylib_f2003],libxrlf03 >= 2.15.0)
PKG_CHECK_MODULES([gsl],gsl >= 1.10)
PKG_CHECK_MODULES([xml2],libxml-2.0 >= 2.7.0)
PKG_CHECK_MODULES([xslt],libxslt >= 1.1.26)
PKG_CHECK_MODULES([fgsl],fgsl >= 0.9.3)
PKG_CHECK_MODULES([glib2],glib-2.0 >= 2.18.0)
if test "x$enable_gui" != xno ; then
	PKG_CHECK_MODULES([gtk],gtk+-3.0 >= 3.0.0, [GTK_FOUND=yes],[GTK_FOUND=no])
	if test "x$GTK_FOUND" = xno ; then
		PKG_CHECK_MODULES([gtk],gtk+-2.0 >= 2.14.0, [GTK_FOUND=yes],[GTK_FOUND=no])
	fi

	#PKG_CHECK_MODULES([gtk2extra],gtkextra-2.0)
	if test "x$GTK_FOUND" = xno && test "x$enable_gui" = xyes ; then
		AC_MSG_ERROR([no GTK found on the system.])
	elif test "x$GTK_FOUND" = xno && test "x$enable_gui" = xcheck ; then
		AC_MSG_WARN([no GTK found on the system. The GUI will not be built.])
	fi
fi

AM_CONDITIONAL([ENABLE_GUI],[test x$GTK_FOUND = xyes])


#check for headers
AC_CHECK_HEADER(stdio.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(errno.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(fcntl.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(signal.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(stdlib.h,,[AC_MSG_ERROR([Required header not found on the system])])
AC_CHECK_HEADER(string.h,,[AC_MSG_ERROR([Required header not found on the system])])
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
fi
AC_CHECK_FUNC(strlen,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(strrchr,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(qsort,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(bsearch,,[AC_MSG_ERROR([Required libc function not found])])
AC_CHECK_FUNC(access,,[AC_MSG_ERROR([Required libc function not found])])

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
#search for chmod
AC_CHECK_PROG([CHMOD],[chmod],[chmod],[fail])
if test x$CHMOD = xfail ; then
	AC_MSG_ERROR([chmod was not found.])
fi



#search for HDF5...
AC_CHECK_PROGS([H5FC],[h5fc-64 h5fc],["fail"])
if test x$H5FC = xfail ; then
	AC_MSG_ERROR([A complete installation of HDF5 is required, including the Fortran 2003 bindings])
fi

H5FC_FC=
H5FC_INCLUDE=
H5FC_LINKER=
H5FC_STATIC=

H5FC_PARSED=$($H5FC -show -shlib)


COUNTER="0"

AC_CHECK_PROGS([BASENAME], [basename gbasename],["fail"])

if test x"$BASENAME" = xfail ; then
	AC_MSG_ERROR([basename shell command not found])
fi

for i in $H5FC_PARSED
do
	if test x$COUNTER = "x0" ; then
		H5FC_FC=$($BASENAME $i)
		COUNTER="1"
	elif test x${i:0:2} = "x-L" || test x${i:0:2} = "x-l" ; then
		H5FC_LINKER+="$i "
	elif test x${i:0:2} = "x-I" ; then
		H5FC_INCLUDE+="$i "
	elif test x${i:0:1} = "x/" && test x${i:${#i}-2:2} = "x.a" ; then
		H5FC_STATIC+="$i "
#	else 
#		AC_MSG_ERROR([Unrecognized output from $H5FC: $i])
	fi
done

FC=$H5FC_FC

AC_SUBST(H5FC_LINKER)
AC_SUBST(H5FC_INCLUDE)
AC_SUBST(H5FC_STATIC)

#echo "H5FC_FC: $H5FC_FC" 
#echo "H5FC_INCLUDE: $H5FC_INCLUDE" 
#echo "H5FC_LINKER: $H5FC_LINKER" 
#echo "H5FC_STATIC: $H5FC_STATIC" 

#search for a fortran compiler (do not check explicitly for the 2003 features yet)
AC_PROG_FC([$FC])
if test -z $FC ; then
	AC_MSG_ERROR([Fortran 2003 compiler was not found])
fi
AC_FC_SRCEXT(f90)


#search for openmp
AC_LANG_PUSH(Fortran)
AC_OPENMP
#AX_OPENMP([
#	AC_DEFINE(HAVE_OPENMP,1,[OpenMP support detected])
#],)
AC_LANG_POP(Fortran)
#AC_SUBST(HAVE_OPENMP)
#OPENMP_FFLAGS=$ax_cv_fc_openmp
#AC_SUBST(OPENMP_FFLAGS)

AC_LANG_PUSH(C)
AC_OPENMP
AC_LANG_POP(C)

XLINKER=""
LDFLAGS_XMIMSIM_LIB=""
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


#search for MPI
OPENMPI_LDFLAGS=
OPENMPI_LIBADD=
OPENMPI_CFLAGS=
AC_ARG_ENABLE([mpi],[AS_HELP_STRING([--disable-mpi],[build without MPI support])],[enable_mpi=$enableval],[enable_mpi=check])

if test x$enable_mpi != "xno" ; then
	AC_CHECK_PROGS([MPICC],[mpicc mpiicc],["fail"])
fi

if test x$MPICC != xfail && test x$enable_mpi != "xno" ; then
	if test x$MPICC = xmpicc ; then
		AC_DEFINE(HAVE_OPENMPI,1,[open-mpi support detected])
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



AC_CONFIG_FILES([Makefile src/Makefile bin/Makefile include/Makefile xml/Makefile etc/Makefile etc/be.ugent.xmi.harvester.plist nsis/Makefile nsis/xmimsim.nsi xmimsim.spec icons/Makefile])
AC_CONFIG_HEADERS([config.h])

AC_OUTPUT
