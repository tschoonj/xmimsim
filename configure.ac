AC_INIT([xmimsim],[0.1],[Tom.Schoonjans@UGent.be])
AC_PREREQ([2.60])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_CANONICAL_HOST

LIB_CURRENT=0
LIB_REVISION=0
LIB_AGE=0
AC_SUBST(LIB_CURRENT)
AC_SUBST(LIB_REVISION)
AC_SUBST(LIB_AGE)


AC_CONFIG_MACRO_DIR([m4])

AC_USE_SYSTEM_EXTENSIONS

#libtool
LT_PREREQ([2.0.0])
LT_INIT([disable-fast-install])
AC_PROG_LIBTOOL

#initialize pkg-config
PKG_PROG_PKG_CONFIG

#search for xraylib and other modules
PKG_CHECK_MODULES([xraylib],libxrlf03 >= 2.13.1)
PKG_CHECK_MODULES([gsl],gsl >= 1.10)
PKG_CHECK_MODULES([xml2],libxml-2.0 >= 2.7.0)
PKG_CHECK_MODULES([gtk2],gtk+-2.0 >= 2.20.0)


AC_PROG_CC
#this next line may never be reached...
if test -z $CC ; then
        AC_MSG_ERROR([no C compiler was found on the system.])
	fi

#search for pthreads
ACX_PTHREAD(,
	[AC_MSG_ERROR([POSIX threads are required for building the xmimsim package])]
)

#search for xmlcatalog
AC_CHECK_PROG([XMLCATALOG],[xmlcatalog],[xmlcatalog],[fail])
if test x$XMLCATALOG = xfail ; then
	AC_MSG_ERROR([xmlcatalog was not found. This should be a part of the libxml2 installation which was detected on the system. Consider verifying your PATH variable.])
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

H5FC_PARSED=$($H5FC -show)


COUNTER="0"

for i in $H5FC_PARSED
do
	if test x$COUNTER = "x0" ; then
		H5FC_FC=$i
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

AC_SUBST(H5FC_LINKER)
AC_SUBST(H5FC_INCLUDE)
AC_SUBST(H5FC_STATIC)

#echo "H5FC_FC: $H5FC_FC" 
#echo "H5FC_INCLUDE: $H5FC_INCLUDE" 
#echo "H5FC_LINKER: $H5FC_LINKER" 
#echo "H5FC_STATIC: $H5FC_STATIC" 

#search for a fortran compiler (do not check explicitly for the 2003 features yet)
AC_PROG_FC([$H5FC])
if test -z $FC ; then
	AC_MSG_ERROR([Fortran 2003 compiler was not found])
fi


#search for openmp
AC_LANG_PUSH(Fortran)
AX_OPENMP([
	AC_DEFINE(HAVE_OPENMP,1,[OpenMP support detected])
],[
	AC_DEFINE(HAVE_OPENMP,0,[no OpenMP support detected])
])
AC_LANG_POP(Fortran)
AC_SUBST(HAVE_OPENMP)
OPENMP_FFLAGS=$ax_cv_fc_openmp
AC_SUBST(OPENMP_FFLAGS)

AC_CONFIG_FILES([Makefile src/Makefile bin/Makefile include/Makefile])
AC_CONFIG_HEADERS([config.h])

AC_OUTPUT
