!Copyright (C) 2010-2019 Tom Schoonjans and Laszlo Vincze

!This program is free software: you can redistribute it and/or modify
!it under the terms of the GNU General Public License as published by
!the Free Software Foundation, either version 3 of the License, or
!(at your option) any later version.

!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.

!You should have received a copy of the GNU General Public License
!along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "config.h"

MODULE xmimsim_glib

USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV

PUBLIC

INTEGER (C_SIZE_T), PARAMETER :: xmi_sizeof_double = 8
INTEGER (C_SIZE_T), PARAMETER :: xmi_sizeof_int = 4


INTERFACE
FUNCTION g_malloc(size) BIND(C, NAME='g_malloc') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      INTEGER (C_SIZE_T), INTENT(IN), VALUE :: size
      TYPE (C_PTR) :: rv
ENDFUNCTION
ENDINTERFACE



ENDMODULE

