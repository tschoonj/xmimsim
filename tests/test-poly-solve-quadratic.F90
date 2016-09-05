PROGRAM test

USE :: xmimsim_aux
USE :: fgsl
USE, INTRINSIC :: ISO_C_BINDING
use, INTRINSIC :: iso_fortran_env

IMPLICIT NONE

REAL (C_DOUBLE), DIMENSION(10) :: a = [0.0, 0.0, 0.0,  0.0, 1.0, -5.00,  5.00, 3.00, -2.56,  1E-3]
REAL (C_DOUBLE), DIMENSION(10) :: b = [0.0, 0.0, 3.0,  3.0, 2.0,  8.50, -8.50, 0.00,  7.25, -2.00]
REAL (C_DOUBLE), DIMENSION(10) :: c = [0.0, 1.0, 0.0, -5.0, 1.0, -27.0,  27.0, 6.00, -6E-3, -2.00]
INTEGER :: i

DO i = 1, 10
  CALL poly_solve_quadratic_test(a(i), b(i), c(i))
ENDDO

CONTAINS

SUBROUTINE poly_solve_quadratic_test(a, b, c)
IMPLICIT NONE
REAL (C_DOUBLE), INTENT(IN) :: a, b, c
REAL (C_DOUBLE) :: fgsl_status, xmi_status
REAL (C_DOUBLE) :: fgsl_rv1, fgsl_rv2
REAL (C_DOUBLE) :: xmi_rv1, xmi_rv2

WRITE (output_unit, '(A, ES10.3)') 'a:', a
WRITE (output_unit, '(A, ES10.3)') 'b:', b
WRITE (output_unit, '(A, ES10.3)') 'c:', c

fgsl_status = fgsl_poly_solve_quadratic(a, b, c, fgsl_rv1, fgsl_rv2)
xmi_status = xmi_poly_solve_quadratic(a, b, c, xmi_rv1, xmi_rv2)

CALL assert(fgsl_status == xmi_status, __LINE__)
IF (xmi_status == 1) THEN
  CALL assert(ABS(fgsl_rv1 - xmi_rv1) .LT. 1E-7, __LINE__)
  WRITE (output_unit, '(A, ES10.3)') 'fgsl_rv1:', fgsl_rv1
  WRITE (output_unit, '(A, ES10.3)') 'xmi_rv1:', xmi_rv1
ELSEIF (xmi_status == 2) THEN
  CALL assert(ABS(fgsl_rv1 - xmi_rv1) .LT. 1E-7, __LINE__)
  CALL assert(ABS(fgsl_rv2 - xmi_rv2) .LT. 1E-7, __LINE__)
  WRITE (output_unit, '(A, ES10.3)') 'fgsl_rv1:', fgsl_rv1
  WRITE (output_unit, '(A, ES10.3)') 'fgsl_rv2:', fgsl_rv2
  WRITE (output_unit, '(A, ES10.3)') 'xmi_rv1:', xmi_rv1
  WRITE (output_unit, '(A, ES10.3)') 'xmi_rv2:', xmi_rv2
ENDIF

ENDSUBROUTINE poly_solve_quadratic_test

SUBROUTINE assert(condition, line)
use, INTRINSIC :: iso_c_binding
use, INTRINSIC :: iso_fortran_env
IMPLICIT NONE
LOGICAL, INTENT(IN) :: condition
INTEGER, INTENT(IN) :: line
IF (condition .EQV. .false.) THEN
  WRITE (error_unit, '(A,I4)') 'assert failure at line ', line
  CALL xmi_exit(1_c_int)
ENDIF
ENDSUBROUTINE assert


ENDPROGRAM
