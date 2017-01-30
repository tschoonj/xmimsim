!Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

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

PROGRAM test

USE :: xmimsim_aux

IMPLICIT NONE

REAL (C_DOUBLE) :: x1 = 1.0, x2 = 2.0, y1 = 3.0, y2 = 4.0
TYPE (xmi_ran_trap_workspace) :: workspace
INTEGER :: i
TYPE (fgsl_rng_type) :: rng_type
TYPE (fgsl_rng) :: rng

IF (xmi_ran_trap_workspace_init(x1, x2, y1, y2, workspace) == 0_C_INT) THEN
        STOP
ENDIF

rng_type = fgsl_rng_mt19937
rng = fgsl_rng_alloc(rng_type)
CALL fgsl_rng_set(rng,2_C_LONG)

DO i=1,1000000
WRITE (output_unit, '(F14.6)') xmi_ran_trap(rng, workspace)
ENDDO


ENDPROGRAM
