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
