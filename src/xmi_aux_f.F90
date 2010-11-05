MODULE xmimsim_aux

USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

TYPE :: interaction_prob
        REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: energies
        REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: Rayl_and_Compt
ENDTYPE


INTERFACE
!interface for the libc qsort function
SUBROUTINE qsort(base,nmemb,size,compar) BIND(C,NAME='qsort')
        USE,INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        TYPE (C_PTR),VALUE :: base
        INTEGER (C_SIZE_T),VALUE :: nmemb, size
        TYPE (C_FUNPTR),VALUE :: compar
ENDSUBROUTINE

ENDINTERFACE

INTERFACE ASSIGNMENT(=)
        MODULE PROCEDURE assign_interaction_prob
ENDINTERFACE


CONTAINS

!used to compare C_INT's with qsort
FUNCTION C_INT_CMP (a,b) RESULT(rv) BIND(C)
        IMPLICIT NONE
        TYPE(C_PTR),VALUE,INTENT(IN) :: a,b
        INTEGER (C_INT) :: rv
        INTEGER (C_INT), POINTER :: aF,bF

        CALL C_F_POINTER(a,aF)
        CALL C_F_POINTER(b,bF)

        rv = aF-bF

        RETURN

ENDFUNCTION C_INT_CMP

!used to compare C_DOUBLE's with qsort
FUNCTION C_DOUBLE_CMP (a,b) RESULT(rv) BIND(C)
        IMPLICIT NONE
        TYPE(C_PTR),VALUE,INTENT(IN) :: a,b
        INTEGER (C_INT) :: rv
        REAL (C_DOUBLE), POINTER :: aF,bF
        REAL (C_DOUBLE) :: diff

        CALL C_F_POINTER(a,aF)
        CALL C_F_POINTER(b,bF)

        diff = aF-bF

        IF (diff > 0.0_C_DOUBLE) THEN
                rv = 1_C_INT
        ELSEIF (diff < 0.0_C_DOUBLE) THEN
                rv = -1_C_INT
        ELSE
                rv = 0_C_INT
        ENDIF

        RETURN
ENDFUNCTION C_DOUBLE_CMP

!used to compare C_FLOAT's with qsort
FUNCTION C_FLOAT_CMP (a,b) RESULT(rv) BIND(C)
        IMPLICIT NONE
        TYPE(C_PTR),VALUE,INTENT(IN) :: a,b
        INTEGER (C_INT) :: rv
        REAL (C_FLOAT), POINTER :: aF,bF
        REAL (C_FLOAT) :: diff

        CALL C_F_POINTER(a,aF)
        CALL C_F_POINTER(b,bF)

        diff = aF-bF

        IF (diff > 0.0_C_FLOAT) THEN
                rv = 1_C_INT
        ELSEIF (diff < 0.0_C_FLOAT) THEN
                rv = -1_C_INT
        ELSE
                rv = 0_C_INT
        ENDIF

        RETURN

ENDFUNCTION C_FLOAT_CMP

SUBROUTINE assign_interaction_prob(outvar,invar)
        IMPLICIT NONE
        TYPE(interaction_prob), INTENT(INOUT) :: outvar
        REAL (KIND=C_FLOAT),DIMENSION(:),INTENT(IN) :: invar
        INTEGER :: i

#if DEBUG == 1
        WRITE (6,*) 'Entering assign_interaction_prob'
#endif

        !invar = energies2
        ALLOCATE(outvar%energies(SIZE(invar)),outvar%Rayl_and_Compt(SIZE(invar),2))
        DO i=1,SIZE(invar)
                outvar%energies(i)=REAL(invar(i),KIND=C_DOUBLE)
        ENDDO

        RETURN
ENDSUBROUTINE assign_interaction_prob


ENDMODULE
