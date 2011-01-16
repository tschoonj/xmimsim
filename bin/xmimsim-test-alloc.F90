PROGRAM test

USE :: xmimsim_main

TYPE (xmi_photon), POINTER :: photon
INTEGER (C_LONG) :: i

!$omp parallel default(shared) private(photon)

!$omp do
DO i=1,10000000
        ALLOCATE(photon)
        ALLOCATE(photon%mus(5))
        ALLOCATE(photon%history(5,2))


        DEALLOCATE(photon%mus)
        DEALLOCATE(photon%history)
        DEALLOCATE(photon)
ENDDO
!$omp enddo

!$omp endparallel

ENDPROGRAM


