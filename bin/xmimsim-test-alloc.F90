!Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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


