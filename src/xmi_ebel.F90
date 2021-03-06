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

#define TUBE_MINIMUM_ENERGY 1.0


MODULE xmimsim_ebel

USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux

REAL (C_DOUBLE), PARAMETER :: DEG2RAD=0.01745329

!Table 8 from Hubbell 1994, necessary for the L intensities
!These are probably not the exact same values that Ebel was using, seeing as I
!am using the values recommended in the 2004 erratum, which was published after
!Ebel's paper was published
REAL (C_DOUBLE), PARAMETER, DIMENSION(100) :: omegaL = [&
0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,&
2.17E-4, 3.04E-4, 4.15E-4, 5.53E-4, 7.24E-4, 9.30E-4, 0.00118, 0.00147, 0.00181, 0.00221,&
0.00268, 0.00321, 0.00381, 0.00450, 0.00527, 0.00614, 0.00711, 0.00819, 0.00939, 0.0107,&
0.122, 0.0138, 0.0155, 0.0174, 0.0195, 0.0218, 0.0242, 0.0263, 0.0285, 0.0309,&
0.0335, 0.0363, 0.0393, 0.0425, 0.0459, 0.0495, 0.0534, 0.0575, 0.0618, 0.0655,&
0.0714, 0.0765, 0.0820, 0.0877, 0.0938, 0.100, 0.107, 0.114, 0.121, 0.129,&
0.137, 0.145, 0.153, 0.163, 0.172, 0.182, 0.192, 0.202, 0.212, 0.223,&
0.234, 0.245, 0.257, 0.269, 0.281, 0.293, 0.305, 0.318, 0.331, 0.343,&
0.356, 0.369, 0.382, 0.395, 0.409, 0.422, 0.435, 0.448, 0.461, 0.474,&
0.486, 0.499, 0.511, 0.524, 0.536, 0.548, 0.560, 0.572, 0.583, 0.595&
]

INTERFACE

FUNCTION xmi_cubic_spline_init(x, y, n)&
BIND(C, NAME='xmi_cubic_spline_init')&
RESULT(spline)
        USE, INTRINSIC :: ISO_C_BINDING
        TYPE (C_PTR), INTENT(IN), VALUE :: x, y
        INTEGER (C_SIZE_T), INTENT(IN), VALUE :: n
        TYPE (C_PTR) :: spline
ENDFUNCTION xmi_cubic_spline_init

SUBROUTINE xmi_cubic_spline_free(spline)&
BIND(C, NAME='xmi_cubic_spline_free')
        USE, INTRINSIC :: ISO_C_BINDING
        TYPE (C_PTR), INTENT(IN), VALUE :: spline
ENDSUBROUTINE xmi_cubic_spline_free

FUNCTION xmi_cubic_spline_eval(spline, x)&
BIND(C, NAME='xmi_cubic_spline_eval')&
RESULT(y)
        USE, INTRINSIC :: ISO_C_BINDING
        TYPE (C_PTR), INTENT(IN), VALUE :: spline
        REAL (C_DOUBLE), INTENT(IN), VALUE :: x
        REAL (C_DOUBLE) :: y
ENDFUNCTION xmi_cubic_spline_eval

FUNCTION xmi_cmp_struct_xmi_energy_discrete(a, b)&
BIND(C,NAME='xmi_cmp_struct_xmi_energy_discrete')&
RESULT(rv)
        USE, INTRINSIC :: ISO_C_BINDING
        IMPORT :: xmi_energy_discrete
        IMPLICIT NONE
        TYPE (xmi_energy_discrete), INTENT(IN) :: a, b
        INTEGER (C_INT) :: rv
ENDFUNCTION xmi_cmp_struct_xmi_energy_discrete

FUNCTION xmi_cmp_struct_xmi_energy_continuous(a, b)&
BIND(C,NAME='xmi_cmp_struct_xmi_energy_continuous')&
RESULT(rv)
        USE, INTRINSIC :: ISO_C_BINDING
        IMPORT :: xmi_energy_continuous
        IMPLICIT NONE
        TYPE (xmi_energy_continuous), INTENT(IN) :: a, b
        INTEGER (C_INT) :: rv
ENDFUNCTION xmi_cmp_struct_xmi_energy_continuous
ENDINTERFACE

CONTAINS


!
!
!xray tube related functions
!
!

REAL (C_DOUBLE) FUNCTION fcorr(Z)
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN) :: Z

        IF (Z .GE. 80_C_INT) THEN
                fcorr = 1.0_C_DOUBLE
                RETURN
        ENDIF

        fcorr = -0.4814_C_DOUBLE + 0.03781_C_DOUBLE*Z - 2.413E-4_C_DOUBLE*(Z**2)

        RETURN
ENDFUNCTION fcorr


INTEGER (C_INT) FUNCTION xmi_tube_ebel (tube_anode,tube_window,&
tube_filter,tube_voltage, tube_current, tube_angle_electron, &
tube_angle_xray, tube_delta_energy, tube_solid_angle, &
tube_transmission, nefficiencies, energies, efficiencies,&
ebel_excitation)&
BIND(C,NAME='xmi_tube_ebel')

!Reference:
!       H. Ebel, X-Ray Spectrometry 28 (1999) 255-266
!       Tube voltage from 5 to 50 kV
!       Electron incident angle from 50 to 90 deg.
!       X-Ray take off angle from 90 to 5 deg.
!
!
!
!Mostly taken from PyMca's XRayTubeEbel.py, with some important differences
!Here support is included for L-lines
!


IMPLICIT NONE

TYPE (C_PTR), VALUE, INTENT(IN) :: tube_anode,tube_window,tube_filter,energies,efficiencies
REAL (C_DOUBLE), VALUE, INTENT(IN) :: tube_voltage, tube_current, &
tube_angle_electron, tube_angle_xray, tube_delta_energy, tube_solid_angle
TYPE (C_PTR), INTENT(INOUT) :: ebel_excitation
TYPE (xmi_excitationC), POINTER :: ebel_excitation_rv
INTEGER (C_INT), VALUE, INTENT(IN) :: tube_transmission
INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: nefficiencies


TYPE (xmi_energy_continuous), ALLOCATABLE, DIMENSION(:) :: ebel_spectrum_cont
TYPE (xmi_energy_discrete), ALLOCATABLE, DIMENSION(:) :: ebel_spectrum_disc
TYPE (xmi_energy_continuous), POINTER, DIMENSION(:) :: ebel_spectrum_cont_rv
TYPE (xmi_energy_discrete), POINTER, DIMENSION(:) :: ebel_spectrum_disc_rv
TYPE (xmi_energy_continuous), ALLOCATABLE, DIMENSION(:) :: ebel_spectrum_cont_temp
TYPE (xmi_energy_discrete), ALLOCATABLE, DIMENSION(:) :: ebel_spectrum_disc_temp


!fortran aux variables
TYPE (xmi_layerC), POINTER :: tube_anodeC, tube_windowC, tube_filterC
TYPE (xmi_layer) :: tube_anodeF, tube_windowF, tube_filterF
INTEGER (C_INT), POINTER, DIMENSION(:) :: Z
REAL (C_DOUBLE), POINTER, DIMENSION(:) :: weight

!characteristic line variables
REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: &
disc_edge_energy,disc_edge_energy_temp
INTEGER (C_INT), ALLOCATABLE, DIMENSION(:) :: disc_lines, disc_lines_temp
REAL (C_DOUBLE) :: my_disc_edge_energy

TYPE (C_PTR) :: spline

!
INTEGER (C_INT) :: i, ndisc, ncont, shell1, shell2
REAL (C_DOUBLE) :: sinalphae, sinalphax,sinfactor
!const2 is mentioned only in the conclusion of the article and appears to be an
!average of the const values for both K and L lines...
REAL (C_DOUBLE), PARAMETER :: const1 = 1.35E+09, const2_K = 5.0E+13,zk=2.0,zl=8.0,bk=0.35,bl=0.25
REAL (C_DOUBLE) :: x, m, logz, eta, p3, rhozmax
REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: u0,logu0,tau,p1,p2,rhoz,rhelp
REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: oneovers, r


!bind the C variables to their fortran counterparts
CALL C_F_POINTER(tube_anode, tube_anodeC)
tube_anodeF%n_elements = tube_anodeC%n_elements
tube_anodeF%density = tube_anodeC%density
tube_anodeF%thickness= tube_anodeC%thickness
ALLOCATE(tube_anodeF%Z(tube_anodeC%n_elements),&
tube_anodeF%weight(tube_anodeC%n_elements))
CALL C_F_POINTER(tube_anodeC%Z, Z, [tube_anodeC%n_elements])
tube_anodeF%Z = Z

CALL C_F_POINTER(tube_anodeC%weight, weight, [tube_anodeC%n_elements])
tube_anodeF%weight = weight

IF (C_ASSOCIATED(tube_window)) THEN
        CALL C_F_POINTER(tube_window, tube_windowC)
        tube_windowF%n_elements = tube_windowC%n_elements
        tube_windowF%density = tube_windowC%density
        tube_windowF%thickness= tube_windowC%thickness
        ALLOCATE(tube_windowF%Z(tube_windowC%n_elements),&
        tube_windowF%weight(tube_windowC%n_elements))
        CALL C_F_POINTER(tube_windowC%Z, Z, [tube_windowC%n_elements])
        tube_windowF%Z = Z

        CALL C_F_POINTER(tube_windowC%weight, weight, [tube_windowC%n_elements])
        tube_windowF%weight = weight
ENDIF

IF (C_ASSOCIATED(tube_filter)) THEN
        CALL C_F_POINTER(tube_filter, tube_filterC)
        tube_filterF%n_elements = tube_filterC%n_elements
        tube_filterF%density = tube_filterC%density
        tube_filterF%thickness= tube_filterC%thickness
        ALLOCATE(tube_filterF%Z(tube_filterC%n_elements),&
        tube_filterF%weight(tube_filterC%n_elements))
        CALL C_F_POINTER(tube_filterC%Z, Z, [tube_filterC%n_elements])
        tube_filterF%Z = Z

        CALL C_F_POINTER(tube_filterC%weight, weight, [tube_filterC%n_elements])
        tube_filterF%weight = weight
ENDIF


!angles
sinalphae = SIN(DEG2RAD*tube_angle_electron)
sinalphax = SIN(DEG2RAD*tube_angle_xray)
sinfactor = sinalphae/sinalphax



!Bremsstrahlung energies
!determine number of intervals
!set minimum energy equal to 1 keV
ALLOCATE(ebel_spectrum_cont(FLOOR((tube_voltage-TUBE_MINIMUM_ENERGY)/tube_delta_energy,KIND=C_LONG)+1))
IF (tube_voltage/tube_delta_energy /= NINT(tube_voltage/tube_delta_energy)) THEN
        ALLOCATE(ebel_spectrum_cont_temp(SIZE(ebel_spectrum_cont)+1))
        ebel_spectrum_cont_temp(1:SIZE(ebel_spectrum_cont)) = ebel_spectrum_cont
        CALL MOVE_ALLOC(ebel_spectrum_cont_temp, ebel_spectrum_cont)
ENDIF

!fill up the energies array
DO i=1,SIZE(ebel_spectrum_cont)-1
        ebel_spectrum_cont(i)%energy=TUBE_MINIMUM_ENERGY+(i-1)*tube_delta_energy
ENDDO

ebel_spectrum_cont(SIZE(ebel_spectrum_cont))%energy = tube_voltage
ncont= SIZE(ebel_spectrum_cont)

!let's calculate some variables important for the bremsstrahlung part
x = 1.109_C_DOUBLE - 0.00435_C_DOUBLE * tube_anodeF%Z(1) + 0.00175_C_DOUBLE*tube_voltage
m = 0.1382_C_DOUBLE -0.9211_C_DOUBLE/SQRT(REAL(tube_anodeF%Z(1),KIND=C_DOUBLE))
logz = LOG(REAL(tube_anodeF%Z(1),KIND=C_DOUBLE))
eta = (0.1904_C_DOUBLE -0.2236_C_DOUBLE*logz +0.1292_C_DOUBLE * (logz**2)-0.0149_C_DOUBLE*(logz**3))*tube_voltage**m

p3 = 0.787E-05_C_DOUBLE*SQRT(REAL(tube_anodeF%Z(1),KIND=C_DOUBLE)*0.0135_C_DOUBLE)*tube_voltage**1.5+0.735E-06*tube_voltage**2
rhozmax = AtomicWeight(tube_anodeF%Z(1))*p3/tube_anodeF%Z(1)

ALLOCATE(u0(ncont))
ALLOCATE(logu0(ncont))
ALLOCATE(tau(ncont))
ALLOCATE(p1(ncont))
ALLOCATE(p2(ncont))
ALLOCATE(rhoz(ncont))
ALLOCATE(rhelp(ncont))

u0=tube_voltage/(ebel_spectrum_cont(:)%energy + tube_delta_energy/2.0_C_DOUBLE)
logu0=LOG(u0)
p1=logu0 * (0.49269_C_DOUBLE - 1.09870_C_DOUBLE * eta + 0.78557_C_DOUBLE*eta**2)
p2=0.70256_C_DOUBLE-1.09865_C_DOUBLE*eta+1.00460_C_DOUBLE*eta**2 + logu0
rhoz = rhozmax*(p1/p2)

!photoelectric absorption of Bremsstrahlung
DO i=1,ncont
        tau(i)=CS_Total(tube_anodeF%Z(1),ebel_spectrum_cont(i)%energy&
        + tube_delta_energy/2.0_C_DOUBLE)
ENDDO

rhelp = tau*2.0_C_DOUBLE*rhoz*sinfactor

IF (tube_transmission .EQ. 0_C_INT) THEN
        !no transmission tube
        ebel_spectrum_cont(:)%horizontal_intensity = 0.0_C_DOUBLE
        WHERE (rhelp > 0.0_C_DOUBLE) ebel_spectrum_cont(:)%horizontal_intensity&
          =const1*tube_anodeF%Z(1)*((u0-1.0_C_DOUBLE)**x)*(1.0_C_DOUBLE-EXP(-1.0_C_DOUBLE*rhelp))/rhelp
ELSE
        !transmission case
        WHERE (rhelp > 0.0_C_DOUBLE) ebel_spectrum_cont(:)%horizontal_intensity&
          =const1*tube_anodeF%Z(1)*((u0-1.0_C_DOUBLE)**x)*&
          (EXP(-tau*(tube_anodeF%density*tube_anodeF%thickness -2.0_C_DOUBLE*rhoz)/sinalphax) -&
          EXP(-tau*tube_anodeF%density*tube_anodeF%thickness /sinalphax))/rhelp
ENDIF


!End of Bremsstrahlung calculation

!
!
!Discrete calculation...
!
!

ndisc=0
DO i=KL1_LINE,L3Q1_LINE,-1
        IF (RadRate(tube_anodeF%Z(1),i) > 0.0 .AND.&
        LineEnergy(tube_anodeF%Z(1),i) > TUBE_MINIMUM_ENERGY) THEN
                SELECT CASE (i)
                CASE (KP5_LINE:KL1_LINE)
                        my_disc_edge_energy = REAL(EdgeEnergy(tube_anodeF%Z(1),K_SHELL),C_DOUBLE)
                CASE (L1P5_LINE:L1L2_LINE)
                        my_disc_edge_energy = REAL(EdgeEnergy(tube_anodeF%Z(1),L1_SHELL),C_DOUBLE)
                CASE (L2Q1_LINE:L2L3_LINE)
                        my_disc_edge_energy = REAL(EdgeEnergy(tube_anodeF%Z(1),L2_SHELL),C_DOUBLE)
                CASE (L3Q1_LINE:L3M1_LINE)
                        my_disc_edge_energy = REAL(EdgeEnergy(tube_anodeF%Z(1),L3_SHELL),C_DOUBLE)
                CASE default
                        WRITE (error_unit,*) &
                        'Unknown line encountered in xmi_tube_ebel: check your xraylib version'
                        xmi_tube_ebel = 0
                        RETURN
                ENDSELECT
                !next if edge energy is non-existent
                !can only happen for low Z elements
                IF (my_disc_edge_energy .EQ. 0.0_C_DOUBLE) CYCLE
                !next if tube voltage is too low to excite the shell
                IF (my_disc_edge_energy .GT. tube_voltage) CYCLE
                ndisc = ndisc + 1
                IF (ndisc .EQ. 1) THEN
                        ALLOCATE(disc_lines(1))
                        ALLOCATE(disc_edge_energy(1))
                        ALLOCATE(ebel_spectrum_disc(1))
                ELSE
                        ALLOCATE(disc_lines_temp(ndisc))
                        disc_lines_temp(1:ndisc-1) = disc_lines
                        CALL MOVE_ALLOC(disc_lines_temp,disc_lines)

                        ALLOCATE(disc_edge_energy_temp(ndisc))
                        disc_edge_energy_temp(1:ndisc-1) = disc_edge_energy
                        CALL MOVE_ALLOC(disc_edge_energy_temp,disc_edge_energy)

                        ALLOCATE(ebel_spectrum_disc_temp(ndisc))
                        ebel_spectrum_disc_temp(1:ndisc-1) = ebel_spectrum_disc
                        CALL &
                        MOVE_ALLOC(ebel_spectrum_disc_temp,ebel_spectrum_disc)
                ENDIF
                disc_lines(ndisc) = i
                disc_edge_energy(ndisc) = my_disc_edge_energy

                ebel_spectrum_disc(ndisc)%energy = REAL(LineEnergy(tube_anodeF%Z(1),i),C_DOUBLE)


        ENDIF
ENDDO


DEALLOCATE(u0,logu0,p1,p2,rhoz,rhelp,tau)

IF (ndisc .GT. 0) THEN

ALLOCATE(u0(ndisc))
ALLOCATE(logu0(ndisc))
ALLOCATE(tau(ndisc))
ALLOCATE(p1(ndisc))
ALLOCATE(p2(ndisc))
ALLOCATE(rhoz(ndisc))
ALLOCATE(rhelp(ndisc))
ALLOCATE(oneovers(ndisc))
ALLOCATE(r(ndisc))

u0 = tube_voltage/disc_edge_energy
logu0 = LOG(u0)
oneovers = (SQRT(u0)*logu0 + 2.0_C_DOUBLE*(1.0_C_DOUBLE - SQRT(u0)) )
oneovers = oneovers/(u0*logu0+1.0_C_DOUBLE-u0)
oneovers = 1.0_C_DOUBLE+(16.05_C_DOUBLE*SQRT(0.0135_C_DOUBLE*tube_anodeF%Z(1)/disc_edge_energy)*oneovers)
WHERE (disc_lines .GT. L1L2_LINE)
        !K lines
        oneovers = (zk*bk/tube_anodeF%Z(1))*(u0*logu0 + 1.0_C_DOUBLE - u0) * oneovers
ELSEWHERE
        !L lines
        oneovers = (zl*bl/tube_anodeF%Z(1))*(u0*logu0 + 1.0_C_DOUBLE - u0) * oneovers
ENDWHERE



r = 1.0_C_DOUBLE -&
(0.0081517_C_DOUBLE*tube_anodeF%Z(1))+(3.613e-05*tube_anodeF%Z(1)**2) +&
(0.009583_C_DOUBLE * tube_anodeF%Z(1) * EXP(-1.0_C_DOUBLE*u0)) + (tube_voltage*0.001141_C_DOUBLE)

p1=logu0 * (0.49269_C_DOUBLE - 1.09870_C_DOUBLE * eta + 0.78557_C_DOUBLE*eta**2)
p2=0.70256_C_DOUBLE-1.09865_C_DOUBLE*eta+1.00460_C_DOUBLE*eta**2 + logu0
rhoz = rhozmax*(p1/p2)

DEALLOCATE(tau)
ALLOCATE(tau(ndisc))
DO i=1,ndisc
        tau(i)=CS_Total(tube_anodeF%Z(1),ebel_spectrum_disc(i)%energy)
       !tau(i)=CS_Photo(Z,REAL(disc_energy(i),C_FLOAT))
ENDDO

rhelp = tau * 2.0_C_DOUBLE * rhoz *sinfactor

IF (tube_transmission .EQ. 0_C_INT) THEN
        !no transmission tube
        WHERE (rhelp > 0.0_C_DOUBLE)&
                rhelp = (1.0_C_DOUBLE-EXP(-1.0_C_DOUBLE*rhelp))/rhelp
ELSE
        WHERE (rhelp > 0.0_C_DOUBLE)
                rhelp = (EXP(-tau*(tube_anodeF%density*tube_anodeF%thickness -2.0_C_DOUBLE*rhoz)/sinalphax) -&
                EXP(-tau*tube_anodeF%density*tube_anodeF%thickness /sinalphax))/rhelp
        ENDWHERE
ENDIF

DO i=1,ndisc
        ebel_spectrum_disc(i)%distribution_type = XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC
        SELECT CASE(disc_lines(i))
                CASE (KP5_LINE:KL1_LINE)
                        ebel_spectrum_disc(i)%horizontal_intensity = rhelp(i)*const2_K*oneovers(i)*r(i)*&
                        RadRate(tube_anodeF%Z(1),disc_lines(i))*FluorYield(tube_anodeF%Z(1),K_SHELL)
                CASE (L1P5_LINE:L1L2_LINE)
                        ebel_spectrum_disc(i)%horizontal_intensity = rhelp(i)*fcorr(tube_anodeF%Z(1))*&
                        0.71E13*oneovers(i)*r(i)*RadRate(tube_anodeF%Z(1),disc_lines(i))*omegaL(tube_anodeF%Z(1))
                CASE (L2Q1_LINE:L2L3_LINE)
                        ebel_spectrum_disc(i)%horizontal_intensity = rhelp(i)*fcorr(tube_anodeF%Z(1))*&
                        2.70E13*oneovers(i)*r(i)*RadRate(tube_anodeF%Z(1),disc_lines(i))*omegaL(tube_anodeF%Z(1))
                CASE (L3Q1_LINE:L3M1_LINE)
                        ebel_spectrum_disc(i)%horizontal_intensity = rhelp(i)*4.94E13*oneovers(i)*r(i)*&
                        RadRate(tube_anodeF%Z(1),disc_lines(i))*omegaL(tube_anodeF%Z(1))
        ENDSELECT
        ebel_spectrum_disc(i)%scale_parameter = 0.0_C_DOUBLE

ENDDO
ENDIF
!take window in account
IF (C_ASSOCIATED(tube_window)) THEN
DO i=1,ndisc
        ebel_spectrum_disc(i)%horizontal_intensity=ebel_spectrum_disc(i)%horizontal_intensity*&
        EXP(-1.0_C_DOUBLE*tube_windowF%density*tube_windowF%thickness*&
        CS_Total_Kissel(tube_windowF%Z(1),ebel_spectrum_disc(i)%energy))
ENDDO
DO i=1,ncont
        ebel_spectrum_cont(i)%horizontal_intensity=ebel_spectrum_cont(i)%horizontal_intensity*&
        EXP(-1.0_C_DOUBLE*tube_windowF%density*tube_windowF%thickness*&
        CS_Total_Kissel(tube_windowF%Z(1),ebel_spectrum_cont(i)%energy&
        +tube_delta_energy/2.0_C_DOUBLE))
ENDDO
ENDIF
!and if there's a filter, use that one too
IF (C_ASSOCIATED(tube_filter)) THEN
DO i=1,ndisc
        ebel_spectrum_disc(i)%horizontal_intensity=ebel_spectrum_disc(i)%horizontal_intensity*&
        EXP(-1.0_C_DOUBLE*tube_filterF%density*tube_filterF%thickness*&
        CS_Total_Kissel(tube_filterF%Z(1),ebel_spectrum_disc(i)%energy))
ENDDO
DO i=1,ncont
        ebel_spectrum_cont(i)%horizontal_intensity=ebel_spectrum_cont(i)%horizontal_intensity*&
        EXP(-1.0_C_DOUBLE*tube_filterF%density*tube_filterF%thickness*&
        CS_Total_Kissel(tube_filterF%Z(1),ebel_spectrum_cont(i)%energy))
ENDDO
ENDIF

!correct for the current
ebel_spectrum_cont(:)%horizontal_intensity=ebel_spectrum_cont(:)%horizontal_intensity*&
tube_solid_angle*tube_current/2.0
ebel_spectrum_cont(:)%vertical_intensity=ebel_spectrum_cont(:)%horizontal_intensity
ebel_spectrum_cont(:)%sigma_x = 0.0_C_DOUBLE
ebel_spectrum_cont(:)%sigma_y = 0.0_C_DOUBLE
ebel_spectrum_cont(:)%sigma_xp = 0.0_C_DOUBLE
ebel_spectrum_cont(:)%sigma_yp = 0.0_C_DOUBLE

IF (ndisc .GT. 0) THEN
ebel_spectrum_disc(:)%horizontal_intensity=ebel_spectrum_disc(:)%horizontal_intensity*&
tube_solid_angle*tube_current/2.0
ebel_spectrum_disc(:)%vertical_intensity=ebel_spectrum_disc(:)%horizontal_intensity
ebel_spectrum_disc(:)%sigma_x = 0.0_C_DOUBLE
ebel_spectrum_disc(:)%sigma_y = 0.0_C_DOUBLE
ebel_spectrum_disc(:)%sigma_xp = 0.0_C_DOUBLE
ebel_spectrum_disc(:)%sigma_yp = 0.0_C_DOUBLE
ENDIF

! apply transmission efficiency profile (if provided)
IF (nefficiencies .GT. 0) THEN
        spline = xmi_cubic_spline_init(energies, efficiencies, nefficiencies)
        DO i=1,ncont
                ebel_spectrum_cont(i)%horizontal_intensity = ebel_spectrum_cont(i)%horizontal_intensity*&
                xmi_cubic_spline_eval(spline, ebel_spectrum_cont(i)%energy)
                ebel_spectrum_cont(i)%vertical_intensity = ebel_spectrum_cont(i)%horizontal_intensity
        ENDDO
        DO i=1,ndisc
                ebel_spectrum_disc(i)%horizontal_intensity = ebel_spectrum_disc(i)%horizontal_intensity*&
                xmi_cubic_spline_eval(spline, ebel_spectrum_disc(i)%energy)
                ebel_spectrum_disc(i)%vertical_intensity = ebel_spectrum_disc(i)%horizontal_intensity
        ENDDO
        CALL xmi_cubic_spline_free(spline)
ENDIF

ALLOCATE(ebel_excitation_rv)
ALLOCATE(ebel_spectrum_cont_rv(ncont))
IF (ndisc .GT. 0) THEN
ALLOCATE(ebel_spectrum_disc_rv(ndisc))
ENDIF

IF (ndisc .GT. 0) THEN
ebel_spectrum_disc_rv = ebel_spectrum_disc
ENDIF
ebel_spectrum_cont_rv = ebel_spectrum_cont

ebel_excitation_rv%n_discrete = ndisc
ebel_excitation_rv%n_continuous = ncont
ebel_excitation_rv%continuous = C_LOC(ebel_spectrum_cont_rv(1))
IF (ndisc .GT. 0) THEN
ebel_excitation_rv%discrete = C_LOC(ebel_spectrum_disc_rv(1))
ELSE
ebel_excitation_rv%discrete= C_NULL_PTR
ENDIF




!CALL qsort(C_LOC(ebel_spectrum_rv(1)), INT(n_ebel_spectrum,KIND=C_SIZE_T),&
!INT(7*8, KIND=C_SIZE_T), C_FUNLOC(xmi_cmp_struct_xmi_energy))

ebel_excitation = C_LOC(ebel_excitation_rv)

xmi_tube_ebel=1

ENDFUNCTION xmi_tube_ebel

ENDMODULE
