!Copyright (C) 2010-2013 Tom Schoonjans and Laszlo Vincze

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

MODULE xmimsim_solid_angle

USE :: xmimsim_aux
USE :: omp_lib
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
#ifdef HAVE_EASYRNG
USE :: easyRNG, ONLY : &
        xmi_rng_type => easy_rng_type, &
        xmi_rng_mt19937 => easy_rng_mt19937, &
        xmi_rng_alloc => easy_rng_alloc, &
        xmi_rng_set => easy_rng_set, &
        xmi_rng_free => easy_rng_free
#else
USE :: fgsl, ONLY : &
        xmi_rng_type => fgsl_rng_type, &
        xmi_rng_mt19937 => fgsl_rng_mt19937, &
        xmi_rng_alloc => fgsl_rng_alloc, &
        xmi_rng_set => fgsl_rng_set, &
        xmi_rng_free => fgsl_rng_free
#endif

INTEGER (C_LONG), PARAMETER :: grid_dims_r_n = 1000, grid_dims_theta_n = 1000
!INTEGER (C_LONG), PARAMETER :: grid_dims_r_n = 500, grid_dims_theta_n = 500
INTEGER (C_LONG) :: hits_per_single = 5000
BIND(C,NAME='hits_per_single') :: hits_per_single
REAL (C_DOUBLE), PARAMETER :: M_PI = 3.14159265358979323846_C_DOUBLE



TYPE, BIND(C) :: xmi_solid_angleC
        TYPE (C_PTR) :: solid_angles
        INTEGER (C_LONG) :: grid_dims_r_n
        INTEGER (C_LONG) :: grid_dims_theta_n
        TYPE (C_PTR) :: grid_dims_r_vals
        TYPE (C_PTR) :: grid_dims_theta_vals
        TYPE (C_PTR) :: xmi_input_string
ENDTYPE



CONTAINS

SUBROUTINE xmi_solid_angle_inputs_f(inputFPtr, solid_anglePtr, &
collimator_present, detector_radius, collimator_radius, &
collimator_height) BIND(C,NAME='xmi_solid_angle_inputs_f')
        IMPLICIT NONE

        TYPE (C_PTR), VALUE, INTENT(IN) :: inputFPtr
        TYPE (C_PTR), INTENT(INOUT) :: solid_anglePtr
        INTEGER (C_INT), INTENT(INOUT) :: collimator_present
        REAL (C_FLOAT), INTENT(INOUT) :: detector_radius,&
        collimator_radius, collimator_height


        TYPE (xmi_solid_angleC), POINTER :: solid_angle
        TYPE (xmi_input), POINTER :: inputF

        REAL (C_DOUBLE), DIMENSION(2) :: grid_dims_r, grid_dims_theta
        !REAL (C_DOUBLE), ALLOCATABLE, TARGET, SAVE, DIMENSION(:) :: grid_dims_r_vals,&
        REAL (C_DOUBLE), POINTER, DIMENSION(:) :: grid_dims_r_vals,&
        grid_dims_theta_vals

        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: solid_angles
        INTEGER (C_INT) :: i,j

        REAL (C_DOUBLE), DIMENSION(:), ALLOCATABLE :: mu
        REAL (C_DOUBLE) :: Pabs, S1, S2
        REAL (C_DOUBLE) :: my_sum, myln
        REAL (C_DOUBLE), PARAMETER :: R1 = 0.00001, R2 = 0.99999
        INTEGER (C_INT) :: m
        REAL (C_DOUBLE) :: energy


        CALL C_F_POINTER(inputFPtr, inputF)

        IF (inputF%detector%collimator_present .EQV. .TRUE.) THEN
                collimator_present = 1_C_INT
        ELSE
                collimator_present = 0_C_INT
        ENDIF

        detector_radius = REAL(inputF%detector%detector_radius, C_FLOAT)
        collimator_radius = REAL(inputF%detector%collimator_radius, C_FLOAT)
        collimator_height = REAL(inputF%geometry%collimator_height, C_FLOAT)

        !first step: determine grid dimensions
        !calculate distance from detector center to S1 and
        !S2

        !S1
        ALLOCATE(mu(inputF%composition%n_layers))
        my_sum = 0.0_C_DOUBLE

        IF (inputF%excitation%n_continuous > 1 .AND. &
        inputF%excitation%n_discrete > 0) THEN
                energy = MIN(inputF%excitation%continuous(1)%energy,&
                inputF%excitation%discrete(1)%energy)
        ELSEIF (inputF%excitation%n_continuous > 1) THEN
                energy = inputF%excitation%continuous(1)%energy
        ELSE
                energy = inputF%excitation%discrete(1)%energy
        ENDIF

        DO i = 1, inputF%composition%n_layers
                mu(i) = 0.0_C_DOUBLE
                DO j = 1, inputF%composition%layers(i)%n_elements
                        mu(i) = &
                        mu(i)+CS_Total_Kissel(inputF%composition%layers(i)%Z(j),&
                        energy)*&
                        inputF%composition%layers(i)%weight(j)
                ENDDO
                my_sum = my_sum + mu(i)*inputF%composition%layers(i)%density*&
                inputF%composition%layers(i)%thickness_along_Z
        ENDDO
        Pabs = -1.0_C_DOUBLE*expm1(-1.0_C_DOUBLE*my_sum)
        myln = -1.0_C_DOUBLE * log1p(-1.0*R1*Pabs)

#if DEBUG == 1
        WRITE (6, '(A, ES14.5)') 'S1 Pabs: ',Pabs
        WRITE (6, '(A, ES14.5)') 'S1 myln: ',myln
#endif

        my_sum = 0.0_C_DOUBLE

        DO i=1, inputF%composition%n_layers
                my_sum = my_sum +mu(i)*inputF%composition%layers(i)%density*&
                inputF%composition%layers(i)%thickness_along_Z
                IF (my_sum .GT. myln) THEN
                        m = i-1
                        EXIT
                ENDIF
        ENDDO


        my_sum = 0.0_C_DOUBLE

        DO i = 1, m
                my_sum = my_sum + (1.0_C_DOUBLE - (mu(i)*inputF%composition%layers(i)%&
                density)/(mu(m+1)*inputF%composition%layers(m+1)%&
                density))*inputF%composition%layers(i)%thickness_along_Z
        ENDDO

#if DEBUG == 1
        WRITE (6, '(A, ES14.5)') 'S1 my_sum: ',my_sum
        WRITE (6, '(A, ES14.5)') 'S1 my_sum plus: ',my_sum+&
        myln/(mu(m+1)*inputF%composition%layers(m+1)%density)
        WRITE (6, '(A, ES14.5)') 'S1 Z_coord_begin',inputF%composition%layers(1)%Z_coord_begin
#endif
        S1 = my_sum +myln/(mu(m+1)*inputF%composition%layers(m+1)%density) + &
                inputF%composition%layers(1)%Z_coord_begin


        !S2
        my_sum = 0.0_C_DOUBLE
        IF (inputF%excitation%n_continuous > 1 .AND. &
        inputF%excitation%n_discrete > 0) THEN
                energy =&
                MAX(inputF%excitation%continuous(inputF%excitation%n_continuous)%energy,&
                inputF%excitation%discrete(inputF%excitation%n_discrete)%energy)
        ELSEIF (inputF%excitation%n_continuous > 1) THEN
                energy = inputF%excitation%continuous(inputF%excitation%n_continuous)%energy
        ELSE
                energy = inputF%excitation%discrete(inputF%excitation%n_discrete)%energy
        ENDIF

        DO i = 1, inputF%composition%n_layers
                mu(i) = 0.0_C_DOUBLE
                DO j = 1, inputF%composition%layers(i)%n_elements
                        mu(i) = mu(i)&
                        +CS_Total_Kissel(inputF%composition%layers(i)%Z(j),&
                        energy)&
                        *inputF%composition%layers(i)%weight(j)
                ENDDO
                my_sum = my_sum + mu(i)*inputF%composition%layers(i)%density*&
                inputF%composition%layers(i)%thickness_along_Z
        ENDDO
        Pabs = -1.0_C_DOUBLE*expm1(-1.0_C_DOUBLE*my_sum)
        myln = -1.0_C_DOUBLE * log1p(-1.0*R2*Pabs)

#if DEBUG == 1
        WRITE (6, '(A, ES14.5)') 'S2 Pabs: ',Pabs
        WRITE (6, '(A, ES14.5)') 'S2 myln: ',myln
#endif
        my_sum = 0.0_C_DOUBLE
        DO i=1, inputF%composition%n_layers
                my_sum = my_sum +mu(i)*inputF%composition%layers(i)%density*&
                inputF%composition%layers(i)%thickness_along_Z
                IF (my_sum .GT. myln) THEN
                        m = i-1
                        EXIT
                ENDIF
        ENDDO


        my_sum = 0.0_C_DOUBLE

        DO i = 1, m
                my_sum = my_sum + (1.0_C_DOUBLE - (mu(i)*inputF%composition%layers(i)%&
                density)/(mu(m+1)*inputF%composition%layers(m+1)%&
                density))*inputF%composition%layers(i)%thickness_along_Z
        ENDDO

        S2 = my_sum +myln/(mu(m+1)*inputF%composition%layers(m+1)%density) + &
                inputF%composition%layers(1)%Z_coord_begin

#if DEBUG == 1
        WRITE (6, '(A,ES14.5)') 'S1 Fortran: ',&
        S1!-inputF%geometry%d_sample_source
        WRITE (6, '(A,ES14.5)') 'S2 Fortran: ',&
        S2!-inputF%geometry%d_sample_source
        WRITE (6, '(A,ES14.5)') 'old S1 Fortran: ',&
        inputF%composition%layers(1)%Z_coord_begin
        WRITE (6, '(A,ES14.5)') 'old S2 Fortran: ',&
        inputF%composition%layers(inputF%composition%n_layers)&
        %Z_coord_end
        !CALL xmi_exit(1)
#endif




        grid_dims_r(2) = MAX(xmi_distance_two_points([0.0_C_DOUBLE, 0.0_C_DOUBLE,S1],&
                                        inputF%geometry%p_detector_window),&
                                        xmi_distance_two_points([0.0_C_DOUBLE,&
                                        0.0_C_DOUBLE,S2],&
                                        inputF%geometry%p_detector_window))*1.25_C_DOUBLE
        grid_dims_r(1) = grid_dims_r(2)/grid_dims_r_n

        !calculate useful theta range
        grid_dims_theta(2) = M_PI/2.0_C_DOUBLE
        grid_dims_theta(1)= 0.00001_C_DOUBLE

        ALLOCATE(grid_dims_r_vals(grid_dims_r_n))
        ALLOCATE(grid_dims_theta_vals(grid_dims_theta_n))

        DO i=1,grid_dims_r_n
                grid_dims_r_vals(i) = grid_dims_r(1) + &
                (grid_dims_r(2)-grid_dims_r(1))*REAL(i-1,C_DOUBLE)&
                /REAL(grid_dims_r_n-1,C_DOUBLE)
        ENDDO
        DO i=1,grid_dims_theta_n
                grid_dims_theta_vals(i) = grid_dims_theta(1) + &
                (grid_dims_theta(2)-grid_dims_theta(1))*REAL(i-1,C_DOUBLE)&
                /REAL(grid_dims_theta_n-1,C_DOUBLE)
        ENDDO


        ALLOCATE(solid_angles(grid_dims_r_n,grid_dims_theta_n))

        solid_angles = 0.0_C_DOUBLE
        !put everything in the structure
        ALLOCATE(solid_angle)
        solid_angle%solid_angles = C_LOC(solid_angles(1,1))
        solid_angle%grid_dims_r_n = grid_dims_r_n
        solid_angle%grid_dims_theta_n = grid_dims_theta_n
        solid_angle%grid_dims_r_vals = C_LOC(grid_dims_r_vals(1))
        solid_angle%grid_dims_theta_vals = C_LOC(grid_dims_theta_vals(1))

        solid_anglePtr = C_LOC(solid_angle)
ENDSUBROUTINE xmi_solid_angle_inputs_f

SUBROUTINE xmi_solid_angle_calculation_f(inputFPtr,&
solid_anglePtr,input_string,options)&
BIND(C,NAME='xmi_solid_angle_calculation_f')
        !let's use some of that cool Fortran 2003 floating point exception
        !handling as there seems to be a problem with the ACOS calls...
#if DEBUG == 1
        USE, INTRINSIC :: ieee_exceptions
#endif


        IMPLICIT NONE

        TYPE (C_PTR), VALUE, INTENT(IN) :: inputFPtr
        TYPE (C_PTR), INTENT(INOUT) :: solid_anglePtr
        TYPE (C_PTR), VALUE, INTENT(IN) :: input_string
        TYPE (xmi_main_options), VALUE, INTENT(IN) :: options
        TYPE (xmi_solid_angleC), POINTER :: solid_angle
        TYPE (xmi_input), POINTER :: inputF

        REAL (C_DOUBLE), DIMENSION(2) :: grid_dims_r, grid_dims_theta
        !REAL (C_DOUBLE), ALLOCATABLE, TARGET, SAVE, DIMENSION(:) :: grid_dims_r_vals,&
        REAL (C_DOUBLE), POINTER, DIMENSION(:) :: grid_dims_r_vals,&
        grid_dims_theta_vals
        INTEGER (C_LONG) :: i,j
        !REAL (C_DOUBLE), ALLOCATABLE, TARGET, SAVE, DIMENSION(:,:) :: solid_angles
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: solid_angles
        INTEGER :: max_threads, thread_num
        TYPE (xmi_rng_type) :: rng_type
        TYPE (xmi_rng) :: rng
        INTEGER (C_INT) :: grid_done
        INTEGER (C_LONG), ALLOCATABLE, TARGET, DIMENSION(:) :: seeds
        INTEGER (C_INT) :: xmlstringlength
        INTEGER (OMP_LOCK_KIND) :: omp_lock
#if DEBUG == 1
        LOGICAL, DIMENSION(3) :: flag_value

        CALL ieee_set_flag(ieee_usual,.FALSE.)
#endif
        REAL (C_DOUBLE), DIMENSION(:), ALLOCATABLE :: mu
        REAL (C_DOUBLE) :: Pabs, S1, S2
        REAL (C_DOUBLE) :: my_sum, myln
        REAL (C_DOUBLE), PARAMETER :: R1 = 0.00001, R2 = 0.99999
        INTEGER (C_INT) :: m
        REAL (C_DOUBLE) :: energy


        CALL C_F_POINTER(inputFPtr, inputF)


        !first step: determine grid dimensions
        !calculate distance from detector center to S1 and
        !S2

        !S1
        ALLOCATE(mu(inputF%composition%n_layers))
        my_sum = 0.0_C_DOUBLE

        IF (inputF%excitation%n_continuous > 1 .AND. &
        inputF%excitation%n_discrete > 0) THEN
                energy = MIN(inputF%excitation%continuous(1)%energy,&
                inputF%excitation%discrete(1)%energy)
        ELSEIF (inputF%excitation%n_continuous > 1) THEN
                energy = inputF%excitation%continuous(1)%energy
        ELSE
                energy = inputF%excitation%discrete(1)%energy
        ENDIF

        DO i = 1, inputF%composition%n_layers
                mu(i) = 0.0_C_DOUBLE
                DO j = 1, inputF%composition%layers(i)%n_elements
                        mu(i) = &
                        mu(i)+CS_Total_Kissel(inputF%composition%layers(i)%Z(j),&
                        energy)*&
                        inputF%composition%layers(i)%weight(j)
                ENDDO
                my_sum = my_sum + mu(i)*inputF%composition%layers(i)%density*&
                inputF%composition%layers(i)%thickness_along_Z
        ENDDO
        Pabs = -1.0_C_DOUBLE*expm1(-1.0_C_DOUBLE*my_sum)
        myln = -1.0_C_DOUBLE * log1p(-1.0_C_DOUBLE*R1*Pabs)

#if DEBUG == 1
        WRITE (6, '(A, ES14.5)') 'S1 Pabs: ',Pabs
        WRITE (6, '(A, ES14.5)') 'S1 myln: ',myln
#endif

        my_sum = 0.0_C_DOUBLE

        DO i=1, inputF%composition%n_layers
                my_sum = my_sum +mu(i)*inputF%composition%layers(i)%density*&
                inputF%composition%layers(i)%thickness_along_Z
                IF (my_sum .GT. myln) THEN
                        m = i-1
                        EXIT
                ENDIF
        ENDDO


        my_sum = 0.0_C_DOUBLE

        DO i = 1, m
                my_sum = my_sum + (1.0_C_DOUBLE - (mu(i)*inputF%composition%layers(i)%&
                density)/(mu(m+1)*inputF%composition%layers(m+1)%&
                density))*inputF%composition%layers(i)%thickness_along_Z
        ENDDO

#if DEBUG == 1
        WRITE (6, '(A, ES14.5)') 'S1 my_sum: ',my_sum
        WRITE (6, '(A, ES14.5)') 'S1 my_sum plus: ',my_sum+&
        myln/(mu(m+1)*inputF%composition%layers(m+1)%density)
        WRITE (6, '(A, ES14.5)') 'S1 Z_coord_begin',inputF%composition%layers(1)%Z_coord_begin
#endif
        S1 = my_sum +myln/(mu(m+1)*inputF%composition%layers(m+1)%density) + &
                inputF%composition%layers(1)%Z_coord_begin


        !S2
        my_sum = 0.0_C_DOUBLE
        IF (inputF%excitation%n_continuous > 1 .AND. &
        inputF%excitation%n_discrete > 0) THEN
                energy =&
                MAX(inputF%excitation%continuous(inputF%excitation%n_continuous)%energy,&
                inputF%excitation%discrete(inputF%excitation%n_discrete)%energy)
        ELSEIF (inputF%excitation%n_continuous > 1) THEN
                energy = inputF%excitation%continuous(inputF%excitation%n_continuous)%energy
        ELSE
                energy = inputF%excitation%discrete(inputF%excitation%n_discrete)%energy
        ENDIF

        DO i = 1, inputF%composition%n_layers
                mu(i) = 0.0_C_DOUBLE
                DO j = 1, inputF%composition%layers(i)%n_elements
                        mu(i) = mu(i)&
                        +CS_Total_Kissel(inputF%composition%layers(i)%Z(j),&
                        energy)&
                        *inputF%composition%layers(i)%weight(j)
                ENDDO
                my_sum = my_sum + mu(i)*inputF%composition%layers(i)%density*&
                inputF%composition%layers(i)%thickness_along_Z
        ENDDO
        Pabs = -1.0_C_DOUBLE*expm1(-1.0_C_DOUBLE*my_sum)
        myln = -1.0_C_DOUBLE * log1p(-1.0_C_DOUBLE*R2*Pabs)

#if DEBUG == 1
        WRITE (6, '(A, ES14.5)') 'S2 Pabs: ',Pabs
        WRITE (6, '(A, ES14.5)') 'S2 myln: ',myln
#endif
        my_sum = 0.0_C_DOUBLE
        DO i=1, inputF%composition%n_layers
                my_sum = my_sum +mu(i)*inputF%composition%layers(i)%density*&
                inputF%composition%layers(i)%thickness_along_Z
                IF (my_sum .GT. myln) THEN
                        m = i-1
                        EXIT
                ENDIF
        ENDDO


        my_sum = 0.0_C_DOUBLE

        DO i = 1, m
                my_sum = my_sum + (1.0_C_DOUBLE - (mu(i)*inputF%composition%layers(i)%&
                density)/(mu(m+1)*inputF%composition%layers(m+1)%&
                density))*inputF%composition%layers(i)%thickness_along_Z
        ENDDO

        S2 = my_sum +myln/(mu(m+1)*inputF%composition%layers(m+1)%density) + &
                inputF%composition%layers(1)%Z_coord_begin

#if DEBUG == 1
        WRITE (6, '(A,ES14.5)') 'S1 Fortran: ',&
        S1!-inputF%geometry%d_sample_source
        WRITE (6, '(A,ES14.5)') 'S2 Fortran: ',&
        S2!-inputF%geometry%d_sample_source
        WRITE (6, '(A,ES14.5)') 'old S1 Fortran: ',&
        inputF%composition%layers(1)%Z_coord_begin
        WRITE (6, '(A,ES14.5)') 'old S2 Fortran: ',&
        inputF%composition%layers(inputF%composition%n_layers)&
        %Z_coord_end
        !CALL xmi_exit(1)
#endif




        grid_dims_r(2) = MAX(xmi_distance_two_points([0.0_C_DOUBLE, 0.0_C_DOUBLE,S1],&
                                        inputF%geometry%p_detector_window),&
                                        xmi_distance_two_points([0.0_C_DOUBLE,&
                                        0.0_C_DOUBLE,S2],&
                                        inputF%geometry%p_detector_window))*1.25_C_DOUBLE
        grid_dims_r(1) = grid_dims_r(2)/grid_dims_r_n

#if DEBUG == 1
        WRITE (*,'(A,2ES14.5)') 'grid_dims_r: ',grid_dims_r
        WRITE (*,'(A,2ES14.5)') 'distances: ',xmi_distance_two_points([0.0_C_DOUBLE, 0.0_C_DOUBLE,&
                                        inputF%composition%layers(1)%Z_coord_begin],&
                                        inputF%geometry%p_detector_window),&
                                        xmi_distance_two_points([0.0_C_DOUBLE, 0.0_C_DOUBLE,&
                                        inputF%composition%layers(inputF%composition%n_layers)&
                                        %Z_coord_end],&
                                        inputF%geometry%p_detector_window)
#endif


        !calculate useful theta range
        grid_dims_theta(2) = M_PI/2.0_C_DOUBLE
!        IF (inputF%detector%collimator_present .EQV. .FALSE.) THEN
                grid_dims_theta(1)= 0.00001_C_DOUBLE
!        ELSE
!                !assume cylindrical collimator for now
!                grid_dims_theta(1) = &
!                ATAN(inputF%geometry%collimator_height/&
!                (inputF%detector%collimator_radius+inputF%detector%detector_radius))
!        ENDIF

        ALLOCATE(grid_dims_r_vals(grid_dims_r_n))
        ALLOCATE(grid_dims_theta_vals(grid_dims_theta_n))

        DO i=1,grid_dims_r_n
                grid_dims_r_vals(i) = grid_dims_r(1) + &
                (grid_dims_r(2)-grid_dims_r(1))*REAL(i-1,C_DOUBLE)&
                /REAL(grid_dims_r_n-1,C_DOUBLE)
        ENDDO
        DO i=1,grid_dims_theta_n
                grid_dims_theta_vals(i) = grid_dims_theta(1) + &
                (grid_dims_theta(2)-grid_dims_theta(1))*REAL(i-1,C_DOUBLE)&
                /REAL(grid_dims_theta_n-1,C_DOUBLE)
        ENDDO


        !second step: for every grid point calculate the solid angle
        max_threads = options%omp_num_threads

        ALLOCATE(seeds(max_threads))

        !fetch some seeds
        IF (xmi_get_random_numbers(C_LOC(seeds), INT(max_threads,KIND=C_LONG)) == 0) RETURN

        ALLOCATE(solid_angles(grid_dims_r_n,grid_dims_theta_n))

        solid_angles = 0.0_C_DOUBLE
        rng_type = xmi_rng_mt19937

        !WRITE (6,'(A)') 'before single solid angle calc'

        grid_done=0
        CALL omp_init_lock(omp_lock)

!$omp parallel default(shared) private(j,rng, thread_num)&
!$omp num_threads(max_threads)
        thread_num = omp_get_thread_num()

        rng = xmi_rng_alloc(rng_type)
        CALL xmi_rng_set(rng,seeds(thread_num+1))

!$omp do schedule(dynamic)
        DO i=1,grid_dims_r_n
           DO j=1,grid_dims_theta_n
                solid_angles(i,j) = xmi_single_solid_angle_calculation(inputF,&
                grid_dims_r_vals(i), grid_dims_theta_vals(j), rng)
           ENDDO



          CALL omp_set_lock(omp_lock)
          grid_done = grid_done+1

          IF (grid_done*100/grid_dims_r_n&
          == REAL(grid_done*100)/REAL(grid_dims_r_n) .AND.&
          options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
                CALL xmi_print_progress('Solid angle calculation at'&
                //C_NULL_CHAR,INT(grid_done*100/grid_dims_r_n,KIND=C_INT))
#else
                WRITE (output_unit,'(A,I3,A)')&
                'Solid angle calculation at ',grid_done*100/grid_dims_r_n,' %'
#endif
          CALL omp_unset_lock(omp_lock)
        ENDDO



!$omp end do
        CALL xmi_rng_free(rng)
!$omp end parallel

        CALL omp_destroy_lock(omp_lock)

        !put everything in the structure
        ALLOCATE(solid_angle)
        solid_angle%solid_angles = C_LOC(solid_angles(1,1))
        solid_angle%grid_dims_r_n = grid_dims_r_n
        solid_angle%grid_dims_theta_n = grid_dims_theta_n
        solid_angle%grid_dims_r_vals = C_LOC(grid_dims_r_vals(1))
        solid_angle%grid_dims_theta_vals = C_LOC(grid_dims_theta_vals(1))
        solid_angle%xmi_input_string = input_string

#if DEBUG == 1
        WRITE (6,'(A,5F13.5)') 'grid_dims_r_vals:',grid_dims_r_vals(1:5)
        WRITE (6,'(A,5F13.5)') 'grid_dims_theta_vals:',grid_dims_theta_vals(1:5)
#endif

        solid_anglePtr = C_LOC(solid_angle)

        IF (options%verbose == 1_C_INT) THEN
#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
                CALL xmi_print_progress('Solid angle calculation finished'&
                //C_NULL_CHAR,-1_C_INT)
#else
                WRITE (output_unit,'(A)') &
                'Solid angle calculation finished'
#endif
        ENDIF


        RETURN
ENDSUBROUTINE xmi_solid_angle_calculation_f


FUNCTION xmi_single_solid_angle_calculation(inputF, r1, theta1, rng) &
RESULT(rv)

        !let's use some of that cool Fortran 2003 floating point exception
        !handling as there seems to be a problem with the ACOS calls...
#if DEBUG == 1
        USE, INTRINSIC :: ieee_exceptions
#endif
        USE, INTRINSIC :: ISO_FORTRAN_ENV
        IMPLICIT NONE



        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (xmi_rng), INTENT(IN) :: rng
        REAL (C_DOUBLE), INTENT(IN) :: r1, theta1
        REAL (C_DOUBLE) :: rv

        REAL (C_DOUBLE) :: theta_planes
        REAL (C_DOUBLE), DIMENSION(3), PARAMETER :: detector_normal = [0.0,&
        0.0,1.0]
        REAL (C_DOUBLE), DIMENSION(3) :: cone_base_normal
        REAL (C_DOUBLE) :: full_cone_solid_angle
        REAL (C_DOUBLE) :: full_cone_base_radius
        REAL (C_DOUBLE) :: full_cone_apex
        REAL (C_DOUBLE), DIMENSION(3,3) :: rotation_matrix
        REAL (C_DOUBLE) :: theta_rng, phi_rng
        INTEGER (C_LONG) :: detector_hits
        REAL (C_DOUBLE), DIMENSION(3) :: dirv_from_cone
        REAL (C_DOUBLE), DIMENSION(3) :: dirv_from_detector
        TYPE (xmi_plane) :: detector_plane,collimator_plane
        TYPE (xmi_line) :: photon_line
        REAL (C_DOUBLE), DIMENSION(3) :: intersection_point
        INTEGER (C_LONG) :: i
        REAL (C_DOUBLE) :: r2, r
        REAL (C_DOUBLE) :: theta2, theta
        LOGICAL :: outside_collimator
        REAL (C_DOUBLE) :: alpha1, alpha2, beta
        REAL (C_DOUBLE) :: cos_full_cone_apex

#if DEBUG == 1
        LOGICAL, DIMENSION(3) :: flag_value

        CALL ieee_set_flag(ieee_usual,.FALSE.)
#endif


        !make distinction between several cases: no collimator, cilindrical
        !collimator, conical collimator
        IF (inputF%detector%collimator_present .EQV. .FALSE.) THEN
                !no collimator
                r = r1
                theta = theta1
                full_cone_base_radius = &
                inputF%detector%detector_radius!/SIN(theta)
                outside_collimator = .FALSE.
        ELSEIF &
        (ABS(inputF%detector%collimator_radius-inputF%detector%detector_radius)&
        .LT. 0.000001_C_DOUBLE) THEN
                !cilindrical collimator
                !if position is within the cylinder, ignore the collimator
                !if not, use the collimator opening as base cone
                IF (r1*COS(theta1) .LE. inputF%detector%detector_radius) THEN
                        r = r1
                        theta = theta1
                        full_cone_base_radius = &
                        inputF%detector%detector_radius!/SIN(theta)
                ELSE
                        r = SQRT(r1**2 - &
                        2.0_C_DOUBLE*r1*SIN(theta1)*inputF%geometry%collimator_height + &
                        inputF%geometry%collimator_height**2)
                        theta = ACOS(r1*COS(theta1)/r)
                        full_cone_base_radius = &
                        inputF%detector%collimator_radius!/SIN(theta)
#if DEBUG == 1
                        CALL ieee_get_flag(ieee_usual, flag_value)
                        IF (ANY(flag_value)) THEN
                                WRITE (*,'(A,I)') &
                                'FPE in xmi_solid angle line ',__LINE__
                                STOP
                        ENDIF
#endif
                ENDIF
                IF (r1*SIN(theta1) .GT. inputF%geometry%collimator_height) THEN
                        outside_collimator = .TRUE.
                ELSE
                        outside_collimator = .FALSE.
                ENDIF
        ELSE
                !conical collimator
                !if position is within cone (FULL cone!), ignore the collimator
                !otherwise use collimator opening as base
                IF (r1*COS(theta1) .LE. inputF%detector%detector_radius .AND.&
                r1*SIN(theta1) .LE.&
                (inputF%geometry%collimator_height)*(r1*COS(theta1)-&
                inputF%detector%detector_radius)/(inputF%detector%collimator_radius&
                -inputF%detector%detector_radius)) THEN
                        r = r1
                        theta = theta1
                        full_cone_base_radius = &
                        inputF%detector%detector_radius!/SIN(theta)
                ELSEIF (r1*SIN(theta1) .LE. inputF%geometry%collimator_height)&
                THEN
                        rv = 0.0_C_DOUBLE
                        RETURN
                ELSE
                        r = SQRT(r1**2 - &
                        2.0_C_DOUBLE*r1*SIN(theta1)*inputF%geometry%collimator_height + &
                        inputF%geometry%collimator_height**2)
                        theta = ACOS(r1*COS(theta1)/r)
                        full_cone_base_radius = &
                        inputF%detector%collimator_radius!/SIN(theta)
#if DEBUG == 1
                        CALL ieee_get_flag(ieee_usual, flag_value)
                        IF (ANY(flag_value)) THEN
                                WRITE (*,'(A,I)') &
                                'FPE in xmi_solid angle line ',__LINE__
                                STOP
                        ENDIF
#endif
                ENDIF
                IF (r1*SIN(theta1) .GT. inputF%geometry%collimator_height) THEN
                        outside_collimator = .TRUE.
                ELSE
                        outside_collimator = .FALSE.
                ENDIF
        ENDIF


        !define proper cone
        !calculate angle between actual detector plane and base of new cone
        cone_base_normal(1) = 0.0_C_DOUBLE
        cone_base_normal(2) = COS(theta)
        cone_base_normal(3) = SIN(theta)


        !calculate full_cone_base_radius
        beta = ATAN(full_cone_base_radius/r)
        alpha1 = ATAN(full_cone_base_radius*SIN(theta)/&
        (r - full_cone_base_radius*COS(theta)))
        !alpha2 = ATAN(full_cone_base_radius*SIN(theta)/&
        !(r + full_cone_base_radius*COS(theta)))

        IF (alpha1 .LE. 0.0) &
                alpha1=alpha1+M_PI

        !full_cone_apex must always be at least equal to beta...
        !IF (beta .GT. alpha1 .AND. beta .GT. alpha2) THEN
        !        full_cone_apex = beta
        !ELSE
        !        full_cone_apex = MAX(alpha1, alpha2)
        !ENDIF
        full_cone_apex = MAX(beta, alpha1)



        cos_full_cone_apex = COS(full_cone_apex)

        full_cone_solid_angle = 2*M_PI*(1.0_C_DOUBLE -&
        cos_full_cone_apex)

        rotation_matrix(:,1) = [1.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]
        rotation_matrix(:,2) = [0.0_C_DOUBLE,&
        -1.0_C_DOUBLE*SIN(theta),COS(theta)]
        rotation_matrix(:,3) = [0.0_C_DOUBLE,&
        -1.0_C_DOUBLE*COS(theta),-1.0_C_DOUBLE*SIN(theta)]


        detector_hits = 0_C_LONG

        detector_plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]
        detector_plane%normv = detector_normal
        IF (outside_collimator .EQV. .TRUE.) THEN
                collimator_plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE, inputF%geometry%collimator_height]
                collimator_plane%normv = detector_normal
        ENDIF

        photon_line%point = [0.0_C_DOUBLE, r1*COS(theta1), r1*SIN(theta1)]
!#define DEBUG 1
#if DEBUG == 1
        WRITE (6,'(A,F12.6)') 'r: ',r
        WRITE (6,'(A,F12.6)') 'theta: ',theta
        WRITE (6,'(A,F12.6)') 'r1: ',r1
        WRITE (6,'(A,F12.6)') 'theta1: ',theta1
        WRITE (6,'(A,L4)') 'collimator_present: ',inputF%detector%collimator_present
        WRITE (6,'(A,F12.6)') 'full_cone_apex:' , full_cone_apex
        WRITE (6,'(A,F12.6)') 'full_cone_solid_angle ' , full_cone_solid_angle
        WRITE (6,'(A,F12.6)') 'detector_radius ',inputF%detector%detector_radius
        WRITE (6,'(A,F12.6)') 'collimator_radius ',inputF%detector%collimator_radius
        WRITE (6,'(A,F12.6)') 'collimator_height ',inputF%geometry%collimator_height
        WRITE (6,'(A,I6)') 'hits_per_single ', hits_per_single
        WRITE (6,'(A,3F12.6)') 'photon_line%point ',photon_line%point
        WRITE (6,'(A,3F12.6)') 'detector_plane%point ',detector_plane%point
        WRITE (6,'(A,3F12.6)') 'detector_plane%normv ',detector_plane%normv
#endif
#undef DEBUG


        DO i=1,hits_per_single
            theta_rng = ACOS(1.0_C_DOUBLE-xmi_rng_uniform(rng)*(1.0_C_DOUBLE-cos_full_cone_apex))
            !theta_rng = ACOS(1.0_C_DOUBLE-0.5*(1.0_C_DOUBLE-cos_full_cone_apex))
#if DEBUG == 1
            CALL ieee_get_flag(ieee_usual, flag_value)
            IF (ANY(flag_value)) THEN
                WRITE (*,'(A,I)') &
                'FPE in xmi_solid angle line ',__LINE__
                STOP
            ENDIF
#endif
            !theta_rng = xmi_rng_uniform(rng)*full_cone_apex
            phi_rng = xmi_rng_uniform(rng)*2.0_C_DOUBLE*M_PI
            !phi_rng = 0.5*2.0_C_DOUBLE*M_PI

#if DEBUG == 1
                WRITE (*,'(A,F12.4)') 'theta_rng:',theta_rng
                WRITE (*,'(A,F12.4)') 'phi_rng:',phi_rng
#endif

                dirv_from_cone(1) = SIN(theta_rng)*COS(phi_rng)
                dirv_from_cone(2) = SIN(theta_rng)*SIN(phi_rng)
                dirv_from_cone(3) = COS(theta_rng)
#if DEBUG == 1
                WRITE (*,'(A,3F12.5)') 'dirv_from_cone:',dirv_from_cone
#endif
                dirv_from_detector = MATMUL(rotation_matrix, dirv_from_cone)

#if DEBUG == 1
                WRITE (*,'(A,3F12.5)') 'dirv_from_detector:',dirv_from_detector
#endif
                !calculate intersection with detector plane
                photon_line%dirv = dirv_from_detector

                IF (DOT_PRODUCT(detector_normal,dirv_from_detector) .GE. 0.0)&
                        CYCLE

                IF (outside_collimator .EQV. .TRUE.) THEN
                        !calculate intersection with collimator opening
                        IF (xmi_intersection_plane_line(collimator_plane, photon_line,&
                        intersection_point) == 0) THEN
                                WRITE(error_unit,'(A,F12.7)') 'theta: ',theta
                                WRITE(error_unit,'(A,F12.7)') 'r: ',r
                                CALL xmi_exit(1)
                        ENDIF
                        intersection_point(3) = 0.0_C_DOUBLE
                        IF (norm(intersection_point) .GT. inputF%detector%collimator_radius) &
                                CYCLE

                ENDIF

                IF (xmi_intersection_plane_line(detector_plane, photon_line,&
                intersection_point) == 0) THEN
                        WRITE(error_unit,'(A,F12.7)') 'theta: ',theta
                        WRITE(error_unit,'(A,F12.7)') 'r: ',r
                        CALL xmi_exit(1)
                ENDIF
#if DEBUG ==1
                WRITE (*,'(A,3F12.4)') 'intersection_point: ',intersection_point
#endif

                IF (norm(intersection_point) .LE. inputF%detector%detector_radius) &
                        detector_hits = detector_hits +1
        ENDDO
!#define DEBUG 1
#if DEBUG == 1
        WRITE (6,'(A, F12.4)') 'beta',beta
        WRITE (6,'(A, F12.4)') 'alpha1',alpha1
        WRITE (6,'(A,I10)') 'detector_hits:',detector_hits
        WRITE (6,'(A, 3F12.4)') 'intersection_point',intersection_point
#endif
#undef DEBUG

        rv = full_cone_solid_angle &
        *REAL(detector_hits,C_DOUBLE)/REAL(hits_per_single,C_DOUBLE)



        !CALL xmi_exit(0)
        RETURN
ENDFUNCTION xmi_single_solid_angle_calculation

FUNCTION xmi_get_solid_angle(solid_angles, inputF, detector_solid_angle, rng,&
coords)&
RESULT(rv)
        !let's use some of that cool Fortran 2003 floating point exception
        !handling as there seems to be a problem with the ACOS calls...
#if DEBUG == 1
        USE, INTRINSIC :: ieee_exceptions
#endif

        IMPLICIT NONE
        INTEGER (C_LONG) :: rv
        TYPE (xmi_solid_angle), INTENT(IN) :: solid_angles
        TYPE (xmi_input), INTENT(IN) :: inputF
        REAL (C_DOUBLE), INTENT(OUT) :: detector_solid_angle
        TYPE (xmi_rng), INTENT(IN) :: rng
        REAL (C_DOUBLE), DIMENSION(3), INTENT(IN) :: coords
        REAL (C_DOUBLE) :: r, theta
        REAL (C_DOUBLE), DIMENSION(3) :: dirv
        REAL (C_DOUBLE) :: temp_theta
        INTEGER (C_INT) :: pos_1, pos_2
#if DEBUG == 1
        LOGICAL, DIMENSION(3) :: flag_value

        CALL ieee_set_flag(ieee_usual,.FALSE.)
#endif

        rv = 0

        !calculate angle and distance
        r = xmi_distance_two_points(inputF%geometry%p_detector_window, coords)
        dirv = coords-inputF%geometry%p_detector_window
        CALL normalize_vector(dirv)



        temp_theta = ACOS(DOT_PRODUCT(dirv, inputF%geometry%n_detector_orientation))
#if DEBUG == 1
        CALL ieee_get_flag(ieee_usual, flag_value)
        IF (ANY(flag_value)) THEN
            WRITE (*,'(A,I)') &
           'FPE in xmi_solid angle line ',__LINE__
            STOP
        ENDIF
#endif

        IF (temp_theta .GT. M_PI/2.0_C_DOUBLE) THEN
                !supplement
                temp_theta = M_PI - temp_theta
        ENDIF

        !complement
        theta = (M_PI/2.0_C_DOUBLE) - temp_theta

        !this is bad...
        IF (theta .LT. solid_angles%grid_dims_theta_vals(1)) THEN
                rv = 0
                detector_solid_angle = 0.0_C_DOUBLE
                RETURN
        ENDIF

        !find positions
        pos_1 = findpos(solid_angles%grid_dims_r_vals,r)
        pos_2 = findpos(solid_angles%grid_dims_theta_vals,theta)

#if DEBUG == 1
        WRITE (6,'(A,ES14.6)') 'r: ',r
        WRITE (6,'(A,ES14.6)') 'theta: ',theta
        WRITE (6,'(A,I)') 'pos_1:',pos_1
        WRITE (6,'(A,I)') 'pos_2:',pos_2
#endif

        IF (pos_1 == -1 .OR. pos_2 == -1) THEN
                !not found in solid angles!
                !baddddddd
                !calculate it ourself...
                detector_solid_angle =&
                xmi_single_solid_angle_calculation(inputF, r, theta, rng)
                rv=1
        ELSE
                !found! interpolate...
                detector_solid_angle =&
                bilinear_interpolation(solid_angles%solid_angles,&
                solid_angles%grid_dims_r_vals,&
                solid_angles%grid_dims_theta_vals,&
                r, theta, pos_1, pos_2)
                rv=0
        ENDIF

        RETURN
ENDFUNCTION xmi_get_solid_angle
ENDMODULE xmimsim_solid_angle
