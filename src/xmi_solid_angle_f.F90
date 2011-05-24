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

MODULE xmimsim_solid_angle

USE :: xmimsim_aux
USE :: omp_lib
USE, INTRINSIC :: ISO_C_BINDING
USE :: fgsl

INTEGER (C_LONG), PARAMETER :: grid_dims_r_n = 1000, grid_dims_theta_n = 1000 
!INTEGER (C_LONG), PARAMETER :: grid_dims_r_n = 1, grid_dims_theta_n = 1 
INTEGER (C_LONG), PARAMETER :: hits_per_single = 5000

TYPE, BIND(C) :: xmi_solid_angleC
        TYPE (C_PTR) :: solid_angles
        INTEGER (C_LONG) :: grid_dims_r_n
        INTEGER (C_LONG) :: grid_dims_theta_n
        TYPE (C_PTR) :: grid_dims_r_vals
        TYPE (C_PTR) :: grid_dims_theta_vals
        TYPE (C_PTR) :: xmi_input_string
ENDTYPE



CONTAINS

SUBROUTINE xmi_solid_angle_calculation(inputFPtr, solid_anglePtr,input_string)&
BIND(C,NAME='xmi_solid_angle_calculation')
        !let's use some of that cool Fortran 2003 floating point exception
        !handling as there seems to be a problem with the ACOS calls...
#if DEBUG == 1
        USE, INTRINSIC :: ieee_exceptions
#endif


        IMPLICIT NONE

        TYPE (C_PTR), VALUE, INTENT(IN) :: inputFPtr
        TYPE (C_PTR), INTENT(INOUT) :: solid_anglePtr
        TYPE (C_PTR), VALUE, INTENT(IN) :: input_string
        TYPE (xmi_solid_angleC), POINTER :: solid_angle
        TYPE (xmi_input), POINTER :: inputF

        REAL (C_DOUBLE), DIMENSION(2) :: grid_dims_r, grid_dims_theta
        REAL (C_DOUBLE), ALLOCATABLE, TARGET, SAVE, DIMENSION(:) :: grid_dims_r_vals,&
        grid_dims_theta_vals
        INTEGER (C_LONG) :: i,j
        REAL (C_DOUBLE), ALLOCATABLE, TARGET, SAVE, DIMENSION(:,:) :: solid_angles 
        INTEGER :: max_threads, thread_num
        TYPE (fgsl_rng_type) :: rng_type
        TYPE (fgsl_rng) :: rng
        INTEGER (C_LONG), ALLOCATABLE, TARGET, DIMENSION(:) :: seeds
        INTEGER (C_INT) :: xmlstringlength
#if DEBUG == 1
        LOGICAL, DIMENSION(3) :: flag_value

        CALL ieee_set_flag(ieee_usual,.FALSE.)
#endif

        !write message 
        WRITE (6,'(A)') 'Precalculating solid angle grid'
        WRITE (6,'(A)') 'This could take a long time...'


        CALL C_F_POINTER(inputFPtr, inputF)


        !first step: determine grid dimensions
        !calculate distance from detector center to beginning of first line and
        !end of last line
        grid_dims_r(2) = MAX(xmi_distance_two_points([0.0_C_DOUBLE, 0.0_C_DOUBLE,&
                                        inputF%composition%layers(1)%Z_coord_begin],&
                                        inputF%geometry%p_detector_window),&
                                        xmi_distance_two_points([0.0_C_DOUBLE, 0.0_C_DOUBLE,&
                                        inputF%composition%layers(inputF%composition%n_layers)&
                                        %Z_coord_end],&
                                        inputF%geometry%p_detector_window))*2.0_C_DOUBLE
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
        IF (inputF%detector%collimator_present .EQV. .FALSE.) THEN
                grid_dims_theta(1)= 0.00001_C_DOUBLE
        ELSE
                !assume cylindrical collimator for now
                grid_dims_theta(1) = &
                ATAN(inputF%geometry%collimator_height/&
                (inputF%detector%collimator_radius+inputF%detector%detector_radius))
        ENDIF

        IF (ALLOCATED(grid_dims_r_vals)) DEALLOCATE(grid_dims_r_vals)
        IF (ALLOCATED(grid_dims_theta_vals)) DEALLOCATE(grid_dims_theta_vals)
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
        max_threads = omp_get_max_threads()

        ALLOCATE(seeds(max_threads))

        !fetch some seeds
        IF (xmi_get_random_numbers(C_LOC(seeds), INT(max_threads,KIND=C_LONG)) == 0) RETURN

        IF (ALLOCATED(solid_angles)) DEALLOCATE(solid_angles)
        ALLOCATE(solid_angles(grid_dims_r_n,grid_dims_theta_n))

        solid_angles = 0.0_C_DOUBLE
        rng_type = fgsl_rng_mt19937

!$omp parallel default(shared) private(j,rng, thread_num)
        thread_num = omp_get_thread_num()

        rng = fgsl_rng_alloc(rng_type)
        CALL fgsl_rng_set(rng,seeds(thread_num+1))

!$omp do
        DO i=1,grid_dims_r_n
        DO j=1,grid_dims_theta_n
                solid_angles(i,j) = xmi_single_solid_angle_calculation(inputF,&
                grid_dims_r_vals(i), grid_dims_theta_vals(j), rng)
!                solid_angles(1,1) = xmi_single_solid_angle_calculation(inputF,&
!                2.0_C_DOUBLE, M_PI/2.0_C_DOUBLE, rng)
        ENDDO
        ENDDO
!$omp end do

!$omp end parallel


        !put everything in the structure
        ALLOCATE(solid_angle)
        solid_angle%solid_angles = C_LOC(solid_angles)
        solid_angle%grid_dims_r_n = grid_dims_r_n
        solid_angle%grid_dims_theta_n = grid_dims_theta_n
        solid_angle%grid_dims_r_vals = C_LOC(grid_dims_r_vals)
        solid_angle%grid_dims_theta_vals = C_LOC(grid_dims_theta_vals)
        solid_angle%xmi_input_string = input_string 

#if DEBUG == 1
        WRITE (6,'(A,5F13.5)') 'grid_dims_r_vals:',grid_dims_r_vals(1:5)
        WRITE (6,'(A,5F13.5)') 'grid_dims_theta_vals:',grid_dims_theta_vals(1:5)
#endif

        solid_anglePtr = C_LOC(solid_angle)



        RETURN
ENDSUBROUTINE xmi_solid_angle_calculation


FUNCTION xmi_single_solid_angle_calculation(inputF, r1, theta1, rng) &
RESULT(rv)

        !let's use some of that cool Fortran 2003 floating point exception
        !handling as there seems to be a problem with the ACOS calls...
#if DEBUG == 1
        USE, INTRINSIC :: ieee_exceptions
#endif
        IMPLICIT NONE



        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (fgsl_rng), INTENT(IN) :: rng
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
        alpha2 = ATAN(full_cone_base_radius*SIN(theta)/&
        (r + full_cone_base_radius*COS(theta)))

        !full_cone_apex must always be at least equal to beta...
        IF (beta .GT. alpha1 .AND. beta .GT. alpha2) THEN
                full_cone_apex = beta
        ELSE
                full_cone_apex = MAX(alpha1, alpha2)
        ENDIF
                
        cos_full_cone_apex = COS(full_cone_apex)

        full_cone_solid_angle = 2*M_PI*(1.0_C_DOUBLE -&
        cos_full_cone_apex)

        rotation_matrix(:,1) = [1.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]
        rotation_matrix(:,2) = [0.0_C_DOUBLE,&
        -1.0_C_DOUBLE*SIN(theta),COS(theta)]
        rotation_matrix(:,3) = [0.0_C_DOUBLE,&
        -1.0_C_DOUBLE*COS(theta),-1.0_C_DOUBLE*SIN(theta)]

#if DEBUG == 1
        WRITE (6,'(A,F12.6)') 'r: ',r
        WRITE (6,'(A,F12.6)') 'theta: ',theta
        WRITE (6,'(A,F12.6)') 'full_cone_apex:' , full_cone_apex 
        WRITE (6,'(A,F12.6)') 'full_cone_solid_angle' , full_cone_solid_angle

#endif

        detector_hits = 0_C_LONG

        detector_plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]
        detector_plane%normv = detector_normal
        IF (outside_collimator .EQV. .TRUE.) THEN
                collimator_plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE, inputF%geometry%collimator_height]
                collimator_plane%normv = detector_normal
        ENDIF
        
        photon_line%point = [0.0_C_DOUBLE, r1*COS(theta1), r1*SIN(theta1)]

        DO i=1,hits_per_single
            theta_rng = ACOS(1.0_C_DOUBLE-fgsl_rng_uniform(rng)*(1.0_C_DOUBLE-cos_full_cone_apex))
#if DEBUG == 1
            CALL ieee_get_flag(ieee_usual, flag_value)
            IF (ANY(flag_value)) THEN
                WRITE (*,'(A,I)') &
                'FPE in xmi_solid angle line ',__LINE__
                STOP
            ENDIF
#endif
            !theta_rng = fgsl_rng_uniform(rng)*full_cone_apex
            phi_rng = fgsl_rng_uniform(rng)*2.0_C_DOUBLE*M_PI

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


                IF (outside_collimator .EQV. .TRUE.) THEN
                        !calculate intersection with collimator opening
                        IF (xmi_intersection_plane_line(collimator_plane, photon_line,&
                        intersection_point) == 0) THEN
                                WRITE(*,'(A,F12.7)') 'theta: ',theta
                                WRITE(*,'(A,F12.7)') 'r: ',r
                                CALL EXIT(1)
                        ENDIF
                        intersection_point(3) = 0.0_C_DOUBLE
                        IF (norm(intersection_point) .GT. inputF%detector%collimator_radius) &
                                CYCLE

                ENDIF

                IF (xmi_intersection_plane_line(detector_plane, photon_line,&
                intersection_point) == 0) THEN
                        WRITE(*,'(A,F12.7)') 'theta: ',theta
                        WRITE(*,'(A,F12.7)') 'r: ',r
                        CALL EXIT(1)
                ENDIF
#if DEBUG ==1
                WRITE (*,'(A,3F12.4)') 'intersection_point: ',intersection_point
#endif

                IF (norm(intersection_point) .LE. inputF%detector%detector_radius) &
                        detector_hits = detector_hits +1
        ENDDO

#if DEBUG == 1
        WRITE (*,'(A, F12.4)') 'full_cone_solid_angle',full_cone_solid_angle
        WRITE (*,'(A,I10)') 'detector_hits:',detector_hits
        WRITE (*,'(A,F12.4)') 'new_cone_solid_angle',full_cone_solid_angle &
        *REAL(detector_hits,C_DOUBLE)/REAL(hits_per_single,C_DOUBLE)
#endif


        rv = full_cone_solid_angle &
        *REAL(detector_hits,C_DOUBLE)/REAL(hits_per_single,C_DOUBLE)

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
        TYPE (fgsl_rng), INTENT(IN) :: rng
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
