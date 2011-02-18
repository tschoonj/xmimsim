MODULE xmimsim_solid_angle

USE :: xmimsim_aux
USE :: omp_lib
USE, INTRINSIC :: ISO_C_BINDING
USE :: fgsl

INTEGER (C_LONG), PARAMETER :: grid_dims_r_n = 100, grid_dims_theta_n = 100 
!INTEGER (C_LONG), PARAMETER :: grid_dims_r_n = 1, grid_dims_theta_n = 1 
INTEGER (C_LONG), PARAMETER :: hits_per_single = 1000

TYPE, BIND(C) :: xmi_solid_angle
        TYPE (C_PTR) :: solid_angles
        INTEGER (C_LONG) :: grid_dims_r_n
        INTEGER (C_LONG) :: grid_dims_theta_n
        TYPE (C_PTR) :: grid_dims_r_vals
        TYPE (C_PTR) :: grid_dims_theta_vals
        TYPE (C_PTR) :: xmi_input_string
ENDTYPE



CONTAINS

SUBROUTINE xmi_solid_angle_calculation(inputFPtr, solid_anglePtr,input_file)&
BIND(C,NAME='xmi_solid_angle_calculation')

        IMPLICIT NONE

        TYPE (C_PTR), VALUE, INTENT(IN) :: inputFPtr
        TYPE (C_PTR), INTENT(INOUT) :: solid_anglePtr
        TYPE (C_PTR), VALUE, INTENT(IN) :: input_file
        TYPE (xmi_solid_angle), POINTER :: solid_angle
        TYPE (xmi_input), POINTER :: inputF

        REAL (C_DOUBLE), DIMENSION(2) :: grid_dims_r, grid_dims_theta
        REAL (C_DOUBLE), DIMENSION(:), POINTER  :: grid_dims_r_vals,&
        grid_dims_theta_vals
        INTEGER (C_LONG) :: i,j
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: solid_angles 
        INTEGER :: max_threads, thread_num
        TYPE (fgsl_rng_type) :: rng_type
        TYPE (fgsl_rng) :: rng
        INTEGER (C_LONG), POINTER, DIMENSION(:) :: seeds
        INTEGER (C_INT) :: xmlstringlength


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
                                        inputF%geometry%p_detector_window))
        grid_dims_r(1) = grid_dims_r(2)/grid_dims_r_n

        !calculate useful theta range
        grid_dims_theta(2) = M_PI/2.0
        IF (inputF%detector%collimator_present .EQ. .FALSE.) THEN
                grid_dims_theta(1)= 0.00001_C_DOUBLE
        ELSE
                !assume cylindrical collimator for now
                grid_dims_theta(1) = &
                ATAN(2*inputF%detector%detector_radius/&
                inputF%detector%collimator_height)
        ENDIF

        ALLOCATE(grid_dims_r_vals(grid_dims_r_n))
        ALLOCATE(grid_dims_theta_vals(grid_dims_theta_n))

        DO i=1,grid_dims_r_n
                grid_dims_r_vals(i) = grid_dims_r(1) + &
                (grid_dims_r(grid_dims_r_n)-grid_dims_r(1))*REAL(i-1,C_DOUBLE)&
                /REAL(grid_dims_r_n-1,C_DOUBLE)
        ENDDO
        DO i=1,grid_dims_theta_n
                grid_dims_theta_vals(i) = grid_dims_theta(1) + &
                (grid_dims_theta(grid_dims_theta_n)-grid_dims_theta(1))*REAL(i-1,C_DOUBLE)&
                /REAL(grid_dims_theta_n-1,C_DOUBLE)
        ENDDO
        

        !second step: for every grid point calculate the solid angle
        max_threads = omp_get_max_threads()

        ALLOCATE(seeds(max_threads))

        !fetch some seeds
        IF (xmi_get_random_numbers(C_LOC(seeds), INT(max_threads,KIND=C_LONG)) == 0) RETURN

        ALLOCATE(solid_angles(grid_dims_r_n,grid_dims_theta_n))

        solid_angles = 0.0_C_DOUBLE

!$omp parallel default(shared) private(j)
        thread_num = omp_get_thread_num()
        rng_type = fgsl_rng_mt19937

        rng = fgsl_rng_alloc(rng_type)
        CALL fgsl_rng_set(rng,seeds(thread_num+1))

!$omp do
        DO i=1,grid_dims_r_n
        DO j=1,grid_dims_theta_n
                solid_angles(i,j) = xmi_single_solid_angle_calculation(inputF,&
                grid_dims_r_vals(i), grid_dims_theta_vals(j), rng)
!                solid_angles(1,1) = xmi_single_solid_angle_calculation(inputF,&
!                1.0_C_DOUBLE, M_PI/4.0_C_DOUBLE, rng)
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

        IF (xmi_xmlfile_to_string(input_file, solid_angle%xmi_input_string,&
        xmlstringlength) .EQ. 0_C_INT) CALL EXIT(1)
#if DEBUG == 0
        WRITE (6,'(A,I7)') 'xmlstringlength:',xmlstringlength
#endif


ENDSUBROUTINE xmi_solid_angle_calculation


FUNCTION xmi_single_solid_angle_calculation(inputF, r, theta, rng) &
RESULT(rv)

        IMPLICIT NONE



        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (fgsl_rng), INTENT(IN) :: rng
        REAL (C_DOUBLE), INTENT(IN) :: r, theta
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
        TYPE (xmi_plane) :: detector_plane
        TYPE (xmi_line) :: photon_line
        REAL (C_DOUBLE), DIMENSION(3) :: intersection_point
        INTEGER (C_LONG) :: i


        !define proper cone
        !calculate angle between actual detector plane and base of new cone
        cone_base_normal(1) = 0.0_C_DOUBLE
        cone_base_normal(2) = COS(theta)
        cone_base_normal(3) = SIN(theta)
        
        
        !calculate full_cone_base_radius
        full_cone_base_radius = &
        inputF%detector%detector_radius/SIN(theta)
        full_cone_apex = ATAN(full_cone_base_radius/r)
        full_cone_solid_angle = 2*M_PI*(1.0_C_DOUBLE -&
        COS(full_cone_apex))

        rotation_matrix(:,1) = [1.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]
        rotation_matrix(:,2) = [0.0_C_DOUBLE,&
        -1.0_C_DOUBLE*SIN(theta),COS(theta)]
        rotation_matrix(:,3) = [0.0_C_DOUBLE,&
        -1.0_C_DOUBLE*COS(theta),-1.0_C_DOUBLE*SIN(theta)]

#if DEBUG ==1
        WRITE (6,'(A,F12.6)') 'full_cone_apex:' , full_cone_apex 
#endif

        detector_hits = 0_C_LONG

        DO i=1,hits_per_single
            theta_rng = fgsl_rng_uniform(rng)*full_cone_apex
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
                detector_plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]
                detector_plane%normv = detector_normal
                photon_line%point = [0.0_C_DOUBLE, r*COS(theta), r*sin(theta)]
                photon_line%dirv = dirv_from_detector

                IF (xmi_intersection_plane_line(detector_plane, photon_line,&
                intersection_point) == 0) THEN
                        WRITE(*,'(A,F12.7)') 'theta: ',theta
                        WRITE(*,'(A,F12.7)') 'r: ',r
                        CALL EXIT(1)
                ENDIF

                intersection_point(3) = 0.0_C_DOUBLE
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

ENDMODULE xmimsim_solid_angle
