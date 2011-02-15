MODULE xmimsim_solid_angle

USE :: xmimsim_aux
USE :: omp_lib
USE, INTRINSIC :: ISO_C_BINDING
USE :: fgsl

INTEGER (C_LONG), PARAMETER :: grid_dims_r_n = 100, grid_dims_theta_n = 100 



CONTAINS

SUBROUTINE xmi_solid_angle_calculation(inputF)

        TYPE (xmi_input), INTENT(IN) :: inputF

        REAL (C_DOUBLE), DIMENSION(2) :: grid_dims_r, grid_dims_theta
        REAL (C_DOUBLE), DIMENSION(:), ALLOCATABLE  :: grid_dims_r_vals,&
        grid_dims_theta_vals
        INTEGER (C_LONG) :: i,j
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: solid_angles 
        INTEGER :: max_threads, thread_num
        TYPE (fgsl_rng_type) :: rng_type
        TYPE (fgsl_rng) :: rng
        INTEGER (C_LONG), POINTER, DIMENSION(:) :: seeds



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
        grid_dims_r(1) = 0.0_C_DOUBLE

        !calculate useful theta range
        grid_dims_theta(2) = M_PI/2.0
        IF (inputF%detector%collimator_present .EQ. .FALSE.) THEN
                grid_dims_theta(1)= 0.0_C_DOUBLE
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
                grid_dims_theta_vals(i) = grid_dims_r(1) + &
                (grid_dims_r(grid_dims_theta_n)-grid_dims_r(1))*REAL(i-1,C_DOUBLE)&
                /REAL(grid_dims_theta_n-1,C_DOUBLE)
        ENDDO
        

        !second step: for every grid point calculate the solid angle
        max_threads = omp_get_max_threads()

        ALLOCATE(seeds(max_threads))

        !fetch some seeds
        IF (xmi_get_random_numbers(C_LOC(seeds), INT(max_threads,KIND=C_LONG)) == 0) RETURN

        ALLOCATE(solid_angles(grid_dims_r_n,grid_dims_theta_n))


!omp parallel default(shared)
        thread_num = omp_get_thread_num()
        rng_type = fgsl_rng_mt19937

        rng = fgsl_rng_alloc(rng_type)
        CALL fgsl_rng_set(rng,seeds(thread_num+1))

!$omp workshare
        FORALL (i=1:grid_dims_r_n, j=1:grid_dims_theta_n)
                solid_angles(i,j) = xmi_single_solid_angle_calculation(inputF,&
                grid_dims_r_vals(i), grid_dims_theta_vals(j), rng)
        ENDFORALL
!$omp end workshare
!omp end parallel


ENDSUBROUTINE xmi_solid_angle_calculation


PURE FUNCTION xmi_single_solid_angle_calculation(inputF, r, theta, rng) &
RESULT(rv)
        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (fgsl_rng), INTENT(IN) :: rng
        REAL (C_DOUBLE), INTENT(IN) :: r, theta
        REAL (C_DOUBLE) :: rv

        REAL (C_DOUBLE) :: theta_planes
        REAL (C_DOUBLE), DIMENSION(3), PARAMETER :: detector_normal = [0.0,&
        0.0,1.0]
        REAL (C_DOUBLE), DIMENSION(3) :: cone_base_normal

        !define proper cone
        !calculate angle between actual detector plane and base of new cone
        cone_base_normal = [0.0_C_DOUBLE, REAL(COS(theta),KIND=C_DOUBLE), REAL(SIN(theta), KIND=C_DOUBLE)]
        theta_planes = DOT_PRODUCT(detector_normal, cone_base_normal)
        




        rv = 0.0_C_DOUBLE

        RETURN
ENDFUNCTION xmi_single_solid_angle_calculation

ENDMODULE xmimsim_solid_angle
