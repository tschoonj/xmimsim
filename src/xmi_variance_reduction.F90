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

MODULE xmimsim_varred

USE :: xmimsim_aux
USE :: xmimsim_solid_angle

!increase this value to obtain a better simulation of the compton peaks, but
!this comes at a significant computational cost
INTEGER (C_INT), PARAMETER :: N_COMPTON_VARRED = 1

CONTAINS

SUBROUTINE xmi_variance_reduction(photon, inputF, hdf5F, rng)
        !let's use some of that cool Fortran 2003 floating point exception
        !handling as there seems to be a problem with the ACOS calls...
#if DEBUG == 1
        USE, INTRINSIC :: ieee_exceptions
#endif


        !to be called before xmi_update_photon_dirv!!!
        !this way we can still deal with the old dirv
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF

        REAL (C_DOUBLE) , DIMENSION(3) :: detector_point,dirv,coords,&
        point_coll,new_dirv_coords,temp_coords,intersect
        REAL (C_DOUBLE) :: radius, theta
        TYPE (xmi_plane) :: plane_coll, plane
        TYPE (xmi_line) :: line_coll, line
        INTEGER (C_INT) :: step_do_max, step_do_dir,i,j,line_new,comp
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: distances
        REAL (C_DOUBLE) :: Pconv, Pdir, Pesc, Pesc_rayl, Pdir_fluo
        REAL (C_DOUBLE) :: temp_murhod, energy_fluo, dotprod
        INTEGER (C_INT) :: line_last
        REAL (C_DOUBLE) :: total_distance
        REAL (C_DOUBLE) :: detector_solid_angle
        REAL (C_DOUBLE) :: phi
        REAL (C_DOUBLE), DIMENSION(3) :: new_dirv_proj, elecv_norm
        !PROCEDURE (CS_FluorLine_Kissel), POINTER :: xmi_CS_FluorLine
        REAL (C_DOUBLE) :: PK, PL1, PL2, PL3, PM1, PM2, PM3, PM4, PM5
        INTEGER (C_INT) :: channel
        REAL (C_DOUBLE) :: temp_weight

#if DEBUG == 1
        LOGICAL, DIMENSION(3) :: flag_value

        CALL ieee_set_flag(ieee_usual,.FALSE.)
#endif



#if DEBUG == 1
        WRITE (*,'(A)') 'Entering variance reduction'
        WRITE (*,'(A,F12.4)') 'photon%energy: ',photon%energy
#endif


        !ignore negative energies
        IF (photon%energy .LE. energy_threshold) RETURN

#if DEBUG == 1
        IF (photon%energy .GT. 28.0_C_DOUBLE) THEN
                WRITE (output_unit, '(A,ES12.6)') 'var energy:',&
                photon%energy
        ENDIF
#endif



        !select random coordinate on detector surface
        radius = SQRT(fgsl_rng_uniform(rng))*inputF%detector%detector_radius
        theta = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)

        detector_point(1) = 0.0_C_DOUBLE
        detector_point(2) = COS(theta)*radius
        detector_point(3) = SIN(theta)*radius

#if DEBUG == 1
        CALL ieee_get_flag(ieee_usual, flag_value)
        IF (ANY(flag_value)) THEN
                WRITE (*,'(A)') &
                'after detector_point assignment'
                STOP
        ENDIF
#endif
        !work in detector coordinate system
        line_coll%point = MATMUL(inputF%detector%n_detector_orientation_inverse, &
                photon%coords-inputF%geometry%p_detector_window) 
        dirv = MATMUL(inputF%detector%n_detector_orientation_inverse, &
                photon%dirv) 
        line_coll%dirv = detector_point-line_coll%point 

        !check if photon is not headed away from the detector
        IF (DOT_PRODUCT(line_coll%dirv,[1.0_C_DOUBLE,&
        0.0_C_DOUBLE,0.0_C_DOUBLE]) .GE. 0.0_C_DOUBLE) RETURN

        total_distance = xmi_distance_two_points(detector_point,&
        line_coll%point)

#if DEBUG == 1
        CALL ieee_get_flag(ieee_usual, flag_value)
        IF (ANY(flag_value)) THEN
                WRITE (*,'(A)') &
                'after total_distance calculation'
                STOP
        ENDIF
#endif

        CALL normalize_vector(dirv)
        CALL normalize_vector(line_coll%dirv)

        !if it hits the collimator -> game over
!        IF (inputF%detector%collimator_present .EQ. .TRUE.) THEN
!                !there is a collimator
!                plane_coll%point = detector_point
!                plane_coll%point(1)=inputF%geometry%collimator_height
!                plane_coll%normv = [1.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]
!
!                IF (xmi_intersection_plane_line(plane_coll, line_coll,&
!                point_coll) == 0) CALL EXIT(1)
!
!                point_coll(1) = 0.0_C_DOUBLE
!                
!                IF (norm(point_coll) .GT. inputF%detector%collimator_radius) THEN
!#if DEBUG == 1
!
!#endif
!                        RETURN
!                ENDIF
!        ENDIF
        new_dirv_coords = MATMUL(inputF%detector%n_detector_orientation_new,line_coll%dirv)

        

        !so we survived the collimator...
        !calculate the angle between the photon*dirv and line_coll%dirv
        dotprod = DOT_PRODUCT(dirv, line_coll%dirv)

        !avoid floating point exceptions...
        IF (dotprod .GT. 1.0_C_DOUBLE) THEN
                dotprod = 1.0_C_DOUBLE
        ELSEIF (dotprod .LT. -1.0_C_DOUBLE) THEN
                dotprod = -1.0_C_DOUBLE
        ENDIF

        theta = ACOS(dotprod)
        !WRITE (*,'(A,F12.5)') 'dotprod: ',dotprod
        
#if DEBUG == 1
        CALL ieee_get_flag(ieee_usual, flag_value)
        IF (ANY(flag_value)) THEN
                WRITE (*,'(A)') &
                'ACOS theta FPE detected, although unlikely to happen'
                WRITE (*,'(A,ES13.5)') 'theta: ',theta
                WRITE (*,'(A,ES13.5)') 'dotprod: ',dotprod
                STOP
        ENDIF
#endif




#if DEBUG == 1
        WRITE(*,'(A,3F12.5)') 'photon%coords: ',photon%coords
        WRITE(*,'(A,3F12.5)') 'p_detector_window: ',inputF%geometry%p_detector_window
        WRITE(*,'(A,3F12.5)') 'dirv: ',dirv
        WRITE(*,'(A,3F12.5)') 'line_coll%dirv: ',line_coll%dirv
        WRITE(*,'(A,3F12.5)') 'line_coll%point: ',line_coll%point
        WRITE(*,'(A,3F12.5)') 'detector_point: ',detector_point
        WRITE (*,'(A,F18.10)') 'theta: ',theta
        WRITE (*,'(A,F18.10)') 'DOTPRODUCT n and dirv: ',&
        DOT_PRODUCT(inputF%detector%n_sample_orientation_det,line_coll%dirv)
        WRITE (*,'(A,F18.10)') 'DOTPRODUCT n and dirv: ',&
        DOT_PRODUCT(inputF%geometry%n_sample_orientation,new_dirv_coords)
#endif

        !calculate the azimutal scattering angle phi
        new_dirv_proj = new_dirv_coords - &
        DOT_PRODUCT(new_dirv_coords,photon%dirv)* &
        photon%dirv
        CALL normalize_vector(new_dirv_proj)
        elecv_norm = photon%elecv/norm(photon%elecv)

        dotprod = DOT_PRODUCT(new_dirv_proj,elecv_norm)
        !avoid floating point exceptions...
        IF (dotprod .GT. 1.0_C_DOUBLE) THEN
                dotprod = 1.0_C_DOUBLE
        ELSEIF (dotprod .LT. -1.0_C_DOUBLE) THEN
                dotprod = -1.0_C_DOUBLE
        ENDIF

        phi = ACOS(dotprod)

#if DEBUG == 1
        WRITE (6,'(A,3ES12.4)') 'new_dirv_proj:',new_dirv_proj
        WRITE (6,'(A,3ES12.4)') 'elecv:',elecv_norm
        WRITE (6,'(A,F12.4)') 'phi: ',phi
#endif

#if DEBUG == 1
        CALL ieee_get_flag(ieee_usual, flag_value)
        IF (ANY(flag_value)) THEN
                WRITE (*,'(A)') &
                'ACOS phi FPE detected'
                WRITE (*,'(A,ES13.5)') 'DOT_PRODUCT is',&
                DOT_PRODUCT(new_dirv_proj,elecv_norm)
                STOP
        ENDIF
#endif


        !calculate the distances that will be traversed through the layers
        !switching back to lab coordinates
        IF (DOT_PRODUCT(new_dirv_coords,inputF%geometry%n_sample_orientation)&
                .GT. 0.0_C_DOUBLE) THEN
                !moving towards higher layers
                step_do_max = inputF%composition%n_layers
                step_do_dir = 1
        ELSE
                !moving towards lower layers
                step_do_max = 1 
                step_do_dir = -1
        ENDIF

        ALLOCATE(distances(inputF%composition%n_layers))

        temp_coords = photon%coords
        line%dirv  = new_dirv_coords 
        plane%normv = inputF%geometry%n_sample_orientation

        distances = 0.0_C_DOUBLE

        DO i=photon%current_layer,step_do_max,step_do_dir
                !if in current_layer, then calculate distance from point to
                !plane
                !else
                !calculate difference between planes
                line%point = temp_coords
                IF (step_do_dir .EQ. 1) THEN
                        plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE,&
                        inputF%composition%layers(i)%Z_coord_end]
                ELSE
                        plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE,&
                        inputF%composition%layers(i)%Z_coord_begin]
                ENDIF

                IF (xmi_intersection_plane_line(plane, line, intersect) == 0)  &
                        CALL EXIT(1)
#if DEBUG == 1
                WRITE (*,'(A,3F12.4)') 'intersection: ',intersect
                CALL&
                xmi_determinant_matrix(C_LOC(intersect),C_LOC(MATMUL(inputF%detector%n_detector_orientation_new,detector_point)+&
        inputF%geometry%p_detector_window),C_LOC(photon%coords))
#endif

                        
                distances(i) = xmi_distance_two_points(temp_coords,intersect)
                IF (distances(i) .GT. total_distance) THEN
                        distances(i) = total_distance
                        EXIT
                ENDIF
                temp_coords = intersect
                total_distance = total_distance - distances(i)
        ENDDO

#if DEBUG == 1
        WRITE (*,'(A,F18.10)') 'theta: ',theta
        WRITE (*,'(A,ES14.5)') 'distances',distances(1)
        WRITE (*,'(A,3ES14.5)') 'photon coords',photon%coords(:)
        WRITE (*,'(A,3ES14.5)') 'detector_point',MATMUL(inputF%detector%n_detector_orientation_new,detector_point)+&
        inputF%geometry%p_detector_window
#endif

        !okay now the actual variance reduction stuff...
!        ASSOCIATE (layer => &
!        inputF%composition%layers(photon%current_layer),n_ia => &
!        photon%n_interactions)
#define layer inputF%composition%layers(photon%current_layer)
#define n_ia photon%n_interactions
        !first the element independent part
        temp_murhod = 0.0_C_DOUBLE
        DO j=photon%current_layer,step_do_max,step_do_dir
                temp_murhod = temp_murhod + photon%mus(j)*&
                inputF%composition%layers(j)%density*distances(j)
        ENDDO
        Pesc_rayl = EXP(-temp_murhod) 
#if DEBUG == 1
        WRITE (*,'(A,ES14.5)') 'distances',distances(1)
        WRITE (*,'(A,ES12.4)') 'Pesc_rayl: ',Pesc_rayl
#endif



        !
        !
        !       Calculate detector solid angle
        !
        photon%detector_solid_angle_not_found = xmi_get_solid_angle(photon%solid_angle,&
        inputF,detector_solid_angle,rng,photon%coords)+&
        photon%detector_solid_angle_not_found
#if DEBUG == 1
        WRITE (6,'(A,ES14.5)') 'detector_solid_angle:',&
        !'detector_solid_angle_diff:',(inputF%detector%detector_solid_angle-detector_solid_angle)/&
        detector_solid_angle
#endif
#if DEBUG == 2
        detector_solid_angle = inputF%detector%detector_solid_angle
#endif
        Pdir_fluo = detector_solid_angle/4.0_C_DOUBLE/M_PI

        IF (photon%options%use_M_lines .EQ. 1) THEN 
                line_last = M5P5_LINE 
        ELSE 
                line_last = L3Q1_LINE
        ENDIF


        var_red: DO i=1,layer%n_elements
                !
                !       starting with RAYLEIGH
                !
                Pconv = layer%weight(i)/photon%mus(photon%current_layer)
                Pdir = detector_solid_angle&
                *DCSP_Rayl(layer%Z(i),photon%energy,&
                theta, phi)

                !find position in history
                !photon%variance_reduction(photon%current_layer,n_ia)%weight(i,383+1)&
                != Pconv*Pdir*Pesc_rayl*photon%weight
                !photon%variance_reduction(photon%current_layer,n_ia)%energy(i,383+1)&
                != photon%energy
                
                temp_weight = Pconv*Pdir*Pesc_rayl*photon%weight
                photon%var_red_history(layer%Z(i),383+1,n_ia) =&
                photon%var_red_history(layer%Z(i),383+1,n_ia)+temp_weight
                !should be multiplied with detector absorption

                IF (photon%energy .GE. energy_threshold) THEN
                        channel = INT((photon%energy - &
                        inputF%detector%zero)/inputF%detector%gain)
                ELSE
                        channel = -1
                ENDIF

                IF (channel .GE. 0 .AND. channel .LE. UBOUND(photon%channels,DIM=2)) THEN
                        photon%channels(n_ia:, channel) =&
                        photon%channels(n_ia:, channel)+&
                        temp_weight
                ENDIF

#if DEBUG == 2
                IF (i == 1) &
                WRITE (*,'(A,F12.5)') 'Cr-Rayleigh: ',&
                photon%variance_reduction(photon%current_layer,n_ia)%weight(i,383+1)
#endif

                !
                !       moving on with COMPTON
                !
                CALL xmi_compton_varred(photon, i, theta, phi, rng, inputF, hdf5F,&
                distances, detector_solid_angle, step_do_max,step_do_dir)

                !
                !      and finishing with FLUORESCENCE 
                !
#define hdf5_Z inputF%composition%layers(photon%current_layer)%xmi_hdf5_Z_local(i)%Ptr%Zindex
                IF (photon%n_interactions .GT. 1 .AND. &
                photon%history(photon%n_interactions-1,1).LT.0) THEN

                        PK = photon%precalc_xrf_cs(hdf5_Z, K_SHELL, &
                        photon%history(photon%n_interactions-1,3), &
                        ABS(photon%history(photon%n_interactions-1,1)))

                        PL1 = photon%precalc_xrf_cs(hdf5_Z, L1_SHELL, &
                        photon%history(photon%n_interactions-1,3), &
                        ABS(photon%history(photon%n_interactions-1,1)))

                        PL2 = photon%precalc_xrf_cs(hdf5_Z, L2_SHELL, &
                        photon%history(photon%n_interactions-1,3), &
                        ABS(photon%history(photon%n_interactions-1,1)))

                        PL3 = photon%precalc_xrf_cs(hdf5_Z, L3_SHELL, &
                        photon%history(photon%n_interactions-1,3), &
                        ABS(photon%history(photon%n_interactions-1,1)))

                        IF (photon%options%use_M_lines .EQ. 1) THEN 
                        PM1 = photon%precalc_xrf_cs(hdf5_Z, M1_SHELL, &
                        photon%history(photon%n_interactions-1,3), &
                        ABS(photon%history(photon%n_interactions-1,1)))

                        PM2 = photon%precalc_xrf_cs(hdf5_Z, M2_SHELL, &
                        photon%history(photon%n_interactions-1,3), &
                        ABS(photon%history(photon%n_interactions-1,1)))

                        PM3 = photon%precalc_xrf_cs(hdf5_Z, M3_SHELL, &
                        photon%history(photon%n_interactions-1,3), &
                        ABS(photon%history(photon%n_interactions-1,1)))

                        PM4 = photon%precalc_xrf_cs(hdf5_Z, M4_SHELL, &
                        photon%history(photon%n_interactions-1,3), &
                        ABS(photon%history(photon%n_interactions-1,1)))

                        PM5 = photon%precalc_xrf_cs(hdf5_Z, M5_SHELL, &
                        photon%history(photon%n_interactions-1,3), &
                        ABS(photon%history(photon%n_interactions-1,1)))

                        ENDIF
#undef hdf5_Z
                ELSE
                PK = 0.0_C_DOUBLE
                PL1 = 0.0_C_DOUBLE
                PL2 = 0.0_C_DOUBLE
                PL3 = 0.0_C_DOUBLE
                PM1 = 0.0_C_DOUBLE
                PM2 = 0.0_C_DOUBLE
                PM3 = 0.0_C_DOUBLE
                PM4 = 0.0_C_DOUBLE
                PM5 = 0.0_C_DOUBLE

                IF (photon%energy .GE. EdgeEnergy(layer%Z(i),K_SHELL)) &
                        PK = CS_Photo_Partial(layer%Z(i),K_SHELL,&
                        photon%energy)

                !set the XRF cross sections according to the options
                SELECT CASE (photon%xmi_cascade_type)
                        CASE(XMI_CASCADE_NONE)
                        PL1 = PL1_pure_kissel(layer%Z(i),&
                        photon%energy)
                        PL2 = PL2_pure_kissel(layer%Z(i),&
                        photon%energy,PL1)
                        PL3 = PL3_pure_kissel(layer%Z(i),&
                        photon%energy,PL1,PL2)
                        IF (photon%options%use_M_lines .EQ. 1) THEN 
                                PM1 = PM1_pure_kissel(layer%Z(i),&
                                photon%energy)
                                PM2 = &
                                PM2_pure_kissel(layer%Z(i),&
                                photon%energy,PM1)
                                PM3 = &
                                PM3_pure_kissel(layer%Z(i),&
                                photon%energy,PM1,PM2)
                                PM4 = &
                                PM4_pure_kissel(layer%Z(i),&
                                photon%energy,&
                                PM1,PM2,PM3)
                                PM5 = &
                                PM5_pure_kissel(layer%Z(i),&
                                photon%energy,&
                                PM1,PM2,PM3,PM4)

                        ENDIF
                        CASE(XMI_CASCADE_NONRADIATIVE)
                        PL1 = PL1_auger_cascade_kissel(layer%Z(i),&
                        photon%energy,PK)
                        PL2 = PL2_auger_cascade_kissel(layer%Z(i),&
                        photon%energy,PK,PL1)
                        PL3 = PL3_auger_cascade_kissel(layer%Z(i),&
                        photon%energy,&
                        PK,PL1,PL2)
                        IF (photon%options%use_M_lines .EQ. 1) THEN 
                                PM1 =&
                                PM1_auger_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK, PL1, PL2, PL3)
                                PM2 = &
                                PM2_auger_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1)
                                PM3 = &
                                PM3_auger_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1,PM2)
                                PM4 = &
                                PM4_auger_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1,PM2,PM3)
                                PM5 = &
                                PM5_auger_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1,PM2,PM3,PM4)
                        ENDIF
                        CASE(XMI_CASCADE_RADIATIVE)
                        PL1 = PL1_rad_cascade_kissel(layer%Z(i),&
                        photon%energy,PK)
                        PL2 = PL2_rad_cascade_kissel(layer%Z(i),&
                        photon%energy,PK,PL1)
                        PL3 = PL3_rad_cascade_kissel(layer%Z(i),&
                        photon%energy,&
                        PK,PL1,PL2)
                        IF (photon%options%use_M_lines .EQ. 1) THEN 
                                PM1 =&
                                PM1_rad_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK, PL1, PL2, PL3)
                                PM2 = &
                                PM2_rad_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1)
                                PM3 = &
                                PM3_rad_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1,PM2)
                                PM4 = &
                                PM4_rad_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1,PM2,PM3)
                                PM5 = &
                                PM5_rad_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1,PM2,PM3,PM4)
                        ENDIF
                        CASE(XMI_CASCADE_FULL)
                        PL1 = PL1_full_cascade_kissel(layer%Z(i),&
                        photon%energy,PK)
                        PL2 = PL2_full_cascade_kissel(layer%Z(i),&
                        photon%energy,PK,PL1)
                        PL3 = PL3_full_cascade_kissel(layer%Z(i),&
                        photon%energy,&
                        PK,PL1,PL2)
                        IF (photon%options%use_M_lines .EQ. 1) THEN 
                                PM1 =&
                                PM1_full_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK, PL1, PL2, PL3)
                                PM2 = &
                                PM2_full_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1)
                                PM3 = &
                                PM3_full_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1,PM2)
                                PM4 = &
                                PM4_full_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1,PM2,PM3)
                                PM5 = &
                                PM5_full_cascade_kissel(layer%Z(i),&
                                photon%energy,&
                                PK,PL1,PL2,PL3,PM1,PM2,PM3,PM4)
                        ENDIF

                        CASE DEFAULT
                                WRITE (*,'(A)') 'Unsupported cascade type'
                                CALL EXIT(1)
                ENDSELECT
                ENDIF


                DO line_new=KL1_LINE,line_last,-1
                        !needs to be checked for each line... will take
                        !forever...
                        energy_fluo = photon%LineEnergies(layer%Z(i), ABS(line_new))
                        IF (energy_fluo .LT. energy_threshold) CYCLE

                        !Pconv calculation...
                        !Pconv = &
                        !layer%weight(i)*CS_FluorLine_Kissel_cascade&
                        !(layer%Z(i),line_new,REAL(photon%energy,KIND=C_FLOAT))/&
                        !photon%mus(photon%current_layer)

                        SELECT CASE(line_new)
                                CASE(KP5_LINE:KL1_LINE)
                                IF (PK .EQ. 0.0_C_DOUBLE) CYCLE
                                Pconv = &
                                layer%weight(i)*PK*FluorYield&
                                (layer%Z(i),K_SHELL)*&
                                RadRate(layer%Z(i),line_new)/&
                                photon%mus(photon%current_layer)
                                CASE(L1P5_LINE:L1M1_LINE)
                                IF (PL1 .EQ. 0.0_C_DOUBLE) CYCLE
                                Pconv = &
                                layer%weight(i)*PL1*FluorYield&
                                (layer%Z(i),L1_SHELL)*&
                                RadRate(layer%Z(i),line_new)/&
                                photon%mus(photon%current_layer)
                                CASE(L2Q1_LINE:L2M1_LINE)
                                IF (PL2 .EQ. 0.0_C_DOUBLE) CYCLE
                                Pconv = &
                                layer%weight(i)*PL2*FluorYield&
                                (layer%Z(i),L2_SHELL)*&
                                RadRate(layer%Z(i),line_new)/&
                                photon%mus(photon%current_layer)
                                CASE(L3Q1_LINE:L3M1_LINE)
                                IF (PL3 .EQ. 0.0_C_DOUBLE) CYCLE
                                Pconv = &
                                layer%weight(i)*PL3*FluorYield&
                                (layer%Z(i),L3_SHELL)*&
                                RadRate(layer%Z(i),line_new)/&
                                photon%mus(photon%current_layer)
                                CASE(M1P5_LINE:M1N1_LINE)
                                IF (PM1 .EQ. 0.0_C_DOUBLE) CYCLE
                                Pconv = &
                                layer%weight(i)*PM1*FluorYield&
                                (layer%Z(i),M1_SHELL)*&
                                RadRate(layer%Z(i),line_new)/&
                                photon%mus(photon%current_layer)
                                CASE(M2P5_LINE:M2N1_LINE)
                                IF (PM2 .EQ. 0.0_C_DOUBLE) CYCLE
                                Pconv = &
                                layer%weight(i)*PM2*FluorYield&
                                (layer%Z(i),M2_SHELL)*&
                                RadRate(layer%Z(i),line_new)/&
                                photon%mus(photon%current_layer)
                                CASE(M3Q1_LINE:M3N1_LINE)
                                IF (PM3 .EQ. 0.0_C_DOUBLE) CYCLE
                                Pconv = &
                                layer%weight(i)*PM3*FluorYield&
                                (layer%Z(i),M3_SHELL)*&
                                RadRate(layer%Z(i),line_new)/&
                                photon%mus(photon%current_layer)
                                CASE(M4P5_LINE:M4N1_LINE)
                                IF (PM4 .EQ. 0.0_C_DOUBLE) CYCLE
                                Pconv = &
                                layer%weight(i)*PM4*FluorYield&
                                (layer%Z(i),M4_SHELL)*&
                                RadRate(layer%Z(i),line_new)/&
                                photon%mus(photon%current_layer)
                                CASE(M5P5_LINE:M5N1_LINE)
                                IF (PM5 .EQ. 0.0_C_DOUBLE) CYCLE
                                Pconv = &
                                layer%weight(i)*PM5*FluorYield&
                                (layer%Z(i),M5_SHELL)*&
                                RadRate(layer%Z(i),line_new)/&
                                photon%mus(photon%current_layer)
                                CASE DEFAULT
                                !other lines -> just cycle
                                CYCLE
                        ENDSELECT

                        !mus=xmi_mu_calc(inputF%composition,&
                        !energy_fluo)

                        temp_murhod = 0.0_C_DOUBLE
                        DO j=photon%current_layer,step_do_max,step_do_dir
                                temp_murhod = temp_murhod +&
                                photon%precalc_mu_cs(j)%mu(layer%Z(i),ABS(line_new))*&
                                inputF%composition%layers(j)%density*distances(j)
                        ENDDO
                        Pesc = EXP(-temp_murhod) 
                        !photon%variance_reduction(photon%current_layer,n_ia)%weight(i,ABS(line_new))&
                        != Pconv*Pdir_fluo*Pesc*photon%weight
                        !photon%variance_reduction(photon%current_layer,n_ia)%energy(i,ABS(line_new))&
                        != energy_fluo
                        temp_weight=Pconv*Pdir_fluo*Pesc*photon%weight
                        IF (temp_weight .EQ. 0.0_C_DOUBLE) CYCLE
                        photon%var_red_history(layer%Z(i),&
                        ABS(line_new),n_ia) =&
                        photon%var_red_history(layer%Z(i),&
                        ABS(line_new),n_ia)+temp_weight*&
                        photon%det_corr_all(layer%Z(i),&
                        ABS(line_new))

                        IF (energy_fluo .GE. energy_threshold) THEN
                                channel = INT((energy_fluo - &
                                inputF%detector%zero)/inputF%detector%gain)
                        ELSE
                                channel = -1
                        ENDIF

                        IF (channel .GE. 0 .AND. channel .LE. UBOUND(photon%channels,DIM=2)) THEN
                                photon%channels(n_ia:, channel) =&
                                photon%channels(n_ia:, channel)+&
                                temp_weight
                        ENDIF
#if DEBUG == 1
                        IF(line_new .EQ. LA1_LINE) THEN
                                WRITE (*,'(A,F12.4)') 'original energy: ',&
                                photon%energy
                                WRITE (*,'(A,F12.4)') 'new energy: ',&
                                energy_fluo
                                WRITE (*,'(A,F12.4)') 'original mu: ' ,&
                                photon%mus(1)
                                WRITE (*,'(A,F12.4)') 'new mu: ' ,&
                                mus(1)
                                WRITE (*,'(A,ES12.4)') 'Pesc: ' ,&
                                Pesc
                                WRITE (*,'(A,ES12.4)') 'Pconv: ' ,&
                                Pconv
                                WRITE (*,'(A,ES12.4)') 'Pdir_fluo: ' ,&
                                Pdir_fluo
                                WRITE (*,'(A,ES12.4)') 'P: ' ,&
                        photon%variance_reduction(photon%current_layer,n_ia)%weight(i,ABS(line_new))
                        ENDIF
#endif
                ENDDO

        ENDDO var_red


!        ENDASSOCIATE
#undef layer
#undef n_ia

#if DEBUG == 1
        photon%theta_i2 = theta
        photon%phi_i2 = phi
#endif



        RETURN
ENDSUBROUTINE xmi_variance_reduction

SUBROUTINE xmi_update_photon_energy_compton_var_red(photon,current_element_index, theta_i, rng,&
inputF, hdf5F, energy_new)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        REAL (C_DOUBLE), INTENT(IN) :: theta_i
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF
        REAL (C_DOUBLE), INTENT(INOUT) :: energy_new
        INTEGER (C_INT), INTENT(IN) :: current_element_index

        REAL (C_DOUBLE) :: K0K,pz,r
        INTEGER (C_INT) :: pos
        REAL (C_DOUBLE), PARAMETER :: c = 1.2399E-6
        REAL (C_DOUBLE), PARAMETER :: c0 = 4.85E-12
        REAL (C_DOUBLE), PARAMETER :: c1 = 1.456E-2
        REAL (C_DOUBLE) :: c_lamb0, dlamb, c_lamb
        REAL (C_DOUBLE) :: energy, sth2
        INTEGER (C_INT) :: np
        INTEGER :: i

!        ASSOCIATE (hdf5_Z => inputF%composition%layers&
!                (photon%current_layer)%xmi_hdf5_Z_local&
!                (current_element_index)%Ptr)
#define hdf5_Z inputF%composition%layers(photon%current_layer)%xmi_hdf5_Z_local(current_element_index)%Ptr

        !K0K = 1.0_C_DOUBLE + (1.0_C_DOUBLE-COS(theta_i))*photon%energy/XMI_MEC2
        
        !convert to eV
        !WRITE (*,'(A,F14.5)') 'photon energy: ',photon%energy
        !WRITE (*,'(A,F14.5)') 'theta_i: ',theta_i
        !WRITE (*,'(A,I)') 'current_element_index: ',current_element_index
        energy = photon%energy*1000.0_C_DOUBLE
        c_lamb0 = c/(energy)

        sth2 = SIN(theta_i/2.0_C_DOUBLE)

        i=0

        DO
                r = fgsl_rng_uniform(rng)
                !pos = findpos(hdf5_Z%RandomNumbers, r)

                pos = INT(r/(&
                hdf5_Z%RandomNumbers(2)&
                -hdf5_Z%RandomNumbers(1)))+1

                pz = interpolate_simple([&
                hdf5_Z%RandomNumbers(pos),&
                hdf5_Z%DopplerPz_ICDF(pos)],&
                [hdf5_Z%RandomNumbers(pos+1),&
                hdf5_Z%&
                DopplerPz_ICDF(pos+1)], r)
                !np = INT(r*(SIZE(hdf5_Z%RandomNumbers)-1))+1
                !pz = hdf5_Z%DopplerPz_ICDF(np)+(hdf5_Z%DopplerPz_ICDF(np+1)-hdf5_Z%DopplerPz_ICDF(np))*SIZE(hdf5_Z%RandomNumbers)*(r - REAL(np)/REAL(SIZE(hdf5_Z%RandomNumbers)))

#if DEBUG == 2
                WRITE (*,'(A,F12.5)') 'original photon energy: ',photon%energy
                WRITE (*,'(A,F12.5)') 'selected pz: ',pz
                WRITE (*,'(A,F12.5)') 'K0K: ',K0K
                WRITE (*,'(A,F12.5)') 'theta_i: ',theta_i
#endif

                IF (fgsl_rng_uniform(rng) .LT. 0.5_C_DOUBLE) pz = -pz

                dlamb = c0*sth2*sth2-c1*c_lamb0*sth2*pz
                c_lamb = c_lamb0+dlamb
                energy = c/c_lamb/1000.0_C_DOUBLE
                !WRITE (*,'(A,F14.5)') 'compton energy: ',energy
                IF (energy .LE. photon%energy ) EXIT
                IF (i .EQ. 100) THEN 
                        WRITE (*,'(A)') 'Infinite loop in xmi_update_photon_energy_compton_var_red'
                        WRITE (*,'(A,F12.5)') 'initial energy: ',photon%energy
                        WRITE (*,'(A,F12.5)') 'theta_i: ',theta_i
                        CALL EXIT(1)
                ENDIF
                i = i+1
        ENDDO

        !photon%energy = &
        !photon%energy/(K0K-2.0_C_DOUBLE*pz*SIN(theta_i/2.0_C_DOUBLE)*XMI_MOM_MEC)

        energy_new = energy

!        ENDASSOCIATE
#undef hdf5_Z

ENDSUBROUTINE xmi_update_photon_energy_compton_var_red

SUBROUTINE xmi_fluorescence_yield_check_varred_optim(photon, rng, shell, hdf5_Z)
        IMPLICIT NONE
        TYPE (fgsl_rng), INTENT(IN) :: rng
        INTEGER (C_INT), INTENT(IN) :: shell
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (xmi_hdf5_Z), POINTER :: hdf5_Z
        REAL (C_DOUBLE) :: r,fluor_yield_corr


        r = fgsl_rng_uniform(rng)
#if DEBUG == 1
        WRITE (*,'(A,F12.4)') 'FluorYield random number: ',r
#endif

        photon%weight = photon%weight * hdf5_Z%FluorYieldsCorr(shell)


        RETURN
ENDSUBROUTINE xmi_fluorescence_yield_check_varred_optim

SUBROUTINE xmi_compton_varred(photon, i, theta, phi, rng, inputF, hdf5F,&
        distances, detector_solid_angle, step_do_max,step_do_dir)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        INTEGER (C_INT), INTENT(IN) :: i
        REAL (C_DOUBLE), INTENT(IN) :: theta, phi
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF
        REAL (C_DOUBLE), DIMENSION(inputF%composition%n_layers), INTENT(IN) :: distances
        REAL (C_DOUBLE), INTENT(IN) :: detector_solid_angle
        INTEGER (C_INT), INTENT(IN) :: step_do_max, step_do_dir
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: mus
        REAL (C_DOUBLE) :: energy_compton, temp_murhod, Pesc_comp
        REAL (C_DOUBLE) :: Pconv, Pdir
        INTEGER (C_INT) :: j, comp
        INTEGER (C_INT) :: channel
        REAL (C_DOUBLE) :: temp_weight

#define layer inputF%composition%layers(photon%current_layer)
#define n_ia photon%n_interactions
        ALLOCATE(mus(inputF%composition%n_layers))
        DO comp=1,N_COMPTON_VARRED
                CALL xmi_update_photon_energy_compton_var_red(photon, i,theta, rng,&
                inputF, hdf5F, energy_compton)
                mus=xmi_mu_calc(inputF%composition,&
                energy_compton)
                temp_murhod = 0.0_C_DOUBLE
                DO j=photon%current_layer,step_do_max,step_do_dir
                temp_murhod = temp_murhod + mus(j)*&
                inputF%composition%layers(j)%density*distances(j)
                ENDDO
                Pesc_comp = EXP(-temp_murhod) 
                Pconv = layer%weight(i)/photon%mus(photon%current_layer)
                Pdir = detector_solid_angle&
                *DCSP_Compt(layer%Z(i),photon%energy,&
                theta,phi)

                temp_weight = Pconv*Pdir*Pesc_comp*photon%weight/&
                REAL(N_COMPTON_VARRED, KIND=C_DOUBLE)
                photon%var_red_history(layer%Z(i),383+2,n_ia) =&
                photon%var_red_history(layer%Z(i),383+2,n_ia)+temp_weight

                IF (energy_compton .GE. energy_threshold) THEN
                        channel = INT((energy_compton - &
                        inputF%detector%zero)/inputF%detector%gain)
                ELSE
                        channel = -1
                ENDIF

                IF (channel .GE. 0 .AND. channel .LE. UBOUND(photon%channels,DIM=2)) THEN
                        photon%channels(n_ia:, channel) =&
                        photon%channels(n_ia:, channel)+&
                        temp_weight
                ENDIF
        ENDDO
#undef layer
#undef n_ia

ENDSUBROUTINE xmi_compton_varred
ENDMODULE xmimsim_varred
