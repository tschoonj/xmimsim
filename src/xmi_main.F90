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

MODULE xmimsim_main

USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux
USE :: xmimsim_varred
USE :: xmimsim_detector
USE :: omp_lib
USE :: fgsl



INTERFACE xmi_coords_dir
        MODULE PROCEDURE xmi_coords_dir_cont, xmi_coords_dir_disc
ENDINTERFACE xmi_coords_dir

INTERFACE xmi_coords_gaussian
        MODULE PROCEDURE xmi_coords_gaussian_cont, xmi_coords_gaussian_disc
ENDINTERFACE xmi_coords_gaussian


!some physical constants
REAL (C_DOUBLE), PARAMETER :: XMI_MEC2 = fgsl_const_mksa_mass_electron*&
        fgsl_const_mksa_speed_of_light**2/&
        fgsl_const_mksa_electron_volt/1000.0_C_DOUBLE
        !source = NIST
REAL (C_DOUBLE), PARAMETER :: momentum_atomic_unit = 1.992851565E-24
REAL (C_DOUBLE), PARAMETER :: XMI_MEC = fgsl_const_mksa_mass_electron*&
        fgsl_const_mksa_speed_of_light
REAL (C_DOUBLE), PARAMETER :: XMI_MOM_MEC = momentum_atomic_unit/XMI_MEC



CONTAINS


FUNCTION xmi_main_msim(inputFPtr, hdf5FPtr, n_mpi_hosts, channelsPtr,&
nchannels, options, brute_historyPtr, var_red_historyPtr, solid_anglesCPtr) BIND(C,NAME='xmi_main_msim') RESULT(rv)


        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN), VALUE :: inputFPtr, hdf5FPtr
        INTEGER (C_INT), VALUE, INTENT(IN) :: n_mpi_hosts, nchannels
        INTEGER (C_INT) :: rv 
        TYPE (xmi_main_options), VALUE, INTENT(IN) :: options
        TYPE (C_PTR), INTENT(INOUT) :: brute_historyPtr, channelsPtr,&
        var_red_historyPtr
        TYPE (xmi_solid_angleC), INTENT(IN) :: solid_anglesCPtr

        TYPE (xmi_hdf5), POINTER :: hdf5F
        TYPE (xmi_input), POINTER :: inputF
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: channelsF
        !REAL (C_DOUBLE), DIMENSION(:,:), ALLOCATABLE, TARGET, SAVE :: channelsF
        INTEGER :: max_threads, thread_num

        TYPE (fgsl_rng_type) :: rng_type
        TYPE (fgsl_rng) :: rng
        INTEGER (C_LONG), ALLOCATABLE, TARGET, DIMENSION(:) :: seeds
        INTEGER (C_LONG) :: i,j,k,l,m,n
        TYPE (xmi_photon), POINTER :: photon,photon_temp,photon_temp2
        REAL (C_DOUBLE) :: hor_ver_ratio
        INTEGER (C_LONG) :: n_photons
        REAL (C_DOUBLE) :: iv_start_energy, iv_end_energy
        REAL (C_DOUBLE) :: iv_start_intensity, iv_end_intensity
        INTEGER :: ipol
        REAL (C_DOUBLE) :: cosalfa, c_alfa, c_ae, c_be
        INTEGER (C_LONG) :: photons_simulated, detector_hits, rayleighs,&
        comptons, einsteins,detector_hits2
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: initial_mus
        INTEGER (C_INT) :: channel,line
        REAL (C_DOUBLE), DIMENSION(:,:), ALLOCATABLE, TARGET :: channels 
        REAL (C_DOUBLE), DIMENSION(:,:,:), ALLOCATABLE :: brute_history
        REAL (C_DOUBLE), DIMENSION(:,:,:), POINTER :: brute_historyF
        !REAL (C_DOUBLE), DIMENSION(:,:,:), ALLOCATABLE, TARGET, SAVE :: brute_historyF
        REAL (C_DOUBLE), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: var_red_history
        !REAL (C_DOUBLE), DIMENSION(:,:,:), ALLOCATABLE, TARGET, SAVE :: var_red_historyF
        REAL (C_DOUBLE), DIMENSION(:,:,:), POINTER :: var_red_historyF
        INTEGER (C_INT), DIMENSION(K_SHELL:M5_SHELL) :: last_shell
        INTEGER (C_INT) :: element
        REAL (C_DOUBLE) :: exc_corr,det_corr, total_intensity
        INTEGER (C_INT) :: xmi_cascade_type
        REAL (C_DOUBLE), DIMENSION(:,:), ALLOCATABLE, TARGET :: det_corr_all
        TYPE (xmi_solid_angle), TARGET :: solid_angles
        INTEGER (C_LONG) :: detector_solid_angle_not_found
        REAL (C_DOUBLE), DIMENSION(:), ALLOCATABLE :: theta_i_s, phi_i_s 
        INTEGER (C_INT), DIMENSION(:), ALLOCATABLE :: theta_i_hist, phi_i_hist
        !begin...
        REAL(C_DOUBLE) :: dirv_z_angle
        INTEGER, PARAMETER :: maxz = 94
        INTEGER (C_INT64_T) :: n_photons_tot, n_photons_sim

        TYPE (xmi_precalc_mu_cs), DIMENSION(:), ALLOCATABLE, TARGET ::&
        precalc_mu_cs
        INTEGER (4) :: time_before, time_after
        REAL (C_DOUBLE) :: weight

        TYPE (xmi_ran_trap_workspace) :: workspace

        
        CALL SetErrorMessages(0)



        rv = 0
        photons_simulated = 0
        detector_hits = 0
        detector_hits2 = 0
        rayleighs = 0
        comptons = 0
        einsteins = 0
        detector_solid_angle_not_found = 0



        !set the XRF cross sections according to the options
        IF (options%use_cascade_auger .EQ. 0 .AND.&
        options%use_cascade_radiative .EQ.0 ) THEN
                xmi_cascade_type = XMI_CASCADE_NONE
        ELSEIF (options%use_cascade_auger .EQ. 1 .AND.&
        options%use_cascade_radiative .EQ.0 ) THEN
                xmi_cascade_type = XMI_CASCADE_NONRADIATIVE
        ELSEIF (options%use_cascade_auger .EQ. 0 .AND.&
        options%use_cascade_radiative .EQ.1 ) THEN
                xmi_cascade_type = XMI_CASCADE_RADIATIVE
        ELSEIF (options%use_cascade_auger .EQ. 1 .AND.&
        options%use_cascade_radiative .EQ.1 ) THEN
                xmi_cascade_type = XMI_CASCADE_FULL
        ENDIF



        CALL C_F_POINTER(inputFPtr, inputF)
        CALL C_F_POINTER(hdf5FPtr, hdf5F) 
       
        IF (options%use_variance_reduction .EQ. 1) THEN
                !ALLOCATE(solid_angles)
                solid_angles%grid_dims_r_n = solid_anglesCPtr%grid_dims_r_n
                solid_angles%grid_dims_theta_n =&
                solid_anglesCPtr%grid_dims_theta_n
                CALL C_F_POINTER(solid_anglesCPtr%grid_dims_r_vals, &
                solid_angles%grid_dims_r_vals,[solid_angles%grid_dims_r_n])
                CALL C_F_POINTER(solid_anglesCPtr%grid_dims_theta_vals, &
                solid_angles%grid_dims_theta_vals,[solid_angles%grid_dims_theta_n])
                CALL C_F_POINTER(solid_anglesCPtr%solid_angles, &
                solid_angles%solid_angles,&
                [solid_angles%grid_dims_r_n,solid_angles%grid_dims_theta_n])

#if DEBUG == 1
                WRITE (6,'(A,I6)') 'solid_angles%grid_dims_r_n: ',solid_angles%grid_dims_r_n
                WRITE (6,'(A,I6)') 'solid_angles%grid_dims_theta_n: ',solid_angles%grid_dims_theta_n
#endif

        ENDIF

        max_threads = options%omp_num_threads

        ALLOCATE(seeds(max_threads))

        !fetch some seeds
        IF (xmi_get_random_numbers(C_LOC(seeds), INT(max_threads,KIND=C_LONG)) == 0) RETURN



        !
        !
        !       Precalculate the absorption coefficients of the XRF photons
        !
        !
        ALLOCATE(precalc_mu_cs(inputF%composition%n_layers))
        DO k=1,inputF%composition%n_layers
                ALLOCATE(precalc_mu_cs(k)%mu(maxz,ABS(M5P5_LINE)))
                DO l=1,maxz
                        DO m=KL1_LINE,M5P5_LINE,-1
                               precalc_mu_cs(k)%mu(l,ABS(m))=xmi_mu_calc(inputF%composition%layers(k),&
                               REAL(LineEnergy(INT(l,C_INT),INT(m,C_INT)),C_DOUBLE)) 
                        ENDDO
                ENDDO

        ENDDO

        ALLOCATE(brute_history(100,383+2,inputF%general%n_interactions_trajectory)) 
        brute_history = 0.0_C_DOUBLE
        last_shell = 0_C_INT

        ALLOCATE(channels(0:inputF%general%n_interactions_trajectory,0:nchannels-1))
        channels = 0.0_C_DOUBLE

        ALLOCATE(det_corr_all(100,383+2))

        det_corr_all = 1.0_C_DOUBLE

        DO i=1,SIZE(hdf5F%xmi_hdf5_Zs)
                DO line=KL1_LINE, P3P5_LINE, -1
                        det_corr = 1.0_C_DOUBLE
                        DO j=1,inputF%absorbers%n_det_layers
                                det_corr = det_corr * EXP(-1.0_C_DOUBLE*&
                                inputF%absorbers%det_layers(j)%density*&
                                inputF%absorbers%det_layers(j)%thickness*&
                                xmi_mu_calc(inputF%absorbers%det_layers(j),&
                                REAL(LineEnergy(hdf5F%xmi_hdf5_Zs(i)%Z,line),KIND=C_DOUBLE))) 
                        ENDDO
                        DO j=1,inputF%detector%n_crystal_layers
                                det_corr = det_corr * (1.0_C_DOUBLE-EXP(-1.0_C_DOUBLE*&
                                inputF%detector%crystal_layers(j)%density*&
                                inputF%detector%crystal_layers(j)%thickness*&
                                xmi_mu_calc(inputF%detector%crystal_layers(j),&
                                REAL(LineEnergy(hdf5F%xmi_hdf5_Zs(i)%Z,line),KIND=C_DOUBLE))))
                        ENDDO
                        det_corr_all(hdf5F%xmi_hdf5_Zs(i)%Z,ABS(line))=det_corr
                ENDDO
        ENDDO



        !because it's unsure how openmp reduction will work with unallocated
        !variables... just allocate it anyway..
        !IF (options%use_variance_reduction .EQ. 1_C_INT) THEN
                ALLOCATE(var_red_history(100,383+2,inputF%general%n_interactions_trajectory)) 
                var_red_history = 0.0_C_DOUBLE
        !ENDIF

        rng_type = fgsl_rng_mt19937
        n_photons_sim = 0_C_LONG
        n_photons_tot = inputF%excitation%n_discrete*inputF%general%n_photons_line +&
                        inputF%excitation%n_continuous*inputF%general%n_photons_interval
        n_photons_tot = n_photons_tot/options%omp_num_threads

        !time_before = TIME()

!$omp parallel default(shared) private(rng,thread_num,i,j,k,l,m,n,photon,&
!$omp photon_temp,photon_temp2,hor_ver_ratio,n_photons,iv_start_energy,&
!$omp iv_end_energy,ipol,cosalfa, c_alfa, c_ae, c_be, initial_mus,channel,&
!$omp element,exc_corr,det_corr,total_intensity,dirv_z_angle,weight, workspace,&
!$omp iv_start_intensity, iv_end_intensity)&
!$omp reduction(+:photons_simulated,detector_hits, detector_hits2,channels,&
!$omp rayleighs,comptons,einsteins,brute_history, last_shell, var_red_history,&
!$omp detector_solid_angle_not_found) &
!$omp num_threads(options%omp_num_threads)

!
!
!       Initialize random number generator
!
!
        thread_num = omp_get_thread_num()

        rng = fgsl_rng_alloc(rng_type)
        CALL fgsl_rng_set(rng,seeds(thread_num+1))
        ALLOCATE(initial_mus(inputF%composition%n_layers))

!
!
!       Start with continuous energies
!
!
!        ASSOCIATE (exc => inputF%excitation)
!
!       Note : Intel Fortran 11.1 and 12.0 seem to have a serious problem with ASSOCIATE
!       constructs when compiling with openMP
!
!



#define exc inputF%excitation
        cont:DO i=1,exc%n_continuous-1
                n_photons = inputF%general%n_photons_interval/omp_get_num_threads()/n_mpi_hosts
                !total_intensity=exc%continuous(i)%vertical_intensity+ &
                !exc%continuous(i)%horizontal_intensity
                !hor_ver_ratio = &
                !exc%continuous(i)%horizontal_intensity*n_photons/ &
                !total_intensity
                !Calculate the initial energy -> interval boundaries
                iv_start_energy = exc%continuous(i)%energy
                iv_end_energy = exc%continuous(i+1)%energy
                iv_start_intensity = exc%continuous(i)%vertical_intensity+ &
                exc%continuous(i)%horizontal_intensity
                iv_end_intensity = exc%continuous(i+1)%vertical_intensity+ &
                exc%continuous(i+1)%horizontal_intensity

                total_intensity = (iv_start_intensity+iv_end_intensity)* &
                (iv_end_energy-iv_start_energy)/2.0_C_DOUBLE

                IF (xmi_ran_trap_workspace_init(iv_start_energy, iv_end_energy,&
                iv_start_intensity, iv_end_intensity,workspace) == 0_C_INT) THEN
                        CALL EXIT(1)
                ENDIF

                photons_cont:DO j=1,n_photons
                        !Allocate the photon
                        ALLOCATE(photon)
                        ALLOCATE(photon%history(inputF%general%n_interactions_trajectory,2))
                        IF (options%use_variance_reduction .EQ. 1) THEN
                                photon%solid_angle => solid_angles 
                                photon%detector_solid_angle_not_found = 0
                                photon%var_red_history => var_red_history
                                photon%channels => channels
                        ENDIF
                        photon%history(1,1)=NO_INTERACTION
                        photon%n_interactions=0
                        NULLIFY(photon%offspring)
                        
                        !Calculate energy with rng -> sample from trapezoidal distribution
                        photon%energy = &
                        xmi_ran_trap(rng,workspace)

                        hor_ver_ratio = interpolate_simple([iv_start_energy,&
                        exc%continuous(i)%horizontal_intensity],[iv_end_energy,&
                        exc%continuous(i+1)%horizontal_intensity],photon%energy)/&
                        interpolate_simple([iv_start_energy,&
                        iv_start_intensity],[iv_end_energy,&
                        iv_end_intensity],photon%energy)

                        exc_corr = 1.0_C_DOUBLE
                        DO k=1,inputF%absorbers%n_exc_layers
                                exc_corr = exc_corr * EXP(-1.0_C_DOUBLE*&
                                inputF%absorbers%exc_layers(k)%density*&
                                inputF%absorbers%exc_layers(k)%thickness*&
                                xmi_mu_calc(inputF%absorbers%exc_layers(k),photon%energy))
                        ENDDO

                        !Calculate initial mu's
                        initial_mus = xmi_mu_calc(inputF%composition,&
                        photon%energy)

                        weight = (total_intensity)*exc_corr/inputF%general%n_photons_interval
                        !/(iv_end_energy-iv_energy)
                        photon%energy_changed=.FALSE.
                        ALLOCATE(photon%mus(inputF%composition%n_layers))
                        photon%mus = initial_mus
                        photon%detector_hit = .FALSE.
                        photon%detector_hit2 = .FALSE.
                        photon%options = options
                        photon%xmi_cascade_type = xmi_cascade_type
                        photon%precalc_mu_cs => precalc_mu_cs
                        photon%det_corr_all => det_corr_all


                        !Calculate its initial coordinates and direction
                        CALL xmi_coords_dir(rng,exc%continuous(i), inputF%geometry,&
                        photon)

                        !Calculate its weight and electric field...
                        IF (fgsl_rng_uniform(rng) .LE. hor_ver_ratio) THEN
                                !horizontal
                                photon%weight = weight 
                                photon%elecv(1) = 0.0_C_DOUBLE
                                photon%elecv(2) = 1.0_C_DOUBLE
                                photon%elecv(3) = 0.0_C_DOUBLE
#if DEBUG == 1
                                WRITE (*,'(A)') 'horizontal'
#endif
                        ELSE
                                !vertical
                                photon%weight = weight 
                                photon%elecv(1) = 1.0_C_DOUBLE
                                photon%elecv(2) = 0.0_C_DOUBLE
                                photon%elecv(3) = 0.0_C_DOUBLE
#if DEBUG == 1
                                WRITE (*,'(A)') 'vertical'
#endif
                        ENDIF




                        cosalfa = DOT_PRODUCT(photon%elecv, photon%dirv)

                        IF (ABS(cosalfa) .GT. 1.0) THEN
                                WRITE (error_unit,'(A)') 'cosalfa exception detected'
                                CALL EXIT(1)
                        ENDIF

                        c_alfa = ACOS(cosalfa)
                        c_ae = 1.0/SIN(c_alfa)
                        c_be = -c_ae*cosalfa

                        photon%elecv = c_ae*photon%elecv + c_be*photon%dirv

                        CALL &
                        xmi_photon_shift_first_layer(photon,inputF%composition,inputF%geometry)


                        IF (xmi_simulate_photon(photon, inputF, hdf5F,rng) == 0) THEN
                                CALL EXIT(1)
                        ENDIF

                        photon_temp => photon
                        photon_eval_cont:DO 

                                photons_simulated = photons_simulated + 1

                                det_hit_cont:IF (photon_temp%detector_hit .EQV. .TRUE.) THEN
                                        detector_hits = detector_hits + 1
!
!
!                                       Add to channelsF
!
!
                                        IF (options%use_variance_reduction .EQ. 0) THEN
#if DEBUG == 1
                                        IF (INT(photon_temp%phi_i*999.0_C_DOUBLE/M_PI/2.0)+1 .GT. 1000 .OR.&
                                        INT(photon_temp%phi_i*999.0_C_DOUBLE/M_PI/2.0)+1&
                                        .LT. 1) CALL EXIT(1)
                                        IF (photon_temp%last_interaction ==&
                                        RAYLEIGH_INTERACTION .OR. &
                                        photon_temp%last_interaction ==&
                                        COMPTON_INTERACTION) THEN
                                        theta_i_hist(INT(photon_temp%theta_i*999.0_C_DOUBLE/M_PI)+1)=&
                                        theta_i_hist(INT(photon_temp%theta_i*999.0_C_DOUBLE/M_PI)+1)+1
                                        phi_i_hist(INT(photon_temp%phi_i*999.0_C_DOUBLE/M_PI/2.0_C_DOUBLE)+1)=&
                                        phi_i_hist(INT(photon_temp%phi_i*999.0_C_DOUBLE/M_PI/2.0_C_DOUBLE)+1)+1
                                        ENDIF
#endif
                                        IF (photon_temp%energy .GE. energy_threshold) THEN
                                                channel = INT((photon_temp%energy - &
                                                inputF%detector%zero)/inputF%detector%gain)
                                        ELSE
                                                channel = -1
                                        ENDIF

                                        IF (channel .GE. 0 .AND. channel .LT. nchannels) THEN
#if DEBUG == 1
!$omp critical                        
                                        WRITE (*,'(A,I)') 'channel:'&
                                        ,channel
                                        WRITE (*,'(A,ES14.4)') &
                                        'photon_temp%weight:',photon_temp%weight
!$omp end critical
#endif
                                                channels(photon_temp%n_interactions:, channel) =&
                                                channels(photon_temp%n_interactions:, channel)+photon_temp%weight
                                        ENDIF
                                        ENDIF
                                        SELECT CASE (photon_temp%last_interaction)
                                                CASE (RAYLEIGH_INTERACTION)
                                                        rayleighs = rayleighs + 1
                                                CASE (COMPTON_INTERACTION)
                                                        comptons = comptons + 1
                                                CASE (PHOTOELECTRIC_INTERACTION)
                                                        einsteins = einsteins + 1
                                        ENDSELECT
                                        !update history -> record only the last
                                        !interaction
                                        IF (photon_temp%n_interactions .GT. 0)&
                                        THEN
                                                k=photon_temp%n_interactions
                                                element =&
                                                photon_temp%history(k,2)
                                                IF (photon_temp%history(k,1) .LE. KL1_LINE .AND.&
                                                photon_temp%history(k,1) .GE. P3P5_LINE) THEN
                                                        !fluorescence
                                                        brute_history(element,&
                                                        ABS(photon_temp%history(k,1)),k) = &
                                                        brute_history(element,&
                                                        ABS(photon_temp%history(k,1)),k) + &
                                                        photon_temp%weight*det_corr_all&
                                                        (element,ABS(photon_temp%history(k,1)))
                                                         
                                                ELSEIF &
                                                        (photon_temp%history(k,1) .EQ. RAYLEIGH_INTERACTION) THEN
                                                        !rayleigh
                                                        brute_history(element,383+1,k) = &
                                                        brute_history(element,383+1,k) + &
                                                        photon_temp%weight
                                                ELSEIF &
                                                (photon_temp%history(k,1) .EQ. COMPTON_INTERACTION) THEN
                                                        !compton
                                                        brute_history(element,383+2,k) = &
                                                        brute_history(element,383+2,k) + &
                                                        photon_temp%weight
                                                ENDIF
                                                !ENDDO
#if DEBUG == 1
                                                IF (photon_temp%last_interaction .EQ. PHOTOELECTRIC_INTERACTION) THEN
                                                last_shell(photon_temp%last_shell) =&
                                                last_shell(photon_temp%last_shell)+1
                                                ENDIF
#endif
                                        ENDIF

                                ENDIF det_hit_cont
                                IF (photon_temp%detector_hit2 .EQV. .TRUE.) THEN
                                        detector_hits2 = detector_hits2 + 1
                                ENDIF

                                IF (ASSOCIATED(photon_temp%offspring)) THEN
                                        photon_temp2 => photon_temp%offspring
                                        IF (.NOT. ASSOCIATED(photon_temp2)) THEN
                                                WRITE (error_unit,'(A)') 'This line should not appear'
                                                CALL EXIT(1)
                                        ENDIF
                                ELSE
                                        NULLIFY(photon_temp2)
                                ENDIF
                               
                                IF (options%use_variance_reduction&
                                .EQ. 1) THEN
                                        detector_solid_angle_not_found =&
                                        photon%detector_solid_angle_not_found+&
                                        detector_solid_angle_not_found
                                ENDIF
                                DEALLOCATE(photon_temp%history)
                                DEALLOCATE(photon_temp%mus)
                                DEALLOCATE(photon_temp)
                                IF (ASSOCIATED(photon_temp2)) THEN
                                        photon_temp => photon_temp2
                                ELSE
                                        EXIT photon_eval_cont
                                ENDIF

                        ENDDO photon_eval_cont

                        IF (omp_get_thread_num() == 0) THEN
                          n_photons_sim = n_photons_sim+1_C_INT64_T
                          IF(n_photons_sim*100_C_INT64_T/n_photons_tot == &
                          REAL(n_photons_sim*100_C_INT64_T,C_DOUBLE)/REAL(n_photons_tot,C_DOUBLE).AND.&
                          options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
                          CALL xmi_print_progress('Simulating interactions at'//C_NULL_CHAR,&
                          INT(n_photons_sim*100_C_INT_64_T/n_photons_tot,KIND=C_INT))
#else
                          WRITE(output_unit,'(A,I3,A)')&
                          'Simulating interactions at ',n_photons_sim*100_C_INT64_T&
                          /n_photons_tot,' %'
#endif
                        ENDIF
                ENDDO photons_cont
        ENDDO cont

        disc:DO i=1,exc%n_discrete
                n_photons = inputF%general%n_photons_line/omp_get_num_threads()/n_mpi_hosts
                total_intensity=exc%discrete(i)%vertical_intensity+ &
                exc%discrete(i)%horizontal_intensity
                hor_ver_ratio = &
                exc%discrete(i)%horizontal_intensity*n_photons/ &
                total_intensity
                !take into account the excitation absorber
                exc_corr = 1.0_C_DOUBLE
                DO j=1,inputF%absorbers%n_exc_layers
                        exc_corr = exc_corr * EXP(-1.0_C_DOUBLE*&
                        inputF%absorbers%exc_layers(j)%density*&
                        inputF%absorbers%exc_layers(j)%thickness*&
                        xmi_mu_calc(inputF%absorbers%exc_layers(j),exc%discrete(i)%energy))
                ENDDO

                !Calculate initial mu's
                IF (exc%discrete(i)%distribution_type .EQ.&
                XMI_DISCRETE_MONOCHROMATIC) THEN
                        initial_mus = xmi_mu_calc(inputF%composition,&
                        exc%discrete(i)%energy)
                ENDIF

                weight = (total_intensity)*exc_corr/inputF%general%n_photons_line

#if DEBUG == 1
!$omp critical
                IF (exc%discrete(i)%distribution_type .EQ.&
                        XMI_DISCRETE_GAUSSIAN) THEN
                WRITE (output_unit, '(A)') 'gaussian'
                ELSEIF (exc%discrete(i)%distribution_type .EQ.&
                        XMI_DISCRETE_LORENTZIAN) THEN
                WRITE (output_unit, '(A)') 'lorentzian'
                ELSE
                WRITE (output_unit, '(A)') 'monochromatic'
                ENDIF
!$omp end critical
#endif


                photons:DO j=1,n_photons
                        !Allocate the photon
                        ALLOCATE(photon)
                        ALLOCATE(photon%history(inputF%general%n_interactions_trajectory,2))
                        IF (options%use_variance_reduction .EQ. 1) THEN
                                photon%solid_angle => solid_angles 
                                photon%detector_solid_angle_not_found = 0
                                photon%var_red_history => var_red_history
                                photon%channels => channels
                        ENDIF
                        photon%history(1,1)=NO_INTERACTION
                        photon%n_interactions=0
                        NULLIFY(photon%offspring)
                        photon%energy_changed=.FALSE.
                        ALLOCATE(photon%mus(inputF%composition%n_layers))
                        IF (exc%discrete(i)%distribution_type .EQ.&
                        XMI_DISCRETE_GAUSSIAN) THEN
                                !gaussian distribution
                                photon%energy = fgsl_ran_gaussian_ziggurat(rng,&
                                exc%discrete(i)%scale_parameter)+exc%discrete(i)%energy
                                IF (photon%energy .LE. energy_threshold) CYCLE &
                                photons 
                                photon%mus = xmi_mu_calc(inputF%composition,&
                                photon%energy)
                        ELSEIF (exc%discrete(i)%distribution_type .EQ.&
                        XMI_DISCRETE_LORENTZIAN) THEN
                                !lorentzian distribution
                                photon%energy = fgsl_ran_cauchy(rng,&
                                exc%discrete(i)%scale_parameter)+exc%discrete(i)%energy
                                IF (photon%energy .LE. energy_threshold) CYCLE &
                                photons 
                                photon%mus = xmi_mu_calc(inputF%composition,&
                                photon%energy)
                        ELSE
                                !monochromatic case
                                photon%energy = exc%discrete(i)%energy 
                                photon%mus = initial_mus
                        ENDIF
#if DEBUG == 1
!$omp critical
                        IF (photon%energy .GT. exc%discrete(i)%energy) &
                        WRITE (output_unit, '(A,ES12.6)') 'energy:',&
                        photon%energy
!$omp end critical
#endif
                        photon%detector_hit = .FALSE.
                        photon%detector_hit2 = .FALSE.
                        photon%options = options
                        photon%xmi_cascade_type = xmi_cascade_type
                        photon%precalc_mu_cs => precalc_mu_cs
                        photon%det_corr_all => det_corr_all



                        !Calculate its initial coordinates and direction
                        CALL xmi_coords_dir(rng,exc%discrete(i), inputF%geometry,&
                        photon)



                        !Calculate its weight and electric field...
                        IF (j .LE. hor_ver_ratio) THEN
                                !horizontal
                                photon%weight = weight 
                                photon%elecv(1) = 0.0_C_DOUBLE
                                photon%elecv(2) = 1.0_C_DOUBLE
                                photon%elecv(3) = 0.0_C_DOUBLE
#if DEBUG == 1
                                WRITE (*,'(A)') 'horizontal'
#endif
                        ELSE
                                !vertical
                                photon%weight = weight 
                                photon%elecv(1) = 1.0_C_DOUBLE
                                photon%elecv(2) = 0.0_C_DOUBLE
                                photon%elecv(3) = 0.0_C_DOUBLE
#if DEBUG == 1
                                WRITE (*,'(A)') 'vertical'
#endif
                        ENDIF




                        cosalfa = DOT_PRODUCT(photon%elecv, photon%dirv)

                        IF (ABS(cosalfa) .GT. 1.0) THEN
                                WRITE (error_unit,'(A)') 'cosalfa exception detected'
                                CALL EXIT(1)
                        ENDIF

                        c_alfa = ACOS(cosalfa)
                        c_ae = 1.0/SIN(c_alfa)
                        c_be = -c_ae*cosalfa

                        photon%elecv = c_ae*photon%elecv + c_be*photon%dirv

                        CALL &
                        xmi_photon_shift_first_layer(photon,inputF%composition,inputF%geometry)


                        IF (xmi_simulate_photon(photon, inputF, hdf5F,rng) == 0) THEN
                                CALL EXIT(1)
                        ENDIF

                        photon_temp => photon
                        photon_eval_disc:DO 

                                photons_simulated = photons_simulated + 1

                                det_hit_disc:IF (photon_temp%detector_hit .EQV. .TRUE.) THEN
                                        detector_hits = detector_hits + 1
!
!
!                                       Add to channelsF
!
!
                                        IF (options%use_variance_reduction .EQ. 0) THEN
#if DEBUG == 1
                                        IF (INT(photon_temp%phi_i*999.0_C_DOUBLE/M_PI/2.0)+1 .GT. 1000 .OR.&
                                        INT(photon_temp%phi_i*999.0_C_DOUBLE/M_PI/2.0)+1&
                                        .LT. 1) CALL EXIT(1)
                                        IF (photon_temp%last_interaction ==&
                                        RAYLEIGH_INTERACTION .OR. &
                                        photon_temp%last_interaction ==&
                                        COMPTON_INTERACTION) THEN
                                        theta_i_hist(INT(photon_temp%theta_i*999.0_C_DOUBLE/M_PI)+1)=&
                                        theta_i_hist(INT(photon_temp%theta_i*999.0_C_DOUBLE/M_PI)+1)+1
                                        phi_i_hist(INT(photon_temp%phi_i*999.0_C_DOUBLE/M_PI/2.0_C_DOUBLE)+1)=&
                                        phi_i_hist(INT(photon_temp%phi_i*999.0_C_DOUBLE/M_PI/2.0_C_DOUBLE)+1)+1
                                        ENDIF
#endif
                                        IF (photon_temp%energy .GE. energy_threshold) THEN
                                                channel = INT((photon_temp%energy - &
                                                inputF%detector%zero)/inputF%detector%gain)
                                        ELSE
                                                channel = -1
                                        ENDIF

                                        IF (channel .GE. 0 .AND. channel .LT. nchannels) THEN
#if DEBUG == 1
!$omp critical                        
                                        WRITE (*,'(A,I)') 'channel:'&
                                        ,channel
                                        WRITE (*,'(A,ES14.4)') &
                                        'photon_temp%weight:',photon_temp%weight
!$omp end critical
#endif
                                                channels(photon_temp%n_interactions:, channel) =&
                                                channels(photon_temp%n_interactions:, channel)+photon_temp%weight
                                        ENDIF
                                        ENDIF
                                        SELECT CASE (photon_temp%last_interaction)
                                                CASE (RAYLEIGH_INTERACTION)
                                                        rayleighs = rayleighs + 1
                                                CASE (COMPTON_INTERACTION)
                                                        comptons = comptons + 1
                                                CASE (PHOTOELECTRIC_INTERACTION)
                                                        einsteins = einsteins + 1
                                        ENDSELECT
                                        !update history -> record only the last
                                        !interaction
                                        IF (photon_temp%n_interactions .GT. 0)&
                                        THEN
                                                k=photon_temp%n_interactions
                                                element =&
                                                photon_temp%history(k,2)
                                                IF (photon_temp%history(k,1) .LE. KL1_LINE .AND.&
                                                photon_temp%history(k,1) .GE. P3P5_LINE) THEN
                                                        !fluorescence
                                                        brute_history(element,&
                                                        ABS(photon_temp%history(k,1)),k) = &
                                                        brute_history(element,&
                                                        ABS(photon_temp%history(k,1)),k) + &
                                                        photon_temp%weight*det_corr_all&
                                                        (element,ABS(photon_temp%history(k,1)))
                                                         
                                                ELSEIF &
                                                        (photon_temp%history(k,1) .EQ. RAYLEIGH_INTERACTION) THEN
                                                        !rayleigh
                                                        brute_history(element,383+1,k) = &
                                                        brute_history(element,383+1,k) + &
                                                        photon_temp%weight
                                                ELSEIF &
                                                (photon_temp%history(k,1) .EQ. COMPTON_INTERACTION) THEN
                                                        !compton
                                                        brute_history(element,383+2,k) = &
                                                        brute_history(element,383+2,k) + &
                                                        photon_temp%weight
                                                ENDIF
                                                !ENDDO
#if DEBUG == 1
                                                IF (photon_temp%last_interaction .EQ. PHOTOELECTRIC_INTERACTION) THEN
                                                last_shell(photon_temp%last_shell) =&
                                                last_shell(photon_temp%last_shell)+1
                                                ENDIF
#endif
                                        ENDIF

                                ENDIF det_hit_disc
                                IF (photon_temp%detector_hit2 .EQV. .TRUE.) THEN
                                        detector_hits2 = detector_hits2 + 1
                                ENDIF

                                IF (ASSOCIATED(photon_temp%offspring)) THEN
                                        photon_temp2 => photon_temp%offspring
                                        IF (.NOT. ASSOCIATED(photon_temp2)) THEN
                                                WRITE (error_unit,'(A)') 'This line should not appear'
                                                CALL EXIT(1)
                                        ENDIF
                                ELSE
                                        NULLIFY(photon_temp2)
                                ENDIF
                               
                                IF (options%use_variance_reduction&
                                .EQ. 1) THEN
                                        detector_solid_angle_not_found =&
                                        photon%detector_solid_angle_not_found+&
                                        detector_solid_angle_not_found
                                ENDIF
                                DEALLOCATE(photon_temp%history)
                                DEALLOCATE(photon_temp%mus)
                                DEALLOCATE(photon_temp)
                                IF (ASSOCIATED(photon_temp2)) THEN
                                        photon_temp => photon_temp2
                                ELSE
                                        EXIT photon_eval_disc
                                ENDIF

                        ENDDO photon_eval_disc

                        IF (omp_get_thread_num() == 0) THEN
                          n_photons_sim = n_photons_sim+1_C_INT64_T
                          IF(n_photons_sim*100_C_INT64_T/n_photons_tot == &
                          REAL(n_photons_sim*100_C_INT64_T,C_DOUBLE)/REAL(n_photons_tot,C_DOUBLE).AND.&
                          options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
                          CALL xmi_print_progress('Simulating interactions at'//C_NULL_CHAR,&
                          INT(n_photons_sim*100_C_INT_64_T/n_photons_tot,KIND=C_INT))
#else
                          WRITE(output_unit,'(A,I3,A)')&
                          'Simulating interactions at ',n_photons_sim*100_C_INT64_T&
                          /n_photons_tot,' %'
#endif
                        ENDIF
                ENDDO photons
        ENDDO disc 

#undef exc
 !       ENDASSOCIATE


        !cleanup
        CALL fgsl_rng_free(rng)

!$omp end parallel



        !time_after = TIME()
        !WRITE (output_unit, '(A,I8, A)') 'Time elapsed: ',&
        !time_after - time_before, ' sec'

#if DEBUG == 1
        WRITE (*,'(A,I10)') 'Photons simulated: ',photons_simulated
        WRITE (*,'(A,I10)') 'Photons hitting the detector...: ',detector_hits
        WRITE (*,'(A,I10)') 'Photons hitting the detector2...: ',detector_hits2
        WRITE (*,'(A,I10)') 'Rayleighs: ',rayleighs
        WRITE (*,'(A,I10)') 'Comptons: ',comptons
        WRITE (*,'(A,I10)') 'Photoelectric: ',einsteins
        WRITE (*,'(A,I10)') 'detector_solid_angle_not_found: ',&
        detector_solid_angle_not_found
!        WRITE (*,'(A)') 'Brute force'
!        WRITE (*,'(A,I)') 'Ba-KL3: ',brute_history(56,ABS(KL3_LINE),1)
!        WRITE (*,'(A,I)') 'Ba-KL3-4: ',brute_history(56,ABS(KL3_LINE),4)
!        WRITE (*,'(A,I)') 'Ba-LA1: ',brute_history(56,ABS(LA1_LINE),1)
!        WRITE (*,'(A,I)') 'Ba-LA1-4: ',brute_history(56,ABS(LA1_LINE),4)
!        WRITE (*,'(A)') 'variance reduction'
!        WRITE (*,'(A,ES14.5)') 'Cr-KL2: ',var_red_history(24,ABS(KL2_LINE),1)
!        WRITE (*,'(A,ES14.5)') 'Cr-Rayleight: ',var_red_history(24,383+1,1)
!        WRITE (*,'(A,ES14.5)') 'Ba-KL3: ',var_red_history(56,ABS(KL3_LINE),1)
!        WRITE (*,'(A,ES14.5)') 'Ba-KL3-4: ',var_red_history(56,ABS(KL3_LINE),4)
!        WRITE (*,'(A,ES14.5)') 'Ba-LA1: ',var_red_history(56,ABS(LA1_LINE),1)
!        WRITE (*,'(A,ES14.5)') 'Ba-LA1-4: ',var_red_history(56,ABS(LA1_LINE),4)
#endif


        DO k=1,inputF%composition%n_layers
                DEALLOCATE(precalc_mu_cs(k)%mu)
        ENDDO
        DEALLOCATE(precalc_mu_cs)

        !multiply with detector absorbers and detector crystal
        DO i=0,nchannels-1
                det_corr = 1.0_C_DOUBLE
                DO j=1,inputF%absorbers%n_det_layers
                        det_corr = det_corr * EXP(-1.0_C_DOUBLE*&
                        inputF%absorbers%det_layers(j)%density*&
                        inputF%absorbers%det_layers(j)%thickness*&
                        xmi_mu_calc(inputF%absorbers%det_layers(j),&
                        i*inputF%detector%gain+inputF%detector%zero)) 
                ENDDO
                DO j=1,inputF%detector%n_crystal_layers
                        det_corr = det_corr * (1.0_C_DOUBLE-EXP(-1.0_C_DOUBLE*&
                        inputF%detector%crystal_layers(j)%density*&
                        inputF%detector%crystal_layers(j)%thickness*&
                        xmi_mu_calc(inputF%detector%crystal_layers(j),&
                        i*inputF%detector%gain+inputF%detector%zero)) )
                ENDDO
                channels(:,i) = channels(:,i)*det_corr
        ENDDO
        
#if DEBUG == 1
        OPEN(UNIT=500,FILE='rayleigh_theta_hist.txt',STATUS='replace',ACTION='write')
        WRITE (500,'(I5)') 1000
        DO i=1,1000
                WRITE (500,'(ES14.5,I12)') theta_i_s(i),theta_i_hist(i)
        ENDDO
        CLOSE(UNIT=500)

        OPEN(UNIT=500,FILE='rayleigh_phi_hist.txt',STATUS='replace',ACTION='write')
        WRITE (500,'(I5)') 1000
        DO i=1,1000
                WRITE (500,'(ES14.5,I12)') phi_i_s(i),phi_i_hist(i)
        ENDDO
        CLOSE(UNIT=500)
#endif


        
        !now we can access the history in C using the same indices...
        ALLOCATE(brute_historyF(inputF%general%n_interactions_trajectory,(383+2),100))
        brute_historyF = RESHAPE(brute_history,[inputF%general%n_interactions_trajectory,(383+2),100],ORDER=[3,2,1])

        !multiply with live time
        brute_historyF = brute_historyF*inputF%detector%live_time*n_mpi_hosts

        brute_historyPtr = C_LOC(brute_historyF(1,1,1))


#if DEBUG == 1
        WRITE (6,'(A,ES14.5)') 'zero_sum:' ,SUM(channels(0,:))
#endif
       
        IF (options%use_variance_reduction .EQ. 1_C_INT) THEN
                ALLOCATE(var_red_historyF(inputF%general%n_interactions_trajectory,(383+2),100))
                var_red_historyF = RESHAPE(var_red_history,[inputF%general%n_interactions_trajectory,(383+2),100],ORDER=[3,2,1])
                !multiply with live time
                var_red_historyF =&
                var_red_historyF*inputF%detector%live_time*n_mpi_hosts

                var_red_historyPtr = C_LOC(var_red_historyF(1,1,1))
        ELSE
                var_red_historyPtr = C_NULL_PTR
        ENDIF



        ALLOCATE(channelsF(0:nchannels-1,0:inputF%general%n_interactions_trajectory))
        channelsF = RESHAPE(channels, [nchannels,inputF%general%n_interactions_trajectory+1],ORDER=[2,1])
        !multiply with live time
        channelsF = channelsF*inputF%detector%live_time

        channelsPtr = C_LOC(channelsF(0,0))

        rv = 1

ENDFUNCTION xmi_main_msim


SUBROUTINE xmi_coords_dir_disc(rng, energy, geometry, photon) 
        IMPLICIT NONE
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_energy_discrete), INTENT(IN) :: energy
        TYPE (xmi_geometry), INTENT(IN) :: geometry
        TYPE (xmi_photon), INTENT(INOUT) :: photon

        REAL (C_DOUBLE) :: x1, y1

#if DEBUG == 2
        WRITE (*,*) 'Entering xmi_coords_dir'
        WRITE (*,*) 'energy: ',energy
#endif



        !Determine whether it's a point or Gaussian source...

        IF (ABS(energy%sigma_x*energy%sigma_y) .LT. 1.0E-20) THEN
                !point source
                CALL xmi_coords_point(rng, geometry, photon, x1, y1)
        ELSE
                !gaussian source
                CALL xmi_coords_gaussian(rng, energy, geometry, photon, x1, y1)
        ENDIF

        photon%dirv(1) = SIN(x1) 
        photon%dirv(2) = SIN(y1) 
        photon%dirv(3) = SQRT(1.0_C_DOUBLE - photon%dirv(1)**2-photon%dirv(2)**2) 

#if DEBUG == 2
        WRITE (*,*) 'dirv: ', photon%dirv
#endif


        photon%theta = ACOS(photon%dirv(3))

        IF (photon%dirv(1) .EQ. 0.0_C_DOUBLE) THEN
                !watch out... if photon%dirv(2) EQ 0.0 then result may be
                !processor dependent...
                photon%phi = SIGN(M_PI_2, photon%dirv(2))
        ELSE
#if DEBUG == 2
                WRITE (*,'(A)') 'Dont think we should get here'
#endif
                photon%phi = ATAN(photon%dirv(2)/photon%dirv(1))
        ENDIF
        
!        photon%phi = ATAN2(photon%dirv(2),photon%dirv(1))
        
        RETURN

ENDSUBROUTINE xmi_coords_dir_disc

SUBROUTINE xmi_coords_dir_cont(rng, energy, geometry, photon) 
        IMPLICIT NONE
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_energy_continuous), INTENT(IN) :: energy
        TYPE (xmi_geometry), INTENT(IN) :: geometry
        TYPE (xmi_photon), INTENT(INOUT) :: photon

        REAL (C_DOUBLE) :: x1, y1

#if DEBUG == 2
        WRITE (*,*) 'Entering xmi_coords_dir'
        WRITE (*,*) 'energy: ',energy
#endif



        !Determine whether it's a point or Gaussian source...

        IF (ABS(energy%sigma_x*energy%sigma_y) .LT. 1.0E-20) THEN
                !point source
                CALL xmi_coords_point(rng, geometry, photon, x1, y1)
        ELSE
                !gaussian source
                CALL xmi_coords_gaussian(rng, energy, geometry, photon, x1, y1)
        ENDIF

        photon%dirv(1) = SIN(x1) 
        photon%dirv(2) = SIN(y1) 
        photon%dirv(3) = SQRT(1.0_C_DOUBLE - photon%dirv(1)**2-photon%dirv(2)**2) 

#if DEBUG == 2
        WRITE (*,*) 'dirv: ', photon%dirv
#endif


        photon%theta = ACOS(photon%dirv(3))

        IF (photon%dirv(1) .EQ. 0.0_C_DOUBLE) THEN
                !watch out... if photon%dirv(2) EQ 0.0 then result may be
                !processor dependent...
                photon%phi = SIGN(M_PI_2, photon%dirv(2))
        ELSE
#if DEBUG == 2
                WRITE (*,'(A)') 'Dont think we should get here'
#endif
                photon%phi = ATAN(photon%dirv(2)/photon%dirv(1))
        ENDIF
        
!        photon%phi = ATAN2(photon%dirv(2),photon%dirv(1))
        
        RETURN

ENDSUBROUTINE xmi_coords_dir_cont

SUBROUTINE xmi_coords_point(rng, geometry, photon, x1, y1) 
        IMPLICIT NONE
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_geometry), INTENT(IN) :: geometry
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        REAL (C_DOUBLE), INTENT(OUT) :: x1, y1

        REAL (C_DOUBLE) :: x1_max, y1_max

#if DEBUG == 2
        WRITE (*,*) 'Entering xmi_coords_point'
#endif

        x1_max = ATAN(geometry%slit_size_x/geometry%d_source_slit/2.0_C_DOUBLE)
        y1_max = ATAN(geometry%slit_size_y/geometry%d_source_slit/2.0_C_DOUBLE)

        x1 = x1_max * fgsl_ran_flat(rng,-1.0_C_DOUBLE, 1.0_C_DOUBLE) 
        y1 = y1_max * fgsl_ran_flat(rng,-1.0_C_DOUBLE, 1.0_C_DOUBLE) 

        photon%coords(1:3) = 0.0_C_DOUBLE

        RETURN
ENDSUBROUTINE xmi_coords_point

SUBROUTINE xmi_coords_gaussian_disc(rng, energy, geometry, photon, x1, y1) 
        IMPLICIT NONE
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_energy_discrete), INTENT(IN) :: energy
        TYPE (xmi_geometry), INTENT(IN) :: geometry
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        REAL (C_DOUBLE), INTENT(OUT) :: x1, y1


        x1 = fgsl_ran_gaussian_ziggurat(rng, energy%sigma_xp)
        y1 = fgsl_ran_gaussian_ziggurat(rng, energy%sigma_yp)

        photon%coords(1) = fgsl_ran_gaussian_ziggurat(rng, energy%sigma_x) - &
                geometry%d_source_slit*SIN(x1)
        photon%coords(2) = fgsl_ran_gaussian_ziggurat(rng, energy%sigma_y) - & 
                geometry%d_source_slit*SIN(y1)
        photon%coords(3) = 0.0_C_DOUBLE

ENDSUBROUTINE xmi_coords_gaussian_disc

SUBROUTINE xmi_coords_gaussian_cont(rng, energy, geometry, photon, x1, y1) 
        IMPLICIT NONE
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_energy_continuous), INTENT(IN) :: energy
        TYPE (xmi_geometry), INTENT(IN) :: geometry
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        REAL (C_DOUBLE), INTENT(OUT) :: x1, y1


        x1 = fgsl_ran_gaussian_ziggurat(rng, energy%sigma_xp)
        y1 = fgsl_ran_gaussian_ziggurat(rng, energy%sigma_yp)

        photon%coords(1) = fgsl_ran_gaussian_ziggurat(rng, energy%sigma_x) - &
                geometry%d_source_slit*SIN(x1)
        photon%coords(2) = fgsl_ran_gaussian_ziggurat(rng, energy%sigma_y) - & 
                geometry%d_source_slit*SIN(y1)
        photon%coords(3) = 0.0_C_DOUBLE

ENDSUBROUTINE xmi_coords_gaussian_cont



SUBROUTINE xmi_photon_shift_first_layer(photon, composition, geometry)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (xmi_geometry), INTENT(IN) :: geometry
        TYPE (xmi_composition), INTENT(IN) :: composition
        TYPE (xmi_line) :: line
        TYPE (xmi_plane) :: plane
        INTEGER :: i

        IF (photon%coords(3) .GE. &
        composition%layers(1)%Z_coord_begin) THEN
                DO i=1,composition%n_layers
                        IF (photon%coords(3) .LT. &
                        composition%layers(i)%Z_coord_end) THEN
                                photon%current_layer=i
                                RETURN
                        ENDIF
                ENDDO
        ENDIF

        !Calculate intersection of photon trajectory with plane of first layer
        line%point = photon%coords
        line%dirv  = photon%dirv

        !d_sample_source is to be corrected with thicknesses of the layers
        !preceding reference_layer...
        plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE,&
        composition%layers(1)%Z_coord_begin]
        plane%normv = geometry%n_sample_orientation

        IF (xmi_intersection_plane_line(plane, line, photon%coords) == 0) THEN
                WRITE (error_unit,'(A)') 'xmi_intersection_plane_line error'
                WRITE (error_unit,'(A)') 'in xmi_photon_shift_first_layer'
                CALL EXIT(1)
        ENDIF

        photon%current_layer = 1

        RETURN
ENDSUBROUTINE xmi_photon_shift_first_layer

FUNCTION xmi_simulate_photon(photon, inputF, hdf5F,rng) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (fgsl_rng), INTENT(IN) :: rng
        INTEGER (C_INT) :: rv

        LOGICAL :: terminated
        REAL (C_DOUBLE) :: interactionR
        INTEGER (C_INT) :: i,j,k
        TYPE (xmi_plane) :: plane
        TYPE (xmi_line) :: line
        REAL (C_DOUBLE), DIMENSION(3) :: intersect,temp_coords
        REAL (C_DOUBLE) :: dist, max_random_layer, min_random_layer
        REAL (C_DOUBLE) :: dist_prev, tempexp, dist_sum, blbs
        INTEGER (C_INT) :: step_do_max, step_do_dir

        REAL (C_DOUBLE) :: atomsel_threshold
        INTEGER (C_INT) :: pos
        INTEGER (C_INT) :: rv_interaction, rv_check
        REAL (C_DOUBLE), DIMENSION(:), ALLOCATABLE :: distances, r_random_layer
        REAL (C_DOUBLE) :: r_random_layer_sum
        REAL (C_DOUBLE) :: Pabs, negln, my_sum,temp_sum, temp_prod
        INTEGER :: my_index

        rv = 0
        terminated = .FALSE.

#if DEBUG == 2
        WRITE (6,'(A)') 'Entering xmi_simulate_photon'
#endif



        main:DO
                !
                !
                !       Calculate steplength
                !
                !
                IF (photon%energy .LT. energy_threshold) THEN
                        EXIT
                ENDIF
                !Check in which layer it will interact
                photon%inside = .FALSE.
#if DEBUG == 2
                WRITE (*,'(A,F12.6)') 'interactionR= ',interactionR
#endif
                min_random_layer = 0.0_C_DOUBLE
                max_random_layer = 0.0_C_DOUBLE
                !recalculate mus if necessary
                IF (photon%energy_changed .EQV. .TRUE.) THEN
                        photon%mus = xmi_mu_calc(inputF%composition,&
                        photon%energy)
                        photon%energy_changed = .FALSE.
                ENDIF
        
                IF (DOT_PRODUCT(photon%dirv,inputF%geometry%&
                n_sample_orientation) .GT. 0.0_C_DOUBLE) THEN
                        !moving towards higher layers
                        step_do_max = inputF%composition%n_layers
                        step_do_dir = 1
                ELSE
                        !moving towards lower layers
                        step_do_max = 1 
                        step_do_dir = -1
                ENDIF

                line%dirv  = photon%dirv
                plane%normv = inputF%geometry%n_sample_orientation
                dist_prev = 0.0_C_DOUBLE
                dist_sum = 0.0_C_DOUBLE
                blbs = 1.0_C_DOUBLE
                min_random_layer = 0.0_C_DOUBLE
                max_random_layer = 0.0_C_DOUBLE
                interactionR = fgsl_rng_uniform(rng)

#if DEBUG == 1
                WRITE (*,'(A)') 'Before do loop'
                WRITE (*,'(A,I2)') 'optimizations: ',photon%options%use_optimizations
                WRITE (*,'(A,3F12.4)') 'initial photon%coords:',photon%coords
                WRITE (*,'(A,3F12.4)') 'initial photon%dirv:',photon%dirv
                WRITE (*,'(A,I2)') 'step_do_dir: ',step_do_dir
#endif

!!old
                IF (photon%options%use_optimizations .EQ. 0_C_INT .OR.&
                photon%options%use_variance_reduction .EQ. 0_C_INT) THEN
                        DO i=photon%current_layer,step_do_max, step_do_dir
                                !calculate distance between current coords and
                                !intersection with next layer
                                !determine next plane
#if DEBUG == 1
                               IF (photon%n_interactions .EQ. 1 .AND.&
                                photon%detector_hit2 .EQ. .TRUE.) THEN
                                        WRITE (*,'(A,I2)') 'last interaction:',&
                                        photon%last_interaction
                                        WRITE (*,'(A,3F12.5)') 'photon%coords: ', photon%coords
                                        WRITE (*,'(A,I2)') 'step_do_dir: ', step_do_dir
        
                                ENDIF
#endif
                                line%point = photon%coords
                                IF (step_do_dir .EQ. 1) THEN
                                        plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE,&
                                        inputF%composition%layers(i)%Z_coord_end]
                                ELSE
                                        plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE,&
                                        inputF%composition%layers(i)%Z_coord_begin]
                                ENDIF

                                IF (xmi_intersection_plane_line(plane, line, intersect) == 0) THEN
                                        WRITE (error_unit,'(A)') 'xmi_intersection_plane_line error'
                                        WRITE (error_unit,'(A)') 'in xmi_simulate_photon'
                                        CALL EXIT(1)
                                ENDIF
                        
                                dist = xmi_distance_two_points(photon%coords,intersect)
#if DEBUG == 1
                                WRITE (6,'(A,F12.6)') 'dist:',dist
                                WRITE (6,'(A,F12.6)') 'mus(i):',photon%mus(i)
                                WRITE (6,'(A,3F12.6)') 'intersect:',intersect
#endif


#if DEBUG == 1
                                IF (photon%n_interactions .EQ. 1 .AND.&
                                photon%detector_hit2 .EQ. .TRUE.) THEN
                                      WRITE (*,'(A,F12.5)') 'dist: ', dist
                                        WRITE (*,'(A,3F12.5)') 'intersect: ', intersect
                                        WRITE (*,'(A,3F12.5)') 'plane%point: ', plane%point
                                        WRITE (*,'(A,3F12.5)') 'plane%normv: ', plane%normv
                                        WRITE (*,'(A,F12.5)') 'mu: ', photon%mus(i)
                                ENDIF
#endif

                                !calculate max value of random number
                                !tempexp = EXP(-1.0_C_DOUBLE*(dist)*inputF%composition%layers(i)%density*&
                                !photon%mus(i))
                                temp_prod=-1.0_C_DOUBLE*dist*&
                                inputF%composition%layers(i)%density*&
                                photon%mus(i)
                                tempexp = EXP(temp_prod)

                                min_random_layer = max_random_layer 
                                max_random_layer = max_random_layer + blbs*(&
                                1.0_C_DOUBLE - tempexp)
                                !dist here must be total dist: from previous interaction
                                !until current layer
                       
#if DEBUG == 1
                                WRITE (6,'(A,F12.4)') 'tempexp: ',tempexp
                                WRITE (6,'(A,F12.4)') 'min_random_layer: ',min_random_layer
                                WRITE (6,'(A,F12.4)') 'max_random_layer: ',max_random_layer
                                WRITE (*,'(A,F12.5)') 'interactionR: ',&
                                interactionR
#endif
#if DEBUG == 1
                                !WRITE (*,'(A,F12.5)') 'tempexp: ', tempexp
                                IF (photon%n_interactions .EQ. 1 .AND.&
                                photon%detector_hit2 .EQ. .TRUE.) THEN
                                        WRITE (*,'(A,F12.5)') 'min_random_layer: ',&
                                       min_random_layer
                                        WRITE (*,'(A,F12.5)') 'max_random_layer: ',&
                                        max_random_layer
                                        WRITE (*,'(A,F12.5)') 'interactionR: ',&
                                        interactionR
                                ENDIF
#endif
                                IF (interactionR .LE. max_random_layer) THEN
                                        !interaction occurs in this layer!!!
                                        !update coordinates of photon
                                        dist = -1.0_C_DOUBLE*LOG(1.0_C_DOUBLE-(interactionR-&
                                        min_random_layer)/blbs)&
                                        /photon%mus(i)/inputF%composition%layers(i)%density
                                        temp_coords=photon%coords
                                        CALL xmi_move_photon_with_dist(photon,dist)
                                        IF (photon%options%escape_ratios_mode .NE. 1) THEN
                                          rv_check=xmi_check_detector_intersection&
                                          (inputF,temp_coords,photon%coords) 
                                          IF (rv_check .EQ.&
                                          XMI_COLLIMATOR_INTERSECTION .OR.&
                                          rv_check.EQ.XMI_DETECTOR_BAD_INTERSECTION) THEN
                                                EXIT main
                                          ELSEIF(rv_check.EQ.&
                                          XMI_DETECTOR_INTERSECTION) THEN
                                                photon%detector_hit = .TRUE.
                                                EXIT main
                                          ENDIF
#if DEBUG == 1
                                          WRITE (*,'(A,F12.5)') 'Actual dist: ',dist
                                          WRITE (*,'(A,3F12.5)') 'New coords: '&
                                          ,photon%coords
#endif
                                        ENDIF
                                        !update current_layer
                                        photon%current_layer = i
                                        photon%inside = .TRUE.
                                        !exit loop 
                                        EXIT
                                ELSE
                                        !goto next layer
                                        !coordinates equal to layer boundaries
                                        IF (photon%options%escape_ratios_mode .NE. 1) THEN
                                          rv_check=xmi_check_detector_intersection&
                                          (inputF,photon%coords,intersect) 
                                          IF (rv_check .EQ.&
                                          XMI_COLLIMATOR_INTERSECTION .OR.&
                                          rv_check.EQ.XMI_DETECTOR_BAD_INTERSECTION) THEN
                                                EXIT main
                                          ELSEIF(rv_check.EQ.&
                                          XMI_DETECTOR_INTERSECTION) THEN
                                                photon%detector_hit = .TRUE.
                                                EXIT main
                                          ENDIF
                                        ENDIF
                                        photon%coords = intersect
                                        dist_prev = dist_prev+dist
                                        blbs = blbs*tempexp
                                        !check if we are not leaving the system!
                                ENDIF
                        ENDDO
                ELSEIF (photon%options%use_optimizations .EQ. 1_C_INT) THEN
                        !new version of stepsize selection!!!
                        !optimize by forcing interactions
                        IF(photon%n_interactions .EQ.&
                        inputF%general%n_interactions_trajectory) THEN
                                EXIT main
                        ENDIF
                        IF (step_do_dir  .GT. 0) THEN
                                ALLOCATE(distances(photon%current_layer:step_do_max))
                        ELSE
                                ALLOCATE(distances(step_do_max:photon%current_layer))
                        ENDIF
                        line%point = photon%coords

                        !calculate distances and r_random_layer values
                        DO i=photon%current_layer,step_do_max, step_do_dir
                                IF (step_do_dir .EQ. 1) THEN
                                        plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE,&
                                        inputF%composition%layers(i)%Z_coord_end]
                                ELSE
                                        plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE,&
                                        inputF%composition%layers(i)%Z_coord_begin]
                                ENDIF

                                IF (xmi_intersection_plane_line(plane, line, intersect) == 0) THEN
                                        WRITE (error_unit,'(A)') 'xmi_intersection_plane_line error'
                                        WRITE (error_unit,'(A)') 'in xmi_photon_shift_first_layer'
                                        CALL EXIT(1)
                                ENDIF
                                
                                distances(i) = xmi_distance_two_points(line%point,intersect)
                                line%point = intersect
#if DEBUG == 1
                                WRITE (6,'(A,ES14.5)') 'distances(i): ',distances(i)
#endif
                        ENDDO

                        !calculate Pabs
                        Pabs = 0.
                        DO i=photon%current_layer,step_do_max, step_do_dir
                                Pabs = Pabs + photon%mus(i)*inputF%composition%layers(i)%density*distances(i)
                        ENDDO

                        Pabs = 1.0_C_DOUBLE - EXP(-1.0_C_DOUBLE*Pabs)
                        photon%weight = photon%weight * Pabs



                        negln = -1.0_C_DOUBLE*LOG(1.0_C_DOUBLE - interactionR*Pabs)

                        my_index = 0
                        my_sum = 0.0_C_DOUBLE
                        DO i=photon%current_layer,step_do_max, step_do_dir
                                my_sum = my_sum+photon%mus(i)*inputF%composition%layers(i)%density*distances(i)
                                IF (my_sum .GT. negln) THEN
                                        my_index =i
                                        EXIT
                                ENDIF
                        ENDDO
#if DEBUG == 1
                        WRITE (*,'(A,F12.4)') 'my_sum: ',my_sum
                        WRITE (*,'(A,F12.4)') 'negln: ',negln
#endif
                        temp_sum = 0.0_C_DOUBLE
                        DO i=photon%current_layer,my_index, step_do_dir
                                temp_sum = temp_sum + (1.0_C_DOUBLE-(photon%mus(i)*&
                                inputF%composition%layers(i)%density/(photon%mus(my_index)*&
                                inputF%composition%layers(my_index)%density)))*distances(i)
                        ENDDO

                        temp_sum = temp_sum - 1.0_C_DOUBLE*LOG(1.0_C_DOUBLE -&
                        interactionR*Pabs)/(photon%mus(my_index)*&
                        inputF%composition%layers(my_index)%density)
                        CALL xmi_move_photon_with_dist(photon,temp_sum)
#if DEBUG == 1
                        WRITE (*,'(A,F12.4)') 'Pabs: ',Pabs
                        WRITE (*,'(A,I2)') 'my_index: ',my_index
                        WRITE (*,'(A,F12.4)') 'temp_sum: ', temp_sum
                        WRITE (*,'(A,F12.4)') 'negln: ', negln
                        WRITE (*,'(A,F12.4)') 'R: ',interactionR
#endif
                        DEALLOCATE(distances)
                        photon%inside = .TRUE.
                        photon%current_layer = my_index
                ENDIF


#if DEBUG == 2
                WRITE (*,'(A)') 'After do loop'
#endif

                IF (photon%inside .EQV. .FALSE.) THEN
                        !photon has left the system
                        !check if it will make it to the detector
                        IF (photon%options%escape_ratios_mode .NE. 1) THEN
                                photon%detector_hit=xmi_check_photon_detector_hit(&
                                photon,inputF)
                        ENDIF
                        EXIT main
                ENDIF

                !update number of interactions
                photon%n_interactions = photon%n_interactions + 1

                IF(photon%n_interactions-1 .EQ.&
                inputF%general%n_interactions_trajectory) THEN
                        EXIT main
                ENDIF
                
                !variance reduction
#if DEBUG == 1
                WRITE (6,'(A,I2)') 'current_layer:',photon%current_layer
#endif
                IF (photon%options%use_variance_reduction .EQ. 1) THEN
                        CALL xmi_variance_reduction(photon, inputF, hdf5F, rng)
                ENDIF


 

                !selection of atom type
                !get a new random number
                !maybe this could be done faster... but I'm not sure
                interactionR = fgsl_rng_uniform(rng)
                atomsel_threshold = 0.0_C_DOUBLE
                DO i = 1, inputF%composition%layers&
                        (photon%current_layer)%n_elements
                       atomsel_threshold = atomsel_threshold + &
                       inputF%composition%layers(photon%current_layer)%weight(i)*&
                       CS_Total_Kissel(inputF%composition%layers(photon%current_layer)%Z(i),&
                       photon%energy)/photon%mus(photon%current_layer)
                       IF (interactionR .LT. atomsel_threshold) THEN
                                photon%current_element = inputF%composition&
                                %layers(photon%current_layer)%Z(i)
                                photon%current_element_index = i
                                EXIT
                       ENDIF
                ENDDO

#if DEBUG == 2
                WRITE (*,'(A,I)') 'Element selected: ',photon%current_element
#endif


                !selection of interaction type
                interactionR = fgsl_rng_uniform(rng)                

#if DEBUG == 2
                WRITE (*,'(A,I)') 'random number interactionprob: ',interactionR
#endif
                !find energy in interaction_probs
!                ASSOCIATE (hdf5_Z => inputF%composition%layers&
!                (photon%current_layer)%xmi_hdf5_Z_local&
!                (photon%current_element_index)%Ptr)

#define hdf5_Z inputF%composition%layers(photon%current_layer)%xmi_hdf5_Z_local(photon%current_element_index)%Ptr
        pos = findpos_fast(hdf5_Z%&
                interaction_probs%energies, photon%energy)
                IF (pos .LT. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Invalid result for findpos interaction type'
                        WRITE (error_unit,'(A,F12.6)') 'photon%energy: ',photon%energy
                        WRITE (error_unit,'(A,F12.6)') 'lowval: ',&
                        hdf5_Z%&
                        interaction_probs%energies(1)
                        WRITE (error_unit,'(A,F12.6)') 'highval: ',&
                        hdf5_Z%&
                        interaction_probs%energies(SIZE(&
                        hdf5_Z%&
                        interaction_probs%energies))
#if DEBUG == 1
                        WRITE (*,'(A,I2)') 'last interaction type: '&
                        ,photon%last_interaction
#endif
                        CALL EXIT(1)
                ENDIF

#if DEBUG == 2
                WRITE (*,'(A,I,F12.6)') 'pos and energy: ',pos, hdf5_Z%interaction_probs%energies(pos)
#endif

                IF (interactionR .LT. interpolate_simple([&
                        hdf5_Z%&
                        interaction_probs%energies(pos),&
                        hdf5_Z%&
                        interaction_probs%Rayl_and_Compt(pos,1)],&
                        [hdf5_Z%&
                        interaction_probs%energies(pos+1),&
                        hdf5_Z%&
                        interaction_probs%Rayl_and_Compt(pos+1,1)],&
                        photon%energy)) THEN
                        !we've got Rayleigh
                        photon%last_interaction = RAYLEIGH_INTERACTION
                        rv_interaction = xmi_simulate_photon_rayleigh(photon,&
                        inputF, hdf5F, rng) 
                ELSEIF (interactionR .LT. interpolate_simple([&
                        hdf5_Z%&
                        interaction_probs%energies(pos),&
                        hdf5_Z%&
                        interaction_probs%Rayl_and_Compt(pos,2)],&
                        [hdf5_Z%&
                        interaction_probs%energies(pos+1),&
                        hdf5_Z%&
                        interaction_probs%Rayl_and_Compt(pos+1,2)],&
                        photon%energy)) THEN
                        !we've got Compton 
                        photon%last_interaction = COMPTON_INTERACTION
                        rv_interaction = xmi_simulate_photon_compton(photon,&
                        inputF, hdf5F, rng) 
!                        photon%last_interaction = RAYLEIGH_INTERACTION
!                        rv_interaction = xmi_simulate_photon_rayleigh(photon,&
!                        inputF, hdf5F, rng) 
                ELSE
                        !we've got photoelectric
#if DEBUG == 1
                        WRITE (*,'(A)') 'photoelectric'
#endif
                        photon%last_interaction = PHOTOELECTRIC_INTERACTION
                        rv_interaction = xmi_simulate_photon_fluorescence(photon,&
                        inputF, hdf5F, rng) 
                ENDIF

                !abort if necessary
                IF (rv_interaction /= 1) THEN
                        RETURN
                ENDIF

#if DEBUG == 1
                !photon has left the system
                !check if it will make it to the detector
                photon%detector_hit2=xmi_check_photon_detector_hit(&
                        photon,inputF)
                IF (photon%energy .LT. energy_threshold) THEN
                        photon%detector_hit2 = .FALSE.
                ENDIF

#endif


                !quit if maximum number of interactions has been reached...



#if DEBUG == 2
                EXIT main       
#endif  

                !ENDASSOCIATE
#undef hdf5_Z
        ENDDO main

        rv = 1

ENDFUNCTION xmi_simulate_photon

FUNCTION xmi_init_input_escape_ratios(inputFPtr)&
BIND(C,NAME='xmi_init_input_escape_ratios') RESULT(rv)
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN) :: inputFPtr
        INTEGER (C_INT) :: rv,i,j
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: inverse

        TYPE (C_PTR) :: inversePtr
        TYPE (xmi_input), POINTER :: inputF
        REAL (C_DOUBLE) :: distance_sample_detector,half_apex 
        REAL (C_DOUBLE), ALLOCATABLE, TARGET, DIMENSION(:) :: &
        n_detector_orientation_new_x,&
        n_detector_orientation_new_y, n_detector_orientation_new_z

        rv = 0

        !associate pointers
        CALL C_F_POINTER(inputFPtr, inputF)

!        ASSOCIATE (layers => inputF%composition%layers, &
!        n_sample_orientation => inputF%geometry%n_sample_orientation )
#define layer inputF%composition%layers
#define my_n_sample_orientation inputF%geometry%n_sample_orientation
        DO j=1,SIZE(layer)
                !calculate thickness in Z direction
                layer(j)%thickness_along_Z = &
                ABS(layer(j)%thickness/DOT_PRODUCT(my_n_sample_orientation,&
                [0.0_C_DOUBLE,0.0_C_DOUBLE,1.0_C_DOUBLE]))

        ENDDO

        layer(inputF%composition%reference_layer)%Z_coord_begin =&
        0.0_C_DOUBLE+inputF%geometry%d_sample_source
        layer(inputF%composition%reference_layer)%Z_coord_end =&
        layer(inputF%composition%reference_layer)%thickness_along_Z+&
        inputF%geometry%d_sample_source

        DO j=inputF%composition%reference_layer+1,SIZE(layer)
                layer(j)%Z_coord_begin =layer(j-1)%Z_coord_end
                layer(j)%Z_coord_end = layer(j)%Z_coord_begin+&
                layer(j)%thickness_along_Z
        ENDDO
        DO j=inputF%composition%reference_layer-1,1,-1
                layer(j)%Z_coord_end = layer(j+1)%Z_coord_begin
                layer(j)%Z_coord_begin = layer(j)%Z_coord_end-&
                layer(j)%thickness_along_Z
        ENDDO


!        ENDASSOCIATE
#undef layers
#undef n_sample_orientation

        rv = 1

        RETURN

ENDFUNCTION xmi_init_input_escape_ratios


FUNCTION xmi_init_input(inputFPtr) BIND(C,NAME='xmi_init_input') RESULT(rv)
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN) :: inputFPtr
        INTEGER (C_INT) :: rv,i,j
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: inverse

        TYPE (C_PTR) :: inversePtr
        TYPE (xmi_input), POINTER :: inputF
        REAL (C_DOUBLE) :: distance_sample_detector,half_apex 
        REAL (C_DOUBLE), ALLOCATABLE, TARGET, DIMENSION(:) :: &
        n_detector_orientation_new_x,&
        n_detector_orientation_new_y, n_detector_orientation_new_z

        rv = 0

        !associate pointers
        CALL C_F_POINTER(inputFPtr, inputF)

        !geometry normalizations
        CALL normalize_vector(inputF%geometry%n_sample_orientation)
        IF (inputF%geometry%n_sample_orientation(3) .LT. 0.0) THEN
             inputF%geometry%n_sample_orientation=-1.0*inputF%geometry%n_sample_orientation   
        ENDIF
        CALL normalize_vector(inputF%geometry%n_detector_orientation)




        !investigate detector
        inputF%detector%detector_radius = SQRT(inputF%geometry%area_detector/M_PI)
        !IF (inputF%geometry%acceptance_detector .GE. M_PI) THEN
        !        inputF%detector%collimator_present = .FALSE.
        !        inputF%detector%collimator_height = 0.0_C_DOUBLE 
        !ELSE
        !        inputF%detector%collimator_present = .TRUE.
        !        inputF%detector%collimator_height = inputF%detector%detector_radius&
        !        *2.0_C_DOUBLE/TAN(inputF%geometry%acceptance_detector/2.0_C_DOUBLE) 
        !ENDIF

        !check for presence of collimator
        IF (inputF%geometry%collimator_height .GT. 0.0_C_DOUBLE .AND.&
        inputF%geometry%collimator_diameter .GT. 0.0_C_DOUBLE) THEN
                inputF%detector%collimator_present = .TRUE.
                inputF%detector%collimator_radius = &
                inputF%geometry%collimator_diameter/2.0_C_DOUBLE
                IF (inputF%detector%collimator_radius .GE. &
                inputF%detector%detector_radius) THEN
                        WRITE (error_unit,'(A)') 'Non conical collimator found'
                        CALL EXIT(1)
                ENDIF
                !if cylindrical... bad things may happen
                inputF%detector%half_apex = ATAN((inputF%detector%detector_radius-&
                inputF%detector%collimator_radius)/inputF%geometry%collimator_height)
                inputF%detector%vertex(1)=inputF%detector%detector_radius&
                /TAN(inputF%detector%half_apex)
                inputF%detector%vertex(2)=0.0_C_DOUBLE
                inputF%detector%vertex(3)=0.0_C_DOUBLE
        ELSE
                inputF%detector%collimator_present = .FALSE.
        ENDIF



        inputF%detector%n_detector_orientation_new_x = &
        inputF%geometry%n_detector_orientation

#if DEBUG == 1
        WRITE (*,'(A)') 'inputF%geometry%n_detector_orientation '
        WRITE (*,'(3F12.5)') inputF%geometry%n_detector_orientation
#endif

        !IF (ABS(DOT_PRODUCT(inputF%detector%n_detector_orientation_new_x,[1.0,0.0,0.0]))-1.0_C_DOUBLE .LT. 1.0E-6) THEN
        IF (ABS(DOT_PRODUCT(inputF%detector%n_detector_orientation_new_x,[1.0,0.0,0.0])) .GT. 1.0E-6) THEN
                !use y for cross product
                inputF%detector%n_detector_orientation_new_y = &
                cross_product([0.0_C_DOUBLE,1.0_C_DOUBLE,0.0_C_DOUBLE],inputF%detector%n_detector_orientation_new_x) 
        ELSE
#if DEBUG == 1
                WRITE (*,'(A)') 'This line should appear'
#endif
                !use x
                inputF%detector%n_detector_orientation_new_y = &
                cross_product([1.0_C_DOUBLE,0.0_C_DOUBLE,0.0_C_DOUBLE],inputF%detector%n_detector_orientation_new_x) 
        ENDIF
        CALL normalize_vector(inputF%detector%n_detector_orientation_new_y)
        inputF%detector%n_detector_orientation_new_z = &
        cross_product(inputF%detector%n_detector_orientation_new_x,&
        inputF%detector%n_detector_orientation_new_y)

#if DEBUG == 1
        WRITE (*,'(A)') 'before calling xmi_inverse_matrix'
        WRITE (*,'(3F12.5)') inputF%detector%n_detector_orientation_new_x
        WRITE (*,'(3F12.5)') inputF%detector%n_detector_orientation_new_y
        WRITE (*,'(3F12.5)') inputF%detector%n_detector_orientation_new_z
#endif

        inputF%detector%n_detector_orientation_new(:,1)&
        = inputF%detector%n_detector_orientation_new_x
        inputF%detector%n_detector_orientation_new(:,2)&
        = inputF%detector%n_detector_orientation_new_y
        inputF%detector%n_detector_orientation_new(:,3)&
        = inputF%detector%n_detector_orientation_new_z

        !calculate inverse of n_detector_orientation_new -> C
        ALLOCATE(n_detector_orientation_new_x(3))
        ALLOCATE(n_detector_orientation_new_y(3))
        ALLOCATE(n_detector_orientation_new_z(3))
        n_detector_orientation_new_x=inputF%detector%n_detector_orientation_new_x
        n_detector_orientation_new_y=inputF%detector%n_detector_orientation_new_y
        n_detector_orientation_new_z=inputF%detector%n_detector_orientation_new_z
        CALL xmi_inverse_matrix(C_LOC(n_detector_orientation_new_x),&
        C_LOC(n_detector_orientation_new_y),&
        C_LOC(n_detector_orientation_new_z), inversePtr)
        CALL C_F_POINTER(inversePtr, inverse,[3,3]) 
#if DEBUG == 1
        WRITE (*,'(A)') 'before inverting'
        WRITE (*,'(3(3F12.4,/))') inputF%detector%n_detector_orientation_new(1,:), &
        inputF%detector%n_detector_orientation_new(2,:), &
        inputF%detector%n_detector_orientation_new(3,:)
        WRITE (*,'(A)') 'inverse'
        WRITE (*,'(3(3F12.4,/))') inverse(1,:), inverse(2,:),inverse(3,:)
#endif
        inputF%detector%n_detector_orientation_inverse = inverse

        !calculate detector solid angle
        distance_sample_detector =&
        xmi_distance_two_points(inputF%geometry%p_detector_window,&
        [0.0_C_DOUBLE, 0.0_C_DOUBLE,inputF%geometry%d_sample_source]) 
        half_apex = &
        ATAN(inputF%detector%detector_radius/distance_sample_detector)
        inputF%detector%detector_solid_angle = &
        2*M_PI*(1.0_C_DOUBLE-COS(half_apex))
#if DEBUG == 1
        WRITE (*,'(A,F12.4)') 'detector solid angle: ',inputF%detector%&
        detector_solid_angle
#endif


        !calculate n_sample_orientation in detector coordinates
        inputF%detector%n_sample_orientation_det = &
        MATMUL(inputF%detector%n_detector_orientation_inverse, &
        inputF%geometry%n_sample_orientation) 


!        ASSOCIATE (layers => inputF%composition%layers, &
!        n_sample_orientation => inputF%geometry%n_sample_orientation )
#define layer inputF%composition%layers
#define my_n_sample_orientation inputF%geometry%n_sample_orientation
        DO j=1,SIZE(layer)
                !calculate thickness in Z direction
                layer(j)%thickness_along_Z = &
                ABS(layer(j)%thickness/DOT_PRODUCT(my_n_sample_orientation,&
                [0.0_C_DOUBLE,0.0_C_DOUBLE,1.0_C_DOUBLE]))
        ENDDO

        layer(inputF%composition%reference_layer)%Z_coord_begin =&
        0.0_C_DOUBLE+inputF%geometry%d_sample_source
        layer(inputF%composition%reference_layer)%Z_coord_end =&
        layer(inputF%composition%reference_layer)%thickness_along_Z+&
        inputF%geometry%d_sample_source

        DO j=inputF%composition%reference_layer+1,SIZE(layer)
                layer(j)%Z_coord_begin =layer(j-1)%Z_coord_end
                layer(j)%Z_coord_end = layer(j)%Z_coord_begin+&
                layer(j)%thickness_along_Z
        ENDDO
        DO j=inputF%composition%reference_layer-1,1,-1
                layer(j)%Z_coord_end = layer(j+1)%Z_coord_begin
                layer(j)%Z_coord_begin = layer(j)%Z_coord_end-&
                layer(j)%thickness_along_Z
        ENDDO


!        ENDASSOCIATE
#undef layers
#undef n_sample_orientation

        rv = 1

        RETURN

ENDFUNCTION xmi_init_input

FUNCTION xmi_check_photon_detector_hit(photon, inputF) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(IN) :: photon
        TYPE (xmi_input), INTENT(IN) :: inputF
        LOGICAL :: rv
        REAL (C_DOUBLE), DIMENSION(3) :: photon_dirv_det, photon_coords_det
        TYPE (xmi_plane) :: plane_det_base, plane_det_coll
        TYPE (xmi_line) :: photon_trajectory
        REAL (C_DOUBLE), DIMENSION(3) :: intersect

        !assume 
        rv = .FALSE.

        !first thing to check: is the direction of the photon at least the
        !opposite of the detector normal
        !statistically speaking, one loses half of the photons this way
        IF (DOT_PRODUCT(photon%dirv, inputF%geometry%n_detector_orientation)&
        .GE. 0.0_C_DOUBLE) RETURN

        !second: calculate intersection with detector plane, using the detector
        !coordinate system
        photon_dirv_det = MATMUL(inputF%detector%n_detector_orientation_inverse, &
                photon%dirv)
        photon_coords_det = MATMUL(inputF%detector%n_detector_orientation_inverse, &
                photon%coords-inputF%geometry%p_detector_window)
        plane_det_base%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]
        plane_det_base%normv = [1.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]
       
        photon_trajectory%dirv = photon_dirv_det
        photon_trajectory%point = photon_coords_det

        IF (xmi_intersection_plane_line(plane_det_base, photon_trajectory,intersect) == 0) THEN
                WRITE (error_unit,'(A)') 'xmi_intersection_plane_line error'
                WRITE (error_unit,'(A)') 'in xmi_check_photon_detector_hit'
                CALL EXIT(1)
        ENDIF

        IF (norm(intersect) .GT. inputF%detector%detector_radius) RETURN

        !third: ok it will hit the detector, but will it make it through the
        !collimator as well??
        IF (inputF%detector%collimator_present .EQV. .FALSE.) THEN
                !there is no collimator
                rv = .TRUE.
                RETURN
        ENDIF
        
        !there is a collimator...
        plane_det_base%point = [inputF%geometry%collimator_height, 0.0_C_DOUBLE, 0.0_C_DOUBLE]

        IF (xmi_intersection_plane_line(plane_det_base, photon_trajectory,intersect) == 0) THEN
                WRITE (error_unit,'(A)') 'xmi_intersection_plane_line error'
                WRITE (error_unit,'(A)') 'in xmi_check_photon_detector_hit'
                CALL EXIT(1)
        ENDIF

        intersect(1) = 0.0_C_DOUBLE

        IF (norm(intersect) .GT. inputF%detector%collimator_radius) RETURN

        !ok, valid hit
        rv = .TRUE.

        RETURN
ENDFUNCTION xmi_check_photon_detector_hit

FUNCTION xmi_simulate_photon_rayleigh(photon, inputF, hdf5F, rng) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (fgsl_rng), INTENT(IN) :: rng
        INTEGER (C_INT) :: rv
        REAL (C_DOUBLE) :: theta_i, phi_i
        INTEGER (C_INT) :: pos_1, pos_2
        REAL (C_DOUBLE) :: r,sinphi0,cosphi0,phi0
        REAL (C_DOUBLE) :: costheta, sintheta, sinphi, cosphi

        rv = 0

!        ASSOCIATE (hdf5_Z => inputF%composition%layers&
!                (photon%current_layer)%xmi_hdf5_Z_local&
!                (photon%current_element_index)%Ptr)

#define hdf5_Z inputF%composition%layers(photon%current_layer)%xmi_hdf5_Z_local(photon%current_element_index)%Ptr


        !calculate theta
        pos_1 = 0_C_INT
        pos_2 = 0_C_INT
        r = fgsl_rng_uniform(rng)

#if DEBUG == 2
        WRITE (*,'(A,I2)') 'element: ',hdf5_Z%Z
        WRITE (*,'(A,F12.6)') 'energy: ',photon%energy
        WRITE (*,'(A,F12.6)') 'r: ',r
        WRITE (*,'(A,F12.6)') 'hdf5_Z%Energies(1): ',hdf5_Z%Energies(1)
        WRITE (*,'(A,F12.6)') 'hdf5_Z%Energies(last): ',hdf5_Z%Energies(SIZE(hdf5_Z%Energies))
        WRITE (*,'(A,F12.6)') 'hdf5_Z%RandomNumbers(1): ',hdf5_Z%RandomNumbers(1)
        WRITE (*,'(A,F12.6)') 'hdf5_Z%RandomNumbers(last): ',hdf5_Z%RandomNumbers(SIZE(hdf5_Z%RandomNumbers))
        WRITE (*,'(A,F12.6)') 'hdf5_Z%RayleighTheta_ICDF(20,20): ',hdf5_Z%RayleighTheta_ICDF(20,20)
#endif


        theta_i = bilinear_interpolation(&
        hdf5_Z%&
        RayleighTheta_ICDF, &
        hdf5_Z%Energies, &
        hdf5_Z%RandomNumbers, &
        photon%energy,&
        r, pos_1, pos_2) 

#if DEBUG == 2
        WRITE (*,'(A,F12.6)') 'theta_i: ',theta_i
#endif


        !calculate phi
        pos_1 = 0_C_INT
        pos_2 = 0_C_INT

        phi_i = bilinear_interpolation(hdf5F%RayleighPhi_ICDF, &
                hdf5F%RayleighThetas, hdf5F%RayleighRandomNumbers,&
                theta_i, fgsl_rng_uniform(rng), pos_1, pos_2)
        
#if DEBUG == 2
        WRITE (*,'(A,F12.6)') 'phi_i: ',phi_i
#endif
        !according to laszlos code (around line 1360), some things need to get
        !done here first involving the electric field and Theta_i and Phi_i of
        !the previous run
        cosphi = COS(photon%phi)
        sinphi = SIN(photon%phi)
        costheta = COS(photon%theta)
        sintheta = SIN(photon%theta)

        cosphi0 = DOT_PRODUCT(photon%elecv,[cosphi*costheta,&
        costheta*sinphi,-sintheta])
        sinphi0 = DOT_PRODUCT(photon%elecv,[sinphi,&
        -cosphi, 0.0_C_DOUBLE])
        IF (ABS(cosphi0) .GT. 1.0_C_DOUBLE) cosphi0 = SIGN(1.0_C_DOUBLE,cosphi0)
        phi0 = ACOS(cosphi0)
        IF (sinphi0 .GT. 0.0_C_DOUBLE) phi0 = -phi0

#if DEBUG == 1
        WRITE (*,'(A)') 'Rayleigh scattering angles'
        WRITE (*,'(A, ES12.5)') 'theta_i: ',theta_i
        WRITE (*,'(A, ES12.5)') 'phi_i: ',phi_i
        WRITE (*,'(A, ES12.5)') 'phi_i+phi0: ',phi_i+phi0
#endif


        !
        !update photon%theta and photon%phi
        !
        CALL xmi_update_photon_dirv(photon, theta_i, phi0+phi_i)

        !
        !update electric field
        !
        CALL xmi_update_photon_elecv(photon)

        !update history
        photon%history(photon%n_interactions,1) = RAYLEIGH_INTERACTION
        photon%history(photon%n_interactions,2) = photon%current_element 


!        ENDASSOCIATE
#undef hdf5_Z

        rv = 1

#if DEBUG == 1
        IF (photon%energy .GT. 28.0_C_DOUBLE) THEN
                WRITE (output_unit, '(A,ES12.6)') 'rayl energy:',&
                photon%energy
        ENDIF
#endif
        RETURN
ENDFUNCTION xmi_simulate_photon_rayleigh

FUNCTION xmi_simulate_photon_compton(photon, inputF, hdf5F, rng) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (fgsl_rng), INTENT(IN) :: rng
        INTEGER (C_INT) :: rv, pos_1, pos_2
        REAL (C_DOUBLE) :: theta_i, phi_i
        REAL (C_DOUBLE) :: r,sinphi0,cosphi0,phi0
        REAL (C_DOUBLE) :: costheta, sintheta, sinphi, cosphi
        REAL (C_DOUBLE) :: pp, rat, rk, w_h
       
        rv = 0
        pos_1 = 0
        pos_2 = 0

!        ASSOCIATE (hdf5_Z => inputF%composition%layers&
!                (photon%current_layer)%xmi_hdf5_Z_local&
!                (photon%current_element_index)%Ptr)
#define hdf5_Z inputF%composition%layers(photon%current_layer)%xmi_hdf5_Z_local(photon%current_element_index)%Ptr

        !calculate theta
        theta_i = bilinear_interpolation(&
        hdf5_Z%ComptonTheta_ICDF, &
        hdf5_Z%Energies, &
        hdf5_Z%RandomNumbers, &
        photon%energy,fgsl_rng_uniform(rng), pos_1, pos_2) 

        !calculate phi
        phi_i = trilinear_interpolation(&
        hdf5F%ComptonPhi_ICDF, &
        hdf5F%ComptonThetas, &
        hdf5F%ComptonEnergies,&
        hdf5F%ComptonRandomNumbers,&
        theta_i, photon%energy,fgsl_rng_uniform(rng)) 


        !again according to laszlo... some things need to happen here first...
        !see comment with rayleigh
        cosphi = COS(photon%phi)
        sinphi = SIN(photon%phi)
        costheta = COS(photon%theta)
        sintheta = SIN(photon%theta)

        cosphi0 = DOT_PRODUCT(photon%elecv,[cosphi*costheta,&
        costheta*sinphi,-sintheta])
        sinphi0 = DOT_PRODUCT(photon%elecv,[sinphi,&
        -cosphi, 0.0_C_DOUBLE])
        IF (ABS(cosphi0) .GT. 1.0_C_DOUBLE) cosphi0 = SIGN(1.0_C_DOUBLE,cosphi0)
        phi0 = ACOS(cosphi0)
        IF (sinphi0 .GT. 0.0_C_DOUBLE) phi0 = -phi0

#if DEBUG == 1
        WRITE (*,'(A)') 'Compton scattering angles'
        WRITE (*,'(A, ES12.5)') 'theta_i: ',theta_i
        WRITE (*,'(A, ES12.5)') 'phi_i: ',phi_i
        WRITE (*,'(A, ES12.5)') 'phi_i+phi0: ',phi_i+phi0
#endif


        !
        !update energy of photon!!!
        !
        CALL xmi_update_photon_energy_compton(photon, theta_i, rng, inputF,&
        hdf5f)

        !
        !update photon%theta and photon%phi
        !
        CALL xmi_update_photon_dirv(photon, theta_i, phi_i+phi0)

        !
        !update electric field
        !
        CALL xmi_update_photon_elecv(photon)

        !
        !for compton, laszlo does a further manipulation, probably has to with
        !change of degree of polarization
        !
        pp = 2.0_C_DOUBLE * ((COS(theta_i)*COS(phi_i))**2+SIN(phi_i)**2)
        rat = 1.0_C_DOUBLE/(1.0_C_DOUBLE +&
        (1-COS(theta_i))*photon%energy/510.998910)
        rk = rat - 2.0_C_DOUBLE + 1.0_C_DOUBLE/rat
        pp = pp/(rk+pp)
        r = fgsl_rng_uniform(rng)
        w_h = (1.0_C_DOUBLE+pp)/2.0_C_DOUBLE

        IF (r .GT. w_h) THEN
                photon%elecv = cross_product(photon%dirv, photon%elecv)
        ENDIF



        !update history
        photon%history(photon%n_interactions,1) = COMPTON_INTERACTION
        photon%history(photon%n_interactions,2) = photon%current_element 

!        ENDASSOCIATE
#undef hdf5_Z
        rv = 1

#if DEBUG == 1
        IF (photon%energy .GT. 28.0_C_DOUBLE) THEN
                WRITE (output_unit, '(A,ES12.6)') 'compt energy:',&
                photon%energy
        ENDIF
#endif
        RETURN

ENDFUNCTION xmi_simulate_photon_compton

FUNCTION xmi_simulate_photon_fluorescence(photon, inputF, hdf5F, rng) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (fgsl_rng), INTENT(IN) :: rng
        INTEGER (C_INT) :: rv,trans

        REAL (C_DOUBLE) :: photo_total 
        REAL (C_DOUBLE) :: sumz
        INTEGER (C_INT) :: shell,line_first, line_last, line
        REAL (C_DOUBLE) :: r
        REAL (C_DOUBLE) :: theta_i, phi_i

        LOGICAL :: shell_found, line_found
        INTEGER (C_INT) :: max_shell
        INTEGER :: i

#if DEBUG == 1
        WRITE (*,'(A)') 'Entering fluorescence'
        WRITE (*,'(A,I2)') 'element: ',photon%current_element
#endif

        rv = 0
        !so we've got photo electric effect
        !first is to check which shell got lucky
        photo_total = CS_Photo_Total(photon%current_element, photon%energy)

        sumz = 0.0_C_DOUBLE
        shell_found = .FALSE.

        !for now let's just look at K- and L-lines
        r = fgsl_rng_uniform(rng)

        IF (photon%options%use_M_lines .EQ. 1_C_INT) THEN
                max_shell = M5_SHELL
        ELSE
                max_shell = L3_SHELL
        ENDIF

        DO shell=K_SHELL,max_shell
                sumz = sumz + CS_Photo_Partial(photon%current_element, shell,&
                photon%energy)/photo_total
                IF (r .LT. sumz) THEN
                        shell_found = .TRUE.
                        EXIT
                ENDIF
        ENDDO

#if DEBUG == 1
        photon%last_shell = shell
#endif


        IF (.NOT. shell_found) THEN
                ! no shell matches -> probably M or higher...
#if DEBUG == 1
              WRITE (*,'(A)') 'No shell found'
              WRITE (*,'(A)') 'r: ', r
              WRITE (*,'(A)') 'sumz: ', sumz
              WRITE (*,'(A)') 'photo_total: ', photo_total
#endif
               photon%energy = 0.0_C_DOUBLE
               rv = 1
               RETURN
        ENDIF

#if DEBUG == 1
        WRITE (*,'(A,I2)') 'shell found: ',shell
#endif

        !first fluorescence yield check, then Coster-Kronig!!!
        IF (photon%options%use_optimizations .EQ. 0_C_INT .OR. &
        photon%options%use_variance_reduction .EQ. 0_C_INT) THEN
                !no optimizations // no variance reduction
                IF (xmi_fluorescence_yield_check(rng, shell, inputF%composition%layers&
                (photon%current_layer)%xmi_hdf5_Z_local&
                (photon%current_element_index)%Ptr,&
                photon%energy) .EQ. 0_C_INT) THEN
                        IF (photon%options%use_cascade_auger .EQ. 1_C_INT .AND.&
                        photon%options%use_variance_reduction .EQ. 0_C_INT) THEN
                                CALL xmi_simulate_photon_cascade_auger(photon,shell&
                                ,rng,inputF,hdf5F)
                        ENDIF
#if DEBUG == 1
                        WRITE (*,'(A)') 'No fluorescence: Auger'
#endif
                        rv = 1
                        RETURN
                ENDIF
        ELSE
                !optimizations and variance reduction
                CALL xmi_fluorescence_yield_check_varred_optim(photon, rng, shell, &
                inputF%composition%layers&
                (photon%current_layer)%xmi_hdf5_Z_local&
                (photon%current_element_index)%Ptr)
        ENDIF

        !Coster-Kronig for L and M
        CALL xmi_coster_kronig_check(rng, shell, photon%current_element)

#if DEBUG == 1
        WRITE (*,'(A)') 'after CK check'
#endif
        !so now that we determined the shell to be used, see if we get
        !fluorescence...
        IF (xmi_fluorescence_line_check(rng, shell, photon%current_element,&
        photon%energy, line) .EQ. 0_C_INT) THEN
                rv = 1
                RETURN
        ENDIF

#if DEBUG == 1
        WRITE (*,'(A)') 'after fluor line check'
#endif
        photon%energy_changed = .FALSE.
        DO i=1,inputF%composition%n_layers
                photon%mus(i) = &
                photon%precalc_mu_cs(i)%mu(photon%current_element,ABS(line)) 
        ENDDO

        !calculate theta and phi
        theta_i = ACOS(2.0_C_DOUBLE*fgsl_rng_uniform(rng)-1.0_C_DOUBLE)
        phi_i = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)

#if DEBUG == 1
        WRITE (*,'(A)') 'Before update_photon_dirv'
        WRITE (*,'(A,I3)') 'photon%current_element: ',photon%current_element
        WRITE (*,'(A,F12.5)') 'photon%energy: ',photon%energy
        WRITE (*,'(A,F12.5)') 'theta_i: ',theta_i
        WRITE (*,'(A,F12.5)') 'phi_i: ',phi_i
        WRITE (*,'(A,ES12.5)') 'photon%theta: ',photon%theta
        WRITE (*,'(A,ES12.5)') 'photon%phi: ',photon%phi
        WRITE (*,'(A,3ES12.5)') 'photon%coords: ',photon%coords
        WRITE (*,'(A,3ES12.5)') 'photon%dirv: ',photon%dirv
#endif



        !
        !update photon%theta and photon%phi
        !
        CALL xmi_update_photon_dirv(photon, theta_i, phi_i)

#if DEBUG == 1
        WRITE (*,'(A)') 'After update_photon_dirv'
        WRITE (*,'(A,F12.5)') 'theta_i: ',theta_i
        WRITE (*,'(A,F12.5)') 'phi_i: ',phi_i
        WRITE (*,'(A,F12.5)') 'photon%theta: ',photon%theta
        WRITE (*,'(A,F12.5)') 'photon%phi: ',photon%phi
        WRITE (*,'(A,3F12.5)') 'photon%coords: ',photon%coords
        WRITE (*,'(A,3F12.5)') 'photon%dirv: ',photon%dirv
        CALL EXIT(1)
#endif
        !
        !update electric field
        !
        CALL xmi_update_photon_elecv(photon)

        !update history
        photon%history(photon%n_interactions,1) = line 
        photon%history(photon%n_interactions,2) = photon%current_element 

        IF (photon%options%use_cascade_radiative .EQ. 1_C_INT .AND.&
        photon%options%use_variance_reduction .EQ. 0_C_INT) THEN
                CALL xmi_simulate_photon_cascade_radiative(photon,shell,line&
                ,rng,inputF,hdf5F)
        ENDIF

#if DEBUG == 1
        IF (photon%energy .GT. 28.0_C_DOUBLE) THEN
                WRITE (output_unit, '(A,ES12.6)') 'xrf energy:',&
                photon%energy
        ENDIF
#endif

        rv = 1

        RETURN

ENDFUNCTION xmi_simulate_photon_fluorescence

SUBROUTINE xmi_simulate_photon_cascade_auger(photon, shell, rng,inputF,hdf5F)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        INTEGER (C_INT), INTENT(IN) :: shell
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF
        INTEGER (C_INT) :: shell_new, line_new, auger,&
        auger_first, auger_last, shell_new1, shell_new2
        TYPE (fgsl_rng), INTENT(IN) :: rng
        REAL (C_DOUBLE) :: energy,r,cosalfa,c_alfa,c_ae,c_be
        REAL (C_DOUBLE) :: sumz

        LOGICAL :: auger_found

        !
        !
        !       Auger cascades!
        !
        !

        ! At this point we have a vacancy in shell, which will produce two new
        ! vacancies in lower shells. These will be determined with xraylib's
        ! AugerRate function
        ! We are only considering L by default and M when requested
        !

        auger_found =.FALSE.
        sumz = 0.0_C_DOUBLE

        IF (shell .EQ. K_SHELL) THEN
                !K shell excitation
                auger_first = K_L1L1_AUGER
                auger_last = K_M5Q3_AUGER
        ELSEIF (shell .EQ. L1_SHELL) THEN
                !L1 shell excitation
                auger_first = L1_M1M1_AUGER
                auger_last = L1_M5Q3_AUGER
        ELSEIF (shell .EQ. L2_SHELL) THEN
                !L2 shell excitation
                auger_first = L2_M1M1_AUGER
                auger_last = L2_M5Q3_AUGER
        ELSEIF (shell .EQ. L3_SHELL) THEN
                !L3 shell excitation
                auger_first = L3_M1M1_AUGER
                auger_last = L3_M5Q3_AUGER
        ELSE
                RETURN
        ENDIF
                

        r = fgsl_rng_uniform(rng)

        DO auger=auger_first, auger_last
                sumz = sumz + AugerRate(photon%current_element,auger)
                IF (r .LT. sumz) THEN
                        auger_found = .TRUE.
                        EXIT
                ENDIF
        ENDDO

        IF (.NOT. auger_found) THEN
                !found nothing... perhaps an X_NN transition which is not
                !covered here
                RETURN
        ENDIF

        !if we found something, determine the new shells involved
        !thanks to vim and its substitute command, this is not so tricky :-)
        !'a,'bs/.\+_\(..\)\(..\)_AUGER/\t\tCASE(\0)\r\t\t\tshell_new1 = \1_SHELL\r\t\t\tshell_new2 = \2_SHELL/
        SELECT CASE (auger)
                CASE(K_L1L1_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = L1_SHELL
                CASE(K_L1L2_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = L2_SHELL
                CASE(K_L1L3_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = L3_SHELL
                CASE(K_L1M1_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = M1_SHELL
                CASE(K_L1M2_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = M2_SHELL
                CASE(K_L1M3_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = M3_SHELL
                CASE(K_L1M4_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = M4_SHELL
                CASE(K_L1M5_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = M5_SHELL
                CASE(K_L1N1_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = N1_SHELL
                CASE(K_L1N2_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = N2_SHELL
                CASE(K_L1N3_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = N3_SHELL
                CASE(K_L1N4_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = N4_SHELL
                CASE(K_L1N5_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = N5_SHELL
                CASE(K_L1N6_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = N6_SHELL
                CASE(K_L1N7_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = N7_SHELL
                CASE(K_L1O1_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = O1_SHELL
                CASE(K_L1O2_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = O2_SHELL
                CASE(K_L1O3_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = O3_SHELL
                CASE(K_L1O4_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = O4_SHELL
                CASE(K_L1O5_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = O5_SHELL
                CASE(K_L1O6_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = O6_SHELL
                CASE(K_L1O7_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = O7_SHELL
                CASE(K_L1P1_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = P1_SHELL
                CASE(K_L1P2_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = P2_SHELL
                CASE(K_L1P3_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = P3_SHELL
                CASE(K_L1P4_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = P4_SHELL
                CASE(K_L1P5_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = P5_SHELL
                CASE(K_L1Q1_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = Q1_SHELL
                CASE(K_L1Q2_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = Q2_SHELL
                CASE(K_L1Q3_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = Q3_SHELL
                CASE(K_L2L1_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = L1_SHELL
                CASE(K_L2L2_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = L2_SHELL
                CASE(K_L2L3_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = L3_SHELL
                CASE(K_L2M1_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M1_SHELL
                CASE(K_L2M2_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M2_SHELL
                CASE(K_L2M3_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M3_SHELL
                CASE(K_L2M4_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M4_SHELL
                CASE(K_L2M5_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M5_SHELL
                CASE(K_L2N1_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = N1_SHELL
                CASE(K_L2N2_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = N2_SHELL
                CASE(K_L2N3_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = N3_SHELL
                CASE(K_L2N4_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = N4_SHELL
                CASE(K_L2N5_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = N5_SHELL
                CASE(K_L2N6_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = N6_SHELL
                CASE(K_L2N7_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = N7_SHELL
                CASE(K_L2O1_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = O1_SHELL
                CASE(K_L2O2_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = O2_SHELL
                CASE(K_L2O3_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = O3_SHELL
                CASE(K_L2O4_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = O4_SHELL
                CASE(K_L2O5_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = O5_SHELL
                CASE(K_L2O6_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = O6_SHELL
                CASE(K_L2O7_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = O7_SHELL
                CASE(K_L2P1_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = P1_SHELL
                CASE(K_L2P2_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = P2_SHELL
                CASE(K_L2P3_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = P3_SHELL
                CASE(K_L2P4_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = P4_SHELL
                CASE(K_L2P5_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = P5_SHELL
                CASE(K_L2Q1_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = Q1_SHELL
                CASE(K_L2Q2_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = Q2_SHELL
                CASE(K_L2Q3_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = Q3_SHELL
                CASE(K_L3L1_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = L1_SHELL
                CASE(K_L3L2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = L2_SHELL
                CASE(K_L3L3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = L3_SHELL
                CASE(K_L3M1_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M1_SHELL
                CASE(K_L3M2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M2_SHELL
                CASE(K_L3M3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M3_SHELL
                CASE(K_L3M4_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M4_SHELL
                CASE(K_L3M5_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M5_SHELL
                CASE(K_L3N1_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = N1_SHELL
                CASE(K_L3N2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = N2_SHELL
                CASE(K_L3N3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = N3_SHELL
                CASE(K_L3N4_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = N4_SHELL
                CASE(K_L3N5_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = N5_SHELL
                CASE(K_L3N6_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = N6_SHELL
                CASE(K_L3N7_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = N7_SHELL
                CASE(K_L3O1_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = O1_SHELL
                CASE(K_L3O2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = O2_SHELL
                CASE(K_L3O3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = O3_SHELL
                CASE(K_L3O4_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = O4_SHELL
                CASE(K_L3O5_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = O5_SHELL
                CASE(K_L3O6_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = O6_SHELL
                CASE(K_L3O7_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = O7_SHELL
                CASE(K_L3P1_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = P1_SHELL
                CASE(K_L3P2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = P2_SHELL
                CASE(K_L3P3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = P3_SHELL
                CASE(K_L3P4_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = P4_SHELL
                CASE(K_L3P5_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = P5_SHELL
                CASE(K_L3Q1_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = Q1_SHELL
                CASE(K_L3Q2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = Q2_SHELL
                CASE(K_L3Q3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = Q3_SHELL
                CASE(K_M1L1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = L1_SHELL
                CASE(K_M1L2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = L2_SHELL
                CASE(K_M1L3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = L3_SHELL
                CASE(K_M1M1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M1_SHELL
                CASE(K_M1M2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M2_SHELL
                CASE(K_M1M3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M3_SHELL
                CASE(K_M1M4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M4_SHELL
                CASE(K_M1M5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M5_SHELL
                CASE(K_M1N1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N1_SHELL
                CASE(K_M1N2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N2_SHELL
                CASE(K_M1N3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N3_SHELL
                CASE(K_M1N4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N4_SHELL
                CASE(K_M1N5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N5_SHELL
                CASE(K_M1N6_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N6_SHELL
                CASE(K_M1N7_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N7_SHELL
                CASE(K_M1O1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O1_SHELL
                CASE(K_M1O2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O2_SHELL
                CASE(K_M1O3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O3_SHELL
                CASE(K_M1O4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O4_SHELL
                CASE(K_M1O5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O5_SHELL
                CASE(K_M1O6_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O6_SHELL
                CASE(K_M1O7_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O7_SHELL
                CASE(K_M1P1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P1_SHELL
                CASE(K_M1P2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P2_SHELL
                CASE(K_M1P3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P3_SHELL
                CASE(K_M1P4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P4_SHELL
                CASE(K_M1P5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P5_SHELL
                CASE(K_M1Q1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q1_SHELL
                CASE(K_M1Q2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q2_SHELL
                CASE(K_M1Q3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q3_SHELL
                CASE(K_M2L1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = L1_SHELL
                CASE(K_M2L2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = L2_SHELL
                CASE(K_M2L3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = L3_SHELL
                CASE(K_M2M1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M1_SHELL
                CASE(K_M2M2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M2_SHELL
                CASE(K_M2M3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M3_SHELL
                CASE(K_M2M4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M4_SHELL
                CASE(K_M2M5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M5_SHELL
                CASE(K_M2N1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N1_SHELL
                CASE(K_M2N2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N2_SHELL
                CASE(K_M2N3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N3_SHELL
                CASE(K_M2N4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N4_SHELL
                CASE(K_M2N5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N5_SHELL
                CASE(K_M2N6_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N6_SHELL
                CASE(K_M2N7_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N7_SHELL
                CASE(K_M2O1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O1_SHELL
                CASE(K_M2O2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O2_SHELL
                CASE(K_M2O3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O3_SHELL
                CASE(K_M2O4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O4_SHELL
                CASE(K_M2O5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O5_SHELL
                CASE(K_M2O6_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O6_SHELL
                CASE(K_M2O7_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O7_SHELL
                CASE(K_M2P1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P1_SHELL
                CASE(K_M2P2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P2_SHELL
                CASE(K_M2P3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P3_SHELL
                CASE(K_M2P4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P4_SHELL
                CASE(K_M2P5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P5_SHELL
                CASE(K_M2Q1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q1_SHELL
                CASE(K_M2Q2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q2_SHELL
                CASE(K_M2Q3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q3_SHELL
                CASE(K_M3L1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = L1_SHELL
                CASE(K_M3L2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = L2_SHELL
                CASE(K_M3L3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = L3_SHELL
                CASE(K_M3M1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M1_SHELL
                CASE(K_M3M2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M2_SHELL
                CASE(K_M3M3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M3_SHELL
                CASE(K_M3M4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M4_SHELL
                CASE(K_M3M5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M5_SHELL
                CASE(K_M3N1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N1_SHELL
                CASE(K_M3N2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N2_SHELL
                CASE(K_M3N3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N3_SHELL
                CASE(K_M3N4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N4_SHELL
                CASE(K_M3N5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N5_SHELL
                CASE(K_M3N6_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N6_SHELL
                CASE(K_M3N7_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N7_SHELL
                CASE(K_M3O1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O1_SHELL
                CASE(K_M3O2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O2_SHELL
                CASE(K_M3O3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O3_SHELL
                CASE(K_M3O4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O4_SHELL
                CASE(K_M3O5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O5_SHELL
                CASE(K_M3O6_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O6_SHELL
                CASE(K_M3O7_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O7_SHELL
                CASE(K_M3P1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P1_SHELL
                CASE(K_M3P2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P2_SHELL
                CASE(K_M3P3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P3_SHELL
                CASE(K_M3P4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P4_SHELL
                CASE(K_M3P5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P5_SHELL
                CASE(K_M3Q1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q1_SHELL
                CASE(K_M3Q2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q2_SHELL
                CASE(K_M3Q3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q3_SHELL
                CASE(K_M4L1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = L1_SHELL
                CASE(K_M4L2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = L2_SHELL
                CASE(K_M4L3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = L3_SHELL
                CASE(K_M4M1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M1_SHELL
                CASE(K_M4M2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M2_SHELL
                CASE(K_M4M3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M3_SHELL
                CASE(K_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE(K_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE(K_M4N1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N1_SHELL
                CASE(K_M4N2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N2_SHELL
                CASE(K_M4N3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N3_SHELL
                CASE(K_M4N4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N4_SHELL
                CASE(K_M4N5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N5_SHELL
                CASE(K_M4N6_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N6_SHELL
                CASE(K_M4N7_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N7_SHELL
                CASE(K_M4O1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O1_SHELL
                CASE(K_M4O2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O2_SHELL
                CASE(K_M4O3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O3_SHELL
                CASE(K_M4O4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O4_SHELL
                CASE(K_M4O5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O5_SHELL
                CASE(K_M4O6_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O6_SHELL
                CASE(K_M4O7_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O7_SHELL
                CASE(K_M4P1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P1_SHELL
                CASE(K_M4P2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P2_SHELL
                CASE(K_M4P3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P3_SHELL
                CASE(K_M4P4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P4_SHELL
                CASE(K_M4P5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P5_SHELL
                CASE(K_M4Q1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q1_SHELL
                CASE(K_M4Q2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q2_SHELL
                CASE(K_M4Q3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q3_SHELL
                CASE(K_M5L1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = L1_SHELL
                CASE(K_M5L2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = L2_SHELL
                CASE(K_M5L3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = L3_SHELL
                CASE(K_M5M1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M1_SHELL
                CASE(K_M5M2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M2_SHELL
                CASE(K_M5M3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M3_SHELL
                CASE(K_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE(K_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE(K_M5N1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N1_SHELL
                CASE(K_M5N2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N2_SHELL
                CASE(K_M5N3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N3_SHELL
                CASE(K_M5N4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N4_SHELL
                CASE(K_M5N5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N5_SHELL
                CASE(K_M5N6_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N6_SHELL
                CASE(K_M5N7_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N7_SHELL
                CASE(K_M5O1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O1_SHELL
                CASE(K_M5O2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O2_SHELL
                CASE(K_M5O3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O3_SHELL
                CASE(K_M5O4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O4_SHELL
                CASE(K_M5O5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O5_SHELL
                CASE(K_M5O6_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O6_SHELL
                CASE(K_M5O7_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O7_SHELL
                CASE(K_M5P1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P1_SHELL
                CASE(K_M5P2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P2_SHELL
                CASE(K_M5P3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P3_SHELL
                CASE(K_M5P4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P4_SHELL
                CASE(K_M5P5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P5_SHELL
                CASE(K_M5Q1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q1_SHELL
                CASE(K_M5Q2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q2_SHELL
                CASE(K_M5Q3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L1_M1M1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M1_SHELL
                CASE(L1_M1M2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M2_SHELL
                CASE(L1_M1M3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M3_SHELL
                CASE(L1_M1M4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M4_SHELL
                CASE(L1_M1M5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M5_SHELL
                CASE(L1_M1N1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N1_SHELL
                CASE(L1_M1N2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N2_SHELL
                CASE(L1_M1N3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N3_SHELL
                CASE(L1_M1N4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N4_SHELL
                CASE(L1_M1N5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N5_SHELL
                CASE(L1_M1N6_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N6_SHELL
                CASE(L1_M1N7_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N7_SHELL
                CASE(L1_M1O1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O1_SHELL
                CASE(L1_M1O2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O2_SHELL
                CASE(L1_M1O3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O3_SHELL
                CASE(L1_M1O4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O4_SHELL
                CASE(L1_M1O5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O5_SHELL
                CASE(L1_M1O6_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O6_SHELL
                CASE(L1_M1O7_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O7_SHELL
                CASE(L1_M1P1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P1_SHELL
                CASE(L1_M1P2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P2_SHELL
                CASE(L1_M1P3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P3_SHELL
                CASE(L1_M1P4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P4_SHELL
                CASE(L1_M1P5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P5_SHELL
                CASE(L1_M1Q1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L1_M1Q2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L1_M1Q3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L1_M2M1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M1_SHELL
                CASE(L1_M2M2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M2_SHELL
                CASE(L1_M2M3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M3_SHELL
                CASE(L1_M2M4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M4_SHELL
                CASE(L1_M2M5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M5_SHELL
                CASE(L1_M2N1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N1_SHELL
                CASE(L1_M2N2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N2_SHELL
                CASE(L1_M2N3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N3_SHELL
                CASE(L1_M2N4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N4_SHELL
                CASE(L1_M2N5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N5_SHELL
                CASE(L1_M2N6_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N6_SHELL
                CASE(L1_M2N7_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N7_SHELL
                CASE(L1_M2O1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O1_SHELL
                CASE(L1_M2O2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O2_SHELL
                CASE(L1_M2O3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O3_SHELL
                CASE(L1_M2O4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O4_SHELL
                CASE(L1_M2O5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O5_SHELL
                CASE(L1_M2O6_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O6_SHELL
                CASE(L1_M2O7_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O7_SHELL
                CASE(L1_M2P1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P1_SHELL
                CASE(L1_M2P2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P2_SHELL
                CASE(L1_M2P3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P3_SHELL
                CASE(L1_M2P4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P4_SHELL
                CASE(L1_M2P5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P5_SHELL
                CASE(L1_M2Q1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L1_M2Q2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L1_M2Q3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L1_M3M1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M1_SHELL
                CASE(L1_M3M2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M2_SHELL
                CASE(L1_M3M3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M3_SHELL
                CASE(L1_M3M4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M4_SHELL
                CASE(L1_M3M5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M5_SHELL
                CASE(L1_M3N1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N1_SHELL
                CASE(L1_M3N2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N2_SHELL
                CASE(L1_M3N3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N3_SHELL
                CASE(L1_M3N4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N4_SHELL
                CASE(L1_M3N5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N5_SHELL
                CASE(L1_M3N6_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N6_SHELL
                CASE(L1_M3N7_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N7_SHELL
                CASE(L1_M3O1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O1_SHELL
                CASE(L1_M3O2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O2_SHELL
                CASE(L1_M3O3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O3_SHELL
                CASE(L1_M3O4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O4_SHELL
                CASE(L1_M3O5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O5_SHELL
                CASE(L1_M3O6_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O6_SHELL
                CASE(L1_M3O7_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O7_SHELL
                CASE(L1_M3P1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P1_SHELL
                CASE(L1_M3P2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P2_SHELL
                CASE(L1_M3P3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P3_SHELL
                CASE(L1_M3P4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P4_SHELL
                CASE(L1_M3P5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P5_SHELL
                CASE(L1_M3Q1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L1_M3Q2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L1_M3Q3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L1_M4M1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M1_SHELL
                CASE(L1_M4M2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M2_SHELL
                CASE(L1_M4M3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M3_SHELL
                CASE(L1_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE(L1_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE(L1_M4N1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N1_SHELL
                CASE(L1_M4N2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N2_SHELL
                CASE(L1_M4N3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N3_SHELL
                CASE(L1_M4N4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N4_SHELL
                CASE(L1_M4N5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N5_SHELL
                CASE(L1_M4N6_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N6_SHELL
                CASE(L1_M4N7_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N7_SHELL
                CASE(L1_M4O1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O1_SHELL
                CASE(L1_M4O2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O2_SHELL
                CASE(L1_M4O3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O3_SHELL
                CASE(L1_M4O4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O4_SHELL
                CASE(L1_M4O5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O5_SHELL
                CASE(L1_M4O6_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O6_SHELL
                CASE(L1_M4O7_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O7_SHELL
                CASE(L1_M4P1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P1_SHELL
                CASE(L1_M4P2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P2_SHELL
                CASE(L1_M4P3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P3_SHELL
                CASE(L1_M4P4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P4_SHELL
                CASE(L1_M4P5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P5_SHELL
                CASE(L1_M4Q1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L1_M4Q2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L1_M4Q3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L1_M5M1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M1_SHELL
                CASE(L1_M5M2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M2_SHELL
                CASE(L1_M5M3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M3_SHELL
                CASE(L1_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE(L1_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE(L1_M5N1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N1_SHELL
                CASE(L1_M5N2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N2_SHELL
                CASE(L1_M5N3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N3_SHELL
                CASE(L1_M5N4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N4_SHELL
                CASE(L1_M5N5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N5_SHELL
                CASE(L1_M5N6_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N6_SHELL
                CASE(L1_M5N7_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N7_SHELL
                CASE(L1_M5O1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O1_SHELL
                CASE(L1_M5O2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O2_SHELL
                CASE(L1_M5O3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O3_SHELL
                CASE(L1_M5O4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O4_SHELL
                CASE(L1_M5O5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O5_SHELL
                CASE(L1_M5O6_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O6_SHELL
                CASE(L1_M5O7_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O7_SHELL
                CASE(L1_M5P1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P1_SHELL
                CASE(L1_M5P2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P2_SHELL
                CASE(L1_M5P3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P3_SHELL
                CASE(L1_M5P4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P4_SHELL
                CASE(L1_M5P5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P5_SHELL
                CASE(L1_M5Q1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L1_M5Q2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L1_M5Q3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L2_M1M1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M1_SHELL
                CASE(L2_M1M2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M2_SHELL
                CASE(L2_M1M3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M3_SHELL
                CASE(L2_M1M4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M4_SHELL
                CASE(L2_M1M5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M5_SHELL
                CASE(L2_M1N1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N1_SHELL
                CASE(L2_M1N2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N2_SHELL
                CASE(L2_M1N3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N3_SHELL
                CASE(L2_M1N4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N4_SHELL
                CASE(L2_M1N5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N5_SHELL
                CASE(L2_M1N6_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N6_SHELL
                CASE(L2_M1N7_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N7_SHELL
                CASE(L2_M1O1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O1_SHELL
                CASE(L2_M1O2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O2_SHELL
                CASE(L2_M1O3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O3_SHELL
                CASE(L2_M1O4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O4_SHELL
                CASE(L2_M1O5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O5_SHELL
                CASE(L2_M1O6_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O6_SHELL
                CASE(L2_M1O7_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O7_SHELL
                CASE(L2_M1P1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P1_SHELL
                CASE(L2_M1P2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P2_SHELL
                CASE(L2_M1P3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P3_SHELL
                CASE(L2_M1P4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P4_SHELL
                CASE(L2_M1P5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P5_SHELL
                CASE(L2_M1Q1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L2_M1Q2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L2_M1Q3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L2_M2M1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M1_SHELL
                CASE(L2_M2M2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M2_SHELL
                CASE(L2_M2M3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M3_SHELL
                CASE(L2_M2M4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M4_SHELL
                CASE(L2_M2M5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M5_SHELL
                CASE(L2_M2N1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N1_SHELL
                CASE(L2_M2N2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N2_SHELL
                CASE(L2_M2N3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N3_SHELL
                CASE(L2_M2N4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N4_SHELL
                CASE(L2_M2N5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N5_SHELL
                CASE(L2_M2N6_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N6_SHELL
                CASE(L2_M2N7_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N7_SHELL
                CASE(L2_M2O1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O1_SHELL
                CASE(L2_M2O2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O2_SHELL
                CASE(L2_M2O3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O3_SHELL
                CASE(L2_M2O4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O4_SHELL
                CASE(L2_M2O5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O5_SHELL
                CASE(L2_M2O6_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O6_SHELL
                CASE(L2_M2O7_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O7_SHELL
                CASE(L2_M2P1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P1_SHELL
                CASE(L2_M2P2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P2_SHELL
                CASE(L2_M2P3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P3_SHELL
                CASE(L2_M2P4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P4_SHELL
                CASE(L2_M2P5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P5_SHELL
                CASE(L2_M2Q1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L2_M2Q2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L2_M2Q3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L2_M3M1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M1_SHELL
                CASE(L2_M3M2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M2_SHELL
                CASE(L2_M3M3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M3_SHELL
                CASE(L2_M3M4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M4_SHELL
                CASE(L2_M3M5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M5_SHELL
                CASE(L2_M3N1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N1_SHELL
                CASE(L2_M3N2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N2_SHELL
                CASE(L2_M3N3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N3_SHELL
                CASE(L2_M3N4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N4_SHELL
                CASE(L2_M3N5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N5_SHELL
                CASE(L2_M3N6_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N6_SHELL
                CASE(L2_M3N7_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N7_SHELL
                CASE(L2_M3O1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O1_SHELL
                CASE(L2_M3O2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O2_SHELL
                CASE(L2_M3O3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O3_SHELL
                CASE(L2_M3O4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O4_SHELL
                CASE(L2_M3O5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O5_SHELL
                CASE(L2_M3O6_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O6_SHELL
                CASE(L2_M3O7_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O7_SHELL
                CASE(L2_M3P1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P1_SHELL
                CASE(L2_M3P2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P2_SHELL
                CASE(L2_M3P3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P3_SHELL
                CASE(L2_M3P4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P4_SHELL
                CASE(L2_M3P5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P5_SHELL
                CASE(L2_M3Q1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L2_M3Q2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L2_M3Q3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L2_M4M1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M1_SHELL
                CASE(L2_M4M2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M2_SHELL
                CASE(L2_M4M3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M3_SHELL
                CASE(L2_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE(L2_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE(L2_M4N1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N1_SHELL
                CASE(L2_M4N2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N2_SHELL
                CASE(L2_M4N3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N3_SHELL
                CASE(L2_M4N4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N4_SHELL
                CASE(L2_M4N5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N5_SHELL
                CASE(L2_M4N6_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N6_SHELL
                CASE(L2_M4N7_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N7_SHELL
                CASE(L2_M4O1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O1_SHELL
                CASE(L2_M4O2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O2_SHELL
                CASE(L2_M4O3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O3_SHELL
                CASE(L2_M4O4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O4_SHELL
                CASE(L2_M4O5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O5_SHELL
                CASE(L2_M4O6_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O6_SHELL
                CASE(L2_M4O7_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O7_SHELL
                CASE(L2_M4P1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P1_SHELL
                CASE(L2_M4P2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P2_SHELL
                CASE(L2_M4P3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P3_SHELL
                CASE(L2_M4P4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P4_SHELL
                CASE(L2_M4P5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P5_SHELL
                CASE(L2_M4Q1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L2_M4Q2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L2_M4Q3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L2_M5M1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M1_SHELL
                CASE(L2_M5M2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M2_SHELL
                CASE(L2_M5M3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M3_SHELL
                CASE(L2_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE(L2_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE(L2_M5N1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N1_SHELL
                CASE(L2_M5N2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N2_SHELL
                CASE(L2_M5N3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N3_SHELL
                CASE(L2_M5N4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N4_SHELL
                CASE(L2_M5N5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N5_SHELL
                CASE(L2_M5N6_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N6_SHELL
                CASE(L2_M5N7_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N7_SHELL
                CASE(L2_M5O1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O1_SHELL
                CASE(L2_M5O2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O2_SHELL
                CASE(L2_M5O3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O3_SHELL
                CASE(L2_M5O4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O4_SHELL
                CASE(L2_M5O5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O5_SHELL
                CASE(L2_M5O6_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O6_SHELL
                CASE(L2_M5O7_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O7_SHELL
                CASE(L2_M5P1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P1_SHELL
                CASE(L2_M5P2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P2_SHELL
                CASE(L2_M5P3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P3_SHELL
                CASE(L2_M5P4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P4_SHELL
                CASE(L2_M5P5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P5_SHELL
                CASE(L2_M5Q1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L2_M5Q2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L2_M5Q3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L3_M1M1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M1_SHELL
                CASE(L3_M1M2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M2_SHELL
                CASE(L3_M1M3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M3_SHELL
                CASE(L3_M1M4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M4_SHELL
                CASE(L3_M1M5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M5_SHELL
                CASE(L3_M1N1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N1_SHELL
                CASE(L3_M1N2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N2_SHELL
                CASE(L3_M1N3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N3_SHELL
                CASE(L3_M1N4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N4_SHELL
                CASE(L3_M1N5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N5_SHELL
                CASE(L3_M1N6_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N6_SHELL
                CASE(L3_M1N7_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = N7_SHELL
                CASE(L3_M1O1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O1_SHELL
                CASE(L3_M1O2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O2_SHELL
                CASE(L3_M1O3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O3_SHELL
                CASE(L3_M1O4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O4_SHELL
                CASE(L3_M1O5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O5_SHELL
                CASE(L3_M1O6_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O6_SHELL
                CASE(L3_M1O7_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = O7_SHELL
                CASE(L3_M1P1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P1_SHELL
                CASE(L3_M1P2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P2_SHELL
                CASE(L3_M1P3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P3_SHELL
                CASE(L3_M1P4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P4_SHELL
                CASE(L3_M1P5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = P5_SHELL
                CASE(L3_M1Q1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L3_M1Q2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L3_M1Q3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L3_M2M1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M1_SHELL
                CASE(L3_M2M2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M2_SHELL
                CASE(L3_M2M3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M3_SHELL
                CASE(L3_M2M4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M4_SHELL
                CASE(L3_M2M5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M5_SHELL
                CASE(L3_M2N1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N1_SHELL
                CASE(L3_M2N2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N2_SHELL
                CASE(L3_M2N3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N3_SHELL
                CASE(L3_M2N4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N4_SHELL
                CASE(L3_M2N5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N5_SHELL
                CASE(L3_M2N6_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N6_SHELL
                CASE(L3_M2N7_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = N7_SHELL
                CASE(L3_M2O1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O1_SHELL
                CASE(L3_M2O2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O2_SHELL
                CASE(L3_M2O3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O3_SHELL
                CASE(L3_M2O4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O4_SHELL
                CASE(L3_M2O5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O5_SHELL
                CASE(L3_M2O6_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O6_SHELL
                CASE(L3_M2O7_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = O7_SHELL
                CASE(L3_M2P1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P1_SHELL
                CASE(L3_M2P2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P2_SHELL
                CASE(L3_M2P3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P3_SHELL
                CASE(L3_M2P4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P4_SHELL
                CASE(L3_M2P5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = P5_SHELL
                CASE(L3_M2Q1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L3_M2Q2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L3_M2Q3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L3_M3M1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M1_SHELL
                CASE(L3_M3M2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M2_SHELL
                CASE(L3_M3M3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M3_SHELL
                CASE(L3_M3M4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M4_SHELL
                CASE(L3_M3M5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M5_SHELL
                CASE(L3_M3N1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N1_SHELL
                CASE(L3_M3N2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N2_SHELL
                CASE(L3_M3N3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N3_SHELL
                CASE(L3_M3N4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N4_SHELL
                CASE(L3_M3N5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N5_SHELL
                CASE(L3_M3N6_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N6_SHELL
                CASE(L3_M3N7_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = N7_SHELL
                CASE(L3_M3O1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O1_SHELL
                CASE(L3_M3O2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O2_SHELL
                CASE(L3_M3O3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O3_SHELL
                CASE(L3_M3O4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O4_SHELL
                CASE(L3_M3O5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O5_SHELL
                CASE(L3_M3O6_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O6_SHELL
                CASE(L3_M3O7_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = O7_SHELL
                CASE(L3_M3P1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P1_SHELL
                CASE(L3_M3P2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P2_SHELL
                CASE(L3_M3P3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P3_SHELL
                CASE(L3_M3P4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P4_SHELL
                CASE(L3_M3P5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = P5_SHELL
                CASE(L3_M3Q1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L3_M3Q2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L3_M3Q3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L3_M4M1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M1_SHELL
                CASE(L3_M4M2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M2_SHELL
                CASE(L3_M4M3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M3_SHELL
                CASE(L3_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE(L3_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE(L3_M4N1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N1_SHELL
                CASE(L3_M4N2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N2_SHELL
                CASE(L3_M4N3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N3_SHELL
                CASE(L3_M4N4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N4_SHELL
                CASE(L3_M4N5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N5_SHELL
                CASE(L3_M4N6_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N6_SHELL
                CASE(L3_M4N7_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = N7_SHELL
                CASE(L3_M4O1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O1_SHELL
                CASE(L3_M4O2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O2_SHELL
                CASE(L3_M4O3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O3_SHELL
                CASE(L3_M4O4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O4_SHELL
                CASE(L3_M4O5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O5_SHELL
                CASE(L3_M4O6_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O6_SHELL
                CASE(L3_M4O7_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = O7_SHELL
                CASE(L3_M4P1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P1_SHELL
                CASE(L3_M4P2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P2_SHELL
                CASE(L3_M4P3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P3_SHELL
                CASE(L3_M4P4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P4_SHELL
                CASE(L3_M4P5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = P5_SHELL
                CASE(L3_M4Q1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L3_M4Q2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L3_M4Q3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = Q3_SHELL
                CASE(L3_M5M1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M1_SHELL
                CASE(L3_M5M2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M2_SHELL
                CASE(L3_M5M3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M3_SHELL
                CASE(L3_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE(L3_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE(L3_M5N1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N1_SHELL
                CASE(L3_M5N2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N2_SHELL
                CASE(L3_M5N3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N3_SHELL
                CASE(L3_M5N4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N4_SHELL
                CASE(L3_M5N5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N5_SHELL
                CASE(L3_M5N6_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N6_SHELL
                CASE(L3_M5N7_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = N7_SHELL
                CASE(L3_M5O1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O1_SHELL
                CASE(L3_M5O2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O2_SHELL
                CASE(L3_M5O3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O3_SHELL
                CASE(L3_M5O4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O4_SHELL
                CASE(L3_M5O5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O5_SHELL
                CASE(L3_M5O6_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O6_SHELL
                CASE(L3_M5O7_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = O7_SHELL
                CASE(L3_M5P1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P1_SHELL
                CASE(L3_M5P2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P2_SHELL
                CASE(L3_M5P3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P3_SHELL
                CASE(L3_M5P4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P4_SHELL
                CASE(L3_M5P5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = P5_SHELL
                CASE(L3_M5Q1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q1_SHELL
                CASE(L3_M5Q2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = Q2_SHELL
                CASE(L3_M5Q3_AUGER)
			shell_new1 = M5_SHELL
			shell_new2 = Q3_SHELL
        ENDSELECT

        !so now we have the shells involved...
        !start with the first photon (parent)...
        !but allocate the offspring already...
        ALLOCATE(photon%offspring)
        ALLOCATE(photon%offspring%mus(inputF%composition%n_layers))
        photon%offspring%options = photon%options
        ALLOCATE(photon%offspring%history(inputF%general%n_interactions_trajectory,2))
        photon%offspring%history=photon%history
        photon%offspring%current_layer = photon%current_layer
        photon%offspring%weight = photon%weight
        photon%offspring%coords = photon%coords
        photon%offspring%detector_hit = .FALSE.
        photon%offspring%detector_hit2 = .FALSE.
        photon%offspring%coords = photon%coords
        photon%offspring%n_interactions=photon%n_interactions
        photon%offspring%current_element = photon%current_element
        photon%offspring%current_element_index = photon%current_element_index
        photon%offspring%precalc_mu_cs => photon%precalc_mu_cs




        IF (.NOT.(shell_new1 .GE. M1_SHELL .AND. shell_new1 .LE. M5_SHELL&
            .AND. photon%options%use_M_lines == 0) .AND.&
            xmi_fluorescence_yield_check(rng, shell_new1,&
            inputF%composition%layers&
            (photon%current_layer)%xmi_hdf5_Z_local&
            (photon%current_element_index)%Ptr,&
            energy) .EQ. 1_C_INT) THEN
                !Coster Kronig check
                CALL xmi_coster_kronig_check(rng, shell_new1, photon%current_element)

                !so now that we determined the shell to be used, see if we get
                !fluorescence...
                IF (xmi_fluorescence_line_check(rng, shell_new1, photon%current_element,&
                photon%energy,line_new) .EQ. 0_C_INT) photon%energy = 0.0_C_DOUBLE

                !leave if energy is too low
                IF (photon%energy .LE. energy_threshold) THEN
                        photon%energy = 0.0_C_DOUBLE 
                ELSE
                        photon%energy_changed = .FALSE.
                        photon%mus = xmi_mu_calc(inputF%composition,photon%energy)
                        photon%detector_hit = .FALSE.
                        photon%detector_hit2 = .FALSE.
                        photon%options%use_cascade_radiative = 0_C_INT
                        photon%options%use_cascade_auger = 0_C_INT
                        photon%theta = ACOS(2.0_C_DOUBLE*fgsl_rng_uniform(rng)-1.0_C_DOUBLE)
                        photon%phi = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)
                        photon%dirv(1) = SIN(photon%theta)*COS(photon%phi)
                        photon%dirv(2) = SIN(photon%theta)*SIN(photon%phi)
                        photon%dirv(3) = COS(photon%theta)
                        photon%history(photon%n_interactions,1) = line_new
                        photon%history(photon%n_interactions,2) =&
                        photon%current_element
                        r = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)
                        photon%elecv(1) = COS(r)
                        photon%elecv(2) = SIN(r)
                        photon%elecv(3) = 0.0_C_DOUBLE 
                        photon%last_interaction = PHOTOELECTRIC_INTERACTION
                        cosalfa = DOT_PRODUCT(photon%elecv, photon%dirv)

                        IF (ABS(cosalfa) .GT. 1.0) THEN
                                WRITE (error_unit,'(A)') 'cosalfa exception detected'
                                CALL EXIT(1)
                        ENDIF

                        c_alfa = ACOS(cosalfa)
                        c_ae = 1.0/SIN(c_alfa)
                        c_be = -c_ae*cosalfa

                        photon%elecv = c_ae*photon%elecv +&
                        c_be*photon%dirv

                        ! do not simulate it
                        !IF (xmi_simulate_photon(photon, inputF, hdf5F,rng) == 0) THEN
                        !        CALL EXIT(1)
                        !ENDIF
                ENDIF

        ENDIF
        !second photon will be considered as the offspring
        IF (.NOT.(shell_new2 .GE. M1_SHELL .AND. shell_new2 .LE. M5_SHELL&
            .AND. photon%offspring%options%use_M_lines == 0) .AND.&
            xmi_fluorescence_yield_check(rng, shell_new2,&
            inputF%composition%layers&
            (photon%offspring%current_layer)%xmi_hdf5_Z_local&
            (photon%offspring%current_element_index)%Ptr,&
            energy) .EQ. 1_C_INT) THEN
                !Coster Kronig check
                CALL xmi_coster_kronig_check(rng, shell_new2, photon%offspring%current_element)

                !so now that we determined the shell to be used, see if we get
                !fluorescence...
                IF (xmi_fluorescence_line_check(rng, shell_new2, photon%offspring%current_element,&
                photon%offspring%energy,line_new) .EQ. 0_C_INT)&
                photon%offspring%energy = 0.0_C_DOUBLE

                !leave if energy is too low
                IF (photon%offspring%energy .LE. energy_threshold) THEN
                        !deallocate photon
                        DEALLOCATE(photon%offspring%history)
                        DEALLOCATE(photon%offspring%mus)
                        DEALLOCATE(photon%offspring)
                        !next line may be redundant
                        NULLIFY(photon%offspring)
                ELSE
                        !IF (photon%offspring%options%use_variance_reduction .EQ. 1) THEN
                        !        ALLOCATE(photon%offspring%variance_reduction(inputF%composition%n_layers,&
                        !        inputF%general%n_interactions_trajectory))
                        !        DO k=1,inputF%composition%n_layers
                        !           DO &
                        !           l=1,inputF%general%n_interactions_trajectory
                        !                ALLOCATE(photon%offspring%variance_reduction(k,l)%&
                        !                weight(inputF%composition%layers(k)%n_elements,383+1+1))
                        !                ALLOCATE(photon%offspring%variance_reduction(k,l)%&
                        !                energy(inputF%composition%layers(k)%n_elements,383+1+1))
                        !                photon%offspring%variance_reduction(k,l)%weight =&
                        !                0.0_C_DOUBLE
                        !                photon%offspring%variance_reduction(k,l)%energy =&
                        !                0.0_C_DOUBLE
                        !          ENDDO
                        !        ENDDO
                        !ENDIF
                        photon%offspring%energy_changed = .FALSE.
                        photon%offspring%mus = xmi_mu_calc(inputF%composition,photon%offspring%energy)
                        photon%offspring%options%use_cascade_auger = 0_C_INT
                        photon%offspring%options%use_cascade_radiative = 0_C_INT
                        photon%offspring%options%use_variance_reduction = 0_C_INT
                        photon%offspring%theta = ACOS(2.0_C_DOUBLE*fgsl_rng_uniform(rng)-1.0_C_DOUBLE)
                        photon%offspring%phi = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)
                        photon%offspring%dirv(1) = SIN(photon%offspring%theta)*COS(photon%offspring%phi)
                        photon%offspring%dirv(2) = SIN(photon%offspring%theta)*SIN(photon%offspring%phi)
                        photon%offspring%dirv(3) = COS(photon%offspring%theta)
                        photon%offspring%history(photon%offspring%n_interactions,1) = line_new
                        photon%offspring%history(photon%offspring%n_interactions,2) =&
                        photon%offspring%current_element
                        r = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)
                        photon%offspring%elecv(1) = COS(r)
                        photon%offspring%elecv(2) = SIN(r)
                        photon%offspring%elecv(3) = 0.0_C_DOUBLE 
                        photon%offspring%precalc_mu_cs => photon%precalc_mu_cs
                        cosalfa = DOT_PRODUCT(photon%offspring%elecv, photon%offspring%dirv)

                        IF (ABS(cosalfa) .GT. 1.0) THEN
                                WRITE (error_unit,'(A)') 'cosalfa exception detected'
                                CALL EXIT(1)
                        ENDIF

                        c_alfa = ACOS(cosalfa)
                        c_ae = 1.0/SIN(c_alfa)
                        c_be = -c_ae*cosalfa

                        photon%offspring%elecv = c_ae*photon%offspring%elecv +&
                        c_be*photon%offspring%dirv

                        NULLIFY(photon%offspring%offspring)

                        !simulate offspring
                        IF (xmi_simulate_photon(photon%offspring, inputF, hdf5F,rng) == 0) THEN
                                CALL EXIT(1)
                        ENDIF
                ENDIF
        ELSE
                !deallocate photon
                DEALLOCATE(photon%offspring%history)
                DEALLOCATE(photon%offspring%mus)
                DEALLOCATE(photon%offspring)
                !next line may be redundant
                NULLIFY(photon%offspring)
        ENDIF
        RETURN
ENDSUBROUTINE xmi_simulate_photon_cascade_auger

SUBROUTINE xmi_simulate_photon_cascade_radiative(photon, shell, line,rng,inputF,hdf5F)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        INTEGER (C_INT), INTENT(IN) :: shell, line
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF
        INTEGER (C_INT) :: shell_new, line_new
        TYPE (fgsl_rng), INTENT(IN) :: rng
        REAL (C_DOUBLE) :: energy,r,cosalfa,c_alfa,c_ae,c_be
        
        !
        !
        !       Calculate the cascade based on radiative transitions
        !
        !
        !
        !       Default is to only consider the cascade L-lines.
        !       IF the use_M_lines option is on, then the cascade M-lines will
        !       also be considered
        !
        !       However: the energy of the line needs to be higher than the
        !       energy_threshold for the photon to be used
        !
        !

        IF (shell .EQ. K_SHELL) THEN
                SELECT CASE (line)
                        CASE (KL1_LINE)
                                shell_new = L1_SHELL
                        CASE (KL2_LINE)
                                shell_new = L2_SHELL
                        CASE (KL3_LINE)
                                shell_new = L3_SHELL
                        CASE (KM1_LINE)
                                shell_new = M1_SHELL
                        CASE (KM2_LINE)
                                shell_new = M2_SHELL
                        CASE (KM3_LINE)
                                shell_new = M3_SHELL
                        CASE (KM4_LINE)
                                shell_new = M4_SHELL
                        CASE (KM5_LINE)
                                shell_new = M5_SHELL
                        CASE DEFAULT
                                shell_new = -1_C_INT
                ENDSELECT
                


        ELSEIF ((shell .EQ. L1_SHELL .OR. shell .EQ. L2_SHELL .OR. shell .EQ.&
        L3_SHELL) .AND. photon%options%use_M_lines .EQ. 1_C_INT) THEN
                SELECT CASE (line)
                        CASE (L1M1_LINE)
                                shell_new = M1_SHELL
                        CASE (L1M2_LINE)
                                shell_new = M2_SHELL
                        CASE (L1M3_LINE)
                                shell_new = M3_SHELL
                        CASE (L1M4_LINE)
                                shell_new = M4_SHELL
                        CASE (L1M5_LINE)
                                shell_new = M5_SHELL
                        CASE (L2M1_LINE)
                                shell_new = M1_SHELL
                        CASE (L2M2_LINE)
                                shell_new = M2_SHELL
                        CASE (L2M3_LINE)
                                shell_new = M3_SHELL
                        CASE (L2M4_LINE)
                                shell_new = M4_SHELL
                        CASE (L2M5_LINE)
                                shell_new = M5_SHELL
                        CASE (L3M1_LINE)
                                shell_new = M1_SHELL
                        CASE (L3M2_LINE)
                                shell_new = M2_SHELL
                        CASE (L3M3_LINE)
                                shell_new = M3_SHELL
                        CASE (L3M4_LINE)
                                shell_new = M4_SHELL
                        CASE (L3M5_LINE)
                                shell_new = M5_SHELL
                        CASE DEFAULT
                                shell_new = -1_C_INT
                ENDSELECT 
        ELSE
                !nothing to do... probably an M-line
                RETURN
        ENDIF
        
        IF (shell_new .EQ. -1_C_INT) RETURN
        !exit if an M shell was found while not allowed
        IF (shell_new .GE. M1_SHELL .AND. shell_new .LE. M5_SHELL .AND.&
        photon%options%use_M_lines .EQ. 0) RETURN

        !first fluorescence yield check
        IF (xmi_fluorescence_yield_check(rng, shell_new,&
            inputF%composition%layers&
            (photon%current_layer)%xmi_hdf5_Z_local&
            (photon%current_element_index)%Ptr,&
            energy) .EQ. 0_C_INT) RETURN

        !Coster Kronig check
        CALL xmi_coster_kronig_check(rng, shell_new, photon%current_element)

        !so now that we determined the shell to be used, see if we get
        !fluorescence...
        IF (xmi_fluorescence_line_check(rng, shell_new, photon%current_element,&
        energy,line_new) .EQ. 0_C_INT) RETURN

        !leave if energy is too low
        IF (energy .LE. energy_threshold) RETURN
        
        !create offspring
        ALLOCATE(photon%offspring)
        !take over its history!
        ALLOCATE(photon%offspring%history(inputF%general%n_interactions_trajectory,2))
        photon%offspring%history=photon%history
        photon%offspring%energy = energy
        photon%offspring%energy_changed = .FALSE.
        ALLOCATE(photon%offspring%mus(inputF%composition%n_layers))
        photon%offspring%mus = xmi_mu_calc(inputF%composition,energy)
        photon%offspring%current_layer = photon%current_layer
        photon%offspring%detector_hit = .FALSE.
        photon%offspring%detector_hit2 = .FALSE.
        photon%offspring%options = photon%options
        photon%options%use_cascade_auger = 0_C_INT
        photon%options%use_cascade_radiative = 0_C_INT
        photon%offspring%options%use_cascade_auger = 0_C_INT
        photon%offspring%options%use_cascade_radiative = 0_C_INT
        photon%offspring%options%use_variance_reduction = 0_C_INT
        photon%offspring%weight = photon%weight
        photon%offspring%coords = photon%coords
        photon%offspring%theta = ACOS(2.0_C_DOUBLE*fgsl_rng_uniform(rng)-1.0_C_DOUBLE)
        !photon%offspring%theta = M_PI *fgsl_rng_uniform(rng)
        photon%offspring%phi = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)
        !photon%offspring%dirv(1) = COS(photon%offspring%phi)
        !photon%offspring%dirv(2) = SIN(photon%offspring%phi)
        !photon%offspring%dirv(3) = COS(photon%offspring%theta)
        photon%offspring%dirv(1) = SIN(photon%offspring%theta)*COS(photon%offspring%phi)
        photon%offspring%dirv(2) = SIN(photon%offspring%theta)*SIN(photon%offspring%phi)
        photon%offspring%dirv(3) = COS(photon%offspring%theta)
        !force photon to detector
        !CALL xmi_force_photon_to_detector(photon%offspring, inputF, rng)

        !CALL normalize_vector(photon%offspring%dirv)
        photon%offspring%n_interactions=photon%n_interactions
        photon%offspring%history(photon%n_interactions,1) = line_new
        photon%offspring%history(photon%n_interactions,2) =&
        photon%current_element
        r = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)
        photon%offspring%elecv(1) = COS(r)
        photon%offspring%elecv(2) = SIN(r)
        photon%offspring%elecv(3) = 0.0_C_DOUBLE 
        photon%offspring%last_interaction = PHOTOELECTRIC_INTERACTION
        photon%offspring%current_element = photon%current_element
        photon%offspring%current_element_index = photon%current_element_index
        photon%offspring%precalc_mu_cs => photon%precalc_mu_cs

#if DEBUG == 1
        WRITE (6,'(A,I4)') 'line_new: ',line_new
#endif


        cosalfa = DOT_PRODUCT(photon%offspring%elecv, photon%offspring%dirv)

        IF (ABS(cosalfa) .GT. 1.0) THEN
                WRITE (error_unit,'(A)') 'cosalfa exception detected'
                CALL EXIT(1)
        ENDIF

        c_alfa = ACOS(cosalfa)
        c_ae = 1.0/SIN(c_alfa)
        c_be = -c_ae*cosalfa

        photon%offspring%elecv = c_ae*photon%offspring%elecv +&
        c_be*photon%offspring%dirv

        NULLIFY(photon%offspring%offspring)

        !simulate offspring
        IF (xmi_simulate_photon(photon%offspring, inputF, hdf5F,rng) == 0) THEN
                CALL EXIT(1)
        ENDIF

        RETURN
ENDSUBROUTINE xmi_simulate_photon_cascade_radiative

SUBROUTINE xmi_update_photon_energy_compton(photon, theta_i, rng, inputF, hdf5F) 
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        REAL (C_DOUBLE), INTENT(IN) :: theta_i
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF

        REAL (C_DOUBLE) :: K0K,pz,r
        INTEGER (C_INT) :: pos
        REAL (C_DOUBLE), PARAMETER :: c = 1.2399E-6
        REAL (C_DOUBLE), PARAMETER :: c0 = 4.85E-12
        REAL (C_DOUBLE), PARAMETER :: c1 = 1.456E-2
        REAL (C_DOUBLE) :: c_lamb0, dlamb, c_lamb
        REAL (C_DOUBLE) :: energy, sth2
        INTEGER (C_INT) :: np

!        ASSOCIATE (hdf5_Z => inputF%composition%layers&
!                (photon%current_layer)%xmi_hdf5_Z_local&
!                (photon%current_element_index)%Ptr)
 
#define hdf5_Z inputF%composition%layers(photon%current_layer)%xmi_hdf5_Z_local(photon%current_element_index)%Ptr

        !K0K = 1.0_C_DOUBLE + (1.0_C_DOUBLE-COS(theta_i))*photon%energy/XMI_MEC2
        
        !convert to eV
        energy = photon%energy*1000.0_C_DOUBLE
        c_lamb0 = c/(energy)

        sth2 = SIN(theta_i/2.0_C_DOUBLE)

        DO
                r = fgsl_rng_uniform(rng)
                pos = findpos(hdf5_Z%&
                RandomNumbers, r)

                pz = interpolate_simple([&
                hdf5_Z%&
                RandomNumbers(pos),&
                hdf5_Z%&
                DopplerPz_ICDF(pos)]&
                ,[hdf5_Z%&
                RandomNumbers(pos+1),&
                hdf5_Z%&
                DopplerPz_ICDF(pos+1)], r)

!                np = INT(r*(SIZE(hdf5_Z%RandomNumbers)-1))+1
!                pz = hdf5_Z%DopplerPz_ICDF(np)+(hdf5_Z%DopplerPz_ICDF(np+1)-hdf5_Z%DopplerPz_ICDF(np))*SIZE(hdf5_Z%RandomNumbers)*(r - REAL(np)/REAL(SIZE(hdf5_Z%RandomNumbers)))

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
                IF (energy .LE. photon%energy ) EXIT
        ENDDO

        !photon%energy = &
        !photon%energy/(K0K-2.0_C_DOUBLE*pz*SIN(theta_i/2.0_C_DOUBLE)*XMI_MOM_MEC)

        photon%energy = energy

#if DEBUG == 2
        WRITE (*,'(A,F12.5)') 'new photon energy: ',photon%energy
#endif
        photon%energy_changed = .FALSE.
        photon%mus = xmi_mu_calc(inputF%composition,&
        photon%energy)


!        ENDASSOCIATE
#undef hdf5_Z

        RETURN
ENDSUBROUTINE xmi_update_photon_energy_compton

SUBROUTINE xmi_update_photon_dirv(photon, theta_i, phi_i)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        REAL (C_DOUBLE), INTENT(IN) :: theta_i, phi_i

        REAL (C_DOUBLE) :: cosphi_i, sinphi_i
        REAL (C_DOUBLE) :: costheta_i, sintheta_i
        REAL (C_DOUBLE), DIMENSION(3,3) :: trans_m
        REAL (C_DOUBLE), DIMENSION(3) :: dirv
        REAL (C_DOUBLE) :: tempsin
        REAL (C_DOUBLE) :: phi_i_new

        !stability problems could arise here...
        !
        !Warning formula in 1993 paper is wrong!!
        !For actual formula consult the PhD thesis of Laszlo Vincze
        !

        IF (phi_i .GT. 2.0_C_DOUBLE*M_PI) THEN
                phi_i_new = phi_i-2.0_C_DOUBLE*M_PI
        ELSEIF (phi_i .LT. 0.0_C_DOUBLE*M_PI) THEN
                phi_i_new = phi_i+2.0_C_DOUBLE*M_PI
        ELSE
                phi_i_new = phi_i
        ENDIF



        cosphi_i = COS(photon%phi)
        sinphi_i = SIN(photon%phi)
        costheta_i = COS(photon%theta)
        sintheta_i = SIN(photon%theta)

        trans_m(1,1) = costheta_i*cosphi_i
        trans_m(1,2) = -sinphi_i
        trans_m(1,3) = sintheta_i*cosphi_i

        trans_m(2,1) = costheta_i*sinphi_i
        trans_m(2,2) = cosphi_i
        trans_m(2,3) = sintheta_i*sinphi_i

        trans_m(3,1) = -sintheta_i
        trans_m(3,2) = 0.0_C_DOUBLE
        trans_m(3,3) = costheta_i

        tempsin = SIN(theta_i)
        dirv = [tempsin*COS(phi_i_new), tempsin*SIN(phi_i_new),COS(theta_i)]

        photon%dirv = MATMUL(trans_m,dirv)

        CALL normalize_vector(photon%dirv)

        !update theta and phi in photon
        photon%theta = ACOS(photon%dirv(3))

        IF (photon%dirv(1) .EQ. 0.0_C_DOUBLE) THEN
                !watch out... if photon%dirv(2) EQ 0.0 then result may be
                !processor dependent...
                photon%phi = SIGN(M_PI_2, photon%dirv(2))
        ELSE
                photon%phi = ATAN(photon%dirv(2)/photon%dirv(1))
        ENDIF

!        photon%phi = ATAN2(photon%dirv(2),photon%dirv(1))

        photon%theta_i = theta_i
        photon%phi_i = phi_i_new

        RETURN
ENDSUBROUTINE xmi_update_photon_dirv

SUBROUTINE xmi_update_photon_elecv(photon)
#if DEBUG == 1
        USE, INTRINSIC :: ieee_exceptions
#endif
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
       
        REAL (C_DOUBLE) :: cosalfa, c_alfa, sinalfa,c_ae, c_be

#if DEBUG == 1
        LOGICAL, DIMENSION(3) :: flag_value

        CALL ieee_set_flag(ieee_usual,.FALSE.)
#endif
        cosalfa = DOT_PRODUCT(photon%dirv,photon%elecv)

        c_alfa = ACOS(cosalfa)
#if DEBUG == 1
        CALL ieee_get_flag(ieee_usual, flag_value)
        IF (ANY(flag_value)) THEN
                WRITE (error_unit,'(A)') &
                'xmi_update_photon_elecv FPE'
                STOP
        ENDIF
#endif
        sinalfa = SIN(c_alfa)
        c_ae = 1.0_C_DOUBLE/sinalfa
        c_be = -c_ae*cosalfa

        photon%elecv = c_ae * photon%elecv + c_be *photon%dirv

ENDSUBROUTINE xmi_update_photon_elecv

SUBROUTINE xmi_coster_kronig_check(rng, shell, element)
        IMPLICIT NONE
        TYPE (fgsl_rng), INTENT(IN) :: rng
        INTEGER(C_INT), INTENT(INOUT) :: shell
        INTEGER(C_INT), INTENT(IN) :: element
        LOGICAL :: trans_found
        REAL (C_DOUBLE) :: r, sumz
        INTEGER (C_INT) :: trans

        IF (shell .EQ. L2_SHELL .OR. shell .EQ. L1_SHELL ) THEN
                DO
                        sumz = 0.0_C_DOUBLE
                        trans_found = .FALSE.
                        r = fgsl_rng_uniform(rng)
                        IF (shell .EQ. L2_SHELL) THEN
                                IF (r .LT. &
                                CosKronTransProb(&
                                element, FL23_TRANS)) THEN
                                        shell = L3_SHELL
                                ENDIF
                                EXIT
                        ELSE IF (shell .EQ. L1_SHELL) THEN
                                DO trans=FL12_TRANS,FL13_TRANS
                                        sumz = sumz+CosKronTransProb(&
                                        element, trans)
                                        IF (r .LT. sumz) THEN
                                                trans_found = .TRUE.           
                                                EXIT
                                        ENDIF
                                ENDDO
                                IF (trans_found) THEN
                                        SELECT CASE (trans)
                                                CASE (FL12_TRANS)
                                                        shell = L2_SHELL
                                                        CYCLE
                                                CASE (FL13_TRANS)
                                                        shell = L3_SHELL
                                                        EXIT
                                        ENDSELECT
                                ELSE
                                        !nothing happened...exiting
                                        EXIT
                                ENDIF
                        ENDIF
                ENDDO
        ELSEIF (shell .EQ. M1_SHELL .OR. shell .EQ. M2_SHELL .OR.&
                shell .EQ. M3_SHELL .OR. shell .EQ. M4_SHELL) THEN
                DO
                        sumz = 0.0_C_DOUBLE
                        trans_found = .FALSE.
                        r = fgsl_rng_uniform(rng)
                        IF (shell .EQ. M4_SHELL) THEN
                                IF (r .LT. &
                                CosKronTransProb(&
                                element, FM45_TRANS)) THEN
                                        shell = M5_SHELL
                                ENDIF
                                EXIT
                        ELSEIF (shell .EQ. M3_SHELL) THEN
                                DO trans=FM34_TRANS,FM35_TRANS
                                        sumz = sumz+CosKronTransProb(&
                                        element, trans)
                                        IF (r .LT. sumz) THEN
                                                trans_found = .TRUE.           
                                                EXIT
                                        ENDIF
                                ENDDO
                                IF (trans_found) THEN
                                        SELECT CASE (trans)
                                                CASE (FM34_TRANS)
                                                        shell = M4_SHELL
                                                        CYCLE
                                                CASE (FM35_TRANS)
                                                        shell = M5_SHELL
                                                        EXIT
                                        ENDSELECT
                                ELSE
                                        !nothing happened...exiting
                                        EXIT
                                ENDIF
                        ELSEIF (shell .EQ. M2_SHELL) THEN
                                DO trans=FM23_TRANS,FM25_TRANS
                                        sumz = sumz+CosKronTransProb(&
                                        element, trans)
                                        IF (r .LT. sumz) THEN
                                                trans_found = .TRUE.           
                                                EXIT
                                        ENDIF
                                ENDDO
                                IF (trans_found) THEN
                                        SELECT CASE (trans)
                                                CASE (FM23_TRANS)
                                                        shell = M3_SHELL
                                                        CYCLE
                                                CASE (FM24_TRANS)
                                                        shell = M4_SHELL
                                                        CYCLE
                                                CASE (FM25_TRANS)
                                                        shell = M5_SHELL
                                                        EXIT
                                        ENDSELECT
                                ELSE
                                        !nothing happened...exiting
                                        EXIT
                                ENDIF
                        ELSEIF (shell .EQ. M1_SHELL) THEN
                                DO trans=FM12_TRANS,FM15_TRANS
                                        sumz = sumz+CosKronTransProb(&
                                        element, trans)
                                        IF (r .LT. sumz) THEN
                                                trans_found = .TRUE.           
                                                EXIT
                                        ENDIF
                                ENDDO
                                IF (trans_found) THEN
                                        SELECT CASE (trans)
                                                CASE (FM12_TRANS)
                                                        shell = M2_SHELL
                                                        CYCLE
                                                CASE (FM13_TRANS)
                                                        shell = M3_SHELL
                                                        CYCLE
                                                CASE (FM14_TRANS)
                                                        shell = M4_SHELL
                                                        CYCLE
                                                CASE (FM15_TRANS)
                                                        shell = M5_SHELL
                                                        EXIT
                                        ENDSELECT
                                ELSE
                                        !nothing happened...exiting
                                        EXIT
                                ENDIF
                        ENDIF
                ENDDO
        ENDIF


        RETURN
ENDSUBROUTINE xmi_coster_kronig_check

FUNCTION xmi_fluorescence_yield_check(rng, shell, hdf5_Z, energy) RESULT(rv)
        IMPLICIT NONE
        TYPE (fgsl_rng), INTENT(IN) :: rng
        INTEGER (C_INT), INTENT(IN) :: shell
        TYPE (xmi_hdf5_Z), POINTER :: hdf5_Z
        REAL (C_DOUBLE), INTENT(INOUT) :: energy
        INTEGER (C_INT) :: rv
        REAL (C_DOUBLE) :: r,fluor_yield_corr

        rv = 0

        r = fgsl_rng_uniform(rng)
#if DEBUG == 1
        WRITE (*,'(A,F12.4)') 'FluorYield random number: ',r
#endif

        IF (r .GT. hdf5_Z%FluorYieldsCorr(shell)) THEN
                !no fluorescence but Auger...
                energy = 0.0_C_DOUBLE
                RETURN
        ENDIF

        rv = 1

        RETURN
ENDFUNCTION xmi_fluorescence_yield_check

FUNCTION xmi_fluorescence_line_check(rng, shell, element, energy, line_rv&
        ) RESULT(rv)
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN) :: shell, element
        TYPE (fgsl_rng), INTENT(IN) :: rng
        REAL (C_DOUBLE), INTENT(INOUT) :: energy
        INTEGER (C_INT), INTENT(INOUT) :: line_rv
        INTEGER (C_INT) :: rv
        REAL (C_DOUBLE) :: r, sumz
        LOGICAL :: line_found
        INTEGER (C_INT) :: line_first, line_last, line

        rv = 0


        !so we have fluorescence... but which line?
        r = fgsl_rng_uniform(rng)
        sumz = 0.0_C_DOUBLE
        line_found = .FALSE.
        IF (shell .EQ. K_SHELL) THEN
                line_first = KL1_LINE
                line_last = KP5_LINE
        ELSEIF (shell .EQ. L1_SHELL) THEN
                line_first = L1L2_LINE
                line_last = L1P5_LINE
        ELSEIF (shell .EQ. L2_SHELL) THEN
                line_first = L2L3_LINE
                line_last = L2Q1_LINE
        ELSEIF (shell .EQ. L3_SHELL) THEN
                line_first = L3M1_LINE
                line_last = L3Q1_LINE
        ELSEIF (shell .EQ. M1_SHELL) THEN
                line_first = M1N1_LINE 
                line_last = M1P5_LINE
        ELSEIF (shell .EQ. M2_SHELL) THEN
                line_first = M2N1_LINE 
                line_last = M2P5_LINE
        ELSEIF (shell .EQ. M3_SHELL) THEN
                line_first = M3N1_LINE 
                line_last = M3Q1_LINE
        ELSEIF (shell .EQ. M4_SHELL) THEN
                line_first = M4N1_LINE 
                line_last = M4P5_LINE
        ELSEIF (shell .EQ. M5_SHELL) THEN
                line_first = M5N1_LINE 
                line_last = M5P5_LINE
        ENDIF

        DO line=line_first,line_last,-1
                sumz = sumz + RadRate(element, line)
                IF (r .LT. sumz) THEN
                        !found it...
                        line_found = .TRUE.
                        EXIT
                ENDIF
        ENDDO

        IF (line_found) THEN
                energy = LineEnergy(element, line)
        ELSE
                !this should not happen since the radiative rates within one
                !linegroup must add up to 1.0
                energy = 0.0_C_DOUBLE
                RETURN
        ENDIF

#if DEBUG == 1
        WRITE (*,'(A,I2)') 'Line found: ',line
#endif
       line_rv = line

        rv = 1


        RETURN
ENDFUNCTION xmi_fluorescence_line_check

SUBROUTINE xmi_force_photon_to_detector(photon, inputF, rng)
        IMPLICIT NONE
        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (fgsl_rng), INTENT(IN) :: rng
        REAL (C_DOUBLE) :: radius, theta
        REAL (C_DOUBLE), DIMENSION(3) :: detector_point


        !pick a spot on the detector surface
        radius = fgsl_rng_uniform(rng)*inputF%detector%detector_radius
        theta = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)

        detector_point(1) = 0.0_C_DOUBLE
        detector_point(2) = COS(theta)*radius
        detector_point(3) = SIN(theta)*radius

        !WRITE (*,'(A,F12.5)') 'radius: ', norm(detector_point)
        detector_point = MATMUL(inputF%detector%n_detector_orientation_new,detector_point)+inputF%geometry%p_detector_window        

#if DEBUG == 1
!        WRITE (*,'(A,3F12.5)') 'detector_point: ',detector_point
!        CALL EXIT(1)
#endif

        photon%dirv = photon%coords-detector_point
        CALL normalize_vector(photon%dirv)

        IF (DOT_PRODUCT(photon%dirv, inputF%geometry%n_detector_orientation)&
        .GE. 0.0_C_DOUBLE) photon%dirv = photon%dirv*(-1_C_DOUBLE)

        RETURN
ENDSUBROUTINE xmi_force_photon_to_detector

SUBROUTINE xmi_escape_ratios_calculation(inputFPtr, hdf5FPtr, escape_ratiosPtr,&
input_string,input_options) BIND(C,NAME='xmi_escape_ratios_calculation_fortran')
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN), VALUE :: inputFPtr, hdf5FPtr
        TYPE (C_PTR), INTENT(INOUT) :: escape_ratiosPtr
        TYPE (C_PTR), VALUE, INTENT(IN) :: input_string


        TYPE (xmi_hdf5), POINTER :: hdf5F
        TYPE (xmi_input), POINTER :: inputF
        TYPE (xmi_main_options), VALUE, INTENT(IN) :: input_options
        TYPE (xmi_main_options) :: options
        TYPE (xmi_escape_ratiosC), POINTER :: escape_ratios
        INTEGER (C_INT) :: xmi_cascade_type
        TYPE (xmi_precalc_mu_cs), DIMENSION(:), ALLOCATABLE, TARGET ::&
        precalc_mu_cs
        INTEGER :: max_threads, thread_num

        TYPE (fgsl_rng_type) :: rng_type
        TYPE (fgsl_rng) :: rng
        INTEGER (C_LONG), ALLOCATABLE, TARGET, DIMENSION(:) :: seeds
        INTEGER (C_LONG) :: i,j,k,l,m,n
        TYPE (xmi_photon), POINTER :: photon
        INTEGER, PARAMETER :: maxz = 94
        INTEGER (C_LONG), PARAMETER :: n_input_energies = 990
        INTEGER (C_LONG), PARAMETER :: n_compton_output_energies = 999
        INTEGER (C_LONG), PARAMETER :: n_photons = 1000000
        !REAL (C_DOUBLE), ALLOCATABLE, TARGET, SAVE, DIMENSION(:) :: &
        REAL (C_DOUBLE), POINTER, DIMENSION(:) :: &
        input_energies, compton_escape_output_energies
        !REAL (C_DOUBLE), ALLOCATABLE, TARGET, SAVE, DIMENSION(:,:,:) :: &
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:,:) :: &
        fluo_escape_ratios
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: &
        !REAL (C_DOUBLE), ALLOCATABLE, TARGET, SAVE, DIMENSION(:,:) :: &
        compton_escape_ratios
        INTEGER (C_INT), POINTER, DIMENSION(:) :: &
        !INTEGER (C_INT), ALLOCATABLE, TARGET, SAVE, DIMENSION(:) :: &
        Z
        REAL (C_DOUBLE) :: theta_elecv
        REAL (C_DOUBLE) :: photons_simulated, photons_no_interaction,&
        photons_rayleigh, photons_compton, photons_einstein,photons_interacted
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: initial_mus
        INTEGER (C_INT) :: element,line
        INTEGER (C_INT) :: compton_index
        TYPE (xmi_energy_discrete) :: energy_disc
        TYPE (xmi_energy_continuous) :: energy_cont
        REAL (C_DOUBLE) :: cosalfa, c_alfa, c_ae, c_be
        INTEGER (C_INT64_T) :: n_photons_sim,n_photons_tot 

        !WRITE (6,'(A)') 'Precalculating escape ratios'
        !WRITE (6,'(A)') 'This could take a long time...'

        !associate c pointers
        CALL C_F_POINTER(inputFPtr, inputF)
        CALL C_F_POINTER(hdf5FPtr, hdf5F) 

        ALLOCATE(input_energies(n_input_energies))
        ALLOCATE(compton_escape_output_energies(n_compton_output_energies))
        DO i=0,n_input_energies-1
                input_energies(i+1) = 1.0+i*0.1
        ENDDO

        DO i=0,n_compton_output_energies-1
                compton_escape_output_energies(i+1) = 0.1+i*0.1
        ENDDO

        ALLOCATE(Z(SIZE(hdf5F%xmi_hdf5_Zs))) 
        Z = hdf5F%xmi_hdf5_Zs(:)%Z
        ALLOCATE(fluo_escape_ratios(SIZE(Z),ABS(L3P3_LINE),n_input_energies))
        ALLOCATE(compton_escape_ratios(n_input_energies,&
        n_compton_output_energies))

        fluo_escape_ratios = 0.0_C_DOUBLE
        compton_escape_ratios = 0.0_C_DOUBLE

        !set options
        options%use_M_lines = 0
        options%use_cascade_auger = 0
        options%use_cascade_radiative = 0
        options%use_variance_reduction = 0
        options%use_optimizations = 0
        options%use_sum_peaks = 0
        options%escape_ratios_mode = 1

        xmi_cascade_type = XMI_CASCADE_NONE


        max_threads = input_options%omp_num_threads

        ALLOCATE(seeds(max_threads))

        !fetch some seeds
        IF (xmi_get_random_numbers(C_LOC(seeds), INT(max_threads,KIND=C_LONG)) == 0) RETURN



        !
        !
        !       Precalculate the absorption coefficients of the XRF photons
        !
        !
        ALLOCATE(precalc_mu_cs(inputF%composition%n_layers))
        DO k=1,inputF%composition%n_layers
                ALLOCATE(precalc_mu_cs(k)%mu(maxz,ABS(M5P5_LINE)))
                DO l=1,maxz
                        DO m=KL1_LINE,M5P5_LINE,-1
                               precalc_mu_cs(k)%mu(l,ABS(m))=xmi_mu_calc(inputF%composition%layers(k),&
                               REAL(LineEnergy(INT(l,C_INT),INT(m,C_INT)),C_DOUBLE)) 
                        ENDDO
                ENDDO

        ENDDO


        !initialize random number generators
        rng_type = fgsl_rng_mt19937


        !allocate the escape ratio arrays...

        n_photons_sim = 0_C_INT64_T
        n_photons_tot = n_input_energies*n_photons/input_options%omp_num_threads
        n_photons_tot = n_photons_tot/100_C_INT64_T
        n_photons_tot = n_photons_tot*100_C_INT64_T



!$omp parallel default(shared) private(rng, thread_num,j,k,l,photon, theta_elecv,&
!$omp initial_mus,photons_simulated, photons_no_interaction,&
!$omp photons_rayleigh, photons_compton,energy_disc,&
!$omp cosalfa, c_alfa, c_ae, c_be,&
!$omp photons_einstein,photons_interacted,element,compton_index,line)&
!$omp num_threads(input_options%omp_num_threads)



        thread_num = omp_get_thread_num()

        rng = fgsl_rng_alloc(rng_type)
        CALL fgsl_rng_set(rng,seeds(thread_num+1))

!$omp do schedule(dynamic)
        DO i=1,n_input_energies
                energy_disc%energy = input_energies(i)
                energy_disc%sigma_x = 0.0
                energy_disc%sigma_xp = 0.0
                energy_disc%sigma_y = 0.0
                energy_disc%sigma_yp = 0.0
                !Calculate initial mu's
                ALLOCATE(initial_mus(inputF%composition%n_layers))
                initial_mus = xmi_mu_calc(inputF%composition,&
                input_energies(i))

                photons_simulated = 0.0_C_DOUBLE
                photons_no_interaction= 0.0_C_DOUBLE
                photons_rayleigh = 0.0_C_DOUBLE
                photons_compton= 0.0_C_DOUBLE
                photons_einstein= 0.0_C_DOUBLE
                photons_interacted= 0.0_C_DOUBLE

                DO j=1,n_photons
                        !Allocate the photon
                        ALLOCATE(photon)
                        ALLOCATE(photon%history(inputF%general%n_interactions_trajectory,2))
                        photon%history(1,1)=NO_INTERACTION
                        photon%last_interaction=NO_INTERACTION
                        photon%n_interactions=0
                        NULLIFY(photon%offspring)
                        !Calculate energy with rng
                        photon%energy = input_energies(i) 
                        photon%energy_changed=.FALSE.
                        ALLOCATE(photon%mus(inputF%composition%n_layers))
                        photon%mus = initial_mus
                        photon%current_layer = 1
                        photon%detector_hit = .FALSE.
                        photon%detector_hit2 = .FALSE.
                        photon%options = options
                        photon%xmi_cascade_type = xmi_cascade_type
                        photon%precalc_mu_cs => precalc_mu_cs

                        CALL xmi_coords_dir(rng,energy_disc, inputF%geometry,&
                        photon)
                
                        photon%weight = 1.0
                        theta_elecv = fgsl_rng_uniform(rng)*M_PI*2.0_C_DOUBLE
                        photon%elecv(1) = COS(theta_elecv)
                        photon%elecv(2) = SIN(theta_elecv)
                        photon%elecv(3) = 0.0_C_DOUBLE 

                        cosalfa = DOT_PRODUCT(photon%elecv, photon%dirv)

                        IF (ABS(cosalfa) .GT. 1.0) THEN
                                WRITE (error_unit,'(A)') 'cosalfa exception detected'
                                CALL EXIT(1)
                        ENDIF

                        c_alfa = ACOS(cosalfa)
                        c_ae = 1.0/SIN(c_alfa)
                        c_be = -c_ae*cosalfa

                        photon%elecv = c_ae*photon%elecv + c_be*photon%dirv
                        CALL &
                        xmi_photon_shift_first_layer(photon,inputF%composition,inputF%geometry)


                        IF (xmi_simulate_photon(photon, inputF, hdf5F,rng) == 0) THEN
                                CALL EXIT(1)
                        ENDIF

                        photons_simulated = photons_simulated + photon%weight
                        IF (photon%last_interaction .EQ. NO_INTERACTION) THEN
                                photons_no_interaction = &
                                photons_no_interaction + photon%weight
                        ELSE
                                photons_interacted = &
                                photons_interacted + photon%weight
                        ENDIF
                        !if photon has left the system and has interacted at
                        !least once, let's look at its energy
                        !and the last interaction type
                        IF (photon%inside .EQV. .FALSE. .AND.&
                        photon%last_interaction .NE. NO_INTERACTION) THEN
                                SELECT CASE (photon%last_interaction)
                                        CASE (RAYLEIGH_INTERACTION)
                                                photons_rayleigh = &
                                                photons_rayleigh + photon%weight 
                                        CASE (COMPTON_INTERACTION)
                                                photons_compton = &
                                                photons_compton + photon%weight  
                                                compton_index=INT((photon%energy-0.1)/0.1)
                                                IF (compton_index .GE. 1 .AND.&
                                                compton_index .LE. &
                                                n_compton_output_energies)&
                                                compton_escape_ratios(i,compton_index)=&
                                                compton_escape_ratios(i,compton_index)+&
                                                photon%weight
                                        CASE (PHOTOELECTRIC_INTERACTION)
                                                photons_einstein= &
                                                photons_einstein + photon%weight  
                                                k=photon%n_interactions
                                                element =&
                                                inputF%composition%layers&
                                                (photon%current_layer)%&
                                                xmi_hdf5_Z_local&
                                                (photon%current_element_index)&
                                                %Ptr%Zindex
                                                line=&
                                                ABS(photon%history(k,1))
                                                IF (photon%history(k,1) .LE. KL1_LINE .AND.&
                                                photon%history(k,1) .GE. L3P3_LINE) THEN
                                                        fluo_escape_ratios(element,line,i) = &
                                                        fluo_escape_ratios(element,line,i) + &
                                                        photon%weight
                                                ENDIF
                                ENDSELECT

                        ENDIF

                        !deallocate photon
                        DEALLOCATE(photon%history)
                        DEALLOCATE(photon%mus)
                        DEALLOCATE(photon)

                        IF (omp_get_thread_num() == 0) THEN
                        n_photons_sim = n_photons_sim+1_C_INT64_T
                        IF(n_photons_sim*100_C_INT64_T/n_photons_tot == &
                        REAL(n_photons_sim*100_C_INT64_T,KIND=C_DOUBLE)&
                        /REAL(n_photons_tot,KIND=C_DOUBLE).AND.&
                        input_options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
                        CALL xmi_print_progress('Escape peak ratios calculation at'&
                        //C_NULL_CHAR,INT(n_photons_sim*100_C_INT64_T/n_photons_tot,KIND=C_INT))
#else
                        WRITE(output_unit,'(A,I3,A)')&
                        'Escape peak ratios calculation at ',&
                        n_photons_sim*100_C_INT64_T/n_photons_tot,' %'
#endif
                        ENDIF
                ENDDO
                fluo_escape_ratios(:,:,i) =&
                fluo_escape_ratios(:,:,i)/photons_interacted
                compton_escape_ratios(i,:)=&
                compton_escape_ratios(i,:)/photons_interacted
                DEALLOCATE(initial_mus)
        ENDDO
!$omp end do
!$omp end parallel




        ALLOCATE(escape_ratios)
        escape_ratios%n_elements = SIZE(Z)
        escape_ratios%n_fluo_input_energies =&
        n_input_energies
        escape_ratios%n_compton_input_energies =&
        n_input_energies
        escape_ratios%n_compton_output_energies =&
        n_compton_output_energies
        escape_ratios%Z=C_LOC(Z(1))
        escape_ratios%fluo_escape_ratios=C_LOC(fluo_escape_ratios(1,1,1))
        escape_ratios%fluo_escape_input_energies=C_LOC(input_energies(1))
        escape_ratios%compton_escape_ratios=C_LOC(compton_escape_ratios(1,1))
        escape_ratios%compton_escape_input_energies=C_LOC(input_energies(1))
        escape_ratios%compton_escape_output_energies=C_LOC(compton_escape_output_energies(1))
        escape_ratios%xmi_input_string = input_string

        escape_ratiosPtr = C_LOC(escape_ratios)

        IF (input_options%verbose == 1_C_INT) THEN
#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
                CALL xmi_print_progress('Escape peak ratios calculation finished'&
                //C_NULL_CHAR,-1_C_INT)
#else
                WRITE (output_unit,'(A)') &
                'Escape peak ratios calculation finished'
#endif
        ENDIF

        RETURN
ENDSUBROUTINE xmi_escape_ratios_calculation

SUBROUTINE xmi_test_brute(inputFPtr) BIND(C,NAME='xmi_test_brute')
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN), VALUE :: inputFPtr
        REAL (C_DOUBLE), DIMENSION(3) :: coords_begin, coords_end
        TYPE (xmi_input), POINTER :: inputF

        CALL C_F_POINTER(inputFPtr, inputF)

        coords_begin = [0.0_C_DOUBLE, 0.0_C_DOUBLE, 100.0_C_DOUBLE]
        coords_end= [0.00001_C_DOUBLE, -2.0_C_DOUBLE, 100.0_C_DOUBLE]

        WRITE (*,'(A,I1)') 'check:',xmi_check_detector_intersection&
        (inputF,coords_begin,coords_end) 

        RETURN
ENDSUBROUTINE xmi_test_brute

ENDMODULE
