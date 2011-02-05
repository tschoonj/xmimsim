MODULE xmimsim_main

USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux
USE :: hdf5
USE :: omp_lib
USE :: fgsl





!global variables
INTEGER (C_INT), PARAMETER ::  NO_INTERACTION = 0
INTEGER (C_INT), PARAMETER ::  RAYLEIGH_INTERACTION = 1
INTEGER (C_INT), PARAMETER ::  COMPTON_INTERACTION = 2
INTEGER (C_INT), PARAMETER ::  PHOTOELECTRIC_INTERACTION = 3
INTEGER (C_INT), PARAMETER ::  XMI_DETECTOR_SILI = 0
INTEGER (C_INT), PARAMETER ::  XMI_DETECTOR_GE = 1
INTEGER (C_INT), PARAMETER ::  XMI_DETECTOR_SI_SDD = 2

!some physical constants
REAL (C_DOUBLE), PARAMETER :: XMI_MEC2 = fgsl_const_mksa_mass_electron*&
        fgsl_const_mksa_speed_of_light**2/&
        fgsl_const_mksa_electron_volt/1000.0_C_DOUBLE
        !source = NIST
REAL (C_DOUBLE), PARAMETER :: momentum_atomic_unit = 1.992851565E-24
REAL (C_DOUBLE), PARAMETER :: XMI_MEC = fgsl_const_mksa_mass_electron*&
        fgsl_const_mksa_speed_of_light
REAL (C_DOUBLE), PARAMETER :: XMI_MOM_MEC = momentum_atomic_unit/XMI_MEC

!threshold is 1 keV
REAL (C_DOUBLE) :: energy_threshold = 1.0_C_DOUBLE


!function pointer for XRF cross section
PROCEDURE (CS_FluorLine_Kissel), POINTER :: xmi_CS_FluorLine



CONTAINS



FUNCTION xmi_init_from_hdf5(xmi_hdf5_file, xmi_inputFPtr, xmi_hdf5FPtr ) &
BIND(C,NAME='xmi_init_from_hdf5') RESULT(rv)
        IMPLICIT NONE

        TYPE (C_PTR), VALUE, INTENT(IN) :: xmi_hdf5_file
        !CHARACTER (KIND=C_CHAR),INTENT(IN), DIMENSION(*) :: xmi_hdf5_file
        TYPE (C_PTR), VALUE, INTENT(IN) :: xmi_inputFPtr
        TYPE (C_PTR), INTENT(INOUT) :: xmi_hdf5FPtr
        INTEGER (C_INT) :: rv
        INTEGER :: error


        CHARACTER (C_CHAR), POINTER, DIMENSION(:) :: xmi_hdf5_fileF
        CHARACTER (KIND=C_CHAR,LEN=:), ALLOCATABLE :: xmi_hdf5_fileFF
        TYPE (xmi_input), POINTER :: xmi_inputF
        TYPE (xmi_hdf5),  POINTER :: xmi_hdf5F
        INTEGER (C_INT), ALLOCATABLE, DIMENSION(:) :: uniqZ
        INTEGER :: i,j,k

        INTEGER (HID_T) :: file_id
        INTEGER (HID_T) :: group_id,group_id2
        INTEGER (HID_T) :: dset_id
        INTEGER (HID_T) :: dspace_id
        INTEGER :: ndims
        INTEGER (HSIZE_T),DIMENSION(:), ALLOCATABLE:: dims,maxdims
        CHARACTER (LEN=2) :: element



#if DEBUG == 1
        WRITE (*,'(A)') 'Entering xmi_init_from_hdf5'
#endif

        !associate pointers C -> Fortran
        CALL C_F_POINTER(xmi_inputFPtr, xmi_inputF)
        CALL C_F_POINTER(xmi_hdf5_file, xmi_hdf5_fileF,[strlen(xmi_hdf5_file)])
        ALLOCATE(CHARACTER(SIZE(xmi_hdf5_fileF)) :: xmi_hdf5_fileFF )
        !xmi_hdf5_fileFF(1:SIZE(xmi_hdf5_fileF)) = xmi_hdf5_fileF(1:SIZE(xmi_hdf5_fileF))
        DO i=1,SIZE(xmi_hdf5_fileF)
                xmi_hdf5_fileFF(i:i) = xmi_hdf5_fileF(i)
        ENDDO

#if DEBUG == 1
        WRITE (*,'(A)') 'hdf5_file: ',xmi_hdf5_fileFF
#endif

        !determine the unique Z and sort them
        ASSOCIATE (layers => xmi_inputF%composition%layers)
        uniqZ = [layers(1)%Z(1)]
        DO i=1,SIZE(layers)
                DO j=1,SIZE(layers(i)%Z) 
                        IF (.NOT. ANY(layers(i)%Z(j) == uniqZ)) uniqZ = &
                        [uniqZ,layers(i)%Z(j)]
                ENDDO
        ENDDO
        ENDASSOCIATE

       CALL qsort(C_LOC(uniqZ),SIZE(uniqZ,KIND=C_SIZE_T),&
       INT(KIND(uniqZ),KIND=C_SIZE_T),C_FUNLOC(C_INT_CMP))

#if DEBUG == 2
        WRITE (*,'(A)') 'uniqZ', uniqZ
#endif
        !DO i=1,SIZE(uniqZ)
        !        WRITE (*,'(A,I2)') 'uniqZ: ',uniqZ(i)
        !ENDDO




        !initialize hdf5 fortran interface
        CALL h5open_f(error)

        !open file for reading
        CALL h5fopen_f(xmi_hdf5_fileFF, H5F_ACC_RDONLY_F, file_id, error)
#if DEBUG == 1
        WRITE (*,'(A,I)') 'error code: ',error
#endif
        IF (error /= 0) THEN
                WRITE (*,'(A,A)') 'Error opening HDF5 file ',xmi_hdf5_fileFF
                rv = 0
                RETURN
        ENDIF

        !allocate xmi_hdf5 structure
        ALLOCATE(xmi_hdf5F)


        !start by reading in the Z independent part...
        !RayleighPhi
        CALL h5gopen_f(file_id,'RayleighPhi',group_id,error)
        CALL h5dopen_f(group_id,'RayleighPhi_ICDF',dset_id,error)
        CALL h5dget_space_f(dset_id, dspace_id,error)
        CALL h5sget_simple_extent_ndims_f(dspace_id, ndims, error)
#if DEBUG == 1
        WRITE (*,'(A,I)') 'ndims: ',ndims
#endif
        !Allocate memory
        ALLOCATE(dims(ndims))
        ALLOCATE(maxdims(ndims))
        CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, error)
#if DEBUG == 1
        WRITE (*,'(A,2I)') 'dims: ',dims
#endif
        !read the dataset
        ALLOCATE(xmi_hdf5F%RayleighPhi_ICDF(dims(1),dims(2)))
        CALL h5dread_f(dset_id,&
        H5T_NATIVE_DOUBLE,xmi_hdf5F%RayleighPhi_ICDF,dims,error)

        CALL h5sclose_f(dspace_id,error)
        CALL h5dclose_f(dset_id,error)
        !Read RayleighThetas and RayleighRandomNumbers
        ALLOCATE(xmi_hdf5F%RayleighThetas(dims(1)))
        CALL h5dopen_f(group_id,'Thetas',dset_id,error)
        CALL h5dread_f(dset_id,&
        H5T_NATIVE_DOUBLE,xmi_hdf5F%RayleighThetas,[dims(1)],error)
        CALL h5dclose_f(dset_id,error)
        ALLOCATE(xmi_hdf5F%RayleighRandomNumbers(dims(2)))
        CALL h5dopen_f(group_id,'Random numbers',dset_id,error)
        CALL h5dread_f(dset_id,&
        H5T_NATIVE_DOUBLE,xmi_hdf5F%RayleighRandomNumbers,[dims(2)],error)
        CALL h5dclose_f(dset_id,error)
        !close group
        CALL h5gclose_f(group_id,error)

        !ComptonPhi
        CALL h5gopen_f(file_id,'ComptonPhi',group_id,error)
        CALL h5dopen_f(group_id,'ComptonPhi_ICDF',dset_id,error)
        CALL h5dget_space_f(dset_id, dspace_id,error)
        CALL h5sget_simple_extent_ndims_f(dspace_id, ndims, error)
#if DEBUG == 1
        WRITE (*,'(A,I)') 'ndims: ',ndims
#endif
        !Allocate memory
        DEALLOCATE(dims)
        DEALLOCATE(maxdims)
        ALLOCATE(dims(ndims))
        ALLOCATE(maxdims(ndims))
        CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, error)
#if DEBUG == 1
        WRITE (*,'(A,3I)') 'dims: ',dims
#endif
        !read the dataset
        ALLOCATE(xmi_hdf5F%ComptonPhi_ICDF(dims(1),dims(2),dims(3)))
        CALL h5dread_f(dset_id,&
        H5T_NATIVE_DOUBLE,xmi_hdf5F%ComptonPhi_ICDF,dims,error)

        CALL h5sclose_f(dspace_id,error)
        CALL h5dclose_f(dset_id,error)
        !Read ComptonThetas and ComptonRandomNumbers
        ALLOCATE(xmi_hdf5F%ComptonThetas(dims(1)))
        CALL h5dopen_f(group_id,'Thetas',dset_id,error)
        CALL h5dread_f(dset_id,&
        H5T_NATIVE_DOUBLE,xmi_hdf5F%ComptonThetas,[dims(1)],error)
        CALL h5dclose_f(dset_id,error)
        ALLOCATE(xmi_hdf5F%ComptonEnergies(dims(2)))
        CALL h5dopen_f(group_id,'Energies',dset_id,error)
        CALL h5dread_f(dset_id,&
        H5T_NATIVE_DOUBLE,xmi_hdf5F%ComptonEnergies,[dims(2)],error)
        CALL h5dclose_f(dset_id,error)
        ALLOCATE(xmi_hdf5F%ComptonRandomNumbers(dims(3)))
        CALL h5dopen_f(group_id,'Random numbers',dset_id,error)
        CALL h5dread_f(dset_id,&
        H5T_NATIVE_DOUBLE,xmi_hdf5F%ComptonRandomNumbers,[dims(3)],error)
        CALL h5dclose_f(dset_id,error)
        !close group
        CALL h5gclose_f(group_id,error)

        DEALLOCATE(dims)
        DEALLOCATE(maxdims)

        !read Z dependent part...
        ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(SIZE(uniqZ)))
        DO i=1,SIZE(uniqZ) 
!
!
!       Internal files issue in intel fortran...
!


!WRITE (*,'(A,I2)') 'Elementi: ',uniqZ(i)
!                WRITE (element, '(I2)') uniqZ(i)
#if DEBUG == 1
                WRITE (*,'(A,A)') 'Reading element: ',elements(uniqZ(i))
#endif
                xmi_hdf5F%xmi_hdf5_Zs(i)%Z = uniqZ(i)

                CALL h5gopen_f(file_id,elements(uniqZ(i)) // '/Theta_ICDF',group_id,error)
                !Read Rayleigh Theta ICDF
                CALL h5dopen_f(group_id,'RayleighTheta_ICDF',dset_id,error)
                CALL h5dget_space_f(dset_id, dspace_id,error)
                CALL h5sget_simple_extent_ndims_f(dspace_id, ndims, error)
                !Allocate memory
                ALLOCATE(dims(ndims))
                ALLOCATE(maxdims(ndims))
                CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, error)
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%RayleighTheta_ICDF(dims(1),dims(2)))
                CALL h5dread_f(dset_id,&
                H5T_NATIVE_DOUBLE,xmi_hdf5F%xmi_hdf5_Zs(i)%RayleighTheta_ICDF,dims,error)

                CALL h5sclose_f(dspace_id,error)
                CALL h5dclose_f(dset_id,error)
                
                !Read Compton Theta ICDF
                CALL h5dopen_f(group_id,'ComptonTheta_ICDF',dset_id,error)
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%ComptonTheta_ICDF(dims(1),dims(2)))
                CALL h5dread_f(dset_id,&
                H5T_NATIVE_DOUBLE,xmi_hdf5F%xmi_hdf5_Zs(i)%ComptonTheta_ICDF,dims,error)
                CALL h5dclose_f(dset_id,error)

                !Read Doppler pz ICDF
                CALL h5dopen_f(group_id,'Doppler_pz_ICDF',dset_id,error)
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%DopplerPz_ICDF(dims(2)))
                CALL h5dread_f(dset_id,&
                H5T_NATIVE_DOUBLE,xmi_hdf5F%xmi_hdf5_Zs(i)%DopplerPz_ICDF,[dims(2)],error)
                CALL h5dclose_f(dset_id,error)

                !Read corrected fluorescence yields
                CALL h5dopen_f(group_id,'Corrected fluorescence yields',dset_id,error)
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%FluorYieldsCorr(K_SHELL:M5_SHELL))
                CALL h5dread_f(dset_id,&
                H5T_NATIVE_DOUBLE,xmi_hdf5F%xmi_hdf5_Zs(i)%FluorYieldsCorr,[INT(M5_SHELL-K_SHELL+1,KIND=HSIZE_T)],error)
                CALL h5dclose_f(dset_id,error)

                !Read energies
                CALL h5dopen_f(group_id,'Energies',dset_id,error)
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%Energies(dims(1)))
                CALL h5dread_f(dset_id,&
                H5T_NATIVE_DOUBLE,xmi_hdf5F%xmi_hdf5_Zs(i)%Energies,dims,error)
                CALL h5dclose_f(dset_id,error)

                !Read random numbers
                CALL h5dopen_f(group_id,'Random numbers',dset_id,error)
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%RandomNumbers(dims(2)))
                CALL h5dread_f(dset_id,&
                H5T_NATIVE_DOUBLE,xmi_hdf5F%xmi_hdf5_Zs(i)%RandomNumbers,dims,error)
                CALL h5dclose_f(dset_id,error)

                DEALLOCATE(dims)
                DEALLOCATE(maxdims)
                CALL h5gclose_f(group_id,error)
                !Read interactions probabilities
                CALL h5gopen_f(file_id,elements(uniqZ(i))// '/Interaction probabilities',group_id,error)
                !Read energies 
                CALL h5dopen_f(group_id,'Energies',dset_id,error)
                CALL h5dget_space_f(dset_id, dspace_id,error)
                CALL h5sget_simple_extent_ndims_f(dspace_id, ndims, error)
                !Allocate memory
                ALLOCATE(dims(ndims))
                ALLOCATE(maxdims(ndims))
                CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, error)
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%energies(dims(1)))
                CALL h5dread_f(dset_id,&
                H5T_NATIVE_DOUBLE,xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%energies,dims,error)
                CALL h5sclose_f(dspace_id,error)
                CALL h5dclose_f(dset_id,error)

                !Read Rayleigh and Compton probabilities
                CALL h5dopen_f(group_id,'Rayleigh and Compton probabilities',dset_id,error)
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%Rayl_and_Compt(dims(1),2))
                CALL h5dread_f(dset_id,&
                H5T_NATIVE_DOUBLE,xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%Rayl_and_Compt,[dims(1),2],error)
                CALL h5dclose_f(dset_id,error)
                CALL h5gclose_f(group_id,error)

                DEALLOCATE(dims)
                DEALLOCATE(maxdims)
                ASSOCIATE (layers => xmi_inputF%composition%layers, &
                n_sample_orientation => xmi_inputF%geometry%n_sample_orientation )
                !create pointers
                DO j=1,SIZE(layers)
                        IF (.NOT. ALLOCATED(layers(j)%xmi_hdf5_Z_local)) & 
                        ALLOCATE(layers(j)%xmi_hdf5_Z_local(layers(j)%n_elements))
                        DO k=1,layers(j)%n_elements 
                                IF (layers(j)%Z(k) == uniqZ(i)) &
                                layers(j)%xmi_hdf5_Z_local(k)%Ptr => xmi_hdf5F%xmi_hdf5_Zs(i)   
                        ENDDO

                ENDDO

                ENDASSOCIATE

        ENDDO

        ASSOCIATE (layers => xmi_inputF%composition%layers, &
        n_sample_orientation => xmi_inputF%geometry%n_sample_orientation )
        DO j=1,SIZE(layers)
                !calculate thickness in Z direction
                layers(j)%thickness_along_Z = &
                layers(j)%thickness/SIN(ATAN(n_sample_orientation(3)/n_sample_orientation(2)))
        ENDDO

        layers(xmi_inputF%composition%reference_layer)%Z_coord_begin =&
        0.0_C_DOUBLE+xmi_inputF%geometry%d_sample_source
        layers(xmi_inputF%composition%reference_layer)%Z_coord_end =&
        layers(xmi_inputF%composition%reference_layer)%thickness_along_Z+&
        xmi_inputF%geometry%d_sample_source

        DO j=xmi_inputF%composition%reference_layer+1,SIZE(layers)
                layers(j)%Z_coord_begin =layers(j-1)%Z_coord_end
                layers(j)%Z_coord_end = layers(j)%Z_coord_begin+layers(j)%thickness_along_Z
        ENDDO
        DO j=xmi_inputF%composition%reference_layer-1,1,-1
                layers(j)%Z_coord_end = layers(j+1)%Z_coord_begin
                layers(j)%Z_coord_begin = layers(j)%Z_coord_end-layers(j)%thickness_along_Z
        ENDDO


        ENDASSOCIATE
        !close file
        CALL h5fclose_f(file_id,error)

        !close hdf5 fortran interface
        CALL h5close_f(error)

#if DEBUG == 1
        ASSOCIATE (layers => xmi_inputF%composition%layers)
        !create pointers
        DO j=1,SIZE(layers)
                DO k=1,SIZE(layers(j)%Z) 
                        WRITE (*,'(A,I)') 'Z confirmation: ',&
                        layers(j)%xmi_hdf5_Z_local(k)%Ptr%Z
                ENDDO
                WRITE (*,'(A,F14.6)') 'thickness: ',layers(j)%thickness
                WRITE (*,'(A,F14.6)') 'thickness_along_Z: ',&
                layers(j)%thickness_along_Z
                WRITE (*,'(A,F14.6)') 'Z_coord_begin: ',layers(j)%Z_coord_begin
                WRITE (*,'(A,F14.6)') 'Z_coord_end: ',layers(j)%Z_coord_end
        ENDDO
        ENDASSOCIATE
#endif


        
        rv=1
        xmi_hdf5FPtr = C_LOC(xmi_hdf5F)
        RETURN

ENDFUNCTION xmi_init_from_hdf5

SUBROUTINE xmi_free_hdf5_F(xmi_hdf5FPtr) BIND(C,NAME='xmi_free_hdf5_F')
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(INOUT) :: xmi_hdf5FPtr
        TYPE (xmi_hdf5), POINTER :: xmi_hdf5F
        INTEGER :: i
        LOGICAL :: alloc

        CALL C_F_POINTER(xmi_hdf5FPtr, xmi_hdf5F)

#if DEBUG == 1
        WRITE (*,'(A)') 'Entering xmi_free_hdf5_F'
#endif

        DEALLOCATE(xmi_hdf5F%RayleighPhi_ICDF)
        DEALLOCATE(xmi_hdf5F%RayleighThetas)
        DEALLOCATE(xmi_hdf5F%RayleighRandomNumbers)
        DEALLOCATE(xmi_hdf5F%ComptonPhi_ICDF)
        DEALLOCATE(xmi_hdf5F%ComptonThetas)
        DEALLOCATE(xmi_hdf5F%ComptonEnergies)
        DEALLOCATE(xmi_hdf5F%ComptonRandomNumbers)

#if DEBUG == 1
        WRITE (*,'(A)') 'Beyond primary deallocates'
#endif
        DO i=1,SIZE(xmi_hdf5F%xmi_hdf5_Zs)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%RayleighTheta_ICDF)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%ComptonTheta_ICDF)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%Energies)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%RandomNumbers)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%energies)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%Rayl_and_Compt)

        ENDDO
        DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs)
        DEALLOCATE(xmi_hdf5F)
        xmi_hdf5FPtr = C_NULL_PTR

ENDSUBROUTINE xmi_free_hdf5_F


FUNCTION xmi_main_msim(inputFPtr, hdf5FPtr, n_mpi_hosts, channelsPtr,&
nchannels, options, historyPtr) BIND(C,NAME='xmi_main_msim') RESULT(rv)
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN), VALUE :: inputFPtr, hdf5FPtr
        INTEGER (C_INT), VALUE, INTENT(IN) :: n_mpi_hosts, nchannels
        INTEGER (C_INT) :: rv 
        TYPE (xmi_main_options), VALUE, INTENT(IN) :: options
        TYPE (C_PTR), INTENT(INOUT) :: historyPtr, channelsPtr

        TYPE (xmi_hdf5), POINTER :: hdf5F
        TYPE (xmi_input), POINTER :: inputF
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: channelsF
        INTEGER :: max_threads, thread_num

        TYPE (fgsl_rng_type) :: rng_type
        TYPE (fgsl_rng) :: rng
        INTEGER (C_LONG), POINTER, DIMENSION(:) :: seeds
        INTEGER (C_LONG) :: i,j,k,l,m,n
        TYPE (xmi_photon), POINTER :: photon,photon_temp,photon_temp2
        REAL (C_DOUBLE) :: hor_ver_ratio
        INTEGER (C_LONG) :: n_photons
        REAL (C_DOUBLE) :: iv_start_energy, iv_end_energy
        INTEGER :: ipol
        REAL (C_DOUBLE) :: cosalfa, c_alfa, c_ae, c_be
        INTEGER (C_LONG) :: photons_simulated, detector_hits, rayleighs,&
        comptons, einsteins,detector_hits2
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: initial_mus
        INTEGER (C_INT) :: channel
        REAL (C_DOUBLE), DIMENSION(:,:), ALLOCATABLE :: channels 
        INTEGER (C_INT), DIMENSION(:,:,:), ALLOCATABLE :: history
        INTEGER (C_INT), DIMENSION(:,:,:), POINTER :: historyF
        INTEGER (C_INT), DIMENSION(K_SHELL:M5_SHELL) :: last_shell
        INTEGER (C_INT) :: element
        REAL (C_DOUBLE) :: exc_corr,det_corr, total_intensity
        !begin...
        
        CALL SetErrorMessages(0)

        rv = 0
        photons_simulated = 0
        detector_hits = 0
        detector_hits2 = 0
        rayleighs = 0
        comptons = 0
        einsteins = 0


        !set the XRF cross sections according to the options
        IF (options%use_cascade_auger .EQ. 0 .AND.&
        options%use_cascade_radiative .EQ.0 ) THEN
                xmi_CS_FluorLine => CS_FluorLine_Kissel_no_cascade
        ELSEIF (options%use_cascade_auger .EQ. 1 .AND.&
        options%use_cascade_radiative .EQ.0 ) THEN
                xmi_CS_FluorLine => CS_FluorLine_Kissel_nonradiative_cascade
        ELSEIF (options%use_cascade_auger .EQ. 0 .AND.&
        options%use_cascade_radiative .EQ.1 ) THEN
                xmi_CS_FluorLine => CS_FluorLine_Kissel_radiative_cascade
        ELSEIF (options%use_cascade_auger .EQ. 1 .AND.&
        options%use_cascade_radiative .EQ.1 ) THEN
#if DEBUG == 0
                WRITE (6,'(A)') 'Full cascade'
#endif
                xmi_CS_FluorLine => CS_FluorLine_Kissel_cascade
        ENDIF



        CALL C_F_POINTER(inputFPtr, inputF)
        CALL C_F_POINTER(hdf5FPtr, hdf5F) 
        !CALL C_F_POINTER(channelsPtr,&
        !channelsF,[0:inputF%general%n_interactions_trajectory,nchannels])


        !channelsF = 0.0_C_DOUBLE
        !channelsFF = channelsF
        
        max_threads = omp_get_max_threads()

#if DEBUG == 1
        WRITE (*,'(A,I)') 'num_threads: ', max_threads
#endif

        ALLOCATE(seeds(max_threads))

        !fetch some seeds
        IF (xmi_get_random_numbers(C_LOC(seeds), INT(max_threads,KIND=C_LONG)) == 0) RETURN

#if DEBUG == 1
        WRITE (*,'(A,4I)') 'seeds: ',seeds
#endif

!
!
!
!  Starting the main OpenMP loop
!
!
!

        ALLOCATE(history(100,383+2,inputF%general%n_interactions_trajectory)) 
        history = 0_C_INT
        last_shell = 0_C_INT

        ALLOCATE(channels(0:inputF%general%n_interactions_trajectory,nchannels))
        channels = 0.0_C_DOUBLE


!$omp parallel default(shared) private(rng,thread_num,i,j,k,l,m,n,photon,photon_temp,photon_temp2,hor_ver_ratio,n_photons,iv_start_energy, iv_end_energy,ipol,cosalfa, c_alfa, c_ae, c_be, initial_mus,channel,element,exc_corr,det_corr,total_intensity) reduction(+:photons_simulated,detector_hits, detector_hits2,channels,rayleighs,comptons,einsteins,history, last_shell)

!
!
!       Initialize random number generator
!
!
        thread_num = omp_get_thread_num()
        rng_type = fgsl_rng_mt19937

        rng = fgsl_rng_alloc(rng_type)
        CALL fgsl_rng_set(rng,seeds(thread_num+1))

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
        cont:DO i=1,exc%n_continuous
                hor_ver_ratio = &
                exc%continuous(i)%horizontal_intensity/ &
                (exc%continuous(i)%vertical_intensity+ &
                exc%continuous(i)%horizontal_intensity)
                n_photons = inputF%general%n_photons_interval/omp_get_num_threads()/n_mpi_hosts
                !Calculate the initial energy -> interval boundaries
                IF (i .EQ. 1) THEN
                        !first interval
                        iv_start_energy = (1.5_C_DOUBLE * &
                        exc%continuous(1)%energy) - 0.5_C_DOUBLE*exc%continuous(2)%energy
                        iv_end_energy = 0.5_C_DOUBLE * (exc%continuous(2)%energy+exc%continuous(1)%energy)
                ELSEIF (i .EQ. exc%n_continuous) THEN
                        iv_start_energy = 0.5_C_DOUBLE * (exc%continuous(i-1)%energy+exc%continuous(i)%energy)
                        !this next line needs verification...
                        iv_end_energy = (1.5_C_DOUBLE * &
                        exc%continuous(i)%energy) - 0.5_C_DOUBLE*exc%continuous(i-1)%energy
                ELSE
                        iv_start_energy = 0.5_C_DOUBLE * (exc%continuous(i-1)%energy+exc%continuous(i)%energy)
                        iv_end_energy = 0.5_C_DOUBLE * (exc%continuous(i)%energy+exc%continuous(i+1)%energy)

                ENDIF

                DO j=1,n_photons
                        !Allocate the photon
                        ALLOCATE(photon)
                        photon%n_interactions=0
                        NULLIFY(photon%offspring)
                        
                        !Calculate energy with rng
                        photon%energy = &
                        fgsl_ran_flat(rng,iv_start_energy,iv_end_energy)

                        !Calculate its initial coordinates and direction
                        CALL xmi_coords_dir(rng,exc%continuous(i), inputF%geometry,&
                        photon)



                        !Calculate the electric field vector
                        !IF (REAL(i,KIND=C_DOUBLE)/REAL(n_photons,KIND=C_DOUBLE) &
                        !.LT. hor_ver_ratio ) THEN
                        !        !horizontal polarization
                        !ELSE
                        !        !vertical polarization
                        !ENDIF





                        DEALLOCATE(photon%mus)
                        DEALLOCATE(photon)
                ENDDO
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
                ALLOCATE(initial_mus(inputF%composition%n_layers))
                initial_mus = xmi_mu_calc(inputF%composition,&
                exc%discrete(i)%energy)

                DO j=1,n_photons
                        !Allocate the photon
                        ALLOCATE(photon)
                        ALLOCATE(photon%history(inputF%general%n_interactions_trajectory,2))
                        IF (options%use_variance_reduction .EQ. 1) THEN
                                ALLOCATE(photon%variance_reduction(inputF%composition%n_layers,&
                                inputF%general%n_interactions_trajectory))
                                DO k=1,inputF%composition%n_layers
                                   DO &
                                   l=1,inputF%general%n_interactions_trajectory
                                        ALLOCATE(photon%variance_reduction(k,l)%&
                                        weight(inputF%composition%layers(k)%n_elements,383+1+1))
                                        ALLOCATE(photon%variance_reduction(k,l)%&
                                        energy(inputF%composition%layers(k)%n_elements,383+1+1))
                                        photon%variance_reduction(k,l)%weight =&
                                        0.0_C_DOUBLE
                                        photon%variance_reduction(k,l)%energy =&
                                        0.0_C_DOUBLE
                                  ENDDO
                                ENDDO
                        ENDIF
                        photon%history(1,1)=NO_INTERACTION
                        photon%n_interactions=0
                        NULLIFY(photon%offspring)
                        !Calculate energy with rng
                        photon%energy = exc%discrete(i)%energy 
                        photon%energy_changed=.FALSE.
                        photon%mus = initial_mus
                        photon%current_layer = 1
                        photon%detector_hit = .FALSE.
                        photon%detector_hit2 = .FALSE.
                        photon%options = options

                        !ipol = MOD(j,2)

                        !Calculate its initial coordinates and direction
                        CALL xmi_coords_dir(rng,exc%discrete(i), inputF%geometry,&
                        photon)

                        !Calculate its weight and electric field...
                        IF (j .LT. hor_ver_ratio) THEN
                                !horizontal
                                photon%weight = (total_intensity)*exc_corr/inputF%general%n_photons_line 
                                photon%elecv(1) = 1.0_C_DOUBLE
                                photon%elecv(2) = 0.0_C_DOUBLE
                                photon%elecv(3) = 0.0_C_DOUBLE
                        ELSE
                                !vertical
                                photon%weight = (total_intensity)*exc_corr/inputF%general%n_photons_line 
                                photon%elecv(1) = 0.0_C_DOUBLE
                                photon%elecv(2) = 1.0_C_DOUBLE
                                photon%elecv(3) = 0.0_C_DOUBLE
                        ENDIF

#if DEBUG == 1
                        WRITE (*,'(A,ES12.4)') 'photon weight: ',photon%weight
#endif

                        cosalfa = DOT_PRODUCT(photon%elecv, photon%dirv)

                        IF (ABS(cosalfa) .GT. 1.0) THEN
                                WRITE (*,'(A)') 'cosalfa exception detected'
                                CALL EXIT(1)
                        ENDIF

                        c_alfa = ACOS(cosalfa)
                        c_ae = 1.0/SIN(c_alfa)
                        c_be = -c_ae*cosalfa

                        photon%elecv = c_ae*photon%elecv + c_be*photon%dirv


                        !Calculate the electric field vector
                        !IF (REAL(i,KIND=C_DOUBLE)/REAL(n_photons,KIND=C_DOUBLE) &
                        !.LT. hor_ver_ratio ) THEN
                        !        !horizontal polarization
                        !ELSE
                        !        !vertical polarization
                        !ENDIF
                        
                        !shift the position towards the first layer
                        !calculate the intersection with the first plane
#if DEBUG == 2
!$omp critical                        
                        WRITE (*,'(A,3F12.6)') 'Coords before shift',&
                        photon%coords
!$omp end critical
#endif
                        CALL &
                        xmi_photon_shift_first_layer(photon,inputF%composition,inputF%geometry)

#if DEBUG == 2
!$omp critical                        
                        WRITE (*,'(A,3F12.6)') 'Coords after shift',&
                        photon%coords
!$omp end critical
#endif

                        IF (xmi_simulate_photon(photon, inputF, hdf5F,rng) == 0) THEN
                                CALL EXIT(1)
                        ENDIF

                        photon_temp => photon
                        photon_eval:DO 

                                photons_simulated = photons_simulated + 1
                                var_red:IF (photon_temp%options%use_variance_reduction .EQ. 1 .AND.&
                                ALLOCATED(photon_temp%variance_reduction))&
                                THEN
                                        !add the variance reductio contribution
                                        !to the channels
                                        DO k=1,inputF%composition%n_layers
                                           DO l=1,inputF%general%n_interactions_trajectory
                                             DO m=1,inputF%composition%layers(k)%n_elements
                                               DO n=1,385
                                                IF (photon_temp%variance_reduction(k,l)%energy(m,n) .GE. energy_threshold) THEN
                                                        channel = INT((photon_temp%variance_reduction(k,l)%energy(m,n) - &
                                                        inputF%detector%zero)/inputF%detector%gain)
                                                ELSE
                                                        channel = 0
                                                ENDIF

                                                IF (channel .GT. 0 .AND. channel .LE. nchannels) THEN
                                                        channels(l:, channel) =&
                                                        channels(l:, channel)+photon_temp%weight*&
                                                        photon_temp%variance_reduction(k,l)%weight(m,n)
                                                ENDIF
                                               ENDDO
                                             ENDDO
                                           ENDDO 
                                        ENDDO
                                ENDIF var_red
                                det_hit:IF (photon_temp%detector_hit .EQ. .TRUE.) THEN
                                        detector_hits = detector_hits + 1
!
!
!                                       Add to channelsF
!
!
                                        IF (options%use_variance_reduction .EQ. 0) THEN
                                        IF (photon_temp%energy .GE. energy_threshold) THEN
                                                channel = INT((photon_temp%energy - &
                                                inputF%detector%zero)/inputF%detector%gain)
                                        ELSE
                                                channel = 0
                                        ENDIF

                                        IF (channel .GT. 0 .AND. channel .LE. nchannels) THEN
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
                                                        history(element,ABS(photon_temp%history(k,1)),k) = &
                                                        history(element,ABS(photon_temp%history(k,1)),k) + 1
                                                ELSEIF &
                                                        (photon_temp%history(k,1) .EQ. RAYLEIGH_INTERACTION) THEN
                                                        !rayleigh
                                                        history(element,383+1,k) = &
                                                        history(element,383+1,k) + 1
                                                ELSEIF &
                                                (photon_temp%history(k,1) .EQ. COMPTON_INTERACTION) THEN
                                                        !compton
                                                        history(element,383+2,k) = &
                                                        history(element,383+2,k) + 1
                                                ENDIF
                                                !ENDDO
#if DEBUG == 1
                                                IF (photon_temp%last_interaction .EQ. PHOTOELECTRIC_INTERACTION) THEN
                                                last_shell(photon_temp%last_shell) =&
                                                last_shell(photon_temp%last_shell)+1
                                                ENDIF
#endif
                                        ENDIF

                                ENDIF det_hit
                                IF (photon_temp%detector_hit2 .EQ. .TRUE.) THEN
                                        detector_hits2 = detector_hits2 + 1
                                ENDIF

                                IF (ASSOCIATED(photon_temp%offspring)) THEN
                                        photon_temp2 => photon_temp%offspring
                                        IF (.NOT. ASSOCIATED(photon_temp2)) THEN
                                                WRITE (*,'(A)') 'This line should not appear'
                                                CALL EXIT(1)
                                        ENDIF
                                ELSE
                                        NULLIFY(photon_temp2)
                                ENDIF
                               
                                IF (options%use_variance_reduction&
                                .EQ. 1 .AND.&
                                ALLOCATED(photon_temp%variance_reduction))&
                                THEN
                                DO k=1, inputF%composition%n_layers
                                   DO &
                                   l=1,inputF%general%n_interactions_trajectory
                                        DEALLOCATE(photon_temp%variance_reduction(k,l)%weight)
                                        DEALLOCATE(photon_temp%variance_reduction(k,l)%energy)
                                   ENDDO
                                ENDDO
                                DEALLOCATE(photon_temp%variance_reduction)
                                ENDIF
                                DEALLOCATE(photon_temp%history)
                                DEALLOCATE(photon_temp%mus)
                                DEALLOCATE(photon_temp)
                                IF (ASSOCIATED(photon_temp2)) THEN
                                        photon_temp => photon_temp2
                                ELSE
                                        EXIT photon_eval
                                ENDIF

                        ENDDO photon_eval
                ENDDO
                DEALLOCATE(initial_mus)
        ENDDO disc 

#undef exc
 !       ENDASSOCIATE


        !cleanup
        CALL fgsl_rng_free(rng)

!$omp end parallel

#if DEBUG == 0
        WRITE (*,'(A,I)') 'Photons simulated: ',photons_simulated
        WRITE (*,'(A,I)') 'Photons hitting the detector...: ',detector_hits
        WRITE (*,'(A,I)') 'Photons hitting the detector2...: ',detector_hits2
        WRITE (*,'(A,I)') 'Rayleighs: ',rayleighs
        WRITE (*,'(A,I)') 'Comptons: ',comptons
        WRITE (*,'(A,I)') 'Photoelectric: ',einsteins
        WRITE (*,'(A,I)') 'Fe-KL2: ',history(26,ABS(KL2_LINE),1)
        WRITE (*,'(A,I)') 'Fe-KL3: ',history(26,ABS(KL3_LINE),1)
        WRITE (*,'(A,I)') 'Fe-KM2: ',history(26,ABS(KM2_LINE),1)
        WRITE (*,'(A,I)') 'Fe-KM3: ',history(26,ABS(KM3_LINE),1)
        !WRITE (*,'(A,I)') 'Fe-KL2: ',history(26,ABS(KL2_LINE),2)
        !WRITE (*,'(A,I)') 'Fe-KL3: ',history(26,ABS(KL3_LINE),2)
        !WRITE (*,'(A,I)') 'Fe-KM2: ',history(26,ABS(KM2_LINE),2)
        !WRITE (*,'(A,I)') 'Fe-KM3: ',history(26,ABS(KM3_LINE),2)
        WRITE (*,'(A,I)') 'Ni-KL2: ',history(28,ABS(KL2_LINE),1)
        WRITE (*,'(A,I)') 'Ni-KL3: ',history(28,ABS(KL3_LINE),1)
        WRITE (*,'(A,I)') 'Ni-KM2: ',history(28,ABS(KM2_LINE),1)
        WRITE (*,'(A,I)') 'Ni-KM3: ',history(28,ABS(KM3_LINE),1)
        !WRITE (*,'(A,I)') 'Ni-KL2: ',history(28,ABS(KL2_LINE),2)
        !WRITE (*,'(A,I)') 'Ni-KL3: ',history(28,ABS(KL3_LINE),2)
        !WRITE (*,'(A,I)') 'Ni-KM2: ',history(28,ABS(KM2_LINE),2)
        !WRITE (*,'(A,I)') 'Ni-KM3: ',history(28,ABS(KM3_LINE),2)
        WRITE (*,'(A,I)') 'Au-KM3: ',history(79,ABS(KM3_LINE),1)
        WRITE (*,'(A,I)') 'Au-LA1: ',history(79,ABS(LA1_LINE),1)
        WRITE (*,'(A,I)') 'Au-LA2: ',history(79,ABS(LA2_LINE),1)
        WRITE (*,'(A,I)') 'Au-LB1: ',history(79,ABS(LB1_LINE),1)
        WRITE (*,'(A,I)') 'Au-LB2: ',history(79,ABS(LB2_LINE),1)
        WRITE (*,'(A,I)') 'Au-LB3: ',history(79,ABS(LB3_LINE),1)
        WRITE (*,'(A,I)') 'Au-LB4: ',history(79,ABS(LB4_LINE),1)
        WRITE (*,'(A,I)') 'Au-LG2: ',history(79,ABS(LG2_LINE),1)
        WRITE (*,'(A,I)') 'Au-MA1: ',history(79,ABS(MA1_LINE),1)
        WRITE (*,'(A,I)') 'Au-MA2: ',history(79,ABS(MA2_LINE),1)
        WRITE (*,'(A,I)') 'Au-MB: ',history(79,ABS(MB_LINE),1)
        WRITE (*,'(A,I)') 'Au-MG: ',history(79,ABS(MG_LINE),1)
        WRITE (*,'(A,I)') 'Ba-KL2: ',SUM(history(56,ABS(KL2_LINE),1:inputF%general%n_interactions_trajectory))
        WRITE (*,'(A,I)') 'Ba-KL3: ',SUM(history(56,ABS(KL3_LINE),1:inputF%general%n_interactions_trajectory))
        WRITE (*,'(A,I)') 'Ba-KM2: ',SUM(history(56,ABS(KM2_LINE),1:inputF%general%n_interactions_trajectory))
        WRITE (*,'(A,I)') 'Ba-KM3: ',SUM(history(56,ABS(KM3_LINE),1:inputF%general%n_interactions_trajectory))
        WRITE (*,'(A,I)') 'Ba-LA1: ',SUM(history(56,ABS(LA1_LINE),1:inputF%general%n_interactions_trajectory))
        WRITE (*,'(A,I)') 'Ba-LA2: ',SUM(history(56,ABS(LA2_LINE),1:inputF%general%n_interactions_trajectory))
        WRITE (*,'(A,I)') 'Ba-LB1: ',SUM(history(56,ABS(LB1_LINE),1:inputF%general%n_interactions_trajectory))
        WRITE (*,'(A,I)') 'Ba-LB2: ',SUM(history(56,ABS(LB2_LINE),1:inputF%general%n_interactions_trajectory))
        WRITE (*,'(A,I)') 'Ba-LB3: ',SUM(history(56,ABS(LB3_LINE),1:inputF%general%n_interactions_trajectory))
        WRITE (*,'(A,I)') 'Ba-LB4: ',SUM(history(56,ABS(LB4_LINE),1:inputF%general%n_interactions_trajectory))
        WRITE (*,'(A,I)') 'Ba-LG2: ',SUM(history(56,ABS(LG2_LINE),1:inputF%general%n_interactions_trajectory))
#endif

        !multiply with detector absorbers and detector crystal
        DO i=1,nchannels
                det_corr = 1.0_C_DOUBLE
                DO j=1,inputF%absorbers%n_det_layers
                        det_corr = det_corr * EXP(-1.0_C_DOUBLE*&
                        inputF%absorbers%det_layers(j)%density*&
                        inputF%absorbers%det_layers(j)%thickness*&
                        xmi_mu_calc(inputF%absorbers%det_layers(j),i*inputF%detector%gain+inputF%detector%zero)) 
                ENDDO
                DO j=1,inputF%detector%n_crystal_layers
                        det_corr = det_corr * (1.0_C_DOUBLE-EXP(-1.0_C_DOUBLE*&
                        inputF%detector%crystal_layers(j)%density*&
                        inputF%detector%crystal_layers(j)%thickness*&
                        xmi_mu_calc(inputF%detector%crystal_layers(j),i*inputF%detector%gain+inputF%detector%zero)) )
                ENDDO
                channels(:,i) = channels(:,i)*det_corr
        ENDDO
        

        
        !now we can access the history in C using the same indices...
        ALLOCATE(historyF(inputF%general%n_interactions_trajectory,(383+2),100))
        historyF = RESHAPE(history,[inputF%general%n_interactions_trajectory,(383+2),100],ORDER=[3,2,1])

        historyPtr = C_LOC(historyF)


#if DEBUG == 0
        WRITE (6,'(A,ES14.5)') 'zero_sum:' ,SUM(channels(0,:))
#endif
        

        ALLOCATE(channelsF(nchannels,0:inputF%general%n_interactions_trajectory))
        channelsF = RESHAPE(channels, [nchannels,inputF%general%n_interactions_trajectory+1],ORDER=[2,1])

        channelsPtr = C_LOC(channelsF)

        rv = 1

ENDFUNCTION xmi_main_msim


SUBROUTINE xmi_coords_dir(rng, energy, geometry, photon) 
        IMPLICIT NONE
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_energy), INTENT(IN) :: energy
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

!        IF (photon%dirv(1) .EQ. 0.0_C_DOUBLE) THEN
!                !watch out... if photon%dirv(2) EQ 0.0 then result may be
!                !processor dependent...
!                photon%phi = SIGN(M_PI_2, photon%dirv(2))
!        ELSE
!#if DEBUG == 0
!                WRITE (*,'(A)') 'Dont think we should get here'
!#endif
!                photon%phi = ATAN(photon%dirv(2)/photon%dirv(1))
!        ENDIF
        
        photon%phi = ATAN2(photon%dirv(2),photon%dirv(1))
        
        RETURN

ENDSUBROUTINE xmi_coords_dir

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

SUBROUTINE xmi_coords_gaussian(rng, energy, geometry, photon, x1, y1) 
        IMPLICIT NONE
        TYPE (fgsl_rng), INTENT(IN) :: rng
        TYPE (xmi_energy), INTENT(IN) :: energy
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

ENDSUBROUTINE xmi_coords_gaussian



SUBROUTINE xmi_db(filename)

USE :: xraylib
USE :: hdf5
USE :: OMP_LIB
USE :: xmimsim_aux
USE,INTRINSIC :: ISO_C_BINDING
USE,INTRINSIC :: ISO_FORTRAN_ENV

IMPLICIT NONE

INTEGER, PARAMETER :: nintervals_r = 2000, nintervals_e = 200, maxz = 94, &
nintervals_theta=100000, nintervals_theta2=200,nintervals_phi=100000, &
nintervals_e_ip = 10000, nintervals_pz=50000
REAL (KIND=C_DOUBLE), PARAMETER :: maxe = 100.0, lowe = 0.1, &
        PI = 3.14159265359,MEC2 = 510.998910,maxpz = 100.0
CHARACTER(200) :: error_message

REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:,:) :: &
        rayleigh_theta,compton_theta
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: energies, rs, trapez, thetas,sumz,phis,trapez2
REAL (KIND=C_FLOAT), ALLOCATABLE, DIMENSION(:), TARGET :: energies_flt
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: energies_dbl
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: pzs

INTEGER :: stat,i,j,k,l,m,n
REAL (KIND=C_DOUBLE) :: temp_sum,K0K
REAL (KIND=C_FLOAT) :: temp_energy,temp_total_cs

CHARACTER(len=*),INTENT(IN) :: filename  

INTEGER(HID_T) :: file_id       ! File identifier 
INTEGER(HID_T) :: dset_id       ! Dataset identifier 
INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
INTEGER(HID_T) :: group_id, group_id2    
INTEGER(HSIZE_T),DIMENSION(2) :: dims = [nintervals_e, nintervals_r]
INTEGER(HSIZE_T),DIMENSION(2) :: dims2 = [nintervals_theta2, nintervals_r]
INTEGER(HSIZE_T),DIMENSION(3) :: dims3 = [nintervals_theta2, nintervals_e,nintervals_r]
INTEGER(HSIZE_T),DIMENSION(2) :: dims4 

INTEGER :: h5error
CHARACTER(len=2) :: element

REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: &
        rayleigh_phi
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:,:) ::  compton_phi
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: cdfs 

TYPE (interaction_prob), DIMENSION(maxz) :: interaction_probs

REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: doppler_pz 
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: fluor_yield_corr 


CALL h5open_f(h5error)


CALL SetErrorMessages(0)

ALLOCATE(rayleigh_theta(maxz, nintervals_e, nintervals_r),&
compton_theta(maxz, nintervals_e, nintervals_r),&
energies(nintervals_e), rs(nintervals_r),&
thetas(nintervals_theta),&
doppler_pz(maxz, nintervals_r), pzs(nintervals_pz), fluor_yield_corr(maxz,K_SHELL:M5_SHELL), STAT=stat, errmsg=error_message )

IF (stat /= 0) THEN 
        WRITE (error_unit,*) 'Allocation failure:',trim(error_message)
        CALL EXIT(1)
ENDIF

!Fill up the energies and rs arrays...

DO i=1,nintervals_e
        energies(i) = lowe + (maxe-lowe)*(REAL(i,C_DOUBLE)-1.0)/(nintervals_e-1.0)
ENDDO

#if DEBUG == 2
        WRITE (6,*) 'energies(1): ',energies(1)
        WRITE (6,*) 'energies(nintervals_e): ',energies(nintervals_e)
#endif

DO i=1,nintervals_pz
        pzs(i) = 0.0_C_DOUBLE + maxpz*(REAL(i,C_DOUBLE)-1.0)/(nintervals_pz-1.0)
ENDDO



DO i=1,nintervals_r
        rs(i) = (REAL(i,C_DOUBLE)-1.0)/(nintervals_r-1.0)
ENDDO

#if DEBUG == 2
        WRITE (6,*) 'rs(1): ',rs(1)
        WRITE (6,*) 'rs(nintervals_r): ',rs(nintervals_r)
#endif

DO i=1,nintervals_theta
        thetas(i) = 0.0_C_DOUBLE+(PI)*(REAL(i,C_DOUBLE)-1.0)/(nintervals_theta-1.0)
ENDDO

#if DEBUG == 2
        WRITE (6,*) 'thetas(1): ',thetas(1)
        WRITE (6,*) 'thetas(nintervals_theta): ',thetas(nintervals_theta)
#endif

!#if DEBUG != 1

!CALL OMP_SET_NUM_THREADS(1)

!$OMP PARALLEL DEFAULT(shared) PRIVATE(i,j,k,l,m,trapez,temp_sum,sumz,energies_flt,temp_energy,trapez2)

#if DEBUG == 2
WRITE(6,*) 'multiple allocs'
#endif
ALLOCATE(trapez(nintervals_theta-1))
ALLOCATE(trapez2(nintervals_pz-1))
ALLOCATE(sumz(nintervals_theta-1))
!$OMP DO

Zloop:DO i=1,maxz 
#if DEBUG == 1
!$omp critical 
WRITE (6,'(A,I)') 'Element: ',i
!$omp end critical
!IF (i == 26) THEN
!        OPEN(UNIT=100,file='fe_cdf.txt',status='replace',action='write')
!ENDIF

#endif
        Eloop:DO j=1,nintervals_e

                !
                !Rayleigh
                !

                thetaloop: DO k=1,nintervals_theta-1
                        trapez(k) = &
(DCS_Rayl(i,REAL(energies(j),KIND=C_FLOAT),&
REAL(thetas(k),KIND=C_FLOAT))*SIN(thetas(k))+&
DCS_Rayl(i,REAL(energies(j),KIND=C_FLOAT),&
REAL(thetas(k+1),KIND=C_FLOAT))*SIN(thetas(k+1)))&
*(thetas(k+1)-thetas(k))/2.0
                ENDDO thetaloop
                !to avoid database conflicts -> calculate CS_Rayl yourself...
                !trapez = trapez*2.0*PI/CS_Rayl(i,REAL(energies(j),KIND=C_FLOAT))
                trapez = trapez/SUM(trapez)
#if DEBUG == 2
                IF (i == 26) THEN
                        sumz(1)=trapez(1)
                        DO l=2,nintervals_theta-1 
                                sumz(l)=sumz(l-1)+trapez(l)
                        ENDDO
                        WRITE (100,*) sumz
                ENDIF
#endif


                temp_sum=0.0_C_DOUBLE
                l=1
                m=1
                DO 
                        temp_sum = trapez(l)+temp_sum
                        IF (temp_sum >= rs(m)) THEN
                                rayleigh_theta(i,j,m) = thetas(l)
                                IF (m == nintervals_r) EXIT
                                m = m+1
                        ENDIF

                        IF (l == nintervals_theta-1) EXIT
                        l=l+1
                ENDDO
                rayleigh_theta(i,j,nintervals_r) = PI
                rayleigh_theta(i,j,1) = 0.0
                !
                !Compton
                !

                thetaloop2: DO k=1,nintervals_theta-1
                        trapez(k) = &
(DCS_Compt(i,REAL(energies(j),KIND=C_FLOAT),&
REAL(thetas(k),KIND=C_FLOAT))*SIN(thetas(k))+&
DCS_Compt(i,REAL(energies(j),KIND=C_FLOAT),&
REAL(thetas(k+1),KIND=C_FLOAT))&
*SIN(thetas(k+1)))*(thetas(k+1)-thetas(k))/2.0
                ENDDO thetaloop2
                !trapez = trapez*2.0*PI/CS_Compt(i,REAL(energies(j),KIND=C_FLOAT))
                trapez = trapez/SUM(trapez)
                temp_sum=0.0_C_DOUBLE
                l=1
                m=1
                DO 
                        temp_sum = trapez(l)+temp_sum
                        IF (temp_sum >= rs(m)) THEN
                                compton_theta(i,j,m) = thetas(l)
                                IF (m == nintervals_r) EXIT
                                m = m+1
                        ENDIF

                        IF (l == nintervals_theta-1) EXIT
                        l=l+1
                ENDDO
                compton_theta(i,j,nintervals_r) = PI
                compton_theta(i,j,1) = 0.0
        ENDDO Eloop
#if DEBUG == 2
IF (i == 26) THEN
        CLOSE(unit=100)
ENDIF
#endif
        !automatic allocation -> could be a problem for some
        !compilers... yes I'm talking about you Intel!!! (requires -assume lhs_alloc)
        !Gfortran 4.5 also doesn't appear to support automatic allocation when
        !initializing with REAL... sorry Intel!
        ALLOCATE(energies_flt(nintervals_e_ip))
        DO j=1,nintervals_e_ip
                energies_flt(j) = REAL(lowe + (maxe-lowe)*(REAL(j,C_DOUBLE)-1.0)&
                /(nintervals_e_ip-1.0),KIND=C_DOUBLE)
        ENDDO

        !add edge energies
        DO k=K_SHELL,L3_SHELL
                temp_energy = EdgeEnergy(i,k)
                IF (temp_energy > 0.0_C_FLOAT) THEN
                        energies_flt = [energies_flt,&
                         temp_energy+0.00001_C_FLOAT]
                        energies_flt = [energies_flt,&
                         temp_energy-0.00001_C_FLOAT]
                ENDIF
        ENDDO
        

        !SORT them
        CALL qsort(C_LOC(energies_flt),SIZE(energies_flt,KIND=C_SIZE_T),&
        INT(KIND(energies_flt),KIND=C_SIZE_T),C_FUNLOC(C_FLOAT_CMP))
        !calls assign_interaction_prob...
        interaction_probs(i) = energies_flt

        DO k=1,SIZE(energies_flt)
                temp_total_cs = CS_Total_Kissel(i,energies_flt(k))
                interaction_probs(i)%Rayl_and_Compt(k,1) = CS_Rayl(i,energies_flt(k))/temp_total_cs
                interaction_probs(i)%Rayl_and_Compt(k,2) = CS_Compt(i,energies_flt(k))/temp_total_cs+&
                  interaction_probs(i)%Rayl_and_Compt(k,1)
        ENDDO

        DEALLOCATE(energies_flt)

        !
        !
        !       Doppler broadening
        !
        !
        DO j=1,nintervals_pz-1
                trapez2(j) = &
                (ComptonProfile(i, REAL(pzs(j),KIND=C_FLOAT))+&
                ComptonProfile(i, REAL(pzs(j+1),KIND=C_FLOAT)))*&
                (pzs(j+1)-pzs(j))/2.0_C_DOUBLE/REAL(i,KIND=C_DOUBLE)
                !divide by atomic number because we want an average value per
                !electron
        ENDDO
        trapez2 = trapez2/SUM(trapez2)

        temp_sum = 0.0_C_DOUBLE
        l=1
        m=1
        DO
                temp_sum = trapez2(l)+temp_sum
                IF (temp_sum >= rs(m)) THEN
                       doppler_pz(i,m) = pzs(l)
                        IF (m == nintervals_r) EXIT
                        m = m+1
                ENDIF
                IF (l == nintervals_pz-1) EXIT
                l = l+1
        ENDDO
        doppler_pz(i,nintervals_r) = maxpz
        doppler_pz(i,1) = 0.0_C_DOUBLE

        !
        !
        !       Corrected fluorescence yields (in terms of the primary vacancy
        !       distributions)
        !
        !
#define CKTB CosKronTransProb
        fluor_yield_corr(i,K_SHELL) = FluorYield(i,K_SHELL) 
        fluor_yield_corr(i,L1_SHELL) = FluorYield(i,L1_SHELL)+&
                        (CKTB(i,FL12_TRANS)*FluorYield(i,L2_SHELL))+&
                        (CKTB(i,FL13_TRANS)+CKTB(i,FL12_TRANS)*CKTB(i,FL23_TRANS))*&
                        FluorYield(i,L3_SHELL)
        fluor_yield_corr(i,L2_SHELL) = FluorYield(i,L2_SHELL)+&
                        (CKTB(i,FL23_TRANS)*FluorYield(i,L3_SHELL))
        fluor_yield_corr(i,L3_SHELL) = FluorYield(i,L3_SHELL)
        fluor_yield_corr(i,M1_SHELL) = &
                        !M1_SHELL
                        FluorYield(i,M1_SHELL)+&
                        !M2_SHELL
                        CKTB(i,FM12_TRANS)*FluorYield(i,M2_SHELL)+&
                        !M3_SHELL
                        (CKTB(i,FM13_TRANS)+CKTB(i,FM12_TRANS)*CKTB(i,FM23_TRANS))*&
                        FluorYield(i,M3_SHELL)+&
                        !M4_SHELL
                        (CKTB(i,FM14_TRANS)+CKTB(i,FM13_TRANS)*CKTB(i,FM34_TRANS)+&
                        CKTB(i,FM12_TRANS)*CKTB(i,FM24_TRANS)+&
                        CKTB(i,FM12_TRANS)*CKTB(i,FM23_TRANS)*CKTB(i,FM34_TRANS))*&
                        FluorYield(i,M4_SHELL)+&
                        !M5_SHELL
                        (CKTB(i,FM15_TRANS)+&
                        CKTB(i,FM14_TRANS)*CKTB(i,FM45_TRANS)+&
                        CKTB(i,FM13_TRANS)*CKTB(i,FM35_TRANS)+&
                        CKTB(i,FM12_TRANS)*CKTB(i,FM25_TRANS)+&
                        CKTB(i,FM13_TRANS)*CKTB(i,FM34_TRANS)*CKTB(i,FM45_TRANS)+&
                        CKTB(i,FM12_TRANS)*CKTB(i,FM24_TRANS)*CKTB(i,FM45_TRANS)+&
                        CKTB(i,FM12_TRANS)*CKTB(i,FM23_TRANS)*CKTB(i,FM35_TRANS)+&
                        CKTB(i,FM12_TRANS)*CKTB(i,FM23_TRANS)*CKTB(i,FM34_TRANS)*CKTB(i,FM45_TRANS))*&
                        FluorYield(i,M5_SHELL)
        fluor_yield_corr(i,M2_SHELL) = &
                        !M2_SHELL
                        FluorYield(i,M2_SHELL)+&
                        !M3_SHELL
                        CKTB(i,FM23_TRANS)*FluorYield(i,M3_SHELL)+&
                        !M4_SHELL
                        (CKTB(i,FM24_TRANS)+CKTB(i,FM23_TRANS)*CKTB(i,FM34_TRANS))*&
                        FluorYield(i,M4_SHELL)+&
                        !M5_SHELL
                        (CKTB(i,FM25_TRANS)+CKTB(i,FM24_TRANS)*CKTB(i,FM45_TRANS)+&
                        CKTB(i,FM23_TRANS)*CKTB(i,FM35_TRANS)+&
                        CKTB(i,FM23_TRANS)*CKTB(i,FM34_TRANS)*CKTB(i,FM45_TRANS))*&
                        FluorYield(i,M5_SHELL)
        fluor_yield_corr(i,M3_SHELL) = &
                        !M3_SHELL
                        FluorYield(i,M3_SHELL)+&
                        !M4_SHELL
                        CKTB(i,FM34_TRANS)*FluorYield(i,M4_SHELL)+&
                        !M5_SHELL
                        (CKTB(i,FM35_TRANS)+CKTB(i,FM34_TRANS)*CKTB(i,FM45_TRANS))*&
                        FluorYield(i,M5_SHELL)
        fluor_yield_corr(i,M4_SHELL) = &
                        !M4_SHELL
                        FluorYield(i,M4_SHELL)+&
                        !M5_SHELL
                        CKTB(i,FM45_TRANS)*FluorYield(i,M5_SHELL)
        fluor_yield_corr(i,M5_SHELL) = &
                        !M5_SHELL
                        FluorYield(i,M5_SHELL)
#undef CKTB

ENDDO Zloop
!$OMP END DO
!$OMP END PARALLEL

!#endif

!Write Theta inverse cdfs to hdf5 file
CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F,file_id,h5error)

!Create groups and fill them up...

!#if DEBUG != 1
DO i=1,maxz
!
!
!       Internal files are problematic in Intel Fortran...
!
!

!        WRITE(element,'(I2)') i
!#if DEBUG == 2
!        WRITE (*,*) 'Writing element: ',element
!#endif
        !group creation -> element
        CALL h5gcreate_f(file_id,elements(i),group_id,h5error)

        !group creation -> theta_icdf 
        CALL h5gcreate_f(group_id,'Theta_ICDF',group_id2,h5error)

        !create rayleigh theta dataset, including the energies and random
        !numbers 
        CALL h5screate_simple_f(2,dims,dspace_id,h5error)
        CALL h5dcreate_f(group_id2,'RayleighTheta_ICDF',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,rayleigh_theta(i,:,:),dims,h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)
        CALL h5screate_simple_f(1,[dims(1)],dspace_id,h5error)
        CALL h5dcreate_f(group_id2,'Energies',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,energies,[dims(1)],h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)
        CALL h5screate_simple_f(1,[dims(2)],dspace_id,h5error)
        CALL h5dcreate_f(group_id2,'Random numbers',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,rs,[dims(2)],h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)


        !create compton theta dataset
        CALL h5screate_simple_f(2,dims,dspace_id,h5error)
        CALL h5dcreate_f(group_id2,'ComptonTheta_ICDF',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,compton_theta(i,:,:),dims,h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)

        !create doppler compton broadening
        CALL h5screate_simple_f(1,[nintervals_r],dspace_id,h5error)
        CALL h5dcreate_f(group_id2,'Doppler_pz_ICDF',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,doppler_pz(i,:),[nintervals_r],h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)

        !create corrected fluorescence yields
        CALL h5screate_simple_f(1,[INT(M5_SHELL-K_SHELL+1,KIND=HSIZE_T)],dspace_id,h5error)
        CALL h5dcreate_f(group_id2,'Corrected fluorescence yields',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,fluor_yield_corr(i,:),[INT(M5_SHELL-K_SHELL+1,KIND=HSIZE_T)],h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)

        !group close -> theta_icdf
        CALL h5gclose_f(group_id2,h5error)


        !group creation -> interaction_probs 
        CALL h5gcreate_f(group_id,'Interaction probabilities',group_id2,h5error)
        CALL h5screate_simple_f(1,[SIZE(interaction_probs(i)%energies ,KIND=HSIZE_T)],dspace_id,h5error)
        CALL h5dcreate_f(group_id2,'Energies',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,interaction_probs(i)%energies,&
                [SIZE(interaction_probs(i)%energies ,KIND=HSIZE_T)] ,h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)
        CALL h5screate_simple_f(2,[SIZE(interaction_probs(i)%Rayl_and_Compt, DIM=1 &
                ,KIND=HSIZE_T),SIZE(interaction_probs(i)%Rayl_and_Compt, DIM=2 ,KIND=HSIZE_T)],dspace_id,h5error)
        CALL h5dcreate_f(group_id2,'Rayleigh and Compton probabilities',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,interaction_probs(i)%Rayl_and_Compt,&
                [SIZE(interaction_probs(i)%Rayl_and_Compt, DIM=1 ,KIND=HSIZE_T),&
                SIZE(interaction_probs(i)%Rayl_and_Compt, DIM=2 ,KIND=HSIZE_T)] ,h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)

        !group close -> interaction_probs
        CALL h5gclose_f(group_id2,h5error)





        !group close -> element
        CALL h5gclose_f(group_id,h5error)
ENDDO

!#endif
!free memory
DEALLOCATE(rayleigh_theta,compton_theta,thetas)

!CALL h5close_f(h5error)


!azimuthal angle -> Z independent
ALLOCATE(rayleigh_phi(nintervals_theta2,nintervals_r),& 
compton_phi(nintervals_theta2,nintervals_e,nintervals_r),&
thetas(nintervals_theta2),phis(nintervals_phi)&
, STAT=stat, errmsg=error_message)

IF (stat /= 0) THEN 
        WRITE (error_unit,*) 'Allocation failure:',trim(error_message)
        CALL EXIT(1)
ENDIF

DO i=1,nintervals_theta2
        thetas(i) = 0.0_C_DOUBLE+(PI-0.0_C_DOUBLE)*(REAL(i,C_DOUBLE)-1.0)/(nintervals_theta2-1.0)
ENDDO

#if DEBUG == 1
        WRITE (6,*) 'thetas(1): ',thetas(1)
        WRITE (6,*) 'thetas(nintervals_theta2): ',thetas(nintervals_theta2)
#endif

DO i=1,nintervals_phi
        phis(i)=0.0_C_DOUBLE+(2.0*PI-0.0_C_DOUBLE)*(REAL(i,C_DOUBLE)-1.0)/(nintervals_phi-1.0)
ENDDO

#if DEBUG == 1
        WRITE (6,*) 'phis(1): ',phis(1)
        WRITE (6,*) 'phis(nintervals_phi): ',phis(nintervals_phi)
#endif



!CALL OMP_SET_NUM_THREADS(1)

!OPEN(UNIT=100,file='rayleigh_phi_cdf.txt',status='replace',action='write')

!$OMP PARALLEL DEFAULT(shared) PRIVATE(i,j,k,l,m,cdfs)

ALLOCATE(cdfs(nintervals_phi))

!$OMP DO
DO i=1,nintervals_theta2
        !Rayleigh first...
        DO j=1,nintervals_phi 
        cdfs(j)= &
        (phis(j)-(SIN(thetas(i))*SIN(thetas(i))*&
        SIN(2.0*phis(j)))/2.0/&
        (2.0-SIN(thetas(i))*SIN(thetas(i))))/2.0/PI
        ENDDO
        k=1

 !       WRITE (100,*) cdfs 

        DO j=1,nintervals_phi
                IF (cdfs(j) >= rs(k)) THEN
                        rayleigh_phi(i,k) = phis(j)
                        IF (k == nintervals_r) EXIT
                        k=k+1
                ENDIF
        ENDDO
        rayleigh_phi(i,1) = 0.0 
        rayleigh_phi(i,nintervals_r) = 2.0*PI

        !...and then of course Compton.
!        DO m=1,nintervals_e
!               K0K= 1.0 + energies(m)*(1.0-COS(thetas(i)))/MEC2
!                DO j=1,nintervals_phi
!                cdfs(j)= &
!                (phis(j)-(SIN(thetas(i))*SIN(thetas(i))*&
!                SIN(2.0*phis(j)))/2.0/&
!                (K0K+(1.0/K0K)-SIN(thetas(i)*SIN(thetas(i)))))/2.0/PI
!                ENDDO
!
!                k=1
!
!               DO j=1,nintervals_phi
!                        IF (cdfs(j) >= rs(k)) THEN
!                                compton_phi(i,m,k) = phis(j)
!                                IF (k == nintervals_r) EXIT
!                                k=k+1
!                        ENDIF
!                ENDDO
!                compton_phi(i,m,1) = 0.0 
!                compton_phi(i,m,nintervals_r) = 2.0*PI
!        ENDDO

ENDDO

!$OMP END DO
DEALLOCATE(cdfs)
!$OMP END PARALLEL

!CLOSE(UNIT=100)



!$OMP PARALLEL DEFAULT(shared) PRIVATE(j,k,l,m,cdfs,K0K)

ALLOCATE(cdfs(nintervals_phi))

!$OMP DO
DO i=1,nintervals_theta2

        !...and then of course Compton.
        DO m=1,nintervals_e
               K0K= 1.0 + energies(m)*(1.0-COS(thetas(i)))/MEC2
                DO j=1,nintervals_phi
                cdfs(j)= &
                (phis(j)-(SIN(thetas(i))*SIN(thetas(i))*&
                SIN(2.0*phis(j)))/2.0/&
                (K0K+(1.0/K0K)-SIN(thetas(i)*SIN(thetas(i)))))/2.0/PI
                ENDDO

                k=1

               DO j=1,nintervals_phi
                        IF (cdfs(j) >= rs(k)) THEN
                                compton_phi(i,m,k) = phis(j)
                                IF (k == nintervals_r) EXIT
                                k=k+1
                        ENDIF
                ENDDO
                compton_phi(i,m,1) = 0.0 
                compton_phi(i,m,nintervals_r) = 2.0*PI
        ENDDO
ENDDO

!$OMP END DO
DEALLOCATE(cdfs)
!$OMP END PARALLEL


#if DEBUG == 1
WRITE (6,*) 'Continuing to write to HDF5 file'
#endif
!write to hdf5 file...
!CALL h5gopen_f(file_id,'/',group_id,h5error)

CALL h5gcreate_f(file_id,'RayleighPhi',group_id,h5error)

CALL h5screate_simple_f(2,dims2,dspace_id,h5error)
CALL &
h5dcreate_f(group_id,'RayleighPhi_ICDF',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,rayleigh_phi,dims2,h5error)
CALL h5sclose_f(dspace_id,h5error)
CALL h5dclose_f(dset_id,h5error)

CALL h5screate_simple_f(1,[dims2(1)],dspace_id,h5error)
CALL &
h5dcreate_f(group_id,'Thetas',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,thetas,[dims2(1)],h5error)
CALL h5sclose_f(dspace_id,h5error)
CALL h5dclose_f(dset_id,h5error)

CALL h5screate_simple_f(1,[dims2(2)],dspace_id,h5error)
CALL &
h5dcreate_f(group_id,'Random numbers',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,rs,[dims2(2)],h5error)
CALL h5sclose_f(dspace_id,h5error)
CALL h5dclose_f(dset_id,h5error)
CALL h5gclose_f(group_id,h5error)




CALL h5gcreate_f(file_id,'ComptonPhi',group_id,h5error)
CALL h5screate_simple_f(3,dims3,dspace_id,h5error)
CALL &
h5dcreate_f(group_id,'ComptonPhi_ICDF',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,compton_phi,dims3,h5error)
CALL h5sclose_f(dspace_id,h5error)
CALL h5dclose_f(dset_id,h5error)

CALL h5screate_simple_f(1,[dims3(1)],dspace_id,h5error)
CALL &
h5dcreate_f(group_id,'Thetas',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,thetas,[dims3(1)],h5error)
CALL h5sclose_f(dspace_id,h5error)
CALL h5dclose_f(dset_id,h5error)

CALL h5screate_simple_f(1,[dims3(2)],dspace_id,h5error)
CALL &
h5dcreate_f(group_id,'Energies',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,energies,[dims3(2)],h5error)
CALL h5sclose_f(dspace_id,h5error)
CALL h5dclose_f(dset_id,h5error)

CALL h5screate_simple_f(1,[dims3(3)],dspace_id,h5error)
CALL &
h5dcreate_f(group_id,'Random numbers',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,rs,[dims3(3)],h5error)
CALL h5sclose_f(dspace_id,h5error)
CALL h5dclose_f(dset_id,h5error)
CALL h5gclose_f(group_id,h5error)


CALL h5fclose_f(file_id,h5error)



CALL h5close_f(h5error)


ENDSUBROUTINE xmi_db

SUBROUTINE xmi_photon_shift_first_layer(photon, composition, geometry)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (xmi_geometry), INTENT(IN) :: geometry
        TYPE (xmi_composition), INTENT(IN) :: composition
        TYPE (xmi_line) :: line
        TYPE (xmi_plane) :: plane

        !Calculate intersection of photon trajectory with plane of first layer
        line%point = photon%coords
        line%dirv  = photon%dirv

        !d_sample_source is to be corrected with thicknesses of the layers
        !preceding reference_layer...
        plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE,&
        composition%layers(1)%Z_coord_begin]
        plane%normv = geometry%n_sample_orientation

        IF (xmi_intersection_plane_line(plane, line, photon%coords) == 0)  &
                CALL EXIT(1)

        RETURN
ENDSUBROUTINE xmi_photon_shift_first_layer

FUNCTION xmi_simulate_photon(photon, inputF, hdf5F,rng) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (fgsl_rng), INTENT(IN) :: rng
        INTEGER (C_INT) :: rv

        LOGICAL :: terminated, inside
        REAL (C_DOUBLE) :: interactionR
        INTEGER (C_INT) :: i,j,k
        TYPE (xmi_plane) :: plane
        TYPE (xmi_line) :: line
        REAL (C_DOUBLE), DIMENSION(3) :: intersect
        REAL (C_DOUBLE) :: dist, max_random_layer, min_random_layer
        REAL (C_DOUBLE) :: dist_prev, tempexp, dist_sum, blbs
        INTEGER (C_INT) :: step_do_max, step_do_dir

        REAL (C_DOUBLE) :: atomsel_threshold
        INTEGER (C_INT) :: pos
        INTEGER (C_INT) :: rv_interaction 

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
                inside = .FALSE.
                interactionR = fgsl_rng_uniform(rng)
#if DEBUG == 2
                WRITE (*,'(A,F12.6)') 'interactionR= ',interactionR
#endif
                min_random_layer = 0.0_C_DOUBLE
                max_random_layer = 0.0_C_DOUBLE
                !recalculate mus if necessary
                IF (photon%energy_changed .EQ. .TRUE.) THEN
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

#if DEBUG == 2
                WRITE (*,'(A)') 'Before do loop'
#endif

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

                        IF (xmi_intersection_plane_line(plane, line, intersect) == 0)  &
                                CALL EXIT(1)
                        
                        dist = xmi_distance_two_points(photon%coords,intersect)
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
                        tempexp = EXP(-1.0_C_DOUBLE*(dist)*&
                        inputF%composition%layers(i)%density*&
                        photon%mus(i))
                        min_random_layer = max_random_layer 
                        max_random_layer = max_random_layer + blbs*(&
                        1.0_C_DOUBLE - tempexp)
                        !dist here must be total dist: from previous interaction
                        !until current layer
                       
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
                                
                                CALL xmi_move_photon_with_dist(photon,dist)
#if DEBUG == 2
                                WRITE (*,'(A,F12.5)') 'Actual dist: ',dist
                                WRITE (*,'(A,3F12.5)') 'New coords: '&
                                ,photon%coords
#endif
                                !update current_layer
                                photon%current_layer = i
                                inside = .TRUE.
                                !exit loop 
                                EXIT
                        ELSE
                                !goto next layer
                                !coordinates equal to layer boundaries
                                photon%coords = intersect
                                dist_prev = dist_prev+dist
                                blbs = blbs*tempexp
                                !check if we are not leaving the system!
                        ENDIF
                ENDDO

#if DEBUG == 2
                WRITE (*,'(A)') 'After do loop'
#endif

                IF (inside .EQ. .FALSE.) THEN
                        !photon has left the system
                        !check if it will make it to the detector
                        photon%detector_hit=xmi_check_photon_detector_hit(&
                        photon,inputF)
                        EXIT main
                ENDIF

                !update number of interactions
                photon%n_interactions = photon%n_interactions + 1

                IF(photon%n_interactions-1 .EQ.&
                inputF%general%n_interactions_trajectory) THEN
                        EXIT main
                ENDIF
                
                !variance reduction
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
                       REAL(photon%energy,C_FLOAT))/photon%mus(photon%current_layer)
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
                ASSOCIATE (hdf5_Z => inputF%composition%layers&
                (photon%current_layer)%xmi_hdf5_Z_local&
                (photon%current_element_index)%Ptr)

                pos = findpos(hdf5_Z%interaction_probs%energies, photon%energy)
                IF (pos .LT. 1_C_INT) THEN
                        WRITE (*,'(A)') &
                        'Invalid result for findpos interaction type'
#if DEBUG == 0
                        WRITE (*,'(A,I2)') 'last interaction type: '&
                        ,photon%last_interaction
                        WRITE (*,'(A,F12.6)') 'photon%energy: ',photon%energy
#endif
                        CALL EXIT(1)
                ENDIF

#if DEBUG == 2
                WRITE (*,'(A,I,F12.6)') 'pos and energy: ',pos, hdf5_Z%interaction_probs%energies(pos)
#endif

                IF (interactionR .LT. interpolate_simple([hdf5_Z%interaction_probs%energies(pos),&
                        hdf5_Z%interaction_probs%Rayl_and_Compt(pos,1)],&
                        [hdf5_Z%interaction_probs%energies(pos+1),&
                        hdf5_Z%interaction_probs%Rayl_and_Compt(pos+1,1)],&
                        photon%energy)) THEN
                        !we've got Rayleigh
                        photon%last_interaction = RAYLEIGH_INTERACTION
                        rv_interaction = xmi_simulate_photon_rayleigh(photon,&
                        inputF, hdf5F, rng) 
                ELSEIF (interactionR .LT. interpolate_simple([hdf5_Z%interaction_probs%energies(pos),&
                        hdf5_Z%interaction_probs%Rayl_and_Compt(pos,2)],&
                        [hdf5_Z%interaction_probs%energies(pos+1),&
                        hdf5_Z%interaction_probs%Rayl_and_Compt(pos+1,2)],&
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

#if DEBUG == 0
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

                ENDASSOCIATE
        ENDDO main

        rv = 1

ENDFUNCTION xmi_simulate_photon

FUNCTION xmi_init_input(inputFPtr) BIND(C,NAME='xmi_init_input') RESULT(rv)
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN) :: inputFPtr
        INTEGER (C_INT) :: rv
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: inverse

        TYPE (C_PTR) :: inversePtr
        TYPE (xmi_input), POINTER :: inputF
        REAL (C_DOUBLE) :: distance_sample_detector,half_apex 

        rv = 0

        !associate pointers
        CALL C_F_POINTER(inputFPtr, inputF)

        !investigate detector
        inputF%detector%detector_radius = SQRT(inputF%geometry%area_detector/M_PI)
        IF (inputF%geometry%acceptance_detector .GE. M_PI) THEN
                inputF%detector%collimator_present = .FALSE.
                inputF%detector%collimator_height = 0.0_C_DOUBLE 
        ELSE
                inputF%detector%collimator_present = .TRUE.
                inputF%detector%collimator_height = inputF%detector%detector_radius&
                *2.0_C_DOUBLE/TAN(inputF%geometry%acceptance_detector/2.0_C_DOUBLE) 
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
        CALL xmi_inverse_matrix(C_LOC(inputF%detector%n_detector_orientation_new_x),&
        C_LOC(inputF%detector%n_detector_orientation_new_y),&
        C_LOC(inputF%detector%n_detector_orientation_new_z), inversePtr)
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
        inputF%detector%n_sample_orientation_det = MATMUL(inputF%detector%n_detector_orientation_inverse, &
                inputF%geometry%n_sample_orientation) 



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

        IF (xmi_intersection_plane_line(plane_det_base, photon_trajectory,&
        intersect) == 0) CALL EXIT(1)

        IF (norm(intersect) .GT. inputF%detector%detector_radius) RETURN

        !third: ok it will hit the detector, but will it make it through the
        !collimator as well??
        IF (inputF%detector%collimator_present .EQ. .FALSE.) THEN
                !there is no collimator
                rv = .TRUE.
                RETURN
        ENDIF
        
        !there is a collimator...
        plane_det_base%point = [inputF%detector%collimator_height, 0.0_C_DOUBLE, 0.0_C_DOUBLE]

        IF (xmi_intersection_plane_line(plane_det_base, photon_trajectory,&
        intersect) == 0) CALL EXIT(1)

        intersect(1) = 0.0_C_DOUBLE

        IF (norm(intersect) .GT. inputF%detector%detector_radius) RETURN

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

        ASSOCIATE (hdf5_Z => inputF%composition%layers&
                (photon%current_layer)%xmi_hdf5_Z_local&
                (photon%current_element_index)%Ptr)



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


        theta_i = bilinear_interpolation(hdf5_Z%RayleighTheta_ICDF, &
                hdf5_Z%Energies, hdf5_Z%RandomNumbers, photon%energy,&
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
        sinphi0 = DOT_PRODUCT(photon%elecv,[-sinphi,&
        cosphi, 0.0_C_DOUBLE])
        IF (ABS(cosphi0) .GT. 1.0_C_DOUBLE) cosphi0 = SIGN(1.0_C_DOUBLE,cosphi0)
        phi0 = ACOS(cosphi0)
        IF (sinphi0 .GT. 0.0_C_DOUBLE) phi0 = -phi0



        !
        !update photon%theta and photon%phi
        !
        CALL xmi_update_photon_dirv(photon, theta_i, phi_i+phi0)

        !
        !update electric field
        !
        CALL xmi_update_photon_elecv(photon)

        !update history
        photon%history(photon%n_interactions,1) = RAYLEIGH_INTERACTION
        photon%history(photon%n_interactions,2) = photon%current_element 


        ENDASSOCIATE

        rv = 1

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

        ASSOCIATE (hdf5_Z => inputF%composition%layers&
                (photon%current_layer)%xmi_hdf5_Z_local&
                (photon%current_element_index)%Ptr)

        !calculate theta
        theta_i = bilinear_interpolation(hdf5_Z%ComptonTheta_ICDF, &
                hdf5_Z%Energies, hdf5_Z%RandomNumbers, photon%energy,&
                fgsl_rng_uniform(rng), pos_1, pos_2) 

        !calculate phi
        phi_i = trilinear_interpolation(hdf5F%ComptonPhi_ICDF, &
                hdf5F%ComptonThetas, hdf5F%ComptonEnergies,&
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
        sinphi0 = DOT_PRODUCT(photon%elecv,[-sinphi,&
        cosphi, 0.0_C_DOUBLE])
        IF (ABS(cosphi0) .GT. 1.0_C_DOUBLE) cosphi0 = SIGN(1.0_C_DOUBLE,cosphi0)
        phi0 = ACOS(cosphi0)
        IF (sinphi0 .GT. 0.0_C_DOUBLE) phi0 = -phi0



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

        ENDASSOCIATE

        rv = 1

        RETURN

ENDFUNCTION xmi_simulate_photon_compton

FUNCTION xmi_simulate_photon_fluorescence(photon, inputF, hdf5F, rng) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        TYPE (xmi_hdf5), INTENT(IN) :: hdf5F
        TYPE (xmi_input), INTENT(IN) :: inputF
        TYPE (fgsl_rng), INTENT(IN) :: rng
        INTEGER (C_INT) :: rv,trans

        REAL (C_FLOAT) :: photo_total, energy_flt
        REAL (C_DOUBLE) :: sumz
        INTEGER (C_INT) :: shell,line_first, line_last, line
        REAL (C_DOUBLE) :: r
        REAL (C_DOUBLE) :: theta_i, phi_i

        LOGICAL :: shell_found, line_found
        INTEGER (C_INT) :: max_shell

#if DEBUG == 1
        WRITE (*,'(A)') 'Entering fluorescence'
        WRITE (*,'(A,I2)') 'element: ',photon%current_element
#endif


        rv = 0

        !so we've got photo electric effect
        !first is to check which shell got lucky
        energy_flt = REAL(photon%energy,C_FLOAT)
        photo_total = CS_Photo_Total(photon%current_element, energy_flt)

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
                energy_flt)/photo_total
                IF (r .LT. sumz) THEN
                        shell_found = .TRUE.
                        EXIT
                ENDIF
        ENDDO

#if DEBUG == 0
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


        !Coster-Kronig for L and M
        CALL xmi_coster_kronig_check(rng, shell, photon%current_element)

#if DEBUG == 1
        WRITE (*,'(A)') 'after CK check'
#endif
        !so now that we determined the shell to be used, see if we get
        !fluorescence...
        IF (xmi_fluorescence_line_check(rng, shell, photon%current_element,&
        photon%energy, line, photon%options%use_self_enhancement) .EQ. 0_C_INT) THEN
                rv = 1
                RETURN
        ENDIF

#if DEBUG == 1
        WRITE (*,'(A)') 'after fluor line check'
#endif
        photon%energy_changed = .FALSE.
        photon%mus = xmi_mu_calc(inputF%composition, photon%energy)

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
                auger_last = K_M5M5_AUGER
        ELSEIF (shell .EQ. L1_SHELL) THEN
                !L1 shell excitation
                auger_first = L1_L2L2_AUGER
                auger_last = L1_M5M5_AUGER
        ELSEIF (shell .EQ. L2_SHELL) THEN
                !L2 shell excitation
                auger_first = L2_L3L3_AUGER
                auger_last = L2_M5M5_AUGER
        ELSEIF (shell .EQ. L3_SHELL) THEN
                !L3 shell excitation
                auger_first = L3_M1M1_AUGER
                auger_last = L3_M5M5_AUGER
        ELSEIF (shell .EQ. M1_SHELL) THEN
                !M1 shell excitation
                auger_first = M1_M2M2_AUGER
                auger_last = M1_M5M5_AUGER
        ELSEIF (shell .EQ. M2_SHELL) THEN
                !M2 shell excitation
                auger_first = M2_M3M3_AUGER
                auger_last = M2_M5M5_AUGER
        ELSEIF (shell .EQ. M3_SHELL) THEN
                !M3 shell excitation
                auger_first = M3_M4M4_AUGER
                auger_last = M3_M5M5_AUGER
        ELSEIF (shell .EQ. M4_SHELL) THEN
                !M4 shell excitation
                auger_first = M4_M5M5_AUGER
                auger_last = M4_M5M5_AUGER
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
        SELECT CASE (auger)
                CASE (K_L1L1_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = L1_SHELL
                CASE (K_L1L2_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = L2_SHELL
                CASE (K_L1L3_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = L3_SHELL
                CASE (K_L1M1_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = M1_SHELL
                CASE (K_L1M2_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = M2_SHELL
                CASE (K_L1M3_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = M3_SHELL
                CASE (K_L1M4_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = M4_SHELL
                CASE (K_L1M5_AUGER)
                        shell_new1 = L1_SHELL
                        shell_new2 = M5_SHELL
                CASE (K_L2L1_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = L1_SHELL
                CASE (K_L2L2_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = L2_SHELL
                CASE (K_L2L3_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = L3_SHELL
                CASE (K_L2M1_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M1_SHELL
                CASE (K_L2M2_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M2_SHELL
                CASE (K_L2M3_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M3_SHELL
                CASE (K_L2M4_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M4_SHELL
                CASE (K_L2M5_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M5_SHELL
                CASE (K_L3L1_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = L1_SHELL
                CASE (K_L3L2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = L2_SHELL
                CASE (K_L3L3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = L3_SHELL
                CASE (K_L3M1_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M1_SHELL
                CASE (K_L3M2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M2_SHELL
                CASE (K_L3M3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M3_SHELL
                CASE (K_L3M4_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M4_SHELL
                CASE (K_L3M5_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M5_SHELL
                CASE (K_M1L1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = L1_SHELL
                CASE (K_M1L2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = L2_SHELL
                CASE (K_M1L3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = L3_SHELL
                CASE (K_M1M1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M1_SHELL
                CASE (K_M1M2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M2_SHELL
                CASE (K_M1M3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M3_SHELL
                CASE (K_M1M4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M4_SHELL
                CASE (K_M1M5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M5_SHELL
                CASE (K_M2L1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = L1_SHELL
                CASE (K_M2L2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = L2_SHELL
                CASE (K_M2L3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = L3_SHELL
                CASE (K_M2M1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M1_SHELL
                CASE (K_M2M2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M2_SHELL
                CASE (K_M2M3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M3_SHELL
                CASE (K_M2M4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M4_SHELL
                CASE (K_M2M5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M5_SHELL
                CASE (K_M3L1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = L1_SHELL
                CASE (K_M3L2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = L2_SHELL
                CASE (K_M3L3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = L3_SHELL
                CASE (K_M3M1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M1_SHELL
                CASE (K_M3M2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M2_SHELL
                CASE (K_M3M3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M3_SHELL
                CASE (K_M3M4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M4_SHELL
                CASE (K_M3M5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M5_SHELL
                CASE (K_M4L1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = L1_SHELL
                CASE (K_M4L2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = L2_SHELL
                CASE (K_M4L3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = L3_SHELL
                CASE (K_M4M1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M1_SHELL
                CASE (K_M4M2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M2_SHELL
                CASE (K_M4M3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M3_SHELL
                CASE (K_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE (K_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE (K_M5L1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = L1_SHELL
                CASE (K_M5L2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = L2_SHELL
                CASE (K_M5L3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = L3_SHELL
                CASE (K_M5M1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M1_SHELL
                CASE (K_M5M2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M2_SHELL
                CASE (K_M5M3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M3_SHELL
                CASE (K_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE (K_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE (L1_L2L2_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = L2_SHELL
                CASE (L1_L2L3_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = L3_SHELL
                CASE (L1_L2M1_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M1_SHELL
                CASE (L1_L2M2_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M2_SHELL
                CASE (L1_L2M3_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M3_SHELL
                CASE (L1_L2M4_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M4_SHELL
                CASE (L1_L2M5_AUGER)
                        shell_new1 = L2_SHELL
                        shell_new2 = M5_SHELL
                CASE (L1_L3L2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = L2_SHELL
                CASE (L1_L3L3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = L3_SHELL
                CASE (L1_L3M1_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M1_SHELL
                CASE (L1_L3M2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M2_SHELL
                CASE (L1_L3M3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M3_SHELL
                CASE (L1_L3M4_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M4_SHELL
                CASE (L1_L3M5_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M5_SHELL
                CASE (L1_M1L2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = L2_SHELL
                CASE (L1_M1L3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = L3_SHELL
                CASE (L1_M1M1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M1_SHELL
                CASE (L1_M1M2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M2_SHELL
                CASE (L1_M1M3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M3_SHELL
                CASE (L1_M1M4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M4_SHELL
                CASE (L1_M1M5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M5_SHELL
                CASE (L1_M2L2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = L2_SHELL
                CASE (L1_M2L3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = L3_SHELL
                CASE (L1_M2M1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M1_SHELL
                CASE (L1_M2M2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M2_SHELL
                CASE (L1_M2M3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M3_SHELL
                CASE (L1_M2M4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M4_SHELL
                CASE (L1_M2M5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M5_SHELL
                CASE (L1_M3L2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = L2_SHELL
                CASE (L1_M3L3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = L3_SHELL
                CASE (L1_M3M1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M1_SHELL
                CASE (L1_M3M2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M2_SHELL
                CASE (L1_M3M3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M3_SHELL
                CASE (L1_M3M4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M4_SHELL
                CASE (L1_M3M5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M5_SHELL
                CASE (L1_M4L2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = L2_SHELL
                CASE (L1_M4L3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = L3_SHELL
                CASE (L1_M4M1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M1_SHELL
                CASE (L1_M4M2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M2_SHELL
                CASE (L1_M4M3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M3_SHELL
                CASE (L1_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE (L1_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE (L1_M5L2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = L2_SHELL
                CASE (L1_M5L3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = L3_SHELL
                CASE (L1_M5M1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M1_SHELL
                CASE (L1_M5M2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M2_SHELL
                CASE (L1_M5M3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M3_SHELL
                CASE (L1_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE (L1_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE (L2_L3L3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = L3_SHELL
                CASE (L2_L3M1_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M1_SHELL
                CASE (L2_L3M2_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M2_SHELL
                CASE (L2_L3M3_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M3_SHELL
                CASE (L2_L3M4_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M4_SHELL
                CASE (L2_L3M5_AUGER)
                        shell_new1 = L3_SHELL
                        shell_new2 = M5_SHELL
                CASE (L2_M1L3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = L3_SHELL
                CASE (L2_M1M1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M1_SHELL
                CASE (L2_M1M2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M2_SHELL
                CASE (L2_M1M3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M3_SHELL
                CASE (L2_M1M4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M4_SHELL
                CASE (L2_M1M5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M5_SHELL
                CASE (L2_M2L3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = L3_SHELL
                CASE (L2_M2M1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M1_SHELL
                CASE (L2_M2M2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M2_SHELL
                CASE (L2_M2M3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M3_SHELL
                CASE (L2_M2M4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M4_SHELL
                CASE (L2_M2M5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M5_SHELL
                CASE (L2_M3L3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = L3_SHELL
                CASE (L2_M3M1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M1_SHELL
                CASE (L2_M3M2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M2_SHELL
                CASE (L2_M3M3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M3_SHELL
                CASE (L2_M3M4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M4_SHELL
                CASE (L2_M3M5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M5_SHELL
                CASE (L2_M4L3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = L3_SHELL
                CASE (L2_M4M1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M1_SHELL
                CASE (L2_M4M2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M2_SHELL
                CASE (L2_M4M3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M3_SHELL
                CASE (L2_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE (L2_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE (L2_M5L3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = L3_SHELL
                CASE (L2_M5M1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M1_SHELL
                CASE (L2_M5M2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M2_SHELL
                CASE (L2_M5M3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M3_SHELL
                CASE (L2_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE (L2_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE (L3_M1M1_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M1_SHELL
                CASE (L3_M1M2_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M2_SHELL
                CASE (L3_M1M3_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M3_SHELL
                CASE (L3_M1M4_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M4_SHELL
                CASE (L3_M1M5_AUGER)
                        shell_new1 = M1_SHELL
                        shell_new2 = M5_SHELL
                CASE (L3_M2M1_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M1_SHELL
                CASE (L3_M2M2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M2_SHELL
                CASE (L3_M2M3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M3_SHELL
                CASE (L3_M2M4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M4_SHELL
                CASE (L3_M2M5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M5_SHELL
                CASE (L3_M3M1_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M1_SHELL
                CASE (L3_M3M2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M2_SHELL
                CASE (L3_M3M3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M3_SHELL
                CASE (L3_M3M4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M4_SHELL
                CASE (L3_M3M5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M5_SHELL
                CASE (L3_M4M1_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M1_SHELL
                CASE (L3_M4M2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M2_SHELL
                CASE (L3_M4M3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M3_SHELL
                CASE (L3_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE (L3_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE (L3_M5M1_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M1_SHELL
                CASE (L3_M5M2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M2_SHELL
                CASE (L3_M5M3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M3_SHELL
                CASE (L3_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE (L3_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE (M1_M2M2_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M2_SHELL
                CASE (M1_M2M3_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M3_SHELL
                CASE (M1_M2M4_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M4_SHELL
                CASE (M1_M2M5_AUGER)
                        shell_new1 = M2_SHELL
                        shell_new2 = M5_SHELL
                CASE (M1_M3M2_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M2_SHELL
                CASE (M1_M3M3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M3_SHELL
                CASE (M1_M3M4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M4_SHELL
                CASE (M1_M3M5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M5_SHELL
                CASE (M1_M4M2_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M2_SHELL
                CASE (M1_M4M3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M3_SHELL
                CASE (M1_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE (M1_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE (M1_M5M2_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M2_SHELL
                CASE (M1_M5M3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M3_SHELL
                CASE (M1_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE (M1_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE (M2_M3M3_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M3_SHELL
                CASE (M2_M3M4_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M4_SHELL
                CASE (M2_M3M5_AUGER)
                        shell_new1 = M3_SHELL
                        shell_new2 = M5_SHELL
                CASE (M2_M4M3_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M3_SHELL
                CASE (M2_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE (M2_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE (M2_M5M3_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M3_SHELL
                CASE (M2_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE (M2_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE (M3_M4M4_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M4_SHELL
                CASE (M3_M4M5_AUGER)
                        shell_new1 = M4_SHELL
                        shell_new2 = M5_SHELL
                CASE (M3_M5M4_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M4_SHELL
                CASE (M3_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
                CASE (M4_M5M5_AUGER)
                        shell_new1 = M5_SHELL
                        shell_new2 = M5_SHELL
        ENDSELECT

        !so now we have the shells involved...
        !start with the first photon (parent)...
        !but allocate the offspring already...
        ALLOCATE(photon%offspring)
        ALLOCATE(photon%offspring%mus(inputF%composition%n_layers))
        photon%offspring%options = photon%options
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
                photon%energy,line_new,photon%options%use_self_enhancement) .EQ. 0_C_INT) photon%energy = 0.0_C_DOUBLE

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
                                WRITE (*,'(A)') 'cosalfa exception detected'
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
                photon%offspring%energy,line_new,photon%offspring%options%use_self_enhancement) .EQ. 0_C_INT)&
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
                        cosalfa = DOT_PRODUCT(photon%offspring%elecv, photon%offspring%dirv)

                        IF (ABS(cosalfa) .GT. 1.0) THEN
                                WRITE (*,'(A)') 'cosalfa exception detected'
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
        L3_SHELL) .AND. photon%options%use_M_lines) THEN
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
        photon%options%use_M_lines == 0) RETURN

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
        energy,line_new,photon%options%use_self_enhancement) .EQ. 0_C_INT) RETURN

        !leave if energy is too low
        IF (energy .LE. energy_threshold) RETURN
        
        !create offspring
        ALLOCATE(photon%offspring)
        !take over its history!
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

#if DEBUG == 1
        WRITE (6,'(A,I4)') 'line_new: ',line_new
#endif


        cosalfa = DOT_PRODUCT(photon%offspring%elecv, photon%offspring%dirv)

        IF (ABS(cosalfa) .GT. 1.0) THEN
                WRITE (*,'(A)') 'cosalfa exception detected'
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

        ASSOCIATE (hdf5_Z => inputF%composition%layers&
                (photon%current_layer)%xmi_hdf5_Z_local&
                (photon%current_element_index)%Ptr)

        !K0K = 1.0_C_DOUBLE + (1.0_C_DOUBLE-COS(theta_i))*photon%energy/XMI_MEC2
        
        !convert to eV
        energy = photon%energy*1000.0_C_DOUBLE
        c_lamb0 = c/(energy)

        sth2 = SIN(theta_i/2.0_C_DOUBLE)

        DO
                r = fgsl_rng_uniform(rng)
                pos = findpos(hdf5_Z%RandomNumbers, r)

                pz = interpolate_simple([hdf5_Z%RandomNumbers(pos),&
                hdf5_Z%DopplerPz_ICDF(pos)],[hdf5_Z%RandomNumbers(pos+1),&
                hdf5_Z%DopplerPz_ICDF(pos+1)], r)

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


        ENDASSOCIATE


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

        !stability problems could arise here...
        !
        !Warning formula in 1993 paper is wrong!!
        !For actual formula consult the PhD thesis of Laszlo Vincze
        !


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
        dirv = [tempsin*COS(phi_i), tempsin*SIN(phi_i),COS(theta_i)]

        photon%dirv = MATMUL(trans_m,dirv)

        CALL normalize_vector(photon%dirv)

        !update theta and phi in photon
        photon%theta = ACOS(photon%dirv(3))

!        IF (photon%dirv(1) .EQ. 0.0_C_DOUBLE) THEN
!                !watch out... if photon%dirv(2) EQ 0.0 then result may be
!                !processor dependent...
!                photon%phi = SIGN(M_PI_2, photon%dirv(2))
!        ELSE
!                photon%phi = ATAN(photon%dirv(2)/photon%dirv(1))
!        ENDIF

        photon%phi = ATAN2(photon%dirv(2),photon%dirv(1))

        RETURN
ENDSUBROUTINE xmi_update_photon_dirv

SUBROUTINE xmi_update_photon_elecv(photon)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
       
        REAL (C_DOUBLE) :: cosalfa, c_alfa, sinalfa,c_ae, c_be

        cosalfa = DOT_PRODUCT(photon%dirv,photon%elecv)

        c_alfa = ACOS(cosalfa)
        sinalfa = SIN(c_alfa)
        c_ae = 1.0_C_DOUBLE/sinalfa
        c_be = -c_ae*cosalfa

        photon%elecv = c_ae * photon%elecv + c_be *photon%dirv

ENDSUBROUTINE xmi_update_photon_elecv

SUBROUTINE xmi_detector_convolute(inputFPtr, hdf5FPtr, channels_noconvPtr,&
channels_convPtr,nchannels) BIND(C,NAME='xmi_detector_convolute')
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN), VALUE :: inputFPtr, hdf5FPtr, channels_noconvPtr
        INTEGER (C_INT), VALUE, INTENT(IN) :: nchannels
        TYPE (C_PTR), INTENT(INOUT) :: channels_convPtr

        TYPE (xmi_hdf5), POINTER :: hdf5F
        TYPE (xmi_input), POINTER :: inputF
        REAL (C_DOUBLE), POINTER, DIMENSION(:) :: channels_noconv,&
        channels_conv, channels_temp
        INTEGER (C_LONG) :: nlim
        REAL (C_DOUBLE) :: a,b
        REAL (C_DOUBLE), PARAMETER :: c =&
        SQRT(2.0_C_DOUBLE)/(2.0_C_DOUBLE*SQRT(2.0_C_DOUBLE*LOG(2.0_C_DOUBLE)))
        REAL (C_DOUBLE), DIMENSION(4096) :: R 
        INTEGER (C_INT) :: I0, I
        REAL (C_DOUBLE) :: E0, E, B0, FWHM, A0, A3, A4, ALFA, X, G, F, my_sum,&
        CBG




        CALL C_F_POINTER(inputFPtr, inputF)
        CALL C_F_POINTER(hdf5FPtr, hdf5F) 
        CALL C_F_POINTER(channels_noconvPtr, channels_noconv,[nchannels])

#if DEBUG == 1
        WRITE (*,'(A,F15.4)') 'channel 223 contents: ', channels_noconv(223)
#endif

        !allocate memory for results
        ALLOCATE(channels_conv(nchannels), channels_temp(nchannels))

        !
        nlim = INT(inputF%detector%max_convolution_energy/inputF%detector%gain)
        IF (nlim .GT. nchannels) nlim = nchannels


        a = inputF%detector%noise**2
        b = (2.3548)**2 * 3.85 *inputF%detector%fano/1000.0

        channels_temp = channels_noconv
        channels_conv = 0.0_C_DOUBLE

#if DEBUG == 1
        WRITE (*,'(A,ES14.6)') 'channels_noconv max: ',MAXVAL(channels_noconv)
        WRITE (*,'(A,ES14.6)') 'channels_temp max: ',MAXVAL(channels_temp)
        WRITE (*,'(A,ES14.6)') 'channels_conv max: ',MAXVAL(channels_conv)
#endif


        !escape peak
        IF (inputF%detector%detector_type .EQ. XMI_DETECTOR_SILI) THEN
                CALL xmi_detector_escape_SiLi(channels_temp, inputF)
        ELSE
                WRITE (*,'(A)') 'Unsupported detector type'
                CALL EXIT(1)
        ENDIF

#if DEBUG == 1
        WRITE (*,'(A,ES14.6)') 'channels_temp max after escape: ',MAXVAL(channels_temp)
        WRITE (*,'(A,F14.6)') 'a: ',a
        WRITE (*,'(A,F14.6)') 'b: ',b
        WRITE (*,'(A,I)') 'nlim: ',nlim
#endif

        R = 0.0_C_DOUBLE

!!!$omp parallel do default(private) shared(a,b,inputF,I0,nlim,nchannels)
        DO I0=1,nlim
                E0 = inputF%detector%zero + inputF%detector%gain*I0
                FWHM = SQRT(a+b*E0)
                B0=C*FWHM
                A0 = 1.0_C_DOUBLE/(B0*M_SQRTPI)
#if DEBUG == 1
                IF (I0 .EQ. 100) THEN
                WRITE (*,'(A,F14.5)') 'E0: ',E0
                WRITE (*,'(A,F14.5)') 'FWHM: ',FWHM
                WRITE (*,'(A,F14.5)') 'B0: ',B0
                WRITE (*,'(A,F14.5)') 'A0: ',A0
                ENDIF
#endif


                !IF (I0 .LE. 1800) THEN
                        A3=2.73E-3*EXP(-0.21*E0)+1.E-4_C_DOUBLE
                        A4=0.000188*EXP(-0.00296*(E0**0.763))+&
                          1.355E-5*EXP(0.968*(E0**0.498))
                        ALFA=1.179*EXP(8.6E-4*(E0**1.877))-&
                          7.793*EXP(-3.81*(E0**(-0.0716)))
                !ENDIF
                my_sum = 0.0_C_DOUBLE
                DO I=1,I0+100
                        IF (I .GT. nchannels) THEN
                                EXIT
                        ENDIF
                        E = inputF%detector%zero + inputF%detector%gain*I
                        X=(E-E0)/B0
                        G=EXP(-X*X)
                        F=M_PI*ERFC(X)
                        R(I)= A0*G+1.0_C_DOUBLE*(2.7_C_DOUBLE*A3+15.0_C_DOUBLE*A4*EXP(ALFA*(E-E0)))*F       
#if DEBUG == 1
                        IF (I .EQ. 150 .AND. I0 .EQ. 100) THEN
                                WRITE (*,'(A,F14.5)') 'R(I): ',R(I) 
                                WRITE (*,'(A,F14.5)') 'F: ',F 
                        ENDIF
#endif
                        my_sum = my_sum + R(I)
                ENDDO
                !my_sum = SUM(R)
                DO I=1,I0+200
                        IF (I .GT. nchannels) THEN
                                EXIT
                        ENDIF
                        channels_conv(I)=channels_conv(I)+R(I)*channels_temp(I0)/my_sum
                ENDDO

        ENDDO
!!!omp end parallel do
!        my_sum = SUM(channels_conv(1:nlim))
!        DO I=1, NLIM
!                E = inputF%detector%gain*I
!                CBG=EXP(1./(0.15_C_DOUBLE+1.4E-2_C_DOUBLE*(E-1.0_C_DOUBLE)))*my_sum/1.0E7
!                channels_conv(I)=channels_conv(I)+CBG*0.4_C_DOUBLE
!        ENDDO

#if DEBUG == 1
        WRITE (*,'(A,F15.4)') 'channel 223 contents after conv: ', channels_conv(223)
        WRITE (*,'(A,ES14.6)') 'channels_temp max: ',MAXVAL(channels_temp)
        WRITE (*,'(A,ES14.6)') 'channels_conv max: ',MAXVAL(channels_conv)
#endif
        DEALLOCATE(channels_temp)
        channels_convPtr = C_LOC(channels_conv)

        RETURN
ENDSUBROUTINE xmi_detector_convolute

SUBROUTINE xmi_detector_escape_SiLi(channels_conv, inputF)
        TYPE (xmi_input), INTENT(IN) :: inputF
        REAL (C_DOUBLE), INTENT(INOUT),DIMENSION(:) :: channels_conv

        REAL (C_DOUBLE) :: omegaK, E_Si_Ka, E_Si_Kb, E_Si_Kedge, RR_Si_Ka,&
        RR_Si_Kb, const, mu_si
        INTEGER (C_LONG) :: i,i_esc_ka, i_esc_kb
        REAL (C_DOUBLE) :: e,mu_e,esc_rat

        omegaK = FluorYield(14, K_SHELL)
        E_Si_Ka  = LineEnergy(14, KA_LINE)
        E_Si_Kb  = LineEnergy(14, KB_LINE)
        E_Si_Kedge = EdgeEnergy(14, K_SHELL)
        RR_Si_Ka = RadRate(14,KA_LINE)
        RR_Si_Kb = RadRate(14,KB_LINE)
        const = omegaK*(1.0_C_DOUBLE-1.0_C_DOUBLE/JumpFactor(14,K_SHELL))
        mu_si = CS_Total_Kissel(14,REAL(E_Si_Ka,KIND=C_FLOAT))

        DO i=1,SIZE(channels_conv)
                e = (REAL(i)+0.5_C_DOUBLE)*inputF%detector%gain+&
                        inputF%detector%zero
                IF (e .LT. E_Si_Kedge) CYCLE
                mu_e = CS_Total_Kissel(14,REAL(e,KIND=C_FLOAT))
                esc_rat = 0.5_C_DOUBLE*(1.0_C_DOUBLE -&
                mu_si/mu_e*LOG(1.0_C_DOUBLE + mu_e/mu_si))
                esc_rat = const*esc_rat/(1.0_C_DOUBLE-const*esc_rat)
                
                i_esc_ka = &
                INT((e-E_Si_Ka-inputF%detector%zero)/inputF%detector%gain)
                i_esc_kb = &
                INT((e-E_Si_Kb-inputF%detector%zero)/inputF%detector%gain)

                channels_conv(i_esc_ka) = &
                channels_conv(i_esc_ka)+esc_rat*channels_conv(i)*RR_Si_Ka
                channels_conv(i_esc_kb) = &
                channels_conv(i_esc_kb)+esc_rat*channels_conv(i)*RR_Si_Kb
                channels_conv(i) = (1.0_C_DOUBLE-esc_rat)*channels_conv(i)

        ENDDO


        RETURN
ENDSUBROUTINE xmi_detector_escape_SiLi

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

FUNCTION xmi_fluorescence_line_check(rng, shell, element, energy, line_rv,&
        self_enhancement) RESULT(rv)
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN) :: shell, element
        TYPE (fgsl_rng), INTENT(IN) :: rng
        REAL (C_DOUBLE), INTENT(INOUT) :: energy
        INTEGER (C_INT), INTENT(INOUT) :: line_rv
        INTEGER (C_INT), INTENT(IN) :: self_enhancement
        INTEGER (C_INT) :: rv
        REAL (C_DOUBLE) :: r, sumz
        LOGICAL :: line_found
        INTEGER (C_INT) :: line_first, line_last, line

        rv = 0


        !so we have fluorescence... but which line?
        r = fgsl_rng_uniform(rng)
        sumz = 0.0_C_FLOAT
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
                IF (self_enhancement .EQ. 1_C_INT) THEN
                        CALL xmi_self_enhancement(rng, element, shell, line, energy)
                ELSE
                        energy = LineEnergy(element, line)
                ENDIF
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

SUBROUTINE xmi_self_enhancement(rng, element, shell, line, energy) 
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN) :: element, shell, line
        REAL (C_DOUBLE), INTENT(INOUT) :: energy
        TYPE (fgsl_rng), INTENT(IN) :: rng
        INTEGER (C_INT) :: shell_new
        REAL (C_DOUBLE) :: hwhm


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
                CASE (KN1_LINE)
                        shell_new = N1_SHELL
                CASE (KN2_LINE)
                        shell_new = N2_SHELL
                CASE (KN3_LINE)
                        shell_new = N3_SHELL
                CASE (KN4_LINE)
                        shell_new = N4_SHELL
                CASE (KN5_LINE)
                        shell_new = N5_SHELL
                CASE (KN6_LINE)
                        shell_new = N6_SHELL
                CASE (KN7_LINE)
                        shell_new = N7_SHELL
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
                CASE (L1N1_LINE)
                        shell_new = N1_SHELL
                CASE (L1N2_LINE)
                        shell_new = N2_SHELL
                CASE (L1N3_LINE)
                        shell_new = N3_SHELL
                CASE (L1N4_LINE)
                        shell_new = N4_SHELL
                CASE (L1N5_LINE)
                        shell_new = N5_SHELL
                CASE (L1N6_LINE)
                        shell_new = N6_SHELL
                CASE (L1N7_LINE)
                        shell_new = N7_SHELL
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
                CASE (L2N1_LINE)
                        shell_new = N1_SHELL
                CASE (L2N2_LINE)
                        shell_new = N2_SHELL
                CASE (L2N3_LINE)
                        shell_new = N3_SHELL
                CASE (L2N4_LINE)
                        shell_new = N4_SHELL
                CASE (L2N5_LINE)
                        shell_new = N5_SHELL
                CASE (L2N6_LINE)
                        shell_new = N6_SHELL
                CASE (L2N7_LINE)
                        shell_new = N7_SHELL
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
                CASE (L3N1_LINE)
                        shell_new = N1_SHELL
                CASE (L3N2_LINE)
                        shell_new = N2_SHELL
                CASE (L3N3_LINE)
                        shell_new = N3_SHELL
                CASE (L3N4_LINE)
                        shell_new = N4_SHELL
                CASE (L3N5_LINE)
                        shell_new = N5_SHELL
                CASE (L3N6_LINE)
                        shell_new = N6_SHELL
                CASE (L3N7_LINE)
                        shell_new = N7_SHELL
        ENDSELECT

        hwhm = 0.5_C_DOUBLE*(AtomicLevelWidth(element,shell)+&
                    AtomicLevelWidth(element,shell_new))
        energy = fgsl_ran_cauchy(rng,hwhm)+LineEnergy(element,line) 

        IF (energy .LT. energy_threshold .OR. energy .GT. 99.0_C_DOUBLE) energy = 0.0_C_DOUBLE

        RETURN
ENDSUBROUTINE xmi_self_enhancement

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

#if DEBUG == 0
!        WRITE (*,'(A,3F12.5)') 'detector_point: ',detector_point
!        CALL EXIT(1)
#endif

        photon%dirv = photon%coords-detector_point
        CALL normalize_vector(photon%dirv)

        IF (DOT_PRODUCT(photon%dirv, inputF%geometry%n_detector_orientation)&
        .GE. 0.0_C_DOUBLE) photon%dirv = photon%dirv*-1_C_DOUBLE 

        RETURN
ENDSUBROUTINE xmi_force_photon_to_detector


SUBROUTINE xmi_variance_reduction(photon, inputF, hdf5F, rng)
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
        INTEGER (C_INT) :: step_do_max, step_do_dir,i,j,line_new
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: distances, mus
        REAL (C_DOUBLE) :: Pconv, Pdir, Pesc, Pesc_comp, Pesc_rayl, Pdir_fluo
        REAL (C_DOUBLE) :: temp_murhod, energy_compton, energy_fluo, dotprod
        INTEGER (C_INT) :: line_last


#if DEBUG == 1
        WRITE (*,'(A)') 'Entering variance reduction'
        WRITE (*,'(A,F12.4)') 'photon%energy: ',photon%energy
#endif


        !ignore negative energies
        IF (photon%energy .LE. energy_threshold) RETURN



        !select random coordinate on detector surface
        radius = fgsl_rng_uniform(rng)*inputF%detector%detector_radius
        theta = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)

        detector_point(1) = 0.0_C_DOUBLE
        detector_point(2) = COS(theta)*radius
        detector_point(3) = SIN(theta)*radius

        !work in detector coordinate system
        line_coll%point = MATMUL(inputF%detector%n_detector_orientation_inverse, &
                photon%coords-inputF%geometry%p_detector_window) 
        dirv = MATMUL(inputF%detector%n_detector_orientation_inverse, &
                photon%dirv) 
        line_coll%dirv = detector_point-line_coll%point 

        CALL normalize_vector(dirv)
        CALL normalize_vector(line_coll%dirv)

        !if it hits the collimator -> game over
        IF (inputF%detector%collimator_present .EQ. .TRUE.) THEN
                !there is a collimator
                plane_coll%point = detector_point
                plane_coll%point(1)=inputF%detector%collimator_height
                plane_coll%normv = [1.0_C_DOUBLE, 0.0_C_DOUBLE, 0.0_C_DOUBLE]

                IF (xmi_intersection_plane_line(plane_coll, line_coll,&
                point_coll) == 0) CALL EXIT(1)

                point_coll(1) = 0.0_C_DOUBLE
                
                IF (norm(point_coll) .GT. inputF%detector%detector_radius) RETURN
        ENDIF
        new_dirv_coords = MATMUL(inputF%detector%n_detector_orientation_new,line_coll%dirv)


        !so we survived the collimator...
        !calculate the angle between the photon*dirv and line_coll%dirv
        dotprod = DOT_PRODUCT(dirv, line_coll%dirv)
        IF (dotprod .GE. 1.0 .OR. dotprod .LT. -1.0) THEN
                WRITE(*,'(A,3F12.5)') 'dirv: ',dirv
                WRITE(*,'(A,3F12.5)') 'line_coll%dirv: ',line_coll%dirv
        ENDIF
        theta = ACOS(dotprod)
        !WRITE (*,'(A,F12.5)') 'dotprod: ',dotprod

#if DEBUG == 1
        WRITE (*,'(A,F18.10)') 'theta: ',theta
        WRITE (*,'(A,F18.10)') 'DOTPRODUCT n and dirv: ',&
        DOT_PRODUCT(inputF%detector%n_sample_orientation_det,line_coll%dirv)
        WRITE (*,'(A,F18.10)') 'DOTPRODUCT n and dirv: ',&
        DOT_PRODUCT(inputF%geometry%n_sample_orientation,new_dirv_coords)
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
                temp_coords = intersect
        ENDDO

#if DEBUG == 1
        WRITE (*,'(A,F18.10)') 'theta: ',theta
        WRITE (*,'(A,ES14.5)') 'distances',distances(1)
        WRITE (*,'(A,3ES14.5)') 'photon coords',photon%coords(:)
        WRITE (*,'(A,3ES14.5)') 'detector_point',MATMUL(inputF%detector%n_detector_orientation_new,detector_point)+&
        inputF%geometry%p_detector_window
#endif

        !okay now the actual variance reduction stuff...
        ASSOCIATE (layer => &
        inputF%composition%layers(photon%current_layer),n_ia => &
        photon%n_interactions)
        !first the element independent part
        temp_murhod = 0.0_C_DOUBLE
        DO j=photon%current_layer,step_do_max,step_do_dir
                temp_murhod = temp_murhod + photon%mus(j)*&
                inputF%composition%layers(j)%density*distances(j)
        ENDDO
        Pesc_rayl = EXP(-temp_murhod) 


        Pdir_fluo = inputF%detector%detector_solid_angle/4.0_C_DOUBLE/M_PI

        IF (photon%options%use_M_lines .EQ. 1) THEN 
                line_last = M5P5_LINE 
        ELSE 
                line_last = L3Q1_LINE
        ENDIF

        ALLOCATE(mus(inputF%composition%n_layers))

        var_red: DO i=1,layer%n_elements
                !
                !       starting with RAYLEIGH
                !
                Pconv = layer%weight(i)/photon%mus(photon%current_layer)
                Pdir = inputF%detector%detector_solid_angle&
                *DCS_Rayl(layer%Z(i),REAL(photon%energy,KIND=C_FLOAT),REAL(theta,KIND=C_FLOAT))

                !find position in history
                photon%variance_reduction(photon%current_layer,n_ia)%weight(i,383+1)&
                = Pconv*Pdir*Pesc_rayl
                photon%variance_reduction(photon%current_layer,n_ia)%energy(i,383+1)&
                = photon%energy

                !
                !       moving on with COMPTON
                !
                !calculate compton energy
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
                Pdir = inputF%detector%detector_solid_angle&
                *DCS_Compt(layer%Z(i),REAL(photon%energy,KIND=C_FLOAT),REAL(theta,KIND=C_FLOAT))
                photon%variance_reduction(photon%current_layer,n_ia)%weight(i,383+2)&
                = Pconv*Pdir*Pesc_comp
                photon%variance_reduction(photon%current_layer,n_ia)%energy(i,383+2)&
                = energy_compton

                !
                !      and finishing with FLUORESCENCE 
                !
                DO line_new=KL1_LINE,line_last,-1
                        !needs to be checked for each line... will take
                        !forever...
                        energy_fluo = LineEnergy(layer%Z(i), line_new)
                        IF (energy_fluo .LT. energy_threshold) CYCLE
                        !this will not be suitable for now if there are cascade
                        !effects... but at least it will be much better than
                        !Laszlo's implementation which seems to ignore
                        !Coster-Kronig transitions
                        Pconv = &
                        layer%weight(i)*xmi_CS_FluorLine(layer%Z(i),line_new,REAL(photon%energy,KIND=C_FLOAT))/&
                        !layer%weight(i)*CS_FluorLine_Kissel_cascade(layer%Z(i),line_new,REAL(photon%energy,KIND=C_FLOAT))/&
                        photon%mus(photon%current_layer)
                        mus=xmi_mu_calc(inputF%composition,&
                        energy_fluo)
                        temp_murhod = 0.0_C_DOUBLE
                        DO j=photon%current_layer,step_do_max,step_do_dir
                                temp_murhod = temp_murhod + mus(j)*&
                                inputF%composition%layers(j)%density*distances(j)
                        ENDDO
                        Pesc = EXP(-temp_murhod) 
                        photon%variance_reduction(photon%current_layer,n_ia)%weight(i,ABS(line_new))&
                        = Pconv*Pdir_fluo*Pesc
                        photon%variance_reduction(photon%current_layer,n_ia)%energy(i,ABS(line_new))&
                        = energy_fluo
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


        ENDASSOCIATE


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
        INTEGER :: i

        ASSOCIATE (hdf5_Z => inputF%composition%layers&
                (photon%current_layer)%xmi_hdf5_Z_local&
                (current_element_index)%Ptr)

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
                pos = findpos(hdf5_Z%RandomNumbers, r)

                pz = interpolate_simple([hdf5_Z%RandomNumbers(pos),&
                hdf5_Z%DopplerPz_ICDF(pos)],[hdf5_Z%RandomNumbers(pos+1),&
                hdf5_Z%DopplerPz_ICDF(pos+1)], r)

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

        ENDASSOCIATE


ENDSUBROUTINE xmi_update_photon_energy_compton_var_red


ENDMODULE
