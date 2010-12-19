MODULE xmimsim_main

USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux
USE :: hdf5
USE :: omp_lib
USE :: fgsl



!global variables, defined in C, external for Fortran



!global variables
INTEGER (C_INT), PARAMETER ::  RAYLEIGH_INTERACTION = 1
INTEGER (C_INT), PARAMETER ::  COMPTON_INTERACTION = 2
INTEGER (C_INT), PARAMETER ::  PHOTOELECTRIC_INTERACTION = 3

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
                WRITE (*,*) 'Error opening HDF5 file ',xmi_hdf5_fileFF
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
nchannels) BIND(C,NAME='xmi_main_msim') RESULT(rv)
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN), VALUE :: inputFPtr, hdf5FPtr, channelsPtr
        INTEGER (C_INT), VALUE, INTENT(IN) :: n_mpi_hosts, nchannels
        INTEGER (C_INT) :: rv 

        TYPE (xmi_hdf5), POINTER :: hdf5F
        TYPE (xmi_input), POINTER :: inputF
        REAL (C_DOUBLE), POINTER, DIMENSION(:) :: channelsF
        INTEGER :: max_threads, thread_num

        TYPE (fgsl_rng_type) :: rng_type
        TYPE (fgsl_rng) :: rng
        INTEGER (C_LONG), POINTER, DIMENSION(:) :: seeds
        INTEGER (C_LONG) :: i,j
        TYPE (xmi_photon), POINTER :: photon
        REAL (C_DOUBLE) :: hor_ver_ratio
        INTEGER (C_LONG) :: n_photons
        REAL (C_DOUBLE) :: iv_start_energy, iv_end_energy
        INTEGER :: ipol
        REAL (C_DOUBLE) :: cosalfa, c_alfa, c_ae, c_be
        INTEGER (C_LONG) :: photons_simulated, detector_hits, rayleighs,&
        comptons, einsteins
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: initial_mus
        INTEGER (C_INT) :: channel
        REAL (C_DOUBLE), DIMENSION(:), ALLOCATABLE :: channelsFF 
        !begin...
        
        CALL SetErrorMessages(0)

        rv = 0
        photons_simulated = 0
        detector_hits = 0
        rayleighs = 0
        comptons = 0
        einsteins = 0


        CALL C_F_POINTER(inputFPtr, inputF)
        CALL C_F_POINTER(hdf5FPtr, hdf5F) 
        CALL C_F_POINTER(channelsPtr, channelsF,[nchannels])

        channelsF = 0.0_C_DOUBLE
        channelsFF = channelsF
        
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




!$omp parallel default(shared) private(rng,thread_num,i,j,photon,hor_ver_ratio,n_photons,iv_start_energy, iv_end_energy,ipol,cosalfa, c_alfa, c_ae, c_be, initial_mus,channel) reduction(+:photons_simulated,detector_hits, channelsFF,rayleighs,comptons,einsteins)

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
                hor_ver_ratio = &
                exc%discrete(i)%horizontal_intensity/ &
                (exc%discrete(i)%vertical_intensity+ &
                exc%discrete(i)%horizontal_intensity)
                n_photons = inputF%general%n_photons_line/omp_get_num_threads()/n_mpi_hosts
#if DEBUG == 2
!$omp critical                        
                WRITE (*,'(A,I)') 'n_photons: ',n_photons
!$omp end critical                        
#endif
                !Calculate initial mu's
                !ALLOCATE(initial_mus(inputF%composition%n_layers))
                initial_mus = xmi_mu_calc(inputF%composition,&
                exc%discrete(i)%energy)
#if DEBUG == 2
!$omp master             
                DO j=1,SIZE(initial_mus)
                        WRITE (*,'(F14.6)') initial_mus(j)
                ENDDO
!$omp end master             
#endif

                DO j=1,n_photons
                        !Allocate the photon
                        ALLOCATE(photon)
                        photon%n_interactions=0
                        NULLIFY(photon%offspring)
                        !Calculate energy with rng
                        photon%energy = exc%discrete(i)%energy 
                        photon%energy_changed=.FALSE.
                        photon%mus = initial_mus
                        photon%current_layer = 1
                        photon%detector_hit = .FALSE.

                        ipol = MOD(n_photons,2)

                        !Calculate its initial coordinates and direction
                        CALL xmi_coords_dir(rng,exc%discrete(i), inputF%geometry,&
                        photon)

                        !Calculate its weight and electric field...
                        IF (ipol .EQ. 0) THEN
                                !horizontal
                                photon%weight = exc%discrete(i)%horizontal_intensity/inputF%general%n_photons_line 
                                photon%elecv(1) = 1.0_C_DOUBLE
                                photon%elecv(2) = 0.0_C_DOUBLE
                                photon%elecv(3) = 0.0_C_DOUBLE
                        ELSE
                                !vertical
                                photon%weight = exc%discrete(i)%vertical_intensity/inputF%general%n_photons_line 
                                photon%elecv(1) = 0.0_C_DOUBLE
                                photon%elecv(2) = 1.0_C_DOUBLE
                                photon%elecv(3) = 0.0_C_DOUBLE
                        ENDIF

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

                        IF (photon%energy .GE. energy_threshold) THEN
                                channel = INT((photon%energy - &
                                inputF%detector%zero)/inputF%detector%gain)
                        ELSE
                                channel = 0
                        ENDIF

!!!$omp critical                        
                        photons_simulated = photons_simulated + 1
                        IF (photon%detector_hit .EQ. .TRUE.) THEN
                                detector_hits = detector_hits + 1
!
!
!                              Add to channelsF
!
!
                                IF (channel .GT. 0 .AND. channel .LE. nchannels) THEN
                                        channelsFF(channel) = channelsFF(channel)+photon%weight
                                ENDIF
                                SELECT CASE (photon%last_interaction)
                                        CASE (RAYLEIGH_INTERACTION)
                                                rayleighs = rayleighs + 1
                                        CASE (COMPTON_INTERACTION)
                                                comptons = comptons + 1
                                        CASE (PHOTOELECTRIC_INTERACTION)
                                                einsteins = einsteins + 1
                                ENDSELECT
                        ENDIF

!!!$omp end critical                        
                        DEALLOCATE(photon%mus)
                        DEALLOCATE(photon)
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
        WRITE (*,'(A,I)') 'Rayleighs: ',rayleighs
        WRITE (*,'(A,I)') 'Comptons: ',comptons
        WRITE (*,'(A,I)') 'Photoelectric: ',einsteins
#endif

        channelsF = channelsFF

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

        IF (photon%dirv(1) .EQ. 0.0_C_DOUBLE) THEN
                !watch out... if photon%dirv(2) EQ 0.0 then result may be
                !processor dependent...
                photon%phi = SIGN(M_PI_2, photon%dirv(2))
        ELSE
                photon%phi = ATAN(photon%dirv(2)/photon%dirv(1))
        ENDIF

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

CALL h5open_f(h5error)


CALL SetErrorMessages(0)

ALLOCATE(rayleigh_theta(maxz, nintervals_e, nintervals_r),&
compton_theta(maxz, nintervals_e, nintervals_r),&
energies(nintervals_e), rs(nintervals_r),&
thetas(nintervals_theta),&
doppler_pz(maxz, nintervals_r), pzs(nintervals_pz), STAT=stat, errmsg=error_message )

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

!$OMP PARALLEL DEFAULT(shared) PRIVATE(j,k,l,m,trapez,temp_sum,sumz,energies_flt,temp_energy,trapez2)

#if DEBUG == 2
WRITE(6,*) 'multiple allocs'
#endif
ALLOCATE(trapez(nintervals_theta-1))
ALLOCATE(trapez2(nintervals_pz-1))
ALLOCATE(sumz(nintervals_theta-1))
!$OMP DO

Zloop:DO i=1,maxz 
#if DEBUG == 1
!WRITE (6,*) 'Element: ',i
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

!$OMP PARALLEL DEFAULT(shared) PRIVATE(j,k,l,m,cdfs)

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
                IF (photon%energy .LT. energy_threshold .OR.&
                photon%n_interactions .EQ.&
                inputF%general%n_interactions_trajectory) THEN
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
                        DEALLOCATE(photon%mus)
                        photon%mus = xmi_mu_calc(inputF%composition,&
                        photon%energy)
                        photon%energy_changed = .FALSE.
                ENDIF
        
                IF (photon%dirv(3) .GT. 0.0_C_DOUBLE) THEN
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
#if DEBUG == 2
                        WRITE (*,'(A,F12.5)') 'dist: ', dist
                        WRITE (*,'(A,3F12.5)') 'intersect: ', intersect
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
                       
#if DEBUG == 2
                        !WRITE (*,'(A,F12.5)') 'tempexp: ', tempexp
                        WRITE (*,'(A,F12.5)') 'min_random_layer: ',&
                        min_random_layer
                        WRITE (*,'(A,F12.5)') 'max_random_layer: ',&
                        max_random_layer
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
                        photon%last_interaction = PHOTOELECTRIC_INTERACTION
                        rv_interaction = xmi_simulate_photon_fluorescence(photon,&
                        inputF, hdf5F, rng) 
                ENDIF

                !abort if necessary
                IF (rv_interaction /= 1) THEN
                        RETURN
                ENDIF



                !update number of interactions
                photon%n_interactions = photon%n_interactions + 1
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
        REAL (C_DOUBLE) :: r

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

        !
        !update photon%theta and photon%phi
        !
        CALL xmi_update_photon_dirv(photon, theta_i, phi_i)

        !
        !update electric field
        !
        CALL xmi_update_photon_elecv(photon)

        


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

        !
        !update photon%theta and photon%phi
        !
        CALL xmi_update_photon_dirv(photon, theta_i, phi_i)

        !
        !update electric field
        !
        CALL xmi_update_photon_elecv(photon)

        !
        !for compton, laszlo does a further manipulation, probably has to with
        !change of degree of polarization
        !

        !
        !update energy of photon!!!
        !
        CALL xmi_update_photon_energy_compton(photon, theta_i, rng, inputF,&
        hdf5f)


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
        INTEGER (C_INT) :: rv

        REAL (C_FLOAT) :: photo_total, energy_flt
        REAL (C_FLOAT) :: sumz
        INTEGER (C_INT) :: shell,line_first, line_last, line
        REAL (C_DOUBLE) :: r
        REAL (C_DOUBLE) :: theta_i, phi_i
        
        LOGICAL :: shell_found, line_found

        rv = 0

        !so we've got photo electric effect
        !first is to check which shell got lucky
        energy_flt = REAL(photon%energy,C_FLOAT)
        photo_total = CS_Photo_Total(photon%current_element, energy_flt)

        sumz = 0.0_C_FLOAT
        shell_found = .FALSE.

        !for now let's just look at K- and L-lines
        r = fgsl_rng_uniform(rng)
        DO shell=K_SHELL,L3_SHELL
                sumz = sumz + CS_Photo_Partial(photon%current_element, shell,&
                energy_flt)/photo_total
                IF (r .LT. sumz) THEN
                        shell_found = .TRUE.
                        EXIT
                ENDIF
        ENDDO
        
        IF (.NOT. shell_found) THEN
                ! no shell matches -> probably M or higher...
               photon%energy = 0.0_C_DOUBLE
               rv = 1
               RETURN
        ENDIF

        !so now that we determined the shell to be used, see if we get
        !fluorescence...
        r = fgsl_rng_uniform(rng)
        IF (r .GT. FluorYield(photon%current_element,shell)) THEN
                !no fluorescence but Auger...
               photon%energy = 0.0_C_DOUBLE
               rv = 1
               RETURN
        ENDIF

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
        ENDIF

        DO line=line_first,line_last,-1
                sumz = sumz + RadRate(photon%current_element, line)
                IF (r .LT. sumz) THEN
                        !found it...
                        line_found = .TRUE.
                        EXIT
                ENDIF
        ENDDO

        IF (line_found) THEN
                photon%energy = LineEnergy(photon%current_element, line)
                photon%energy_changed = .TRUE.
        ELSE
                photon%energy = 0.0_C_DOUBLE
                rv = 1
                RETURN
        ENDIF

        !calculate theta and phi
        theta_i = ACOS(2.0_C_DOUBLE*fgsl_rng_uniform(rng)-1.0_C_DOUBLE)
        phi_i = 2.0_C_DOUBLE * M_PI *fgsl_rng_uniform(rng)

        !
        !update photon%theta and photon%phi
        !
        CALL xmi_update_photon_dirv(photon, theta_i, phi_i)

        !
        !update electric field
        !
        CALL xmi_update_photon_elecv(photon)

        rv = 1

        RETURN

ENDFUNCTION xmi_simulate_photon_fluorescence

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
        photon%energy_changed = .TRUE.

        ENDASSOCIATE


        RETURN
ENDSUBROUTINE xmi_update_photon_energy_compton

SUBROUTINE xmi_update_photon_dirv(photon, theta_i, phi_i)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT) :: photon
        REAL (C_DOUBLE), INTENT(INOUT) :: theta_i, phi_i

        REAL (C_DOUBLE) :: cosphi_i, sinphi_i
        REAL (C_DOUBLE) :: costheta_i, sintheta_i
        REAL (C_DOUBLE), DIMENSION(3,3) :: trans_m

        !stability problems could arise here...

        cosphi_i = COS(phi_i)
        sinphi_i = SIN(phi_i)
        costheta_i = COS(theta_i)
        sintheta_i = SIN(theta_i)

        trans_m(1,1) = costheta_i*cosphi_i
        trans_m(1,2) = -sinphi_i
        trans_m(1,3) = sintheta_i*cosphi_i

        trans_m(2,1) = costheta_i*sinphi_i
        trans_m(2,2) = cosphi_i
        trans_m(2,3) = sintheta_i*sinphi_i

        trans_m(3,1) = -sintheta_i
        trans_m(3,2) = 0.0_C_DOUBLE
        trans_m(3,3) = costheta_i

        photon%dirv = MATMUL(trans_m,photon%dirv)

        !update theta and phi in photon
        photon%theta = ACOS(photon%dirv(3))

        IF (photon%dirv(1) .EQ. 0.0_C_DOUBLE) THEN
                !watch out... if photon%dirv(2) EQ 0.0 then result may be
                !processor dependent...
                photon%phi = SIGN(M_PI_2, photon%dirv(2))
        ELSE
                photon%phi = ATAN(photon%dirv(2)/photon%dirv(1))
        ENDIF


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

ENDMODULE
