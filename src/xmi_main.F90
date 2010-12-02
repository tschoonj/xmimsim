MODULE xmimsim_main

USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux
USE :: hdf5
USE :: omp_lib
USE :: fgsl



!global variables, defined in C, external for Fortran



!global variables




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
                WRITE (element, '(I2)') uniqZ(i)
#if DEBUG == 1
                WRITE (*,'(A,A)') 'Reading element: ',element
#endif
                xmi_hdf5F%xmi_hdf5_Zs(i)%Z = uniqZ(i)

                CALL h5gopen_f(file_id,element // '/Theta_ICDF',group_id,error)
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
                CALL h5gopen_f(file_id,element // '/Interaction probabilities',group_id,error)
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
                ASSOCIATE (layers => xmi_inputF%composition%layers)
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


        !close file
        CALL h5fclose_f(file_id,error)

        !close hdf5 fortran interface
        CALL h5close_f(error)

#if DEBUG == 2
        ASSOCIATE (layers => xmi_inputF%composition%layers)
        !create pointers
        DO j=1,SIZE(layers)
                DO k=1,SIZE(layers(j)%Z) 
                        WRITE (*,*) 'Z confirmation: ',&
                        layers(j)%xmi_hdf5_Z_local(k)%Ptr%Z
                ENDDO
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
        INTEGER (C_LONG) :: photons_simulated = 0
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: initial_mus
        !begin...
        
        rv = 0



        CALL C_F_POINTER(inputFPtr, inputF)
        CALL C_F_POINTER(hdf5FPtr, hdf5F) 

        
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




!$omp parallel default(shared) private(rng,thread_num,i,j,photon,hor_ver_ratio,n_photons,iv_start_energy, iv_end_energy,ipol,cosalfa, c_alfa, c_ae, c_be, initial_mus)

!
!
!       Initialize random number generator
!
!
        thread_num = omp_get_thread_num()
        rng_type = fgsl_rng_mt19937

        rng = fgsl_rng_alloc(rng_type)
        CALL fgsl_rng_set(rng,seeds(thread_num))

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





                        DEALLOCATE(photon)
                ENDDO
        ENDDO cont

        disc:DO i=1,exc%n_discrete
                hor_ver_ratio = &
                exc%discrete(i)%horizontal_intensity/ &
                (exc%discrete(i)%vertical_intensity+ &
                exc%discrete(i)%horizontal_intensity)
                n_photons = inputF%general%n_photons_line/omp_get_num_threads()/n_mpi_hosts
#if DEBUG == 1
!$omp critical                        
                WRITE (*,'(A,I)') 'n_photons: ',n_photons
!$omp end critical                        
#endif
                !Calculate initial mu's
                !ALLOCATE(initial_mus(inputF%composition%n_layers))
                initial_mus = xmi_mu_calc(inputF%composition,&
                exc%discrete(i)%energy)
#if DEBUG == 1
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
                        CALL &
                        photon_shift_first_layer(photon,inputF%composition,inputF%geometry)



                        DEALLOCATE(photon)
!$omp critical                        
                        photons_simulated = photons_simulated + 1
!$omp end critical                        
                ENDDO
                DEALLOCATE(initial_mus)
        ENDDO disc 

#undef exc
 !       ENDASSOCIATE


        !cleanup
        CALL fgsl_rng_free(rng)

!$omp end parallel

#if DEBUG == 1
        WRITE (*,'(A,I)') 'Photons simulated: ',photons_simulated
#endif


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
nintervals_theta=100000, nintervals_theta2=200,nintervals_phi=100000
REAL (KIND=C_DOUBLE), PARAMETER :: maxe = 100.0, lowe = 0.1, &
        PI = 3.14159265359,MEC2 = 510.998910
CHARACTER(200) :: error_message

REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:,:) :: &
        rayleigh_theta,compton_theta
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: energies, rs, trapez, thetas,sumz,phis
REAL (KIND=C_FLOAT), ALLOCATABLE, DIMENSION(:), TARGET :: energies2

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


CALL h5open_f(h5error)


CALL SetErrorMessages(0)

ALLOCATE(rayleigh_theta(maxz, nintervals_e, nintervals_r),&
compton_theta(maxz, nintervals_e, nintervals_r),&
energies(nintervals_e), rs(nintervals_r),&
thetas(nintervals_theta), STAT=stat, errmsg=error_message )

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


DO i=1,nintervals_r
        rs(i) = (REAL(i,C_DOUBLE)-1.0)/(nintervals_r-1.0)
ENDDO

#if DEBUG == 2
        WRITE (6,*) 'rs(1): ',rs(1)
        WRITE (6,*) 'rs(nintervals_r): ',rs(nintervals_r)
#endif

DO i=1,nintervals_theta
        thetas(i) = PI/nintervals_theta+(PI-(PI/nintervals_theta))*(REAL(i,C_DOUBLE)-1.0)/(nintervals_theta-1.0)
ENDDO

#if DEBUG == 2
        WRITE (6,*) 'thetas(1): ',thetas(1)
        WRITE (6,*) 'thetas(nintervals_theta): ',thetas(nintervals_theta)
#endif

!#if DEBUG != 1

!CALL OMP_SET_NUM_THREADS(1)

!$OMP PARALLEL DEFAULT(shared) PRIVATE(j,k,l,m,trapez,temp_sum,sumz,energies2,temp_energy)

#if DEBUG == 2
WRITE(6,*) 'multiple allocs'
#endif
ALLOCATE(trapez(nintervals_theta-1))
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
        ALLOCATE(energies2(SIZE(energies)))
        energies2 = REAL(energies,KIND=C_FLOAT)
        !add edge energies
        DO k=K_SHELL,L3_SHELL
                temp_energy = EdgeEnergy(i,k)
                IF (temp_energy > 0.0_C_FLOAT) THEN
                        energies2 = [energies2,&
                         temp_energy+0.00001_C_FLOAT]
                        energies2 = [energies2,&
                         temp_energy-0.00001_C_FLOAT]
                ENDIF
        ENDDO
        

        !SORT them
        CALL qsort(C_LOC(energies2),SIZE(energies2,KIND=C_SIZE_T),&
        INT(KIND(energies2),KIND=C_SIZE_T),C_FUNLOC(C_FLOAT_CMP))
        !calls assign_interaction_prob...
        interaction_probs(i) = energies2

        DO k=1,SIZE(energies2)
                temp_total_cs = CS_Total_Kissel(i,energies2(k))
                interaction_probs(i)%Rayl_and_Compt(k,1) = CS_Rayl(i,energies2(k))/temp_total_cs
                interaction_probs(i)%Rayl_and_Compt(k,2) = CS_Compt(i,energies2(k))/temp_total_cs+&
                  interaction_probs(i)%Rayl_and_Compt(k,1)
        ENDDO

        DEALLOCATE(energies2)


ENDDO Zloop
!$OMP END DO
!$OMP END PARALLEL

!#endif

!Write Theta inverse cdfs to hdf5 file
CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F,file_id,h5error)

!Create groups and fill them up...

!#if DEBUG != 1
DO i=1,maxz
        WRITE(element,'(I2)') i
#if DEBUG == 2
        WRITE (*,*) 'Writing element: ',element
#endif
        !group creation -> element
        CALL h5gcreate_f(file_id,element,group_id,h5error)

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
        thetas(i) = PI/nintervals_theta2+(PI-(PI/nintervals_theta2))*(REAL(i,C_DOUBLE)-1.0)/(nintervals_theta2-1.0)
ENDDO

#if DEBUG == 1
        WRITE (6,*) 'thetas(1): ',thetas(1)
        WRITE (6,*) 'thetas(nintervals_theta2): ',thetas(nintervals_theta2)
#endif

DO i=1,nintervals_phi
        phis(i)=2.0*PI/nintervals_phi+(2.0*PI-(2.0*PI/nintervals_phi))*(REAL(i,C_DOUBLE)-1.0)/(nintervals_phi-1.0)
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

SUBROUTINE photon_shift_first_layer(photon, composition, geometry)
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
        plane%point = [0.0_C_DOUBLE, 0.0_C_DOUBLE, geometry%d_sample_source]


ENDSUBROUTINE photon_shift_first_layer

ENDMODULE
