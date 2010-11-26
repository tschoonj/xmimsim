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

        !TYPE (C_PTR), VALUE, INTENT(IN) :: xmi_hdf5_file
        CHARACTER (KIND=C_CHAR,LEN=*),INTENT(IN) :: xmi_hdf5_file
        TYPE (C_PTR), VALUE, INTENT(IN) :: xmi_inputFPtr
        TYPE (C_PTR), INTENT(INOUT) :: xmi_hdf5FPtr
        INTEGER (C_INT) :: rv
        INTEGER :: error


        CHARACTER (C_CHAR), POINTER, DIMENSION(:) :: xmi_hdf5_fileF
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
        WRITE (6,*) 'Entering xmi_init_from_hdf5'
#endif

        !associate pointers C -> Fortran
        CALL C_F_POINTER(xmi_inputFPtr, xmi_inputF)
        !CALL C_F_POINTER(xmi_hdf5_file, xmi_hdf5_fileF,[strlen(xmi_hdf5_file)])

#if DEBUG == 1
        WRITE (6,*) 'hdf5_file: ',xmi_hdf5_file(:strlen(xmi_hdf5_file))
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

#if DEBUG == 1
        WRITE (6,*) 'uniqZ', uniqZ
#endif




        !initialize hdf5 fortran interface
        CALL h5open_f(error)

        !open file for reading
        CALL h5fopen_f(xmi_hdf5_file(:strlen(xmi_hdf5_file)), H5F_ACC_RDONLY_F, file_id, error)
#if DEBUG == 1
        WRITE (6,*) 'error code: ',error
#endif
        IF (error /= 0) THEN
                WRITE (6,*) 'Error opening HDF5 file ',xmi_hdf5_file(:strlen(xmi_hdf5_file))
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
        WRITE (6,*) 'ndims: ',ndims
#endif
        !Allocate memory
        ALLOCATE(dims(ndims))
        ALLOCATE(maxdims(ndims))
        CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, error)
#if DEBUG == 1
        WRITE (6,*) 'dims: ',dims
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
        WRITE (6,*) 'ndims: ',ndims
#endif
        !Allocate memory
        DEALLOCATE(dims)
        DEALLOCATE(maxdims)
        ALLOCATE(dims(ndims))
        ALLOCATE(maxdims(ndims))
        CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, error)
#if DEBUG == 1
        WRITE (6,*) 'dims: ',dims
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
                WRITE (6,*) 'Reading element: ',element
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

#if DEBUG == 1
        ASSOCIATE (layers => xmi_inputF%composition%layers)
        !create pointers
        DO j=1,SIZE(layers)
                DO k=1,SIZE(layers(j)%Z) 
                        WRITE (6,*) 'Z confirmation: ',&
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
        WRITE (*,*) 'Entering xmi_free_hdf5_F'
#endif

        DEALLOCATE(xmi_hdf5F%RayleighPhi_ICDF)
        DEALLOCATE(xmi_hdf5F%RayleighThetas)
        DEALLOCATE(xmi_hdf5F%RayleighRandomNumbers)
        DEALLOCATE(xmi_hdf5F%ComptonPhi_ICDF)
        DEALLOCATE(xmi_hdf5F%ComptonThetas)
        DEALLOCATE(xmi_hdf5F%ComptonEnergies)
        DEALLOCATE(xmi_hdf5F%ComptonRandomNumbers)

#if DEBUG == 1
        WRITE (*,*) 'Beyond primary deallocates'
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


        !begin...
        
        rv = 0



        CALL C_F_POINTER(inputFPtr, inputF)
        CALL C_F_POINTER(hdf5FPtr, hdf5F) 

        
        max_threads = omp_get_max_threads()

#if DEBUG == 1
        WRITE (6,*) 'num_threads: ', max_threads
#endif

        ALLOCATE(seeds(max_threads))

        !fetch some seeds
        IF (xmi_get_random_numbers(C_LOC(seeds), INT(max_threads,KIND=C_LONG)) == 0) RETURN

#if DEBUG == 1
        WRITE (*,*) 'seeds: ',seeds
#endif

!
!
!
!  Starting the main OpenMP loop
!
!
!




!$omp parallel default(shared) private(rng,thread_num,i,j,photon,hor_ver_ratio,n_photons,iv_start_energy, iv_end_energy)

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
!       Note : Intel Fortran 11.1 seems to have a serious problem with ASSOCIATE
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

                DO j=1,n_photons
                        !Allocate the photon
                        ALLOCATE(photon)
                        photon%n_interactions=0
                        NULLIFY(photon%offspring)
                        !Calculate energy with rng
                        photon%energy = exc%discrete(i)%energy 

                        !Calculate the electric field vector
                        !IF (REAL(i,KIND=C_DOUBLE)/REAL(n_photons,KIND=C_DOUBLE) &
                        !.LT. hor_ver_ratio ) THEN
                        !        !horizontal polarization
                        !ELSE
                        !        !vertical polarization
                        !ENDIF
                        




                        DEALLOCATE(photon)
                ENDDO
        ENDDO disc 

#undef exc
 !       ENDASSOCIATE


        !cleanup
        CALL fgsl_rng_free(rng)

!$omp end parallel

        rv = 1

ENDFUNCTION xmi_main_msim

ENDMODULE
