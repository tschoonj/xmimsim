MODULE xmimsim_hdf5

USE, INTRINSIC :: ISO_C_BINDING
USE :: hdf5
USE :: xmimsim_aux
USE :: fgsl

CONTAINS

FUNCTION xmi_update_input_from_hdf5(xmi_inputFPtr, xmi_hdf5FPtr) &
BIND(C,NAME='xmi_update_input_from_hdf5') RESULT(rv)
        IMPLICIT NONE
        TYPE (C_PTR), VALUE, INTENT(IN) :: xmi_inputFPtr, xmi_hdf5FPtr
        TYPE (xmi_input), POINTER :: xmi_inputF
        TYPE (xmi_hdf5),  POINTER :: xmi_hdf5F
        INTEGER :: i,j,k
        INTEGER (C_INT), ALLOCATABLE, DIMENSION(:) :: uniqZ, temp_array
        TARGET :: uniqZ
        INTEGER (C_INT) :: rv
        
        CALL C_F_POINTER(xmi_inputFPtr, xmi_inputF)
        CALL C_F_POINTER(xmi_hdf5FPtr, xmi_hdf5F)

        !determine the unique Z and sort them
!        ASSOCIATE (layers => xmi_inputF%composition%layers)
#define layer xmi_inputF%composition%layers
        ALLOCATE(uniqZ(1))
        uniqZ(1) = layer(1)%Z(1)
        DO i=1,SIZE(layer)
                DO j=1,SIZE(layer(i)%Z) 
                        IF (.NOT. ANY(layer(i)%Z(j) == uniqZ)) THEN
                                !uniqZ = &
                                ![uniqZ,layers(i)%Z(j)]
                                ALLOCATE(temp_array(SIZE(uniqZ)+1))
                                temp_array(1:SIZE(uniqZ)) = uniqZ
                                CALL MOVE_ALLOC(temp_array, uniqZ)
                                uniqZ(SIZE(uniqZ)) = layer(i)%Z(j)
                        ENDIF
                ENDDO
        ENDDO
!        ENDASSOCIATE
#undef layer

        CALL qsort(C_LOC(uniqZ),SIZE(uniqZ,KIND=C_SIZE_T),&
        INT(KIND(uniqZ),KIND=C_SIZE_T),C_FUNLOC(C_INT_CMP))

        DO i=1,SIZE(uniqZ) 
                IF (xmi_hdf5F%xmi_hdf5_Zs(i)%Z /= uniqZ(i)) THEN
                        WRITE (*,'(A)') &
                        'Error from xmi_update_input_from_hdf5: elements inconsistency'
                ENDIF
!                ASSOCIATE (layers => xmi_inputF%composition%layers, &
!                n_sample_orientation => xmi_inputF%geometry%n_sample_orientation )
#define layer xmi_inputF%composition%layers
#define n_sample_orientation xmi_inputF%geometry%n_sample_orientation
                !create pointers
                DO j=1,SIZE(layer)
                        IF (.NOT. ALLOCATED(layer(j)%xmi_hdf5_Z_local)) & 
                        ALLOCATE(layer(j)%xmi_hdf5_Z_local(layer(j)%n_elements))
                        DO k=1,layer(j)%n_elements 
                                IF (layer(j)%Z(k) == uniqZ(i)) &
                                layer(j)%xmi_hdf5_Z_local(k)%Ptr => xmi_hdf5F%xmi_hdf5_Zs(i)   
                        ENDDO

                ENDDO

!                ENDASSOCIATE
#undef layer
#undef n_sample_orientation
        ENDDO
        rv=1
        RETURN

ENDFUNCTION xmi_update_input_from_hdf5

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
        !CHARACTER (KIND=C_CHAR,LEN=:), ALLOCATABLE :: xmi_hdf5_fileFF
        CHARACTER (KIND=C_CHAR,LEN=1000) :: xmi_hdf5_fileFF
        TYPE (xmi_input), POINTER :: xmi_inputF
        TYPE (xmi_hdf5),  POINTER :: xmi_hdf5F
        INTEGER (C_INT), ALLOCATABLE, DIMENSION(:) :: uniqZ, temp_array
        TARGET :: uniqZ
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
        !ALLOCATE(CHARACTER(SIZE(xmi_hdf5_fileF)) :: xmi_hdf5_fileFF )
        !xmi_hdf5_fileFF(1:SIZE(xmi_hdf5_fileF)) = xmi_hdf5_fileF(1:SIZE(xmi_hdf5_fileF))
        DO i=1,LEN(xmi_hdf5_fileFF)
                IF (i .LE. SIZE(xmi_hdf5_fileF)) THEN
                        xmi_hdf5_fileFF(i:i) = xmi_hdf5_fileF(i)
                ELSE
                        xmi_hdf5_fileFF(i:i) = ' ' 
                ENDIF
        ENDDO

#if DEBUG == 1
        WRITE (*,'(A)') 'hdf5_file: ',xmi_hdf5_fileFF
#endif

        !determine the unique Z and sort them
!        ASSOCIATE (layers => xmi_inputF%composition%layers)
#define layer xmi_inputF%composition%layers
        ALLOCATE(uniqZ(1))
        uniqZ(1) = layer(1)%Z(1)
        DO i=1,SIZE(layer)
                DO j=1,SIZE(layer(i)%Z) 
                        IF (.NOT. ANY(layer(i)%Z(j) == uniqZ)) THEN
                                !uniqZ = &
                                ![uniqZ,layers(i)%Z(j)]
                                ALLOCATE(temp_array(SIZE(uniqZ)+1))
                                temp_array(1:SIZE(uniqZ)) = uniqZ
                                CALL MOVE_ALLOC(temp_array, uniqZ)
                                uniqZ(SIZE(uniqZ)) = layer(i)%Z(j)
                        ENDIF
                ENDDO
        ENDDO
!        ENDASSOCIATE
#undef layer


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
        CALL h5fopen_f(TRIM(xmi_hdf5_fileFF), H5F_ACC_RDONLY_F, file_id, error)
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
                H5T_NATIVE_DOUBLE,xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%Rayl_and_Compt,[dims(1),2_C_LONG],error)
                CALL h5dclose_f(dset_id,error)
                CALL h5gclose_f(group_id,error)

                DEALLOCATE(dims)
                DEALLOCATE(maxdims)

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


SUBROUTINE xmi_db(filename)

USE :: xraylib
USE :: hdf5
USE :: OMP_LIB
USE :: xmimsim_aux
USE,INTRINSIC :: ISO_C_BINDING
USE,INTRINSIC :: ISO_FORTRAN_ENV

IMPLICIT NONE

INTEGER (C_LONG), PARAMETER :: nintervals_r = 2000, nintervals_e = 200, maxz = 94, &
nintervals_theta=100000, nintervals_theta2=200,nintervals_phi=100000, &
nintervals_e_ip = 10000, nintervals_pz=1000000
REAL (KIND=C_DOUBLE), PARAMETER :: maxe = 100.0, lowe = 0.1, &
        PI = 3.14159265359,MEC2 = 510.998910,maxpz = 100.0
CHARACTER(200) :: error_message

REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:,:) :: &
        rayleigh_theta,compton_theta
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: energies, rs, trapez, thetas,sumz,phis,trapez2
REAL (KIND=C_FLOAT), ALLOCATABLE, DIMENSION(:), TARGET :: energies_flt,&
temp_array
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
doppler_pz(maxz, nintervals_r), pzs(nintervals_pz), fluor_yield_corr(maxz,K_SHELL:M5_SHELL), STAT=stat)

IF (stat /= 0) THEN 
        WRITE (error_unit,*) 'Allocation failure in xmi_db'
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
        DO k=K_SHELL,M5_SHELL
                temp_energy = EdgeEnergy(i,k)
                IF (temp_energy < lowe) CYCLE
                IF (temp_energy > 0.0_C_FLOAT) THEN
                        !energies_flt = [energies_flt,&
                        ! temp_energy+0.00001_C_FLOAT]
                        !energies_flt = [energies_flt,&
                        ! temp_energy-0.00001_C_FLOAT]
                        ALLOCATE(temp_array(SIZE(energies_flt)+2))
                        temp_array(1:SIZE(energies_flt)) = energies_flt
                        CALL MOVE_ALLOC(temp_array, energies_flt)
                        energies_flt(SIZE(energies_flt)-1)=temp_energy+0.00001_C_FLOAT
                        energies_flt(SIZE(energies_flt))=temp_energy-0.00001_C_FLOAT
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
                        CKTB(i,FM12_TRANS)*CKTB(i,FM23_TRANS)*CKTB(i,FM34_TRANS)*&
                        CKTB(i,FM45_TRANS))*&
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
, STAT=stat)

IF (stat /= 0) THEN 
        WRITE (error_unit,*) 'Allocation failure in xmi_db'
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


SUBROUTINE xmi_test_phis

IMPLICIT NONE

REAL (C_DOUBLE) :: theta = M_PI/2.0_C_DOUBLE
REAL (C_DOUBLE),ALLOCATABLE, DIMENSION(:) :: phis

TYPE (fgsl_rng_type) :: rng_type
TYPE (fgsl_rng) :: rng
INTEGER (HID_T) :: file_id
INTEGER (HID_T) :: group_id,group_id2
INTEGER (HID_T) :: dset_id
INTEGER (HID_T) :: dspace_id
INTEGER :: ndims
INTEGER (HSIZE_T),DIMENSION(:), ALLOCATABLE:: dims,maxdims
INTEGER :: error

REAL (C_DOUBLE), DIMENSION(:,:), ALLOCATABLE :: RayleighPhi_ICDF
REAL (C_DOUBLE), DIMENSION(:), ALLOCATABLE :: RayleighThetas,RayleighRandomNumbers

INTEGER :: i
REAL (C_DOUBLE) :: r
INTEGER (C_INT) :: pos_1, pos_2
INTEGER (C_LONG),PARAMETER :: nphis = 10000000



rng_type = fgsl_rng_mt19937
rng = fgsl_rng_alloc(rng_type)
CALL fgsl_rng_set(rng, 314_C_LONG)

CALL h5open_f(error)

!open file for reading
CALL h5fopen_f(XMIMSIM_HDF5_DEFAULT, H5F_ACC_RDONLY_F, file_id, error)
#if DEBUG == 1
WRITE (*,'(A,I)') 'error code: ',error
#endif
IF (error /= 0) THEN
        WRITE (*,'(A,A)') 'Error opening HDF5 file ',XMIMSIM_HDF5_DEFAULT
        STOP
ENDIF

!RayleighPhi
CALL h5gopen_f(file_id,'RayleighPhi',group_id,error)
CALL h5dopen_f(group_id,'RayleighPhi_ICDF',dset_id,error)
CALL h5dget_space_f(dset_id, dspace_id,error)
CALL h5sget_simple_extent_ndims_f(dspace_id, ndims, error)
WRITE (*,'(A,I6)') 'ndims: ',ndims
!Allocate memory
ALLOCATE(dims(ndims))
ALLOCATE(maxdims(ndims))
CALL h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, error)
WRITE (*,'(A,2I6)') 'dims: ',dims
!read the dataset
ALLOCATE(RayleighPhi_ICDF(dims(1),dims(2)))
CALL h5dread_f(dset_id,&
H5T_NATIVE_DOUBLE,RayleighPhi_ICDF,dims,error)

CALL h5sclose_f(dspace_id,error)
CALL h5dclose_f(dset_id,error)
WRITE (*,'(A)') 'After RayleighPhi_ICDF'
!Read RayleighThetas and RayleighRandomNumbers
ALLOCATE(RayleighThetas(dims(1)))
CALL h5dopen_f(group_id,'Thetas',dset_id,error)
CALL h5dread_f(dset_id,&
H5T_NATIVE_DOUBLE,RayleighThetas,[dims(1)],error)
CALL h5dclose_f(dset_id,error)
WRITE (*,'(A)') 'After RayleighThetas'
ALLOCATE(RayleighRandomNumbers(dims(2)))
CALL h5dopen_f(group_id,'Random numbers',dset_id,error)
CALL h5dread_f(dset_id,&
H5T_NATIVE_DOUBLE,RayleighRandomNumbers,[dims(2)],error)
CALL h5dclose_f(dset_id,error)
!close group
CALL h5gclose_f(group_id,error)
WRITE (*,'(A)') 'After RayleighRandomNumbers'


!close file
CALL h5fclose_f(file_id,error)

!close hdf5 fortran interface
CALL h5close_f(error)



!everything read in... calculate the phis
ALLOCATE(phis(nphis))

DO i=1, nphis 
        pos_1=0
        pos_2=0
        phis(i) = bilinear_interpolation(RayleighPhi_ICDF, &
                RayleighThetas, RayleighRandomNumbers,&
                theta, fgsl_rng_uniform(rng), pos_1, pos_2)

ENDDO

OPEN(UNIT=500,FILE='rayleigh_phis_pi2.txt',STATUS='replace',ACTION='write')
WRITE (500,'(I15)') nphis 
DO i=1,nphis
        WRITE (500,'(ES14.5)') phis(i)
ENDDO
        CLOSE(UNIT=500)

ENDSUBROUTINE xmi_test_phis


ENDMODULE xmimsim_hdf5
