MODULE xmimsim_main

USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux
USE :: hdf5


TYPE :: xmi_hdf5_Z
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: RayleighTheta_ICDF
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: ComptonTheta_ICDF
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: Energies
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: RandomNumbers
        !interaction_probs ...
ENDTYPE



TYPE :: xmi_hdf5
        TYPE (xmi_hdf5_Z), ALLOCATABLE, DIMENSION(:) :: xmi_hdf5_Zs
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: RayleighPhi_ICDF
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: RayleighThetas
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: RayleighRandomNumbers
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:,:,:) :: ComptonPhi_ICDF
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: ComptonThetas
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: ComptonEnergies
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: ComptonRandomNumbers
ENDTYPE




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
        INTEGER :: i,j

        INTEGER (HID_T) :: file_id
        INTEGER (HID_T) :: group_id
        INTEGER (HID_T) :: dset_id
        INTEGER (HID_T) :: dspace_id
        INTEGER :: ndims
        INTEGER (HSIZE_T),DIMENSION(:), ALLOCATABLE:: dims,maxdims



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

        !close file
        CALL h5fclose_f(file_id,error)

        !close hdf5 fortran interface
        CALL h5close_f(error)


        rv=1

        RETURN

ENDFUNCTION xmi_init_from_hdf5

ENDMODULE
