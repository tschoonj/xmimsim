MODULE xmimsim_main

USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux
USE :: hdf5



TYPE :: xmi_hdf5
!        TYPE (xmi_hdf5_Z), ALLOCATABLE, DIMENSION(:) :: xmi_hdf5_Zs
        INTEGER :: junk
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
        INTEGER (HID_T) :: file_id
        INTEGER :: error


        CHARACTER (C_CHAR), POINTER, DIMENSION(:) :: xmi_hdf5_fileF
        TYPE (xmi_input), POINTER :: xmi_inputF

#if DEBUG == 1
        WRITE (6,*) 'Entering xmi_init_from_hdf5'
#endif

        !associate pointers C -> Fortran
        CALL C_F_POINTER(xmi_inputFPtr, xmi_inputF)
        !CALL C_F_POINTER(xmi_hdf5_file, xmi_hdf5_fileF,[strlen(xmi_hdf5_file)])

#if DEBUG == 1
        WRITE (6,*) 'hdf5_file: ',xmi_hdf5_file(:strlen(xmi_hdf5_file))
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

        !close file
        CALL h5fclose_f(file_id,error)

        !close hdf5 fortran interface
        CALL h5close_f(error)


        rv=1

        RETURN

ENDFUNCTION xmi_init_from_hdf5

ENDMODULE
