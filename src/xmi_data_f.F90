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

#include "config.h"

#define MIN_VERSION 2.1


MODULE xmimsim_data

USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux
USE :: ISO_FORTRAN_ENV

INTEGER (C_INT64_T), BIND(C, NAME='XMI_H5T_NATIVE_DOUBLE') :: XMI_H5T_NATIVE_DOUBLE
INTEGER (C_INT64_T), BIND(C, NAME='XMI_H5T_NATIVE_INT') :: XMI_H5T_NATIVE_INT


INTERFACE
        !wrappers in xmi_hdf5.c around hdf5 calls
        FUNCTION xmi_db_open(filename)&
                BIND(C,NAME='xmi_db_open')&
                RESULT(rv)
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                TYPE (C_PTR), INTENT(IN), VALUE :: filename
                TYPE (C_PTR) :: rv
        ENDFUNCTION xmi_db_open

        FUNCTION xmi_db_open_group(hdf5_vars, group_name)&
                BIND(C,NAME='xmi_db_open_group')&
                RESULT(rv)
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                TYPE (C_PTR), INTENT(IN), VALUE :: hdf5_vars
                CHARACTER (LEN=1,KIND=C_CHAR), INTENT(IN) :: group_name
                INTEGER (C_INT) :: rv
        ENDFUNCTION xmi_db_open_group

        FUNCTION xmi_db_close_group(hdf5_vars)&
                BIND(C,NAME='xmi_db_close_group')&
                RESULT(rv)
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                TYPE (C_PTR), INTENT(IN), VALUE :: hdf5_vars
                INTEGER (C_INT) :: rv
        ENDFUNCTION xmi_db_close_group

        FUNCTION xmi_db_open_dataset(hdf5_vars, dataset_name, ndims, dims)&
                BIND(C,NAME='xmi_db_open_dataset')&
                RESULT(rv)
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                TYPE (C_PTR), INTENT(IN), VALUE :: hdf5_vars
                CHARACTER (LEN=1,KIND=C_CHAR), INTENT(IN) :: dataset_name
                INTEGER (C_INT), INTENT(OUT) :: ndims
                TYPE (C_PTR), INTENT(OUT) :: dims
                INTEGER (C_INT) :: rv
        ENDFUNCTION xmi_db_open_dataset

        FUNCTION xmi_db_read_dataset(hdf5_vars, data, type)&
                BIND(C,NAME='xmi_db_read_dataset')&
                RESULT(rv)
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                TYPE (C_PTR), INTENT(IN), VALUE :: hdf5_vars
                TYPE (C_PTR), INTENT(IN), VALUE :: data
                INTEGER (C_INT64_T), INTENT(IN), VALUE :: type
                INTEGER (C_INT) :: rv
        ENDFUNCTION xmi_db_read_dataset

        FUNCTION xmi_db_close(hdf5_vars)&
                BIND(C,NAME='xmi_db_close')&
                RESULT(rv)
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                TYPE (C_PTR), INTENT(IN), VALUE :: hdf5_vars
                INTEGER (C_INT) :: rv
        ENDFUNCTION xmi_db_close

ENDINTERFACE


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
                        WRITE (ERROR_UNIT,'(A)') &
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

FUNCTION xmi_init_from_hdf5(xmi_hdf5_file, xmi_inputFPtr, xmi_hdf5FPtr, options) &
BIND(C,NAME='xmi_init_from_hdf5') RESULT(rv)
        IMPLICIT NONE

        TYPE (C_PTR), VALUE, INTENT(IN) :: xmi_hdf5_file
        TYPE (C_PTR), VALUE, INTENT(IN) :: xmi_inputFPtr
        TYPE (C_PTR), INTENT(INOUT) :: xmi_hdf5FPtr
        TYPE (xmi_main_options), VALUE, INTENT(IN) :: options
        INTEGER (C_INT) :: rv
        INTEGER :: error


        TYPE (C_PTR) :: hdf5_vars
        TYPE (xmi_input), POINTER :: xmi_inputF
        TYPE (xmi_hdf5),  POINTER :: xmi_hdf5F
        INTEGER (C_INT), ALLOCATABLE, DIMENSION(:) :: uniqZ, temp_array
        TARGET :: uniqZ
        INTEGER :: i,j,k


        INTEGER (C_INT) :: ndims
        INTEGER (C_INT),DIMENSION(:), POINTER :: dims
        TYPE (C_PTR) :: dimsPtr
        INTEGER (C_INT) :: xmi_cascade_type
        REAL (C_DOUBLE), DIMENSION(:), ALLOCATABLE, TARGET :: precalc_xrf_cs_local
        INTEGER :: last_shell, last_line, shell

        CHARACTER (LEN=28, KIND=C_CHAR), DIMENSION(4) :: cascade_group_names = ['No cascade effect           ',&
        'Non-radiative cascade effect', 'Radiative cascade effect    ', 'Full cascade effect         ']
        CHARACTER (LEN=8, KIND=C_CHAR), DIMENSION(K_SHELL:M5_SHELL) :: shell_names = ['K shell ', 'L1 shell', 'L2 shell',&
        'L3 shell', 'M1 shell', 'M2 shell', 'M3 shell', 'M4 shell', 'M5 shell']

        rv = 0
        !associate pointers C -> Fortran
        CALL C_F_POINTER(xmi_inputFPtr, xmi_inputF)

        hdf5_vars = xmi_db_open(xmi_hdf5_file)

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


        IF (.NOT. C_ASSOCIATED(hdf5_vars)) THEN
                RETURN
        ENDIF

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



        IF (options%use_M_lines .EQ. 1_C_INT) THEN
                last_shell = M5_SHELL
                last_line = M5P5_LINE
        ELSE
                last_shell = L3_SHELL
                last_line = L3Q1_LINE
        ENDIF
        ALLOCATE(precalc_xrf_cs_local(ABS(M5P5_LINE)))

        !allocate xmi_hdf5 structure
        ALLOCATE(xmi_hdf5F)


        !start by reading in the Z independent part...
        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A)') 'Opening group Phi'
        ENDIF
        IF (xmi_db_open_group(hdf5_vars, C_CHAR_'Phi'//C_NULL_CHAR) &
                .EQ. 0_C_INT) RETURN

        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A)') 'Opening dataset Phi_ICDF'
        ENDIF
        IF (xmi_db_open_dataset(hdf5_vars,&
                C_CHAR_'Phi_ICDF'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                0_C_INT) RETURN
        CALL C_F_POINTER(dimsPtr, dims, [ndims])
        IF (ndims .NE. 2_C_INT) THEN
                WRITE (error_unit,'(A)') &
                'Wrong dimensions found after opening dataset'
                RETURN
        ENDIF

        !read the dataset
        ALLOCATE(xmi_hdf5F%Phi_ICDF(dims(1),dims(2)))
        NULLIFY(dims)
        CALL xmi_free(dimsPtr)

        IF (xmi_db_read_dataset(hdf5_vars, C_LOC(xmi_hdf5F%Phi_ICDF(1,1)),&
                XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

        !thetas
        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A)') 'Opening dataset Thetas'
        ENDIF
        IF (xmi_db_open_dataset(hdf5_vars,&
                C_CHAR_'Thetas'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                0_C_INT) RETURN
        CALL C_F_POINTER(dimsPtr, dims, [ndims])
        IF (ndims .NE. 1_C_INT) THEN
                WRITE (error_unit,'(A)') &
                'Wrong dimensions found after opening dataset'
                RETURN
        ENDIF

        !read the dataset
        ALLOCATE(xmi_hdf5F%Thetas(dims(1)))
        NULLIFY(dims)
        CALL xmi_free(dimsPtr)

        IF (xmi_db_read_dataset(hdf5_vars, C_LOC(xmi_hdf5F%Thetas(1)),&
                XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

        !random numbers
        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A)') 'Opening dataset Random numbers'
        ENDIF
        IF (xmi_db_open_dataset(hdf5_vars,&
                C_CHAR_'Random numbers'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                0_C_INT) RETURN
        CALL C_F_POINTER(dimsPtr, dims, [ndims])
        IF (ndims .NE. 1_C_INT) THEN
                WRITE (error_unit,'(A)') &
                'Wrong dimensions found after opening dataset'
                RETURN
        ENDIF

        !read the dataset
        ALLOCATE(xmi_hdf5F%RandomNumbers(dims(1)))
        NULLIFY(dims)
        CALL xmi_free(dimsPtr)

        IF (xmi_db_read_dataset(hdf5_vars, C_LOC(xmi_hdf5F%RandomNumbers(1)),&
                XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

        !close group
        IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN


        !read Z dependent part...
        ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(SIZE(uniqZ)))
        xmi_hdf5F%uniqZ = 0

        DO i=1,SIZE(uniqZ)
                xmi_hdf5F%xmi_hdf5_Zs(i)%Z = uniqZ(i)
                xmi_hdf5F%xmi_hdf5_Zs(i)%Zindex = i
                xmi_hdf5F%uniqZ(uniqZ(i)) = i

                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Opening group ',elements(uniqZ(i))&
                //C_NULL_CHAR
                ENDIF
                IF (xmi_db_open_group(hdf5_vars, elements(uniqZ(i)) &
                        //C_NULL_CHAR) &
                        .EQ. 0_C_INT) RETURN

                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Opening group ',&
                C_CHAR_'Theta_ICDF'//C_NULL_CHAR
                ENDIF
                IF (xmi_db_open_group(hdf5_vars,&
                        C_CHAR_'Theta_ICDF'//C_NULL_CHAR) &
                        .EQ. 0_C_INT) RETURN

                !Read Rayleigh Theta ICDF
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening dataset RayleighTheta_ICDF'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'RayleighTheta_ICDF'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 2_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%RayleighTheta_ICDF(dims(1),dims(2)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%RayleighTheta_ICDF(1,1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

                !Read Compton Theta ICDF
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening dataset ComptonTheta_ICDF'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'ComptonTheta_ICDF'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 2_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%ComptonTheta_ICDF(dims(1),dims(2)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%ComptonTheta_ICDF(1,1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN


                !Read corrected fluorescence yields
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening dataset corrected fluorescence yields'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Corrected fluorescence yields'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%FluorYieldsCorr(0:dims(1)-1))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)

                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%FluorYieldsCorr(0)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

                !Read energies
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening dataset energies'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Energies'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%Energies(dims(1)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%Energies(1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

                !Read random numbers
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening dataset random numbers'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Random numbers'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%RandomNumbers(dims(1)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%RandomNumbers(1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

                !close group
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Closing group ',&
                C_CHAR_'Theta_ICDF'//C_NULL_CHAR
                ENDIF
                IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN



                !Read interactions probabilities
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Opening group ',&
                 C_CHAR_'Interaction probabilities'//C_NULL_CHAR
                ENDIF
                IF (xmi_db_open_group(hdf5_vars,&
                        C_CHAR_'Interaction probabilities'//C_NULL_CHAR) &
                        .EQ. 0_C_INT) RETURN

                !Read energies
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening dataset energies'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Energies'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%energies(dims(1)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%energies(1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN


                !Read Rayleigh and Compton probabilities
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') &
                        'Opening dataset Rayleigh and Compton probabilities'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Rayleigh and Compton probabilities'&
                        //C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 2_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                IF (dims(2) .NE. 2_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%Rayl_and_Compt(dims(1),dims(2)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%Rayl_and_Compt(1,1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

                !close group
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Closing group ',&
                 C_CHAR_'Interaction probabilities'//C_NULL_CHAR
                ENDIF
                IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN


                !Read Compton profiles
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Opening group ',&
                C_CHAR_'Compton profiles'//C_NULL_CHAR
                ENDIF
                IF (xmi_db_open_group(hdf5_vars,&
                        C_CHAR_'Compton profiles'//C_NULL_CHAR) &
                        .EQ. 0_C_INT) RETURN

                !Fernandez and Scot
                !Read Shell indices
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening Shell indices'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Shell indices'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%shell_indices(dims(1)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%shell_indices(1)),&
                        XMI_H5T_NATIVE_INT) .EQ. 0_C_INT) RETURN

                !Read Qs
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening Qs'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Qs'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%Qs(dims(1)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%Qs(1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

                !Read Partial profile CDF
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening Partial profile CDF'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Partial profile CDF'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 2_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%profile_partial_cdf(dims(1),dims(2)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%profile_partial_cdf(1,1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

                !Read Partial profile CDF inverted
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') &
                        'Opening Partial profile CDF inverted'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Partial profile CDF inverted'//C_NULL_CHAR,&
                        ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%profile_partial_cdf_inv(dims(1)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%profile_partial_cdf_inv(1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

                !Read Qs inverted
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening Qs inverted'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Qs inverted'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 2_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%Qs_inv(dims(1),dims(2)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%Qs_inv(1,1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN


                !Vincze style
                !Read Random numbers
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') &
                        'Opening Random numbers'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Random numbers'//C_NULL_CHAR,&
                        ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%random_numbers(dims(1)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%random_numbers(1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN

                !Read Total profile ICDF
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') &
                        'Opening Total profile ICDF'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Total profile ICDF'//C_NULL_CHAR,&
                        ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%profile_total_icdf(dims(1)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                        C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%compton_profiles%profile_total_icdf(1)),&
                        XMI_H5T_NATIVE_DOUBLE) .EQ. 0_C_INT) RETURN


                !close group
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Closing group ',&
                 C_CHAR_'Compton profiles'//C_NULL_CHAR
                ENDIF
                IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN



                !the rest of the loop is only useful when using variance
                !reduction
                IF (options%use_variance_reduction .EQ. 0_C_INT) THEN
                        !close group
                        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                                WRITE (output_unit,'(A,A)') 'Closing group ',&
                                elements(uniqZ(i))//C_NULL_CHAR
                        ENDIF
                        IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN
                        NULLIFY(xmi_hdf5F%xmi_hdf5_Zs(i)%precalc_xrf_cs)
                        CYCLE
                ENDIF

                !open group
                !Read precalculated XRF cross sections
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A,A)') 'Opening group ',&
                        C_CHAR_'Precalculated XRF cross sections'//C_NULL_CHAR
                ENDIF
                IF (xmi_db_open_group(hdf5_vars,&
                        C_CHAR_'Precalculated XRF cross sections'//C_NULL_CHAR) &
                        .EQ. 0_C_INT) RETURN

                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%precalc_xrf_cs(K_SHELL:last_shell,SIZE(uniqZ),ABS(KL1_LINE):ABS(last_line)))

                !Open cascade mode
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A,A)') 'Opening group ',&
                        TRIM(cascade_group_names(xmi_cascade_type))//C_NULL_CHAR
                ENDIF
                IF (xmi_db_open_group(hdf5_vars,&
                        TRIM(cascade_group_names(xmi_cascade_type))//C_NULL_CHAR) &
                        .EQ. 0_C_INT) RETURN

                !loop over all shells
                DO shell=K_SHELL, last_shell
                        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                                WRITE (output_unit,'(A,A)') 'Opening group ',&
                                TRIM(shell_names(shell))
                        ENDIF
                        IF (xmi_db_open_group(hdf5_vars,&
                                TRIM(shell_names(shell))//C_NULL_CHAR) &
                                .EQ. 0_C_INT) RETURN

                        !loop over all elements
                        DO j=1,SIZE(uniqZ)
                                !open element dataset
                                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                                        WRITE (output_unit,'(A,A)') 'Opening dataset ',&
                                        elements(uniqZ(j))
                                ENDIF
                                IF (xmi_db_open_dataset(hdf5_vars,&
                                        elements(uniqZ(j))//C_NULL_CHAR,ndims,dimsPtr) &
                                        .EQ. 0_C_INT) RETURN

                                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                                IF (ndims .NE. 1_C_INT) THEN
                                        WRITE (error_unit,'(A)') &
                                        'Wrong dimensions found after opening dataset'
                                        RETURN
                                ENDIF
                                !read the dataset
                                NULLIFY(dims)
                                CALL xmi_free(dimsPtr)
                                IF (xmi_db_read_dataset(hdf5_vars, &
                                C_LOC(precalc_xrf_cs_local),&
                                XMI_H5T_NATIVE_DOUBLE)&
                                .EQ. 0_C_INT) RETURN
                                xmi_hdf5F%xmi_hdf5_Zs(i)%precalc_xrf_cs(shell,j,:)=&
                                precalc_xrf_cs_local(ABS(KL1_LINE):ABS(last_line))
                        ENDDO

                        !close group
                        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                                WRITE (output_unit,'(A,A)') 'Closing group ',&
                                TRIM(shell_names(shell))
                        ENDIF
                        IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN
                ENDDO

                !close group
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Closing group ',&
                TRIM(cascade_group_names(xmi_cascade_type))//C_NULL_CHAR
                ENDIF
                IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN

                !close group
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Closing group ',&
                'Precalculated XRF cross sections'//C_NULL_CHAR
                ENDIF
                IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN

                !close group
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Closing group ',&
                 elements(uniqZ(i))//C_NULL_CHAR
                ENDIF
                IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN

        ENDDO

        IF (xmi_db_close(hdf5_vars) .EQ. 0_C_INT) RETURN


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


        DEALLOCATE(xmi_hdf5F%Phi_ICDF)
        DEALLOCATE(xmi_hdf5F%Thetas)
        DEALLOCATE(xmi_hdf5F%RandomNumbers)

        DO i=1,SIZE(xmi_hdf5F%xmi_hdf5_Zs)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%RayleighTheta_ICDF)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%ComptonTheta_ICDF)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%Energies)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%RandomNumbers)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%energies)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%Rayl_and_Compt)
                IF (ASSOCIATED(xmi_hdf5F%xmi_hdf5_Zs(i)%precalc_xrf_cs)) DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%precalc_xrf_cs)
                DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%FluorYieldsCorr)
        ENDDO
        DEALLOCATE(xmi_hdf5F%xmi_hdf5_Zs)
        DEALLOCATE(xmi_hdf5F)
        xmi_hdf5FPtr = C_NULL_PTR

ENDSUBROUTINE xmi_free_hdf5_F

SUBROUTINE xmi_db_Z_specific(rayleigh_thetaPtr, compton_thetaPtr, energiesPtr, rsPtr,&
fluor_yield_corrPtr, ipPtr, nintervals_r, nintervals_e, nintervals_e_ip,&
precalc_xrf_csPtr, compton_profiles_Ptr, ncompton_profiles, ZsPtr, nZs) &
BIND(C,NAME='xmi_db_Z_specific')

IMPLICIT NONE

TYPE (C_PTR), VALUE, INTENT(IN) :: rayleigh_thetaPtr, compton_thetaPtr, &
energiesPtr, rsPtr, fluor_yield_corrPtr, ipPtr,&
precalc_xrf_csPtr, compton_profiles_Ptr, ZsPtr
INTEGER (C_INT), VALUE, INTENT(IN) :: nintervals_r, nintervals_e, &
nintervals_e_ip, ncompton_profiles, nZs

REAL (C_DOUBLE), DIMENSION(:,:,:), POINTER::&
rayleigh_theta, compton_theta
REAL (C_DOUBLE), DIMENSION(:,:,:,:,:), POINTER::&
precalc_xrf_cs
REAL (C_DOUBLE), DIMENSION(:), POINTER :: energies,rs
REAL (C_DOUBLE), DIMENSION(:,:), POINTER :: fluor_yield_corr
TYPE (interaction_probC), DIMENSION(:), POINTER :: ip
TYPE (interaction_prob) :: ip_temp
TYPE (compton_profilesC), DIMENSION(:), POINTER :: cp
TYPE (compton_profiles) :: cp_temp

INTEGER (8), PARAMETER :: nintervals_theta=100000, nintervals_theta2=200,nintervals_phi=100000, &
nintervals_pz=10000000
REAL (KIND=C_DOUBLE), PARAMETER :: maxe = 200.0, lowe = 0.1, &
        PI = 3.14159265359, maxpz = 100.0

REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: trapez,&
thetas, sumz, phis, trapez2
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:), TARGET :: energies_flt,&
temp_array
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: energies_dbl
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: pzs
REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:) :: Qs, profile_partial_cdf_inv, rs2
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: Qs_big, profile_partial_cdf_big

INTEGER :: stat,i,j,k,l,m,n,line
REAL (KIND=C_DOUBLE) :: temp_sum,K0K
REAL (KIND=C_DOUBLE) :: temp_energy,temp_total_cs,energy
REAL (KIND=C_DOUBLE) :: PK, PL1, PL2, PL3, PM1, PM2, PM3, PM4, PM5
INTEGER (KIND=C_INT), ALLOCATABLE, DIMENSION(:) :: shell_indices, shell_indices_temp
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: electron_config,&
electron_config_temp
INTEGER (KIND=C_INT) :: pos, old_pos
INTEGER (KIND=C_INT), POINTER, DIMENSION(:) :: Zs
INTEGER (KIND=C_INT) :: Z, Z2

CALL C_F_POINTER(rayleigh_thetaPtr,rayleigh_theta,[nZs, nintervals_e,&
nintervals_r])
CALL C_F_POINTER(compton_thetaPtr,compton_theta,[nZs, nintervals_e,&
nintervals_r])
CALL C_F_POINTER(energiesPtr,energies,[nintervals_e])
CALL C_F_POINTER(rsPtr,rs,[nintervals_r])
CALL C_F_POINTER(fluor_yield_corrPtr,fluor_yield_corr,[nZs,M5_SHELL+1])
CALL C_F_POINTER(ipPtr, ip, [nZs])
CALL C_F_POINTER(precalc_xrf_csPtr, precalc_xrf_cs, [nZs, 4, M5_SHELL+1, nZs, ABS(M5P5_LINE)])
CALL C_F_POINTER(compton_profiles_Ptr, cp, [nZs])
CALL C_F_POINTER(ZsPtr, Zs, [nZs])

CALL SetErrorMessages(0)

ALLOCATE(thetas(nintervals_theta))
ALLOCATE(Qs(ncompton_profiles))
ALLOCATE(rs2(ncompton_profiles))
ALLOCATE(profile_partial_cdf_inv(ncompton_profiles))
ALLOCATE(Qs_big(nintervals_pz))
ALLOCATE(pzs(nintervals_pz))

!Fill up the energies and rs arrays...
DO i=1,ncompton_profiles
       Qs(i) = &
       100.0*(REAL(i,C_DOUBLE)-1.0)/(REAL(ncompton_profiles,C_DOUBLE)-1.0)
       profile_partial_cdf_inv(i) = 0.5*(REAL(i,C_DOUBLE)-1.0)/&
       (REAL(ncompton_profiles,C_DOUBLE)-1)
       rs2(i) = (REAL(i,C_DOUBLE)-1.0)/(REAL(ncompton_profiles,C_DOUBLE)-1.0)
ENDDO

DO i=1,nintervals_pz
        Qs_big(i) = 100.0*(REAL(i,C_DOUBLE)-1.0)/(REAL(nintervals_pz,C_DOUBLE)-1.0)
        pzs(i) = maxpz*(REAL(i,C_DOUBLE)-1.0)/(REAL(nintervals_pz,C_DOUBLE)-1.0)
ENDDO


DO i=1,nintervals_e
        energies(i) = lowe + (maxe-lowe)*(REAL(i,C_DOUBLE)-1.0)/&
        (REAL(nintervals_e,C_DOUBLE)-1.0)
ENDDO

DO i=1,nintervals_r
        rs(i) = (REAL(i,C_DOUBLE)-1.0)/(REAL(nintervals_r,C_DOUBLE)-1.0)
ENDDO

DO i=1,nintervals_theta
        thetas(i) = 0.0_C_DOUBLE+(PI)*(REAL(i,C_DOUBLE)-1.0)/&
        (REAL(nintervals_theta,C_DOUBLE)-1.0)
ENDDO

!$OMP PARALLEL DEFAULT(shared) PRIVATE(i,j,k,l,m,trapez,temp_sum,sumz,energies_flt,&
!$OMP temp_energy,temp_array,ip_temp,temp_total_cs,trapez2,&
!$OMP energy,PK,PL1,PL2,PL3,PM1,PM2,PM3,PM4,PM5,line, cp_temp, shell_indices,&
!$OMP shell_indices_temp, electron_config, electron_config_temp,&
!$OMP profile_partial_cdf_big, pos, Z, Z2)

ALLOCATE(trapez(nintervals_theta-1))
ALLOCATE(trapez2(nintervals_pz-1))
ALLOCATE(sumz(nintervals_theta-1))
ALLOCATE(profile_partial_cdf_big(nintervals_pz))

!$OMP DO &
!$OMP SCHEDULE(dynamic,1)

Zloop:DO i=1,nZs
        Z = Zs(i)
        WRITE (output_unit, '(A,I3)') 'Generating datasets for element ', Z
        Eloop:DO j=1,nintervals_e

                !
                !Rayleigh
                !

                thetaloop: DO k=1,nintervals_theta-1
                        trapez(k) = &
                        (DCS_Rayl(Z,energies(j),&
                        thetas(k))*SIN(thetas(k))+&
                        DCS_Rayl(Z,energies(j),&
                        thetas(k+1))*SIN(thetas(k+1)))&
                        *(thetas(k+1)-thetas(k))/2.0
                ENDDO thetaloop
                !to avoid database conflicts -> calculate CS_Rayl yourself...
                !trapez = trapez*2.0*PI/CS_Rayl(Z,REAL(energies(j),KIND=C_FLOAT))
                trapez = trapez/SUM(trapez)
#if DEBUG == 2
                IF (Z == 26) THEN
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
                        (DCS_Compt(Z,energies(j),&
                        thetas(k))*SIN(thetas(k))+&
                        DCS_Compt(Z,energies(j),&
                        thetas(k+1))&
                        *SIN(thetas(k+1)))*(thetas(k+1)-thetas(k))/2.0
                ENDDO thetaloop2
                !trapez = trapez*2.0*PI/CS_Compt(Z,REAL(energies(j),KIND=C_FLOAT))
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
IF (Z == 26) THEN
        CLOSE(unit=100)
ENDIF
#endif
        !automatic allocation -> could be a problem for some
        !compilers... yes I'm talking about you Intel!!! (requires -assume lhs_alloc)
        !Gfortran 4.5 also doesn't appear to support automatic allocation when
        !initializing with REAL... sorry Intel!
        ALLOCATE(energies_flt(nintervals_e_ip))
        DO j=1,nintervals_e_ip
                energies_flt(j) = lowe + (maxe-lowe)*(REAL(j,C_DOUBLE)-1.0)&
                /(REAL(nintervals_e_ip,C_DOUBLE)-1.0)
        ENDDO

        !add edge energies
        DO k=K_SHELL,M5_SHELL
                temp_energy = EdgeEnergy(Z,k)
                IF (temp_energy < lowe) CYCLE
                IF (temp_energy > 0.0_C_DOUBLE) THEN
                        !energies_flt = [energies_flt,&
                        ! temp_energy+0.00001_C_FLOAT]
                        !energies_flt = [energies_flt,&
                        ! temp_energy-0.00001_C_FLOAT]
                        ALLOCATE(temp_array(SIZE(energies_flt)+2))
                        temp_array(1:SIZE(energies_flt)) = energies_flt
                        CALL MOVE_ALLOC(temp_array, energies_flt)
                        energies_flt(SIZE(energies_flt)-1)=temp_energy+0.00001_C_DOUBLE
                        energies_flt(SIZE(energies_flt))=temp_energy-0.00001_C_DOUBLE
                ENDIF
        ENDDO


        !SORT them
        CALL qsort(C_LOC(energies_flt),SIZE(energies_flt,KIND=C_SIZE_T),&
        INT(KIND(energies_flt),KIND=C_SIZE_T),C_FUNLOC(C_DOUBLE_CMP))


        ip_temp = energies_flt

        DO k=1,SIZE(energies_flt)
                temp_total_cs = CS_Total_Kissel(Z,energies_flt(k))
                ip_temp%Rayl_and_Compt(k,1) = CS_Rayl(Z,energies_flt(k))/temp_total_cs
                ip_temp%Rayl_and_Compt(k,2) = CS_Compt(Z,energies_flt(k))/temp_total_cs+&
                  ip_temp%Rayl_and_Compt(k,1)
        ENDDO
        ip(i)%len = SIZE(energies_flt)
        ip(i)%energies = C_LOC(ip_temp%energies(1))
        ip(i)%Rayl_and_Compt = C_LOC(ip_temp%Rayl_and_Compt(1,1))

        DEALLOCATE(energies_flt)

        !
        !
        !       Doppler broadening
        !
        !
        ALLOCATE(shell_indices(1), electron_config(1))
        shell_indices(1) = K_SHELL
        electron_config(1) = ElectronConfig_Biggs(Z, K_SHELL)
        DO j=L1_SHELL,Q3_SHELL
                IF(ElectronConfig_Biggs(Z, j) .GT. 0) THEN
                        ALLOCATE(shell_indices_temp(SIZE(shell_indices)+1))
                        shell_indices_temp(1:SIZE(shell_indices)) = &
                        shell_indices
                        CALL MOVE_ALLOC(shell_indices_temp,&
                        shell_indices)
                        shell_indices(SIZE(shell_indices)) = j
                        ALLOCATE(electron_config_temp(SIZE(electron_config)+1))
                        electron_config_temp(1:SIZE(electron_config)) = &
                        electron_config
                        CALL MOVE_ALLOC(electron_config_temp,&
                        electron_config)
                        electron_config(SIZE(electron_config)) = ElectronConfig_Biggs(Z,j)
                ENDIF
        ENDDO

        ALLOCATE(cp_temp%shell_indices(SIZE(shell_indices)))
        cp_temp%shell_indices = shell_indices
        ALLOCATE(cp_temp%profile_partial_cdf(SIZE(shell_indices), ncompton_profiles))
        ALLOCATE(cp_temp%Qs_inv(SIZE(shell_indices), ncompton_profiles))
        ALLOCATE(cp_temp%profile_total_icdf(ncompton_profiles))

        cp(i)%shell_indices = C_LOC(cp_temp%shell_indices(1))
        cp(i)%shell_indices_len = SIZE(shell_indices)
        cp(i)%data_len = ncompton_profiles
        cp(i)%Qs = C_LOC(Qs(1))
        cp(i)%profile_partial_cdf_inv = C_LOC(profile_partial_cdf_inv(1))
        cp(i)%profile_partial_cdf = C_LOC(cp_temp%profile_partial_cdf(1,1))
        cp(i)%Qs_inv= C_LOC(cp_temp%Qs_inv(1,1))
        cp(i)%profile_total_icdf = C_LOC(cp_temp%profile_total_icdf(1))
        cp(i)%random_numbers = C_LOC(rs2(1))

        DO j=1,nintervals_pz-1
                trapez2(j) = &
                (ComptonProfile(Z, pzs(j))+&
                ComptonProfile(Z, pzs(j+1)))*&
                (pzs(j+1)-pzs(j))/2.0_C_DOUBLE/REAL(Z,KIND=C_DOUBLE)
                !divide by atomic number because we want an average value per
                !electron
        ENDDO
        trapez2 = trapez2/SUM(trapez2)

        temp_sum = 0.0_C_DOUBLE
        l=1
        m=1
        DO
               temp_sum = trapez2(l)+temp_sum
               IF (temp_sum >= rs2(m)) THEN
                      cp_temp%profile_total_icdf(m) = pzs(l)
                      IF (m == ncompton_profiles) EXIT
                      m = m+1
                ENDIF
                IF (l == nintervals_pz-1) EXIT
                l = l+1
        ENDDO
        cp_temp%profile_total_icdf(ncompton_profiles) = maxpz
        cp_temp%profile_total_icdf(1) = 0.0_C_DOUBLE


        cp_temp%profile_partial_cdf(:,1) = 0.0
        DO k=1,SIZE(shell_indices)
          DO j=2,ncompton_profiles
               cp_temp%profile_partial_cdf(k,j) = cp_temp%profile_partial_cdf(k,j-1)+&
               (Qs(2)-Qs(1))*(ComptonProfile_Partial(Z, shell_indices(k), Qs(j))+&
               ComptonProfile_Partial(Z, shell_indices(k), Qs(j-1)))*0.5_C_DOUBLE
          ENDDO
          cp_temp%profile_partial_cdf(k,:) =&
          cp_temp%profile_partial_cdf(k,:)*0.5/cp_temp%profile_partial_cdf(k,ncompton_profiles)
          !inverted
          profile_partial_cdf_big(1) = 0.0
          DO j=2,nintervals_pz
                profile_partial_cdf_big(j) = profile_partial_cdf_big(j-1)+&
               (Qs_big(2)-Qs_big(1))*(ComptonProfile_Partial(Z, shell_indices(k), Qs_big(j))+&
               ComptonProfile_Partial(Z, shell_indices(k), Qs_big(j-1)))*0.5_C_DOUBLE
          ENDDO
          profile_partial_cdf_big = &
          profile_partial_cdf_big*0.5/profile_partial_cdf_big(nintervals_pz)
          cp_temp%Qs_inv(k,1) = 0.0
          pos = 1
          DO j=1,ncompton_profiles
                IF (pos .GT.2) THEN
                        pos = findpos(profile_partial_cdf_big,&
                        profile_partial_cdf_inv(j), pos)
                ELSE
                        pos = findpos(profile_partial_cdf_big,&
                        profile_partial_cdf_inv(j))
                ENDIF
                IF (pos .LT. 1 .OR. pos .GT. nintervals_pz-1) THEN
                        WRITE (error_unit,'(A)') 'findpos error'
                        CALL xmi_exit(1)
                ENDIF
                cp_temp%Qs_inv(k,j) = interpolate_simple([&
                profile_partial_cdf_big(pos),&
                Qs_big(pos)&
                ],[&
                profile_partial_cdf_big(pos+1),&
                Qs_big(pos+1)&
                ], profile_partial_cdf_inv(j))
          ENDDO

        ENDDO
        DEALLOCATE(shell_indices)
        DEALLOCATE(electron_config)
        !
        !
        !       Corrected fluorescence yields (in terms of the primary vacancy
        !       distributions)
        !
        !
#define CKTB CosKronTransProb
        fluor_yield_corr(i,K_SHELL+1) = FluorYield(Z,K_SHELL)
        fluor_yield_corr(i,L1_SHELL+1) = FluorYield(Z,L1_SHELL)+&
                        (CKTB(Z,FL12_TRANS)*FluorYield(Z,L2_SHELL))+&
                        (CKTB(Z,FL13_TRANS)+CKTB(Z,FL12_TRANS)*CKTB(Z,FL23_TRANS))*&
                        FluorYield(Z,L3_SHELL)
        fluor_yield_corr(i,L2_SHELL+1) = FluorYield(Z,L2_SHELL)+&
                        (CKTB(Z,FL23_TRANS)*FluorYield(Z,L3_SHELL))
        fluor_yield_corr(i,L3_SHELL+1) = FluorYield(Z,L3_SHELL)
        fluor_yield_corr(i,M1_SHELL+1) = &
                        !M1_SHELL
                        FluorYield(Z,M1_SHELL)+&
                        !M2_SHELL
                        CKTB(Z,FM12_TRANS)*FluorYield(Z,M2_SHELL)+&
                        !M3_SHELL
                        (CKTB(Z,FM13_TRANS)+CKTB(Z,FM12_TRANS)*CKTB(Z,FM23_TRANS))*&
                        FluorYield(Z,M3_SHELL)+&
                        !M4_SHELL
                        (CKTB(Z,FM14_TRANS)+CKTB(Z,FM13_TRANS)*CKTB(Z,FM34_TRANS)+&
                        CKTB(Z,FM12_TRANS)*CKTB(Z,FM24_TRANS)+&
                        CKTB(Z,FM12_TRANS)*CKTB(Z,FM23_TRANS)*CKTB(Z,FM34_TRANS))*&
                        FluorYield(Z,M4_SHELL)+&
                        !M5_SHELL
                        (CKTB(Z,FM15_TRANS)+&
                        CKTB(Z,FM14_TRANS)*CKTB(Z,FM45_TRANS)+&
                        CKTB(Z,FM13_TRANS)*CKTB(Z,FM35_TRANS)+&
                        CKTB(Z,FM12_TRANS)*CKTB(Z,FM25_TRANS)+&
                        CKTB(Z,FM13_TRANS)*CKTB(Z,FM34_TRANS)*CKTB(Z,FM45_TRANS)+&
                        CKTB(Z,FM12_TRANS)*CKTB(Z,FM24_TRANS)*CKTB(Z,FM45_TRANS)+&
                        CKTB(Z,FM12_TRANS)*CKTB(Z,FM23_TRANS)*CKTB(Z,FM35_TRANS)+&
                        CKTB(Z,FM12_TRANS)*CKTB(Z,FM23_TRANS)*CKTB(Z,FM34_TRANS)*&
                        CKTB(Z,FM45_TRANS))*&
                        FluorYield(Z,M5_SHELL)
        fluor_yield_corr(i,M2_SHELL+1) = &
                        !M2_SHELL
                        FluorYield(Z,M2_SHELL)+&
                        !M3_SHELL
                        CKTB(Z,FM23_TRANS)*FluorYield(Z,M3_SHELL)+&
                        !M4_SHELL
                        (CKTB(Z,FM24_TRANS)+CKTB(Z,FM23_TRANS)*CKTB(Z,FM34_TRANS))*&
                        FluorYield(Z,M4_SHELL)+&
                        !M5_SHELL
                        (CKTB(Z,FM25_TRANS)+CKTB(Z,FM24_TRANS)*CKTB(Z,FM45_TRANS)+&
                        CKTB(Z,FM23_TRANS)*CKTB(Z,FM35_TRANS)+&
                        CKTB(Z,FM23_TRANS)*CKTB(Z,FM34_TRANS)*CKTB(Z,FM45_TRANS))*&
                        FluorYield(Z,M5_SHELL)
        fluor_yield_corr(i,M3_SHELL+1) = &
                        !M3_SHELL
                        FluorYield(Z,M3_SHELL)+&
                        !M4_SHELL
                        CKTB(Z,FM34_TRANS)*FluorYield(Z,M4_SHELL)+&
                        !M5_SHELL
                        (CKTB(Z,FM35_TRANS)+CKTB(Z,FM34_TRANS)*CKTB(Z,FM45_TRANS))*&
                        FluorYield(Z,M5_SHELL)
        fluor_yield_corr(i,M4_SHELL+1) = &
                        !M4_SHELL
                        FluorYield(Z,M4_SHELL)+&
                        !M5_SHELL
                        CKTB(Z,FM45_TRANS)*FluorYield(Z,M5_SHELL)
        fluor_yield_corr(i,M5_SHELL+1) = &
                        !M5_SHELL
                        FluorYield(Z,M5_SHELL)
#undef CKTB

        DO j=1,nZs
                Z2 = Zs(j)
                DO line=KL1_LINE, M5P5_LINE, -1
                        PK = 0.0_C_DOUBLE
                        PL1 = 0.0_C_DOUBLE
                        PL2 = 0.0_C_DOUBLE
                        PL3 = 0.0_C_DOUBLE
                        PM1 = 0.0_C_DOUBLE
                        PM2 = 0.0_C_DOUBLE
                        PM3 = 0.0_C_DOUBLE
                        PM4 = 0.0_C_DOUBLE
                        PM5 = 0.0_C_DOUBLE
                        energy = LineEnergy(Z2, line)
                        IF (energy .GE. EdgeEnergy(Z,K_SHELL)) &
                                PK = CS_Photo_Partial(Z,K_SHELL,&
                                energy)

                        !no cascade
                        PL1 = PL1_pure_kissel(Z,&
                        energy)
                        PL2 = PL2_pure_kissel(Z,&
                        energy,PL1)
                        PL3 = PL3_pure_kissel(Z,&
                        energy,PL1,PL2)
                        PM1 = PM1_pure_kissel(Z,&
                        energy)
                        PM2 = &
                        PM2_pure_kissel(Z,&
                        energy,PM1)
                        PM3 = &
                        PM3_pure_kissel(Z,&
                        energy,PM1,PM2)
                        PM4 = &
                        PM4_pure_kissel(Z,&
                        energy,&
                        PM1,PM2,PM3)
                        PM5 = &
                        PM5_pure_kissel(Z,&
                        energy,&
                        PM1,PM2,PM3,PM4)

                        precalc_xrf_cs(i,XMI_CASCADE_NONE,K_SHELL+1,j,ABS(line)) = PK
                        precalc_xrf_cs(i,XMI_CASCADE_NONE,L1_SHELL+1,j,ABS(line)) = PL1
                        precalc_xrf_cs(i,XMI_CASCADE_NONE,L2_SHELL+1,j,ABS(line)) = PL2
                        precalc_xrf_cs(i,XMI_CASCADE_NONE,L3_SHELL+1,j,ABS(line)) = PL3
                        precalc_xrf_cs(i,XMI_CASCADE_NONE,M1_SHELL+1,j,ABS(line)) = PM1
                        precalc_xrf_cs(i,XMI_CASCADE_NONE,M2_SHELL+1,j,ABS(line)) = PM2
                        precalc_xrf_cs(i,XMI_CASCADE_NONE,M3_SHELL+1,j,ABS(line)) = PM3
                        precalc_xrf_cs(i,XMI_CASCADE_NONE,M4_SHELL+1,j,ABS(line)) = PM4
                        precalc_xrf_cs(i,XMI_CASCADE_NONE,M5_SHELL+1,j,ABS(line)) = PM5

                        !nonradiative only
                        PL1 = PL1_auger_cascade_kissel(Z,&
                        energy,PK)
                        PL2 = PL2_auger_cascade_kissel(Z,&
                        energy,PK,PL1)
                        PL3 = PL3_auger_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2)
                        PM1 =&
                        PM1_auger_cascade_kissel(Z,&
                        energy,&
                        PK, PL1, PL2, PL3)
                        PM2 = &
                        PM2_auger_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1)
                        PM3 = &
                        PM3_auger_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1,PM2)
                        PM4 = &
                        PM4_auger_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1,PM2,PM3)
                        PM5 = &
                        PM5_auger_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1,PM2,PM3,PM4)

                        precalc_xrf_cs(i,XMI_CASCADE_NONRADIATIVE,K_SHELL+1,j,ABS(line)) = PK
                        precalc_xrf_cs(i,XMI_CASCADE_NONRADIATIVE,L1_SHELL+1,j,ABS(line)) = PL1
                        precalc_xrf_cs(i,XMI_CASCADE_NONRADIATIVE,L2_SHELL+1,j,ABS(line)) = PL2
                        precalc_xrf_cs(i,XMI_CASCADE_NONRADIATIVE,L3_SHELL+1,j,ABS(line)) = PL3
                        precalc_xrf_cs(i,XMI_CASCADE_NONRADIATIVE,M1_SHELL+1,j,ABS(line)) = PM1
                        precalc_xrf_cs(i,XMI_CASCADE_NONRADIATIVE,M2_SHELL+1,j,ABS(line)) = PM2
                        precalc_xrf_cs(i,XMI_CASCADE_NONRADIATIVE,M3_SHELL+1,j,ABS(line)) = PM3
                        precalc_xrf_cs(i,XMI_CASCADE_NONRADIATIVE,M4_SHELL+1,j,ABS(line)) = PM4
                        precalc_xrf_cs(i,XMI_CASCADE_NONRADIATIVE,M5_SHELL+1,j,ABS(line)) = PM5

                        !radiative only
                        PL1 = PL1_rad_cascade_kissel(Z,&
                        energy,PK)
                        PL2 = PL2_rad_cascade_kissel(Z,&
                        energy,PK,PL1)
                        PL3 = PL3_rad_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2)
                        PM1 =&
                        PM1_rad_cascade_kissel(Z,&
                        energy,&
                        PK, PL1, PL2, PL3)
                        PM2 = &
                        PM2_rad_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1)
                        PM3 = &
                        PM3_rad_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1,PM2)
                        PM4 = &
                        PM4_rad_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1,PM2,PM3)
                        PM5 = &
                        PM5_rad_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1,PM2,PM3,PM4)

                        precalc_xrf_cs(i,XMI_CASCADE_RADIATIVE,K_SHELL+1,j,ABS(line)) = PK
                        precalc_xrf_cs(i,XMI_CASCADE_RADIATIVE,L1_SHELL+1,j,ABS(line)) = PL1
                        precalc_xrf_cs(i,XMI_CASCADE_RADIATIVE,L2_SHELL+1,j,ABS(line)) = PL2
                        precalc_xrf_cs(i,XMI_CASCADE_RADIATIVE,L3_SHELL+1,j,ABS(line)) = PL3
                        precalc_xrf_cs(i,XMI_CASCADE_RADIATIVE,M1_SHELL+1,j,ABS(line)) = PM1
                        precalc_xrf_cs(i,XMI_CASCADE_RADIATIVE,M2_SHELL+1,j,ABS(line)) = PM2
                        precalc_xrf_cs(i,XMI_CASCADE_RADIATIVE,M3_SHELL+1,j,ABS(line)) = PM3
                        precalc_xrf_cs(i,XMI_CASCADE_RADIATIVE,M4_SHELL+1,j,ABS(line)) = PM4
                        precalc_xrf_cs(i,XMI_CASCADE_RADIATIVE,M5_SHELL+1,j,ABS(line)) = PM5

                        !full cascade
                        PL1 = PL1_full_cascade_kissel(Z,&
                        energy,PK)
                        PL2 = PL2_full_cascade_kissel(Z,&
                        energy,PK,PL1)
                        PL3 = PL3_full_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2)
                        PM1 =&
                        PM1_full_cascade_kissel(Z,&
                        energy,&
                        PK, PL1, PL2, PL3)
                        PM2 = &
                        PM2_full_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1)
                        PM3 = &
                        PM3_full_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1,PM2)
                        PM4 = &
                        PM4_full_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1,PM2,PM3)
                        PM5 = &
                        PM5_full_cascade_kissel(Z,&
                        energy,&
                        PK,PL1,PL2,PL3,PM1,PM2,PM3,PM4)

                        precalc_xrf_cs(i,XMI_CASCADE_FULL,K_SHELL+1,j,ABS(line)) = PK
                        precalc_xrf_cs(i,XMI_CASCADE_FULL,L1_SHELL+1,j,ABS(line)) = PL1
                        precalc_xrf_cs(i,XMI_CASCADE_FULL,L2_SHELL+1,j,ABS(line)) = PL2
                        precalc_xrf_cs(i,XMI_CASCADE_FULL,L3_SHELL+1,j,ABS(line)) = PL3
                        precalc_xrf_cs(i,XMI_CASCADE_FULL,M1_SHELL+1,j,ABS(line)) = PM1
                        precalc_xrf_cs(i,XMI_CASCADE_FULL,M2_SHELL+1,j,ABS(line)) = PM2
                        precalc_xrf_cs(i,XMI_CASCADE_FULL,M3_SHELL+1,j,ABS(line)) = PM3
                        precalc_xrf_cs(i,XMI_CASCADE_FULL,M4_SHELL+1,j,ABS(line)) = PM4
                        precalc_xrf_cs(i,XMI_CASCADE_FULL,M5_SHELL+1,j,ABS(line)) = PM5

                ENDDO
        ENDDO
ENDDO Zloop
!$OMP END DO
!$OMP END PARALLEL

ENDSUBROUTINE xmi_db_Z_specific

SUBROUTINE xmi_db_Z_independent(phiPtr, thetasPtr, &
rsPtr, nintervals_theta2, nintervals_r) &
BIND(C,NAME='xmi_db_Z_independent')

IMPLICIT NONE

TYPE (C_PTR), VALUE, INTENT(IN) :: phiPtr, thetasPtr, &
rsPtr
INTEGER (C_INT), VALUE, INTENT(IN) :: nintervals_theta2, nintervals_r
REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:,:) :: phi
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: cdfs, phis
REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:) :: thetas, energies, rs
INTEGER (8), PARAMETER :: nintervals_phi=100000
INTEGER (8) :: i,j,k,l,m
REAL (KIND=C_DOUBLE) :: K0K
REAL (KIND=C_DOUBLE) :: PI = 3.14159265359


CALL C_F_POINTER(phiPtr, phi, [nintervals_theta2,nintervals_r])
CALL C_F_POINTER(thetasPtr, thetas, [nintervals_theta2])
!energies and rs will be reused
CALL C_F_POINTER(rsPtr,rs,[nintervals_r])

ALLOCATE(phis(nintervals_phi))

DO i=1,nintervals_theta2
        thetas(i) = 0.0_C_DOUBLE+(0.5_C_DOUBLE-0.0_C_DOUBLE)*(REAL(i,C_DOUBLE)-1.0)/(nintervals_theta2-1.0)
ENDDO

DO i=1,nintervals_phi
        phis(i)=0.0_C_DOUBLE+(2.0*PI-0.0_C_DOUBLE)*(REAL(i,C_DOUBLE)-1.0)/(nintervals_phi-1.0)
ENDDO

WRITE (output_unit, '(A)') 'Generating element independent datasets'

!$OMP PARALLEL DEFAULT(shared) PRIVATE(i,j,k,l,m,cdfs)

ALLOCATE(cdfs(nintervals_phi))



!$OMP DO
DO i=1,nintervals_theta2
        DO j=1,nintervals_phi
        cdfs(j)= &
        (phis(j)-thetas(i)*&
        SIN(2.0*phis(j)))/2.0/PI
        ENDDO
        k=1

 !       WRITE (100,*) cdfs

        DO j=1,nintervals_phi
                IF (cdfs(j) >= rs(k)) THEN
                        phi(i,k) = phis(j)
                        IF (k == nintervals_r) EXIT
                        k=k+1
                ENDIF
        ENDDO
        phi(i,1) = 0.0
        phi(i,nintervals_r) = 2.0*PI

ENDDO

!$OMP END DO
DEALLOCATE(cdfs)
!$OMP END PARALLEL

ENDSUBROUTINE xmi_db_Z_independent
ENDMODULE xmimsim_data
