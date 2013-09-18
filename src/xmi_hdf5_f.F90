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


MODULE xmimsim_hdf5

USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux
USE :: fgsl
USE :: ISO_FORTRAN_ENV


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

        FUNCTION xmi_db_read_dataset(hdf5_vars, data)&
                BIND(C,NAME='xmi_db_read_dataset')&
                RESULT(rv)
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                TYPE (C_PTR), INTENT(IN), VALUE :: hdf5_vars
                TYPE (C_PTR), INTENT(IN), VALUE :: data
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
        CHARACTER (LEN=2) :: element


        rv = 0
        !associate pointers C -> Fortran
        CALL C_F_POINTER(xmi_inputFPtr, xmi_inputF)

        hdf5_vars = xmi_db_open(xmi_hdf5_file) 



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





        !allocate xmi_hdf5 structure
        ALLOCATE(xmi_hdf5F)


        !start by reading in the Z independent part...
        !RayleighPhi
        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A)') 'Opening group RayleighPhi'
        ENDIF
        IF (xmi_db_open_group(hdf5_vars, C_CHAR_'RayleighPhi'//C_NULL_CHAR) &
                .EQ. 0_C_INT) RETURN

        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A)') 'Opening dataset RayleighPhi_ICDF'
        ENDIF
        IF (xmi_db_open_dataset(hdf5_vars,&
                C_CHAR_'RayleighPhi_ICDF'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                0_C_INT) RETURN
        CALL C_F_POINTER(dimsPtr, dims, [ndims])
        IF (ndims .NE. 2_C_INT) THEN
                WRITE (error_unit,'(A)') &
                'Wrong dimensions found after opening dataset'
                RETURN
        ENDIF

        !read the dataset
        ALLOCATE(xmi_hdf5F%RayleighPhi_ICDF(dims(1),dims(2)))
        NULLIFY(dims)
        CALL xmi_free(dimsPtr)

        IF (xmi_db_read_dataset(hdf5_vars, C_LOC(xmi_hdf5F%RayleighPhi_ICDF(1,1)))&
                .EQ. 0_C_INT) RETURN

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
        ALLOCATE(xmi_hdf5F%RayleighThetas(dims(1)))
        NULLIFY(dims)
        CALL xmi_free(dimsPtr)

        IF (xmi_db_read_dataset(hdf5_vars, C_LOC(xmi_hdf5F%RayleighThetas(1)))&
                .EQ. 0_C_INT) RETURN

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
        ALLOCATE(xmi_hdf5F%RayleighRandomNumbers(dims(1)))
        NULLIFY(dims)
        CALL xmi_free(dimsPtr)

        IF (xmi_db_read_dataset(hdf5_vars, C_LOC(xmi_hdf5F%RayleighRandomNumbers(1)))&
                .EQ. 0_C_INT) RETURN

        !close group
        IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN

        !ComptonPhi
        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A)') 'Opening group ComptonPhi'
        ENDIF
        IF (xmi_db_open_group(hdf5_vars, C_CHAR_'ComptonPhi'//C_NULL_CHAR) &
                .EQ. 0_C_INT) RETURN

        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A)') 'Opening dataset ComptonPhi_ICDF'
        ENDIF
        IF (xmi_db_open_dataset(hdf5_vars,&
                C_CHAR_'ComptonPhi_ICDF'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                0_C_INT) RETURN
        CALL C_F_POINTER(dimsPtr, dims, [ndims])
        IF (ndims .NE. 3_C_INT) THEN
                WRITE (error_unit,'(A)') &
                'Wrong dimensions found after opening dataset'
                RETURN
        ENDIF

        !read the dataset
        ALLOCATE(xmi_hdf5F%ComptonPhi_ICDF(dims(1),dims(2),dims(3)))
        NULLIFY(dims)
        CALL xmi_free(dimsPtr)

        IF (xmi_db_read_dataset(hdf5_vars, C_LOC(xmi_hdf5F%ComptonPhi_ICDF(1,1,1)))&
                .EQ. 0_C_INT) RETURN

        !Read ComptonThetas and ComptonRandomNumbers
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
        ALLOCATE(xmi_hdf5F%ComptonThetas(dims(1)))
        NULLIFY(dims)
        CALL xmi_free(dimsPtr)

        IF (xmi_db_read_dataset(hdf5_vars, C_LOC(xmi_hdf5F%ComptonThetas(1)))&
                .EQ. 0_C_INT) RETURN

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
        ALLOCATE(xmi_hdf5F%ComptonRandomNumbers(dims(1)))
        NULLIFY(dims)
        CALL xmi_free(dimsPtr)

        IF (xmi_db_read_dataset(hdf5_vars, C_LOC(xmi_hdf5F%ComptonRandomNumbers(1)))&
                .EQ. 0_C_INT) RETURN

        !energies
        IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A)') 'Opening dataset Energies'
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
        ALLOCATE(xmi_hdf5F%ComptonEnergies(dims(1)))
        NULLIFY(dims)
        CALL xmi_free(dimsPtr)

        IF (xmi_db_read_dataset(hdf5_vars, C_LOC(xmi_hdf5F%ComptonEnergies(1)))&
                .EQ. 0_C_INT) RETURN



        !close group
        IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN


        !read Z dependent part...
        ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(SIZE(uniqZ)))
        DO i=1,SIZE(uniqZ) 
!
!
!       Internal files issue in intel fortran...
!


                xmi_hdf5F%xmi_hdf5_Zs(i)%Z = uniqZ(i)
                xmi_hdf5F%xmi_hdf5_Zs(i)%Zindex = i

                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Opening group ',elements(uniqZ(i))&
                // C_CHAR_'/Theta_ICDF'//C_NULL_CHAR
                ENDIF
                IF (xmi_db_open_group(hdf5_vars, elements(uniqZ(i)) &
                        // C_CHAR_'/Theta_ICDF'//C_NULL_CHAR) &
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
                C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%RayleighTheta_ICDF(1,1)))&
                .EQ. 0_C_INT) RETURN
                
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
                C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%ComptonTheta_ICDF(1,1)))&
                .EQ. 0_C_INT) RETURN


                !Read Doppler pz ICDF
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                        WRITE (output_unit,'(A)') 'Opening dataset Doppler_pz_ICDF'
                ENDIF
                IF (xmi_db_open_dataset(hdf5_vars,&
                        C_CHAR_'Doppler_pz_ICDF'//C_NULL_CHAR, ndims, dimsPtr) .EQ.&
                        0_C_INT) RETURN
                CALL C_F_POINTER(dimsPtr, dims, [ndims])
                IF (ndims .NE. 1_C_INT) THEN
                        WRITE (error_unit,'(A)') &
                        'Wrong dimensions found after opening dataset'
                        RETURN
                ENDIF
                !read the dataset
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%DopplerPz_ICDF(dims(1)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%DopplerPz_ICDF(1)))&
                .EQ. 0_C_INT) RETURN

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
                C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%FluorYieldsCorr(0)))&
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
                ALLOCATE(xmi_hdf5F%xmi_hdf5_Zs(i)%Energies(dims(1)))
                NULLIFY(dims)
                CALL xmi_free(dimsPtr)
                IF (xmi_db_read_dataset(hdf5_vars, &
                C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%Energies(1)))&
                .EQ. 0_C_INT) RETURN

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
                C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%RandomNumbers(1)))&
                .EQ. 0_C_INT) RETURN

                !close group
                IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN



                !Read interactions probabilities
                IF (options%extra_verbose .EQ. 1_C_INT) THEN
                WRITE (output_unit,'(A,A)') 'Opening group ',elements(uniqZ(i))&
                // C_CHAR_'/Interaction probabilities'//C_NULL_CHAR
                ENDIF
                IF (xmi_db_open_group(hdf5_vars, elements(uniqZ(i))//&
                        C_CHAR_'/Interaction probabilities'//C_NULL_CHAR) &
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
                C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%energies(1)))&
                .EQ. 0_C_INT) RETURN


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
                C_LOC(xmi_hdf5F%xmi_hdf5_Zs(i)%interaction_probs%Rayl_and_Compt(1,1)))&
                .EQ. 0_C_INT) RETURN

                !close group
                IF (xmi_db_close_group(hdf5_vars) .EQ. 0_C_INT) RETURN


        ENDDO


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


        DEALLOCATE(xmi_hdf5F%RayleighPhi_ICDF)
        DEALLOCATE(xmi_hdf5F%RayleighThetas)
        DEALLOCATE(xmi_hdf5F%RayleighRandomNumbers)
        DEALLOCATE(xmi_hdf5F%ComptonPhi_ICDF)
        DEALLOCATE(xmi_hdf5F%ComptonThetas)
        DEALLOCATE(xmi_hdf5F%ComptonEnergies)
        DEALLOCATE(xmi_hdf5F%ComptonRandomNumbers)

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
USE :: fgsl
USE,INTRINSIC :: ISO_C_BINDING
USE,INTRINSIC :: ISO_FORTRAN_ENV

IMPLICIT NONE

INTEGER (8), PARAMETER :: nintervals_r = 2000, nintervals_e = 200, maxz = 94, &
nintervals_theta=100000, nintervals_theta2=200,nintervals_phi=100000, &
nintervals_e_ip = 10000, nintervals_pz=1000000
REAL (KIND=C_DOUBLE), PARAMETER :: maxe = 100.0, lowe = 0.1, &
        PI = 3.14159265359,maxpz = 100.0
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
INTEGER(HID_T) :: attribute_id
REAL (C_DOUBLE) :: version_real
CHARACTER (len=10) :: version_string = VERSION
INTEGER(HSIZE_T),DIMENSION(2) :: dims = [nintervals_e, nintervals_r]
INTEGER(HSIZE_T),DIMENSION(2) :: dims2 = [nintervals_theta2, nintervals_r]
INTEGER(HSIZE_T),DIMENSION(3) :: dims3 = [nintervals_theta2, nintervals_e,nintervals_r]

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

!$OMP PARALLEL DEFAULT(shared) PRIVATE(i,j,k,l,m,trapez,temp_sum,sumz,energies_flt,temp_energy,trapez2,temp_array)

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

!create to hdf5 file
CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F,file_id,h5error)




!Create groups and fill them up...
CALL h5gopen_f(file_id, '/', group_id, h5error)
CALL h5screate_f(H5S_SCALAR_F, dspace_id, h5error)
!create version attribute
CALL h5acreate_f(group_id, 'version', H5T_NATIVE_DOUBLE, dspace_id,&
attribute_id,h5error)
!convert version string to double
READ (version_string,*) version_real
CALL h5awrite_f(attribute_id, H5T_NATIVE_DOUBLE, version_real, [1_HSIZE_T], h5error)
CALL h5aclose_f(attribute_id, h5error)
CALL h5sclose_f(dspace_id,h5error)
CALL h5gclose_f(group_id,h5error)

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

SUBROUTINE xmi_db_Z_specific(rayleigh_thetaPtr, compton_thetaPtr, energiesPtr, rsPtr,&
doppler_pzPtr, fluor_yield_corrPtr, ipPtr, nintervals_r, nintervals_e, maxz, nintervals_e_ip) &
BIND(C,NAME='xmi_db_Z_specific')

IMPLICIT NONE

TYPE (C_PTR), VALUE, INTENT(IN) :: rayleigh_thetaPtr, compton_thetaPtr, &
energiesPtr, rsPtr, doppler_pzPtr, fluor_yield_corrPtr, ipPtr
INTEGER (C_INT), VALUE, INTENT(IN) :: nintervals_r, nintervals_e, maxz, &
nintervals_e_ip

REAL (C_DOUBLE), DIMENSION(:,:,:), POINTER::&
rayleigh_theta, compton_theta
REAL (C_DOUBLE), DIMENSION(:), POINTER :: energies,rs
REAL (C_DOUBLE), DIMENSION(:,:), POINTER :: doppler_pz
REAL (C_DOUBLE), DIMENSION(:,:), POINTER :: fluor_yield_corr
TYPE (interaction_probC), DIMENSION(:), POINTER :: ip
TYPE (interaction_prob) :: ip_temp

INTEGER (8), PARAMETER :: nintervals_theta=100000, nintervals_theta2=200,nintervals_phi=100000, &
nintervals_pz=1000000
REAL (KIND=C_DOUBLE), PARAMETER :: maxe = 100.0, lowe = 0.1, &
        PI = 3.14159265359,maxpz = 100.0

REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: trapez, thetas,sumz,phis,trapez2
REAL (KIND=C_FLOAT), ALLOCATABLE, DIMENSION(:), TARGET :: energies_flt,&
temp_array
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: energies_dbl
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: pzs

INTEGER :: stat,i,j,k,l,m,n
REAL (KIND=C_DOUBLE) :: temp_sum,K0K
REAL (KIND=C_FLOAT) :: temp_energy,temp_total_cs


CALL C_F_POINTER(rayleigh_thetaPtr,rayleigh_theta,[maxz, nintervals_e,&
nintervals_r])
CALL C_F_POINTER(compton_thetaPtr,compton_theta,[maxz, nintervals_e,&
nintervals_r])
CALL C_F_POINTER(energiesPtr,energies,[nintervals_e])
CALL C_F_POINTER(rsPtr,rs,[nintervals_r])
CALL C_F_POINTER(doppler_pzPtr,doppler_pz,[maxz,nintervals_r])
CALL C_F_POINTER(fluor_yield_corrPtr,fluor_yield_corr,[maxz,M5_SHELL+1])
CALL C_F_POINTER(ipPtr, ip,[maxz])


CALL SetErrorMessages(0)

ALLOCATE(thetas(nintervals_theta), pzs(nintervals_pz))

!Fill up the energies and rs arrays...

DO i=1,nintervals_e
        energies(i) = lowe + (maxe-lowe)*(REAL(i,C_DOUBLE)-1.0)/(nintervals_e-1.0)
ENDDO

DO i=1,nintervals_pz
        pzs(i) = 0.0_C_DOUBLE + maxpz*(REAL(i,C_DOUBLE)-1.0)/(nintervals_pz-1.0)
ENDDO

DO i=1,nintervals_r
        rs(i) = (REAL(i,C_DOUBLE)-1.0)/(nintervals_r-1.0)
ENDDO

DO i=1,nintervals_theta
        thetas(i) = 0.0_C_DOUBLE+(PI)*(REAL(i,C_DOUBLE)-1.0)/(nintervals_theta-1.0)
ENDDO

!$OMP PARALLEL DEFAULT(shared) PRIVATE(i,j,k,l,m,trapez,temp_sum,sumz,energies_flt,&
!$OMP temp_energy,trapez2,temp_array,ip_temp,temp_total_cs)

ALLOCATE(trapez(nintervals_theta-1))
ALLOCATE(trapez2(nintervals_pz-1))
ALLOCATE(sumz(nintervals_theta-1))
!$OMP DO

Zloop:DO i=1,maxz 
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


        ip_temp = energies_flt
        !ALLOCATE(ip_temp%energies(SIZE(energies_flt)),ip_temp%Rayl_and_Compt(SIZE(energies_flt),2))
        !DO k=1,SIZE(energies_flt)
        !        ip_temp%energies(i)=REAL(energies_flt(i),KIND=C_DOUBLE)
        !ENDDO

        

        DO k=1,SIZE(energies_flt)
                temp_total_cs = CS_Total_Kissel(i,energies_flt(k))
                ip_temp%Rayl_and_Compt(k,1) = CS_Rayl(i,energies_flt(k))/temp_total_cs
                ip_temp%Rayl_and_Compt(k,2) = CS_Compt(i,energies_flt(k))/temp_total_cs+&
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
        fluor_yield_corr(i,K_SHELL+1) = FluorYield(i,K_SHELL) 
        fluor_yield_corr(i,L1_SHELL+1) = FluorYield(i,L1_SHELL)+&
                        (CKTB(i,FL12_TRANS)*FluorYield(i,L2_SHELL))+&
                        (CKTB(i,FL13_TRANS)+CKTB(i,FL12_TRANS)*CKTB(i,FL23_TRANS))*&
                        FluorYield(i,L3_SHELL)
        fluor_yield_corr(i,L2_SHELL+1) = FluorYield(i,L2_SHELL)+&
                        (CKTB(i,FL23_TRANS)*FluorYield(i,L3_SHELL))
        fluor_yield_corr(i,L3_SHELL+1) = FluorYield(i,L3_SHELL)
        fluor_yield_corr(i,M1_SHELL+1) = &
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
        fluor_yield_corr(i,M2_SHELL+1) = &
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
        fluor_yield_corr(i,M3_SHELL+1) = &
                        !M3_SHELL
                        FluorYield(i,M3_SHELL)+&
                        !M4_SHELL
                        CKTB(i,FM34_TRANS)*FluorYield(i,M4_SHELL)+&
                        !M5_SHELL
                        (CKTB(i,FM35_TRANS)+CKTB(i,FM34_TRANS)*CKTB(i,FM45_TRANS))*&
                        FluorYield(i,M5_SHELL)
        fluor_yield_corr(i,M4_SHELL+1) = &
                        !M4_SHELL
                        FluorYield(i,M4_SHELL)+&
                        !M5_SHELL
                        CKTB(i,FM45_TRANS)*FluorYield(i,M5_SHELL)
        fluor_yield_corr(i,M5_SHELL+1) = &
                        !M5_SHELL
                        FluorYield(i,M5_SHELL)
#undef CKTB

ENDDO Zloop
!$OMP END DO
!$OMP END PARALLEL

ENDSUBROUTINE xmi_db_Z_specific

SUBROUTINE xmi_db_Z_independent(rayleigh_phiPtr, compton_phiPtr, thetasPtr, &
rsPtr, energiesPtr, nintervals_theta2, nintervals_e, nintervals_r) &
BIND(C,NAME='xmi_db_Z_independent')

IMPLICIT NONE

TYPE (C_PTR), VALUE, INTENT(IN) :: rayleigh_phiPtr, compton_phiPtr, thetasPtr, &
rsPtr, energiesPtr
INTEGER (C_INT), VALUE, INTENT(IN) :: nintervals_theta2, nintervals_e, &
nintervals_r
REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:,:) :: &
        rayleigh_phi
REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:,:,:) ::  compton_phi
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: cdfs, phis
REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:) :: thetas, energies, rs
INTEGER (8), PARAMETER :: nintervals_phi=100000
INTEGER (8) :: i,j,k,l,m
REAL (KIND=C_DOUBLE) :: K0K
REAL (KIND=C_DOUBLE) :: PI = 3.14159265359


CALL C_F_POINTER(rayleigh_phiPtr, rayleigh_phi, [nintervals_theta2,nintervals_r])
CALL C_F_POINTER(compton_phiPtr, compton_phi, [nintervals_theta2,nintervals_e,nintervals_r])
CALL C_F_POINTER(thetasPtr, thetas, [nintervals_theta2])
!energies and rs will be reused
CALL C_F_POINTER(energiesPtr,energies,[nintervals_e])
CALL C_F_POINTER(rsPtr,rs,[nintervals_r])

ALLOCATE(phis(nintervals_phi))

DO i=1,nintervals_theta2
        thetas(i) = 0.0_C_DOUBLE+(PI-0.0_C_DOUBLE)*(REAL(i,C_DOUBLE)-1.0)/(nintervals_theta2-1.0)
ENDDO

DO i=1,nintervals_phi
        phis(i)=0.0_C_DOUBLE+(2.0*PI-0.0_C_DOUBLE)*(REAL(i,C_DOUBLE)-1.0)/(nintervals_phi-1.0)
ENDDO

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

ENDDO

!$OMP END DO
DEALLOCATE(cdfs)
!$OMP END PARALLEL


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


ENDSUBROUTINE xmi_db_Z_independent
ENDMODULE xmimsim_hdf5
