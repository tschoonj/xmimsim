!Copyright (C) 2010-2014 Tom Schoonjans and Laszlo Vincze

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

!An example on how to produce a custom detector response function
!Here I have essentially used the default code from xmi_detector_f.F90,
!but the peak shaping is purely Gaussian, so no tailing or so.
!Essentially I removed all the magic number stuff from He et al. for 
!the purpose of demonstration.
!Ensure that the xmi_detector_convolute_all_custom function is exported.
!You can accomplish this with a linker option or by adding compiler-dependent 
!directives.


MODULE xmimsim_custom_response1

USE :: xmimsim_detector
USE, INTRINSIC :: ISO_C_BINDING
USE :: omp_lib

PRIVATE

PUBLIC :: xmi_detector_convolute_all_custom


CONTAINS

SUBROUTINE xmi_detector_convolute_all_custom(inputFPtr, channels_noconvPtr,&
channels_convPtr, options, escape_ratiosCPtr, n_interactions_all,&
zero_inter) BIND(C,NAME='xmi_detector_convolute_all_custom')
        IMPLICIT NONE
!the following lines may be necessary for Windows when not using libtool:
!support is added here for
!gfortran and intel fortran, however the latter is untested!!!
!Keep in mind that you will need to recompile XMI-MSIM from scratch with the
!intel compiler to get this working... which is extremely doubtful since it
!doesn't integrate at all with the buildscripts...
#ifdef __GFORTRAN__
!GCC$ ATTRIBUTES DLLEXPORT:: xmi_detector_convolute_all_custom
#elif defined(__INTEL_COMPILER)
!DEC$ ATTRIBUTES DLLEXPORT:: xmi_detector_convolute_all_custom
#endif
        TYPE (C_PTR), INTENT(IN), VALUE :: inputFPtr
        TYPE (C_PTR), INTENT(IN), VALUE :: channels_noconvPtr
        TYPE (C_PTR), INTENT(IN), VALUE :: channels_convPtr
        TYPE (xmi_escape_ratiosC), INTENT(IN) :: escape_ratiosCPtr
        TYPE (xmi_main_options), VALUE, INTENT(IN) :: options
        INTEGER (C_INT), VALUE, INTENT(IN) :: n_interactions_all, zero_inter

        TYPE (C_PTR), POINTER, DIMENSION(:) :: channels_noconv, channels_conv

        INTEGER (C_INT) :: i, start_index

        IF (zero_inter .EQ. 1_C_INT) THEN 
                start_index = 1 
        ELSE 
                start_index = 2
        ENDIF

        CALL C_F_POINTER(channels_noconvPtr, &
        channels_noconv,[n_interactions_all+1])
        CALL C_F_POINTER(channels_convPtr, &
        channels_conv,[n_interactions_all+1])

!$omp parallel do default(shared) private(i)&
!$omp num_threads(options%omp_num_threads)
        DO i=start_index, n_interactions_all+1
               CALL xmi_detector_convolute_custom(inputFPtr, channels_noconv(i),&
               channels_conv(i), options, escape_ratiosCPtr, i-1) 
        ENDDO
!$omp end parallel do

ENDSUBROUTINE xmi_detector_convolute_all_custom

SUBROUTINE xmi_detector_convolute_custom(inputFPtr, channels_noconvPtr,&
channels_convPtr, options, escape_ratiosCPtr, n_interactions&
)
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN), VALUE :: inputFPtr, channels_noconvPtr
        TYPE (C_PTR), INTENT(INOUT) :: channels_convPtr
        TYPE (xmi_escape_ratiosC), INTENT(IN) :: escape_ratiosCPtr
        TYPE (xmi_main_options), VALUE, INTENT(IN) :: options
        INTEGER (C_INT), VALUE, INTENT(IN) :: n_interactions

        TYPE (xmi_escape_ratios) :: escape_ratios 
        TYPE (xmi_input), POINTER :: inputF
        REAL (C_DOUBLE), POINTER, DIMENSION(:) :: channels_noconv,&
        channels_temp
        !REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:), TARGET, SAVE ::&
        REAL (C_DOUBLE), POINTER, DIMENSION(:) ::&
        channels_conv
        INTEGER (C_LONG) :: nlim
        REAL (C_DOUBLE) :: a,b
        REAL (C_DOUBLE), PARAMETER :: c =&
        SQRT(2.0_C_DOUBLE)/(2.0_C_DOUBLE*SQRT(2.0_C_DOUBLE*LOG(2.0_C_DOUBLE)))
        REAL (C_DOUBLE), DIMENSION(:), ALLOCATABLE :: R 
        INTEGER (C_INT) :: I0, I
        REAL (C_DOUBLE) :: E0, E, B0, FWHM, A0, A3, A4, ALFA, X, G, F, my_sum,&
        CBG



        CALL C_F_POINTER(inputFPtr, inputF)
        CALL C_F_POINTER(channels_noconvPtr, channels_noconv,[inputF%detector%nchannels])
        !pointer remapping doesnt work with gfortran 4.4
        !channels_noconv(0:nchannels-1) => channels_noconv

        !escape_ratios
        IF (options%use_escape_peaks .EQ. 1_C_INT) THEN
        escape_ratios%n_elements = escape_ratiosCPtr%n_elements
        escape_ratios%n_fluo_input_energies = escape_ratiosCPtr%n_fluo_input_energies
        escape_ratios%n_compton_input_energies = escape_ratiosCPtr%n_compton_input_energies
        escape_ratios%n_compton_output_energies = escape_ratiosCPtr%n_compton_output_energies
        CALL C_F_POINTER(escape_ratiosCPtr%Z,&
        escape_ratios%Z,[escape_ratios%n_elements]) 
        CALL C_F_POINTER(escape_ratiosCPtr%fluo_escape_ratios,&
        escape_ratios%fluo_escape_ratios,[escape_ratios%n_elements,ABS(L3P3_LINE),&
        escape_ratios%n_fluo_input_energies]) 
        CALL C_F_POINTER(escape_ratiosCPtr%fluo_escape_input_energies,&
        escape_ratios%fluo_escape_input_energies,[escape_ratios%n_fluo_input_energies]) 
        CALL C_F_POINTER(escape_ratiosCPtr%compton_escape_ratios,&
        escape_ratios%compton_escape_ratios,[escape_ratios%n_compton_input_energies,&
        escape_ratios%n_compton_output_energies]) 
        CALL C_F_POINTER(escape_ratiosCPtr%compton_escape_input_energies,&
        escape_ratios%compton_escape_input_energies,[escape_ratios%n_compton_input_energies]) 
        CALL C_F_POINTER(escape_ratiosCPtr%compton_escape_output_energies,&
        escape_ratios%compton_escape_output_energies,[escape_ratios%n_compton_output_energies]) 
        ENDIF
        

        !allocate memory for results
        ALLOCATE(channels_temp(0:inputF%detector%nchannels-1))
        ALLOCATE(channels_conv(0:inputF%detector%nchannels-1))
        !
        nlim = INT(inputF%detector%max_convolution_energy/inputF%detector%gain)
        IF (nlim .GE. inputF%detector%nchannels) nlim = inputF%detector%nchannels-1


        a = inputF%detector%noise**2
        b = (2.3548)**2 * 3.85 *inputF%detector%fano/1000.0


        channels_temp(0:inputF%detector%nchannels-1) = channels_noconv(1:inputF%detector%nchannels)
        channels_conv = 0.0_C_DOUBLE

        IF (options%use_escape_peaks == 1_C_INT) THEN
                
                IF (options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
                CALL xmi_print_progress('Calculating escape peaks after interactions: '&
                //C_NULL_CHAR, n_interactions)
#else
                WRITE(output_unit,'(A, I2)') 'Calculating escape peaks after interactions: ',&
                n_interactions
#endif
                !escape peak
                CALL xmi_detector_escape(channels_temp, inputF, escape_ratios)
        ENDIF

        !sum peaks
        IF (options%use_sum_peaks == 1_C_INT) THEN
                IF (options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
                        CALL xmi_print_progress('Calculating pile-up after interactions: '&
                        //C_NULL_CHAR, n_interactions)
#else
                        WRITE(output_unit,'(A,I2)') 'Calculating pile-up after interactions: ',&
                        n_interactions
#endif
                CALL xmi_detector_sum_peaks(inputF, channels_temp)
        ENDIF

        ALLOCATE(R(0:nlim+100))
        R = 0.0_C_DOUBLE

        IF (options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
                CALL xmi_print_progress('Applying Gaussian convolution after interactions: '&
                //C_NULL_CHAR, n_interactions)
#else
                WRITE(output_unit,'(A,I2)') 'Applying Gaussian convolution after interactions: ',&
                n_interactions
#endif
        DO I0=0,nlim
                E0 = inputF%detector%zero + inputF%detector%gain*I0
                IF (E0 .LT. 1.0_C_DOUBLE) CYCLE
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


                my_sum = 0.0_C_DOUBLE
                DO I=0,I0+100
                        IF (I .GE. inputF%detector%nchannels) THEN
                                EXIT
                        ENDIF
                        E = inputF%detector%zero + inputF%detector%gain*I
                        X=(E-E0)/B0
                        G=EXP(-X*X)
                        R(I) = A0*G
#if DEBUG == 1
                        IF (I .EQ. 150 .AND. I0 .EQ. 100) THEN
                                WRITE (*,'(A,F14.5)') 'R(I): ',R(I) 
                                WRITE (*,'(A,F14.5)') 'F: ',F 
                        ENDIF
#endif
                        my_sum = my_sum + R(I)
                ENDDO
                !my_sum = SUM(R)
                DO I=0,I0+100
                        IF (I .GE. inputF%detector%nchannels) THEN
                                EXIT
                        ENDIF
                        channels_conv(I)=channels_conv(I)+R(I)*channels_temp(I0)/my_sum
                ENDDO

        ENDDO

        IF (options%use_poisson == 1_C_INT) THEN
                IF (options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
                        CALL xmi_print_progress('Calculating Poisson noise after interactions: '&
                        //C_NULL_CHAR, n_interactions)
#else
                        WRITE(output_unit,'(A,I2)') 'Calculating Poisson noise after interactions: ',&
                        n_interactions
#endif
                CALL xmi_detector_poisson(channels_conv)
        ENDIF


#if DEBUG == 1
        WRITE (*,'(A,F15.4)') 'channel 223 contents after conv: ', channels_conv(223)
        WRITE (*,'(A,ES14.6)') 'channels_temp max: ',MAXVAL(channels_temp)
        WRITE (*,'(A,ES14.6)') 'channels_conv max: ',MAXVAL(channels_conv)
#endif
        DEALLOCATE(channels_temp)
        channels_convPtr = C_LOC(channels_conv(0))

        RETURN

ENDSUBROUTINE xmi_detector_convolute_custom
ENDMODULE xmimsim_custom_response1
