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

MODULE xmimsim_detector


USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux
USE :: omp_lib
USE :: fgsl


CONTAINS

SUBROUTINE xmi_detector_sum_peaks(inputF, channels)
        IMPLICIT NONE
        TYPE (xmi_input), POINTER, INTENT(IN) :: inputF
        REAL (C_DOUBLE), DIMENSION(:), INTENT(INOUT) :: channels
        
        REAL (C_DOUBLE) :: Nt
        INTEGER (C_LONG) :: Nt_long
        INTEGER (C_INT) :: nchannels
        INTEGER (C_LONG) :: i,j
        REAL (C_DOUBLE), DIMENSION(:), ALLOCATABLE :: new_channels
        INTEGER :: max_threads, thread_num

        TYPE (fgsl_rng_type) :: rng_type
        TYPE (fgsl_rng) :: rng
        INTEGER (C_LONG), ALLOCATABLE, TARGET, DIMENSION(:) :: seeds
        INTEGER (fgsl_size_t), DIMENSION(100) :: pulses
        INTEGER (C_LONG) :: npulses, npulses_all
        TYPE (fgsl_ran_discrete_t) :: preproc
        REAL (C_DOUBLE) :: lambda, mu, energies_sum
        REAL (C_DOUBLE), DIMENSION(100) :: deltaT
        INTEGER (C_LONG) :: n_sum_counts
        INTEGER (fgsl_size_t) :: pulses_sum

#if DEBUG == 1
        WRITE (6,'(A)') 'Entering xmi_detector_sum_peaks'
#endif



        nchannels = SIZE(channels)
        Nt = SUM(channels)
        Nt_long = INT(Nt, KIND=C_LONG)
        lambda = Nt/inputF%detector%live_time
        mu = 1.0_C_DOUBLE/lambda

#if DEBUG == 1
        WRITE (6,'(A,ES12.4)') 'Nt: ',Nt
        WRITE (6,'(A,ES12.4)') 'lambda: ',lambda
        WRITE (6,'(A,ES12.4)') 'mu: ',mu
        WRITE (6,'(A,ES12.4)') 'pulse_widht: ',inputF%detector%pulse_width
#endif

        ALLOCATE(new_channels(nchannels))

        new_channels = 0.0_C_DOUBLE
        n_sum_counts = 0

        !prepare discrete distribution
        preproc = &
        fgsl_ran_discrete_preproc(INT(nchannels,KIND=fgsl_size_t),channels)


        max_threads = omp_get_max_threads()
        ALLOCATE(seeds(max_threads))

        max_threads=1

        !fetch some seeds
        IF (xmi_get_random_numbers(C_LOC(seeds), INT(max_threads,KIND=C_LONG)) == 0) RETURN


        rng_type = fgsl_rng_mt19937

!!$omp parallel default(shared) private(pulses_sum,energies_sum,deltaT,pulses,npulses,npulses_all,i,rng,thread_num) reduction(+:new_channels,n_sum_counts)

!
!
!       Initialize random number generator
!
!
        thread_num = omp_get_thread_num()

        rng = fgsl_rng_alloc(rng_type)
        CALL fgsl_rng_set(rng,seeds(thread_num+1))

        !i=1,Nt_long/max_threads
       
        npulses = 0_C_LONG
        npulses_all = 0_C_LONG

        gardner:DO 
                !get pulse        
                npulses = npulses+1
                npulses_all = npulses_all+1
       
                IF (npulses .GT. 100) THEN
                        WRITE (6,'(A)') 'pulsetrain maximum reached'
                        WRITE (6,'(A)') 'Adjust pulsewidth value or'
                        WRITE (6,'(A)') 'disable the pileup generation'
                        CALL EXIT(1)
                ENDIF

                pulses(npulses) = fgsl_ran_discrete(rng, preproc)+1

                !check deltaT
                deltaT(npulses) = fgsl_ran_exponential(rng,mu)

#if DEBUG == 1
                WRITE (6,'(A,I)') 'npulses: ',npulses
                WRITE (6,'(A,I)') 'npulses_all: ',npulses_all
                WRITE (6,'(A,I)') 'pulses: ',pulses(npulses)
                WRITE (6,'(A,ES13.4)') 'deltaT: ',deltaT(npulses)
#endif
                IF (deltaT(npulses) .GT. &
                inputF%detector%pulse_width) THEN
                        !deltaT is larger than pulse_width =>
                        !separate pulses detected!
                        !add count to appropriate channel
                        IF (npulses .EQ. 1) THEN
                                !just one pulse...
                                IF (pulses(1) .LT. 0 .OR. pulses(1) .GE.&
                                nchannels) THEN
                                        WRITE (6,'(A,I10)') 'pulses exception:',&
                                        pulses(1)
                                ENDIF
                                new_channels(pulses(1)) = &
                                new_channels(pulses(1)) + 1
                        ELSE
                                !more than one pulse...
                                energies_sum = SUM((pulses(1:npulses)*inputF%detector%gain)+&
                                inputF%detector%zero)
                                pulses_sum = &
                                (energies_sum-inputF%detector%zero)/&
                                inputF%detector%gain
                                n_sum_counts = n_sum_counts + 1
                                IF (pulses_sum .GT. 0 .AND. pulses_sum .LE.&
                                nchannels) THEN
                                        new_channels(pulses_sum) = &
                                        new_channels(pulses_sum) + 1
                                ENDIF
                        ENDIF
                        IF (npulses_all .GE. Nt_long/max_threads) EXIT
                        npulses = 0
                ENDIF
        ENDDO gardner

        CALL fgsl_rng_free(rng) 

!!$omp end parallel

     !for some reason does ifort not put this function into the fgsl module
     !file!!!
     !CALL fgsl_ran_discrete_free(preproc)

#if DEBUG == 1
        WRITE (6,'(A,I)') 'Nt_long: ',Nt_long
        WRITE (6,'(A,I)') 'n_sum_counts: ',n_sum_counts
#endif

        channels = new_channels

ENDSUBROUTINE xmi_detector_sum_peaks

SUBROUTINE xmi_detector_convolute(inputFPtr, channels_noconvPtr,&
channels_convPtr,nchannels, options, escape_ratiosCPtr) BIND(C,NAME='xmi_detector_convolute')
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN), VALUE :: inputFPtr, channels_noconvPtr
        INTEGER (C_INT), VALUE, INTENT(IN) :: nchannels
        TYPE (C_PTR), INTENT(INOUT) :: channels_convPtr
        TYPE (xmi_escape_ratiosC), INTENT(IN) :: escape_ratiosCPtr
        TYPE (xmi_main_options), VALUE, INTENT(IN) :: options

        TYPE (xmi_escape_ratios) :: escape_ratios 
        TYPE (xmi_input), POINTER :: inputF
        REAL (C_DOUBLE), POINTER, DIMENSION(:) :: channels_noconv,&
        channels_temp
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:), TARGET, SAVE ::&
        channels_conv
        INTEGER (C_LONG) :: nlim
        REAL (C_DOUBLE) :: a,b
        REAL (C_DOUBLE), PARAMETER :: c =&
        SQRT(2.0_C_DOUBLE)/(2.0_C_DOUBLE*SQRT(2.0_C_DOUBLE*LOG(2.0_C_DOUBLE)))
        REAL (C_DOUBLE), DIMENSION(4096) :: R 
        INTEGER (C_INT) :: I0, I
        REAL (C_DOUBLE) :: E0, E, B0, FWHM, A0, A3, A4, ALFA, X, G, F, my_sum,&
        CBG



        CALL C_F_POINTER(inputFPtr, inputF)
        CALL C_F_POINTER(channels_noconvPtr, channels_noconv,[nchannels])

        !escape_ratios
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
        


#if DEBUG == 1
        WRITE (*,'(A,F15.4)') 'channel 223 contents: ', channels_noconv(223)
#endif

        !allocate memory for results
        ALLOCATE(channels_temp(nchannels))
        IF (ALLOCATED(channels_conv)) DEALLOCATE(channels_conv)
        ALLOCATE(channels_conv(nchannels))
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

        
        IF (options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ == 4
                CALL xmi_print_progress('Calculating escape peaks'&
                //C_NULL_CHAR,-1_C_INT)
#else
                WRITE(output_unit,'(A)') 'Calculating escape peaks'
#endif

        !escape peak
        CALL xmi_detector_escape(channels_temp, inputF, escape_ratios)

#if DEBUG == 1
        WRITE (*,'(A,ES14.6)') 'channels_temp max after escape: ',MAXVAL(channels_temp)
        WRITE (*,'(A,F14.6)') 'a: ',a
        WRITE (*,'(A,F14.6)') 'b: ',b
        WRITE (*,'(A,I)') 'nlim: ',nlim
#endif

        !sum peaks
        IF (options%use_sum_peaks == 1_C_INT) THEN
                IF (options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ == 4
                        CALL xmi_print_progress('Calculating pile-up'&
                        //C_NULL_CHAR, -1_C_INT)
#else
                        WRITE(output_unit,'(A)') 'Calculating pile-up'
#endif
                CALL xmi_detector_sum_peaks(inputF, channels_temp)
        ENDIF

        R = 0.0_C_DOUBLE

        IF (options%verbose == 1_C_INT)&
#if __GNUC__ == 4 && __GNUC_MINOR__ == 4
                CALL xmi_print_progress('Applying Gaussian convolution'&
                //C_NULL_CHAR,-1_C_INT)
#else
                WRITE(output_unit,'(A)') 'Applying Gaussian convolution'
#endif

!!!$omp parallel do default(private) shared(a,b,inputF,I0,nlim,nchannels)
        DO I0=1,nlim
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


                        A3=2.73E-3_C_DOUBLE*EXP(-0.21_C_DOUBLE*E0)+1.E-4_C_DOUBLE
                        A4=0.000188_C_DOUBLE*EXP(-0.00296_C_DOUBLE*(E0**0.763_C_DOUBLE))+&
                          1.355E-5_C_DOUBLE*EXP(0.968_C_DOUBLE*(E0**0.498_C_DOUBLE))
                        ALFA=1.179_C_DOUBLE*EXP(8.6E-4_C_DOUBLE*(E0**1.877_C_DOUBLE))-&
                          7.793_C_DOUBLE*EXP(-3.81_C_DOUBLE*(E0**(-0.0716_C_DOUBLE)))
                my_sum = 0.0_C_DOUBLE
                DO I=1,I0+100
                        IF (I .GT. nchannels) THEN
                                EXIT
                        ENDIF
                        E = inputF%detector%zero + inputF%detector%gain*I
                        X=(E-E0)/B0
                        G=EXP(-X*X)
                        F=ERFC(X)
                        IF (inputF%detector%detector_type .EQ. XMI_DETECTOR_SILI) THEN
                                R(I)= A0*G+1.0_C_DOUBLE*(2.7_C_DOUBLE*A3+15.0_C_DOUBLE*A4*EXP(ALFA*(E-E0)))*F       
                        ELSEIF (inputF%detector%detector_type .EQ. XMI_DETECTOR_SI_SDD) THEN
                                R(I)= A0*G+1.0_C_DOUBLE*(0.63_C_DOUBLE*A3+15.0_C_DOUBLE*A4*EXP(ALFA*(E-E0)))*F       
                        ELSE
                                R(I)= A0*G+1.0_C_DOUBLE*(2.7_C_DOUBLE*A3+15.0_C_DOUBLE*A4*EXP(ALFA*(E-E0)))*F       
                        ENDIF
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

SUBROUTINE xmi_detector_escape(channels_conv,inputF,escape_ratios)
        IMPLICIT NONE
        TYPE (xmi_escape_ratios), INTENT(IN) :: escape_ratios 
        TYPE (xmi_input), INTENT(IN) :: inputF
        REAL (C_DOUBLE), INTENT(INOUT),DIMENSION(:) :: channels_conv

        INTEGER (C_INT) :: i,j,k
        REAL (C_DOUBLE) :: ratio,sum_ratio
        REAL (C_DOUBLE) :: channel_e ,channel_1e, line_e,&
        channel_c,compton_diff,compton_out_diff
        INTEGER (C_INT) :: escape_i
        REAL (C_DOUBLE), DIMENSION(2) :: fluo_a,fluo_b
        INTEGER (C_INT) :: pos,pos_1,pos_2

        channel_1e=(1.5_C_DOUBLE)*inputF%detector%gain+&
        inputF%detector%zero
        compton_out_diff = escape_ratios%compton_escape_output_energies(2)-&
        escape_ratios%compton_escape_output_energies(1)


        DO i=1,SIZE(channels_conv)
                channel_e = (REAL(i)+0.5_C_DOUBLE)*inputF%detector%gain+&
                        inputF%detector%zero
                !fluo escape peaks first...
                sum_ratio = 0.0_C_DOUBLE
                DO j=1,escape_ratios%n_elements
                        IF (channel_e .GT. EdgeEnergy(escape_ratios%Z(j),K_SHELL)) THEN
                        DO k=KL1_LINE,KP5_LINE,-1
                                line_e=LineEnergy(escape_ratios%Z(j),k)
                                IF ((channel_e-line_e) .GE. channel_1e .AND. channel_e .GE.&
                                escape_ratios%fluo_escape_input_energies(1) .AND.&
                                channel_e .LT.&
                                escape_ratios%fluo_escape_input_energies(&
                                escape_ratios%n_fluo_input_energies))&
                                THEN
                                !interpolate
                                pos=findpos(escape_ratios%fluo_escape_input_energies,&
                                channel_e)   
                                IF (pos .LT. 1) THEN
                                WRITE (6,*) 'findpos returns low index'
                                CALL EXIT(1)
                                ENDIF
                                !fluo_energies =>&
                                !escape_ratios%fluo_escape_input_energies(pos:pos+1)
                                fluo_a(1)=escape_ratios%&
                                fluo_escape_input_energies(pos)
                                fluo_b(1)=escape_ratios%&
                                fluo_escape_input_energies(pos+1)
                                !fluo_ratios =>&
                                !escape_ratios%fluo_escape_ratios(j,ABS(k),pos:pos+1)
                                fluo_a(2)=escape_ratios%&
                                fluo_escape_ratios(j,ABS(k),pos)
                                fluo_b(2)=escape_ratios%&
                                fluo_escape_ratios(j,ABS(k),pos+1)
                                ratio=interpolate_simple(fluo_a,fluo_b,channel_e)
                                sum_ratio = sum_ratio+ratio
                                escape_i=INT((channel_e-line_e-inputF%detector%zero)/&
                                inputF%detector%gain)
                                IF (escape_i .GE. 1 .AND. escape_i .LE.&
                                SIZE(channels_conv)) THEN
                                channels_conv(escape_i)=&
                                channels_conv(escape_i)+ratio*channels_conv(i)
                                ENDIF
                                ENDIF
                        ENDDO
                        ENDIF
                        IF  (channel_e .GT. EdgeEnergy(escape_ratios%Z(j),L1_SHELL)) THEN
                        DO k=L1M1_LINE,L1P5_LINE,-1
                                line_e=LineEnergy(escape_ratios%Z(j),k)
                                IF ((channel_e-line_e) .GE. channel_1e .AND. channel_e .GE.&
                                escape_ratios%fluo_escape_input_energies(1) .AND.&
                                channel_e .LT.&
                                escape_ratios%fluo_escape_input_energies(&
                                escape_ratios%n_fluo_input_energies))&
                                THEN
                                !interpolate
                                pos=findpos(escape_ratios%fluo_escape_input_energies,&
                                channel_e)   
                                IF (pos .LT. 1) THEN
                                WRITE (6,*) 'findpos returns low index'
                                CALL EXIT(1)
                                ENDIF
                                !fluo_energies =>&
                                !escape_ratios%fluo_escape_input_energies(pos:pos+1)
                                fluo_a(1)=escape_ratios%&
                                fluo_escape_input_energies(pos)
                                fluo_b(1)=escape_ratios%&
                                fluo_escape_input_energies(pos+1)
                                !fluo_ratios =>&
                                !escape_ratios%fluo_escape_ratios(j,ABS(k),pos:pos+1)
                                fluo_a(2)=escape_ratios%&
                                fluo_escape_ratios(j,ABS(k),pos)
                                fluo_b(2)=escape_ratios%&
                                fluo_escape_ratios(j,ABS(k),pos+1)
                                ratio=interpolate_simple(fluo_a,fluo_b,channel_e)
                                sum_ratio = sum_ratio+ratio
                                escape_i=INT((channel_e-line_e-inputF%detector%zero)/&
                                inputF%detector%gain)
                                IF (escape_i .GE. 1 .AND. escape_i .LE.&
                                SIZE(channels_conv)) THEN
                                channels_conv(escape_i)=&
                                channels_conv(escape_i)+ratio*channels_conv(i)
                                ENDIF
                                ENDIF
                                
                        ENDDO
                        ENDIF
                        IF  (channel_e .GT. EdgeEnergy(escape_ratios%Z(j),L2_SHELL)) THEN
                        DO k=L2M1_LINE,L2Q1_LINE,-1
                                line_e=LineEnergy(escape_ratios%Z(j),k)
                                IF ((channel_e-line_e) .GE. channel_1e .AND. channel_e .GE.&
                                escape_ratios%fluo_escape_input_energies(1) .AND.&
                                channel_e .LT.&
                                escape_ratios%fluo_escape_input_energies(&
                                escape_ratios%n_fluo_input_energies))&
                                THEN
                                !interpolate
                                pos=findpos(escape_ratios%fluo_escape_input_energies,&
                                channel_e)   
                                IF (pos .LT. 1) THEN
                                WRITE (6,*) 'findpos returns low index'
                                CALL EXIT(1)
                                ENDIF
                                !fluo_energies =>&
                                !escape_ratios%fluo_escape_input_energies(pos:pos+1)
                                fluo_a(1)=escape_ratios%&
                                fluo_escape_input_energies(pos)
                                fluo_b(1)=escape_ratios%&
                                fluo_escape_input_energies(pos+1)
                                !fluo_ratios =>&
                                !escape_ratios%fluo_escape_ratios(j,ABS(k),pos:pos+1)
                                fluo_a(2)=escape_ratios%&
                                fluo_escape_ratios(j,ABS(k),pos)
                                fluo_b(2)=escape_ratios%&
                                fluo_escape_ratios(j,ABS(k),pos+1)
                                ratio=interpolate_simple(fluo_a,fluo_b,channel_e)
                                sum_ratio = sum_ratio+ratio
                                escape_i=INT((channel_e-line_e-inputF%detector%zero)/&
                                inputF%detector%gain)
                                IF (escape_i .GE. 1 .AND. escape_i .LE.&
                                SIZE(channels_conv)) THEN
                                channels_conv(escape_i)=&
                                channels_conv(escape_i)+ratio*channels_conv(i)
                                ENDIF
                                ENDIF
                                
                        ENDDO
                        ENDIF
                        IF  (channel_e .GT. EdgeEnergy(escape_ratios%Z(j),L3_SHELL)) THEN
                        DO k=L3M1_LINE,L3Q1_LINE,-1
                                line_e=LineEnergy(escape_ratios%Z(j),k)
                                IF ((channel_e-line_e) .GE. channel_1e .AND. channel_e .GE.&
                                escape_ratios%fluo_escape_input_energies(1) .AND.&
                                channel_e .LT.&
                                escape_ratios%fluo_escape_input_energies(&
                                escape_ratios%n_fluo_input_energies))&
                                THEN
                                !interpolate
                                pos=findpos(escape_ratios%fluo_escape_input_energies,&
                                channel_e)   
                                IF (pos .LT. 1) THEN
                                WRITE (6,*) 'findpos returns low index'
                                CALL EXIT(1)
                                ENDIF
                                !fluo_energies =>&
                                !escape_ratios%fluo_escape_input_energies(pos:pos+1)
                                fluo_a(1)=escape_ratios%&
                                fluo_escape_input_energies(pos)
                                fluo_b(1)=escape_ratios%&
                                fluo_escape_input_energies(pos+1)
                                !fluo_ratios =>&
                                !escape_ratios%fluo_escape_ratios(j,ABS(k),pos:pos+1)
                                fluo_a(2)=escape_ratios%&
                                fluo_escape_ratios(j,ABS(k),pos)
                                fluo_b(2)=escape_ratios%&
                                fluo_escape_ratios(j,ABS(k),pos+1)
                                ratio=interpolate_simple(fluo_a,fluo_b,channel_e)
                                sum_ratio = sum_ratio+ratio
                                escape_i=INT((channel_e-line_e-inputF%detector%zero)/&
                                inputF%detector%gain)
                                IF (escape_i .GE. 1 .AND. escape_i .LE.&
                                SIZE(channels_conv)) THEN
                                channels_conv(escape_i)=&
                                channels_conv(escape_i)+ratio*channels_conv(i)
                                ENDIF
                                ENDIF
                                
                        ENDDO
                        ENDIF

                ENDDO
                !compton escape next
                !applied to every channel lower than the current one
                !take into account ratio of gain to output_energies_width
                DO j=1,i-1
                        channel_c = (REAL(j)+0.5_C_DOUBLE)*inputF%detector%gain+&
                        inputF%detector%zero
                        compton_diff=channel_e-channel_c
                        IF (channel_e .GE.&
                        escape_ratios%compton_escape_input_energies(1) .AND.&
                        channel_e .LT.&
                        escape_ratios%compton_escape_input_energies(&
                        escape_ratios%n_compton_input_energies) .AND.&
                        compton_diff .GE.&
                        escape_ratios%compton_escape_output_energies(1) .AND.&
                        compton_diff .LT.&
                        escape_ratios%compton_escape_output_energies(&
                        escape_ratios%n_compton_output_energies)) THEN
                        !make sure the bilinear interpolation is safe
                        pos_1=0_C_INT
                        pos_2=0_C_INT
                        ratio=bilinear_interpolation(escape_ratios%compton_escape_ratios,&
                        escape_ratios%compton_escape_input_energies,&
                        escape_ratios%compton_escape_output_energies,&
                        channel_e,compton_diff,pos_1,pos_2)*inputF%detector%gain/&
                        compton_out_diff
                        channels_conv(j)=&
                        channels_conv(j)+ratio*channels_conv(i)
                        sum_ratio = sum_ratio+ratio


                        ENDIF
                ENDDO
                channels_conv(i)=channels_conv(i)*(1.0_C_DOUBLE-sum_ratio)
        ENDDO

ENDSUBROUTINE xmi_detector_escape

SUBROUTINE xmi_detector_escape_SiLi(channels_conv, inputF)
        IMPLICIT NONE
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

                IF (i_esc_ka .GE. 1 .AND. i_esc_ka .LE. SIZE(channels_conv))&
                THEN
                        channels_conv(i_esc_ka) = &
                        channels_conv(i_esc_ka)+esc_rat*channels_conv(i)*RR_Si_Ka
                ENDIF
                IF (i_esc_kb .GE. 1 .AND. i_esc_kb .LE. SIZE(channels_conv))&
                THEN
                        channels_conv(i_esc_kb) = &
                        channels_conv(i_esc_kb)+esc_rat*channels_conv(i)*RR_Si_Kb
                ENDIF
                channels_conv(i) = (1.0_C_DOUBLE-esc_rat)*channels_conv(i)

        ENDDO


        RETURN
ENDSUBROUTINE xmi_detector_escape_SiLi



ENDMODULE xmimsim_detector
