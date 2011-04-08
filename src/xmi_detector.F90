MODULE xmimsim_detector


USE, INTRINSIC :: ISO_C_BINDING
USE :: xmimsim_aux
USE :: omp_lib
USE :: fgsl


CONTAINS

SUBROUTINE xmi_detector_sum_peaks(inputF, channels)
        IMPLICIT NONE
        TYPE (xmi_input), POINTER, INTENT(IN) :: inputF
        REAL (C_DOUBLE), DIMENSION(*), INTENT(INOUT) :: channels
        
        REAL (C_DOUBLE) :: Nt
        INTEGER (C_LONG) :: Nt_long
        REAL (C_DOUBLE), DIMENSION(:) :: H_cdf, h_pdf
        INTEGER (C_INT) :: nchannels
        INTEGER (C_LONG) :: i,j

        nchannels = SIZE(channels)
        Nt = SUM(channels)
        Nt_long = INT(Nt, KIND=C_LONG)

        ALLOCATE(h_pdf(nchannels), H_cdf(nchannels))

        h_pdf = channels/Nt
        

        H_cdf(1) = h_pdf(1)
        DO i=2, nchannels
              H_cdf(i) = H_cdf(i-1)+h_pdf(i)  
        ENDDO





ENDSUBROUTINE xmi_detector_sum_peaks

SUBROUTINE xmi_detector_convolute(inputFPtr, hdf5FPtr, channels_noconvPtr,&
channels_convPtr,nchannels) BIND(C,NAME='xmi_detector_convolute')
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN), VALUE :: inputFPtr, hdf5FPtr, channels_noconvPtr
        INTEGER (C_INT), VALUE, INTENT(IN) :: nchannels
        TYPE (C_PTR), INTENT(INOUT) :: channels_convPtr

        TYPE (xmi_hdf5), POINTER :: hdf5F
        TYPE (xmi_input), POINTER :: inputF
        REAL (C_DOUBLE), POINTER, DIMENSION(:) :: channels_noconv,&
        channels_conv, channels_temp
        INTEGER (C_LONG) :: nlim
        REAL (C_DOUBLE) :: a,b
        REAL (C_DOUBLE), PARAMETER :: c =&
        SQRT(2.0_C_DOUBLE)/(2.0_C_DOUBLE*SQRT(2.0_C_DOUBLE*LOG(2.0_C_DOUBLE)))
        REAL (C_DOUBLE), DIMENSION(4096) :: R 
        INTEGER (C_INT) :: I0, I
        REAL (C_DOUBLE) :: E0, E, B0, FWHM, A0, A3, A4, ALFA, X, G, F, my_sum,&
        CBG




        CALL C_F_POINTER(inputFPtr, inputF)
        CALL C_F_POINTER(hdf5FPtr, hdf5F) 
        CALL C_F_POINTER(channels_noconvPtr, channels_noconv,[nchannels])

#if DEBUG == 1
        WRITE (*,'(A,F15.4)') 'channel 223 contents: ', channels_noconv(223)
#endif

        !allocate memory for results
        ALLOCATE(channels_conv(nchannels), channels_temp(nchannels))

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


        !escape peak
        IF (inputF%detector%detector_type .EQ. XMI_DETECTOR_SILI .OR.&
        inputF%detector%detector_type .EQ. XMI_DETECTOR_SI_SDD) THEN
                CALL xmi_detector_escape_SiLi(channels_temp, inputF)
        ELSE
                WRITE (*,'(A)') 'Unsupported detector type'
                CALL EXIT(1)
        ENDIF

#if DEBUG == 1
        WRITE (*,'(A,ES14.6)') 'channels_temp max after escape: ',MAXVAL(channels_temp)
        WRITE (*,'(A,F14.6)') 'a: ',a
        WRITE (*,'(A,F14.6)') 'b: ',b
        WRITE (*,'(A,I)') 'nlim: ',nlim
#endif

        R = 0.0_C_DOUBLE

!!!$omp parallel do default(private) shared(a,b,inputF,I0,nlim,nchannels)
        DO I0=1,nlim
                E0 = inputF%detector%zero + inputF%detector%gain*I0
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


                !IF (I0 .LE. 1800) THEN
                        A3=2.73E-3*EXP(-0.21*E0)+1.E-4_C_DOUBLE
                        A4=0.000188*EXP(-0.00296*(E0**0.763))+&
                          1.355E-5*EXP(0.968*(E0**0.498))
                        ALFA=1.179*EXP(8.6E-4*(E0**1.877))-&
                          7.793*EXP(-3.81*(E0**(-0.0716)))
                !ENDIF
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

SUBROUTINE xmi_detector_escape_SiLi(channels_conv, inputF)
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
