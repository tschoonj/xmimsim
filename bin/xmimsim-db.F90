

PROGRAM xmimsim_db

USE :: xraylib
USE :: hdf5
USE :: OMP_LIB
USE,INTRINSIC :: ISO_C_BINDING
USE,INTRINSIC :: ISO_FORTRAN_ENV

IMPLICIT NONE

INTEGER, PARAMETER :: nintervals_r = 2000, nintervals_e = 200, maxz = 4, &
nintervals_theta=100000, nintervals_theta2=200,nintervals_phi=100000
REAL (KIND=C_DOUBLE), PARAMETER :: maxe = 100.0, lowe = 0.1, &
        PI = 3.14159265359,MEC2 = 510.998910
CHARACTER(200) :: error_message

REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:,:) :: &
        rayleigh_theta,compton_theta
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: energies, rs, trapez, thetas,sumz,phis

INTEGER :: stat,i,j,k,l,m,n
REAL (KIND=C_DOUBLE) :: temp_sum,K0K

CHARACTER(len=30) :: filename = 'xmimsimdata.h5'

INTEGER(HID_T) :: file_id       ! File identifier 
INTEGER(HID_T) :: dset_id       ! Dataset identifier 
INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
INTEGER(HID_T) :: group_id     
INTEGER(HSIZE_T),DIMENSION(2) :: dims = [nintervals_e, nintervals_r]
INTEGER(HSIZE_T),DIMENSION(2) :: dims2 = [nintervals_theta2, nintervals_r]
INTEGER(HSIZE_T),DIMENSION(3) :: dims3 = [nintervals_theta2, nintervals_e,nintervals_r]

INTEGER :: h5error
CHARACTER(len=2) :: element

REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: &
        rayleigh_phi
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:,:) ::  compton_phi
REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: cdfs 




CALL h5open_f(h5error)


CALL SetHardExit(1)

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

!$OMP PARALLEL DEFAULT(shared) PRIVATE(j,k,l,m,trapez,temp_sum,sumz)

#if DEBUG == 2
WRITE(6,*) 'multiple allocs'
#endif
ALLOCATE(trapez(nintervals_theta-1))
ALLOCATE(sumz(nintervals_theta-1))
!$OMP DO

Zloop:DO i=1,maxz 
#if DEBUG == 1
WRITE (6,*) 'Element: ',i
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
        !group creation
        CALL h5gcreate_f(file_id,element,group_id,h5error)

        !create rayleigh theta dataset, including the energies and random
        !numbers 
        CALL h5screate_simple_f(2,dims,dspace_id,h5error)
        CALL h5dcreate_f(group_id,'RayleighTheta_ICDF',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,rayleigh_theta(i,:,:),dims,h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)
        CALL h5screate_simple_f(1,[dims(1)],dspace_id,h5error)
        CALL h5dcreate_f(group_id,'Energies',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,energies,[dims(1)],h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)
        CALL h5screate_simple_f(1,[dims(2)],dspace_id,h5error)
        CALL h5dcreate_f(group_id,'Random_numbers',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,rs,[dims(2)],h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)


        !create compton theta dataset
        CALL h5screate_simple_f(2,dims,dspace_id,h5error)
        CALL h5dcreate_f(group_id,'ComptonTheta_ICDF',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
        CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,compton_theta(i,:,:),dims,h5error)
        CALL h5sclose_f(dspace_id,h5error)
        CALL h5dclose_f(dset_id,h5error)

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
h5dcreate_f(group_id,'Random_numbers',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
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
h5dcreate_f(group_id,'Random_numbers',H5T_NATIVE_DOUBLE,dspace_id,dset_id,h5error)
CALL h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,rs,[dims3(3)],h5error)
CALL h5sclose_f(dspace_id,h5error)
CALL h5dclose_f(dset_id,h5error)
CALL h5gclose_f(group_id,h5error)






CALL h5close_f(h5error)


ENDPROGRAM
