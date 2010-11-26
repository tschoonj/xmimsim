MODULE xmimsim_aux

USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

!
!
!  Rayleigh, Compton interaction probabilities
!
!
TYPE :: interaction_prob
        REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: energies
        REAL (KIND=C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: Rayl_and_Compt
ENDTYPE

!
!
! HDF5 data structures
!
!
TYPE :: xmi_hdf5_Z
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: RayleighTheta_ICDF
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:,:) :: ComptonTheta_ICDF
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: Energies
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: RandomNumbers
        !interaction_probs ...
        TYPE (interaction_prob) :: interaction_probs
        INTEGER (C_INT) :: Z
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

TYPE :: xmi_hdf5_ZPtr
        !this type is necessary because Fortran does not support arrays of
        !pointers
        TYPE (xmi_hdf5_Z), POINTER :: Ptr
ENDTYPE

!
!
!  xmi data structs
!
!

!  xmi_general

TYPE, BIND(C) :: xmi_generalC
        REAL (C_FLOAT) :: version
        TYPE (C_PTR) :: outputfile
        INTEGER (C_LONG) :: n_photons_interval
        INTEGER (C_LONG) :: n_photons_line
        INTEGER (C_INT) :: n_interactions_trajectory
ENDTYPE

TYPE :: xmi_general
        REAL (C_FLOAT) :: version
        CHARACTER (KIND=C_CHAR,LEN=:),ALLOCATABLE :: outputfile
        INTEGER (C_LONG) :: n_photons_interval
        INTEGER (C_LONG) :: n_photons_line
        INTEGER (C_INT) :: n_interactions_trajectory
ENDTYPE

!  xmi_layer

TYPE, BIND(C) :: xmi_layerC
        INTEGER (C_INT) :: n_elements
        TYPE (C_PTR) :: Z
        TYPE (C_PTR) :: weight
        REAL (C_DOUBLE) :: density
        REAL (C_DOUBLE) :: thickness
ENDTYPE

TYPE :: xmi_layer
        INTEGER (C_INT) :: n_elements
        INTEGER (C_INT), ALLOCATABLE,DIMENSION(:) :: Z
        REAL (C_DOUBLE), ALLOCATABLE,DIMENSION(:) :: weight
        REAL (C_DOUBLE) :: density
        REAL (C_DOUBLE) :: thickness
        TYPE (xmi_hdf5_ZPtr), ALLOCATABLE, DIMENSION(:) :: xmi_hdf5_Z_local
ENDTYPE

!  xmi_composition

TYPE, BIND(C) :: xmi_compositionC
        INTEGER (C_INT) :: n_layers
        TYPE (C_PTR) :: layers
ENDTYPE

TYPE :: xmi_composition
        INTEGER (C_INT) :: n_layers
        TYPE (xmi_layer), ALLOCATABLE, DIMENSION(:) :: layers
ENDTYPE

!  xmi_geometry

TYPE, BIND(C) :: xmi_geometry
        REAL (C_DOUBLE) :: d_sample_source;
        REAL (C_DOUBLE), DIMENSION(3) :: n_sample_orientation;
        REAL (C_DOUBLE), DIMENSION(3) :: p_detector_window;
        REAL (C_DOUBLE), DIMENSION(3) :: n_detector_orientation;
        REAL (C_DOUBLE) :: area_detector;
        REAL (C_DOUBLE) :: acceptance_detector;
        REAL (C_DOUBLE) :: d_source_slit;
        REAL (C_DOUBLE) :: slit_size_x;
        REAL (C_DOUBLE) :: slit_size_y;
ENDTYPE

!  xmi_energy

TYPE, BIND(C) :: xmi_energy
        REAL (C_DOUBLE) :: energy
        REAL (C_DOUBLE) :: horizontal_intensity
        REAL (C_DOUBLE) :: vertical_intensity
        REAL (C_DOUBLE) :: sigma_x;
        REAL (C_DOUBLE) :: sigma_xp;
        REAL (C_DOUBLE) :: sigma_y;
        REAL (C_DOUBLE) :: sigma_yp;
ENDTYPE

!  xmi_excitation

TYPE, BIND(C) :: xmi_excitationC
        INTEGER (C_INT) :: n_discrete
        TYPE (C_PTR) :: discrete
        INTEGER (C_INT) :: n_continuous
        TYPE (C_PTR) :: continuous
ENDTYPE

TYPE :: xmi_excitation
        INTEGER (C_INT) :: n_discrete
        TYPE (xmi_energy), ALLOCATABLE, DIMENSION(:) :: discrete
        INTEGER (C_INT) :: n_continuous
        TYPE (xmi_energy), ALLOCATABLE, DIMENSION(:) :: continuous
ENDTYPE

!   xmi_absorbers

TYPE, BIND(C) :: xmi_absorbersC
        INTEGER (C_INT) :: n_exc_layers
        TYPE (C_PTR) :: exc_layers
        INTEGER (C_INT) :: n_det_layers
        TYPE (C_PTR) :: det_layers
ENDTYPE

TYPE :: xmi_absorbers
        INTEGER (C_INT) :: n_exc_layers
        TYPE (xmi_layer), ALLOCATABLE, DIMENSION(:) :: exc_layers
        INTEGER (C_INT) :: n_det_layers
        TYPE (xmi_layer), ALLOCATABLE, DIMENSION(:) :: det_layers
ENDTYPE

#define XMI_DETECTOR_SILI 0
#define XMI_DETECTOR_GE 1
#define XMI_DETECTOR_SI_SDD 2

!  xmi_detector

TYPE, BIND(C) :: xmi_detectorC
        INTEGER (C_INT) :: detector_type
        REAL (C_DOUBLE) :: gain
        REAL (C_DOUBLE) :: zero 
        REAL (C_DOUBLE) :: fano 
        REAL (C_DOUBLE) :: noise 
        REAL (C_DOUBLE) :: max_convolution_energy
        INTEGER (C_INT) :: n_crystal_layers
        TYPE (C_PTR) :: crystal_layers
ENDTYPE

TYPE :: xmi_detector
        INTEGER (C_INT) :: detector_type
        REAL (C_DOUBLE) :: gain
        REAL (C_DOUBLE) :: zero 
        REAL (C_DOUBLE) :: fano 
        REAL (C_DOUBLE) :: noise 
        REAL (C_DOUBLE) :: max_convolution_energy
        INTEGER (C_INT) :: n_crystal_layers
        TYPE (xmi_layer), ALLOCATABLE, DIMENSION(:) :: crystal_layers
ENDTYPE

!  xmi_input

TYPE, BIND(C) :: xmi_inputC
        TYPE (C_PTR) :: general
        TYPE (C_PTR) :: composition 
        TYPE (C_PTR) :: geometry 
        TYPE (C_PTR) :: excitation 
        TYPE (C_PTR) :: absorbers 
        TYPE (C_PTR) :: detector 
ENDTYPE

TYPE :: xmi_input
       TYPE (xmi_general) :: general
       TYPE (xmi_composition) :: composition
       TYPE (xmi_geometry) :: geometry
       TYPE (xmi_excitation) :: excitation
       TYPE (xmi_absorbers) :: absorbers
       TYPE (xmi_detector) :: detector
ENDTYPE

!
!
!   xmi_photon: will hold all the properties of a photon during the simulations
!
!
TYPE :: xmi_photon
        !coordinates of the photon
        REAL (C_DOUBLE), DIMENSION(3) :: coords
        !direction of the photon
        REAL (C_DOUBLE), DIMENSION(3) :: dirv
        !electric field vector of the photon
        REAL (C_DOUBLE), DIMENSION(3) :: elecv
        !energy of the photon
        REAL (C_DOUBLE) :: energy
        !number of interactions the photon has experienced
        INTEGER (C_INT) :: n_interactions
        !offspring of the photon (the standard fluorescence photon is not
        !considered as offspring!!!)
        TYPE (xmi_photon),POINTER :: offspring
ENDTYPE




INTERFACE
!interface for the libc qsort function
SUBROUTINE qsort(base,nmemb,size,compar) BIND(C,NAME='qsort')
        USE,INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        TYPE (C_PTR),VALUE :: base
        INTEGER (C_SIZE_T),VALUE :: nmemb, size
        TYPE (C_FUNPTR),VALUE :: compar
ENDSUBROUTINE
!interface for the libc strlen function
FUNCTION strlen(s) BIND(C,NAME='strlen')
        USE,INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        !TYPE (C_PTR), VALUE :: s
        CHARACTER (KIND=C_CHAR,LEN=*) :: s
        INTEGER (C_SIZE_T) :: strlen
ENDFUNCTION strlen

!interface for the xmi_get_random_numbers function
FUNCTION xmi_get_random_numbers(numbers, n) BIND(C,NAME='xmi_get_random_numbers')
        USE, INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        TYPE (C_PTR), VALUE :: numbers !points to a C array of unsigned long
        !int's
        INTEGER (C_LONG), VALUE :: n
        INTEGER (C_INT) :: xmi_get_random_numbers
ENDFUNCTION xmi_get_random_numbers

ENDINTERFACE

INTERFACE ASSIGNMENT(=)
        MODULE PROCEDURE assign_interaction_prob
ENDINTERFACE


CONTAINS

!used to compare C_INT's with qsort
FUNCTION C_INT_CMP (a,b) RESULT(rv) BIND(C)
        IMPLICIT NONE
        TYPE(C_PTR),VALUE,INTENT(IN) :: a,b
        INTEGER (C_INT) :: rv
        INTEGER (C_INT), POINTER :: aF,bF

        CALL C_F_POINTER(a,aF)
        CALL C_F_POINTER(b,bF)

        rv = aF-bF

        RETURN

ENDFUNCTION C_INT_CMP

!used to compare C_DOUBLE's with qsort
FUNCTION C_DOUBLE_CMP (a,b) RESULT(rv) BIND(C)
        IMPLICIT NONE
        TYPE(C_PTR),VALUE,INTENT(IN) :: a,b
        INTEGER (C_INT) :: rv
        REAL (C_DOUBLE), POINTER :: aF,bF
        REAL (C_DOUBLE) :: diff

        CALL C_F_POINTER(a,aF)
        CALL C_F_POINTER(b,bF)

        diff = aF-bF

        IF (diff > 0.0_C_DOUBLE) THEN
                rv = 1_C_INT
        ELSEIF (diff < 0.0_C_DOUBLE) THEN
                rv = -1_C_INT
        ELSE
                rv = 0_C_INT
        ENDIF

        RETURN
ENDFUNCTION C_DOUBLE_CMP

!used to compare C_FLOAT's with qsort
FUNCTION C_FLOAT_CMP (a,b) RESULT(rv) BIND(C)
        IMPLICIT NONE
        TYPE(C_PTR),VALUE,INTENT(IN) :: a,b
        INTEGER (C_INT) :: rv
        REAL (C_FLOAT), POINTER :: aF,bF
        REAL (C_FLOAT) :: diff

        CALL C_F_POINTER(a,aF)
        CALL C_F_POINTER(b,bF)

        diff = aF-bF

        IF (diff > 0.0_C_FLOAT) THEN
                rv = 1_C_INT
        ELSEIF (diff < 0.0_C_FLOAT) THEN
                rv = -1_C_INT
        ELSE
                rv = 0_C_INT
        ENDIF

        RETURN

ENDFUNCTION C_FLOAT_CMP

SUBROUTINE assign_interaction_prob(outvar,invar)
        IMPLICIT NONE
        TYPE(interaction_prob), INTENT(INOUT) :: outvar
        REAL (KIND=C_FLOAT),DIMENSION(:),INTENT(IN) :: invar
        INTEGER :: i

#if DEBUG == 1
        WRITE (6,*) 'Entering assign_interaction_prob'
#endif

        !invar = energies2
        ALLOCATE(outvar%energies(SIZE(invar)),outvar%Rayl_and_Compt(SIZE(invar),2))
        DO i=1,SIZE(invar)
                outvar%energies(i)=REAL(invar(i),KIND=C_DOUBLE)
        ENDDO

        RETURN
ENDSUBROUTINE assign_interaction_prob

!
! xmi_input_C2F creates a complete copy of the C structure!!!
!

SUBROUTINE xmi_input_C2F(xmi_inputC_in,xmi_inputFPtr) BIND(C,NAME='xmi_input_C2F')
        IMPLICIT NONE
        !pass by reference, no VALUE
        TYPE (xmi_inputC), INTENT(IN) :: xmi_inputC_in
        TYPE (C_PTR), INTENT(OUT) :: xmi_inputFPtr
        TYPE (xmi_input), POINTER :: xmi_inputF

        TYPE (xmi_generalC),POINTER :: xmi_general_temp
        TYPE (xmi_compositionC),POINTER :: xmi_composition_temp
        TYPE (xmi_geometry),POINTER :: xmi_geometry_temp
        TYPE (xmi_excitationC),POINTER :: xmi_excitation_temp
        TYPE (xmi_absorbersC),POINTER :: xmi_absorbers_temp
        TYPE (xmi_detectorC),POINTER :: xmi_detector_temp
        TYPE (xmi_layerC),POINTER, DIMENSION(:) :: xmi_layer_temp
        TYPE (xmi_energy),POINTER, DIMENSION(:) :: xmi_energy_temp
        INTEGER (C_INT), POINTER,DIMENSION(:) :: Z_temp
        REAL (C_DOUBLE), POINTER,DIMENSION(:) :: weight_temp

        INTEGER :: i


        !!
        !! allocate xmi_inputF
        !!

        ALLOCATE(xmi_inputF)
#if DEBUG == 1
        WRITE (6,*) 'Entered xmi_input_C2F'
#endif


        !!
        !! associate xmi_general 
        !!
        CALL C_F_POINTER (xmi_inputC_in%general, xmi_general_temp)
        xmi_inputF%general%version = xmi_general_temp%version
        xmi_inputF%general%n_photons_interval = xmi_general_temp%n_photons_interval
        xmi_inputF%general%n_photons_line = xmi_general_temp%n_photons_line
        xmi_inputF%general%n_interactions_trajectory = &
                xmi_general_temp%n_interactions_trajectory
        !todo is outputfile... not so important actually  
        
#if DEBUG == 1
        WRITE (6,*) 'xmi_general ok'
#endif

        !!
        !! associate xmi_composition 
        !!
        CALL C_F_POINTER (xmi_inputC_in%composition , xmi_composition_temp)
        xmi_inputF%composition%n_layers = xmi_composition_temp%n_layers
        ALLOCATE(xmi_inputF%composition%layers(xmi_inputF%composition%n_layers))
        CALL C_F_POINTER (xmi_composition_temp%layers,xmi_layer_temp,[xmi_inputF%composition%n_layers])

        DO i=1,xmi_inputF%composition%n_layers
                xmi_inputF%composition%layers(i)%n_elements = &
                xmi_layer_temp(i)%n_elements
                xmi_inputF%composition%layers(i)%density = &
                xmi_layer_temp(i)%density
                xmi_inputF%composition%layers(i)%thickness = &
                xmi_layer_temp(i)%thickness
                CALL C_F_POINTER &
                (xmi_layer_temp(i)%Z,Z_temp,[xmi_layer_temp(i)%n_elements]  )
                xmi_inputF%composition%layers(i)%Z = &
                Z_temp
                CALL C_F_POINTER &
                (xmi_layer_temp(i)%weight,weight_temp,[xmi_layer_temp(i)%n_elements]  )
                xmi_inputF%composition%layers(i)%weight = &
                weight_temp
        ENDDO

#if DEBUG == 1
        WRITE (6,*) 'xmi_composition ok'
#endif
        !!
        !! associate xmi_geometry
        !!
        !should be easy...
        CALL C_F_POINTER (xmi_inputC_in%geometry, xmi_geometry_temp)
        xmi_inputF%geometry = xmi_geometry_temp

#if DEBUG == 1
        WRITE (6,*) 'xmi_geometry ok'
#endif

        !!
        !! associate xmi_excitation
        !!
        CALL C_F_POINTER (xmi_inputC_in%excitation, xmi_excitation_temp)

        xmi_inputF%excitation%n_discrete = xmi_excitation_temp%n_discrete
        xmi_inputF%excitation%n_continuous = xmi_excitation_temp%n_continuous

        IF (xmi_inputF%excitation%n_discrete .GT. 0) THEN
                CALL C_F_POINTER (xmi_excitation_temp%discrete,&
                xmi_energy_temp,&
                [xmi_inputF%excitation%n_discrete]) 
                xmi_inputF%excitation%discrete = xmi_energy_temp
        ENDIF

        IF (xmi_inputF%excitation%n_continuous .GT. 0) THEN
                CALL C_F_POINTER (xmi_excitation_temp%continuous,&
                xmi_energy_temp,&
                [xmi_inputF%excitation%n_continuous]) 
                xmi_inputF%excitation%continuous = xmi_energy_temp
        ENDIF

#if DEBUG == 1
        WRITE (6,*) 'xmi_excitation ok'
#endif
        !!
        !! associate xmi_absorbers
        !!
        CALL C_F_POINTER (xmi_inputC_in%absorbers, xmi_absorbers_temp)

        xmi_inputF%absorbers%n_exc_layers = xmi_absorbers_temp%n_exc_layers
        xmi_inputF%absorbers%n_det_layers = xmi_absorbers_temp%n_det_layers

#if DEBUG == 1
        WRITE (6,*) 'n_exc_layers: ',xmi_inputF%absorbers%n_exc_layers
        WRITE (6,*) 'n_det_layers: ',xmi_inputF%absorbers%n_det_layers
#endif
        IF (xmi_inputF%absorbers%n_exc_layers .GT. 0) THEN
                ALLOCATE(xmi_inputF%absorbers%exc_layers(xmi_inputF%absorbers%n_exc_layers))
                CALL C_F_POINTER (xmi_absorbers_temp%exc_layers,&
                xmi_layer_temp,&
                [xmi_inputF%absorbers%n_exc_layers])
                DO i=1,xmi_inputF%absorbers%n_exc_layers
                        xmi_inputF%absorbers%exc_layers(i)%n_elements = &
                        xmi_layer_temp(i)%n_elements
                        xmi_inputF%absorbers%exc_layers(i)%density = &
                        xmi_layer_temp(i)%density
                        xmi_inputF%absorbers%exc_layers(i)%thickness = &
                        xmi_layer_temp(i)%thickness
                        CALL C_F_POINTER &
                        (xmi_layer_temp(i)%Z,Z_temp,[xmi_layer_temp(i)%n_elements]  )
                        xmi_inputF%absorbers%exc_layers(i)%Z = &
                        Z_temp
                        CALL C_F_POINTER &
                        (xmi_layer_temp(i)%weight,weight_temp,[xmi_layer_temp(i)%n_elements]  )
                        xmi_inputF%absorbers%exc_layers(i)%weight = &
                        weight_temp
                ENDDO
        ENDIF

        IF (xmi_inputF%absorbers%n_det_layers .GT. 0) THEN
                ALLOCATE(xmi_inputF%absorbers%det_layers(xmi_inputF%absorbers%n_det_layers))
                CALL C_F_POINTER (xmi_absorbers_temp%det_layers,&
                xmi_layer_temp,&
                [xmi_inputF%absorbers%n_det_layers])
                DO i=1,xmi_inputF%absorbers%n_det_layers
                        xmi_inputF%absorbers%det_layers(i)%n_elements = &
                        xmi_layer_temp(i)%n_elements
                        xmi_inputF%absorbers%det_layers(i)%density = &
                        xmi_layer_temp(i)%density
                        xmi_inputF%absorbers%det_layers(i)%thickness = &
                        xmi_layer_temp(i)%thickness
                        CALL C_F_POINTER &
                        (xmi_layer_temp(i)%Z,Z_temp,[xmi_layer_temp(i)%n_elements]  )
                        xmi_inputF%absorbers%det_layers(i)%Z = &
                        Z_temp
                        CALL C_F_POINTER &
                        (xmi_layer_temp(i)%weight,weight_temp,[xmi_layer_temp(i)%n_elements]  )
                        xmi_inputF%absorbers%det_layers(i)%weight = &
                        weight_temp
                ENDDO
        ENDIF

#if DEBUG == 1
        WRITE (6,*) 'xmi_absorbers ok'
#endif
        !!
        !! associate xmi_detector
        !!
        CALL C_F_POINTER (xmi_inputC_in%detector,xmi_detector_temp)
        xmi_inputF%detector%detector_type = xmi_detector_temp%detector_type 
        xmi_inputF%detector%gain= xmi_detector_temp%gain
        xmi_inputF%detector%zero= xmi_detector_temp%zero
        xmi_inputF%detector%fano= xmi_detector_temp%fano
        xmi_inputF%detector%noise= xmi_detector_temp%noise
        xmi_inputF%detector%max_convolution_energy= xmi_detector_temp%max_convolution_energy
        xmi_inputF%detector%n_crystal_layers =&
        xmi_detector_temp%n_crystal_layers 

        IF (xmi_inputF%detector%n_crystal_layers .GT. 0) THEN
                ALLOCATE(xmi_inputF%detector%crystal_layers(xmi_inputF%detector%n_crystal_layers))
                CALL C_F_POINTER (xmi_detector_temp%crystal_layers,&
                xmi_layer_temp,&
                [xmi_inputF%detector%n_crystal_layers])
                DO i=1,xmi_inputF%detector%n_crystal_layers
                        xmi_inputF%detector%crystal_layers(i)%n_elements = &
                        xmi_layer_temp(i)%n_elements
                        xmi_inputF%detector%crystal_layers(i)%density = &
                        xmi_layer_temp(i)%density
                        xmi_inputF%detector%crystal_layers(i)%thickness = &
                        xmi_layer_temp(i)%thickness
                        CALL C_F_POINTER &
                        (xmi_layer_temp(i)%Z,Z_temp,[xmi_layer_temp(i)%n_elements]  )
                        xmi_inputF%detector%crystal_layers(i)%Z = &
                        Z_temp
                        CALL C_F_POINTER &
                        (xmi_layer_temp(i)%weight,weight_temp,[xmi_layer_temp(i)%n_elements]  )
                        xmi_inputF%detector%crystal_layers(i)%weight = &
                        weight_temp
                ENDDO
        ENDIF

#if DEBUG == 1
        WRITE (6,*) 'xmi_detector ok'
#endif
        !!return value
        xmi_inputFPtr = C_LOC(xmi_inputF)

#if DEBUG == 1
        WRITE (6,*) 'Exiting xmi_input_C2F'
#endif

        RETURN

ENDSUBROUTINE xmi_input_C2F

SUBROUTINE xmi_free_input_F(xmi_inputFPtr)  BIND(C,NAME='xmi_free_input_F')
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(INOUT) :: xmi_inputFPtr
        TYPE (xmi_input), POINTER :: xmi_inputF
        INTEGER :: i,j


        !associate C and Fortran pointers
        CALL C_F_POINTER(xmi_inputFPTR, xmi_inputF)

        !free general
        !nothing to do

        !free composition
        DO i=1,xmi_inputF%composition%n_layers
                DEALLOCATE(xmi_inputF%composition%layers(i)%Z)
                DEALLOCATE(xmi_inputF%composition%layers(i)%weight)
                DO j=1,xmi_inputF%composition%layers(i)%n_elements
                        NULLIFY(xmi_inputF%composition%layers(i)%xmi_hdf5_Z_local(j)%Ptr)
                ENDDO
                DEALLOCATE(xmi_inputF%composition%layers(i)%xmi_hdf5_Z_local)
        ENDDO

        !free geometry
        !nothing to do

        !free excitation
        IF (xmi_inputF%excitation%n_discrete .GT. 0) THEN
                DEALLOCATE(xmi_inputF%excitation%discrete)
        ENDIF

        IF (xmi_inputF%excitation%n_continuous .GT. 0) THEN
                DEALLOCATE(xmi_inputF%excitation%continuous)
        ENDIF


        !free absorbers
        IF (xmi_inputF%absorbers%n_exc_layers .GT. 0) THEN
                DO i=1,xmi_inputF%absorbers%n_exc_layers
                        DEALLOCATE(xmi_inputF%absorbers%exc_layers(i)%Z)
                        DEALLOCATE(xmi_inputF%absorbers%exc_layers(i)%weight)
                ENDDO
                DEALLOCATE(xmi_inputF%absorbers%exc_layers)
        ENDIF

        IF (xmi_inputF%absorbers%n_det_layers .GT. 0) THEN
                DO i=1,xmi_inputF%absorbers%n_det_layers
                        DEALLOCATE(xmi_inputF%absorbers%det_layers(i)%Z)
                        DEALLOCATE(xmi_inputF%absorbers%det_layers(i)%weight)
                ENDDO
                DEALLOCATE(xmi_inputF%absorbers%det_layers)
        ENDIF
        
        !free detector
        IF (xmi_inputF%detector%n_crystal_layers .GT. 0) THEN
                DO i=1,xmi_inputF%detector%n_crystal_layers
                        DEALLOCATE(xmi_inputF%detector%crystal_layers(i)%Z)
                        DEALLOCATE(xmi_inputF%detector%crystal_layers(i)%weight)
                ENDDO
                DEALLOCATE(xmi_inputF%detector%crystal_layers)
        ENDIF



        !free input
        DEALLOCATE(xmi_inputF)
        xmi_inputFPtr=C_NULL_PTR


ENDSUBROUTINE xmi_free_input_F




ENDMODULE
