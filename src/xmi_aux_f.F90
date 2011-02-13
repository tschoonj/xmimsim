MODULE xmimsim_aux

USE, INTRINSIC :: ISO_C_BINDING
USE :: xraylib
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
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: DopplerPz_ICDF
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: Energies
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: RandomNumbers
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:)   :: FluorYieldsCorr
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
        REAL (C_DOUBLE) :: thickness_along_Z
        REAL (C_DOUBLE) :: Z_coord_begin
        REAL (C_DOUBLE) :: Z_coord_end
ENDTYPE

!  xmi_composition

TYPE, BIND(C) :: xmi_compositionC
        INTEGER (C_INT) :: n_layers
        TYPE (C_PTR) :: layers
        INTEGER (C_INT) :: reference_layer 
ENDTYPE

TYPE :: xmi_composition
        INTEGER (C_INT) :: n_layers
        TYPE (xmi_layer), ALLOCATABLE, DIMENSION(:) :: layers
        INTEGER (C_INT) :: reference_layer 
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
        !below is not present in C variable!!!
        LOGICAL :: collimator_present
        REAL (C_DOUBLE) :: detector_radius
        REAL (C_DOUBLE) :: collimator_height
        REAL (C_DOUBLE), DIMENSION(3) :: n_detector_orientation_new_x
        REAL (C_DOUBLE), DIMENSION(3) :: n_detector_orientation_new_y
        REAL (C_DOUBLE), DIMENSION(3) :: n_detector_orientation_new_z
        REAL (C_DOUBLE), DIMENSION(3,3) :: n_detector_orientation_inverse
        REAL (C_DOUBLE), DIMENSION(3,3) :: n_detector_orientation_new
        REAL (C_DOUBLE) :: detector_solid_angle
        REAL (C_DOUBLE), DIMENSION(3) :: n_sample_orientation_det
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

TYPE, BIND(C) :: xmi_main_options 
        INTEGER (C_INT) :: use_M_lines
        INTEGER (C_INT) :: use_self_enhancement
        INTEGER (C_INT) :: use_cascade_auger
        INTEGER (C_INT) :: use_cascade_radiative
        INTEGER (C_INT) :: use_variance_reduction
ENDTYPE xmi_main_options

!
!
!       xmi_var_red_layer: Datatype that will hold information about the
!       variance reduction
!
!


TYPE :: xmi_var_red_layer
        !dimensions should be (n_elements, 383+1+1)
        REAL (C_DOUBLE), DIMENSION(:,:), ALLOCATABLE :: weight
        REAL (C_DOUBLE), DIMENSION(:,:), ALLOCATABLE :: energy 
ENDTYPE xmi_var_red_layer

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

        !polar angle
        REAL (C_DOUBLE) :: theta

        !azimuthal angle
        REAL (C_DOUBLE) :: phi 

        !energy changed from initial
        LOGICAL :: energy_changed

        !weight of the photon
        REAL (C_DOUBLE) :: weight

        INTEGER (C_LONG) :: weight_long

        !mus
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: mus

        !current layer
        INTEGER (C_INT) :: current_layer

        !current element
        INTEGER (C_INT) :: current_element

        !current element index
        INTEGER (C_INT) :: current_element_index

        !did it hit the detector?
        LOGICAL :: detector_hit

        !last interaction type
        INTEGER :: last_interaction

        !debug
        LOGICAL :: detector_hit2

        !options
        TYPE (xmi_main_options) :: options

        !interaction history
        INTEGER (C_INT), ALLOCATABLE, DIMENSION(:,:) :: history

        !last shell -> debugging
        INTEGER (C_INT) :: last_shell

        !variance reduction
        TYPE(xmi_var_red_layer), DIMENSION(:,:), ALLOCATABLE :: variance_reduction
        
        !cascade type
        INTEGER (C_INT) :: xmi_cascade_type

ENDTYPE xmi_photon

!
!
!       geometrical data types
!
!

TYPE :: xmi_line
        REAL (C_DOUBLE), DIMENSION(3) :: point
        REAL (C_DOUBLE), DIMENSION(3) :: dirv 
ENDTYPE

TYPE :: xmi_plane
        REAL (C_DOUBLE), DIMENSION(3) :: point
        REAL (C_DOUBLE), DIMENSION(3) :: normv 
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
        TYPE (C_PTR), VALUE :: s
        !CHARACTER (KIND=C_CHAR), DIMENSION(*) :: s
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

!interface for xmi_inverse_matrix function
SUBROUTINE xmi_inverse_matrix(x, y, z, inverse) BIND(C,NAME='xmi_inverse_matrix')
        USE, INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        TYPE (C_PTR), VALUE, INTENT(IN) :: x, y, z
        TYPE (C_PTR), INTENT(INOUT) :: inverse
ENDSUBROUTINE

!interface for xmi_determinant_matrix function
SUBROUTINE xmi_determinant_matrix(x, y, z) BIND(C,NAME='xmi_determinant_matrix')
        USE, INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        TYPE (C_PTR), VALUE, INTENT(IN) :: x, y, z
ENDSUBROUTINE


ENDINTERFACE

INTERFACE ASSIGNMENT(=)
        MODULE PROCEDURE assign_interaction_prob
ENDINTERFACE

INTERFACE xmi_mu_calc
        MODULE PROCEDURE xmi_mu_calc_xmi_layer_single_energy, &
        xmi_mu_calc_xmi_composition_single_energy
ENDINTERFACE

CHARACTER (LEN=2), DIMENSION(99) :: elements = &
       [' 1', ' 2', ' 3', ' 4', ' 5', ' 6', ' 7', ' 8', ' 9', '10',&
        '11', '12', '13', '14', '15', '16', '17', '18', '19', '20',&
        '21', '22', '23', '24', '25', '26', '27', '28', '29', '30',&
        '31', '32', '33', '34', '35', '36', '37', '38', '39', '40',&
        '41', '42', '43', '44', '45', '46', '47', '48', '49', '50',&
        '51', '52', '53', '54', '55', '56', '57', '58', '59', '60',&
        '61', '62', '63', '64', '65', '66', '67', '68', '69', '70',&
        '71', '72', '73', '74', '75', '76', '77', '78', '79', '80',&
        '81', '82', '83', '84', '85', '86', '87', '88', '89', '90',&
        '91', '92', '93', '94', '95', '96', '97', '98', '99']


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
        WRITE (*,'(A)') 'Entered xmi_input_C2F'
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
        WRITE (*,'(A)') 'xmi_general ok'
#endif

        !!
        !! associate xmi_composition 
        !!
        CALL C_F_POINTER (xmi_inputC_in%composition , xmi_composition_temp)
        xmi_inputF%composition%n_layers = xmi_composition_temp%n_layers
        xmi_inputF%composition%reference_layer = &
        xmi_composition_temp%reference_layer
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
        WRITE (*,'(A)') 'xmi_composition ok'
#endif
        !!
        !! associate xmi_geometry
        !!
        !should be easy...
        CALL C_F_POINTER (xmi_inputC_in%geometry, xmi_geometry_temp)
        xmi_inputF%geometry = xmi_geometry_temp

#if DEBUG == 1
        WRITE (*,'(A)') 'xmi_geometry ok'
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
        WRITE (*,'(A)') 'xmi_excitation ok'
#endif
        !!
        !! associate xmi_absorbers
        !!
        CALL C_F_POINTER (xmi_inputC_in%absorbers, xmi_absorbers_temp)

        xmi_inputF%absorbers%n_exc_layers = xmi_absorbers_temp%n_exc_layers
        xmi_inputF%absorbers%n_det_layers = xmi_absorbers_temp%n_det_layers

#if DEBUG == 1
        WRITE (*,'(A,I)') 'n_exc_layers: ',xmi_inputF%absorbers%n_exc_layers
        WRITE (*,'(A,I)') 'n_det_layers: ',xmi_inputF%absorbers%n_det_layers
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
        WRITE (*,'(A)') 'xmi_absorbers ok'
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
        WRITE (*,'(A)') 'xmi_detector ok'
#endif
        !!return value
        xmi_inputFPtr = C_LOC(xmi_inputF)

#if DEBUG == 1
        WRITE (*,'(A)') 'Exiting xmi_input_C2F'
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
#if DEBUG == 0
        WRITE (6,'(A,I2)') 'n_layers: ',xmi_inputF%composition%n_layers
#endif


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

!
!
! xmi_mu_calc functions: xmi_composition/xmi_layer   single/multiple energies
!
!

FUNCTION xmi_mu_calc_xmi_composition_single_energy(composition, energy) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_composition), INTENT(IN) :: composition
        REAL (C_DOUBLE), INTENT(IN) :: energy
        REAL (C_DOUBLE), DIMENSION(composition%n_layers) :: rv
        INTEGER (C_INT) :: i

        DO i=1,composition%n_layers
                rv(i) = &
                xmi_mu_calc_xmi_layer_single_energy(& 
                composition%layers(i),energy)
        ENDDO

        RETURN
ENDFUNCTION xmi_mu_calc_xmi_composition_single_energy

FUNCTION xmi_mu_calc_xmi_layer_single_energy(layer, energy) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_layer), INTENT(IN) :: layer 
        REAL (C_DOUBLE), INTENT(IN) :: energy
        REAL (C_DOUBLE) :: rv
        INTEGER (C_INT) :: i

        rv = 0.0_C_DOUBLE

        DO i=1,layer%n_elements
                rv = rv + REAL(CS_Total_Kissel(layer%Z(i),&
                REAL(energy,KIND=C_FLOAT))*layer%weight(i),KIND=C_DOUBLE)
        ENDDO
                 

        RETURN
ENDFUNCTION

FUNCTION xmi_intersection_plane_line(plane, line, point) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_plane), INTENT(IN) :: plane
        TYPE (xmi_line), INTENT(IN) :: line
        REAL (C_DOUBLE), INTENT(INOUT), DIMENSION(3) :: point
        INTEGER (C_INT) :: rv
        REAL (C_DOUBLE) :: ItimesN,d


        rv = 0

        ItimesN = DOT_PRODUCT(line%dirv,plane%normv)

        IF (ItimesN .EQ. 0.0_C_DOUBLE) THEN
                WRITE (*,'(A)') 'Parallel plane and line found'
                RETURN
        ENDIF

        d = DOT_PRODUCT(plane%point-line%point, plane%normv)/ItimesN

        point = d*line%dirv+line%point

        rv = 1

        RETURN
ENDFUNCTION xmi_intersection_plane_line

FUNCTION xmi_distance_two_points(point1, point2) RESULT(rv)
        IMPLICIT NONE
        REAL (C_DOUBLE), INTENT(IN), DIMENSION(3) :: point1, point2
        REAL (C_DOUBLE) :: rv
        
        rv = SQRT((point1(1)-point2(1))**2 + &
                (point1(2)-point2(2))**2 + &
                (point1(3)-point2(3))**2)

        RETURN 
ENDFUNCTION xmi_distance_two_points

SUBROUTINE xmi_move_photon_with_dist(photon, dist)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT)  :: photon
        REAL (C_DOUBLE), INTENT(IN) :: dist

        REAL (C_DOUBLE) :: theta, phi

        !photon%dirv are normalized...
        theta = ASIN(photon%dirv(3))
        phi = ATAN(photon%dirv(2)/photon%dirv(1))

        photon%coords(1) = photon%coords(1) + dist*COS(theta)*COS(phi)
        photon%coords(2) = photon%coords(2) + dist*COS(theta)*SIN(phi)
        photon%coords(3) = photon%coords(3) + dist*SIN(theta)

        RETURN
ENDSUBROUTINE xmi_move_photon_with_dist

FUNCTION cross_product(a,b) RESULT(rv)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(3), INTENT(IN) :: a,b
        REAL (C_DOUBLE), DIMENSION(3) :: rv

        rv(1) = a(2)*b(3)-a(3)*b(2)
        rv(2) = a(3)*b(1)-a(1)*b(3)
        rv(3) = a(1)*b(2)-a(2)*b(1)

#if DEBUG == 1
        WRITE (*,'(A)') 'cross product'
        WRITE (*,'(3F12.5)') a
        WRITE (*,'(3F12.5)') b
        WRITE (*,'(3F12.5)') rv
#endif

        RETURN
ENDFUNCTION cross_product

SUBROUTINE normalize_vector(a)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(3), INTENT(INOUT) :: a
        REAL (C_DOUBLE) :: norm

        norm = SQRT(DOT_PRODUCT(a,a))

        a = a/norm

ENDSUBROUTINE normalize_vector

FUNCTION norm(a)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(3), INTENT(IN) :: a
        REAL (C_DOUBLE) :: norm

        norm = SQRT(DOT_PRODUCT(a,a))
        
        RETURN
ENDFUNCTION norm

FUNCTION interpolate_simple(a,b,c) RESULT(rv)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(2), INTENT(IN) :: a,b
        REAL (C_DOUBLE), INTENT(IN) :: c 
        REAL (C_DOUBLE) :: rv

        rv = a(2) + ((b(2)-a(2))*(c-a(1))/(b(1)-a(1)))
         
        RETURN
ENDFUNCTION interpolate_simple

FUNCTION findpos(array, searchvalue)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(:), INTENT(IN) :: array
        REAL (C_DOUBLE) , INTENT(IN) :: searchvalue
        INTEGER (C_INT) :: findpos, i

        findpos = -1

        DO i=1, SIZE(array)
                IF (searchvalue .LT. array(i)) THEN
                        findpos = i-1
                        RETURN
                ENDIF
        ENDDO
        
        RETURN
ENDFUNCTION findpos

FUNCTION bilinear_interpolation(array2D, array1D_1, array1D_2, x_1, x_2, pos_1,&
pos_2) &
RESULT(rv)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(:,:), INTENT(IN) :: array2D
        REAL (C_DOUBLE), DIMENSION(:), INTENT(IN) :: array1D_1
        REAL (C_DOUBLE), DIMENSION(:), INTENT(IN) :: array1D_2
        REAL (C_DOUBLE), INTENT(IN) :: x_1
        REAL (C_DOUBLE), INTENT(IN) :: x_2
        INTEGER (C_INT), INTENT(INOUT) :: pos_1, pos_2
        REAL (C_DOUBLE) :: rv

        REAL (C_DOUBLE) :: c1, c2, c3, c4,denom

#if DEBUG == 2
        WRITE (*,'(A,I5)') 'pos_1: ',pos_1
        WRITE (*,'(A,I5)') 'pos_2: ',pos_2
        WRITE (*,'(A,F12.6)') 'x_1: ',x_1
        WRITE (*,'(A,F12.6)') 'x_2: ',x_2

#endif



        !check if the dimensions are safe... to be removed later
        IF (SIZE(array2D,DIM=1) .NE. SIZE(array1D_1) &
        .OR. SIZE(array2D,DIM=2) .NE. SIZE(array1D_2)) THEN
                WRITE (*,'(A)') &
                'Array dimensions mismatch in bilinear interpolation'
                CALL EXIT(1)
        ENDIF

        !get positions
        IF (pos_1 == 0_C_INT .AND. pos_2 == 0_C_INT) THEN
                pos_1 = findpos(array1D_1, x_1)        
                IF (pos_1 .LT. 1_C_INT) THEN
                        WRITE (*,'(A,I5)') &
                        'Invalid result for findpos bilinear interpolation -> pos_1: ',&
                        pos_1
                        WRITE (*,'(A,F12.6)') 'array1D_1(1): ',array1D_1(1)
                        WRITE (*,'(A,F12.6)') 'x_1: ',x_1
                        WRITE (*,'(A,F12.6)') 'x_2: ',x_2
                        CALL EXIT(1)
                ENDIF

                pos_2 = findpos(array1D_2, x_2)        
                IF (pos_2 .LT. 1_C_INT) THEN
                        WRITE (*,'(A,I5)') &
                        'Invalid result for findpos bilinear interpolation -> pos_2: ',&
                        pos_2
                        WRITE (*,'(A,F12.6)') 'x_1: ',x_1
                        WRITE (*,'(A,F12.6)') 'x_2: ',x_2
                        CALL EXIT(1)
                ENDIF
        ENDIF

#if DEBUG == 2
        WRITE (*,'(A,I5)') 'pos_1 result',pos_1
        WRITE (*,'(A,I5)') 'pos_2 result',pos_2
        WRITE (*,'(A,F12.6)') 'pos_1 eval',array1D_1(pos_1)
        WRITE (*,'(A,F12.6)') 'pos_2 eval',array1D_2(pos_2)
        WRITE (*,'(A,F12.6)') 'array2D: pos1, pos2',array2D(pos_1,pos_2)
#endif


        !looks good, calculate coefficients
        denom = (array1D_1(pos_1+1)-array1D_1(pos_1))*(array1D_2(pos_2+1)-array1D_2(pos_2))
        c1 = (array1D_1(pos_1+1)-x_1)*(array1D_2(pos_2+1)-x_2)/denom
        c2 = (x_1-array1D_1(pos_1))*(array1D_2(pos_2+1)-x_2)/denom
        c3 = (array1D_1(pos_1+1)-x_1)*(x_2-array1D_2(pos_2))/denom
        c4 = (x_1-array1D_1(pos_1))*(x_2-array1D_2(pos_2))/denom
#if DEBUG == 2
        WRITE (*,'(A,F12.6)') 'denom: ',denom
        WRITE (*,'(A,F12.6)') 'c1: ',c1
        WRITE (*,'(A,F12.6)') 'c2: ',c2
        WRITE (*,'(A,F12.6)') 'c3: ',c3
        WRITE (*,'(A,F12.6)') 'c4: ',c4
#endif


        rv = c1*array2D(pos_1,pos_2) + c2*array2D(pos_1+1,pos_2) +&
                c3*array2D(pos_1, pos_2+1) + c4*array2D(pos_1+1,pos_2+1)


ENDFUNCTION bilinear_interpolation

FUNCTION xmi_sum_double(array, n_elements) BIND(C,NAME='xmi_sum_double')
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN),VALUE :: n_elements
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        REAL (C_DOUBLE) :: xmi_sum_double
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: arrayF

        xmi_sum_double = 0.0_C_DOUBLE
        CALL C_F_POINTER(array, arrayF, [n_elements])


        xmi_sum_double = SUM(arrayF)

        RETURN
ENDFUNCTION xmi_sum_double

SUBROUTINE xmi_scale_double(array, n_elements,scale_factor) BIND(C, NAME='xmi_scale_double')
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN),VALUE :: n_elements
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        REAL (C_DOUBLE), VALUE :: scale_factor
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: arrayF

        CALL C_F_POINTER(array, arrayF, [n_elements])
        
        arrayF = scale_factor*arrayF

ENDSUBROUTINE xmi_scale_double

FUNCTION trilinear_interpolation(array3D, array1D_1, array1D_2, array1D_3, x_1,&
x_2, x_3) RESULT(rv)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(:,:,:), INTENT(IN) :: array3D
        REAL (C_DOUBLE), DIMENSION(:), INTENT(IN) :: array1D_1
        REAL (C_DOUBLE), DIMENSION(:), INTENT(IN) :: array1D_2
        REAL (C_DOUBLE), DIMENSION(:), INTENT(IN) :: array1D_3
        REAL (C_DOUBLE), INTENT(IN) :: x_1
        REAL (C_DOUBLE), INTENT(IN) :: x_2
        REAL (C_DOUBLE), INTENT(IN) :: x_3
        REAL (C_DOUBLE) :: rv
        REAL (C_DOUBLE) :: bi_rv_1, bi_rv_2 

        INTEGER (C_INT) :: pos_1, pos_2, pos_3


        !check if the dimensions are safe... to be removed later
        IF (SIZE(array3D,DIM=1) .NE. SIZE(array1D_1) &
        .OR. SIZE(array3D,DIM=2) .NE. SIZE(array1D_2) &
        .OR. SIZE(array3D,DIM=3) .NE. SIZE(array1D_3) &
        ) THEN
                WRITE (*,'(A)') &
                'Array dimensions mismatch in trilinear interpolation'
                CALL EXIT(1)
        ENDIF

        !get positions
        pos_3 = findpos(array1D_3, x_3)        
        IF (pos_3 .LT. 1_C_INT) THEN
                WRITE (*,'(A)') &
                'Invalid result for findpos trilinear interpolation'
                CALL EXIT(1)
        ENDIF


        !two bilinear interpolations...
        pos_1 = 0_C_INT
        pos_2 = 0_C_INT
        bi_rv_1 = bilinear_interpolation(array3D(:,:,pos_3),array1D_1,&
        array1D_2, x_1, x_2,pos_1,pos_2)

        bi_rv_2 = bilinear_interpolation(array3D(:,:,pos_3+1),array1D_1,&
        array1D_2, x_1, x_2,pos_1,pos_2)

        !...and one linear interpolation
        rv = interpolate_simple([array1D_3(pos_3), bi_rv_1], [array1D_3(pos_3+1), bi_rv_2],&
        x_3)


        RETURN
ENDFUNCTION trilinear_interpolation

ENDMODULE 
