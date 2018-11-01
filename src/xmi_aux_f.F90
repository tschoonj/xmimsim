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

MODULE xmimsim_aux

USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: xraylib

#ifdef HAVE_EASYRNG
USE :: easyRNG, ONLY : &
        xmi_rng => easy_rng, &
        xmi_rng_uniform => easy_rng_uniform
#else
USE :: fgsl, ONLY : &
        xmi_rng => fgsl_rng, &
        xmi_rng_uniform => fgsl_rng_uniform
#endif
USE :: omp_lib

IMPLICIT NONE

!
!
!  Rayleigh, Compton interaction probabilities
!
!
TYPE :: interaction_prob
        REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:) :: energies
        REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:,:) :: Rayl_and_Compt
ENDTYPE

TYPE, BIND(C) :: interaction_probC
        INTEGER (KIND=C_INT) :: len
        TYPE (C_PTR) :: energies
        TYPE (C_PTR) :: Rayl_and_Compt
ENDTYPE

!
!
!  Doppler broadening profiles
!
!
TYPE :: compton_profiles
        !Fernandez and Scot method -> slow but very good simulation
        INTEGER (KIND=C_INT), POINTER, DIMENSION(:) :: shell_indices
        REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:) :: Qs
        REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:,:) :: profile_partial_cdf
        REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:) :: profile_partial_cdf_inv
        REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:,:) :: Qs_inv
        !Laszlo method -> fast but not as good
        REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:) :: random_numbers
        REAL (KIND=C_DOUBLE), POINTER, DIMENSION(:) :: profile_total_icdf
ENDTYPE

TYPE, BIND(C) :: compton_profilesC
        INTEGER (KIND=C_INT) :: shell_indices_len
        INTEGER (KIND=C_INT) :: data_len
        TYPE (C_PTR) :: shell_indices
        TYPE (C_PTR) :: Qs
        TYPE (C_PTR) :: profile_partial_cdf
        TYPE (C_PTR) :: profile_partial_cdf_inv
        TYPE (C_PTR) :: Qs_inv
        TYPE (C_PTR) :: random_numbers
        TYPE (C_PTR) :: profile_total_icdf
ENDTYPE


!
!
! HDF5 data structures
!
!
TYPE :: xmi_hdf5_Z
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: RayleighTheta_ICDF
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: ComptonTheta_ICDF
        REAL (C_DOUBLE), POINTER, DIMENSION(:)   :: Energies
        REAL (C_DOUBLE), POINTER, DIMENSION(:)   :: RandomNumbers
        REAL (C_DOUBLE), POINTER, DIMENSION(:)   :: FluorYieldsCorr
        !interaction_probs ...
        TYPE (interaction_prob) :: interaction_probs
        TYPE (compton_profiles) :: compton_profiles
        INTEGER (C_INT) :: Z
        INTEGER (C_INT) :: Zindex
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:,:) :: precalc_xrf_cs
ENDTYPE



TYPE :: xmi_hdf5
        TYPE (xmi_hdf5_Z), POINTER, DIMENSION(:) :: xmi_hdf5_Zs
        REAL (C_DOUBLE), POINTER, DIMENSION(:,:) :: Phi_ICDF
        REAL (C_DOUBLE), POINTER, DIMENSION(:)   :: Thetas
        REAL (C_DOUBLE), POINTER, DIMENSION(:)   :: RandomNumbers
        INTEGER (C_INT), DIMENSION(94) :: uniqZ
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
        TYPE (C_PTR) :: comments
ENDTYPE

TYPE :: xmi_general
        REAL (C_FLOAT) :: version
        !CHARACTER (KIND=C_CHAR,LEN=:),ALLOCATABLE :: outputfile
        INTEGER (C_LONG) :: n_photons_interval
        INTEGER (C_LONG) :: n_photons_line
        INTEGER (C_INT) :: n_interactions_trajectory
        !CHARACTER (KIND=C_CHAR,LEN=:),ALLOCATABLE :: comments
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
        REAL (C_DOUBLE) :: d_sample_source
        REAL (C_DOUBLE), DIMENSION(3) :: n_sample_orientation
        REAL (C_DOUBLE), DIMENSION(3) :: p_detector_window
        REAL (C_DOUBLE), DIMENSION(3) :: n_detector_orientation
        REAL (C_DOUBLE) :: area_detector
        REAL (C_DOUBLE) :: collimator_height
        REAL (C_DOUBLE) :: collimator_diameter
        REAL (C_DOUBLE) :: d_source_slit
        REAL (C_DOUBLE) :: slit_size_x
        REAL (C_DOUBLE) :: slit_size_y
ENDTYPE

! XmiEnergyDiscreteDistribution

ENUM, BIND(C)
        ENUMERATOR :: XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC
        ENUMERATOR :: XMI_ENERGY_DISCRETE_DISTRIBUTION_GAUSSIAN
        ENUMERATOR :: XMI_ENERGY_DISCRETE_DISTRIBUTION_LORENTZIAN
ENDENUM

!  xmi_energy_discrete

TYPE, BIND(C) :: xmi_energy_discrete
        REAL (C_DOUBLE) :: energy
        REAL (C_DOUBLE) :: horizontal_intensity
        REAL (C_DOUBLE) :: vertical_intensity
        REAL (C_DOUBLE) :: sigma_x
        REAL (C_DOUBLE) :: sigma_xp
        REAL (C_DOUBLE) :: sigma_y
        REAL (C_DOUBLE) :: sigma_yp
        INTEGER (KIND(XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC)) :: distribution_type
        REAL (C_DOUBLE) :: scale_parameter
ENDTYPE

!  xmi_energy_continuous

TYPE, BIND(C) :: xmi_energy_continuous
        REAL (C_DOUBLE) :: energy
        REAL (C_DOUBLE) :: horizontal_intensity
        REAL (C_DOUBLE) :: vertical_intensity
        REAL (C_DOUBLE) :: sigma_x
        REAL (C_DOUBLE) :: sigma_xp
        REAL (C_DOUBLE) :: sigma_y
        REAL (C_DOUBLE) :: sigma_yp
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
        TYPE (xmi_energy_discrete), ALLOCATABLE, DIMENSION(:) :: discrete
        INTEGER (C_INT) :: n_continuous
        TYPE (xmi_energy_continuous), ALLOCATABLE, DIMENSION(:) :: continuous
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

!  xmi_detector

TYPE, BIND(C) :: xmi_detectorC
        INTEGER (C_INT) :: detector_type
        REAL (C_DOUBLE) :: live_time
        REAL (C_DOUBLE) :: pulse_width
        REAL (C_DOUBLE) :: gain
        REAL (C_DOUBLE) :: zero
        REAL (C_DOUBLE) :: fano
        REAL (C_DOUBLE) :: noise
        INTEGER (C_INT) :: nchannels
        INTEGER (C_INT) :: n_crystal_layers
        TYPE (C_PTR) :: crystal_layers
ENDTYPE

TYPE :: xmi_detector
        INTEGER (C_INT) :: detector_type
        REAL (C_DOUBLE) :: live_time
        REAL (C_DOUBLE) :: pulse_width
        REAL (C_DOUBLE) :: gain
        REAL (C_DOUBLE) :: zero
        REAL (C_DOUBLE) :: fano
        REAL (C_DOUBLE) :: noise
        INTEGER (C_INT) :: nchannels
        INTEGER (C_INT) :: n_crystal_layers
        TYPE (xmi_layer), ALLOCATABLE, DIMENSION(:) :: crystal_layers
        !below is not present in C variable!!!
        LOGICAL :: collimator_present
        REAL (C_DOUBLE) :: detector_radius
        REAL (C_DOUBLE) :: collimator_radius
        REAL (C_DOUBLE) :: half_apex
        REAL (C_DOUBLE), DIMENSION(3) :: vertex
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
       TYPE (C_PTR) :: xmi_inputC
ENDTYPE

TYPE, BIND(C) :: xmi_main_options
        INTEGER (C_INT) :: use_M_lines = 1_C_INT
        INTEGER (C_INT) :: use_cascade_auger = 1_C_INT
        INTEGER (C_INT) :: use_cascade_radiative = 1_C_INT
        INTEGER (C_INT) :: use_variance_reduction = 1_C_INT
        INTEGER (C_INT) :: use_sum_peaks = 0_C_INT
        INTEGER (C_INT) :: use_escape_peaks = 1_C_INT
        INTEGER (C_INT) :: escape_ratios_mode = 0_C_INT
        INTEGER (C_INT) :: verbose = 0_C_INT
        INTEGER (C_INT) :: use_poisson = 0_C_INT
        INTEGER (C_INT) :: use_opencl = 1_C_INT
        INTEGER (C_INT) :: omp_num_threads
        INTEGER (C_INT) :: extra_verbose = 0_C_INT
        TYPE (C_PTR) :: custom_detector_response = C_NULL_PTR
        INTEGER (C_INT) :: use_advanced_compton = 0_C_INT
        INTEGER (C_INT) :: use_default_seeds = 0_C_INT
ENDTYPE xmi_main_options

!
!
!       xmi_solid_angle: Contains the solid angles for a particular geometry
!
!
TYPE :: xmi_solid_angle
        REAL (C_DOUBLE), DIMENSION(:,:), POINTER :: solid_angles
        INTEGER (C_LONG) :: grid_dims_r_n
        INTEGER (C_LONG) :: grid_dims_theta_n
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: grid_dims_r_vals
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: grid_dims_theta_vals
ENDTYPE

!
!
!       xmi_escape_ratios
!
!
TYPE :: xmi_escape_ratios
        INTEGER (C_INT) :: n_elements
        INTEGER (C_INT) :: n_fluo_input_energies
        INTEGER (C_INT) :: n_compton_input_energies
        INTEGER (C_INT) :: n_compton_output_energies
        !dimension of Z is (n_elements)
        INTEGER (C_INT), DIMENSION(:), POINTER :: Z
        !dimension of fluo_escape_ratios is (n_elements,KL1_LINE-L3P3_LINE,n_fluo_input_energies)
        REAL (C_DOUBLE), DIMENSION(:,:,:), POINTER :: fluo_escape_ratios
        !dimension of fluo_escape_input_energies is n_fluo_input_energies
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: fluo_escape_input_energies
        !dimension of compton_escape_ratios is
        !(n_compton_input_energies,n_compton_output_energies)
        REAL (C_DOUBLE), DIMENSION(:,:), POINTER :: compton_escape_ratios
        !dimension of compton_escape_input_energies is n_compton_input_energies
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: compton_escape_input_energies
        !dimension of compton_escape_output_energies is n_compton_output_energies
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: compton_escape_output_energies
ENDTYPE xmi_escape_ratios

TYPE, BIND(C) :: xmi_escape_ratiosC
        INTEGER (C_INT) :: n_elements
        INTEGER (C_INT) :: n_fluo_input_energies
        INTEGER (C_INT) :: n_compton_input_energies
        INTEGER (C_INT) :: n_compton_output_energies
        TYPE (C_PTR) :: Z
        TYPE (C_PTR) :: fluo_escape_ratios
        TYPE (C_PTR) :: fluo_escape_input_energies
        TYPE (C_PTR) :: compton_escape_ratios
        TYPE (C_PTR) :: compton_escape_input_energies
        TYPE (C_PTR) :: compton_escape_output_energies
        TYPE (C_PTR) :: xmi_input_string
ENDTYPE xmi_escape_ratiosC

!
!
!       xmi_precalc_mu_cs: contains precalculated absorption coefficients for
!       XRF photons
!
!

TYPE :: xmi_precalc_mu_cs
        REAL (C_DOUBLE), DIMENSION(:,:), ALLOCATABLE :: mu
ENDTYPE xmi_precalc_mu_cs



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

        !initial energy of the photon
        REAL (C_DOUBLE) :: initial_energy

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

        !secondary weight of the photon for escape ratios mode
        !this one will be used to calculated the fluo_escape_ratios
        REAL (C_DOUBLE) :: weight_escape

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

        !cascade type
        INTEGER (C_INT) :: xmi_cascade_type

        !solid angles pointer
        TYPE (xmi_solid_angle), POINTER :: solid_angle

        !detector_solid_angle_not_found
        INTEGER (C_LONG) :: detector_solid_angle_not_found

        !xmi_precalc_mu_cs
        TYPE (xmi_precalc_mu_cs), DIMENSION(:), POINTER :: precalc_mu_cs

        !variance reduction history
        REAL (C_DOUBLE), DIMENSION(:,:,:), POINTER :: var_red_history

        !channels
        REAL (C_DOUBLE), DIMENSION(:,:), POINTER :: channels

        !the XRF line energies
        REAL (C_DOUBLE), DIMENSION(:,:), POINTER :: LineEnergies

        LOGICAL :: inside

        !debug variables
        REAL (C_DOUBLE) :: theta_i
        REAL (C_DOUBLE) :: phi_i
        REAL (C_DOUBLE) :: theta_i2
        REAL (C_DOUBLE) :: phi_i2
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

!
!
!       datatype for the trapezoidal distribution
!
!
TYPE, PUBLIC :: xmi_ran_trap_workspace
        PRIVATE
        REAL (C_DOUBLE) :: denom
        REAL (C_DOUBLE) :: m
        REAL (C_DOUBLE) :: x1
        REAL (C_DOUBLE) :: x2
        REAL (C_DOUBLE) :: y1
        REAL (C_DOUBLE) :: y2
ENDTYPE

!
!
!       options for the escape ratios calculation
!
!
TYPE, BIND(C) :: xmi_escape_ratios_options
        INTEGER (C_LONG) :: n_input_energies = 1990
        INTEGER (C_LONG) :: n_compton_output_energies = 1999
        INTEGER (C_LONG) :: n_photons = 500000
        REAL (C_DOUBLE) :: input_energy_min
        REAL (C_DOUBLE) :: input_energy_delta
        REAL (C_DOUBLE) :: compton_output_energy_min
        REAL (C_DOUBLE) :: compton_output_energy_delta
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

SUBROUTINE xmi_print_progress(string, progress)&
BIND(C,NAME='xmi_print_progress')
        USE,INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        !TYPE (C_PTR), VALUE :: string
        CHARACTER (KIND=C_CHAR), DIMENSION(*) :: string
        INTEGER (C_INT), VALUE :: progress
ENDSUBROUTINE xmi_print_progress

!interface for the xmi_get_random_numbers function
FUNCTION xmi_get_random_numbers(numbers, n) BIND(C,NAME='xmi_get_random_numbers')
        USE, INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        TYPE (C_PTR), VALUE :: numbers !points to a C array of unsigned long
        !int's
        INTEGER (C_LONG), VALUE :: n
        INTEGER (C_INT) :: xmi_get_random_numbers
ENDFUNCTION xmi_get_random_numbers

FUNCTION xmi_xmlfile_to_string(xmlfile, xmlstring, xmlstringlength) &
BIND(C,NAME='xmi_xmlfile_to_string') RESULT(rv)
        USE, INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        TYPE (C_PTR), VALUE, INTENT(IN) :: xmlfile
        TYPE (C_PTR), INTENT(INOUT) :: xmlstring
        INTEGER (C_INT), INTENT(INOUT) :: xmlstringlength
        INTEGER (C_INT) :: rv
ENDFUNCTION xmi_xmlfile_to_string

SUBROUTINE xmi_free(ptr) BIND(C,NAME='xmi_free')
        USE, INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        TYPE (C_PTR), VALUE, INTENT(IN) :: ptr
ENDSUBROUTINE xmi_free

FUNCTION log1p(x) BIND(C,NAME='log1p') RESULT(rv)
        USE, INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        REAL (C_DOUBLE), VALUE, INTENT(IN) :: x
        REAL (C_DOUBLE) :: rv
ENDFUNCTION log1p

FUNCTION expm1(x) BIND(C,NAME='expm1') RESULT(rv)
        USE, INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        REAL (C_DOUBLE), VALUE, INTENT(IN) :: x
        REAL (C_DOUBLE) :: rv
ENDFUNCTION expm1

SUBROUTINE xmi_exit(status) BIND(C,NAME='exit')
        USE, INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        INTEGER (C_INT), VALUE, INTENT(IN) :: status
ENDSUBROUTINE xmi_exit
ENDINTERFACE

INTERFACE ASSIGNMENT(=)
        MODULE PROCEDURE assign_interaction_prob
ENDINTERFACE

INTERFACE xmi_mu_calc
        MODULE PROCEDURE xmi_mu_calc_xmi_layer_single_energy, &
        xmi_mu_calc_xmi_composition_single_energy
ENDINTERFACE

CHARACTER (LEN=2,KIND=C_CHAR), DIMENSION(99) :: elements = &
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

!global variables
INTEGER (C_INT), PARAMETER ::  NO_INTERACTION = 0
INTEGER (C_INT), PARAMETER ::  RAYLEIGH_INTERACTION = 1
INTEGER (C_INT), PARAMETER ::  COMPTON_INTERACTION = 2
INTEGER (C_INT), PARAMETER ::  PHOTOELECTRIC_INTERACTION = 3

INTEGER (C_INT), PARAMETER ::  XMI_DETECTOR_SILI = 0
INTEGER (C_INT), PARAMETER ::  XMI_DETECTOR_GE = 1
INTEGER (C_INT), PARAMETER ::  XMI_DETECTOR_SI_SDD = 2

INTEGER (C_INT), PARAMETER ::  XMI_CASCADE_FULL = 4
INTEGER (C_INT), PARAMETER ::  XMI_CASCADE_RADIATIVE = 3
INTEGER (C_INT), PARAMETER ::  XMI_CASCADE_NONRADIATIVE = 2
INTEGER (C_INT), PARAMETER ::  XMI_CASCADE_NONE = 1

INTEGER (C_INT), PARAMETER ::  XMI_NO_INTERSECTION = 0
INTEGER (C_INT), PARAMETER ::  XMI_COLLIMATOR_INTERSECTION = 1
INTEGER (C_INT), PARAMETER ::  XMI_DETECTOR_INTERSECTION = 2
INTEGER (C_INT), PARAMETER ::  XMI_DETECTOR_BAD_INTERSECTION = 3

!threshold is 1 keV
REAL (C_DOUBLE), PARAMETER :: energy_threshold = 1.0_C_DOUBLE
REAL (C_DOUBLE), PARAMETER :: energy_max = 200.0_C_DOUBLE

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
        REAL (KIND=C_DOUBLE),DIMENSION(:),INTENT(IN) :: invar
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

SUBROUTINE xmi_input_F2C(xmi_inputFPtr, xmi_inputC_out) &
        BIND(C, NAME='xmi_input_F2C')
        IMPLICIT NONE
        TYPE (C_PTR), INTENT(IN), VALUE :: xmi_inputFPtr
        TYPE (xmi_input), POINTER :: xmi_inputF
        TYPE (C_PTR), INTENT(OUT) :: xmi_inputC_out

        CALL C_F_POINTER(xmi_inputFPtr, xmi_inputF)
        xmi_inputC_out = xmi_inputF%xmi_inputC

ENDSUBROUTINE xmi_input_F2C

!
! xmi_input_C2F creates a complete copy of the C structure!!!
!

SUBROUTINE xmi_input_C2F(xmi_inputC_in,xmi_inputFPtr) BIND(C,NAME='xmi_input_C2F')
        IMPLICIT NONE
        !pass by reference, no VALUE
        TYPE (xmi_inputC), INTENT(IN), TARGET :: xmi_inputC_in
        TYPE (C_PTR), INTENT(OUT) :: xmi_inputFPtr
        TYPE (xmi_input), POINTER :: xmi_inputF

        TYPE (xmi_generalC),POINTER :: xmi_general_temp
        TYPE (xmi_compositionC),POINTER :: xmi_composition_temp
        TYPE (xmi_geometry),POINTER :: xmi_geometry_temp
        TYPE (xmi_excitationC),POINTER :: xmi_excitation_temp
        TYPE (xmi_absorbersC),POINTER :: xmi_absorbers_temp
        TYPE (xmi_detectorC),POINTER :: xmi_detector_temp
        TYPE (xmi_layerC),POINTER, DIMENSION(:) :: xmi_layer_temp
        TYPE (xmi_energy_discrete),POINTER, DIMENSION(:) :: xmi_energy_temp_disc
        TYPE (xmi_energy_continuous),POINTER, DIMENSION(:) :: xmi_energy_temp_cont
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
        xmi_inputF%xmi_inputC = C_LOC(xmi_inputC_in)
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
                ALLOCATE(xmi_inputF%composition%layers(i)%Z(xmi_layer_temp(i)%n_elements))
                xmi_inputF%composition%layers(i)%Z = &
                Z_temp
                CALL C_F_POINTER &
                (xmi_layer_temp(i)%weight,weight_temp,[xmi_layer_temp(i)%n_elements]  )
                ALLOCATE(xmi_inputF%composition%layers(i)%weight(xmi_layer_temp(i)%n_elements))
                xmi_inputF%composition%layers(i)%weight = &
                weight_temp
                xmi_inputF%composition%layers(i)%weight = xmi_inputF%composition%layers(i)%weight&
                /SUM(xmi_inputF%composition%layers(i)%weight)
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
                xmi_energy_temp_disc,&
                [xmi_inputF%excitation%n_discrete])
                ALLOCATE(xmi_inputF%excitation%discrete(xmi_inputF%excitation%n_discrete))
                xmi_inputF%excitation%discrete = xmi_energy_temp_disc
        ENDIF

        IF (xmi_inputF%excitation%n_continuous .GT. 0) THEN
                CALL C_F_POINTER (xmi_excitation_temp%continuous,&
                xmi_energy_temp_cont,&
                [xmi_inputF%excitation%n_continuous])
                ALLOCATE(xmi_inputF%excitation%continuous(xmi_inputF%excitation%n_continuous))
                xmi_inputF%excitation%continuous = xmi_energy_temp_cont
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
                        ALLOCATE(xmi_inputF%absorbers%exc_layers(i)%Z(xmi_layer_temp(i)%n_elements))
                        xmi_inputF%absorbers%exc_layers(i)%Z = &
                        Z_temp
                        CALL C_F_POINTER &
                        (xmi_layer_temp(i)%weight,weight_temp,[xmi_layer_temp(i)%n_elements]  )
                        ALLOCATE(xmi_inputF%absorbers%exc_layers(i)%weight(xmi_layer_temp(i)%n_elements))
                        xmi_inputF%absorbers%exc_layers(i)%weight = &
                        weight_temp
                        xmi_inputF%absorbers%exc_layers(i)%weight = xmi_inputF%absorbers%exc_layers(i)%weight&
                        /SUM(xmi_inputF%absorbers%exc_layers(i)%weight)
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
                        ALLOCATE(xmi_inputF%absorbers%det_layers(i)%Z(xmi_layer_temp(i)%n_elements))
                        xmi_inputF%absorbers%det_layers(i)%Z = &
                        Z_temp
                        CALL C_F_POINTER &
                        (xmi_layer_temp(i)%weight,weight_temp,[xmi_layer_temp(i)%n_elements]  )
                        ALLOCATE(xmi_inputF%absorbers%det_layers(i)%weight(xmi_layer_temp(i)%n_elements))
                        xmi_inputF%absorbers%det_layers(i)%weight = &
                        weight_temp
                        xmi_inputF%absorbers%det_layers(i)%weight = xmi_inputF%absorbers%det_layers(i)%weight&
                        /SUM(xmi_inputF%absorbers%det_layers(i)%weight)
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
        xmi_inputF%detector%pulse_width= xmi_detector_temp%pulse_width
        xmi_inputF%detector%live_time= xmi_detector_temp%live_time
        xmi_inputF%detector%gain= xmi_detector_temp%gain
        xmi_inputF%detector%zero= xmi_detector_temp%zero
        xmi_inputF%detector%fano= xmi_detector_temp%fano
        xmi_inputF%detector%noise= xmi_detector_temp%noise
        xmi_inputF%detector%nchannels = xmi_detector_temp%nchannels
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
                        ALLOCATE(xmi_inputF%detector%crystal_layers(i)%Z(xmi_layer_temp(i)%n_elements))
                        xmi_inputF%detector%crystal_layers(i)%Z = &
                        Z_temp
                        CALL C_F_POINTER &
                        (xmi_layer_temp(i)%weight,weight_temp,[xmi_layer_temp(i)%n_elements]  )
                        ALLOCATE(xmi_inputF%detector%crystal_layers(i)%weight(xmi_layer_temp(i)%n_elements))
                        xmi_inputF%detector%crystal_layers(i)%weight = &
                        weight_temp
                        xmi_inputF%detector%crystal_layers(i)%weight = xmi_inputF%detector%crystal_layers(i)%weight&
                        /SUM(xmi_inputF%detector%crystal_layers(i)%weight)
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
#if DEBUG == 1
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
                rv = rv + CS_Total_Kissel(layer%Z(i),&
                energy)*layer%weight(i)
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
                WRITE (error_unit,'(A)') 'Parallel plane and line found'
                WRITE (error_unit,'(A,3F12.5)') 'line%dirv:',line%dirv
                WRITE (error_unit,'(A,3F12.5)') 'plane%normv:',plane%normv
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
        INTEGER :: i

        rv = SQRT((point1(1)-point2(1))**2 + &
                (point1(2)-point2(2))**2 + &
                (point1(3)-point2(3))**2)

        !IF (rv .EQ. 0.0_C_DOUBLE) THEN
        !  WRITE (error_unit,'(A)') 'xmi_distance_two_points: SQRT is zero'
        !  WRITE (error_unit,'(A,3ES30.20)') 'point1:', (point1(i),i=1,3)
        !  WRITE (error_unit,'(A,3ES30.20)') 'point2:', (point2(i),i=1,3)
        !  CALL xmi_exit(1)
        !ENDIF

        RETURN
ENDFUNCTION xmi_distance_two_points

SUBROUTINE xmi_move_photon_with_dist(photon, dist)
        IMPLICIT NONE
        TYPE (xmi_photon), INTENT(INOUT)  :: photon
        REAL (C_DOUBLE), INTENT(IN) :: dist


        photon%coords = photon%coords + dist*photon%dirv

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

        a = a/norm(a)

        RETURN
ENDSUBROUTINE normalize_vector

FUNCTION norm(a)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(3), INTENT(IN) :: a
        REAL (C_DOUBLE) :: norm

        norm = SQRT(DOT_PRODUCT(a,a))

        RETURN
ENDFUNCTION norm

SUBROUTINE xmi_normalize_vector_double(array, n_elements) BIND(C, NAME='xmi_normalize_vector_double')
        IMPLICIT NONE
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: arrayF
        INTEGER (C_INT), VALUE, INTENT(IN) :: n_elements

        CALL C_F_POINTER(array,arrayF,[n_elements])

        CALL normalize_vector(arrayF)

        RETURN
ENDSUBROUTINE xmi_normalize_vector_double

FUNCTION xmi_norm_double(array, n_elements) BIND(C, NAME='xmi_norm_double')
        IMPLICIT NONE
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: arrayF
        INTEGER (C_INT), VALUE, INTENT(IN) :: n_elements
        REAL (C_DOUBLE) :: xmi_norm_double

        CALL C_F_POINTER(array,arrayF,[n_elements])

        xmi_norm_double = norm(arrayF)

        RETURN
ENDFUNCTION xmi_norm_double

FUNCTION interpolate_simple(a,b,c) RESULT(rv)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(2), INTENT(IN) :: a,b
        REAL (C_DOUBLE), INTENT(IN) :: c
        REAL (C_DOUBLE) :: rv

        rv = a(2) + ((b(2)-a(2))*(c-a(1))/(b(1)-a(1)))

        RETURN
ENDFUNCTION interpolate_simple

FUNCTION findpos_fast(array,searchvalue)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(:), INTENT(IN) :: array
        REAL (C_DOUBLE) , INTENT(IN) :: searchvalue
        INTEGER (C_INT) :: findpos_fast, i, guess

        findpos_fast = -1

        IF (ABS(searchvalue-array(LBOUND(array, DIM=1))) .LT. 1E-10_C_DOUBLE) THEN
                findpos_fast = 1
                RETURN
        ENDIF

        guess = INT((searchvalue-0.1_C_DOUBLE)*(10000.0_C_DOUBLE-1.0_C_DOUBLE)/&
                (100.0_C_DOUBLE-0.1_C_DOUBLE)+1.0_C_DOUBLE,KIND=C_INT)


        DO i=guess, UBOUND(array,DIM=1)
                IF (searchvalue .LE. array(i)) THEN
                        findpos_fast = i-1
                        RETURN
                ENDIF
        ENDDO

        RETURN
ENDFUNCTION findpos_fast

FUNCTION findpos(array, searchvalue, start_pos_arg)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(:), INTENT(IN) :: array
        REAL (C_DOUBLE) , INTENT(IN) :: searchvalue
        INTEGER (C_INT), INTENT(IN), OPTIONAL :: start_pos_arg
        INTEGER (C_INT) :: findpos, i, start_pos

        findpos = -1

        !this would appear to be necessary... the rng generates exactly 0.0
        !sometimes...
        IF (.NOT.PRESENT(start_pos_arg) .AND. ABS(searchvalue-array(LBOUND(array,DIM=1))) .LT. 1E-10_C_DOUBLE) THEN
                findpos = 1
                RETURN
        ENDIF

        IF (PRESENT(start_pos_arg)) THEN
                start_pos = start_pos_arg
        ELSE
                start_pos = LBOUND(array,DIM=1)+1
        ENDIF

        DO i=start_pos, UBOUND(array,DIM=1)
                IF (searchvalue .LE. array(i)) THEN
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
                WRITE (error_unit,'(A)') &
                'Array dimensions mismatch in bilinear interpolation'
                WRITE (error_unit,'(A,I5)') 'SIZE(array2D,DIM=1)',SIZE(array2D,DIM=1)
                WRITE (error_unit,'(A,I5)') 'SIZE(array1D_1)',SIZE(array1D_1)
                WRITE (error_unit,'(A,I5)') 'SIZE(array2D,DIM=2)',SIZE(array2D,DIM=2)
                WRITE (error_unit,'(A,I5)') 'SIZE(array1D_2)',SIZE(array1D_2)
                CALL xmi_exit(1)
        ENDIF

        !get positions
        IF (pos_1 == 0_C_INT .AND. pos_2 == 0_C_INT) THEN
                pos_1 = findpos(array1D_1, x_1)
                IF (pos_1 .LT. 1_C_INT) THEN
                        WRITE (error_unit,'(A,I5)') &
                        'Invalid result for findpos bilinear interpolation -> pos_1: ',&
                        pos_1
                        WRITE (error_unit,'(A,ES14.6)') 'array1D_1(1): ',array1D_1(1)
                        WRITE (error_unit,'(A,ES14.6)') 'x_1: ',x_1
                        WRITE (error_unit,'(A,ES14.6)') 'x_2: ',x_2
                        WRITE (error_unit,'(A,I4)') 'pos_1: ',pos_1
                        CALL xmi_exit(1)
                ENDIF

                pos_2 = findpos(array1D_2, x_2)
                IF (pos_2 .LT. 1_C_INT) THEN
                        WRITE (error_unit,'(A,I5)') &
                        'Invalid result for findpos bilinear interpolation -> pos_2: ',&
                        pos_2
                        WRITE (error_unit,'(A,ES14.6)') 'array1D_2(1): ',array1D_2(1)
                        WRITE (error_unit,'(A,ES14.6)') 'x_1: ',x_1
                        WRITE (error_unit,'(A,ES14.6)') 'x_2: ',x_2
                        WRITE (error_unit,'(A,I4)') 'pos_2: ',pos_2
                        CALL xmi_exit(1)
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

FUNCTION xmi_maxloc_double(array, n_elements) BIND(C,NAME='xmi_maxloc_double')
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN),VALUE :: n_elements
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        INTEGER (C_INT) :: xmi_maxloc_double
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: arrayF

        INTEGER, DIMENSION(1) :: loc

        CALL C_F_POINTER(array, arrayF, [n_elements])
        loc = MAXLOC(arrayF)

        xmi_maxloc_double = loc(1)-1

        RETURN
ENDFUNCTION xmi_maxloc_double


FUNCTION xmi_sum_double(array, n_elements) BIND(C,NAME='xmi_sum_double')
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN),VALUE :: n_elements
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        REAL (C_DOUBLE) :: xmi_sum_double
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: arrayF

        xmi_sum_double = 0.0_C_DOUBLE

        IF (n_elements == 0_C_INT) THEN
                RETURN
        ENDIF

        CALL C_F_POINTER(array, arrayF, [n_elements])


        xmi_sum_double = SUM(arrayF)

        RETURN
ENDFUNCTION xmi_sum_double

FUNCTION xmi_sum_int(array, n_elements) BIND(C,NAME='xmi_sum_int')
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN),VALUE :: n_elements
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        INTEGER (C_INT) :: xmi_sum_int
        INTEGER (C_INT), DIMENSION(:), POINTER :: arrayF

        xmi_sum_int = 0_C_INT
        CALL C_F_POINTER(array, arrayF, [n_elements])


        xmi_sum_int = SUM(arrayF)

        RETURN
ENDFUNCTION xmi_sum_int

SUBROUTINE xmi_scale_double(array, n_elements,scale_factor) BIND(C, NAME='xmi_scale_double')
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN),VALUE :: n_elements
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        REAL (C_DOUBLE), VALUE :: scale_factor
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: arrayF

        CALL C_F_POINTER(array, arrayF, [n_elements])

        arrayF = scale_factor*arrayF

        RETURN
ENDSUBROUTINE xmi_scale_double

SUBROUTINE xmi_add_val_to_array_double(array, n_elements,increment)&
BIND(C, NAME='xmi_add_val_to_array_double')
        IMPLICIT NONE
        INTEGER (C_INT), INTENT(IN),VALUE :: n_elements
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        REAL (C_DOUBLE), VALUE :: increment
        REAL (C_DOUBLE), DIMENSION(:), POINTER :: arrayF

        CALL C_F_POINTER(array, arrayF, [n_elements])

        arrayF = arrayF+increment

        RETURN
ENDSUBROUTINE xmi_add_val_to_array_double



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
                WRITE (error_unit,'(A)') &
                'Array dimensions mismatch in trilinear interpolation'
                CALL xmi_exit(1)
        ENDIF

        !get positions
        pos_3 = findpos(array1D_3, x_3)
        IF (pos_3 .LT. 1_C_INT) THEN
                WRITE (error_unit,'(A)') &
                'Invalid result for findpos trilinear interpolation'
                WRITE (error_unit,'(A,ES12.4)') 'Requested valued x_3: ',x_3
                WRITE (error_unit,'(A,ES12.4)') 'array1D_3(1): ',array1D_3(1)
                WRITE (error_unit,'(A,ES12.4)') 'array1D_3(last): ',array1D_3(SIZE(array1D_3))
                WRITE (error_unit,'(A,I4)') 'pos_3: ',pos_3
                CALL xmi_exit(1)
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

FUNCTION xmi_dindgen(n) RESULT(rv)
        IMPLICIT NONE
        REAL (C_DOUBLE), ALLOCATABLE, DIMENSION(:) :: rv
        INTEGER (C_INT), INTENT(IN) :: n
        INTEGER (C_INT) :: i

        IF (n .LT. 1) THEN
                WRITE (error_unit,'(A)') 'xmi_dindgen expects a strict positive integer'
                CALL xmi_exit(1)
        ENDIF

        ALLOCATE(rv(n))
        DO i=0_C_INT,n-1
                rv(i+1) = REAL(i,KIND=C_DOUBLE)
        ENDDO

        RETURN
ENDFUNCTION xmi_dindgen


FUNCTION xmi_maxval_double(array, n) RESULT(rv)&
BIND(C,NAME='xmi_maxval_double')
        IMPLICIT NONE
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        INTEGER (C_INT), INTENT(IN), VALUE :: n
        REAL (C_DOUBLE) :: rv
        REAL (C_DOUBLE), POINTER, DIMENSION(:) :: arrayF

        CALL C_F_POINTER(array, arrayF, [n])

        rv = MAXVAL(arrayF)

        RETURN
ENDFUNCTION xmi_maxval_double

FUNCTION xmi_minval_double(array, n) RESULT(rv)&
BIND(C,NAME='xmi_minval_double')
        IMPLICIT NONE
        TYPE (C_PTR), VALUE, INTENT(IN) :: array
        INTEGER (C_INT), INTENT(IN), VALUE :: n
        REAL (C_DOUBLE) :: rv
        REAL (C_DOUBLE), POINTER, DIMENSION(:) :: arrayF

        CALL C_F_POINTER(array, arrayF, [n])

        rv = MINVAL(arrayF)

        RETURN
ENDFUNCTION xmi_minval_double

FUNCTION xmi_check_detector_intersection&
(inputF,begin_coords,end_coords) RESULT(rv)
        IMPLICIT NONE
        REAL (C_DOUBLE), DIMENSION(3), INTENT(IN) ::&
        begin_coords, end_coords
        TYPE (xmi_input), INTENT(IN) :: inputF
        INTEGER (C_INT) :: rv

        REAL (C_DOUBLE), DIMENSION(3) :: begin_coords_det,&
        end_coords_det
        REAL (C_DOUBLE) :: x_coord_prod
        TYPE (xmi_plane) :: plane
        TYPE (xmi_line) :: line
        REAL (C_DOUBLE), DIMENSION(3) :: intersection
        REAL (C_DOUBLE), DIMENSION(3) :: delta,dM,deltaM,X1,X2
        REAL (C_DOUBLE) :: c0, c1, c2,cos2theta,disc,t1,t2,t_begin,t_end
        REAL (C_DOUBLE), DIMENSION(3,3) :: M
        INTEGER (C_INT) :: n_valid_t



        !convert lab coordinates to detector coordinate system
        begin_coords_det = MATMUL(inputF%detector%n_detector_orientation_inverse, &
                begin_coords-inputF%geometry%p_detector_window)
        end_coords_det = MATMUL(inputF%detector%n_detector_orientation_inverse, &
                end_coords-inputF%geometry%p_detector_window)



        !easy case: no collimator present...
        IF (inputF%detector%collimator_present .EQV. .FALSE.) THEN
                x_coord_prod = begin_coords_det(1)*end_coords_det(1)
                IF (x_coord_prod .GT. 0) THEN
                        !no intersection with detector plane
                        rv = XMI_NO_INTERSECTION
                        RETURN
                ENDIF
                !else: calculate intersection point
                !first check if direction is ok
                line%point=end_coords_det
                line%dirv=norm(end_coords_det-begin_coords_det)
                plane%point=[0.0_C_DOUBLE,0.0_C_DOUBLE,0.0_C_DOUBLE]
                plane%normv=[1.0_C_DOUBLE,0.0_C_DOUBLE,0.0_C_DOUBLE]
                IF (xmi_intersection_plane_line(plane, line,&
                intersection) == 0) CALL xmi_exit(1)
                intersection(1)=0.0_C_DOUBLE
                IF (norm(intersection) .LE. inputF%detector%detector_radius)&
                THEN
                        IF (DOT_PRODUCT(line%dirv,plane%normv&
                        ) .GE. 0.0_C_DOUBLE) THEN
                                rv = XMI_DETECTOR_BAD_INTERSECTION
                                RETURN
                        ENDIF
                        rv = XMI_DETECTOR_INTERSECTION
                        RETURN
                ENDIF
                !else
                rv = XMI_NO_INTERSECTION
        ELSE
                !so we have a collimator...
                !see if we went through it
                !solution depends on whether it is conical or cylindrical
                !cylindrical is bit difficult here... the radii will never be
                !identical since the detector one is calculated through its
                !area...
                !collimator diameter should be changed to collimator area
                !so cone only now...
                line%point=end_coords_det
                line%dirv=end_coords_det-begin_coords_det
                plane%point=[0.0_C_DOUBLE,0.0_C_DOUBLE,0.0_C_DOUBLE]
                plane%normv=[1.0_C_DOUBLE,0.0_C_DOUBLE,0.0_C_DOUBLE]
                t_begin=(begin_coords_det(1)-line%point(1))/line%dirv(1)
                t_end=(end_coords_det(1)-line%point(1))/line%dirv(1)
                delta = line%point-inputF%detector%vertex
                M=0.0_C_DOUBLE
                cos2theta=COS(inputF%detector%half_apex)**2
                M(1,1) = 1.0_C_DOUBLE-cos2theta
                M(2,2) = -cos2theta
                M(3,3) = -cos2theta
                dM = MATMUL(line%dirv,M)
                deltaM = MATMUL(delta,M)
                c2=DOT_PRODUCT(dM,line%dirv)
                c1=DOT_PRODUCT(dM,delta)
                c0=DOT_PRODUCT(deltaM,delta)
                !c2 = line%dirv(1)**2-cos2theta*DOT_PRODUCT(line%dirv,line%dirv)
                !c1 = line%point(1)*line%dirv(1)+&
                !(cos2theta-1.0_C_DOUBLE)*line%dirv(1)*inputF%detector%vertex(1)-&
                !cos2theta*DOT_PRODUCT(line%point,line%dirv)
                !c0 = line%point(1)**2+line%dirv(1)**2-2.0_C_DOUBLE*&
                !line%dirv(1)*line%point(1)-cos2theta*(DOT_PRODUCT(line%point,line%point)+&
                !line%dirv(1)**2-2.0_C_DOUBLE*line%dirv(1)*line%point(1))
                !c0 =&
                !((line%dirv(1)-line%point(1))**2)*(1.0_C_DOUBLE-cos2theta)-&
                !(line%point(2)**2+line%point(3)**2)*cos2theta
                !WRITE (6,'(A,F14.6)') 'c2:',c2
                !WRITE (6,'(A,F14.6)') 'c1:',c1
                !WRITE (6,'(A,F14.6)') 'c0:',c0
                disc=c1**2-c0*c2
                IF (disc .LT. 0.0_C_DOUBLE) THEN
                        !no intersection with complete cone->
                        !means no intersection with detector possible
                        rv = XMI_NO_INTERSECTION
                        RETURN
                ENDIF
                !n_valid_t == 0 -> none
                !n_valid_t == 1 -> t1 valid only
                !n_valid_t == 2 -> t2 valid only
                !n_valid_t == 3 -> both are valid
                n_valid_t = 0
                !start with t1
                t1=(-c1+SQRT(disc))/c2
                X1=line%point+t1*line%dirv
                IF (DOT_PRODUCT([-1.0_C_DOUBLE,0.0_C_DOUBLE,0.0_C_DOUBLE],&
                (X1-inputF%detector%vertex)).GE.0.0_C_DOUBLE) THEN
                        n_valid_t=1
                ENDIF
                !moving on with t2
                t2=(-c1-SQRT(disc))/c2
                X2=line%point+t2*line%dirv
                IF (DOT_PRODUCT([-1.0_C_DOUBLE,0.0_C_DOUBLE,0.0_C_DOUBLE],&
                (X2-inputF%detector%vertex)).GE.0.0_C_DOUBLE) THEN
                        IF (n_valid_t .EQ. 1) THEN
                                n_valid_t = 3
                        ELSE
                                n_valid_t = 2
                        ENDIF
                ENDIF
                IF (n_valid_t .EQ. 0) THEN
                        !two hits in reflected cone
                        !impossible to hit the detector at this point
                        rv = XMI_NO_INTERSECTION
                        RETURN
                ELSEIF (n_valid_t .EQ. 1) THEN
                        !one hit in actual and one hit in reflected cone
                        !check if hit was between t_begin and t_end and if it
                        !was lower than the collimator height
                        IF(t1 .LE. MAX(t_begin,t_end) .AND. &
                        t1 .GE. MIN(t_begin,t_end) .AND.&
                        X1(1).LE.inputF%geometry%collimator_height) THEN
                                rv = XMI_COLLIMATOR_INTERSECTION
                                RETURN
                        ENDIF
                        !still possible to hit the detector!
                        IF (xmi_intersection_plane_line(plane, line,&
                        intersection) == 0) CALL xmi_exit(1)
                        intersection(1)=0.0_C_DOUBLE
                        IF (norm(intersection) .LE. inputF%detector%&
                        detector_radius .AND. xmi_distance_two_points(&
                        begin_coords_det,intersection) .LE.&
                        xmi_distance_two_points(begin_coords_det,end_coords_det)) THEN
                                IF (DOT_PRODUCT(line%dirv,plane%normv&
                                ) .GE. 0.0_C_DOUBLE) THEN
                                        rv = XMI_DETECTOR_BAD_INTERSECTION
                                        RETURN
                                ENDIF
                                rv = XMI_DETECTOR_INTERSECTION
                                RETURN
                        ENDIF
                        !else
                        rv = XMI_NO_INTERSECTION
                        RETURN
                ELSEIF (n_valid_t .EQ. 2) THEN
                        IF(t2 .LE. MAX(t_begin,t_end) .AND. &
                        t2 .GE. MIN(t_begin,t_end) .AND.&
                        X2(1).LE.inputF%geometry%collimator_height) THEN
                                rv = XMI_COLLIMATOR_INTERSECTION
                                RETURN
                        ENDIF
                        !still possible to hit the detector!
                        IF (xmi_intersection_plane_line(plane, line,&
                        intersection) == 0) CALL xmi_exit(1)
                        intersection(1)=0.0_C_DOUBLE
                        IF (norm(intersection) .LE. inputF%detector%&
                        detector_radius .AND. xmi_distance_two_points(&
                        begin_coords_det,intersection) .LE.&
                        xmi_distance_two_points(begin_coords_det,end_coords_det)) THEN
                                IF (DOT_PRODUCT(line%dirv,plane%normv&
                                ) .GE. 0.0_C_DOUBLE) THEN
                                        rv = XMI_DETECTOR_BAD_INTERSECTION
                                        RETURN
                                ENDIF
                                rv = XMI_DETECTOR_INTERSECTION
                                RETURN
                        ENDIF
                        !else
                        rv = XMI_NO_INTERSECTION
                        RETURN
                ELSEIF (n_valid_t .EQ. 3) THEN
                        !two hits in actual cone
                        !if both occur outside of the collimator
                        !nothing happens...
                        IF(t1 .LE. MAX(t_begin,t_end) .AND. &
                        t1 .GE. MIN(t_begin,t_end) .AND.&
                        X1(1).LE.inputF%geometry%collimator_height) THEN
                                rv = XMI_COLLIMATOR_INTERSECTION
                                RETURN
                        ENDIF
                        IF(t2 .LE. MAX(t_begin,t_end) .AND. &
                        t2 .GE. MIN(t_begin,t_end) .AND.&
                        X2(1).LE.inputF%geometry%collimator_height) THEN
                                rv = XMI_COLLIMATOR_INTERSECTION
                                RETURN
                        ENDIF
                        !no collimator intersection -> detector cannot be hit
                        rv = XMI_NO_INTERSECTION
                ELSE
                        WRITE (error_unit,'(A)') 'should never appear...'
                        CALL xmi_exit(1)
                ENDIF
        ENDIF
        RETURN
ENDFUNCTION xmi_check_detector_intersection

!wrapper around deallocate
SUBROUTINE xmi_deallocate(ptr) BIND(C,NAME='xmi_deallocate')
        IMPLICIT NONE
        TYPE (C_PTR), VALUE :: ptr
        REAL(C_DOUBLE), DIMENSION(:), POINTER :: array

        IF (.NOT. C_ASSOCIATED(ptr)) RETURN

        CALL C_F_POINTER(ptr, array, [1])

        DEALLOCATE(array)

        RETURN
ENDSUBROUTINE xmi_deallocate

!
!
!       initialize xmi_trap_dist_workspace
!
!

FUNCTION xmi_ran_trap_workspace_init(x1, x2, y1, y2, workspace)&
        RESULT(rv)
        IMPLICIT NONE
        REAL (C_DOUBLE), INTENT(IN) :: x1, x2, y1, y2
        TYPE (xmi_ran_trap_workspace), INTENT(OUT) :: workspace
        INTEGER (C_INT) :: rv

        IF (x1 .GE. x2) THEN
                WRITE (error_unit, '(A)') &
                'Error in xmi_trap_dist_workspace_init: x1 >= x2'
                rv = 0_C_INT
                RETURN
        ELSEIF (x1 .EQ. 0.0_C_DOUBLE .AND. x2 .EQ. 0.0_C_DOUBLE) THEN
                WRITE (error_unit, '(A)') &
                'Error in xmi_trap_dist_workspace_init: x1 == x2 == 0'
                rv = 0_C_INT
                RETURN
        ENDIF

        workspace%x1 = x1
        workspace%x2 = x2
        workspace%y1 = y1
        workspace%y2 = y2
        workspace%m = (y2-y1)/(x2-x1)
        workspace%denom = (x2-x1)*(y1-x1*workspace%m) + &
        workspace%m*(x2**2-x1**2)/2.0

        rv = 1_C_INT
        RETURN
ENDFUNCTION xmi_ran_trap_workspace_init

FUNCTION xmi_poly_solve_quadratic(a, b, c, rv1, rv2) RESULT(rv)
        IMPLICIT NONE
        REAL (C_DOUBLE), INTENT(IN) :: a, b, c
        REAL (C_DOUBLE), INTENT(OUT) :: rv1, rv2
        INTEGER (C_INT) :: rv
        REAL (C_DOUBLE) :: delta, rv1_temp, rv2_temp, sqrt_delta

        rv = 0_C_INT

        IF (a .EQ. 0.0_C_DOUBLE) THEN
                IF (b .EQ. 0.0_C_DOUBLE) RETURN
                rv1 = -1.0 * c / b
                rv = 1_C_INT
                RETURN
        ENDIF

        delta = b * b - 4.0_C_DOUBLE * a * c

        IF (delta .LT. 0.0_C_DOUBLE) THEN
                RETURN
        ELSEIF (delta .EQ. 0.0_C_DOUBLE) THEN
                rv1 = -b / 2.0 / a
                rv2 = rv1
                rv = 2_C_INT
        ELSE
                sqrt_delta = SQRT(delta)
                rv1_temp = (-b + sqrt_delta) / 2.0 / a
                rv2_temp = (-b - sqrt_delta) / 2.0 / a
                rv1 = MIN(rv1_temp, rv2_temp)
                rv2 = MAX(rv1_temp, rv2_temp)
                rv = 2_C_INT
        ENDIF

ENDFUNCTION xmi_poly_solve_quadratic

FUNCTION xmi_ran_trap(rng, workspace) RESULT(rv)
        IMPLICIT NONE
        TYPE (xmi_rng), INTENT(IN) :: rng
        TYPE (xmi_ran_trap_workspace) :: workspace
        REAL (C_DOUBLE) :: rv
        REAL (C_DOUBLE) :: rv1, rv2
        REAL (C_DOUBLE) :: a, b, c

        a = workspace%m/2.0_C_DOUBLE
        b = workspace%y1-workspace%x1*workspace%m
        c = -workspace%x1*workspace%y1+workspace%m*workspace%x1**2/2.0_C_DOUBLE&
            -workspace%denom*xmi_rng_uniform(rng)

        IF (xmi_poly_solve_quadratic(a, &
            b, &
            c, &
            rv1, &
            rv2) == 0_C_INT) THEN
                WRITE (error_unit, '(A)') 'Error in xmi_ran_trap:'
                WRITE (error_unit, '(A)') 'xmi_poly_solve_quadratic failure'
                CALL xmi_exit(1)
        ENDIF

        IF (workspace%x1 .LE. rv1 .AND. rv1 .LE. workspace%x2) THEN
                rv = rv1
        ELSEIF (workspace%x1 .LE. rv2 .AND. rv2 .LE. workspace%x2) THEN
                rv = rv2
        ELSE
                WRITE (error_unit, '(A)') 'Error in xmi_ran_trap:'
                WRITE (error_unit, '(A)') 'roots are not within interval'
                CALL xmi_exit(1)
        ENDIF

        RETURN
ENDFUNCTION xmi_ran_trap

FUNCTION xmi_omp_get_max_threads() RESULT(rv)&
BIND(C,NAME='xmi_omp_get_max_threads')
        IMPLICIT NONE
        INTEGER (C_INT) :: rv

        rv = INT(omp_get_max_threads(), KIND=C_INT)
ENDFUNCTION xmi_omp_get_max_threads

FUNCTION xmi_get_qimax(energy, element, shell, theta) RESULT(Qimax)
        IMPLICIT NONE
        REAL (C_DOUBLE), INTENT(IN) :: energy, theta
        INTEGER (C_INT), INTENT(IN) :: element, shell
        REAL (C_DOUBLE) :: Qimax

        REAL (C_DOUBLE) :: Ii, EminIi, costheta

        !WRITE (output_unit, '(A)') 'Entering xmi_get_qimax'

        !check what happens when zero is returned here...
        Ii = EdgeEnergy(element, shell)
        IF (Ii .EQ. 0.0_C_DOUBLE) THEN
                !WRITE (output_unit,'(A)') 'Warning: EdgeEnergy equal to zero in xmi_get_qimax'
                !WRITE (output_unit,'(A,I3,AI2)') 'Element: ',&
                !element,' shell: ',shell
        ELSEIF (energy .LT. Ii) THEN
                Qimax = 0.0_C_DOUBLE
                RETURN
        ENDIF
        EminIi = energy-Ii
        costheta = COS(theta)
        Qimax = 137.0_C_DOUBLE*(EminIi*energy*(1.0_C_DOUBLE-costheta)/MEC2-Ii)
        Qimax = &
        Qimax/SQRT(EminIi**2+energy**2-2.0_C_DOUBLE*EminIi*energy*costheta)

ENDFUNCTION xmi_get_qimax

FUNCTION xmi_get_energy_from_q(init_energy, Q, theta) RESULT(energy)
        IMPLICIT NONE
        REAL (C_DOUBLE), INTENT(IN) :: init_energy, Q, theta
        REAL (C_DOUBLE) :: energy

        REAL (C_DOUBLE) :: a, b, c, d
        REAL (C_DOUBLE) :: aq, bq, cq
        REAL (C_DOUBLE), PARAMETER :: onethreeseven = 137.0_C_DOUBLE
        REAL (C_DOUBLE) :: E1, E2, Q1, Q2
        INTEGER (C_INT) :: rv

        a = init_energy
        b = MEC2
        c = COS(theta)

        IF (ABS(c-1.0_C_DOUBLE) .LT. 1E-8) THEN
                !exception for theta very close to 0
                energy = 0.0
                RETURN
        ENDIF
        IF (ABS(Q) .LT. 1E-4) THEN
                !exception for Q very close to zero
                energy = init_energy/(1.0 + init_energy*(1.0-c)/MEC2)
                RETURN
        ENDIF

        d = 1.0_C_DOUBLE + a/b - a*c/b

        aq = onethreeseven**2*d**2 - Q**2
        bq = -2.0_C_DOUBLE*onethreeseven**2*a*d + 2.0_C_DOUBLE*a*c*Q**2
        cq = onethreeseven**2*a**2 - a**2*Q**2

        rv = xmi_poly_solve_quadratic(aq, bq, cq, E1, E2)

        IF (rv == 0_C_INT) THEN
                !WRITE (error_unit, '(A)') 'Error in xmi_get_energy_from_q:'
                !WRITE (error_unit, '(A)') 'xmi_poly_solve_quadratic failure'
                !WRITE (error_unit, '(A, F14.5)') 'Q:', Q
                !WRITE (error_unit, '(A, F14.5)') 'init_energy:', init_energy
                !WRITE (error_unit, '(A, F14.5)') 'theta:', theta
                !CALL xmi_exit(1)
                energy = 0.0
                RETURN
        ENDIF

        !WRITE (output_unit,'(A, F14.5)') 'Energy1: ', E1
        !WRITE (output_unit,'(A, F14.5)') 'Q1: ', Q1
        !WRITE (output_unit,'(A, F14.5)') 'Energy2: ', E2
        !WRITE (output_unit,'(A, F14.5)') 'Q2: ', Q2


        !in order to know which energy to use
        !calculate the corresponding Qs and see which one matches the input
        Q1 = xmi_get_q_from_energy(init_energy, E1, theta)
        Q2 = xmi_get_q_from_energy(init_energy, E2, theta)

        IF (Q*Q1 .GT. 0.0) THEN
                energy = E1
        ELSEIF (Q*Q2 .GT. 0.0) THEN
                energy = E2
        ELSEIF (ABS(E1-E2) .LT. 1E-10 .OR. ABS(Q1-Q2) .LT. 1E-10) THEN
                !Q is extremely close to zero -> math issues start to emerge
                !should check for floating point exceptions and so
                !anyway, in this case E1 and E2 are identical
                energy = E1
        ELSE
                WRITE (error_unit, '(A)') 'Error in xmi_get_energy_from_q:'
                WRITE (error_unit, '(A)') 'Invalid values for Q1 and Q2'
                WRITE (error_unit, '(A, ES14.5)') 'Q1:', Q1
                WRITE (error_unit, '(A, ES14.5)') 'Q2:', Q2
                WRITE (error_unit, '(A, F14.5)') 'E1:', E1
                WRITE (error_unit, '(A, F14.5)') 'E2:', E2
                WRITE (error_unit, '(A, ES14.5)') 'Q:', Q
                WRITE (error_unit, '(A, F14.5)') 'init_energy:', init_energy
                WRITE (error_unit, '(A, F14.5)') 'theta:', theta
                CALL xmi_exit(1)
        ENDIF

ENDFUNCTION xmi_get_energy_from_q

FUNCTION xmi_get_q_from_energy(old_energy, new_energy, theta) RESULT(Q)
        IMPLICIT NONE
        REAL (C_DOUBLE), INTENT(IN) :: old_energy, new_energy, theta
        REAL (C_DOUBLE) :: Q

        REAL (C_DOUBLE) :: costheta

        costheta = COS(theta)
        Q = 137.0_C_DOUBLE*(new_energy-old_energy+&
        (1.0-costheta)*old_energy*new_energy/MEC2)
        Q = Q/SQRT(new_energy**2+old_energy**2-&
        2.0*old_energy*new_energy*costheta)


ENDFUNCTION xmi_get_q_from_energy

! taken from http://fortranwiki.org/fortran/show/Matrix+inversion
PURE FUNCTION xmi_inverse_matrix(A) RESULT(B)
    implicit none
    !! Performs a direct calculation of the inverse of a 3×3 matrix.
    real (c_double), intent(in) :: A(3,3)   !! Matrix
    real (c_double)             :: B(3,3)   !! Inverse matrix
    real (c_double)             :: detinv

    ! Calculate the inverse determinant of the matrix
    detinv = 1/xmi_determinant_matrix(A)

    ! Calculate the inverse of the matrix
    B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
    B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
    B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
    B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
    B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
    B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
    B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
    B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
    B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
ENDFUNCTION xmi_inverse_matrix

PURE FUNCTION xmi_determinant_matrix(A) RESULT(det)
    implicit none
    real (c_double), intent(in) :: A(3,3)   !! Matrix
    real (c_double)             :: det

    det = A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
          - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
          + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1)
ENDFUNCTION xmi_determinant_matrix
ENDMODULE
