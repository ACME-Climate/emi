module ExternalModelConstants

  implicit none
  private

  ! ID for various external models
  integer, public, parameter :: EM_ID_BeTR                            = 1
  integer, public, parameter :: EM_ID_FATES                           = 2
  integer, public, parameter :: EM_ID_PFLOTRAN                        = 3
  integer, public, parameter :: EM_ID_VSFM                            = 4

  ! IDs for state variables sent from ALM to External Model
  integer, parameter, public :: S_L2E_TSOIL                           = 1
  integer, parameter, public :: S_L2E_H2OSOI_LIQ                      = 2
  integer, parameter, public :: S_L2E_H2OSOI_ICE                      = 3

  ! IDs for states sent from External Model to ALM
  integer, parameter, public :: S_E2L_H2OSOI_LIQ                      = 101
  integer, parameter, public :: S_E2L_H2OSOI_ICE                      = 102
  integer, parameter, public :: S_E2L_SOIL_MATRIC_POTENTIAL           = 103
  integer, parameter, public :: S_E2L_WTD                             = 104
  integer, parameter, public :: S_E2L_VSFM_PROGNOSTIC_SOILP           = 105

  ! IDs for fluxes sent from ALM to External Model
  integer, parameter, public :: F_L2E_INFIL_MASS_FLUX                 = 201
  integer, parameter, public :: F_L2E_VERTICAL_ET_MASS_FLUX           = 202
  integer, parameter, public :: F_L2E_DEW_MASS_FLUX                   = 203
  integer, parameter, public :: F_L2E_SNOW_SUBLIMATION_MASS_FLUX      = 204
  integer, parameter, public :: F_L2E_SNOW_LYR_DISAPPERANCE_MASS_FLUX = 205
  integer, parameter, public :: F_L2E_DRAINAGE_MASS_FLUX              = 206

  ! IDs for fluxes sent from External Model to ALM
  integer, parameter, public :: F_E2L_AQUIFER_RECHARGE                = 301

  ! IDs for filter variables sent from ALM to External Model
  integer, parameter, public :: FILTER_L2E_HYDROLOGYC                 = 401
  integer, parameter, public :: FILTER_L2E_NUM_HYDROLOGYC             = 402

  ! IDs for mesh attributes sent from ALM to External Model
  integer, parameter, public :: MESH_L2E_ZI                           = 501
  integer, parameter, public :: MESH_L2E_AREA                         = 502

end module ExternalModelConstants
