module ExternalModelConstants

  implicit none
  private

  integer, parameter, public :: EM_INITIALIZATION_STAGE = 1001

  ! ID for various external models
  integer, public, parameter :: EM_ID_BeTR                                    = 1
  integer, public, parameter :: EM_ID_FATES                                   = 2
  integer, public, parameter :: EM_ID_PFLOTRAN                                = 3
  integer, public, parameter :: EM_ID_VSFM                                    = 4

  ! IDs for state variables sent from ALM to External Model
  integer, parameter, public :: S_L2E_TSOIL                                   = 1
  integer, parameter, public :: S_L2E_H2OSOI_LIQ                              = 2
  integer, parameter, public :: S_L2E_H2OSOI_ICE                              = 3
  integer, parameter, public :: S_L2E_WTD                                     = 4
  integer, parameter, public :: S_L2E_VSFM_PROGNOSTIC_SOILP                   = 5

  ! IDs for states sent from External Model to ALM
  integer, parameter, public :: S_E2L_H2OSOI_LIQ                              = 101
  integer, parameter, public :: S_E2L_H2OSOI_ICE                              = 102
  integer, parameter, public :: S_E2L_SOIL_MATRIC_POTENTIAL                   = 103
  integer, parameter, public :: S_E2L_WTD                                     = 104
  integer, parameter, public :: S_E2L_VSFM_PROGNOSTIC_SOILP                   = 105

  ! IDs for fluxes sent from ALM to External Model
  integer, parameter, public :: F_L2E_INFIL_MASS_FLUX                         = 201
  integer, parameter, public :: F_L2E_VERTICAL_ET_MASS_FLUX                   = 202
  integer, parameter, public :: F_L2E_DEW_MASS_FLUX                           = 203
  integer, parameter, public :: F_L2E_SNOW_SUBLIMATION_MASS_FLUX              = 204
  integer, parameter, public :: F_L2E_SNOW_LYR_DISAPPERANCE_MASS_FLUX         = 205
  integer, parameter, public :: F_L2E_RESTART_SNOW_LYR_DISAPPERANCE_MASS_FLUX = 206
  integer, parameter, public :: F_L2E_DRAINAGE_MASS_FLUX                      = 207

  ! IDs for fluxes sent from External Model to ALM
  integer, parameter, public :: F_E2L_AQUIFER_RECHARGE                        = 301
  integer, parameter, public :: F_E2L_SNOW_LYR_DISAPPERANCE_MASS_FLUX         = 305

  ! IDs for filter variables sent from ALM to External Model
  integer, parameter, public :: FILTER_L2E_HYDROLOGYC                         = 401
  integer, parameter, public :: FILTER_L2E_NUM_HYDROLOGYC                     = 402

  ! IDs for column-level attributes sent from ALM to External Model
  integer, parameter, public :: COLUMN_L2E_ACTIVE                             = 501
  integer, parameter, public :: COLUMN_L2E_TYPE                               = 502
  integer, parameter, public :: COLUMN_L2E_LANDUNIT_INDEX                     = 503
  integer, parameter, public :: COLUMN_L2E_ZI                                 = 504
  integer, parameter, public :: COLUMN_L2E_DZ                                 = 505
  integer, parameter, public :: COLUMN_L2E_Z                                  = 506
  integer, parameter, public :: COLUMN_L2E_AREA                               = 507

  ! IDs for landunit-level attributes sent from ALM to External Model
  integer, parameter, public :: LANDUNIT_L2E_TYPE                             = 601
  integer, parameter, public :: LANDUNIT_L2E_LAKEPOINT                        = 602
  integer, parameter, public :: LANDUNIT_L2E_URBANPOINT                       = 603

  ! IDs for parameters sent from ALM to External Model
  integer, parameter, public :: PARAMETER_L2E_WATSATC                         = 701
  integer, parameter, public :: PARAMETER_L2E_HKSATC                          = 702
  integer, parameter, public :: PARAMETER_L2E_BSWC                            = 703
  integer, parameter, public :: PARAMETER_L2E_SUCSATC                         = 704
  integer, parameter, public :: PARAMETER_L2E_EFFPOROSITYC                    = 705

end module ExternalModelConstants
