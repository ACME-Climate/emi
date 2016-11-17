module ExternalModelConstants

  implicit none
  private

  integer, parameter, public :: EM_INITIALIZATION_STAGE                          = 1001
  integer, parameter, public :: EM_FATES_SUNFRAC_STAGE                           = 21
  integer, parameter, public :: EM_VSFM_SOIL_HYDRO_STAGE                         = 41

  ! ID for various external models
  integer, public, parameter :: EM_ID_BeTR                                       = 1
  integer, public, parameter :: EM_ID_FATES                                      = 2
  integer, public, parameter :: EM_ID_PFLOTRAN                                   = 3
  integer, public, parameter :: EM_ID_VSFM                                       = 4

  ! IDs for state variables sent from ALM to External Model
  integer, parameter, public :: L2E_STATE_TSOIL                                  = 1
  integer, parameter, public :: L2E_STATE_H2OSOI_LIQ                             = 2
  integer, parameter, public :: L2E_STATE_H2OSOI_ICE                             = 3
  integer, parameter, public :: L2E_STATE_WTD                                    = 4
  integer, parameter, public :: L2E_STATE_VSFM_PROGNOSTIC_SOILP                  = 5

  ! IDs for states sent from External Model to ALM
  integer, parameter, public :: E2L_STATE_H2OSOI_LIQ                             = 101
  integer, parameter, public :: E2L_STATE_H2OSOI_ICE                             = 102
  integer, parameter, public :: E2L_STATE_SOIL_MATRIC_POTENTIAL                  = 103
  integer, parameter, public :: E2L_STATE_WTD                                    = 104
  integer, parameter, public :: E2L_STATE_VSFM_PROGNOSTIC_SOILP                  = 105
  integer, parameter, public :: E2L_STATE_FSUN                                   = 106
  integer, parameter, public :: E2L_STATE_LAISUN                                 = 107
  integer, parameter, public :: E2L_STATE_LAISHA                                 = 108

  ! IDs for fluxes sent from ALM to External Model
  integer, parameter, public :: L2E_FLUX_INFIL_MASS_FLUX                         = 201
  integer, parameter, public :: L2E_FLUX_VERTICAL_ET_MASS_FLUX                   = 202
  integer, parameter, public :: L2E_FLUX_DEW_MASS_FLUX                           = 203
  integer, parameter, public :: L2E_FLUX_SNOW_SUBLIMATION_MASS_FLUX              = 204
  integer, parameter, public :: L2E_FLUX_SNOW_LYR_DISAPPERANCE_MASS_FLUX         = 205
  integer, parameter, public :: L2E_FLUX_RESTART_SNOW_LYR_DISAPPERANCE_MASS_FLUX = 206
  integer, parameter, public :: L2E_FLUX_DRAINAGE_MASS_FLUX                      = 207
  integer, parameter, public :: L2E_FLUX_SOLAR_DIRECT_RADDIATION                 = 208
  integer, parameter, public :: L2E_FLUX_SOLAR_DIFFUSE_RADDIATION                = 209

  ! IDs for fluxes sent from External Model to ALM
  integer, parameter, public :: E2L_FLUX_AQUIFER_RECHARGE                        = 301
  integer, parameter, public :: E2L_FLUX_SNOW_LYR_DISAPPERANCE_MASS_FLUX         = 305

  ! IDs for filter variables sent from ALM to External Model
  integer, parameter, public :: L2E_FILTER_HYDROLOGYC                            = 401
  integer, parameter, public :: L2E_FILTER_NUM_HYDROLOGYC                        = 402

  ! IDs for column-level attributes sent from ALM to External Model
  integer, parameter, public :: L2E_COLUMN_ACTIVE                                = 501
  integer, parameter, public :: L2E_COLUMN_TYPE                                  = 502
  integer, parameter, public :: L2E_COLUMN_LANDUNIT_INDEX                        = 503
  integer, parameter, public :: L2E_COLUMN_ZI                                    = 504
  integer, parameter, public :: L2E_COLUMN_DZ                                    = 505
  integer, parameter, public :: L2E_COLUMN_Z                                     = 506
  integer, parameter, public :: L2E_COLUMN_AREA                                  = 507
  integer, parameter, public :: L2E_COLUMN_GRIDCELL_INDEX                        = 508
  integer, parameter, public :: L2E_COLUMN_PATCH_INDEX                           = 509

  ! IDs for landunit-level attributes sent from ALM to External Model
  integer, parameter, public :: L2E_LANDUNIT_TYPE                                = 601
  integer, parameter, public :: L2E_LANDUNIT_LAKEPOINT                           = 602
  integer, parameter, public :: L2E_LANDUNIT_URBANPOINT                          = 603

  ! IDs for parameters sent from ALM to External Model
  integer, parameter, public :: L2E_PARAMETER_WATSATC                            = 701
  integer, parameter, public :: L2E_PARAMETER_HKSATC                             = 702
  integer, parameter, public :: L2E_PARAMETER_BSWC                               = 703
  integer, parameter, public :: L2E_PARAMETER_SUCSATC                            = 704
  integer, parameter, public :: L2E_PARAMETER_EFFPOROSITYC                       = 705

end module ExternalModelConstants
