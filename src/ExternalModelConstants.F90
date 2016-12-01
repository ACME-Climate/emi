module ExternalModelConstants

  implicit none
  private

  integer, parameter, public :: EM_INITIALIZATION_STAGE                          = 1001
  integer, parameter, public :: EM_BETR_BEGIN_MASS_BALANCE_STAGE                 = 11
  integer, parameter, public :: EM_BETR_PRE_DIAG_WATER_FLUX_STAGE                = 12
  integer, parameter, public :: EM_FATES_SUNFRAC_STAGE                           = 21
  integer, parameter, public :: EM_VSFM_SOIL_HYDRO_STAGE                         = 41
  integer, parameter, public :: EM_PTM_TBASED_SOLVE_STAGE                        = 51

  ! ID for various external models
  integer, public, parameter :: EM_ID_BETR                                       = 1
  integer, public, parameter :: EM_ID_FATES                                      = 2
  integer, public, parameter :: EM_ID_PFLOTRAN                                   = 3
  integer, public, parameter :: EM_ID_VSFM                                       = 4
  integer, public, parameter :: EM_ID_PTM                                        = 5

  ! IDs for state variables sent from ALM to External Model
  integer, parameter, public :: L2E_STATE_TSOIL_NLEVGRND                         = 1
  integer, parameter, public :: L2E_STATE_H2OSOI_LIQ_NLEVGRND                    = 2
  integer, parameter, public :: L2E_STATE_H2OSOI_ICE_NLEVGRND                    = 3
  integer, parameter, public :: L2E_STATE_WTD                                    = 4
  integer, parameter, public :: L2E_STATE_VSFM_PROGNOSTIC_SOILP                  = 5
  integer, parameter, public :: L2E_STATE_FRAC_H2OSFC                            = 6
  integer, parameter, public :: L2E_STATE_FRAC_INUNDATED                         = 7
  integer, parameter, public :: L2E_STATE_H2OSOI_LIQ_VOL_NLEVSOI                 = 8
  integer, parameter, public :: L2E_STATE_H2OSOI_ICE_VOL_NLEVSOI                 = 9
  integer, parameter, public :: L2E_STATE_H2OSOI_VOL_NLEVSOI                     = 10
  integer, parameter, public :: L2E_STATE_AIR_VOL_NLEVSOI                        = 11
  integer, parameter, public :: L2E_STATE_RHO_VAP_NLEVSOI                        = 12
  integer, parameter, public :: L2E_STATE_RHVAP_SOI_NLEVSOI                      = 13
  integer, parameter, public :: L2E_STATE_SOIL_MATRIC_POTENTIAL_NLEVSOI          = 14
  integer, parameter, public :: L2E_STATE_H2OSOI_LIQ_NLEVSOI                     = 15
  integer, parameter, public :: L2E_STATE_H2OSOI_ICE_NLEVSOI                     = 16
  integer, parameter, public :: L2E_STATE_TSNOW                                  = 17
  integer, parameter, public :: L2E_STATE_TH2OSFC                                = 18
  integer, parameter, public :: L2E_STATE_H2OSOI_LIQ_NLEVSNOW                    = 19
  integer, parameter, public :: L2E_STATE_H2OSOI_ICE_NLEVSNOW                    = 20
  integer, parameter, public :: L2E_STATE_H2OSNOW                                = 21
  integer, parameter, public :: L2E_STATE_H2OSFC                                 = 22
  integer, parameter, public :: L2E_STATE_FRAC_SNOW_EFFECTIVE                    = 23

  ! IDs for states sent from External Model to ALM
  integer, parameter, public :: E2L_STATE_H2OSOI_LIQ                             = 101
  integer, parameter, public :: E2L_STATE_H2OSOI_ICE                             = 102
  integer, parameter, public :: E2L_STATE_SOIL_MATRIC_POTENTIAL                  = 103
  integer, parameter, public :: E2L_STATE_WTD                                    = 104
  integer, parameter, public :: E2L_STATE_VSFM_PROGNOSTIC_SOILP                  = 105
  integer, parameter, public :: E2L_STATE_FSUN                                   = 106
  integer, parameter, public :: E2L_STATE_LAISUN                                 = 107
  integer, parameter, public :: E2L_STATE_LAISHA                                 = 108
  integer, parameter, public :: E2L_STATE_TSOIL_NLEVGRND                         = 109
  integer, parameter, public :: E2L_STATE_TSNOW_NLEVSNOW                         = 110
  integer, parameter, public :: E2L_STATE_TH2OSFC                                = 111

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
  integer, parameter, public :: L2E_FLUX_ABSORBED_SOLAR_RADIATION                = 210
  integer, parameter, public :: L2E_FLUX_SOIL_HEAT_FLUX                          = 211
  integer, parameter, public :: L2E_FLUX_SNOW_HEAT_FLUX                          = 212
  integer, parameter, public :: L2E_FLUX_H2OSFC_HEAT_FLUX                        = 213
  integer, parameter, public :: L2E_FLUX_DERIVATIVE_OF_HEAT_FLUX                 = 214

  ! IDs for fluxes sent from External Model to ALM
  integer, parameter, public :: E2L_FLUX_AQUIFER_RECHARGE                        = 301
  integer, parameter, public :: E2L_FLUX_SNOW_LYR_DISAPPERANCE_MASS_FLUX         = 305

  ! IDs for filter variables sent from ALM to External Model
  integer, parameter, public :: L2E_FILTER_HYDROLOGYC                            = 401
  integer, parameter, public :: L2E_FILTER_NUM_HYDROLOGYC                        = 402
  integer, parameter, public :: L2E_FILTER_NOLAKEC                               = 403
  integer, parameter, public :: L2E_FILTER_NUM_NOLAKEC                           = 404
  integer, parameter, public :: L2E_FILTER_NOLAKEC_AND_NOURBANC                  = 405
  integer, parameter, public :: L2E_FILTER_NUM_NOLAKEC_AND_NOURBANC              = 406

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
  integer, parameter, public :: L2E_COLUMN_NUM_SNOW_LAYERS                       = 510
  integer, parameter, public :: L2E_COLUMN_ZI_SNOW_AND_SOIL                      = 511
  integer, parameter, public :: L2E_COLUMN_DZ_SNOW_AND_SOIL                      = 512
  integer, parameter, public :: L2E_COLUMN_Z_SNOW_AND_SOIL                       = 513

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
  integer, parameter, public :: L2E_PARAMETER_CSOL                               = 706
  integer, parameter, public :: L2E_PARAMETER_TKMG                               = 707
  integer, parameter, public :: L2E_PARAMETER_TKDRY                              = 708

end module ExternalModelConstants
