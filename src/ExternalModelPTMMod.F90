module ExternalModelPTMMod

#ifdef USE_PETSC_LIB

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! This module provides
  !
  use abortutils                   , only : endrun
  use shr_kind_mod                 , only : r8 => shr_kind_r8
  use shr_log_mod                  , only : errMsg => shr_log_errMsg
  use ExternalModelInterfaceDataMod, only : emi_data_list, emi_data
  use mpp_varctl                   , only : iulog
  use MultiPhysicsProbThermal      , only : thermal_mpp
  !
  implicit none
  !
  integer :: index_l2e_col_num_snow_lyrs
  integer :: index_l2e_col_zi
  integer :: index_l2e_col_dz
  integer :: index_l2e_col_z
  integer :: index_l2e_col_active
  integer :: index_l2e_col_landunit_index

  integer :: index_l2e_landunit_lakepoint
  integer :: index_l2e_landunit_urbanpoint

  integer :: index_l2e_filter_nolakec_and_nourbanc
  integer :: index_l2e_filter_num_nolakec_and_nourbanc

  integer :: index_l2e_state_frac_snow_eff
  integer :: index_l2e_state_frac_h2osfc
  integer :: index_l2e_state_h2osno
  integer :: index_l2e_state_h2osfc
  integer :: index_l2e_state_h2osoi_liq_nlevgrnd
  integer :: index_l2e_state_h2osoi_ice_nlevgrnd
  integer :: index_l2e_state_h2osoi_liq_nlevsnow
  integer :: index_l2e_state_h2osoi_ice_nlevsnow

  integer :: index_l2e_state_temperature_soil_nlevgrnd
  integer :: index_l2e_state_temperature_snow
  integer :: index_l2e_state_temperature_h2osfc

  integer :: index_l2e_flux_hs_soil
  integer :: index_l2e_flux_hs_top_snow
  integer :: index_l2e_flux_hs_h2osfc
  integer :: index_l2e_flux_dhsdT
  integer :: index_l2e_flux_sabg_lyr

  integer :: index_e2l_state_temperature_soil_nlevgrnd
  integer :: index_e2l_state_temperature_snow
  integer :: index_e2l_state_temperature_h2osfc


  public :: EM_PTM_Populate_L2E_List, &
            EM_PTM_Populate_E2L_List, &
            EM_PTM_Solve

contains

  !------------------------------------------------------------------------
  subroutine EM_PTM_Populate_L2E_List(l2e_list)
    !
    ! !DESCRIPTION:
    ! Create a list of all variables needed by PETSc-based Thermal Model (PTM) from ALM
    !
    ! !USES:
    use ExternalModelConstants    , only : EM_PTM_TBASED_SOLVE_STAGE
    use ExternalModelConstants    , only : L2E_COLUMN_NUM_SNOW_LAYERS
    use ExternalModelConstants    , only : L2E_COLUMN_ZI
    use ExternalModelConstants    , only : L2E_COLUMN_DZ
    use ExternalModelConstants    , only : L2E_COLUMN_Z
    use ExternalModelConstants    , only : L2E_COLUMN_ACTIVE
    use ExternalModelConstants    , only : L2E_COLUMN_LANDUNIT_INDEX
    use ExternalModelConstants    , only : L2E_STATE_FRAC_SNOW_EFFECTIVE
    use ExternalModelConstants    , only : L2E_STATE_FRAC_H2OSFC
    use ExternalModelConstants    , only : L2E_STATE_H2OSNOW
    use ExternalModelConstants    , only : L2E_STATE_H2OSFC
    use ExternalModelConstants    , only : L2E_STATE_H2OSOI_LIQ_NLEVGRND
    use ExternalModelConstants    , only : L2E_STATE_H2OSOI_ICE_NLEVGRND
    use ExternalModelConstants    , only : L2E_STATE_H2OSOI_LIQ_NLEVSNOW
    use ExternalModelConstants    , only : L2E_STATE_H2OSOI_ICE_NLEVSNOW
    use ExternalModelConstants    , only : L2E_STATE_TSOIL_NLEVGRND
    use ExternalModelConstants    , only : L2E_STATE_TSNOW
    use ExternalModelConstants    , only : L2E_STATE_TH2OSFC
    use ExternalModelConstants    , only : L2E_FLUX_ABSORBED_SOLAR_RADIATION
    use ExternalModelConstants    , only : L2E_FLUX_SOIL_HEAT_FLUX
    use ExternalModelConstants    , only : L2E_FLUX_SNOW_HEAT_FLUX
    use ExternalModelConstants    , only : L2E_FLUX_H2OSFC_HEAT_FLUX
    use ExternalModelConstants    , only : L2E_FLUX_DERIVATIVE_OF_HEAT_FLUX
    use ExternalModelConstants    , only : L2E_LANDUNIT_LAKEPOINT
    use ExternalModelConstants    , only : L2E_LANDUNIT_URBANPOINT
    use ExternalModelConstants    , only : L2E_FILTER_NOLAKEC_AND_NOURBANC
    use ExternalModelConstants    , only : L2E_FILTER_NUM_NOLAKEC_AND_NOURBANC
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list), intent(inout) :: l2e_list
    !
    ! !LOCAL VARIABLES:
    class(emi_data), pointer :: data
    integer        , pointer :: em_stages(:)
    integer                  :: number_em_stages
    integer                  :: count

    count            = 0
    number_em_stages = 1
    allocate(em_stages(number_em_stages))
    em_stages(1) = EM_PTM_TBASED_SOLVE_STAGE

    call l2e_list%AddDataByID(L2E_COLUMN_NUM_SNOW_LAYERS          , number_em_stages, em_stages, index_l2e_col_num_snow_lyrs               )
    call l2e_list%AddDataByID(L2E_COLUMN_ZI                       , number_em_stages, em_stages, index_l2e_col_zi                          )
    call l2e_list%AddDataByID(L2E_COLUMN_DZ                       , number_em_stages, em_stages, index_l2e_col_dz                          )
    call l2e_list%AddDataByID(L2E_COLUMN_Z                        , number_em_stages, em_stages, index_l2e_col_z                           )
    call l2e_list%AddDataByID(L2E_COLUMN_ACTIVE                   , number_em_stages, em_stages, index_l2e_col_active                      )
    call l2e_list%AddDataByID(L2E_COLUMN_LANDUNIT_INDEX           , number_em_stages, em_stages, index_l2e_col_landunit_index              )

    call l2e_list%AddDataByID(L2E_LANDUNIT_LAKEPOINT              , number_em_stages, em_stages, index_l2e_landunit_lakepoint              )
    call l2e_list%AddDataByID(L2E_LANDUNIT_URBANPOINT             , number_em_stages, em_stages, index_l2e_landunit_urbanpoint             )

    call l2e_list%AddDataByID(L2E_FILTER_NOLAKEC_AND_NOURBANC     , number_em_stages, em_stages, index_l2e_filter_nolakec_and_nourbanc     )
    call l2e_list%AddDataByID(L2E_FILTER_NUM_NOLAKEC_AND_NOURBANC , number_em_stages, em_stages, index_l2e_filter_num_nolakec_and_nourbanc )

    call l2e_list%AddDataByID(L2E_STATE_FRAC_SNOW_EFFECTIVE       , number_em_stages, em_stages, index_l2e_state_frac_snow_eff             )
    call l2e_list%AddDataByID(L2E_STATE_FRAC_H2OSFC               , number_em_stages, em_stages, index_l2e_state_frac_h2osfc               )
    call l2e_list%AddDataByID(L2E_STATE_H2OSNOW                   , number_em_stages, em_stages, index_l2e_state_h2osno                    )
    call l2e_list%AddDataByID(L2E_STATE_H2OSFC                    , number_em_stages, em_stages, index_l2e_state_h2osfc                    )
    call l2e_list%AddDataByID(L2E_STATE_H2OSOI_LIQ_NLEVGRND       , number_em_stages, em_stages, index_l2e_state_h2osoi_liq_nlevgrnd       )
    call l2e_list%AddDataByID(L2E_STATE_H2OSOI_ICE_NLEVGRND       , number_em_stages, em_stages, index_l2e_state_h2osoi_ice_nlevgrnd       )
    call l2e_list%AddDataByID(L2E_STATE_H2OSOI_LIQ_NLEVSNOW       , number_em_stages, em_stages, index_l2e_state_h2osoi_liq_nlevsnow       )
    call l2e_list%AddDataByID(L2E_STATE_H2OSOI_ICE_NLEVSNOW       , number_em_stages, em_stages, index_l2e_state_h2osoi_ice_nlevsnow       )

    call l2e_list%AddDataByID(L2E_STATE_TSOIL_NLEVGRND            , number_em_stages, em_stages, index_l2e_state_temperature_soil_nlevgrnd )
    call l2e_list%AddDataByID(L2E_STATE_TSNOW                     , number_em_stages, em_stages, index_l2e_state_temperature_snow          )
    call l2e_list%AddDataByID(L2E_STATE_TH2OSFC                   , number_em_stages, em_stages, index_l2e_state_temperature_h2osfc        )

    call l2e_list%AddDataByID(L2E_FLUX_ABSORBED_SOLAR_RADIATION   , number_em_stages, em_stages, index_l2e_flux_sabg_lyr                   )
    call l2e_list%AddDataByID(L2E_FLUX_SOIL_HEAT_FLUX             , number_em_stages, em_stages, index_l2e_flux_hs_soil                    )
    call l2e_list%AddDataByID(L2E_FLUX_SNOW_HEAT_FLUX             , number_em_stages, em_stages, index_l2e_flux_hs_top_snow                )
    call l2e_list%AddDataByID(L2E_FLUX_H2OSFC_HEAT_FLUX           , number_em_stages, em_stages, index_l2e_flux_hs_h2osfc                  )
    call l2e_list%AddDataByID(L2E_FLUX_DERIVATIVE_OF_HEAT_FLUX    , number_em_stages, em_stages, index_l2e_flux_dhsdT                      )

    deallocate(em_stages)

  end subroutine EM_PTM_Populate_L2E_List

  !------------------------------------------------------------------------
  subroutine EM_PTM_Populate_E2L_List(e2l_list)
    !
    !
    ! !DESCRIPTION:
    ! Create a list of all variables to be returned by PTM from ALM
    !
    ! !USES:
    use ExternalModelConstants    , only : EM_PTM_TBASED_SOLVE_STAGE
    use ExternalModelConstants    , only : E2L_STATE_TSOIL_NLEVGRND
    use ExternalModelConstants    , only : E2L_STATE_TSNOW_NLEVSNOW
    use ExternalModelConstants    , only : E2L_STATE_TH2OSFC
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list) , intent(inout) :: e2l_list
    !
    ! !LOCAL VARIABLES:
    integer              , pointer       :: em_stages(:)
    integer                              :: number_em_stages

    number_em_stages = 1
    allocate(em_stages(number_em_stages))
    em_stages(1) = EM_PTM_TBASED_SOLVE_STAGE

    call e2l_list%AddDataByID(E2L_STATE_TSOIL_NLEVGRND , number_em_stages, em_stages, index_e2l_state_temperature_soil_nlevgrnd )
    call e2l_list%AddDataByID(E2L_STATE_TSNOW_NLEVSNOW , number_em_stages, em_stages, index_e2l_state_temperature_snow          )
    call e2l_list%AddDataByID(E2L_STATE_TH2OSFC        , number_em_stages, em_stages, index_e2l_state_temperature_h2osfc        )

    deallocate(em_stages)

  end subroutine EM_PTM_Populate_E2L_List

    !------------------------------------------------------------------------
  subroutine EM_PTM_Solve(em_stage, dt, nstep, l2e_list, e2l_list)
    !
    ! !DESCRIPTION:
    ! The PTM dirver subroutine
    !
    ! !USES:
    use ExternalModelConstants , only : EM_PTM_TBASED_SOLVE_STAGE
    !
    implicit none
    !
    ! !ARGUMENTS:
    integer              , intent(in)    :: em_stage
    real(r8)             , intent(in)    :: dt
    integer              , intent(in)    :: nstep
    class(emi_data_list) , intent(in)    :: l2e_list
    class(emi_data_list) , intent(inout) :: e2l_list
    !

    select case (em_stage)
    case (EM_PTM_TBASED_SOLVE_STAGE)
       call EM_PTM_TBased_Solve(dt, nstep, l2e_list, e2l_list)
    case default
       write(iulog,*)'EM_PTM_Solve: Unknown em_stage.'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    end select

  end subroutine EM_PTM_Solve

    !------------------------------------------------------------------------
  subroutine EM_PTM_TBased_Solve(dt, nstep, l2e_list, e2l_list)
    !
    ! !DESCRIPTION:
    ! The PETSc-based Thermal Model dirver
    !
    ! !USES:
    use ExternalModelConstants    , only : EM_PTM_TBASED_SOLVE_STAGE
    use mpp_bounds                , only : bounds_proc_begc, bounds_proc_endc
    use mpp_varpar                , only : nlevgrnd, nlevsno
    use mpp_varcon                , only : capr
    use MultiPhysicsProbConstants , only : VAR_BC_SS_CONDITION
    use MultiPhysicsProbConstants , only : VAR_TEMPERATURE
    use MultiPhysicsProbConstants , only : VAR_LIQ_AREAL_DEN
    use MultiPhysicsProbConstants , only : VAR_ICE_AREAL_DEN
    use MultiPhysicsProbConstants , only : VAR_FRAC
    use MultiPhysicsProbConstants , only : VAR_SNOW_WATER
    use MultiPhysicsProbConstants , only : VAR_NUM_SNOW_LYR
    use MultiPhysicsProbConstants , only : VAR_ACTIVE
    use MultiPhysicsProbConstants , only : VAR_DZ
    use MultiPhysicsProbConstants , only : VAR_DIST_UP
    use MultiPhysicsProbConstants , only : VAR_DIST_DN
    use MultiPhysicsProbConstants , only : VAR_TUNING_FACTOR
    use MultiPhysicsProbConstants , only : VAR_BC_SS_CONDITION
    use MultiPhysicsProbConstants , only : VAR_DHS_DT
    use MultiPhysicsProbConstants , only : AUXVAR_INTERNAL
    use MultiPhysicsProbConstants , only : AUXVAR_BC
    use MultiPhysicsProbConstants , only : AUXVAR_SS
    !
    implicit none
    !
#include "finclude/petscsys.h"
    !
    ! !ARGUMENTS:
    real(r8)             , intent(in)    :: dt
    integer              , intent(in)    :: nstep
    class(emi_data_list) , intent(in)    :: l2e_list
    class(emi_data_list) , intent(inout) :: e2l_list
    !
    ! !LOCAL VARIABLES:
    integer                              :: j,c,l,idx
    integer                              :: offset
    integer                              :: fc
    integer                              :: soe_auxvar_id
    integer                              :: begc, endc

    ! internal auxvars
    real(r8) , pointer                   :: temperature_1d         (:)
    real(r8) , pointer                   :: liq_areal_den_1d       (:)
    real(r8) , pointer                   :: ice_areal_den_1d       (:)
    real(r8) , pointer                   :: snow_water_1d          (:)
    real(r8) , pointer                   :: dz_1d                  (:)
    real(r8) , pointer                   :: dist_up_1d             (:)
    real(r8) , pointer                   :: dist_dn_1d             (:)
    real(r8) , pointer                   :: frac_1d                (:)
    integer  , pointer                   :: num_snow_layer_1d      (:)
    logical  , pointer                   :: is_active_1d           (:)

    ! boundary auxvars
    real(r8) , pointer                   :: hs_snow_1d             (:)
    real(r8) , pointer                   :: hs_sh2o_1d             (:)
    real(r8) , pointer                   :: hs_soil_1d             (:)
    real(r8) , pointer                   :: dhsdT_snow_1d          (:)
    real(r8) , pointer                   :: dhsdT_sh2o_1d          (:)
    real(r8) , pointer                   :: dhsdT_soil_1d          (:)
    real(r8) , pointer                   :: frac_soil_1d           (:)

    ! source sink
    real(r8) , pointer                   :: sabg_snow_1d           (:)
    real(r8) , pointer                   :: sabg_soil_1d           (:)

    real(r8) , pointer                   :: tsurf_tuning_factor_1d (:)

    real(r8) , pointer                   :: l2e_frac_snow_eff(:)
    real(r8) , pointer                   :: l2e_frac_h2osfc(:)
    real(r8) , pointer                   :: l2e_h2osno(:)
    real(r8) , pointer                   :: l2e_h2osfc(:)
    real(r8) , pointer                   :: l2e_th2osfc(:)
    real(r8) , pointer                   :: l2e_hs_soil(:)
    real(r8) , pointer                   :: l2e_hs_top_snow(:)
    real(r8) , pointer                   :: l2e_hs_h2osfc(:)
    real(r8) , pointer                   :: l2e_dhsdT(:)

    real(r8) , pointer                   :: l2e_h2osoi_liq_nlevgrnd(:,:)
    real(r8) , pointer                   :: l2e_h2osoi_ice_nlevgrnd(:,:)
    real(r8) , pointer                   :: l2e_h2osoi_liq_nlevsnow(:,:)
    real(r8) , pointer                   :: l2e_h2osoi_ice_nlevsnow(:,:)
    real(r8) , pointer                   :: l2e_tsoil(:,:)
    real(r8) , pointer                   :: l2e_tsnow(:,:)
    real(r8) , pointer                   :: l2e_sabg_lyr(:,:)

    real(r8) , pointer                   :: e2l_th2osfc(:)
    real(r8) , pointer                   :: e2l_tsoil(:,:)
    real(r8) , pointer                   :: e2l_tsnow(:,:)

    real(r8) , pointer                   :: col_dz(:,:)
    real(r8) , pointer                   :: col_zi(:,:)
    real(r8) , pointer                   :: col_z(:,:)
    integer  , pointer                   :: col_active(:)
    integer  , pointer                   :: col_type(:)
    integer  , pointer                   :: col_landunit(:)
    integer  , pointer                   :: col_snl(:)

    integer  , pointer                   :: lun_lakpoi(:)
    integer  , pointer                   :: lun_urbpoi(:)

    integer  , pointer                   :: l2e_filter(:)
    integer                              :: l2e_num_filter

    PetscErrorCode                       :: ierr
    PetscBool                            :: converged
    PetscInt                             :: converged_reason

    begc = bounds_proc_begc
    endc = bounds_proc_endc

    allocate(temperature_1d         ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))
    allocate(liq_areal_den_1d       ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))
    allocate(ice_areal_den_1d       ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))
    allocate(snow_water_1d          ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))
    allocate(dz_1d                  ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))
    allocate(dist_up_1d             ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))
    allocate(dist_dn_1d             ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))
    allocate(frac_1d                ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))

    allocate(num_snow_layer_1d      ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))
    allocate(is_active_1d           ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))

    allocate(hs_snow_1d             ((endc-begc+1                      )))
    allocate(hs_sh2o_1d             ((endc-begc+1                      )))
    allocate(hs_soil_1d             ((endc-begc+1                      )))
    allocate(dhsdT_snow_1d          ((endc-begc+1                      )))
    allocate(dhsdT_sh2o_1d          ((endc-begc+1                      )))
    allocate(dhsdT_soil_1d          ((endc-begc+1                      )))
    allocate(frac_soil_1d           ((endc-begc+1                      )))

    allocate(sabg_snow_1d           ((endc-begc+1)*nlevsno ))
    allocate(sabg_soil_1d           ((endc-begc+1)*nlevgrnd))

    allocate(tsurf_tuning_factor_1d ((endc-begc+1)*(nlevgrnd+nlevsno+1 )))

    call l2e_list%GetPointerToReal1D(index_l2e_state_frac_snow_eff            , l2e_frac_snow_eff)
    call l2e_list%GetPointerToReal1D(index_l2e_state_frac_h2osfc              , l2e_frac_h2osfc)
    call l2e_list%GetPointerToReal1D(index_l2e_state_h2osno                   , l2e_h2osno)
    call l2e_list%GetPointerToReal1D(index_l2e_state_h2osfc                   , l2e_h2osfc)
    call l2e_list%GetPointerToReal1D(index_l2e_state_temperature_h2osfc       , l2e_th2osfc)
    call l2e_list%GetPointerToReal1D(index_l2e_flux_hs_soil                   , l2e_hs_soil)
    call l2e_list%GetPointerToReal1D(index_l2e_flux_hs_top_snow               , l2e_hs_top_snow)
    call l2e_list%GetPointerToReal1D(index_l2e_flux_hs_h2osfc                 , l2e_hs_h2osfc)
    call l2e_list%GetPointerToReal1D(index_l2e_flux_dhsdT                     , l2e_dhsdT)

    call l2e_list%GetPointerToReal2D(index_l2e_state_h2osoi_liq_nlevgrnd      , l2e_h2osoi_liq_nlevgrnd)
    call l2e_list%GetPointerToReal2D(index_l2e_state_h2osoi_ice_nlevgrnd      , l2e_h2osoi_ice_nlevgrnd)
    call l2e_list%GetPointerToReal2D(index_l2e_state_h2osoi_liq_nlevsnow      , l2e_h2osoi_liq_nlevsnow)
    call l2e_list%GetPointerToReal2D(index_l2e_state_h2osoi_ice_nlevsnow      , l2e_h2osoi_ice_nlevsnow)
    call l2e_list%GetPointerToReal2D(index_l2e_state_temperature_soil_nlevgrnd, l2e_tsoil)
    call l2e_list%GetPointerToReal2D(index_l2e_state_temperature_snow         , l2e_tsnow)
    call l2e_list%GetPointerToReal2D(index_l2e_flux_sabg_lyr                  , l2e_sabg_lyr)

    call l2e_list%GetPointerToReal2D(index_l2e_col_zi             , col_zi           )
    call l2e_list%GetPointerToReal2D(index_l2e_col_dz             , col_dz           )
    call l2e_list%GetPointerToReal2D(index_l2e_col_z              , col_z            )

    call l2e_list%GetPointerToInt1D(index_l2e_col_num_snow_lyrs    , col_snl   )
    call l2e_list%GetPointerToInt1D(index_l2e_col_active          , col_active   )
    call l2e_list%GetPointerToInt1D(index_l2e_col_landunit_index  , col_landunit )
    call l2e_list%GetPointerToInt1D(index_l2e_landunit_lakepoint  , lun_lakpoi   )
    call l2e_list%GetPointerToInt1D(index_l2e_landunit_urbanpoint , lun_urbpoi   )

    call l2e_list%GetPointerToInt1D(index_l2e_filter_nolakec_and_nourbanc , l2e_filter )
    call l2e_list%GetIntValue(index_l2e_filter_num_nolakec_and_nourbanc   , l2e_num_filter    )

    call e2l_list%GetPointerToReal2D(index_e2l_state_temperature_soil_nlevgrnd, e2l_tsoil)
    call e2l_list%GetPointerToReal2D(index_e2l_state_temperature_snow         , e2l_tsnow)
    call e2l_list%GetPointerToReal1D(index_e2l_state_temperature_h2osfc       , e2l_th2osfc)

    ! Initialize
    temperature_1d(:)         = 273.15_r8
    liq_areal_den_1d(:)       = 0._r8
    ice_areal_den_1d(:)       = 0._r8
    frac_soil_1d(:)           = 1._r8
    frac_1d(:)                = 1._r8
    num_snow_layer_1d(:)      = 0
    is_active_1d(:)           = .false.
    hs_snow_1d(:)             = 0._r8
    hs_sh2o_1d(:)             = 0._r8
    hs_soil_1d(:)             = 0._r8
    dhsdT_snow_1d(:)          = 0._r8
    dhsdT_sh2o_1d(:)          = 0._r8
    dhsdT_soil_1d(:)          = 0._r8
    sabg_snow_1d(:)           = 0._r8
    sabg_soil_1d(:)           = 0._r8
    snow_water_1d(:)          = 0._r8
    tsurf_tuning_factor_1d(:) = 1._r8
    dz_1d(:)                  = 0._r8
    dist_up_1d(:)             = 0._r8
    dist_dn_1d(:)             = 0._r8

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Save data for snow
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    offset = 0

    do fc = 1, l2e_num_filter
       c = l2e_filter(fc)
       do j = -nlevsno+1, 0

          l = col_landunit(c)

          ! Is this a soil column on which PETSc based thermal solver works?
          if ((col_active(c)==1) .and. (lun_lakpoi(l) == 0) .and. (lun_urbpoi(l) == 0)) then

             if (j >= col_snl(c)+1) then

                ! Index for internal SoE auxvars
                idx = (c-begc)*nlevsno + j + nlevsno + offset

                ! Save data for internal SoE auxvars
                temperature_1d(idx)    = l2e_tsnow(c,j)
                dz_1d(idx)             = col_dz(c,j)
                liq_areal_den_1d(idx)  = l2e_h2osoi_liq_nlevsnow(c,j)
                ice_areal_den_1d(idx)  = l2e_h2osoi_ice_nlevsnow(c,j)
                num_snow_layer_1d(idx) = -col_snl(c)
                is_active_1d(idx)      = .true.
                dist_up_1d(idx)        = col_zi(c,j  ) - col_z(c,j)
                dist_dn_1d(idx)        = col_z(c,j) - col_zi(c,j-1)
                frac_1d(idx)           = l2e_frac_snow_eff(c)

                ! If not the top snow layer, save amount of absorbed solar
                ! radiation
                if (j /= col_snl(c) +  1) then
                   sabg_snow_1d(idx)      = l2e_sabg_lyr(c,j)
                endif

                ! Save follow data only for the top snow layer
                if (j == col_snl(c)+1) then

                   ! Save tuning_factor for internal SoE auxvars
                   tsurf_tuning_factor_1d(idx) = col_dz(c,j) / &
                        (0.5_r8*(col_z(c,j)-col_zi(c,j-1)+capr*(col_z(c,j+1)-col_zi(c,j-1))))

                   ! Index for boundary SoE auxvars
                   idx = (c-begc)+1

                   ! Save data for boundary SoE auxvars
                   hs_snow_1d(idx)    = l2e_hs_top_snow(c)
                   dhsdT_snow_1d(idx) = l2e_dhsdT(c)
                   frac_soil_1d(idx)  = frac_soil_1d(idx) - l2e_frac_snow_eff(c)
                endif

             endif
          endif
       enddo
    enddo

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Save data for h2osfc
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    offset = (endc - begc + 1)*nlevsno ! Number of data for snow

    do fc = 1, l2e_num_filter
       c = l2e_filter(fc)
       l = col_landunit(c)

       if ((col_active(c)==1) .and. (lun_lakpoi(l) == 0) .and. (lun_urbpoi(l) == 0)) then

          if (l2e_frac_h2osfc(c) > 0._r8) then

             ! Index for internal SoE auxvars
             idx = (c-begc) + 1 + offset

             ! Save data for internal SoE auxvars
             temperature_1d(idx) = l2e_th2osfc(c)
             dz_1d(idx)          = 1.0e-3*l2e_h2osfc(c)
             is_active_1d(idx)   = .true.
             frac_1d(idx)        = l2e_frac_h2osfc(c)
             dist_up_1d(idx)     = dz_1d(idx)/2.d0
             dist_dn_1d(idx)     = dz_1d(idx)/2.d0

             ! Index for boundary SoE auxvars
             idx                 = (c-begc) + 1

             ! Save data for boundary SoE auxvars
             frac_soil_1d(idx)   = frac_soil_1d(idx) - l2e_frac_h2osfc(c)
             dhsdT_sh2o_1d(idx)  = l2e_dhsdT(c)
             hs_sh2o_1d(idx)     = l2e_hs_h2osfc(c)
          endif
       endif
    enddo

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Save data for soil
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    offset = (endc - begc + 1)*(nlevsno + 1) ! Number of data for snow + sh2o

    do fc = 1, l2e_num_filter
       c = l2e_filter(fc)
       l = col_landunit(c)

       do j = 1,nlevgrnd

          ! Index for internal SoE auxvars
          idx = (c-begc)*nlevgrnd + j + offset

          if ((col_active(c)==1) .and. (lun_lakpoi(l) == 0) .and. (lun_urbpoi(l) == 0)) then

             ! Save data for internal SoE auxvars
             temperature_1d(idx)    = l2e_tsoil(c,j)
             dz_1d(idx)             = col_dz(c,j)
             is_active_1d(idx)      = .true.
             liq_areal_den_1d(idx)  = l2e_h2osoi_liq_nlevgrnd(c,j)
             ice_areal_den_1d(idx)  = l2e_h2osoi_ice_nlevgrnd(c,j)
             frac_1d(idx)           = 1.0_r8
             dist_up_1d(idx)        = col_zi(c,j) - col_z(c,j)
             dist_dn_1d(idx)        = col_zi(c,j) - col_z(c,j)

             ! Is this the top soil layer?
             if (j == 1) then

                dz_1d(idx)             = col_z(c,j)*2.d0
                num_snow_layer_1d(idx) = -col_snl(c)

                if (col_snl(c) /= 0) then
                   ! Save data for boundary SoE auxvars
                   sabg_soil_1d((c-begc)*nlevgrnd + j) = l2e_frac_snow_eff(c)*l2e_sabg_lyr(c,j)
                   ! Save data for internal SoE auxvars
                   snow_water_1d(idx)                         = l2e_h2osno(c)
                else
                   ! Save data for internal SoE auxvars
                   tsurf_tuning_factor_1d(idx) = col_dz(c,j) / &
                        (0.5_r8*(col_z(c,j)-col_zi(c,j-1)+capr*(col_z(c,j+1)-col_zi(c,j-1))))
                endif

                ! Save data for boundary SoE auxvars
                hs_soil_1d(   (c-begc) + 1) = l2e_hs_soil(c)
                dhsdT_soil_1d((c-begc) + 1) = l2e_dhsdT(c)

             endif
          endif
       enddo
    enddo

    ! Set temperature
    call thermal_mpp%sysofeqns%SetSolnPrevCLM(temperature_1d)

    ! Set h2soi_liq
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_INTERNAL, &
         VAR_LIQ_AREAL_DEN, soe_auxvar_id, liq_areal_den_1d)

    ! Set h2osi_ice
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_INTERNAL, &
         VAR_ICE_AREAL_DEN, soe_auxvar_id, ice_areal_den_1d)

    ! Set snow water
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_INTERNAL, &
         VAR_SNOW_WATER, soe_auxvar_id, snow_water_1d)

    ! Set dz
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_INTERNAL, &
         VAR_DZ, soe_auxvar_id, dz_1d)

    ! Set dist_up
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_INTERNAL, &
         VAR_DIST_UP, soe_auxvar_id, dist_up_1d)

    ! Set dist_dn
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_INTERNAL, &
         VAR_DIST_DN, soe_auxvar_id, dist_dn_1d)

    ! Set number of snow layers
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetIDataFromCLM(AUXVAR_INTERNAL, &
         VAR_NUM_SNOW_LYR, soe_auxvar_id, num_snow_layer_1d)

    ! Set if cell is active
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetBDataFromCLM(AUXVAR_INTERNAL, &
         VAR_ACTIVE, soe_auxvar_id, is_active_1d)

    ! Set tuning factor
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_INTERNAL, &
         VAR_TUNING_FACTOR, soe_auxvar_id, tsurf_tuning_factor_1d)

    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_INTERNAL, &
         VAR_FRAC, soe_auxvar_id, frac_1d)

    !
    ! Set heat flux for:
    !

    ! 1) top snow layer
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_BC, &
         VAR_BC_SS_CONDITION, soe_auxvar_id, hs_snow_1d)

    ! 2) top standing water layer
    soe_auxvar_id = 2;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_BC, &
         VAR_BC_SS_CONDITION, soe_auxvar_id, hs_sh2o_1d)

    ! 3) soil
    soe_auxvar_id = 3;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_BC, &
         VAR_BC_SS_CONDITION, soe_auxvar_id, hs_soil_1d)

    !
    ! Set derivative of heat flux w.r.t temperature for:
    !

    ! 1) top snow layer
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_BC, &
         VAR_DHS_DT, soe_auxvar_id, dhsdT_snow_1d)

    ! 2) top standing water layer
    soe_auxvar_id = 2;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_BC, &
         VAR_DHS_DT, soe_auxvar_id, dhsdT_sh2o_1d)

    ! 3) soil
    soe_auxvar_id = 3;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_BC, &
         VAR_DHS_DT, soe_auxvar_id, dhsdT_soil_1d)

    !
    ! Set fraction of soil not covered by snow and standing water
    !
    soe_auxvar_id = 3;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_BC, &
         VAR_FRAC, soe_auxvar_id, frac_soil_1d)


    ! Set absorbed solar radiation
    soe_auxvar_id = 1;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_SS, &
         VAR_BC_SS_CONDITION, soe_auxvar_id, sabg_snow_1d)

    ! Set absorbed solar radiation
    soe_auxvar_id = 2;
    call thermal_mpp%sysofeqns%SetRDataFromCLM(AUXVAR_SS, &
         VAR_BC_SS_CONDITION, soe_auxvar_id, sabg_soil_1d)


    ! Preform Pre-StepDT operations
    call thermal_mpp%sysofeqns%PreStepDT()

    ! Solve
    call thermal_mpp%sysofeqns%StepDT(dt, nstep, &
         converged, converged_reason, ierr); CHKERRQ(ierr)

    ! Did the model converge
    if (.not. converged) then
       call endrun(msg=' ERROR: PETSc thermal model failed to converge '//&
            errMsg(__FILE__, __LINE__))
    endif

    ! Get the updated soil tempreature
    call thermal_mpp%sysofeqns%GetSoln(temperature_1d)

    ! Put temperature back in ALM structure for snow
    offset = 0

    do fc = 1, l2e_num_filter
       c = l2e_filter(fc)

       do j = -nlevsno+1, 0
          idx = (c-begc)*nlevsno + j + nlevsno + offset
          l = col_landunit(c)
          if ((col_active(c)==1) .and. (lun_lakpoi(l) == 0) .and. (lun_urbpoi(l) == 0)) then
             if (j >= col_snl(c)+1) then
                e2l_tsnow(c,j-1) = temperature_1d(idx)
             endif
          endif
       enddo
    enddo

    ! Put temperature back in ALM structure for soil
    offset = (endc - begc + 1)*(nlevsno + 1)
    do fc = 1, l2e_num_filter
       c = l2e_filter(fc)
       do j = 1,nlevgrnd
          idx = (c-begc)*nlevgrnd + j + offset
          l = col_landunit(c)
          if ((col_active(c)==1) .and. (lun_lakpoi(l) == 0) .and. (lun_urbpoi(l) == 0)) then
             e2l_tsoil(c,j) = temperature_1d(idx)
          endif
       enddo
    enddo

    ! Put temperature back in ALM structure for standing water
    ! NOTE: Soil temperature needs to be updated first
    offset = (endc - begc + 1)*nlevsno
    do fc = 1, l2e_num_filter
       c = l2e_filter(fc)
       idx = (c-begc) + 1 + offset
       l = col_landunit(c)
       if ((col_active(c)==1) .and. (lun_lakpoi(l) == 0) .and. (lun_urbpoi(l) == 0)) then
          if (l2e_frac_h2osfc(c) > 0._r8) then
             e2l_th2osfc(c) = temperature_1d(idx)
          else
             e2l_th2osfc(c) = e2l_tsoil(c,1)
          endif
       endif
    enddo

    ! Free up memory
    deallocate (temperature_1d         )
    deallocate (liq_areal_den_1d       )
    deallocate (ice_areal_den_1d       )
    deallocate (snow_water_1d          )
    deallocate (dz_1d                  )
    deallocate (dist_up_1d             )
    deallocate (dist_dn_1d             )
    deallocate (frac_1d                )
    deallocate (num_snow_layer_1d      )
    deallocate (is_active_1d           )
    deallocate (hs_snow_1d             )
    deallocate (hs_sh2o_1d             )
    deallocate (hs_soil_1d             )
    deallocate (dhsdT_snow_1d          )
    deallocate (dhsdT_sh2o_1d          )
    deallocate (dhsdT_soil_1d          )
    deallocate (frac_soil_1d           )
    deallocate (sabg_snow_1d           )
    deallocate (sabg_soil_1d           )
    deallocate (tsurf_tuning_factor_1d )

  end subroutine EM_PTM_TBased_Solve

#endif

end module ExternalModelPTMMod
