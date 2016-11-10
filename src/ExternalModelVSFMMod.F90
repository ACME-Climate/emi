module ExternalModelVSFMMod
  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! This module provides
  !
  use abortutils                   , only : endrun
  use shr_kind_mod                 , only : r8 => shr_kind_r8
  use shr_log_mod                  , only : errMsg => shr_log_errMsg
  use ExternalModelInterfaceDataMod, only : emi_data_list, emi_data
  use mpp_varctl                   , only : iulog
  !
  implicit none
  !
  integer :: index_s_l2e_tsoil
  integer :: index_s_l2e_h2osoi_liq
  integer :: index_s_l2e_h2osoi_ice

  integer :: index_s_e2l_h2osoi_liq
  integer :: index_s_e2l_h2osoi_ice
  integer :: index_s_e2l_smp
  integer :: index_s_e2l_wtd
  integer :: index_s_e2l_soilp

  integer :: index_f_l2e_infil
  integer :: index_f_l2e_et
  integer :: index_f_l2e_dew
  integer :: index_f_l2e_snow_sub
  integer :: index_f_l2e_snowlyr
  integer :: index_f_l2e_drainage

  integer :: index_f_e2l_qrecharge

  integer :: index_filter_l2e_hydrologyc
  integer :: index_filter_l2e_num_hydrologyc

  integer :: index_mesh_l2e_zi

  !
  public :: EM_VSFM_Populate_L2E_List, &
            EM_VSFM_Populate_E2L_List, &
            EM_VSFM_Solve

contains

  !------------------------------------------------------------------------
  subroutine EM_VSFM_Populate_L2E_List(l2e_list)
    !
    ! !DESCRIPTION:
    ! Create a list of all variables needed by VSFM from ALM
    !
    ! !USES:
    use ExternalModelVSFMConstants, only : EM_VSFM_SOIL_HYDRO_STAGE
    use ExternalModelConstants    , only : S_L2E_TSOIL
    use ExternalModelConstants    , only : S_L2E_H2OSOI_LIQ
    use ExternalModelConstants    , only : S_L2E_H2OSOI_ICE
    use ExternalModelConstants    , only : F_L2E_INFIL_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_VERTICAL_ET_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_DEW_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_SNOW_SUBLIMATION_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_SNOW_LYR_DISAPPERANCE_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_DRAINAGE_MASS_FLUX
    use ExternalModelConstants    , only : FILTER_L2E_HYDROLOGYC
    use ExternalModelConstants    , only : FILTER_L2E_NUM_HYDROLOGYC
    use ExternalModelConstants    , only : MESH_L2E_ZI
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
    em_stages(1) = EM_VSFM_SOIL_HYDRO_STAGE

    allocate(data)
    call data%Init()
    call data%Setup(id=S_L2E_TSOIL,                           &
                    name = "Soil temperature",                &
                    units = "[K]",                            &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count             = count + 1
    index_s_l2e_tsoil = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=S_L2E_H2OSOI_LIQ,                      &
                    name = "Soil liquid water",               &
                    units = "[kg/m2]",                        &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    count                  = count + 1
    index_s_l2e_h2osoi_liq = count
    call l2e_list%AddData(data)
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=S_L2E_H2OSOI_ICE,                      &
                    name = "Soil ice water",                  &
                    units = "[kg/m2]",                        &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count                  = count + 1
    index_s_l2e_h2osoi_ice = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=F_L2E_INFIL_MASS_FLUX,                 &
                    name = "Soil infiltration source",        &
                    units = "[kg/s]",                         &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count             = count + 1
    index_f_l2e_infil = count
    nullify(data)
    
    allocate(data)
    call data%Init()
    call data%Setup(id=F_L2E_VERTICAL_ET_MASS_FLUX,           &
                    name = "Evapotranspiration sink",         &
                    units = "[kg/s]",                         &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count          = count + 1
    index_f_l2e_et = count
    nullify(data)
    
    allocate(data)
    call data%Init()
    call data%Setup(id=F_L2E_DEW_MASS_FLUX,                   &
                    name = "Dew sink",                        &
                    units = "[kg/s]",                         &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count           = count + 1
    index_f_l2e_dew = count
    nullify(data)
    
    allocate(data)
    call data%Init()
    call data%Setup(id=F_L2E_SNOW_SUBLIMATION_MASS_FLUX,      &
                    name = "Snow sublimation sink",           &
                    units = "[kg/s]",                         &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count                = count + 1
    index_f_l2e_snow_sub = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=F_L2E_SNOW_LYR_DISAPPERANCE_MASS_FLUX, &
                    name = "Snow layer disappearance sink",   &
                    units = "[kg/s]",                         &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count               = count + 1
    index_f_l2e_snowlyr = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=F_L2E_DRAINAGE_MASS_FLUX,              &
                    name = "Drainage",                        &
                    units = "[kg/s]",                         &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count               = count + 1
    index_f_l2e_drainage = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=FILTER_L2E_HYDROLOGYC,                 &
                    name = "Hydrology filter",                &
                    units = "[-]",                            &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count                       = count + 1
    index_filter_l2e_hydrologyc = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=FILTER_L2E_NUM_HYDROLOGYC,             &
                    name = "Number of hydrology filter",      &
                    units = "[-]",                            &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count                          = count + 1
    index_filter_l2e_num_hydrologyc = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=MESH_L2E_ZI,                           &
                    name = "Depth of interface layer",        &
                    units = "[m]",                            &
                    num_em_stages = number_em_stages,         &
                    em_stage_ids = em_stages)
    call l2e_list%AddData(data)
    count             = count + 1
    index_mesh_l2e_zi = count
    nullify(data)

    deallocate(em_stages)

  end subroutine EM_VSFM_Populate_L2E_List

  !------------------------------------------------------------------------
  subroutine EM_VSFM_Populate_E2L_List(e2l_list)
    !
    !
    ! !DESCRIPTION:
    ! Create a list of all variables to be returned by VSFM from ALM
    !
    ! !USES:
    use ExternalModelVSFMConstants, only : EM_VSFM_SOIL_HYDRO_STAGE
    use ExternalModelConstants    , only : S_E2L_H2OSOI_LIQ
    use ExternalModelConstants    , only : S_E2L_H2OSOI_ICE
    use ExternalModelConstants    , only : S_E2L_SOIL_MATRIC_POTENTIAL
    use ExternalModelConstants    , only : S_E2L_WTD
    use ExternalModelConstants    , only : S_E2L_VSFM_PROGNOSTIC_SOILP
    use ExternalModelConstants    , only : F_E2L_AQUIFER_RECHARGE
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list) , intent(inout) :: e2l_list
    !
    ! !LOCAL VARIABLES:
    class(emi_data)      , pointer       :: data
    integer              , pointer       :: em_stages(:)
    integer                              :: number_em_stages
    integer                              :: count

    count            = 0
    number_em_stages = 1
    allocate(em_stages(number_em_stages))
    em_stages(1) = EM_VSFM_SOIL_HYDRO_STAGE

    allocate(data)
    call data%Init()
    call data%Setup(id=S_E2L_H2OSOI_LIQ,              &
                    name = "Soil liquid water",       &
                    units = "[kg/m2]",                &
                    num_em_stages = number_em_stages, &
                    em_stage_ids = em_stages)
    call e2l_list%AddData(data)
    count                  = count + 1
    index_s_e2l_h2osoi_liq = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=S_E2L_H2OSOI_ICE,              &
                    name = "Soil ice water",          &
                    units = "[kg/m2]",                &
                    num_em_stages = number_em_stages, &
                    em_stage_ids = em_stages)
    call e2l_list%AddData(data)
    count                  = count + 1
    index_s_e2l_h2osoi_ice = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=S_E2L_SOIL_MATRIC_POTENTIAL,   &
                    name = "Soil matric potential",   &
                    units = "[mm]",                   &
                    num_em_stages = number_em_stages, &
                    em_stage_ids = em_stages)
    call e2l_list%AddData(data)
    count                          = count + 1
    index_s_e2l_smp = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=S_E2L_WTD,                     &
                    name = "Water table depth",       &
                    units = "[m]",                    &
                    num_em_stages = number_em_stages, &
                    em_stage_ids = em_stages)
    call e2l_list%AddData(data)
    count           = count + 1
    index_s_e2l_wtd = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=S_E2L_VSFM_PROGNOSTIC_SOILP,   &
                    name = "Soil liquid pressure",    &
                    units = "[Pa]",                   &
                    num_em_stages = number_em_stages, &
                    em_stage_ids = em_stages)
    call e2l_list%AddData(data)
    count             = count + 1
    index_s_e2l_soilp = count
    nullify(data)

    allocate(data)
    call data%Init()
    call data%Setup(id=F_E2L_AQUIFER_RECHARGE,        &
                    name = "Aquifer recharge rate",   &
                    units = "[mm/s]",                 &
                    num_em_stages = number_em_stages, &
                    em_stage_ids = em_stages)
    call e2l_list%AddData(data)
    count                 = count + 1
    index_f_e2l_qrecharge = count
    nullify(data)

    deallocate(em_stages)
end subroutine EM_VSFM_Populate_E2L_List

  !------------------------------------------------------------------------
  subroutine EM_VSFM_Solve(em_stage, dt, nstep, l2e_list, e2l_list)

    !
    ! !DESCRIPTION:
    !
    ! !USES:
    use shr_kind_mod              , only : r8 => shr_kind_r8
    use abortutils                , only : endrun
    use shr_log_mod               , only : errMsg => shr_log_errMsg
    use MultiPhysicsProbVSFM      , only : vsfm_mpp
    use MultiPhysicsProbConstants , only : VAR_BC_SS_CONDITION
    use MultiPhysicsProbConstants , only : VAR_TEMPERATURE
    use MultiPhysicsProbConstants , only : VAR_PRESSURE
    use MultiPhysicsProbConstants , only : VAR_LIQ_SAT
    use MultiPhysicsProbConstants , only : VAR_FRAC_LIQ_SAT
    use MultiPhysicsProbConstants , only : VAR_MASS
    use MultiPhysicsProbConstants , only : VAR_SOIL_MATRIX_POT
    use MultiPhysicsProbConstants , only : VAR_LATERAL_MASS_EXCHANGED
    use MultiPhysicsProbConstants , only : VAR_BC_MASS_EXCHANGED
    use MultiPhysicsProbConstants , only : AUXVAR_INTERNAL
    use MultiPhysicsProbConstants , only : AUXVAR_BC
    use MultiPhysicsProbConstants , only : AUXVAR_SS
    use MPPVSFMALM_Initialize     , only : vsfm_cond_id_for_infil
    use MPPVSFMALM_Initialize     , only : vsfm_cond_id_for_et
    use MPPVSFMALM_Initialize     , only : vsfm_cond_id_for_dew
    use MPPVSFMALM_Initialize     , only : vsfm_cond_id_for_drainage
    use MPPVSFMALM_Initialize     , only : vsfm_cond_id_for_snow
    use MPPVSFMALM_Initialize     , only : vsfm_cond_id_for_sublimation
    use MPPVSFMALM_Initialize     , only : vsfm_cond_id_for_lateral_flux
    use mpp_varpar                , only : nlevgrnd
    !
    implicit none
    !
#include "finclude/petscsys.h"
#include "finclude/petscsnes.h"
#include "finclude/petscsnes.h90"
    !
    ! !ARGUMENTS:
    integer              , intent(in)    :: em_stage
    real(r8)             , intent(in)    :: dt
    integer              , intent(in)    :: nstep
    class(emi_data_list) , intent(in)    :: l2e_list
    class(emi_data_list) , intent(inout) :: e2l_list
    !
    ! !LOCAL VARIABLES:
    integer                              :: p,c,fc,j,g                                                       ! do loop indices
    integer                              :: pi                                                               ! pft index
    real(r8)                             :: dzsum                                                            ! summation of dzmm of layers below water table (mm)
    real(r8)                             :: dtime

    real(r8)  , pointer                  :: mflx_et_col_1d         (:)
    real(r8)  , pointer                  :: mflx_infl_col_1d       (:)
    real(r8)  , pointer                  :: mflx_dew_col_1d        (:)
    real(r8)  , pointer                  :: mflx_drain_col_1d      (:)
    real(r8)  , pointer                  :: mflx_sub_snow_col_1d   (:)
    real(r8)  , pointer                  :: mflx_snowlyr_col_1d    (:)
    real(r8)  , pointer                  :: t_soil_col_1d          (:)

    real(r8)  , pointer                  :: vsfm_fliq_col_1d       (:)
    real(r8)  , pointer                  :: vsfm_mass_col_1d       (:)
    real(r8)  , pointer                  :: vsfm_smpl_col_1d       (:)
    real(r8)  , pointer                  :: vsfm_soilp_col_1d      (:)
    real(r8)  , pointer                  :: vsfm_sat_col_1d        (:)

    real(r8)  , pointer                  :: frac_ice                    (:,:) ! fraction of ice
    real(r8)  , pointer                  :: total_mass_flux_col         (:)            ! Sum of all source-sinks conditions for VSFM solver at column level
    real(r8)  , pointer                  :: total_mass_flux_et_col      (:)            ! ET sink for VSFM solver at column level
    real(r8)  , pointer                  :: total_mass_flux_infl_col    (:)            ! Infiltration source for VSFM solver at column level
    real(r8)  , pointer                  :: total_mass_flux_dew_col     (:)            ! Dew source for VSFM solver at column level
    real(r8)  , pointer                  :: total_mass_flux_drain_col   (:)            ! Drainage sink for VSFM solver at column level
    real(r8)  , pointer                  :: total_mass_flux_snowlyr_col (:)            ! Flux due to disappearance of snow for VSFM solver at column level
    real(r8)  , pointer                  :: total_mass_flux_sub_col     (:)            ! Sublimation sink for VSFM solver at column level
    real(r8)  , pointer                  :: total_mass_flux_lateral_col (:)            ! Lateral flux computed by VSFM solver at column level
    real(r8)  , pointer                  :: total_mass_flux_seepage_col (:)            ! Seepage flux computed by VSFM solver at column level
    real(r8)  , pointer                  :: qflx_seepage                (:)            ! Seepage flux computed by VSFM solver at column level
    real(r8)  , pointer                  :: vsfm_mass_prev_col          (:,:) ! Mass of water before a VSFM solve
    real(r8)  , pointer                  :: vsfm_dmass_col              (:)            ! Change in mass of water after a VSFM solve
    real(r8)  , pointer                  :: mass_beg_col                (:)            ! Total mass before a VSFM solve
    real(r8)  , pointer                  :: mass_end_col                (:)            ! Total mass after a VSFM solve
    integer                              :: ier                                                              ! error status

    integer                              :: begc, endc
    integer                              :: idx
    real(r8)                             :: area

    PetscInt                             :: soe_auxvar_id                                                    ! Index of system-of-equation's (SoE's) auxvar
    PetscErrorCode                       :: ierr                                                             ! PETSc return error code

    PetscBool                            :: converged                                                        ! Did VSFM solver converge to a solution with given PETSc SNES tolerances
    PetscInt                             :: converged_reason                                                 ! SNES converged due to which criteria
    PetscReal                            :: atol_default                                                     ! Default SNES absolute convergance tolerance
    PetscReal                            :: rtol_default                                                     ! Default SNES relative convergance tolerance
    PetscReal                            :: stol_default                                                     ! Default SNES solution convergance tolerance
    PetscInt                             :: max_it_default                                                   ! Default SNES maximum number of iteration
    PetscInt                             :: max_f_default                                                    ! Default SNES maximum number of function evaluation
    PetscReal                            :: stol                                                             ! solution convergance tolerance
    PetscReal                            :: rtol                                                             ! relative convergance tolerance
    PetscReal,parameter                  :: stol_alternate = 1.d-10                                          ! Alternate solution convergance tolerance

    PetscReal                            :: mass_beg                                                         ! Sum of mass of water for all active soil columns before VSFM is called
    PetscReal                            :: mass_end                                                         ! Sum of mass of water for all active soil columns after VSFM is called
    PetscReal                            :: total_mass_flux_et                                               ! Sum of mass ET mass flux of water for all active soil columns
    PetscReal                            :: total_mass_flux_infl                                             ! Sum of mass infiltration mass flux of water for all active soil columns
    PetscReal                            :: total_mass_flux_dew                                              ! Sum of mass dew mass flux of water for all active soil columns
    PetscReal                            :: total_mass_flux_drain                                            ! Sum of mass drainage mass flux of water for all active soil columns
    PetscReal                            :: total_mass_flux_snowlyr                                          ! Sum of mass snow layer disappearance mass flux of water for all active soil columns
    PetscReal                            :: total_mass_flux_sub                                              ! Sum of mass sublimation mass flux of water for all active soil columns
    PetscReal                            :: total_mass_flux_lateral                                          ! Sum of lateral mass flux for all active soil columns
    PetscReal                            :: total_mass_flux                                                  ! Sum of mass ALL mass flux of water for all active soil columns
    PetscInt                             :: iter_count                                                       ! How many times VSFM solver is called

    PetscInt, parameter                  :: max_iter_count = 10                                              ! Maximum number of times VSFM can be called
    PetscInt                             :: diverged_count                                                   ! Number of time VSFM solver diverged
    PetscInt                             :: mass_bal_err_count                                               ! Number of time VSFM solver returns a solution that isn't within acceptable mass balance error threshold
    PetscReal                            :: abs_mass_error_col                                               ! Maximum absolute error for any active soil column
    PetscReal, parameter                 :: max_abs_mass_error_col  = 1.e-5                                  ! Acceptable mass balance error
    PetscBool                            :: successful_step                                                  ! Is the solution return by VSFM acceptable
    PetscReal , pointer                  :: vsfm_soilp_col_ghosted_1d(:)
    PetscReal , pointer                  :: vsfm_fliq_col_ghosted_1d(:)
    PetscReal , pointer                  :: mflx_lateral_col_1d(:)
    PetscReal , pointer                  :: lat_mass_exc_col_1d(:)
    PetscReal , pointer                  :: seepage_mass_exc_col_1d(:)
    PetscReal , pointer                  :: seepage_press_1d(:)

    integer                              :: jwt
    real(r8)                             :: z_dn, z_up

    real(r8)  , pointer                  :: l2e_mflux_infil(:)
    real(r8)  , pointer                  :: l2e_mflux_dew(:)
    real(r8)  , pointer                  :: l2e_mflux_sub_snow(:)
    real(r8)  , pointer                  :: l2e_mflux_snowlyr(:)
    real(r8)  , pointer                  :: l2e_mflux_et(:,:)
    real(r8)  , pointer                  :: l2e_mflux_drain(:,:)
    real(r8)  , pointer                  :: l2e_h2osoi_liq(:,:)
    real(r8)  , pointer                  :: l2e_h2osoi_ice(:,:)
    real(r8)  , pointer                  :: l2e_zi(:,:)
    integer   , pointer                  :: l2e_filter_hydrologyc(:)
    integer                              :: l2e_num_hydrologyc

    real(r8)  , pointer                  :: e2l_h2osoi_liq(:,:)
    real(r8)  , pointer                  :: e2l_h2osoi_ice(:,:)
    real(r8)  , pointer                  :: e2l_smp(:,:)
    real(r8)  , pointer                  :: e2l_wtd(:)
    real(r8)  , pointer                  :: e2l_soilp(:,:)
    real(r8)  , pointer                  :: e2l_qrecharge(:)

    !-----------------------------------------------------------------------

      ! Get time step

      dtime = dt

      l2e_mflux_infil       => l2e_list%data_ptr(index_f_l2e_infil              )%data%data_real_1d
      l2e_mflux_dew         => l2e_list%data_ptr(index_f_l2e_dew                )%data%data_real_1d
      l2e_mflux_sub_snow    => l2e_list%data_ptr(index_f_l2e_snow_sub           )%data%data_real_1d
      l2e_mflux_snowlyr     => l2e_list%data_ptr(index_f_l2e_snowlyr            )%data%data_real_1d

      l2e_mflux_et          => l2e_list%data_ptr(index_f_l2e_et                 )%data%data_real_2d
      l2e_mflux_drain       => l2e_list%data_ptr(index_f_l2e_drainage           )%data%data_real_2d
      l2e_h2osoi_liq        => l2e_list%data_ptr(index_s_l2e_h2osoi_liq         )%data%data_real_2d
      l2e_h2osoi_ice        => l2e_list%data_ptr(index_s_l2e_h2osoi_ice         )%data%data_real_2d

      l2e_filter_hydrologyc => l2e_list%data_ptr(index_filter_l2e_hydrologyc    )%data%data_int_1d
      l2e_num_hydrologyc    =  l2e_list%data_ptr(index_filter_l2e_num_hydrologyc)%data%data_int_1d(1)

      l2e_zi                => l2e_list%data_ptr(index_mesh_l2e_zi              )%data%data_real_2d

      e2l_h2osoi_liq        => e2l_list%data_ptr(index_s_e2l_h2osoi_liq         )%data%data_real_2d
      e2l_h2osoi_ice        => e2l_list%data_ptr(index_s_e2l_h2osoi_ice         )%data%data_real_2d
      e2l_smp               => e2l_list%data_ptr(index_s_e2l_smp                )%data%data_real_2d
      e2l_wtd               => e2l_list%data_ptr(index_s_e2l_wtd                )%data%data_real_1d
      e2l_soilp             => e2l_list%data_ptr(index_s_e2l_soilp              )%data%data_real_2d
      e2l_qrecharge         => e2l_list%data_ptr(index_f_e2l_qrecharge          )%data%data_real_1d

      begc = l2e_list%data_ptr(index_f_l2e_et)%data%dim1_beg_clump(1)
      endc = l2e_list%data_ptr(index_f_l2e_et)%data%dim1_end_clump(1)

      allocate(frac_ice                    (begc:endc,1:nlevgrnd))
      allocate(total_mass_flux_col         (begc:endc))
      allocate(total_mass_flux_et_col      (begc:endc))
      allocate(total_mass_flux_infl_col    (begc:endc))
      allocate(total_mass_flux_dew_col     (begc:endc))
      allocate(total_mass_flux_drain_col   (begc:endc))
      allocate(total_mass_flux_snowlyr_col (begc:endc))
      allocate(total_mass_flux_sub_col     (begc:endc))
      allocate(total_mass_flux_lateral_col (begc:endc))
      allocate(total_mass_flux_seepage_col (begc:endc))
      allocate(qflx_seepage                (begc:endc))
      allocate(vsfm_mass_prev_col          (begc:endc,1:nlevgrnd))
      allocate(vsfm_dmass_col              (begc:endc))
      allocate(mass_beg_col                (begc:endc))
      allocate(mass_end_col                (begc:endc))

      allocate(mflx_et_col_1d              ((endc-begc+1)*nlevgrnd))
      allocate(mflx_drain_col_1d           ((endc-begc+1)*nlevgrnd))
      allocate(mflx_infl_col_1d            (endc-begc+1))
      allocate(mflx_dew_col_1d             (endc-begc+1))
      allocate(mflx_sub_snow_col_1d        (endc-begc+1))
      allocate(mflx_snowlyr_col_1d         (endc-begc+1))
      allocate(t_soil_col_1d               ((endc-begc+1)*nlevgrnd))

      allocate(vsfm_mass_col_1d            ((endc-begc+1)*nlevgrnd))
      allocate(vsfm_fliq_col_1d            ((endc-begc+1)*nlevgrnd))
      allocate(vsfm_smpl_col_1d            ((endc-begc+1)*nlevgrnd))
      allocate(vsfm_soilp_col_1d           ((endc-begc+1)*nlevgrnd))
      allocate(vsfm_sat_col_1d             ((endc-begc+1)*nlevgrnd))

      ! initialize
      mflx_et_col_1d(:)                = 0.d0
      mflx_infl_col_1d(:)              = 0.d0
      mflx_dew_col_1d(:)               = 0.d0
      mflx_drain_col_1d(:)             = 0.d0
      mflx_sub_snow_col_1d(:)          = 0.d0
      mflx_snowlyr_col_1d(:)           = 0.d0
      t_soil_col_1d(:)                 = 298.15d0

      mass_beg                         = 0.d0
      mass_end                         = 0.d0
      total_mass_flux                  = 0.d0
      total_mass_flux_et               = 0.d0
      total_mass_flux_infl             = 0.d0
      total_mass_flux_dew              = 0.d0
      total_mass_flux_drain            = 0.d0
      total_mass_flux_snowlyr          = 0.d0
      total_mass_flux_sub              = 0.d0
      total_mass_flux_lateral          = 0.d0

      mass_beg_col(:)                  = 0.d0
      mass_end_col(:)                  = 0.d0
      total_mass_flux_col(:)           = 0.d0
      total_mass_flux_et_col(:)        = 0.d0
      total_mass_flux_infl_col(:)      = 0.d0
      total_mass_flux_dew_col(:)       = 0.d0
      total_mass_flux_drain_col(:)     = 0.d0
      total_mass_flux_snowlyr_col(:)   = 0.d0
      total_mass_flux_sub_col(:)       = 0.d0
      total_mass_flux_lateral_col(:)   = 0.d0

      vsfm_mass_prev_col(:,:)          = 0.d0
      vsfm_dmass_col(:)                = 0.d0

      ! Get total mass
      soe_auxvar_id = 1;
      call vsfm_mpp%sysofeqns%GetDataForCLM(AUXVAR_INTERNAL ,       &
                                            VAR_MASS        ,       &
                                            soe_auxvar_id   ,       &
                                            vsfm_mass_col_1d        &
                                           )

      do fc = 1, l2e_num_hydrologyc
         c = l2e_filter_hydrologyc(fc)

         do j = 1, nlevgrnd

            idx = (c - begc)*nlevgrnd + j
            mflx_et_col_1d(idx)          = l2e_mflux_et(c,j)
            mflx_drain_col_1d(idx)       = l2e_mflux_drain(c,j)

            total_mass_flux_et           = total_mass_flux_et           + mflx_et_col_1d(idx)
            total_mass_flux_et_col(c)    = total_mass_flux_et_col(c)    + mflx_et_col_1d(idx)

            total_mass_flux_drain        = total_mass_flux_drain        + mflx_drain_col_1d(idx)
            total_mass_flux_drain_col(c) = total_mass_flux_drain_col(c) + mflx_drain_col_1d(idx)

            mass_beg                     = mass_beg                     + vsfm_mass_col_1d(idx)
            mass_beg_col(c)              = mass_beg_col(c)              + vsfm_mass_col_1d(idx)
            vsfm_mass_prev_col(c,j)      = vsfm_mass_col_1d(idx)
         end do

         idx = c - begc+1

         mflx_dew_col_1d(idx)           = l2e_mflux_dew(c)
         mflx_infl_col_1d(idx)          = l2e_mflux_infil(c)
         mflx_snowlyr_col_1d(idx)       = l2e_mflux_snowlyr(c)
         mflx_sub_snow_col_1d(idx)      = l2e_mflux_sub_snow(c)

         total_mass_flux_dew            = total_mass_flux_dew            + mflx_dew_col_1d(idx)
         total_mass_flux_dew_col(c)     = total_mass_flux_dew_col(c)     + mflx_dew_col_1d(idx)

         total_mass_flux_infl           = total_mass_flux_infl           + mflx_infl_col_1d(idx)
         total_mass_flux_infl_col(c)    = total_mass_flux_infl_col(c)    + mflx_infl_col_1d(idx)

         total_mass_flux_snowlyr        = total_mass_flux_snowlyr        + mflx_snowlyr_col_1d(idx)
         total_mass_flux_snowlyr_col(c) = total_mass_flux_snowlyr_col(c) + mflx_snowlyr_col_1d(idx)

         total_mass_flux_sub            = total_mass_flux_sub            + mflx_sub_snow_col_1d(idx)
         total_mass_flux_sub_col(c)     = total_mass_flux_sub_col(c)     + mflx_sub_snow_col_1d(idx)

         total_mass_flux_col(c) = total_mass_flux_et_col(c)      + &
                                  total_mass_flux_infl_col(c)    + &
                                  total_mass_flux_dew_col(c)     + &
                                  total_mass_flux_drain_col(c)   + &
                                  total_mass_flux_snowlyr_col(c) + &
                                  total_mass_flux_sub_col(c)     + &
                                  total_mass_flux_lateral_col(c)
      end do
      total_mass_flux        = total_mass_flux_et        + &
                               total_mass_flux_infl      + &
                               total_mass_flux_dew       + &
                               total_mass_flux_drain     + &
                               total_mass_flux_snowlyr   + &
                               total_mass_flux_sub       + &
                               total_mass_flux_lateral

      ! Set temperature
      soe_auxvar_id = 1;
      call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_INTERNAL ,      &
                                             VAR_TEMPERATURE ,      &
                                             soe_auxvar_id   ,      &
                                             t_soil_col_1d          &
                                            )
      ! Set Infiltration
      soe_auxvar_id = vsfm_cond_id_for_infil;
      call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_SS           ,  &
                                             VAR_BC_SS_CONDITION ,  &
                                             soe_auxvar_id       ,  &
                                             mflx_infl_col_1d       &
                                            )
      ! Set ET
      soe_auxvar_id = vsfm_cond_id_for_et;
      call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_SS           ,  &
                                             VAR_BC_SS_CONDITION ,  &
                                             soe_auxvar_id       ,  &
                                             mflx_et_col_1d         &
                                            )
      ! Set Dew
      soe_auxvar_id = vsfm_cond_id_for_dew;
      call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_SS           ,  &
                                             VAR_BC_SS_CONDITION ,  &
                                             soe_auxvar_id       ,  &
                                             mflx_dew_col_1d        &
                                            )
      ! Set Drainage sink
      soe_auxvar_id = vsfm_cond_id_for_drainage;
      call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_SS           ,  &
                                             VAR_BC_SS_CONDITION ,  &
                                             soe_auxvar_id       ,  &
                                             mflx_drain_col_1d      &
                                            )
      ! Set mass flux associated with disappearance of snow layer
      ! from last time step
      soe_auxvar_id = vsfm_cond_id_for_snow;
      call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_SS           ,  &
                                             VAR_BC_SS_CONDITION ,  &
                                             soe_auxvar_id       ,  &
                                             mflx_snowlyr_col_1d    &
                                            )
      ! Set mass flux associated with sublimation of snow
      soe_auxvar_id = vsfm_cond_id_for_sublimation;
      call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_SS            , &
                                             VAR_BC_SS_CONDITION  , &
                                             soe_auxvar_id        , &
                                             mflx_sub_snow_col_1d   &
                                            )

      frac_ice(:,:)       = 0.d0
      vsfm_fliq_col_1d(:) = 1.d0
      do fc = 1, l2e_num_hydrologyc
         c = l2e_filter_hydrologyc(fc)
         do j = 1, nlevgrnd

            frac_ice(c,j) = l2e_h2osoi_ice(c,j)/(l2e_h2osoi_liq(c,j) + l2e_h2osoi_ice(c,j))

            idx = (c - begc)*nlevgrnd + j
            vsfm_fliq_col_1d(idx) = 1._r8 - frac_ice(c,j)
         end do
      end do

      ! Set frac_liq
      soe_auxvar_id = 1;
      call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_INTERNAL  , &
                                             VAR_FRAC_LIQ_SAT , &
                                             soe_auxvar_id    , &
                                             vsfm_fliq_col_1d   &
                                            )


#if 0
      if (vsfm_lateral_model_type == 'source_sink') then

         call get_proc_bounds(bounds_proc)

         allocate(vsfm_soilp_col_ghosted_1d((bounds_proc%endc_all - bounds_proc%begc_all+1)*nlevgrnd))
         allocate(vsfm_fliq_col_ghosted_1d( (bounds_proc%endc_all - bounds_proc%begc_all+1)*nlevgrnd))
         allocate(mflx_lateral_col_1d( (bounds_proc%endc - bounds_proc%begc+1)*nlevgrnd))

         soe_auxvar_id = 1;
         call vsfm_mpp%sysofeqns%GetDataForCLM(AUXVAR_INTERNAL   , &
                                               VAR_PRESSURE      , &
                                               soe_auxvar_id     , &
                                               vsfm_soilp_col_1d   &
                                               )

         call ExchangeColumnLevelGhostData(bounds, nlevgrnd, vsfm_soilp_col_1d, vsfm_soilp_col_ghosted_1d)
         call ExchangeColumnLevelGhostData(bounds, nlevgrnd, vsfm_fliq_col_1d,  vsfm_fliq_col_ghosted_1d )

         soe_auxvar_id = 1;
         call vsfm_mpp%sysofeqns%SetDataFromCLMForGhost(AUXVAR_INTERNAL           , &
                                                        VAR_PRESSURE              , &
                                                        soe_auxvar_id             , &
                                                        vsfm_soilp_col_ghosted_1d   &
                                                       )

         soe_auxvar_id = 1;
         call vsfm_mpp%sysofeqns%SetDataFromCLMForGhost(AUXVAR_INTERNAL          , &
                                                        VAR_FRAC_LIQ_SAT         , &
                                                        soe_auxvar_id            , &
                                                        vsfm_fliq_col_ghosted_1d   &
                                                       )

         call vsfm_mpp%sysofeqns%ComputeLateralFlux(dtime)

         soe_auxvar_id = vsfm_cond_id_for_lateral_flux;
         call vsfm_mpp%sysofeqns%GetDataForCLM(AUXVAR_SS   , &
                                               VAR_BC_SS_CONDITION      , &
                                               soe_auxvar_id     , &
                                               mflx_lateral_col_1d   &
                                               )

         do fc = 1, num_hydrologyc
            c = filter_hydrologyc(fc)

            g    = col%gridCell(c)
            area = ldomain_lateral%ugrid%areaGrid_ghosted(g)

            ! [mm/s] --> [kg/s]   [m^2] [kg/m^3]  [m/mm]
            flux_unit_conversion     = area * denh2o * 1.0d-3

            qflx_lateral(c) = 0._r8
            do j = 1, nlevgrnd
               idx = (c-bounds%begc)*nlevgrnd + j

               total_mass_flux_lateral_col(c) =      &
                    total_mass_flux_lateral_col(c) + &
                    mflx_lateral_col_1d(idx)

               qflx_lateral(c) = qflx_lateral(c) - &
                    mflx_lateral_col_1d(idx)/flux_unit_conversion
            enddo

            total_mass_flux_lateral = total_mass_flux_lateral + &
                 total_mass_flux_lateral_col(c)
         enddo

         deallocate(vsfm_soilp_col_ghosted_1d )
         deallocate(vsfm_fliq_col_ghosted_1d  )
         deallocate(mflx_lateral_col_1d       )

         if (vsfm_include_seepage_bc) then
            allocate(seepage_press_1d( (bounds_proc%endc - bounds_proc%begc+1)))
            seepage_press_1d(:) = 101325.d0
            soe_auxvar_id = 1
            call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_BC,  &
                 VAR_BC_SS_CONDITION, soe_auxvar_id, seepage_press_1d)
            deallocate(seepage_press_1d)
         endif

      else if (vsfm_lateral_model_type == 'three_dimensional') then

         call get_proc_bounds(bounds_proc)

         if (vsfm_include_seepage_bc) then
            allocate(seepage_press_1d( (bounds_proc%endc - bounds_proc%begc+1)))
            seepage_press_1d(:) = 101325.d0
            soe_auxvar_id = 1
            call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_BC,  &
                 VAR_BC_SS_CONDITION, soe_auxvar_id, seepage_press_1d)
            deallocate(seepage_press_1d)
         endif

      endif
#endif

      ! Preform Pre-StepDT operations
      call vsfm_mpp%sysofeqns%PreStepDT()

      ! Get default SNES settings
      call SNESGetTolerances(vsfm_mpp%sysofeqns%snes , &
                             atol_default            , &
                             rtol_default            , &
                             stol_default            , &
                             max_it_default          , &
                             max_f_default           , &
                             ierr                      &
                            )
      CHKERRQ(ierr)

      stol = stol_default
      rtol = rtol_default

      !
      ! Solve the VSFM.
      !
      iter_count           = 0
      diverged_count       = 0
      mass_bal_err_count   = 0
      abs_mass_error_col   = 0.d0
      successful_step      = PETSC_FALSE

      do

         iter_count = iter_count + 1

         call SNESSetTolerances(vsfm_mpp%sysofeqns%snes , &
                                atol_default            , &
                                rtol                    , &
                                stol                    , &
                                max_it_default          , &
                                max_f_default           , &
                                ierr                      &
                               );
         CHKERRQ(ierr)

         call vsfm_mpp%sysofeqns%StepDT(dtime, nstep, &
              converged, converged_reason, ierr); CHKERRQ(ierr)

         if (.not. converged) then

            ! VSFM solver did not converge, so let's try again with different
            ! solver settings.

            stol             = stol_alternate
            diverged_count   = diverged_count + 1
            successful_step  = PETSC_FALSE

            ! Reduce total run length time by the amount VSFM ran successfully
            ! with previous solver settings
            dtime = dtime - vsfm_mpp%sysofeqns%time

            if (diverged_count > 1) then
               ! Set frac_liq
               vsfm_fliq_col_1d(:) = 1.d0
               soe_auxvar_id = 1;

               call vsfm_mpp%sysofeqns%SetDataFromCLM(AUXVAR_INTERNAL  , &
                                                      VAR_FRAC_LIQ_SAT , &
                                                      soe_auxvar_id    , &
                                                      vsfm_fliq_col_1d   &
                                                     )
            end if
         else

            ! Solver converged, so let's copy data from VSFM model to
            ! CLM's data structure.

            ! Get Liquid saturation
            soe_auxvar_id = 1;
            call vsfm_mpp%sysofeqns%GetDataForCLM(AUXVAR_INTERNAL , &
                                                  VAR_LIQ_SAT     , &
                                                  soe_auxvar_id   , &
                                                  vsfm_sat_col_1d   &
                                                 )

            ! Get total mass
            soe_auxvar_id = 1;
            call vsfm_mpp%sysofeqns%GetDataForCLM(AUXVAR_INTERNAL  , &
                                                  VAR_MASS         , &
                                                  soe_auxvar_id    , &
                                                  vsfm_mass_col_1d   &
                                                 )

            ! Get liquid soil matrix potential
            soe_auxvar_id = 1;
            call vsfm_mpp%sysofeqns%GetDataForCLM(AUXVAR_INTERNAL     , &
                                                  VAR_SOIL_MATRIX_POT , &
                                                  soe_auxvar_id       , &
                                                  vsfm_smpl_col_1d      &
                                                 )

            ! Get soil liquid pressure. This is the prognostic state of VSFM
            ! and needs to be saved in the restart file.
            soe_auxvar_id = 1;
            call vsfm_mpp%sysofeqns%GetDataForCLM(AUXVAR_INTERNAL   , &
                                                  VAR_PRESSURE      , &
                                                  soe_auxvar_id     , &
                                                  vsfm_soilp_col_1d   &
                                                 )

            qflx_seepage(:) = 0._r8

#if 0
            if (vsfm_lateral_model_type == 'source_sink') then

               ! Get following fluxes from VSFM:
               ! (i) seepage mass exchanged.

               call get_proc_bounds(bounds_proc)

               allocate(seepage_mass_exc_col_1d( (bounds_proc%endc - bounds_proc%begc+1)         ))
               seepage_mass_exc_col_1d = 0.d0

               if (vsfm_include_seepage_bc) then
                  soe_auxvar_id = 1;
                  call vsfm_mpp%sysofeqns%GetDataForCLM(AUXVAR_BC              ,  &
                                                        VAR_BC_MASS_EXCHANGED  ,  &
                                                        soe_auxvar_id          ,  &
                                                        seepage_mass_exc_col_1d   &
                                                        )
               endif

               do fc = 1, num_hydrologyc
                  c = filter_hydrologyc(fc)

                  g    = col%gridCell(c)
                  area = ldomain_lateral%ugrid%areaGrid_ghosted(g)

                  ! [mm/s] --> [kg/s]   [m^2] [kg/m^3]  [m/mm]
                  flux_unit_conversion     = area * denh2o * 1.0d-3

                  idx = (c-bounds%begc) + 1

                  qflx_seepage(c)                = seepage_mass_exc_col_1d(idx)/flux_unit_conversion/dtime
                  total_mass_flux_seepage_col(c) = -seepage_mass_exc_col_1d(idx)/dtime

                  total_mass_flux_col(c) = total_mass_flux_et_col(c)      + &
                                           total_mass_flux_infl_col(c)    + &
                                           total_mass_flux_dew_col(c)     + &
                                           total_mass_flux_drain_col(c)   + &
                                           total_mass_flux_snowlyr_col(c) + &
                                           total_mass_flux_sub_col(c)     + &
                                           total_mass_flux_lateral_col(c) + &
                                           total_mass_flux_seepage_col(c)
               enddo

            else if (vsfm_lateral_model_type == 'three_dimensional') then

               ! Get following fluxes from VSFM:
               ! (i) lateral mass exchanged, and
               ! (ii) seepage mass exchanged.

               call get_proc_bounds(bounds_proc)

               allocate(lat_mass_exc_col_1d(     (bounds_proc%endc - bounds_proc%begc+1)*nlevgrnd))
               allocate(seepage_mass_exc_col_1d( (bounds_proc%endc - bounds_proc%begc+1)         ))

               lat_mass_exc_col_1d(:) = 0.d0
               seepage_mass_exc_col_1d(:) = 0.d0

               soe_auxvar_id = 1
               call vsfm_mpp%sysofeqns%GetDataForCLM(AUXVAR_INTERNAL            , &
                                                     VAR_LATERAL_MASS_EXCHANGED , &
                                                     soe_auxvar_id              , &
                                                     lat_mass_exc_col_1d          &
                                                     )

               if (vsfm_include_seepage_bc) then
                  soe_auxvar_id = 1;
                  call vsfm_mpp%sysofeqns%GetDataForCLM(AUXVAR_BC              ,  &
                                                        VAR_BC_MASS_EXCHANGED  ,  &
                                                        soe_auxvar_id          ,  &
                                                        seepage_mass_exc_col_1d   &
                                                        )
               endif

               total_mass_flux_lateral_col(:)   = 0.d0
               total_mass_flux_lateral          = 0.d0

               do fc = 1, num_hydrologyc
                  c = filter_hydrologyc(fc)

                  g    = col%gridCell(c)
                  area = ldomain_lateral%ugrid%areaGrid_ghosted(g)

                  ! [mm/s] --> [kg/s]   [m^2] [kg/m^3]  [m/mm]
                  flux_unit_conversion     = area * denh2o * 1.0d-3

                  qflx_lateral(c) = 0._r8
                  do j = 1, nlevgrnd
                     idx = (c-bounds%begc)*nlevgrnd + j

                     total_mass_flux_lateral_col(c) = &
                          total_mass_flux_lateral_col(c) + &
                          lat_mass_exc_col_1d(idx)/dtime

                     qflx_lateral(c)  = qflx_lateral(c) - &
                          lat_mass_exc_col_1d(idx)/flux_unit_conversion/dtime

                  enddo

                  idx = (c-bounds%begc) + 1
                  qflx_seepage(c)                = seepage_mass_exc_col_1d(idx)/flux_unit_conversion/dtime
                  total_mass_flux_seepage_col(c) = -seepage_mass_exc_col_1d(idx)/dtime

                  total_mass_flux_col(c) = total_mass_flux_et_col(c)      + &
                                           total_mass_flux_infl_col(c)    + &
                                           total_mass_flux_dew_col(c)     + &
                                           total_mass_flux_drain_col(c)   + &
                                           total_mass_flux_snowlyr_col(c) + &
                                           total_mass_flux_sub_col(c)     + &
                                           total_mass_flux_lateral_col(c) + &
                                           total_mass_flux_seepage_col(c)

                  total_mass_flux_lateral = total_mass_flux_lateral + &
                       total_mass_flux_lateral_col(c)
               enddo

               deallocate(lat_mass_exc_col_1d)
            endif
#endif

            ! Put the data in CLM's data structure
            mass_end        = 0.d0
            area            = 1.d0 ! [m^2]

            do fc = 1, l2e_num_hydrologyc
               c = l2e_filter_hydrologyc(fc)

               !if (lateral_connectivity) then
               !  g    = col%gridCell(c)
               !   area = ldomain_lateral%ugrid%areaGrid_ghosted(g)
               !endif

               ! initialization
               jwt = -1

               ! Loops in decreasing j so WTD can be computed in the same loop
               do j = nlevgrnd, 1, -1
                  idx = (c-begc)*nlevgrnd + j

                  e2l_h2osoi_liq(c,j) = (1.d0 - frac_ice(c,j))*vsfm_mass_col_1d(idx)/area
                  e2l_h2osoi_ice(c,j) = frac_ice(c,j)         *vsfm_mass_col_1d(idx)/area

                  mass_end        = mass_end        + vsfm_mass_col_1d(idx)
                  mass_end_col(c) = mass_end_col(c) + vsfm_mass_col_1d(idx)

                  vsfm_dmass_col(c) = vsfm_dmass_col(c) + &
                                      (vsfm_mass_col_1d(idx)-vsfm_mass_prev_col(c,j))

                  e2l_smp(c,j)    = vsfm_smpl_col_1d(idx)*1000.0_r8      ! [m] --> [mm]

                  if (jwt == -1) then
                     ! Find the first soil that is unsaturated
                     if (e2l_smp(c,j) < 0._r8) jwt = j
                  end if

               end do

               ! Find maximum water balance error over the column
               abs_mass_error_col = max(abs_mass_error_col,                     &
                                        abs(mass_beg_col(c) - mass_end_col(c) + &
                                            total_mass_flux_col(c)*dt))
               e2l_qrecharge(c) = 0._r8

               if (jwt == -1 .or. jwt == nlevgrnd) then
                  ! Water table below or in the last layer
                  e2l_wtd(c) = l2e_zi(c,nlevgrnd)
               else
                  z_dn = (l2e_zi(c,jwt-1) + l2e_zi(c,jwt  ))/2._r8
                  z_up = (l2e_zi(c,jwt ) + l2e_zi(c,jwt+1))/2._r8
                  e2l_wtd(c) = (0._r8 - e2l_smp(c,jwt))/(e2l_smp(c,jwt) - e2l_smp(c,jwt+1))*(z_dn - z_up) + z_dn
               endif
            end do

            ! Save soil liquid pressure from VSFM for all (active+nonactive) cells.
            ! soilp_col is used for restarting VSFM.
            do c = begc, endc
               do j = 1, nlevgrnd
                  idx = (c - begc)*nlevgrnd + j
                  e2l_soilp(c,j) = vsfm_soilp_col_1d(idx)
               end do
            end do

            ! For the solution that did converge, is the mass error acceptable?
            if (abs_mass_error_col >= max_abs_mass_error_col) then

               ! For the solution that converged, the mass error
               ! is unacceptable. So let's try again with tighter
               ! solution tolerance (stol) for SNES.

               mass_bal_err_count  = mass_bal_err_count + 1

               if (converged_reason == SNES_CONVERGED_FNORM_RELATIVE) then
                  rtol = rtol/10._r8
               else if (converged_reason == SNES_CONVERGED_SNORM_RELATIVE) then
                  stol = stol/10._r8
               endif

               dtime               = dt
               successful_step     = PETSC_FALSE
               abs_mass_error_col  = 0._r8
               mass_end_col(:)     = 0._r8

               ! Perform Pre-StepDT operations
               call vsfm_mpp%sysofeqns%PreStepDT()

            else

               successful_step  = PETSC_TRUE

            endif

         endif

         if (successful_step) exit

         if (iter_count >= max_iter_count) then
            write(iulog,*)'In soilwater_vsfm: VSFM failed to converge after multiple attempts.'
            call endrun(msg=errMsg(__FILE__, __LINE__))
         end if

      end do

#if 0
      ! Add seepage flux from VSFM to surface runoff
      do fc = 1, num_hydrologyc
         c = filter_hydrologyc(fc)

         qflx_surf(c) = qflx_surf(c) + qflx_seepage(c)
      enddo
#endif

      call SNESSetTolerances(vsfm_mpp%sysofeqns%snes, atol_default, rtol_default, stol_default, &
                             max_it_default, max_f_default, ierr); CHKERRQ(ierr)

      call vsfm_mpp%sysofeqns%PostStepDT()

#if VSFM_DEBUG
      write(iulog,*)'VSFM-DEBUG: nstep                      = ',get_nstep()
      write(iulog,*)'VSFM-DEBUG: dtime                      = ',dt
      write(iulog,*)'VSFM-DEBUG: change in mass between dt  = ',-(mass_beg - mass_end)
      write(iulog,*)'VSFM-DEBUG: change in mass due to flux = ',total_mass_flux*dt
      write(iulog,*)'VSFM-DEBUG: Error in mass conservation = ',mass_beg - mass_end + total_mass_flux*dt
      write(iulog,*)'VSFM-DEBUG: et_flux    * dtime         = ',total_mass_flux_et*dt
      write(iulog,*)'VSFM-DEBUG: infil_flux * dtime         = ',total_mass_flux_infl*dt
      write(iulog,*)'VSFM-DEBUG: dew_flux   * dtime         = ',total_mass_flux_dew*dt
      write(iulog,*)'VSFM-DEBUG: drain_flux * dtime         = ',total_mass_flux_drain*dt
      write(iulog,*)'VSFM-DEBUG: snow_flux  * dtime         = ',total_mass_flux_snowlyr*dt
      write(iulog,*)'VSFM-DEBUG: sub_flux   * dtime         = ',total_mass_flux_sub*dt
      write(iulog,*)'VSFM-DEBUG: lat_flux   * dtime         = ',total_mass_flux_lateral*dt
      write(iulog,*)'VSFM-DEBUG: total_mass_flux            = ',total_mass_flux/flux_unit_conversion
      write(iulog,*)'VSFM-DEBUG: et_flux                    = ',total_mass_flux_et
      write(iulog,*)'VSFM-DEBUG: infil_flux                 = ',total_mass_flux_infl
      write(iulog,*)'VSFM-DEBUG: dew_flux                   = ',total_mass_flux_dew
      write(iulog,*)'VSFM-DEBUG: drain_flux                 = ',total_mass_flux_drain
      write(iulog,*)'VSFM-DEBUG: snow_flux                  = ',total_mass_flux_snowlyr
      write(iulog,*)'VSFM-DEBUG: sub_flux                   = ',total_mass_flux_sub
      write(iulog,*)''
#endif

      deallocate(frac_ice                    )
      deallocate(total_mass_flux_col         )
      deallocate(total_mass_flux_et_col      )
      deallocate(total_mass_flux_infl_col    )
      deallocate(total_mass_flux_dew_col     )
      deallocate(total_mass_flux_drain_col   )
      deallocate(total_mass_flux_snowlyr_col )
      deallocate(total_mass_flux_sub_col     )
      deallocate(total_mass_flux_lateral_col )
      deallocate(total_mass_flux_seepage_col )
      deallocate(qflx_seepage                )
      deallocate(vsfm_mass_prev_col          )
      deallocate(vsfm_dmass_col              )
      deallocate(mass_beg_col                )
      deallocate(mass_end_col                )

      deallocate(mflx_et_col_1d              )
      deallocate(mflx_drain_col_1d           )
      deallocate(mflx_infl_col_1d            )
      deallocate(mflx_dew_col_1d             )
      deallocate(mflx_sub_snow_col_1d        )
      deallocate(mflx_snowlyr_col_1d         )
      deallocate(t_soil_col_1d               )

      deallocate(vsfm_mass_col_1d            )
      deallocate(vsfm_fliq_col_1d            )
      deallocate(vsfm_smpl_col_1d            )
      deallocate(vsfm_soilp_col_1d           )
      deallocate(vsfm_sat_col_1d             )

  end subroutine EM_VSFM_Solve


end module ExternalModelVSFMMod
