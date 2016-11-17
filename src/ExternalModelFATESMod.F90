module ExternalModelFATESMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! This module provides
  !
  use abortutils                   , only : endrun
  use shr_kind_mod                 , only : r8 => shr_kind_r8
  use shr_log_mod                  , only : errMsg => shr_log_errMsg
  use ExternalModelInterfaceDataMod, only : emi_data_list, emi_data

  !
  implicit none
  !

  integer :: index_l2e_col_gridcell_index
  integer :: index_l2e_col_patch_index

  integer :: index_l2e_flux_solad
  integer :: index_l2e_flux_solai

  integer :: index_e2l_state_fsun
  integer :: index_e2l_state_laisun
  integer :: index_e2l_state_laisha

  public :: EM_FATES_Populate_L2E_List, &
            EM_FATES_Populate_E2L_List, &
            EM_FATES_Solve

contains

  !------------------------------------------------------------------------
  subroutine EM_FATES_Populate_L2E_List(l2e_list)
    !
    ! !DESCRIPTION:
    ! Create a list of all variables needed by FATES from ALM
    !
    ! !USES:
    use ExternalModelConstants, only : EM_FATES_SUNFRAC_STAGE
    use ExternalModelConstants, only : L2E_FLUX_SOLAR_DIRECT_RADDIATION
    use ExternalModelConstants, only : L2E_FLUX_SOLAR_DIFFUSE_RADDIATION
    use ExternalModelConstants, only : L2E_COLUMN_GRIDCELL_INDEX
    use ExternalModelConstants, only : L2E_COLUMN_PATCH_INDEX
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list), intent(inout) :: l2e_list
    !

    call EM_FATES_Populate_L2E_List_For_Sunfrac_Stage(l2e_list)

  end subroutine EM_FATES_Populate_L2E_List

  !------------------------------------------------------------------------
  subroutine EM_FATES_Populate_E2L_List(e2l_list)
    !
    !
    ! !DESCRIPTION:
    ! Create a list of all variables to be returned by FATES from ALM
    !
    ! !USES:
    use ExternalModelConstants, only : EM_FATES_SUNFRAC_STAGE
    use ExternalModelConstants, only : E2L_STATE_FSUN
    use ExternalModelConstants, only : E2L_STATE_LAISUN
    use ExternalModelConstants, only : E2L_STATE_LAISHA
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list) , intent(inout) :: e2l_list

    call EM_FATES_Populate_E2L_List_For_Surfac_Stage(e2l_list)

  end subroutine EM_FATES_Populate_E2L_List

  !------------------------------------------------------------------------
  subroutine EM_FATES_Populate_L2E_List_For_Sunfrac_Stage(l2e_list)
    !
    ! !DESCRIPTION:
    ! Create a list of all variables needed by FATES from ALM for
    ! SUNFRAC computation
    !
    ! !USES:
    use ExternalModelConstants, only : EM_FATES_SUNFRAC_STAGE
    use ExternalModelConstants, only : L2E_FLUX_SOLAR_DIRECT_RADDIATION
    use ExternalModelConstants, only : L2E_FLUX_SOLAR_DIFFUSE_RADDIATION
    use ExternalModelConstants, only : L2E_COLUMN_GRIDCELL_INDEX
    use ExternalModelConstants, only : L2E_COLUMN_PATCH_INDEX
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
    em_stages(1) = EM_FATES_SUNFRAC_STAGE

    call l2e_list%AddDataByID(L2E_FLUX_SOLAR_DIRECT_RADDIATION , number_em_stages, em_stages, index_l2e_flux_solad)
    call l2e_list%AddDataByID(L2E_FLUX_SOLAR_DIFFUSE_RADDIATION, number_em_stages, em_stages, index_l2e_flux_solai)
    call l2e_list%AddDataByID(L2E_COLUMN_GRIDCELL_INDEX        , number_em_stages, em_stages, index_l2e_col_gridcell_index)
    call l2e_list%AddDataByID(L2E_COLUMN_PATCH_INDEX           , number_em_stages, em_stages, index_l2e_col_patch_index)

    deallocate(em_stages)

  end subroutine EM_FATES_Populate_L2E_List_For_Sunfrac_Stage

  !------------------------------------------------------------------------
  subroutine EM_FATES_Populate_E2L_List_For_Surfac_Stage(e2l_list)
    !
    ! !DESCRIPTION:
    ! Create a list of all variables to be returned by FATES from ALM
    ! after SUNFRAC computation
    !
    ! !USES:
    use ExternalModelConstants, only : EM_FATES_SUNFRAC_STAGE
    use ExternalModelConstants, only : E2L_STATE_FSUN
    use ExternalModelConstants, only : E2L_STATE_LAISUN
    use ExternalModelConstants, only : E2L_STATE_LAISHA
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
    em_stages(1) = EM_FATES_SUNFRAC_STAGE

    call e2l_list%AddDataByID(E2L_STATE_FSUN  , number_em_stages, em_stages, index_e2l_state_fsun   )
    call e2l_list%AddDataByID(E2L_STATE_LAISUN, number_em_stages, em_stages, index_e2l_state_laisun )
    call e2l_list%AddDataByID(E2L_STATE_LAISHA, number_em_stages, em_stages, index_e2l_state_laisha )

    deallocate(em_stages)

  end subroutine EM_FATES_Populate_E2L_List_For_Surfac_Stage

    !------------------------------------------------------------------------
  subroutine EM_FATES_Solve(em_stage, dt, nstep, clump_rank, l2e_list, e2l_list)
    !
    ! !DESCRIPTION:
    !
    ! !USES:
    use shr_kind_mod              , only : r8 => shr_kind_r8
    use abortutils                , only : endrun
    use shr_log_mod               , only : errMsg => shr_log_errMsg
    use ExternalModelConstants    , only : EM_FATES_SUNFRAC_STAGE
    use clm_varctl                , only : iulog
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
    integer              , intent(in)    :: clump_rank
    class(emi_data_list) , intent(in)    :: l2e_list
    class(emi_data_list) , intent(inout) :: e2l_list

    select case(em_stage)
    case (EM_FATES_SUNFRAC_STAGE)
       call EM_FATES_Sunfrac_Solve(clump_rank, l2e_list, e2l_list)
    case default
       write(iulog,*)'EM_FATES_Solve: Unknown em_stage.'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    end select

  end subroutine EM_FATES_Solve

    !------------------------------------------------------------------------
  subroutine EM_FATES_Sunfrac_Solve(clump_rank, l2e_list, e2l_list)
    !
    ! !DESCRIPTION:
    ! This interface function is a wrapper call on ED_SunShadeFracs. The only
    ! returned variable is a patch vector, fsun_patch, which describes the fraction
    ! of the canopy that is exposed to sun.
    !
    ! !USES:
    use shr_kind_mod              , only : r8 => shr_kind_r8
    use abortutils                , only : endrun
    use shr_log_mod               , only : errMsg => shr_log_errMsg
    use clm_varctl                , only : iulog
#ifdef FATES_VIA_EMI
    use clm_instMod               , only : clm_fates
    use EDSurfaceRadiationMod     , only : ED_SunShadeFracs
#endif
    !
    implicit none
    !
#include "finclude/petscsys.h"
#include "finclude/petscsnes.h"
#include "finclude/petscsnes.h90"
    !
    ! !ARGUMENTS:
    integer              , intent(in)    :: clump_rank
    class(emi_data_list) , intent(in)    :: l2e_list
    class(emi_data_list) , intent(inout) :: e2l_list
    !
    ! Local Variables
    real(r8)  , pointer                  :: l2e_solad(:,:)
    real(r8)  , pointer                  :: l2e_solai(:,:)
    real(r8)  , pointer                  :: e2l_fsun(:)
    real(r8)  , pointer                  :: e2l_laisun(:)
    real(r8)  , pointer                  :: e2l_laisha(:)
    integer   , pointer                  :: l2e_col_gridcell(:)
    integer   , pointer                  :: l2e_col_patchi(:)
    integer  :: p                           ! global index of the host patch
    integer  :: g                           ! global index of the host gridcell
    integer  :: c                           ! global index of the host column
    integer  :: s                           ! FATES site index
    integer  :: ifp                         ! FATEs patch index
                                            ! this is the order increment of patch
                                            ! on the site

    call l2e_list%GetPointerToInt1D(index_l2e_col_gridcell_index, l2e_col_gridcell)
    call l2e_list%GetPointerToInt1D(index_l2e_col_patch_index   , l2e_col_patchi)
    call l2e_list%GetPointerToReal2D(index_l2e_flux_solad       , l2e_solad)
    call l2e_list%GetPointerToReal2D(index_l2e_flux_solai       , l2e_solai)
    call e2l_list%GetPointerToReal1D(index_e2l_state_fsun       , e2l_fsun)
    call e2l_list%GetPointerToReal1D(index_e2l_state_laisun     , e2l_laisun)
    call e2l_list%GetPointerToReal1D(index_e2l_state_laisha     , e2l_laisha)

#ifdef FATES_VIA_EMI
        ! -------------------------------------------------------------------------------
        ! Convert input BC's
        ! The sun-shade calculations are performed only on FATES patches
        ! -------------------------------------------------------------------------------

    do s = 1, clm_fates%fates(clump_rank)%nsites
       c = clm_fates%f2hmap(clump_rank)%fcolumn(s)
       g = l2e_col_gridcell(c)

       do ifp = 1, clm_fates%fates(clump_rank)%sites(s)%youngest_patch%patchno

          p = ifp + l2e_col_patchi(c)
          clm_fates%fates(clump_rank)%bc_in(s)%solad_parb(ifp,:) = l2e_solad(g,:)
          clm_fates%fates(clump_rank)%bc_in(s)%solai_parb(ifp,:) = l2e_solai(g,:)

       end do
    end do

    ! -------------------------------------------------------------------------------
    ! Call FATES public function to calculate internal sun/shade structures
    ! as well as total patch sun/shade fraction output boundary condition
    ! -------------------------------------------------------------------------------

    call ED_SunShadeFracs(clm_fates%fates(clump_rank)%nsites, &
          clm_fates%fates(clump_rank)%sites,  &
          clm_fates%fates(clump_rank)%bc_in,  &
          clm_fates%fates(clump_rank)%bc_out)

    ! -------------------------------------------------------------------------------
    ! Transfer the FATES output boundary condition for canopy sun/shade fraction
    ! to the HLM
    ! -------------------------------------------------------------------------------

    do s = 1, clm_fates%fates(clump_rank)%nsites
       c = clm_fates%f2hmap(clump_rank)%fcolumn(s)
       do ifp = 1, clm_fates%fates(clump_rank)%sites(s)%youngest_patch%patchno
          p = ifp + l2e_col_patchi(c)
          e2l_fsun(p)   = clm_fates%fates(clump_rank)%bc_out(s)%fsun_pa(ifp)
          e2l_laisun(p) = clm_fates%fates(clump_rank)%bc_out(s)%laisun_pa(ifp)
          e2l_laisha(p) = clm_fates%fates(clump_rank)%bc_out(s)%laisha_pa(ifp)
       end do
    end do
#else
       call endrun('FATES is on but code was not compiled with -DFATES_VIA_EMI')
#endif

  end subroutine EM_FATES_Sunfrac_Solve

end module ExternalModelFATESMod
