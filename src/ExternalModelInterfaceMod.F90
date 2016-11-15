module ExternalModelInterfaceMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! This module provides an interface to couple ALM with external model
  !
  use shr_kind_mod                  , only : r8 => shr_kind_r8
  use spmdMod                       , only : masterproc, iam
  use shr_log_mod                   , only : errMsg => shr_log_errMsg
  use decompMod                     , only : bounds_type, get_proc_bounds 
  use abortutils                    , only : endrun
  use clm_varctl                    , only : iulog
  use MultiPhysicsProbVSFM          , only : vsfm_mpp
  use ExternalModelInterfaceDataMod , only : emi_data_list, emi_data
  !
  implicit none
  !
  private

  integer :: num_em              ! Number of external models

  ! Index of the various external models (EMs) in a simulation
  integer :: index_em_betr
  integer :: index_em_fates
  integer :: index_em_pflotran
  integer :: index_em_vsfm

  class(emi_data_list), pointer :: l2e_list(:)
  class(emi_data_list), pointer :: e2l_list(:)

  public :: EMI_Determine_Active_EMs
  public :: EMI_Init_EM
  public :: EMI_Driver

contains

  !-----------------------------------------------------------------------
  subroutine EMI_Determine_Active_EMs()
    !
    ! !DESCRIPTION:
    ! Determine which EMs are active
    !
    ! !USES:
    use clm_varctl, only : use_betr
    use clm_varctl, only : use_ed
    use clm_varctl, only : use_pflotran
    use clm_varctl, only : use_vsfm
    !
    implicit none
    !
    ! !LOCAL VARIABLES:
    integer :: iem

    ! Initializes
    num_em               = 0
    index_em_betr        = 0
    index_em_fates       = 0
    index_em_pflotran    = 0
    index_em_vsfm        = 0

    ! Is BeTR active?
    if (use_betr) then
       num_em            = num_em + 1
       index_em_betr     = num_em
    endif

    ! Is FATES active?
    if (use_ed) then
       num_em            = num_em + 1
       index_em_fates    = num_em
    endif

    ! Is PFLOTRAN active?
    if (use_pflotran) then
       num_em            = num_em + 1
       index_em_pflotran = num_em
    endif

    ! Is VSFM active?
    if (use_vsfm) then
       num_em            = num_em + 1
       index_em_vsfm     = num_em
    endif

    if ( masterproc ) then
       write(iulog,*) 'Number of Exteranl Models = ', num_em
       write(iulog,*) '  BeTR is present     ',(index_em_betr     >0)
       write(iulog,*) '  FATES is present    ',(index_em_fates    >0)
       write(iulog,*) '  PFLOTRAN is present ',(index_em_pflotran >0)
       write(iulog,*) '  VSFM is present     ',(index_em_vsfm     >0)
    endif

    if (num_em > 1) then
       call endrun(msg='More than 1 external model is not supported.')
    endif

    allocate(l2e_list(num_em))
    allocate(e2l_list(num_em))

    do iem = 1, 1, num_em
       call l2e_list(iem)%Init()
       call e2l_list(iem)%Init()
    enddo

  end subroutine EMI_Determine_Active_EMs
  
  !-----------------------------------------------------------------------
  subroutine EMI_Init_EM(em_id)
    !
    ! !DESCRIPTION:
    ! Initialize EMI
    !
    ! !USES:
    use ExternalModelConstants, only : EM_INITIALIZATION_STAGE
    use ExternalModelVSFMMod  , only : EM_VSFM_Populate_L2E_Init_List
    use ExternalModelVSFMMod  , only : EM_VSFM_Populate_E2L_Init_List
    use ExternalModelVSFMMod  , only : EM_VSFM_Populate_L2E_List
    use ExternalModelVSFMMod  , only : EM_VSFM_Populate_E2L_List
    use ExternalModelVSFMMod  , only : EM_VSFM_Init
    use ExternalModelConstants, only : EM_ID_BeTR
    use ExternalModelConstants, only : EM_ID_FATES
    use ExternalModelConstants, only : EM_ID_PFLOTRAN
    use ExternalModelConstants, only : EM_ID_VSFM
    use clm_instMod           , only : soilstate_vars
    use clm_instMod           , only : soilhydrology_vars
    use clm_instMod           , only : waterflux_vars
    use clm_instMod           , only : waterstate_vars
    !
    implicit none
    !
    ! !ARGUMENTS:
    integer, intent(in)  :: em_id
    !
    ! !LOCAL VARIABLES:
    class(emi_data_list), pointer :: l2e_init_list
    class(emi_data_list), pointer :: e2l_init_list
    integer                       :: em_stage
    integer                       :: ii
    integer                       :: num_filter_col
    integer                       :: num_filter_lun
    integer, pointer              :: filter_col(:)
    integer, pointer              :: filter_lun(:)
    type(bounds_type)             :: bounds_proc

    em_stage = EM_INITIALIZATION_STAGE

    select case (em_id)
    case (EM_ID_BeTR)

    case (EM_ID_FATES)

    case (EM_ID_PFLOTRAN)

    case (EM_ID_VSFM)
       ! Initialize EM

       ! Initialize lists of data to be exchanged between ALM and VSFM
       ! during initialization step
       allocate(l2e_init_list)
       allocate(e2l_init_list)

       call l2e_init_list%Init()
       call e2l_init_list%Init()

       ! Fill the data list:
       !  - Data need during the initialization
       call EM_VSFM_Populate_L2E_Init_List(l2e_init_list)
       call EM_VSFM_Populate_E2L_Init_List(e2l_init_list)
       !  - Data need during timestepping
       call EM_VSFM_Populate_L2E_List(l2e_list(index_em_vsfm))
       call EM_VSFM_Populate_E2L_List(e2l_list(index_em_vsfm))

       ! Allocate memory for data
       call EMI_Setup_Data_List(l2e_init_list)
       call EMI_Setup_Data_List(e2l_init_list)
       call EMI_Setup_Data_List(l2e_list(index_em_vsfm))
       call EMI_Setup_Data_List(e2l_list(index_em_vsfm))

       ! GB_FIX_ME: Create a temporary filter
       call get_proc_bounds(bounds_proc)
       num_filter_col = bounds_proc%endc - bounds_proc%begc + 1
       num_filter_lun = bounds_proc%endl - bounds_proc%begl + 1

       allocate(filter_col(num_filter_col))
       allocate(filter_lun(num_filter_lun))

       do ii = 1, num_filter_col
          filter_col(ii) = bounds_proc%begc + ii - 1
       enddo

       do ii = 1, num_filter_lun
          filter_lun(ii) = bounds_proc%begl + ii - 1
       enddo

       ! Reset values in the data list
       call EMID_Reset_Data_for_EM(l2e_init_list, em_stage)
       call EMID_Reset_Data_for_EM(e2l_init_list, em_stage)

       ! Pack all ALM data needed by the external model
       call EMID_Pack_WaterState_Vars_for_EM(l2e_init_list, em_stage, &
            num_filter_col, filter_col, waterstate_vars)
       call EMID_Pack_WaterFlux_Vars_for_EM(l2e_init_list, em_stage, &
            num_filter_col, filter_col, waterflux_vars)
       call EMID_Pack_SoilHydrology_Vars_for_EM(l2e_init_list, em_stage, &
            num_filter_col, filter_col, soilhydrology_vars)
       call EMID_Pack_SoilState_Vars_for_EM(l2e_init_list, em_stage, &
            num_filter_col, filter_col, soilstate_vars)
       call EMID_Pack_Column_for_EM(l2e_init_list, em_stage, &
            num_filter_col, filter_col)
       call EMID_Pack_Landunit_for_EM(l2e_init_list, em_stage, &
            num_filter_lun, filter_lun)

       ! Ensure all data needed by external model is packed
       call EMID_Verify_All_Data_Is_Set(l2e_init_list, em_stage)

       ! Initialize the external model
       call EM_VSFM_Init(l2e_init_list, e2l_init_list)

       ! Unpack all data sent from the external model
       call EMID_Unpack_SoilState_Vars_for_EM(e2l_init_list, em_stage, &
            num_filter_col, filter_col, soilstate_vars)
       call EMID_Unpack_WaterState_Vars_for_EM(e2l_init_list, em_stage, &
            num_filter_col, filter_col, waterstate_vars)
       call EMID_Unpack_WaterFlux_Vars_for_EM(e2l_init_list, em_stage, &
            num_filter_col, filter_col, waterflux_vars)
       call EMID_Unpack_SoilHydrology_Vars_for_EM(e2l_init_list, em_stage, &
            num_filter_col, filter_col, soilhydrology_vars)

       ! Ensure all data sent by external model is unpacked
       call EMID_Verify_All_Data_Is_Set(e2l_init_list, em_stage)

       ! Clean up memory
       call l2e_init_list%Destroy()
       call e2l_init_list%Destroy()

    case default
       call endrun('Unknown External Model')
    end select

  end subroutine EMI_Init_EM

  !-----------------------------------------------------------------------
  subroutine EMI_Setup_Data_List(data_list)
    !
    ! !DESCRIPTION:
    ! Setup the EMI data list
    !
    implicit none
    !
    ! !USES:
    class(emi_data_list) , intent(inout) :: data_list
    !
    ! !LOCAL VARIABLES:
    class(emi_data)      , pointer       :: cur_data
    integer                              :: idata

    allocate(data_list%data_ptr(data_list%num_data))

    idata = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit

       idata = idata + 1

       data_list%data_ptr(idata)%data => cur_data
       cur_data => cur_data%next
    enddo

    do idata = 1, data_list%num_data
       cur_data => data_list%data_ptr(idata)%data
       call EMI_Setup_Data(cur_data)
    enddo

    do idata = 1, data_list%num_data
       call data_list%data_ptr(idata)%data%AllocateMemory()
    enddo

  end subroutine EMI_Setup_Data_List

  !-----------------------------------------------------------------------
  subroutine EMI_Setup_Data(data)
    !
    ! !DESCRIPTION:
    ! Setup a EMI data
    !
    ! !USES:
    use ExternalModelConstants    , only : S_L2E_TSOIL
    use ExternalModelConstants    , only : S_L2E_H2OSOI_LIQ
    use ExternalModelConstants    , only : S_L2E_H2OSOI_ICE
    use ExternalModelConstants    , only : S_L2E_WTD
    use ExternalModelConstants    , only : S_L2E_VSFM_PROGNOSTIC_SOILP

    use ExternalModelConstants    , only : S_E2L_H2OSOI_LIQ
    use ExternalModelConstants    , only : S_E2L_H2OSOI_ICE
    use ExternalModelConstants    , only : S_E2L_SOIL_MATRIC_POTENTIAL
    use ExternalModelConstants    , only : S_E2L_WTD
    use ExternalModelConstants    , only : S_E2L_VSFM_PROGNOSTIC_SOILP

    use ExternalModelConstants    , only : F_L2E_INFIL_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_VERTICAL_ET_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_DEW_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_SNOW_SUBLIMATION_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_SNOW_LYR_DISAPPERANCE_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_RESTART_SNOW_LYR_DISAPPERANCE_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_DRAINAGE_MASS_FLUX

    use ExternalModelConstants    , only : F_E2L_AQUIFER_RECHARGE
    use ExternalModelConstants    , only : F_E2L_SNOW_LYR_DISAPPERANCE_MASS_FLUX

    use ExternalModelConstants    , only : FILTER_L2E_HYDROLOGYC
    use ExternalModelConstants    , only : FILTER_L2E_NUM_HYDROLOGYC

    use ExternalModelConstants    , only : COLUMN_L2E_ACTIVE
    use ExternalModelConstants    , only : COLUMN_L2E_TYPE
    use ExternalModelConstants    , only : COLUMN_L2E_LANDUNIT_INDEX
    use ExternalModelConstants    , only : COLUMN_L2E_ZI
    use ExternalModelConstants    , only : COLUMN_L2E_DZ
    use ExternalModelConstants    , only : COLUMN_L2E_Z
    use ExternalModelConstants    , only : COLUMN_L2E_AREA

    use ExternalModelConstants    , only : LANDUNIT_L2E_TYPE
    use ExternalModelConstants    , only : LANDUNIT_L2E_LAKEPOINT
    use ExternalModelConstants    , only : LANDUNIT_L2E_URBANPOINT

    use ExternalModelConstants    , only : PARAMETER_L2E_WATSATC
    use ExternalModelConstants    , only : PARAMETER_L2E_HKSATC
    use ExternalModelConstants    , only : PARAMETER_L2E_BSWC
    use ExternalModelConstants    , only : PARAMETER_L2E_SUCSATC
    use ExternalModelConstants    , only : PARAMETER_L2E_EFFPOROSITYC

    use decompMod                 , only : get_proc_bounds
    use decompMod                 , only : get_proc_clumps
    use decompMod                 , only : get_clump_bounds
    use clm_varpar                , only : nlevgrnd
    !
    implicit none
    !
    class(emi_data), pointer, intent(inout) :: data
    !
    ! !LOCAL VARIABLES:
    integer                                 :: ndim
    integer                                 :: nc
    integer                                 :: nclumps   ! Number of threads
    !
    integer, dimension(:), pointer          :: dim1_beg_clump, dim1_end_clump
    integer, dimension(:), pointer          :: dim2_beg_clump, dim2_end_clump
    integer, dimension(:), pointer          :: dim3_beg_clump, dim3_end_clump
    integer, dimension(:), pointer          :: dim4_beg_clump, dim4_end_clump
    !
    integer                                 :: dim1_beg_proc, dim1_end_proc
    integer                                 :: dim2_beg_proc, dim2_end_proc
    integer                                 :: dim3_beg_proc, dim3_end_proc
    integer                                 :: dim4_beg_proc, dim4_end_proc
    !
    logical                                 :: is_int_type, is_real_type
    !
    type(bounds_type)                       :: bounds_proc
    type(bounds_type)                       :: bounds_clump


    nclumps = get_proc_clumps()

    ndim = 0

    allocate(dim1_beg_clump(nclumps))
    allocate(dim2_beg_clump(nclumps))
    allocate(dim3_beg_clump(nclumps))
    allocate(dim4_beg_clump(nclumps))
    
    allocate(dim1_end_clump(nclumps))
    allocate(dim2_end_clump(nclumps))
    allocate(dim3_end_clump(nclumps))
    allocate(dim4_end_clump(nclumps))

    dim1_beg_clump(:) = 0
    dim2_beg_clump(:) = 0
    dim3_beg_clump(:) = 0
    dim4_beg_clump(:) = 0

    dim1_end_clump(:) = 0
    dim2_end_clump(:) = 0
    dim3_end_clump(:) = 0
    dim4_end_clump(:) = 0

    dim1_beg_proc  = 0
    dim2_beg_proc  = 0
    dim3_beg_proc  = 0
    dim4_beg_proc  = 0
    
    dim1_end_proc  = 0
    dim2_end_proc  = 0
    dim3_end_proc  = 0
    dim4_end_proc  = 0

    is_int_type    = .false.
    is_real_type   = .false.

    call get_proc_bounds(bounds_proc)

    ! Determine the size of data
    select case (data%id)

    case (S_L2E_TSOIL,                 &
          S_L2E_H2OSOI_LIQ,            &
          S_L2E_H2OSOI_ICE,            &
          S_E2L_H2OSOI_LIQ,            &
          S_L2E_VSFM_PROGNOSTIC_SOILP, &
          S_E2L_H2OSOI_ICE,            &
          S_E2L_SOIL_MATRIC_POTENTIAL, &
          S_E2L_VSFM_PROGNOSTIC_SOILP, &
          F_L2E_VERTICAL_ET_MASS_FLUX, &
          F_L2E_DRAINAGE_MASS_FLUX,    &
          COLUMN_L2E_DZ,               &
          COLUMN_L2E_Z,                &
          PARAMETER_L2E_WATSATC,       &
          PARAMETER_L2E_HKSATC,        &
          PARAMETER_L2E_BSWC,          &
          PARAMETER_L2E_SUCSATC,       &
          PARAMETER_L2E_EFFPOROSITYC   )

       ! Dim: Column x nlevgrnd

       ndim           = 2

       do nc = 1, nclumps
          call get_clump_bounds(nc, bounds_clump)
          dim1_beg_clump(nc) = bounds_clump%begc
          dim1_end_clump(nc) = bounds_clump%endc
       enddo

       dim2_beg_clump(:) = 1
       dim2_end_clump(:) = nlevgrnd

       dim1_beg_proc     = bounds_proc%begc
       dim1_end_proc     = bounds_proc%endc
       dim2_beg_proc     = 1
       dim2_end_proc     = nlevgrnd

    case (F_L2E_INFIL_MASS_FLUX,                 &
          S_L2E_WTD,                             &
          S_E2L_WTD,                             &
          FILTER_L2E_HYDROLOGYC,                 &
          F_L2E_DEW_MASS_FLUX,                   &
          F_L2E_SNOW_SUBLIMATION_MASS_FLUX,      &
          F_L2E_SNOW_LYR_DISAPPERANCE_MASS_FLUX, &
          F_L2E_RESTART_SNOW_LYR_DISAPPERANCE_MASS_FLUX, &
          F_E2L_AQUIFER_RECHARGE,                &
          F_E2L_SNOW_LYR_DISAPPERANCE_MASS_FLUX, &
          COLUMN_L2E_ACTIVE,                     &
          COLUMN_L2E_TYPE,                       &
          COLUMN_L2E_LANDUNIT_INDEX,             &
          COLUMN_L2E_AREA)

       ! Dim: Column

       ndim           = 1

       do nc = 1, nclumps
          call get_clump_bounds(nc, bounds_clump)
          dim1_beg_clump(nc) = bounds_clump%begc
          dim1_end_clump(nc) = bounds_clump%endc
       enddo

       dim1_beg_proc = bounds_proc%begc
       dim1_end_proc = bounds_proc%endc

    case (FILTER_L2E_NUM_HYDROLOGYC)

       ! Dim: 1

       ndim           = 1

       do nc = 1, nclumps
          call get_clump_bounds(nc, bounds_clump)
          dim1_beg_clump(nc) = 1
          dim1_end_clump(nc) = 1
       enddo

       dim1_beg_proc = 1
       dim1_end_proc = 1

    case (COLUMN_L2E_ZI)

       ! Dim: Column x (0:nlevgrnd)

       ndim           = 2

       do nc = 1, nclumps
          call get_clump_bounds(nc, bounds_clump)
          dim1_beg_clump(nc) = bounds_clump%begc
          dim1_end_clump(nc) = bounds_clump%endc
       enddo

       dim2_beg_clump(:) = 1
       dim2_end_clump(:) = nlevgrnd

       dim1_beg_proc     = bounds_proc%begc
       dim1_end_proc     = bounds_proc%endc
       dim2_beg_proc     = 0
       dim2_end_proc     = nlevgrnd

    case (LANDUNIT_L2E_TYPE,       &
          LANDUNIT_L2E_LAKEPOINT,  &
          LANDUNIT_L2E_URBANPOINT  &
         )

       ! Dim: Landunit

       ndim           = 1

       do nc = 1, nclumps
          call get_clump_bounds(nc, bounds_clump)
          dim1_beg_clump(nc) = bounds_clump%begl
          dim1_end_clump(nc) = bounds_clump%endl
       enddo

       dim1_beg_proc = bounds_proc%begl
       dim1_end_proc = bounds_proc%endl


    case default
       write(*,*)'Unknown data%id = ',data%id
       write(*,*)'Unknown data%name = ',trim(data%name)
       call endrun(msg='Unknown data%id while trying to set dimensions.')
    end select


    ! Determine the type of data
    select case (data%id)
       
    case (S_L2E_TSOIL,                           &
          S_L2E_H2OSOI_LIQ,                      &
          S_L2E_H2OSOI_ICE,                      &
          S_L2E_WTD,                             &
          S_L2E_VSFM_PROGNOSTIC_SOILP,           &
          S_E2L_H2OSOI_LIQ,                      &
          S_E2L_H2OSOI_ICE,                      &
          S_E2L_SOIL_MATRIC_POTENTIAL,           &
          S_E2L_VSFM_PROGNOSTIC_SOILP,           &
          S_E2L_WTD,                             &
          F_L2E_VERTICAL_ET_MASS_FLUX,           &
          F_L2E_DRAINAGE_MASS_FLUX,              &
          F_L2E_INFIL_MASS_FLUX,                 &
          F_L2E_DEW_MASS_FLUX,                   &
          F_L2E_SNOW_SUBLIMATION_MASS_FLUX,      &
          F_L2E_SNOW_LYR_DISAPPERANCE_MASS_FLUX, &
          F_L2E_RESTART_SNOW_LYR_DISAPPERANCE_MASS_FLUX, &
          F_E2L_AQUIFER_RECHARGE,                &
          F_E2L_SNOW_LYR_DISAPPERANCE_MASS_FLUX, &
          COLUMN_L2E_ZI,                         &
          COLUMN_L2E_DZ,                         &
          COLUMN_L2E_Z,                          &
          COLUMN_L2E_AREA,                       &
          PARAMETER_L2E_WATSATC,                 &
          PARAMETER_L2E_HKSATC,                  &
          PARAMETER_L2E_BSWC,                    &
          PARAMETER_L2E_SUCSATC,                 &
          PARAMETER_L2E_EFFPOROSITYC             &
          )

       is_int_type    = .false.
       is_real_type   = .true.

    case (FILTER_L2E_HYDROLOGYC,     &
          FILTER_L2E_NUM_HYDROLOGYC, &
          COLUMN_L2E_ACTIVE,         &
          COLUMN_L2E_TYPE,           &
          COLUMN_L2E_LANDUNIT_INDEX, &
          LANDUNIT_L2E_TYPE,         &
          LANDUNIT_L2E_LAKEPOINT,    &
          LANDUNIT_L2E_URBANPOINT )

       is_int_type    = .true.
       is_real_type   = .false.

    case default
       write(*,*)'Unknown data%id = ',data%id
       write(*,*)'Unknown data%name = ',trim(data%name)
       call endrun(msg='Unknown data%id while trying to specify data type.')
    end select

    call data%SetDimensions(ndim, nclumps,                               &
         dim1_beg_clump, dim1_end_clump, dim2_beg_clump, dim2_end_clump, &
         dim3_beg_clump, dim3_end_clump, dim4_beg_clump, dim4_end_clump, &
         dim1_beg_proc, dim1_end_proc, dim2_beg_proc, dim2_end_proc,     &
         dim3_beg_proc, dim3_end_proc, dim4_beg_proc, dim4_end_proc)

    call data%SetType(is_int_type, is_real_type)

  end subroutine EMI_Setup_Data

!-----------------------------------------------------------------------
  subroutine EMI_Driver(em_id, em_stage, dt, number_step,  &
       num_hydrologyc, filter_hydrologyc,                  &
       soilhydrology_vars, soilstate_vars, waterflux_vars, &
       waterstate_vars, temperature_vars)
    !
    ! !DESCRIPTION:
    !
    ! !USES:
    use ExternalModelConstants , only : EM_ID_BeTR
    use ExternalModelConstants , only : EM_ID_FATES
    use ExternalModelConstants , only : EM_ID_PFLOTRAN
    use ExternalModelConstants , only : EM_ID_VSFM
    use SoilStateType          , only : soilstate_type
    use SoilHydrologyType      , only : soilhydrology_type
    use TemperatureType        , only : temperature_type
    use WaterFluxType          , only : waterflux_type
    use WaterStateType         , only : waterstate_type
    use ExternalModelVSFMMod   , only : EM_VSFM_Solve
    !
    implicit none
    !
    integer                             , intent(in)    :: em_id
    integer                             , intent(in)    :: em_stage
    real(r8)                 , optional , intent(in)    :: dt
    integer                  , optional , intent(in)    :: number_step
    integer                  , optional , intent(in)    :: num_hydrologyc       ! number of column soil points in column filter
    integer                  , optional , intent(in)    :: filter_hydrologyc(:) ! column filter for soil points
    type(soilhydrology_type) , optional , intent(inout) :: soilhydrology_vars
    type(soilstate_type)     , optional , intent(inout) :: soilstate_vars
    type(waterflux_type)     , optional , intent(inout) :: waterflux_vars
    type(waterstate_type)    , optional , intent(inout) :: waterstate_vars
    type(temperature_type)   , optional , intent(inout) :: temperature_vars
    !
    integer  :: index_em
    real(r8) :: dtime
    integer  :: nstep

    ! Find the index_em
    select case (em_id)
    case (EM_ID_BeTR)
       index_em = index_em_betr
    case (EM_ID_FATES)
       index_em = index_em_fates
    case (EM_ID_PFLOTRAN)
       index_em = index_em_pflotran
    case (EM_ID_VSFM)
       index_em = index_em_vsfm
    case default
       call endrun('Unknown External Model')
    end select

    ! ------------------------------------------------------------------------
    ! Pack the data for EM
    ! ------------------------------------------------------------------------

    call EMID_Reset_Data_for_EM(l2e_list(index_em), em_stage)
    call EMID_Reset_Data_for_EM(e2l_list(index_em), em_stage)

    if ( present(temperature_vars) .and. &
         present(num_hydrologyc)   .and. &
         present(filter_hydrologyc)) then

       call EMID_Pack_Temperature_Vars_for_EM(l2e_list(index_em), em_stage, &
            num_hydrologyc, filter_hydrologyc, temperature_vars)
    endif

    if ( present(waterstate_vars) .and. &
         present(num_hydrologyc)  .and. &
         present(filter_hydrologyc)) then

       call EMID_Pack_WaterState_Vars_for_EM(l2e_list(index_em), em_stage, &
            num_hydrologyc, filter_hydrologyc, waterstate_vars)
    endif

    if ( present(waterflux_vars) .and. &
         present(num_hydrologyc) .and. &
         present(filter_hydrologyc)) then

       call EMID_Pack_WaterFlux_Vars_for_EM(l2e_list(index_em), em_stage, &
            num_hydrologyc, filter_hydrologyc, waterflux_vars)
    endif

    if ( present(num_hydrologyc) .and. &
         present(filter_hydrologyc)) then

       call EMID_Pack_Filter_for_EM(l2e_list(index_em), em_stage, &
            num_hydrologyc, filter_hydrologyc)

       call EMID_Pack_Column_for_EM(l2e_list(index_em), em_stage, &
            num_hydrologyc, filter_hydrologyc)

    endif

    call EMID_Verify_All_Data_Is_Set(l2e_list(index_em), em_stage)

    ! ------------------------------------------------------------------------
    ! Solve EM
    ! ------------------------------------------------------------------------
    nstep = 0
    dtime = 0._r8
    if (present(number_step)) nstep = number_step
    if (present(dt))          dtime = dt

    select case (em_id)
    case (EM_ID_BeTR)
    case (EM_ID_FATES)
    case (EM_ID_PFLOTRAN)
    case (EM_ID_VSFM)
       call EM_VSFM_Solve(em_stage, dtime, nstep, l2e_list(index_em), e2l_list(index_em))
    case default
       call endrun('Unknown External Model')
    end select

    ! ------------------------------------------------------------------------
    ! Unpack the data for EM
    ! ------------------------------------------------------------------------
    if ( present(waterstate_vars) .and. &
         present(num_hydrologyc)  .and. &
         present(filter_hydrologyc)) then

       call EMID_Unpack_WaterState_Vars_for_EM(e2l_list(index_em), em_stage, &
            num_hydrologyc, filter_hydrologyc, waterstate_vars)
    endif

    if ( present(waterflux_vars) .and. &
         present(num_hydrologyc) .and. &
         present(filter_hydrologyc)) then

       call EMID_Unpack_WaterFlux_Vars_for_EM(e2l_list(index_em), em_stage, &
            num_hydrologyc, filter_hydrologyc, waterflux_vars)
    endif

    if ( present(soilstate_vars) .and. &
         present(num_hydrologyc) .and. &
         present(filter_hydrologyc)) then

       call EMID_Unpack_SoilState_Vars_for_EM(e2l_list(index_em), em_stage, &
            num_hydrologyc, filter_hydrologyc, soilstate_vars)
    endif

    if ( present(soilhydrology_vars) .and. &
         present(num_hydrologyc)     .and. &
         present(filter_hydrologyc)) then

       call EMID_Unpack_SoilHydrology_Vars_for_EM(e2l_list(index_em), em_stage, &
            num_hydrologyc, filter_hydrologyc, soilhydrology_vars)
    endif

    call EMID_Verify_All_Data_Is_Set(l2e_list(index_em), em_stage)

  end subroutine EMI_Driver
  
!-----------------------------------------------------------------------
  subroutine EMID_Reset_Data_for_EM(data_list, em_stage)
    !
    ! !DESCRIPTION:
    ! Reset all EMI data that will be exchanged between ALM and external
    ! model for em_stage
    !
    implicit none
    !
    class(emi_data_list)   , intent(in) :: data_list
    integer                , intent(in) :: em_stage
    !
    class(emi_data), pointer            :: cur_data
    integer                             :: istage

    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit

       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             call cur_data%Reset()
             exit
          endif
       enddo

       cur_data => cur_data%next
    enddo

  end subroutine EMID_Reset_Data_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Verify_All_Data_Is_Set(data_list, em_stage)
    !
    ! !DESCRIPTION:
    ! Verify that all EMI data that will be exchanged between ALM and external
    ! model for em_stage was set
    !
    implicit none
    !
    class(emi_data_list)   , intent(in) :: data_list
    integer                , intent(in) :: em_stage
    !
    class(emi_data), pointer            :: cur_data
    integer                             :: istage

    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit

       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             if (.not. cur_data%is_set) then
                write(iulog,*)'EMID%id   = ', cur_data%id
                write(iulog,*)'EMID%name = ', trim(cur_data%name)
                call endrun(msg='EMID is not set.')
             endif
             exit
          endif
       enddo

       cur_data => cur_data%next
    enddo

  end subroutine EMID_Verify_All_Data_Is_Set

!-----------------------------------------------------------------------
  subroutine EMID_Pack_WaterFlux_Vars_for_EM(data_list, em_stage, &
        num_hydrologyc, filter_hydrologyc, waterflux_vars)
    !
    ! !DESCRIPTION:
    ! Pack data from ALM's waterflux_vars for EM
    !
    ! !USES:
    use ExternalModelConstants    , only : F_L2E_INFIL_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_VERTICAL_ET_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_DEW_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_SNOW_SUBLIMATION_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_SNOW_LYR_DISAPPERANCE_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_RESTART_SNOW_LYR_DISAPPERANCE_MASS_FLUX
    use ExternalModelConstants    , only : F_L2E_DRAINAGE_MASS_FLUX
    use WaterFluxType             , only : waterflux_type
    use clm_varpar                , only : nlevsoi, nlevgrnd
    !
    implicit none
    !
    class(emi_data_list) , intent(in) :: data_list
    integer              , intent(in) :: em_stage
    integer              , intent(in) :: num_hydrologyc       ! number of column soil points in column filter
    integer              , intent(in) :: filter_hydrologyc(:) ! column filter for soil points
    type(waterflux_type) , intent(in) :: waterflux_vars
    !
    integer                           :: c,fc,j
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_pack
    integer                           :: istage
    integer                           :: count

    associate(& 
         mflx_infl_col         => waterflux_vars%mflx_infl_col         , &
         mflx_dew_col          => waterflux_vars%mflx_dew_col          , &
         mflx_snowlyr_disp_col => waterflux_vars%mflx_snowlyr_disp_col , &
         mflx_snowlyr_col      => waterflux_vars%mflx_snowlyr_col      , &
         mflx_sub_snow_col     => waterflux_vars%mflx_sub_snow_col     , &
         mflx_et_col           => waterflux_vars%mflx_et_col           , &
         mflx_drain_col        => waterflux_vars%mflx_drain_col          &
         )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_pack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_pack = .true.
             exit
          endif
       enddo

       if (need_to_pack) then

          select case (cur_data%id)

          case (F_L2E_VERTICAL_ET_MASS_FLUX)

             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevsoi
                   cur_data%data_real_2d(c,j) = mflx_et_col(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (F_L2E_INFIL_MASS_FLUX)

             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                cur_data%data_real_1d(c) = mflx_infl_col(c)
             enddo
             cur_data%is_set = .true.

          case (F_L2E_DEW_MASS_FLUX)

             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                cur_data%data_real_1d(c) = mflx_dew_col(c)
             enddo
             cur_data%is_set = .true.

          case (F_L2E_SNOW_SUBLIMATION_MASS_FLUX)

             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                cur_data%data_real_1d(c) = mflx_sub_snow_col(c)
             enddo
             cur_data%is_set = .true.

          case (F_L2E_SNOW_LYR_DISAPPERANCE_MASS_FLUX)

             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                cur_data%data_real_1d(c) = mflx_snowlyr_disp_col(c)
             enddo
             cur_data%is_set = .true.

          case (F_L2E_RESTART_SNOW_LYR_DISAPPERANCE_MASS_FLUX)

             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                cur_data%data_real_1d(c) = mflx_snowlyr_col(c)
             enddo
             cur_data%is_set = .true.

          case (F_L2E_DRAINAGE_MASS_FLUX)

             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = mflx_drain_col(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMID_Pack_WaterFlux_Vars_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Unpack_WaterFlux_Vars_for_EM(data_list, em_stage, &
        num_hydrologyc, filter_hydrologyc, waterflux_vars)
    !
    ! !DESCRIPTION:
    ! Pack data from EM in ALM's waterflux_vars
    !
    ! !USES:
    use ExternalModelConstants    , only : F_E2L_AQUIFER_RECHARGE
    use ExternalModelConstants    , only : F_E2L_SNOW_LYR_DISAPPERANCE_MASS_FLUX
    use WaterFluxType             , only : waterflux_type
    use clm_varpar                , only : nlevsoi, nlevgrnd
    !
    implicit none
    !
    class(emi_data_list) , intent(in) :: data_list
    integer              , intent(in) :: em_stage
    integer              , intent(in) :: num_hydrologyc       ! number of column soil points in column filter
    integer              , intent(in) :: filter_hydrologyc(:) ! column filter for soil points
    type(waterflux_type) , intent(in) :: waterflux_vars
    !
    integer                           :: c,fc,j
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_unpack
    integer                           :: istage
    integer                           :: count

    associate( &
         mflx_recharge_col   => waterflux_vars%mflx_recharge_col, &
         mflx_snowlyr_col    => waterflux_vars%mflx_snowlyr_col   &
    )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_unpack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_unpack = .true.
             exit
          endif
       enddo

       if (need_to_unpack) then

          select case (cur_data%id)

          case (F_E2L_AQUIFER_RECHARGE)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                mflx_recharge_col(c) = cur_data%data_real_1d(c)
             enddo
             cur_data%is_set = .true.

          case (F_E2L_SNOW_LYR_DISAPPERANCE_MASS_FLUX)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                mflx_snowlyr_col(c) = cur_data%data_real_1d(c)
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMID_Unpack_WaterFlux_Vars_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Pack_Filter_for_EM(data_list, em_stage, &
        num_filter, filter)
    !
    ! !DESCRIPTION:
    ! Pack data from ALM's filter for EM
    !
    ! !USES:
    use ExternalModelConstants    , only : FILTER_L2E_HYDROLOGYC
    use ExternalModelConstants    , only : FILTER_L2E_NUM_HYDROLOGYC
    !
    implicit none
    !
    class(emi_data_list) , intent(in) :: data_list
    integer              , intent(in) :: em_stage
    integer              , intent(in) :: num_filter ! number of column soil points in column filter
    integer              , intent(in) :: filter(:)  ! column filter for soil points
    !
    integer                           :: i
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_pack
    integer                           :: istage
    integer                           :: count

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_pack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_pack = .true.
             exit
          endif
       enddo

       if (need_to_pack) then

          select case (cur_data%id)

          case (FILTER_L2E_HYDROLOGYC)
             do i = 1, num_filter
                cur_data%data_int_1d(i) = filter(i)
             enddo
             cur_data%is_set = .true.

          case (FILTER_L2E_NUM_HYDROLOGYC)

             cur_data%data_int_1d(1) = num_filter
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

  end subroutine EMID_Pack_Filter_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Pack_Column_for_EM(data_list, em_stage, &
        num_filter, filter)
    !
    ! !DESCRIPTION:
    ! Pack data from ALM's column type for EM
    !
    ! !USES:
    use ExternalModelConstants    , only : COLUMN_L2E_ACTIVE
    use ExternalModelConstants    , only : COLUMN_L2E_TYPE
    use ExternalModelConstants    , only : COLUMN_L2E_LANDUNIT_INDEX
    use ExternalModelConstants    , only : COLUMN_L2E_ZI
    use ExternalModelConstants    , only : COLUMN_L2E_DZ
    use ExternalModelConstants    , only : COLUMN_L2E_Z
    use ExternalModelConstants    , only : COLUMN_L2E_AREA
    use ColumnType                , only : col
    use clm_varpar                , only : nlevgrnd
    !
    implicit none
    !
    class(emi_data_list) , intent(in) :: data_list
    integer              , intent(in) :: em_stage
    integer              , intent(in) :: num_filter ! number of column soil points in column filter
    integer              , intent(in) :: filter(:)  ! column filter for soil points
    !
    integer                           :: fc,c,j
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_pack
    integer                           :: istage
    integer                           :: count

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_pack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_pack = .true.
             exit
          endif
       enddo

       if (need_to_pack) then

          select case (cur_data%id)

          case (COLUMN_L2E_ACTIVE)
             do fc = 1, num_filter
                c = filter(fc)
                if (col%active(c)) cur_data%data_int_1d(c) = 1
             enddo
             cur_data%is_set = .true.

          case (COLUMN_L2E_TYPE)
             do fc = 1, num_filter
                c = filter(fc)
                cur_data%data_int_1d(c) = col%itype(c)
             enddo
             cur_data%is_set = .true.

          case (COLUMN_L2E_LANDUNIT_INDEX)
             do fc = 1, num_filter
                c = filter(fc)
                cur_data%data_int_1d(c) = col%landunit(c)
             enddo
             cur_data%is_set = .true.

          case (COLUMN_L2E_ZI)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 0, nlevgrnd
                   cur_data%data_real_2d(c,j) = col%zi(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (COLUMN_L2E_DZ)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = col%dz(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (COLUMN_L2E_Z)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = col%z(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (COLUMN_L2E_AREA)
             do fc = 1, num_filter
                c = filter(fc)
                !cur_data%data_real_1d(c) = col%area(c)
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

  end subroutine EMID_Pack_Column_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Pack_Landunit_for_EM(data_list, em_stage, &
        num_filter, filter)
    !
    ! !DESCRIPTION:
    ! Pack data from ALM's landunit type for EM
    !
    ! !USES:
    use ExternalModelConstants    , only : LANDUNIT_L2E_TYPE
    use ExternalModelConstants    , only : LANDUNIT_L2E_LAKEPOINT
    use ExternalModelConstants    , only : LANDUNIT_L2E_URBANPOINT
    use LandunitType              , only : lun
    !
    implicit none
    !
    class(emi_data_list) , intent(in) :: data_list
    integer              , intent(in) :: em_stage
    integer              , intent(in) :: num_filter ! number of column soil points in column filter
    integer              , intent(in) :: filter(:)  ! column filter for soil points
    !
    integer                           :: fl,l
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_pack
    integer                           :: istage
    integer                           :: count

    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit

       need_to_pack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_pack = .true.
             exit
          endif
       enddo

       if (need_to_pack) then

          select case (cur_data%id)

          case (LANDUNIT_L2E_TYPE)
             do fl = 1, num_filter
                l = filter(fl)
                cur_data%data_int_1d(l) = lun%itype(l)
             enddo
             cur_data%is_set = .true.

          case (LANDUNIT_L2E_LAKEPOINT)
             do fl = 1, num_filter
                l = filter(fl)
                if (lun%lakpoi(l)) cur_data%data_int_1d(l) = 1
             enddo
             cur_data%is_set = .true.

          case (LANDUNIT_L2E_URBANPOINT)
             do fl = 1, num_filter
                l = filter(fl)
                if (lun%urbpoi(l)) cur_data%data_int_1d(l) = 1
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

  end subroutine EMID_Pack_Landunit_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Pack_WaterState_Vars_for_EM(data_list, em_stage, &
        num_hydrologyc, filter_hydrologyc, waterstate_vars)
    !
    ! !DESCRIPTION:
    ! Pack data from ALM's waterstate_vars for EM
    !
    ! !USES:
    use ExternalModelConstants    , only : S_L2E_H2OSOI_LIQ
    use ExternalModelConstants    , only : S_L2E_H2OSOI_ICE
    use ExternalModelConstants    , only : S_L2E_VSFM_PROGNOSTIC_SOILP
    use WaterStateType            , only : waterstate_type
    use clm_varpar                , only : nlevgrnd
    !
    implicit none
    !
    class(emi_data_list) , intent(in) :: data_list
    integer              , intent(in) :: em_stage
    integer              , intent(in) :: num_hydrologyc       ! number of column soil points in column filter
    integer              , intent(in) :: filter_hydrologyc(:) ! column filter for soil points
    type(waterstate_type), intent(in) :: waterstate_vars
    !
    integer                           :: c,fc,j
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_pack
    integer                           :: istage
    integer                           :: count

    associate(& 
         h2osoi_ice =>    waterstate_vars%h2osoi_ice_col , & ! Input:  [real(r8) (:,:) ]  ice water (kg/m2)
         h2osoi_liq =>    waterstate_vars%h2osoi_liq_col , & ! Input:  [real(r8) (:,:) ]  liquid water (kg/m2)
         soilp_col  =>    waterstate_vars%soilp_col        & ! Input:  [real(r8) (:,:) ]  soil water pressure (Pa)
         )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_pack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_pack = .true.
             exit
          endif
       enddo

       if (need_to_pack) then

          select case (cur_data%id)

          case (S_L2E_H2OSOI_LIQ)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = h2osoi_liq(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (S_L2E_H2OSOI_ICE)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = h2osoi_ice(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (S_L2E_VSFM_PROGNOSTIC_SOILP)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = soilp_col(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMID_Pack_WaterState_Vars_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Unpack_WaterState_Vars_for_EM(data_list, em_stage, &
        num_hydrologyc, filter_hydrologyc, waterstate_vars)
    !
    ! !DESCRIPTION:
    ! Save data from EM in ALM's waterflux_vars
    !
    ! !USES:
    use ExternalModelConstants    , only : S_E2L_H2OSOI_LIQ
    use ExternalModelConstants    , only : S_E2L_H2OSOI_ICE
    use ExternalModelConstants    , only : S_E2L_VSFM_PROGNOSTIC_SOILP
    use WaterStateType            , only : waterstate_type
    use clm_varpar                , only : nlevgrnd
    !
    implicit none
    !
    class(emi_data_list) , intent(in) :: data_list
    integer              , intent(in) :: em_stage
    integer              , intent(in) :: num_hydrologyc       ! number of column soil points in column filter
    integer              , intent(in) :: filter_hydrologyc(:) ! column filter for soil points
    type(waterstate_type), intent(in) :: waterstate_vars
    !
    integer                           :: c,fc,j
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_unpack
    integer                           :: istage
    integer                           :: count

    associate(& 
         h2osoi_ice =>    waterstate_vars%h2osoi_ice_col , & ! Output:  [real(r8) (:,:) ]  ice water (kg/m2)
         h2osoi_liq =>    waterstate_vars%h2osoi_liq_col , & ! Output:  [real(r8) (:,:) ]  liquid water (kg/m2)
         soilp_col  =>    waterstate_vars%soilp_col        & ! Output: [real(r8) (:,:) ]  soil water pressure (Pa)
         )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_unpack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_unpack = .true.
             exit
          endif
       enddo

       if (need_to_unpack) then

          select case (cur_data%id)

          case (S_E2L_H2OSOI_LIQ)

             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   h2osoi_liq(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (S_E2L_H2OSOI_ICE)

             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   h2osoi_ice(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (S_E2L_VSFM_PROGNOSTIC_SOILP)

             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   soilp_col(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMID_Unpack_WaterState_Vars_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Pack_Temperature_Vars_for_EM(data_list, em_stage, &
        num_hydrologyc, filter_hydrologyc, temperature_vars)
    !
    ! !DESCRIPTION:
    ! Pack data from ALM's temperature_vars for EM
    !
    ! !USES:
    use ExternalModelConstants , only : S_L2E_TSOIL
    use TemperatureType        , only : temperature_type
    use clm_varpar             , only : nlevgrnd
    !
    implicit none
    !
    class(emi_data_list)   , intent(in) :: data_list
    integer                , intent(in) :: em_stage
    integer                , intent(in) :: num_hydrologyc       ! number of column soil points in column filter
    integer                , intent(in) :: filter_hydrologyc(:) ! column filter for soil points
    type(temperature_type) , intent(in) :: temperature_vars
    !
    integer                             :: c,fc,j
    class(emi_data), pointer            :: cur_data
    logical                             :: need_to_pack
    integer                             :: istage
    integer                             :: count

    associate(& 
         t_soisno => temperature_vars%t_soisno_col                & ! Input:  [real(r8) (:,:) ]  soil temperature (Kelvin)
         )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_pack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_pack = .true.
             exit
          endif
       enddo

       if (need_to_pack) then

          select case (cur_data%id)

          case (S_L2E_TSOIL)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = t_soisno(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMID_Pack_Temperature_Vars_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Pack_SoilHydrology_Vars_for_EM(data_list, em_stage, &
        num_hydrologyc, filter_hydrologyc, soilhydrology_vars)
    !
    ! !DESCRIPTION:
    ! Save data from EM in ALM's soilhydrolgy_vars
    !
    ! !USES:
    use ExternalModelConstants    , only : S_L2E_WTD
    use SoilHydrologyType         , only : soilhydrology_type
    !
    implicit none
    !
    class(emi_data_list)     , intent(inout) :: data_list
    integer                  , intent(in)    :: em_stage
    integer                  , intent(in)    :: num_hydrologyc       ! number of column soil points in column filter
    integer                  , intent(in)    :: filter_hydrologyc(:) ! column filter for soil points
    type(soilhydrology_type) , intent(in)    :: soilhydrology_vars
    !
    integer                           :: c,fc,j
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_pack
    integer                           :: istage
    integer                           :: count

    associate( &
         zwt => soilhydrology_vars%zwt_col &
    )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_pack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_pack = .true.
             exit
          endif
       enddo

       if (need_to_pack) then

          select case (cur_data%id)

          case (S_L2E_WTD)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                cur_data%data_real_1d(c) = zwt(c)
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMID_Pack_SoilHydrology_Vars_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Unpack_SoilHydrology_Vars_for_EM(data_list, em_stage, &
        num_hydrologyc, filter_hydrologyc, soilhydrology_vars)
    !
    ! !DESCRIPTION:
    ! Save data from EM in ALM's soilhydrolgy_vars
    !
    ! !USES:
    use ExternalModelConstants    , only : S_E2L_WTD
    use ExternalModelConstants    , only : F_E2L_AQUIFER_RECHARGE
    use SoilHydrologyType         , only : soilhydrology_type
    !
    implicit none
    !
    class(emi_data_list)     , intent(in) :: data_list
    integer                  , intent(in) :: em_stage
    integer                  , intent(in) :: num_hydrologyc       ! number of column soil points in column filter
    integer                  , intent(in) :: filter_hydrologyc(:) ! column filter for soil points
    type(soilhydrology_type) , intent(in) :: soilhydrology_vars
    !
    integer                           :: c,fc,j
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_unpack
    integer                           :: istage
    integer                           :: count

    associate( &
         qcharge            =>    soilhydrology_vars%qcharge_col        , & ! Output:  [real(r8) (:)   ]  aquifer recharge rate (mm/s)
         zwt => soilhydrology_vars%zwt_col &
    )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_unpack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_unpack = .true.
             exit
          endif
       enddo

       if (need_to_unpack) then

          select case (cur_data%id)

          case (S_E2L_WTD)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                zwt(c) = cur_data%data_real_1d(c)
             enddo
             cur_data%is_set = .true.

          case (F_E2L_AQUIFER_RECHARGE)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                qcharge(c) = cur_data%data_real_1d(c)
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMID_Unpack_SoilHydrology_Vars_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Pack_SoilState_Vars_for_EM(data_list, em_stage, &
        num_hydrologyc, filter_hydrologyc, soilstate_vars)
    !
    ! !DESCRIPTION:
    ! Save data from EM in ALM's soilstate_vars
    !
    ! !USES:
    use ExternalModelConstants    , only : S_E2L_SOIL_MATRIC_POTENTIAL
    use ExternalModelConstants    , only : PARAMETER_L2E_WATSATC
    use ExternalModelConstants    , only : PARAMETER_L2E_HKSATC
    use ExternalModelConstants    , only : PARAMETER_L2E_BSWC
    use ExternalModelConstants    , only : PARAMETER_L2E_SUCSATC
    use ExternalModelConstants    , only : PARAMETER_L2E_EFFPOROSITYC
    use SoilStateType             , only : soilstate_type
    use clm_varpar                , only : nlevsoi, nlevgrnd
    !
    implicit none
    !
    class(emi_data_list) , intent(inout) :: data_list
    integer              , intent(in)    :: em_stage
    integer              , intent(in)    :: num_hydrologyc       ! number of column soil points in column filter
    integer              , intent(in)    :: filter_hydrologyc(:) ! column filter for soil points
    type(soilstate_type) , intent(in)    :: soilstate_vars
    !
    integer                           :: c,fc,j
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_pack
    integer                           :: istage
    integer                           :: count

    associate( &
         watsat       => soilstate_vars%watsat_col,      &
         hksat        => soilstate_vars%hksat_col,       &
         bsw          => soilstate_vars%bsw_col,         &
         sucsat       => soilstate_vars%sucsat_col,      &
         eff_porosity => soilstate_vars%eff_porosity_col &
    )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_pack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_pack = .true.
             exit
          endif
       enddo

       if (need_to_pack) then

          select case (cur_data%id)

          case (PARAMETER_L2E_WATSATC)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = watsat(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (PARAMETER_L2E_HKSATC)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = hksat(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (PARAMETER_L2E_BSWC)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = bsw(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (PARAMETER_L2E_SUCSATC)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = sucsat(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (PARAMETER_L2E_EFFPOROSITYC)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   cur_data%data_real_2d(c,j) = eff_porosity(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMID_Pack_SoilState_Vars_for_EM

!-----------------------------------------------------------------------
  subroutine EMID_Unpack_SoilState_Vars_for_EM(data_list, em_stage, &
        num_hydrologyc, filter_hydrologyc, soilstate_vars)
    !
    ! !DESCRIPTION:
    ! Save data from EM in ALM's soilstate_vars
    !
    ! !USES:
    use ExternalModelConstants    , only : S_E2L_SOIL_MATRIC_POTENTIAL
    use SoilStateType             , only : soilstate_type
    use clm_varpar                , only : nlevsoi, nlevgrnd
    !
    implicit none
    !
    class(emi_data_list) , intent(in) :: data_list
    integer              , intent(in) :: em_stage
    integer              , intent(in) :: num_hydrologyc       ! number of column soil points in column filter
    integer              , intent(in) :: filter_hydrologyc(:) ! column filter for soil points
    type(soilstate_type) , intent(in) :: soilstate_vars
    !
    integer                           :: c,fc,j
    class(emi_data), pointer          :: cur_data
    logical                           :: need_to_unpack
    integer                           :: istage
    integer                           :: count

    associate( &
         smp_l        => soilstate_vars%smp_l_col &
    )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_unpack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_unpack = .true.
             exit
          endif
       enddo

       if (need_to_unpack) then

          select case (cur_data%id)

          case (S_E2L_SOIL_MATRIC_POTENTIAL)
             do fc = 1, num_hydrologyc
                c = filter_hydrologyc(fc)
                do j = 1, nlevgrnd
                   smp_l(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMID_Unpack_SoilState_Vars_for_EM

end module ExternalModelInterfaceMod
