module ExternalModelInterfaceDataMod
  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! This module provides
  !
  use shr_kind_mod         , only : r8 => shr_kind_r8
  use shr_log_mod          , only : errMsg => shr_log_errMsg
  use abortutils           , only : endrun
  use clm_varctl           , only : iulog
  !
  implicit none
  !
  type, public :: emi_data

     !private

     integer             :: id                  ! ID
     character (len=32)  :: name                ! Short name
     character (len=128) :: long_name           ! Long name of data
     character (len=24)  :: units               ! Units

     character (len=1)   :: avgflag             ! Needed for output to history file

     logical             :: is_int_type         ! Is data of an integer type?
     logical             :: is_real_type        ! Is data of real type?
     logical             :: is_set              ! Is data set

     integer             :: num_em_stages       ! Number of EM stages in which the data is exchanged
     integer, pointer    :: em_stage_ids(:)     ! ID of EM stages in which the data is exchanged

     integer             :: ndim                ! Dimension of the data

     ! Dimension name of the data (e.g. 'column','levscpf', etc).
     character (len=10) :: dim1_name
     character (len=10) :: dim2_name           
     character (len=10) :: dim3_name
     character (len=10) :: dim4_name

     integer, dimension(:), pointer :: dim1_beg_clump, dim1_end_clump
     integer, dimension(:), pointer :: dim2_beg_clump, dim2_end_clump
     integer, dimension(:), pointer :: dim3_beg_clump, dim3_end_clump
     integer, dimension(:), pointer :: dim4_beg_clump, dim4_end_clump

     integer :: dim1_beg_proc, dim1_end_proc
     integer :: dim2_beg_proc, dim2_end_proc
     integer :: dim3_beg_proc, dim3_end_proc
     integer :: dim4_beg_proc, dim4_end_proc

     integer, pointer :: data_int_1d(:)
     integer, pointer :: data_int_2d(:,:)
     integer, pointer :: data_int_3d(:,:,:)

     real(r8), pointer :: data_real_1d(:)
     real(r8), pointer :: data_real_2d(:,:)
     real(r8), pointer :: data_real_3d(:,:,:)
     real(r8), pointer :: data_real_4d(:,:,:,:)

     type(emi_data), pointer :: next

   contains

     procedure, public :: Init             => EMIDInit
     procedure, public :: Copy             => EMIDCopy
     procedure, public :: Setup            => EMIDSetup
     procedure, public :: SetDimensions    => EMIDSetDimensions
     procedure, public :: SetType          => EMIDSetType
     procedure, public :: SetID            => EMIDSetID
     procedure, public :: SetName          => EMIDSetName
     procedure, public :: SetUnits         => EMIDSetUnits
     procedure, public :: SetAvgFlag       => EMIDSetAvgFlag
     procedure, public :: SetLongName      => EMIDSetLongName
     procedure, public :: SetEMStages      => EMIDSetEMStages
     procedure, public :: AllocateMemory   => EMIDAllocateMemory
     procedure, public :: Reset            => EMIDReset

  end type emi_data

  type emi_data_ptr
     type(emi_data), pointer :: data
  end type emi_data_ptr

  type, public :: emi_data_list

     integer                     :: num_data

     type(emi_data)    , pointer :: first
     type(emi_data)    , pointer :: last
     type(emi_data_ptr), pointer :: data_ptr(:)

   contains

     procedure, public :: Init    => EMIDListInit
     procedure, public :: AddData => EMIDListAddData
     procedure, public :: Copy    => EMIDListCopy

  end type emi_data_list

contains

  !------------------------------------------------------------------------
  subroutine EMIDInit(this)
    !
    ! !DESCRIPTION:
    ! Initializes a EMI data
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) :: this

    this%id             = 0
    this%name           = ""
    this%long_name      = ""
    this%units          = ""
    this%avgflag        = ""

    this%is_int_type    = .false.
    this%is_real_type   = .false.
    this%is_set         = .false.

    this%num_em_stages  = 0
    nullify(this%em_stage_ids)

    this%dim1_name      = ""
    this%dim2_name      = ""
    this%dim3_name      = ""
    this%dim4_name      = ""

    nullify(this%dim1_beg_clump)
    nullify(this%dim2_beg_clump)
    nullify(this%dim3_beg_clump)
    nullify(this%dim4_beg_clump)
    
    nullify(this%dim1_end_clump)
    nullify(this%dim2_end_clump)
    nullify(this%dim3_end_clump)
    nullify(this%dim4_end_clump)

    this%dim1_beg_proc  = 0
    this%dim2_beg_proc  = 0
    this%dim3_beg_proc  = 0
    this%dim4_beg_proc  = 0
    
    this%dim1_end_proc  = 0
    this%dim2_end_proc  = 0
    this%dim3_end_proc  = 0
    this%dim4_end_proc  = 0

    nullify(this%data_int_1d)
    nullify(this%data_int_2d)
    nullify(this%data_int_3d)

    nullify(this%data_real_1d)
    nullify(this%data_real_2d)
    nullify(this%data_real_3d)
    nullify(this%data_real_4d)

    nullify(this%next)
    
  end subroutine EMIDInit

  !------------------------------------------------------------------------
  subroutine EMIDCopy(this, default_data)
    !
    ! !DESCRIPTION:
    ! Initializes a EMI data
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data), intent(inout) :: this
    class(emi_data), intent(in) :: default_data
    !
    ! !LOCAL VARIABLES:
    integer :: nclumps
    integer :: ier                    ! error code

    call this%Init()

    this%id             = default_data%id
    this%name           = trim(default_data%name)
    this%long_name      = trim(default_data%long_name)
    this%units          = trim(default_data%units)
    this%avgflag        = trim(default_data%avgflag)

    this%is_int_type    = default_data%is_int_type
    this%is_real_type   = default_data%is_int_type

    this%num_em_stages  = default_data%num_em_stages

    if (associated(default_data%em_stage_ids)) then
       allocate(this%em_stage_ids(this%num_em_stages))
       this%em_stage_ids(:) = default_data%em_stage_ids(:)
    endif

    this%dim1_name      = trim(default_data%dim1_name)
    this%dim2_name      = trim(default_data%dim2_name)
    this%dim3_name      = trim(default_data%dim3_name)
    this%dim4_name      = trim(default_data%dim4_name)

    if (associated(default_data%dim1_beg_clump)) then
       nclumps = size(default_data%dim1_beg_clump)
       allocate(this%dim1_beg_clump(nclumps))
       this%dim1_beg_clump(:) = default_data%dim1_beg_clump
    endif

    if (associated(default_data%dim2_beg_clump)) then
       nclumps = size(default_data%dim2_beg_clump)
       allocate(this%dim2_beg_clump(nclumps))
       this%dim2_beg_clump(:) = default_data%dim2_beg_clump
    endif

    if (associated(default_data%dim3_beg_clump)) then
       nclumps = size(default_data%dim3_beg_clump)
       allocate(this%dim3_beg_clump(nclumps))
       this%dim3_beg_clump(:) = default_data%dim3_beg_clump
    endif

    if (associated(default_data%dim4_beg_clump)) then
       nclumps = size(default_data%dim4_beg_clump)
       allocate(this%dim4_beg_clump(nclumps))
       this%dim4_beg_clump(:) = default_data%dim4_beg_clump
    endif

    if (associated(default_data%dim1_end_clump)) then
       nclumps = size(default_data%dim1_end_clump)
       allocate(this%dim1_end_clump(nclumps))
       this%dim1_end_clump(:) = default_data%dim1_end_clump
    endif

    if (associated(default_data%dim2_end_clump)) then
       nclumps = size(default_data%dim2_end_clump)
       allocate(this%dim2_end_clump(nclumps))
       this%dim2_end_clump(:) = default_data%dim2_end_clump
    endif

    if (associated(default_data%dim3_end_clump)) then
       nclumps = size(default_data%dim3_end_clump)
       allocate(this%dim3_end_clump(nclumps))
       this%dim3_end_clump(:) = default_data%dim3_end_clump
    endif

    if (associated(default_data%dim4_end_clump)) then
       nclumps = size(default_data%dim4_end_clump)
       allocate(this%dim4_end_clump(nclumps))
       this%dim4_end_clump(:) = default_data%dim4_end_clump
    endif

    this%dim1_beg_proc  = this%dim1_beg_proc
    this%dim2_beg_proc  = this%dim2_beg_proc
    this%dim3_beg_proc  = this%dim3_beg_proc
    this%dim4_beg_proc  = this%dim4_beg_proc
    
    this%dim1_end_proc  = this%dim1_end_proc
    this%dim2_end_proc  = this%dim2_end_proc
    this%dim3_end_proc  = this%dim3_end_proc
    this%dim4_end_proc  = this%dim4_end_proc

    if (associated(default_data%data_int_1d)) then
       call EMIDAllocateMemory_Int_1D(this)
       this%data_int_1d(:) = default_data%data_int_1d(:)
    endif

    if (associated(default_data%data_int_2d)) then
       call EMIDAllocateMemory_Int_2D(this)
       this%data_int_2d(:,:) = default_data%data_int_2d(:,:)
    endif

    if (associated(default_data%data_int_3d)) then
       call EMIDAllocateMemory_Int_3D(this)
       this%data_int_3d(:,:,:) = default_data%data_int_3d(:,:,:)
    endif

    if (associated(default_data%data_real_1d)) then
       call EMIDAllocateMemory_Real_1D(this)
       this%data_real_1d(:) = default_data%data_real_1d(:)
    endif

    if (associated(default_data%data_real_2d)) then
       call EMIDAllocateMemory_Real_2D(this)
       this%data_real_2d(:,:) = default_data%data_real_2d(:,:)
    endif

    if (associated(default_data%data_real_3d)) then
       call EMIDAllocateMemory_Real_3D(this)
       this%data_real_3d(:,:,:) = default_data%data_real_3d(:,:,:)
    endif

    if (associated(default_data%data_real_4d)) then
       call EMIDAllocateMemory_Real_4D(this)
       this%data_real_4d(:,:,:,:) = default_data%data_real_4d(:,:,:,:)
    endif

    nullify(this%next)
    
  end subroutine EMIDCopy

  !------------------------------------------------------------------------
  subroutine EMIDSetup(this, id, name, long_name, units, avgflag, &
       num_em_stages, em_stage_ids)
    !
    ! !DESCRIPTION:
    ! Set value to data members of EMID object
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data)                      , intent(inout) :: this
    integer          , optional          , intent(in)    :: id
    character(len=*) , optional          , intent(in)    :: name
    character(len=*) , optional          , intent(in)    :: long_name
    character(len=*) , optional          , intent(in)    :: units
    character(len=*) , optional          , intent(in)    :: avgflag
    integer          , optional          , intent(in)    :: num_em_stages
    integer          , optional, pointer , intent(in)    :: em_stage_ids(:)

    if (present(id))        call this%SetID(id)
    if (present(name))      call this%SetName(name)
    if (present(long_name)) call this%SetLongName(name)
    if (present(units))     call this%SetUnits(units)
    if (present(avgflag))   call this%SetAvgFlag(units)

    if (present(num_em_stages) .or. present(em_stage_ids)) then
       if (present(num_em_stages) .and. present(em_stage_ids)) then
          call this%SetEMStages(num_em_stages, em_stage_ids)
       else
          call endrun(msg='EMIDSetup: Number of EM stages AND their IDs required.')
       endif
    endif

  end subroutine EMIDSetup

  !------------------------------------------------------------------------
  subroutine EMIDSetID(this, id)
    !
    ! !DESCRIPTION:
    ! Sets ID
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) , intent(inout) :: this
    integer         , intent(in)    :: id
    
    this%id = id

  end subroutine EMIDSetID

  !------------------------------------------------------------------------
  subroutine EMIDSetName(this, name)
    !
    ! !DESCRIPTION:
    ! Sets short name
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) , intent(inout) :: this
    character(len=*), intent(in)    :: name

    this%name = trim(name)

  end subroutine EMIDSetName

  !------------------------------------------------------------------------
  subroutine EMIDSetLongName(this, long_name)
    !
    ! !DESCRIPTION:
    ! Sets long name
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) , intent(inout) :: this
    character(len=*), intent(in)    :: long_name

    this%long_name = trim(long_name)

  end subroutine EMIDSetLongName

  !------------------------------------------------------------------------
  subroutine EMIDSetUnits(this, units)
    !
    ! !DESCRIPTION:
    ! Sets units
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) , intent(inout) :: this
    character(len=*), intent(in)    :: units

    this%units = trim(units)

  end subroutine EMIDSetUnits

  !------------------------------------------------------------------------
  subroutine EMIDSetAvgFlag(this, avgflag)
    !
    ! !DESCRIPTION:
    ! Sets averaging flag option
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) , intent(inout) :: this
    character(len=*), intent(in)    :: avgflag

    this%avgflag = trim(avgflag)

  end subroutine EMIDSetAvgFlag

  !------------------------------------------------------------------------
  subroutine EMIDSetEMStages(this, num_em_stages, em_stages)
    !
    ! !DESCRIPTION:
    ! Set number of external model IDs and stages in which the data would be
    ! exchanged.
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data), intent(inout) :: this
    integer, intent (in)           :: num_em_stages
    integer, intent (in), pointer  :: em_stages(:)
    
    if (size(em_stages) /= num_em_stages) then
       call endrun(msg='Number of EM stage IDs /= Number of EM Stages.')
    endif

    allocate(this%em_stage_ids(num_em_stages))

    this%num_em_stages   = num_em_stages
    this%em_stage_ids(:) = em_stages(:)

  end subroutine EMIDSetEMStages

  !------------------------------------------------------------------------
  subroutine EMIDSetDimensions(this, ndim, nclumps,                 &
    dim1_beg_clump, dim1_end_clump, dim2_beg_clump, dim2_end_clump, &
    dim3_beg_clump, dim3_end_clump, dim4_beg_clump, dim4_end_clump, &
    dim1_beg_proc, dim1_end_proc, dim2_beg_proc, dim2_end_proc,     &
    dim3_beg_proc, dim3_end_proc, dim4_beg_proc, dim4_end_proc)
    !
    ! !DESCRIPTION:
    ! Sets dimenions for the data and allocates memory
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data)  , intent(inout) :: this
    integer          , intent (in)   :: ndim
    integer          , intent (in)   :: nclumps
    integer, pointer , intent (in)   :: dim1_beg_clump(:), dim1_end_clump(:)
    integer, pointer , intent (in)   :: dim2_beg_clump(:), dim2_end_clump(:)
    integer, pointer , intent (in)   :: dim3_beg_clump(:), dim3_end_clump(:)
    integer, pointer , intent (in)   :: dim4_beg_clump(:), dim4_end_clump(:)
    integer          , intent (in)   :: dim1_beg_proc, dim1_end_proc
    integer          , intent (in)   :: dim2_beg_proc, dim2_end_proc
    integer          , intent (in)   :: dim3_beg_proc, dim3_end_proc
    integer          , intent (in)   :: dim4_beg_proc, dim4_end_proc

    allocate(this%dim1_beg_clump(nclumps))
    allocate(this%dim2_beg_clump(nclumps))
    allocate(this%dim3_beg_clump(nclumps))
    allocate(this%dim4_beg_clump(nclumps))
    
    allocate(this%dim1_end_clump(nclumps))
    allocate(this%dim2_end_clump(nclumps))
    allocate(this%dim3_end_clump(nclumps))
    allocate(this%dim4_end_clump(nclumps))

    this%ndim = ndim

    this%dim1_beg_clump(:) = dim1_beg_clump(:)
    this%dim2_beg_clump(:) = dim2_beg_clump(:)
    this%dim3_beg_clump(:) = dim3_beg_clump(:)
    this%dim4_beg_clump(:) = dim4_beg_clump(:)

    this%dim1_end_clump(:) = dim1_end_clump(:)
    this%dim2_end_clump(:) = dim2_end_clump(:)
    this%dim3_end_clump(:) = dim3_end_clump(:)
    this%dim4_end_clump(:) = dim4_end_clump(:)

    this%dim1_beg_proc = dim1_beg_proc
    this%dim2_beg_proc = dim2_beg_proc
    this%dim3_beg_proc = dim3_beg_proc
    this%dim4_beg_proc = dim4_beg_proc

    this%dim1_end_proc = dim1_end_proc
    this%dim2_end_proc = dim2_end_proc
    this%dim3_end_proc = dim3_end_proc
    this%dim4_end_proc = dim4_end_proc

  end subroutine EMIDSetDimensions

  !------------------------------------------------------------------------
  subroutine EMIDSetType(this, is_int, is_real)
    !
    ! !DESCRIPTION:
    ! Sets data type
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) , intent(inout) :: this
    logical         , intent(in)    :: is_int
    logical         , intent(in)    :: is_real

    if (is_int .and. is_real) then
       call endrun(msg='EMIDType must be either integer or real.')
    endif

    if ((.not.is_int) .and. (.not.is_real)) then
       call endrun(msg='EMIDType must be either integer or real.')
    endif

    this%is_int_type  = is_int
    this%is_real_type = is_real

  end subroutine EMIDSetType

  !------------------------------------------------------------------------
  subroutine EMIDAllocateMemory(this)
    !
    ! !DESCRIPTION:
    ! Initializes a EMI data
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) :: this

    if (this%is_int_type .and. this%is_real_type) then
       call endrun(msg='Data type is defined to be both int and real.')
    endif

    if ((.not.this%is_int_type) .and. (.not.this%is_real_type)) then
       call endrun(msg='Data type is not defined to be either int or real.')
    endif

    if (this%ndim == 0) return

    select case(this%ndim)
    case (1)
       if (this%is_real_type) then
          call EMIDAllocateMemory_Real_1D(this)
       else
          call EMIDAllocateMemory_Int_1D(this)
       endif

    case (2)
       if (this%is_real_type) then
          call EMIDAllocateMemory_Real_2D(this)
       else
          call EMIDAllocateMemory_Int_2D(this)
       endif

    case (3)
       if (this%is_real_type) then
          call EMIDAllocateMemory_Real_3D(this)
       else
          call EMIDAllocateMemory_Int_3D(this)
       endif

    case (4)
       if (this%is_real_type) then
          call EMIDAllocateMemory_Real_4D(this)
       else
          call endrun(msg='EMID of type integer for dimension=4 is not supported.')

       endif

    case default
       call endrun(msg='EMID dimension larger than 4 is not supported.')

    end select

  end subroutine EMIDAllocateMemory

  !------------------------------------------------------------------------
  subroutine EMIDAllocateMemory_Int_1D(this)
    !
    ! !DESCRIPTION:
    ! Allocate 1D integer data
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) :: this
    !
    ! !LOCAL VARIABLES:
    integer         :: ier ! error code

    allocate(this%data_int_1d(this%dim1_beg_proc:this%dim1_end_proc), &
             stat=ier)

    if (ier /= 0) then
       write(iulog,*) 'ERROR: Allocation failure'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    endif

  end subroutine EMIDAllocateMemory_Int_1D

  !------------------------------------------------------------------------
  subroutine EMIDAllocateMemory_Int_2D(this)
    !
    ! !DESCRIPTION:
    ! Allocate 2D integer data type
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) :: this
    !
    ! !LOCAL VARIABLES:
    integer         :: ier ! error code

    allocate(this%data_int_2d(this%dim1_beg_proc:this%dim1_end_proc, &
                              this%dim2_beg_proc:this%dim2_end_proc  ), &
             stat=ier)

    if (ier /= 0) then
       write(iulog,*) 'ERROR: Allocation failure'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    endif

  end subroutine EMIDAllocateMemory_Int_2D

  !------------------------------------------------------------------------
  subroutine EMIDAllocateMemory_Int_3D(this)
    !
    ! !DESCRIPTION:
    ! Allocate 3D integer data type
    !
    implicit none
    !
    class(emi_data) :: this
    !
    ! !LOCAL VARIABLES:
    integer         :: ier ! error code

    allocate(this%data_int_3d( this%dim1_beg_proc:this%dim1_end_proc, &
                               this%dim2_beg_proc:this%dim2_end_proc, &
                               this%dim3_beg_proc:this%dim3_end_proc  ), &
             stat=ier)

    if (ier /= 0) then
       write(iulog,*) 'ERROR: Allocation failure'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    endif

  end subroutine EMIDAllocateMemory_Int_3D

  !------------------------------------------------------------------------
  subroutine EMIDAllocateMemory_Real_1D(this)
    !
    ! !DESCRIPTION:
    ! Allocate 1D real data
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) :: this
    !
    ! !LOCAL VARIABLES:
    integer         :: ier ! error code

    allocate(this%data_real_1d(this%dim1_beg_proc:this%dim1_end_proc), &
             stat=ier)

    if (ier /= 0) then
       write(iulog,*) 'ERROR: Allocation failure'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    endif

  end subroutine EMIDAllocateMemory_Real_1D

  !------------------------------------------------------------------------
  subroutine EMIDAllocateMemory_Real_2D(this)
    !
    ! !DESCRIPTION:
    ! Allocate 2D real data type
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) :: this
    !
    ! !LOCAL VARIABLES:
    integer         :: ier ! error code

    allocate(this%data_real_2d(this%dim1_beg_proc:this%dim1_end_proc, &
                               this%dim2_beg_proc:this%dim2_end_proc  ), &
             stat=ier)

    if (ier /= 0) then
       write(iulog,*) 'ERROR: Allocation failure'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    endif

  end subroutine EMIDAllocateMemory_Real_2D

  !------------------------------------------------------------------------
  subroutine EMIDAllocateMemory_Real_3D(this)
    !
    ! !DESCRIPTION:
    ! Allocate 3D real data type
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) :: this
    !
    ! !LOCAL VARIABLES:
    integer         :: ier ! error code

    allocate(this%data_real_3d(this%dim1_beg_proc:this%dim1_end_proc, &
                               this%dim2_beg_proc:this%dim2_end_proc, &
                               this%dim3_beg_proc:this%dim3_end_proc  ), &
             stat=ier)

    if (ier /= 0) then
       write(iulog,*) 'ERROR: Allocation failure'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    endif

  end subroutine EMIDAllocateMemory_Real_3D

  !------------------------------------------------------------------------
  subroutine EMIDAllocateMemory_Real_4D(this)
    !
    ! !DESCRIPTION:
    ! Allocates 4D real data type
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) :: this
    !
    ! !LOCAL VARIABLES:
    integer         :: ier ! error code

    allocate(this%data_real_4d(this%dim1_beg_proc:this%dim1_end_proc, &
                               this%dim2_beg_proc:this%dim2_end_proc, &
                               this%dim3_beg_proc:this%dim3_end_proc, &
                               this%dim4_beg_proc:this%dim4_end_proc  ), &
             stat=ier)

    if (ier /= 0) then
       write(iulog,*) 'ERROR: Allocation failure'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    endif

  end subroutine EMIDAllocateMemory_Real_4D

  !------------------------------------------------------------------------
  subroutine EMIDReset(this)
    !
    ! !DESCRIPTION:
    ! Resets values of a EMI data
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data) :: this

    if (this%is_int_type .and. this%is_real_type) then
       call endrun(msg='Data type is defined to be both int and real.')
    endif

    if ((.not.this%is_int_type) .and. (.not.this%is_real_type)) then
       call endrun(msg='Data type is not defined to be either int or real.')
    endif

    if (this%ndim == 0) return

    select case(this%ndim)
    case (1)
       if (this%is_real_type) then
          this%data_real_1d(:) = 0._r8
       else
          this%data_int_1d(:) = 0
       endif

    case (2)
       if (this%is_real_type) then
          this%data_real_2d(:,:) = 0._r8
       else
          this%data_int_2d(:,:) = 0
       endif

    case (3)
       if (this%is_real_type) then
          this%data_real_3d(:,:,:) = 0._r8
       else
          this%data_int_3d(:,:,:) = 0
       endif

    case (4)
       if (this%is_real_type) then
          this%data_real_4d(:,:,:,:) = 0._r8
       else
          call endrun(msg='EMID of type integer for dimension=4 is not supported.')

       endif

    case default
       call endrun(msg='EMID dimension larger than 4 is not supported.')

    end select

    this%is_set = .false.

  end subroutine EMIDReset

  !------------------------------------------------------------------------
  subroutine EMIDListInit(this)
    !
    ! !DESCRIPTION:
    ! Initializes a EMID list
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list) :: this

    this%num_data = 0

    nullify(this%first)
    nullify(this%last)
    nullify(this%data_ptr)

  end subroutine EMIDListInit

  !------------------------------------------------------------------------
  subroutine EMIDListAddData(this, new_data)
    !
    ! !DESCRIPTION:
    ! Add a EMID to a list
    !
    ! !ARGUMENTS:
    implicit none
    !
    class(emi_data_list)     :: this
    class(emi_data), pointer :: new_data

    this%num_data = this%num_data + 1

    if (.not.associated(this%first)) then
       this%first => new_data
    endif
    
    if (associated(this%last)) then
       this%last%next => new_data
    endif

    this%last => new_data

  end subroutine EMIDListAddData
  
  !------------------------------------------------------------------------
  subroutine EMIDListCopy(this, data_list)
    !
    ! !DESCRIPTION:
    ! Makes a copy of EMI data list
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)              :: this
    class(emi_data_list) , intent(in) :: data_list
    !
    ! !LOCAL VARIABLES:
    class(emi_data)      , pointer    :: cur_data
    class(emi_data)      , pointer    :: new_data
    integer                           :: idata

    call this%Init()

    allocate(this%data_ptr(data_list%num_data))

    idata = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit

       idata = idata + 1

       allocate(new_data)

       call new_data%Copy(cur_data)

       call this%AddData(new_data)

       this%data_ptr(idata)%data => new_data

       nullify(new_data)

       cur_data => cur_data%next
    enddo


  end subroutine EMIDListCopy


end module ExternalModelInterfaceDataMod
