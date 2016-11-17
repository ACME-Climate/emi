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

     integer :: dim1_beg, dim1_end
     integer :: dim2_beg, dim2_end
     integer :: dim3_beg, dim3_end
     integer :: dim4_beg, dim4_end

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
     procedure, public :: Destroy          => EMIDDestroy

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

     procedure, public :: Init               => EMIDListInit
     procedure, public :: AddData            => EMIDListAddData
     procedure, public :: AddDataByID        => EMIDListAddDataByID
     procedure, public :: Copy               => EMIDListCopy
     procedure, public :: Destroy            => EMIDListDestroy
     procedure, public :: GetIntValue        => EMIDListGetIntValue
     procedure, public :: GetPointerToInt1D  => EMIDListGetPointerToInt1D
     procedure, public :: GetPointerToInt2D  => EMIDListGetPointerToInt2D
     procedure, public :: GetPointerToInt3D  => EMIDListGetPointerToInt3D
     procedure, public :: GetPointerToReal1D => EMIDListGetPointerToReal1D
     procedure, public :: GetPointerToReal2D => EMIDListGetPointerToReal2D
     procedure, public :: GetPointerToReal3D => EMIDListGetPointerToReal3D
     procedure, public :: GetPointerToReal4D => EMIDListGetPointerToReal4D

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

    this%dim1_beg  = 0
    this%dim2_beg  = 0
    this%dim3_beg  = 0
    this%dim4_beg  = 0
    
    this%dim1_end  = 0
    this%dim2_end  = 0
    this%dim3_end  = 0
    this%dim4_end  = 0

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

    this%dim1_beg  = this%dim1_beg
    this%dim2_beg  = this%dim2_beg
    this%dim3_beg  = this%dim3_beg
    this%dim4_beg  = this%dim4_beg
    
    this%dim1_end  = this%dim1_end
    this%dim2_end  = this%dim2_end
    this%dim3_end  = this%dim3_end
    this%dim4_end  = this%dim4_end

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
  subroutine EMIDSetDimensions(this, ndim, &
    dim1_beg, dim1_end, dim2_beg, dim2_end,     &
    dim3_beg, dim3_end, dim4_beg, dim4_end)
    !
    ! !DESCRIPTION:
    ! Sets dimenions for the data and allocates memory
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data)  , intent(inout) :: this
    integer          , intent (in)   :: ndim
    integer          , intent (in)   :: dim1_beg, dim1_end
    integer          , intent (in)   :: dim2_beg, dim2_end
    integer          , intent (in)   :: dim3_beg, dim3_end
    integer          , intent (in)   :: dim4_beg, dim4_end

    this%ndim = ndim

    this%dim1_beg = dim1_beg
    this%dim2_beg = dim2_beg
    this%dim3_beg = dim3_beg
    this%dim4_beg = dim4_beg

    this%dim1_end = dim1_end
    this%dim2_end = dim2_end
    this%dim3_end = dim3_end
    this%dim4_end = dim4_end

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

    allocate(this%data_int_1d(this%dim1_beg:this%dim1_end), &
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

    allocate(this%data_int_2d(this%dim1_beg:this%dim1_end, &
                              this%dim2_beg:this%dim2_end  ), &
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

    allocate(this%data_int_3d( this%dim1_beg:this%dim1_end, &
                               this%dim2_beg:this%dim2_end, &
                               this%dim3_beg:this%dim3_end  ), &
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

    allocate(this%data_real_1d(this%dim1_beg:this%dim1_end), &
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

    allocate(this%data_real_2d(this%dim1_beg:this%dim1_end, &
                               this%dim2_beg:this%dim2_end  ), &
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

    allocate(this%data_real_3d(this%dim1_beg:this%dim1_end, &
                               this%dim2_beg:this%dim2_end, &
                               this%dim3_beg:this%dim3_end  ), &
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

    allocate(this%data_real_4d(this%dim1_beg:this%dim1_end, &
                               this%dim2_beg:this%dim2_end, &
                               this%dim3_beg:this%dim3_end, &
                               this%dim4_beg:this%dim4_end  ), &
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
  subroutine EMIDDestroy(this)
    !
    ! !DESCRIPTION:
    ! Destroys a EMI data
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
          deallocate(this%data_real_1d)
       else
          deallocate(this%data_int_1d)
       endif

    case (2)
       if (this%is_real_type) then
          deallocate(this%data_real_2d)
       else
          deallocate(this%data_int_2d)
       endif

    case (3)
       if (this%is_real_type) then
          deallocate(this%data_real_3d)
       else
          deallocate(this%data_int_3d)
       endif

    case (4)
       if (this%is_real_type) then
          deallocate(this%data_real_4d)
       else
          call endrun(msg='EMID of type integer for dimension=4 is not supported.')
       endif

    case default
       call endrun(msg='EMID dimension larger than 4 is not supported.')

    end select

  end subroutine EMIDDestroy

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
  subroutine EMIDListAddDataByID(this, data_id, num_em_stages_val, em_stage_ids_val, index_of_new_data)
    !
    ! !DESCRIPTION:
    ! Add a EMID to a list
    !
    ! !USES:
    use ExternalModelConstants    , only : L2E_STATE_TSOIL
    use ExternalModelConstants    , only : L2E_STATE_H2OSOI_LIQ
    use ExternalModelConstants    , only : L2E_STATE_H2OSOI_ICE
    use ExternalModelConstants    , only : L2E_STATE_WTD
    use ExternalModelConstants    , only : L2E_STATE_VSFM_PROGNOSTIC_SOILP

    use ExternalModelConstants    , only : E2L_STATE_H2OSOI_LIQ
    use ExternalModelConstants    , only : E2L_STATE_H2OSOI_ICE
    use ExternalModelConstants    , only : E2L_STATE_SOIL_MATRIC_POTENTIAL
    use ExternalModelConstants    , only : E2L_STATE_WTD
    use ExternalModelConstants    , only : E2L_STATE_VSFM_PROGNOSTIC_SOILP
    use ExternalModelConstants    , only : E2L_STATE_FSUN
    use ExternalModelConstants    , only : E2L_STATE_LAISUN
    use ExternalModelConstants    , only : E2L_STATE_LAISHA

    use ExternalModelConstants    , only : L2E_FLUX_INFIL_MASS_FLUX
    use ExternalModelConstants    , only : L2E_FLUX_VERTICAL_ET_MASS_FLUX
    use ExternalModelConstants    , only : L2E_FLUX_DEW_MASS_FLUX
    use ExternalModelConstants    , only : L2E_FLUX_SNOW_SUBLIMATION_MASS_FLUX
    use ExternalModelConstants    , only : L2E_FLUX_SNOW_LYR_DISAPPERANCE_MASS_FLUX
    use ExternalModelConstants    , only : L2E_FLUX_RESTART_SNOW_LYR_DISAPPERANCE_MASS_FLUX
    use ExternalModelConstants    , only : L2E_FLUX_DRAINAGE_MASS_FLUX
    use ExternalModelConstants    , only : L2E_FLUX_SOLAR_DIRECT_RADDIATION
    use ExternalModelConstants    , only : L2E_FLUX_SOLAR_DIFFUSE_RADDIATION

    use ExternalModelConstants    , only : E2L_FLUX_AQUIFER_RECHARGE
    use ExternalModelConstants    , only : E2L_FLUX_SNOW_LYR_DISAPPERANCE_MASS_FLUX

    use ExternalModelConstants    , only : L2E_FILTER_HYDROLOGYC
    use ExternalModelConstants    , only : L2E_FILTER_NUM_HYDROLOGYC

    use ExternalModelConstants    , only : L2E_COLUMN_ACTIVE
    use ExternalModelConstants    , only : L2E_COLUMN_TYPE
    use ExternalModelConstants    , only : L2E_COLUMN_LANDUNIT_INDEX
    use ExternalModelConstants    , only : L2E_COLUMN_ZI
    use ExternalModelConstants    , only : L2E_COLUMN_DZ
    use ExternalModelConstants    , only : L2E_COLUMN_Z
    use ExternalModelConstants    , only : L2E_COLUMN_AREA
    use ExternalModelConstants    , only : L2E_COLUMN_GRIDCELL_INDEX
    use ExternalModelConstants    , only : L2E_COLUMN_PATCH_INDEX

    use ExternalModelConstants    , only : L2E_LANDUNIT_TYPE
    use ExternalModelConstants    , only : L2E_LANDUNIT_LAKEPOINT
    use ExternalModelConstants    , only : L2E_LANDUNIT_URBANPOINT

    use ExternalModelConstants    , only : L2E_PARAMETER_WATSATC
    use ExternalModelConstants    , only : L2E_PARAMETER_HKSATC
    use ExternalModelConstants    , only : L2E_PARAMETER_BSWC
    use ExternalModelConstants    , only : L2E_PARAMETER_SUCSATC
    use ExternalModelConstants    , only : L2E_PARAMETER_EFFPOROSITYC
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)                   :: this
    integer         , intent(in)           :: data_id
    integer         , intent(in)           :: num_em_stages_val
    integer         , pointer , intent(in) :: em_stage_ids_val(:)
    integer         , intent(out)          :: index_of_new_data
    !
    class(emi_data) , pointer              :: data
    integer                                :: id_val
    character (len=32)                     :: name_val
    character (len=128)                    :: long_name_val
    character (len=24)                     :: units_val

    select case(data_id)

       ! --------------------------------------------------------------
       ! ALM-to-EM: State variables
       ! --------------------------------------------------------------
    case (L2E_STATE_TSOIL)
       id_val        = L2E_STATE_TSOIL
       name_val      = 'Soil temperature'
       long_name_val = 'Soil temperature: ALM to External Model'
       units_val     = '[K]'

    case (L2E_STATE_H2OSOI_LIQ)
       id_val        = L2E_STATE_H2OSOI_LIQ
       name_val      = 'Soil liquid water'
       long_name_val = 'Soil liquid water: ALM to External Model'
       units_val     = '[kg/m2]'

    case (L2E_STATE_H2OSOI_ICE)
       id_val        = L2E_STATE_H2OSOI_ICE
       name_val      = 'Soil ice water'
       long_name_val = 'Soil ice water: ALM to External Model'
       units_val     = '[kg/m2]'

    case (L2E_STATE_WTD)
       id_val        = L2E_STATE_WTD
       name_val      = 'Water table depth'
       long_name_val = 'Water table depth: ALM to External Model'
       units_val     = '[m]'

    case (L2E_STATE_VSFM_PROGNOSTIC_SOILP)
       id_val        = L2E_STATE_VSFM_PROGNOSTIC_SOILP
       name_val      = 'Soil liquid pressure'
       long_name_val = 'Soil liquid pressure: ALM to External Model'
       units_val     = '[Pa]'

       ! --------------------------------------------------------------
       ! EM-to-ALM: State variables
       ! --------------------------------------------------------------
    case (E2L_STATE_H2OSOI_LIQ)
       id_val        = E2L_STATE_H2OSOI_LIQ
       name_val      = 'Soil liquid water'
       long_name_val = 'Soil liquid water: External Model to ALM'
       units_val     = '[kg/m2]'

    case (E2L_STATE_H2OSOI_ICE)
       id_val        = E2L_STATE_H2OSOI_ICE
       name_val      = 'Soil ice water'
       long_name_val = 'Soil ice water: External Model to ALM'
       units_val     = '[kg/m2]'

    case (E2L_STATE_SOIL_MATRIC_POTENTIAL)
       id_val        = E2L_STATE_SOIL_MATRIC_POTENTIAL
       name_val      = 'Soil matric potential'
       long_name_val = 'Soil matric potential: External Model to ALM'
       units_val     = '[mm]'

    case (E2L_STATE_WTD)
       id_val        = E2L_STATE_WTD
       name_val      = 'Water table depth'
       long_name_val = 'Water table depth: External Model to ALM'
       units_val     = '[m]'

    case (E2L_STATE_VSFM_PROGNOSTIC_SOILP)
       id_val        = E2L_STATE_VSFM_PROGNOSTIC_SOILP
       name_val      = 'Soil liquid pressure'
       long_name_val = 'Soil liquid pressure: External Model to ALM'
       units_val     = '[Pa]'

    case (E2L_STATE_FSUN)
       id_val        = E2L_STATE_FSUN
       name_val      = 'Fraction of canopy sunlit'
       long_name_val = ': External Model to ALM'
       units_val     = '[-]'

    case (E2L_STATE_LAISUN)
       id_val        = E2L_STATE_LAISUN
       name_val      = 'Sunlit leaf area'
       long_name_val = 'Sunlit leaf area: External Model to ALM'
       units_val     = '[-]'

    case (E2L_STATE_LAISHA)
       id_val        = E2L_STATE_LAISHA
       name_val      = 'Shaded leaf area'
       long_name_val = 'Shaded leaf area: External Model to ALM'
       units_val     = '[-]'

       ! --------------------------------------------------------------
       ! ALM-to-EM: Flux variables
       ! --------------------------------------------------------------
    case (L2E_FLUX_INFIL_MASS_FLUX)
       id_val        = L2E_FLUX_INFIL_MASS_FLUX
       name_val      = 'Soil infiltration source'
       long_name_val = 'Soil infiltration source: ALM to External Model'
       units_val     = '[kg/s]'

    case (L2E_FLUX_VERTICAL_ET_MASS_FLUX)
       id_val        = L2E_FLUX_VERTICAL_ET_MASS_FLUX
       name_val      = 'Evapotranspiration sink'
       long_name_val = 'Evapotranspiration sink: ALM to External Model'
       units_val     = '[kg/s]'

    case (L2E_FLUX_DEW_MASS_FLUX)
       id_val        = L2E_FLUX_DEW_MASS_FLUX
       name_val      = 'Dew sink'
       long_name_val = 'Dew sink: ALM to External Model'
       units_val     = '[kg/s]'

    case (L2E_FLUX_SNOW_SUBLIMATION_MASS_FLUX)
       id_val        = L2E_FLUX_SNOW_SUBLIMATION_MASS_FLUX
       name_val      = 'Snow sublimation sink'
       long_name_val = 'Snow sublimation sink: ALM to External Model'
       units_val     = '[kg/s]'

    case (L2E_FLUX_SNOW_LYR_DISAPPERANCE_MASS_FLUX)
       id_val        = L2E_FLUX_SNOW_LYR_DISAPPERANCE_MASS_FLUX
       name_val      = 'Snow layer disappearance sink'
       long_name_val = 'Snow layer disappearance sink: ALM to External Model'
       units_val     = '[kg/s]'

    case (L2E_FLUX_RESTART_SNOW_LYR_DISAPPERANCE_MASS_FLUX)
       id_val        = L2E_FLUX_RESTART_SNOW_LYR_DISAPPERANCE_MASS_FLUX
       name_val      = 'Snow layer disappearance sink'
       long_name_val = 'Snow layer disappearance sink from restart: ALM to External Model'
       units_val     = '[kg/s]'

    case (L2E_FLUX_DRAINAGE_MASS_FLUX)
       id_val        = L2E_FLUX_DRAINAGE_MASS_FLUX
       name_val      = 'Drainage sink'
       long_name_val = 'Drainage sink: ALM to External Model'
       units_val     = '[kg/s]'

    case (L2E_FLUX_SOLAR_DIRECT_RADDIATION)
       id_val        = L2E_FLUX_SOLAR_DIRECT_RADDIATION
       name_val      = 'Direct beam solar radiation'
       long_name_val = 'Direct beam solar radiation: ALM to External Model'
       units_val     = '[W/m2]'

    case (L2E_FLUX_SOLAR_DIFFUSE_RADDIATION)
       id_val        = L2E_FLUX_SOLAR_DIFFUSE_RADDIATION
       name_val      = 'Diffuse beam solar radiation'
       long_name_val = 'Diffuse beam solar radiation: ALM to External Model'
       units_val     = '[W/m2]'

       ! --------------------------------------------------------------
       ! EM-to-ALM: Flux variables
       ! --------------------------------------------------------------
    case (E2L_FLUX_AQUIFER_RECHARGE)
       id_val        = E2L_FLUX_AQUIFER_RECHARGE
       name_val      = 'Aquifer recharge rate'
       long_name_val = 'Aquifer recharge rate: External Model to ALM'
       units_val     = '[mm/s]'

    case (E2L_FLUX_SNOW_LYR_DISAPPERANCE_MASS_FLUX)
       id_val        = E2L_FLUX_SNOW_LYR_DISAPPERANCE_MASS_FLUX
       name_val      = 'Snow layer disappearance sink'
       long_name_val = 'Snow layer disappearance sink initial value: External Model to ALM'
       units_val     = '[kg/s]'

       ! --------------------------------------------------------------
       ! ALM-to-ELM: Filter variables
       ! --------------------------------------------------------------
    case (L2E_FILTER_HYDROLOGYC)
       id_val        = L2E_FILTER_HYDROLOGYC
       name_val      = 'Hydrology filter'
       long_name_val = 'Hydrology filter: ALM to External Model'
       units_val     = '[-]'

    case (L2E_FILTER_NUM_HYDROLOGYC)
       id_val        = L2E_FILTER_NUM_HYDROLOGYC
       name_val      = 'Number of hydrology filter'
       long_name_val = 'Number of hydrology filter: ALM to External Model'
       units_val     = '[-]'

       ! --------------------------------------------------------------
       ! ALM-to-ELM: Column variables
       ! --------------------------------------------------------------
    case (L2E_COLUMN_ACTIVE)
       id_val        = L2E_COLUMN_ACTIVE
       name_val      = 'Column active'
       long_name_val = 'Column active: ALM to External Model'
       units_val     = '[-]'

    case (L2E_COLUMN_TYPE)
       id_val        = L2E_COLUMN_TYPE
       name_val      = 'Column type'
       long_name_val = 'Column type: ALM to External Model'
       units_val     = '[-]'

    case (L2E_COLUMN_LANDUNIT_INDEX)
       id_val        = L2E_COLUMN_LANDUNIT_INDEX
       name_val      = 'Column to landunit index'
       long_name_val = 'Column landunit index: ALM to External Model'
       units_val     = '[-]'

    case (L2E_COLUMN_ZI)
       id_val        = L2E_COLUMN_ZI
       name_val      = 'Column layer interface depth'
       long_name_val = 'Column layer interface depth: ALM to External Model'
       units_val     = '[m]'


    case (L2E_COLUMN_DZ)
       id_val        = L2E_COLUMN_DZ
       name_val      = 'Column layer thickness'
       long_name_val = 'Column layer thickness: ALM to External Model'
       units_val     = '[m]'

    case (L2E_COLUMN_Z)
       id_val        = L2E_COLUMN_Z
       name_val      = 'Column layer centroid depth'
       long_name_val = 'Column layer centroid depth: ALM to External Model'
       units_val     = '[m]'

    case (L2E_COLUMN_AREA)
       id_val        = L2E_COLUMN_AREA
       name_val      = 'Column surface area'
       long_name_val = 'Column surface area: ALM to External Model'
       units_val     = '[m2]'

    case (L2E_COLUMN_GRIDCELL_INDEX)
       id_val        = L2E_COLUMN_GRIDCELL_INDEX
       name_val      = 'Column to gridcell index'
       long_name_val = 'Column to gridcell index: ALM to External Model'
       units_val     = '[-]'

    case (L2E_COLUMN_PATCH_INDEX)
       id_val        = L2E_COLUMN_PATCH_INDEX
       name_val      = 'Column to patch index'
       long_name_val = 'Column to patch index: ALM to External Model'
       units_val     = '[-]'

       ! --------------------------------------------------------------
       ! ALM-to-ELM: Landunit variables
       ! --------------------------------------------------------------
    case (L2E_LANDUNIT_TYPE)
       id_val        = L2E_LANDUNIT_TYPE
       name_val      = 'Landunit type'
       long_name_val = 'Landunit type: ALM to External Model'
       units_val     = '[-]'

    case (L2E_LANDUNIT_LAKEPOINT)
       id_val        = L2E_LANDUNIT_LAKEPOINT
       name_val      = 'Landunit lake point'
       long_name_val = 'Landunit lake point: ALM to External Model'
       units_val     = '[-]'

    case (L2E_LANDUNIT_URBANPOINT)
       id_val        = L2E_LANDUNIT_URBANPOINT
       name_val      = 'Landunit urban point'
       long_name_val = 'Landunit urban point: ALM to External Model'
       units_val     = '[-]'

       ! --------------------------------------------------------------
       ! ALM-to-ELM: Parameters variables
       ! --------------------------------------------------------------
    case (L2E_PARAMETER_WATSATC)
       id_val        = L2E_PARAMETER_WATSATC
       name_val      = 'Soil porosity'
       long_name_val = 'Soil porosity: ALM to External Model'
       units_val     = '[m^3/m^3]'

    case (L2E_PARAMETER_HKSATC)
       id_val        = L2E_PARAMETER_HKSATC
       name_val      = 'Soil hydraulic conductivity'
       long_name_val = 'Soil hydraulic conductivity at saturation: ALM to External Model'
       units_val     = '[mm/s]'

    case (L2E_PARAMETER_BSWC)
       id_val        = L2E_PARAMETER_BSWC
       name_val      = 'Clapp and Hornberger parameter'
       long_name_val = 'Clapp and Hornberger parameter: ALM to External Model'
       units_val     = '[-]'

    case (L2E_PARAMETER_SUCSATC)
       id_val        = L2E_PARAMETER_SUCSATC
       name_val      = 'Minimum soil suction'
       long_name_val = 'Minimum soil suction: ALM to External Model'
       units_val     = '[mm]'

    case (L2E_PARAMETER_EFFPOROSITYC)
       id_val        = L2E_PARAMETER_EFFPOROSITYC
       name_val      = 'Effective porosity'
       long_name_val = 'Effective porosity: ALM to External Model'
       units_val     = ''

    case default
       write(iulog,*)'Unknown EMID id = ',data_id
       call endrun(msg='EMIDSetup: Number of EM stages AND their IDs required.')
    end select

    allocate(data)
    call data%Init()
    call data%Setup(                        &
         id            = id_val,            &
         name          = name_val,          &
         long_name     = long_name_val,     &
         units         = units_val,         &
         num_em_stages = num_em_stages_val, &
         em_stage_ids  = em_stage_ids_val)
    call this%AddData(data)
    index_of_new_data = this%num_data

    nullify(data)

  end subroutine EMIDListAddDataByID

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

  !------------------------------------------------------------------------
  subroutine EMIDListGetIntValue(this, data_index, int_value)
    !
    ! !DESCRIPTION:
    ! Gives access to data pointer
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)              :: this
    integer , intent(in) :: data_index
    integer, intent(out) :: int_value
    !
    ! !LOCAL VARIABLES:
    integer :: idx

    if (data_index > this%num_data) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to access ' // &
          'data index that is larger than number of data in the list.')
    endif

    if (.not. associated(this%data_ptr(data_index)%data%data_int_1d)) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to asscess ' // &
          'an unallocated data_int_1d.')
    else
       if (this%data_ptr(data_index)%data%dim1_end /= &
           this%data_ptr(data_index)%data%dim1_beg) then
          call endrun(msg='EMIDListGetIntValue: Only extracts values from data ' // &
             'that has 1 value.')
       endif
       idx = this%data_ptr(data_index)%data%dim1_beg
       int_value = this%data_ptr(data_index)%data%data_int_1d(idx)
    endif

  end subroutine EMIDListGetIntValue

  !------------------------------------------------------------------------
  subroutine EMIDListGetPointerToInt1D(this, data_index, int_1d)
    !
    ! !DESCRIPTION:
    ! Gives access to data pointer
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)              :: this
    integer , intent(in) :: data_index
    integer, pointer :: int_1d(:)
    !
    ! !LOCAL VARIABLES:

    if (data_index > this%num_data) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to access ' // &
          'data index that is larger than number of data in the list.')
    endif

    if (.not. associated(this%data_ptr(data_index)%data%data_int_1d)) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to asscess ' // &
          'an unallocated data_int_1d.')
    else
       int_1d => this%data_ptr(data_index)%data%data_int_1d
    endif

  end subroutine EMIDListGetPointerToInt1D

  !------------------------------------------------------------------------
  subroutine EMIDListGetPointerToInt2D(this, data_index, int_2d)
    !
    ! !DESCRIPTION:
    ! Gives access to data pointer
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)              :: this
    integer , intent(in) :: data_index
    integer, pointer :: int_2d(:,:)
    !
    ! !LOCAL VARIABLES:

    if (data_index > this%num_data) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to access ' // &
          'data index that is larger than number of data in the list.')
    endif

    if (.not. associated(this%data_ptr(data_index)%data%data_int_2d)) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to asscess ' // &
          'an unallocated data_int_2d.')
    else
       int_2d => this%data_ptr(data_index)%data%data_int_2d
    endif

  end subroutine EMIDListGetPointerToInt2D

  !------------------------------------------------------------------------
  subroutine EMIDListGetPointerToInt3D(this, data_index, int_3d)
    !
    ! !DESCRIPTION:
    ! Gives access to data pointer
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)              :: this
    integer , intent(in) :: data_index
    integer, pointer :: int_3d(:,:,:)
    !
    ! !LOCAL VARIABLES:

    if (data_index > this%num_data) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to access ' // &
          'data index that is larger than number of data in the list.')
    endif

    if (.not. associated(this%data_ptr(data_index)%data%data_int_3d)) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to asscess ' // &
          'an unallocated data_int_3d.')
    else
       int_3d => this%data_ptr(data_index)%data%data_int_3d
    endif

  end subroutine EMIDListGetPointerToInt3D

  !------------------------------------------------------------------------
  subroutine EMIDListGetPointerToReal1D(this, data_index, real_1d)
    !
    ! !DESCRIPTION:
    ! Gives access to data pointer
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)              :: this
    integer , intent(in) :: data_index
    real(r8), pointer :: real_1d(:)
    !
    ! !LOCAL VARIABLES:

    if (data_index > this%num_data) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to access ' // &
          'data index that is larger than number of data in the list.')
    endif

    if (.not. associated(this%data_ptr(data_index)%data%data_real_1d)) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to asscess ' // &
          'an unallocated data_real_1d.')
    else
       real_1d => this%data_ptr(data_index)%data%data_real_1d
    endif

  end subroutine EMIDListGetPointerToReal1D

  !------------------------------------------------------------------------
  subroutine EMIDListGetPointerToReal2D(this, data_index, real_2d)
    !
    ! !DESCRIPTION:
    ! Gives access to data pointer
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)              :: this
    integer , intent(in) :: data_index
    real(r8), pointer :: real_2d(:,:)
    !
    ! !LOCAL VARIABLES:

    if (data_index > this%num_data) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to access ' // &
          'data index that is larger than number of data in the list.')
    endif

    if (.not. associated(this%data_ptr(data_index)%data%data_real_2d)) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to asscess ' // &
          'an unallocated data_real_2d.')
    else
       real_2d => this%data_ptr(data_index)%data%data_real_2d
    endif

  end subroutine EMIDListGetPointerToReal2D

  !------------------------------------------------------------------------
  subroutine EMIDListGetPointerToReal3D(this, data_index, real_3d)
    !
    ! !DESCRIPTION:
    ! Gives access to data pointer
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)              :: this
    integer , intent(in) :: data_index
    real(r8), pointer :: real_3d(:,:,:)
    !
    ! !LOCAL VARIABLES:

    if (data_index > this%num_data) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to access ' // &
          'data index that is larger than number of data in the list.')
    endif

    if (.not. associated(this%data_ptr(data_index)%data%data_real_3d)) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to asscess ' // &
          'an unallocated data_real_3d.')
    else
       real_3d => this%data_ptr(data_index)%data%data_real_3d
    endif

  end subroutine EMIDListGetPointerToReal3D

  !------------------------------------------------------------------------
  subroutine EMIDListGetPointerToReal4D(this, data_index, real_4d)
    !
    ! !DESCRIPTION:
    ! Gives access to data pointer
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)              :: this
    integer , intent(in) :: data_index
    real(r8), pointer :: real_4d(:,:,:,:)
    !
    ! !LOCAL VARIABLES:

    if (data_index > this%num_data) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to access ' // &
          'data index that is larger than number of data in the list.')
    endif

    if (.not. associated(this%data_ptr(data_index)%data%data_real_4d)) then
       call endrun(msg='EMIDListGetPointerToData: Attempting to asscess ' // &
          'an unallocated data_real_4d.')
    else
       real_4d => this%data_ptr(data_index)%data%data_real_4d
    endif

  end subroutine EMIDListGetPointerToReal4D

  !------------------------------------------------------------------------
  subroutine EMIDListDestroy(this)
    !
    ! !DESCRIPTION:
    ! Destroys a EMID list
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list) :: this
    ! !LOCAL VARIABLES:
    class(emi_data)      , pointer    :: cur_data
    class(emi_data)      , pointer    :: old_data

    cur_data => this%first
    do
       if (.not.associated(cur_data)) exit

       old_data => cur_data
       cur_data => cur_data%next
       call old_data%Destroy()

    enddo

    this%num_data = 0

    nullify(this%first)
    nullify(this%last)
    nullify(this%data_ptr)

  end subroutine EMIDListDestroy

end module ExternalModelInterfaceDataMod
