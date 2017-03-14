module H5_OO_mod
use H5_Func_mod
use Types_mod

implicit none

!#################################################################################################!
  type :: H5Attributable
    integer(I32) :: id
    contains

  !   procedure, public  :: hasAttribute
  !   procedure, public  :: renameAttribute
  !   
  !   procedure, private :: getAttributableIds
  !   procedure, private :: setAttributeChar

      procedure, private :: set_Int16_Attr0
      procedure, private :: set_Int16_Attr1
      procedure, private :: set_Int32_Attr0
      procedure, private :: set_Int32_Attr1
      procedure, private :: set_Real32_Attr0
      procedure, private :: set_Real32_Attr1
      procedure, private :: set_Real64_Attr0
      procedure, private :: set_Real64_Attr1
      procedure, private :: set_Char_Attr0
      procedure, private :: set_Char_Attr1

      procedure, private :: get_Char_Attr0
      procedure, private :: get_Char_Attr1
      procedure, private :: get_Int_Attr0
      procedure, private :: get_Int_Attr1
      procedure, private :: get_Real32_Attr0
      procedure, private :: get_Real32_Attr1
      procedure, private :: get_Real64_Attr0
      procedure, private :: get_Real64_Attr1

      generic, public :: setAttribute => &
                       set_Int16_Attr0,  &
                       set_Int16_Attr1,  &
                       set_Int32_Attr0,  &
                       set_Int32_Attr1,  &
                       set_Real32_Attr0, &
                       set_Real32_Attr1, &
                       set_Real64_Attr0, &
                       set_Real64_Attr1, &
                       set_Char_Attr0,   &
                       set_Char_Attr1

      generic, public :: getAttribute => &
                       get_Char_Attr0,   &
                       get_Char_Attr1,   &
                       get_Int_Attr0,    &
                       get_Int_Attr1,    &
                       get_Real32_Attr0, &
                       get_Real32_Attr1, &
                       get_Real64_Attr0, &
                       get_Real64_Attr1

  end type H5Attributable

!#################################################################################################!
  type, extends(H5Attributable) :: H5Group
    contains
      procedure, public :: setGroup
      procedure, public :: closeGroup
      procedure, public :: initDset
  end type H5Group

  interface H5Group
    procedure newH5Group
  end interface H5Group

!#################################################################################################!
  type, extends(H5Group) :: H5File
    contains
    procedure, public :: closeFile
  end type H5File

  interface H5File
    procedure newH5File
  end interface H5File

!#################################################################################################!
  type, extends(H5Attributable) :: H5Dataset
    character(len=255) :: d_name
    integer(kind=I32), private :: parent_id

    integer(kind=I32), private :: compression_level
    integer(kind=I32), private :: chunk_size
    integer(kind=I32), private :: fill_value
    logical, private :: extendable
    contains
      procedure, public :: setChunkSize
!      procedure, public :: setCompressionLevel
!      procedure, public :: getDims
!      
!      procedure, private :: set_Int8_1d
!      procedure, private :: set_Int16_1d
!      procedure, private :: set_Int32_1d
!      procedure, private :: set_Real32_1d
!      procedure, private :: set_Real64_1d
!      procedure, private :: set_Int8_2d
!      procedure, private :: set_Int16_2d
!      procedure, private :: set_Int32_2d
!      procedure, private :: set_Real32_2d
!      procedure, private :: set_Real64_2d
!      procedure, private :: set_Int8_3d
!      procedure, private :: set_Int16_3d
!      procedure, private :: set_Int32_3d
!      procedure, private :: set_Real32_3d
!      procedure, private :: set_Real64_3d
!      procedure, private :: set_Int8_4d
!      procedure, private :: set_Int16_4d
!      procedure, private :: set_Int32_4d
!      procedure, private :: set_Real32_4d
!      procedure, private :: set_Real64_4d
!      procedure, private :: set_Int8_5d
!      procedure, private :: set_Int16_5d
!      procedure, private :: set_Int32_5d
!      procedure, private :: set_Real32_5d
!      procedure, private :: set_Real64_5d
!      procedure, private :: set_Int8_6d
!      procedure, private :: set_Int16_6d
!      procedure, private :: set_Int32_6d
!      procedure, private :: set_Real32_6d
!      procedure, private :: set_Real64_6d
!      
!      procedure, private :: get_Int_1d
!      procedure, private :: get_Int_2d
!      procedure, private :: get_Int_3d
!      procedure, private :: get_Int_4d
!      procedure, private :: get_Int_5d
!      procedure, private :: get_Int_6d
!      procedure, private :: get_Real_1d
!      procedure, private :: get_Real_2d
!      procedure, private :: get_Real_3d
!      procedure, private :: get_Real_4d
!      procedure, private :: get_Real_5d
!      procedure, private :: get_Real_6d

!      procedure, private :: get_Int_Slab1d
!      procedure, private :: get_Int_Slab2d
!      procedure, private :: get_Int_Slab3d
!      procedure, private :: get_Int_Slab4d
!      procedure, private :: get_Int_Slab5d
!      procedure, private :: get_Real_Slab1d
!      procedure, private :: get_Real_Slab2d
!      procedure, private :: get_Real_Slab3d
!      procedure, private :: get_Real_Slab4d
!      procedure, private :: get_Real_Slab5d

!      generic, public :: setDataset => &
!                        set_Int8_1d,   &
!                        set_Int16_1d,  &
!                        set_Int32_1d,  &
!                        set_Real32_1d, &
!                        set_Real64_1d, &
!                        set_Int8_2d,   &
!                        set_Int16_2d,  &
!                        set_Int32_2d,  &
!                        set_Real32_2d, &
!                        set_Real64_2d, &
!                        set_Int8_3d,   &
!                        set_Int16_3d,  &
!                        set_Int32_3d,  &
!                        set_Real32_3d, &
!                        set_Real64_3d, &
!                        set_Int8_4d,   &
!                        set_Int16_4d,  &
!                        set_Int32_4d,  &
!                        set_Real32_4d, &
!                        set_Real64_4d, &
!                        set_Int8_5d,   &
!                        set_Int16_5d,  &
!                        set_Int32_5d,  &
!                        set_Real32_5d, &
!                        set_Real64_5d, &
!                        set_Int8_6d,   &
!                        set_Int16_6d,  &
!                        set_Int32_6d,  &
!                        set_Real32_6d, &
!                        set_Real64_6d

!      generic, public :: getDataset => &
!                          get_Int_1d,  &
!                          get_Int_2d,  &
!                          get_Int_3d,  &
!                          get_Int_4d,  &
!                          get_Int_5d,  &
!                          get_Int_6d,  &
!                          get_Real_1d, &
!                          get_Real_2d, &
!                          get_Real_3d, &
!                          get_Real_4d, &
!                          get_Real_5d, &
!                          get_Real_6d

!      generic, public :: getBlock => &
!                    get_Int_Slab1d,  &
!                    get_Int_Slab2d,  &
!                    get_Int_Slab3d,  &
!                    get_Int_Slab4d,  &
!                    get_Int_Slab5d,  &
!                    get_Real_Slab1d, &
!                    get_Real_Slab2d, &
!                    get_Real_Slab3d, &
!                    get_Real_Slab4d, &
!                    get_Real_Slab5d

  end type H5Dataset

!#################################################################################################!

  contains
!#################################################################################################!
!###################################### Attribute Methods ########################################!
!#################################################################################################!
subroutine set_Int16_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I16), intent (in):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Create_Int16_Attr0(self%id, a_name, val)
end subroutine set_Int16_Attr0

subroutine set_Int16_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I16), intent (in):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Create_Int16_Attr1(self%id, a_name, val)
end subroutine set_Int16_Attr1

subroutine set_Int32_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I32), intent (in):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Create_Int32_Attr0(self%id, a_name, val)
end subroutine set_Int32_Attr0

subroutine set_Int32_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I32), intent (in):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Create_Int32_Attr1(self%id, a_name, val)
end subroutine set_Int32_Attr1

subroutine set_Real32_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=SP), intent (in):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Create_Real32_Attr0(self%id, a_name, val)
end subroutine set_Real32_Attr0

subroutine set_Real32_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=SP), intent (in):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Create_Real32_Attr1(self%id, a_name, val)
end subroutine set_Real32_Attr1

subroutine set_Real64_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=DP), intent (in):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Create_Real64_Attr0(self%id, a_name, val)
end subroutine set_Real64_Attr0

subroutine set_Real64_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=DP), intent (in):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Create_Real64_Attr1(self%id, a_name, val)
end subroutine set_Real64_Attr1

subroutine set_Char_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  character(len=*), intent (in):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Create_Char_Attr0(self%id, a_name, val)
end subroutine set_Char_Attr0

subroutine set_Char_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  character(len=*), intent (in):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Create_Char_Attr1(self%id, a_name, val)
end subroutine set_Char_Attr1

subroutine get_Char_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  character(len=*), intent (out):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Read_Char_Attr0(self%id, a_name, val)
end subroutine get_Char_Attr0

subroutine get_Char_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  character(len=*), intent (out):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Read_Char_Attr1(self%id, a_name, val)
end subroutine get_Char_Attr1

subroutine get_Int_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I32), intent (out):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Read_Int_Attr0(self%id, a_name, val)
end subroutine get_Int_Attr0

subroutine get_Int_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I32), intent (out):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Read_Int_Attr1(self%id, a_name, val)
end subroutine get_Int_Attr1

subroutine get_Real32_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=SP), intent (out):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Read_Real32_Attr0(self%id, a_name, val)
end subroutine get_Real32_Attr0

subroutine get_Real32_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=SP), intent (out):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Read_Real32_Attr1(self%id, a_name, val)
end subroutine get_Real32_Attr1

subroutine get_Real64_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=DP), intent (out):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Read_Real64_Attr0(self%id, a_name, val)
end subroutine get_Real64_Attr0

subroutine get_Real64_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=DP), intent (out):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error

  error = Read_Real64_Attr1(self%id, a_name, val)
end subroutine get_Real64_Attr1

!#################################################################################################!
!######################################## Group Methods ##########################################!
!#################################################################################################!
function newH5Group( parent_id, g_name )
  type(H5Group) :: newH5Group
  integer(kind=I32), intent(in) :: parent_id
  character(len=*), intent(in) :: g_name  
  integer(kind=I32) :: error

  newH5Group%id = hdf_open_group(parent_id, g_name)
end function newH5Group

subroutine setGroup(self, g_name, newGroup)
  type(H5Group) :: self
  character(len=*), intent(in) :: g_name
  type(H5Group), intent(out) :: newGroup

  newGroup%id=hdf_create_group(self%id, g_name)
end subroutine setGroup

subroutine closeGroup(self)
  type(H5Group) :: self
  integer(kind=I32) :: error

  error=hdf_close_group(self%id)
end subroutine closeGroup

!#################################################################################################!
!######################################### File Methods ##########################################!
!#################################################################################################!
function newH5File( filename, state, mode )
  type(h5file) :: newH5File
  character(len=*), intent(in) :: filename        !< the HDF5 filename
  character(len=*), optional, intent(in) :: state !< file state (OLD, NEW, REPLACE)
  character(len=*), optional, intent(in) :: mode  !< file mode (READ, WRITE, READWRITE)
  integer(kind=I32) :: error

  newH5File%id = hdf_open_file(filename, state, mode)
end function newH5File

subroutine closeFile( self )
  type(h5file) :: self
  integer(kind=I32) :: error

  error = hdf_close_file(self%id)
end subroutine closeFile

subroutine initDset(self, newDataset, dset_name)
  class(H5Group) :: self
  type(H5Dataset), intent(out) :: newDataset
  character(len=*), intent(in) :: dset_name

  newDataset%d_name            = dset_name
  newDataset%parent_id         = self%id
  newDataset%compression_level = 9
  newDataset%chunk_size        = 100
  newDataset%extendable        = .false.
  newDataset%fill_value        = 0
end subroutine initDset

!#################################################################################################!
!######################################## Dataset Methods ########################################!
!#################################################################################################!
subroutine get_Int_1d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(out) :: val(:)
  integer(kind=I32) :: error

  error=Read_Int_1d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_1d

subroutine get_Int_2d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(out) :: val(:,:)
  integer(kind=I32) :: error

  error=Read_Int_2d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_2d

subroutine get_Int_3d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(out) :: val(:,:,:)
  integer(kind=I32) :: error

  error=Read_Int_3d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_3d

subroutine get_Int_4d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(out) :: val(:,:,:,:)
  integer(kind=I32) :: error

  error=Read_Int_4d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_4d

subroutine get_Int_5d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(out) :: val(:,:,:,:,:)
  integer(kind=I32) :: error

  error=Read_Int_5d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_5d

subroutine get_Int_6d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(out) :: val(:,:,:,:,:,:)
  integer(kind=I32) :: error

  error=Read_Int_6d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_6d

subroutine get_Real_1d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(out) :: val(:)
  integer(kind=I32) :: error

  error=Read_Real_1d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real_1d

subroutine get_Real_2d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(out) :: val(:,:)
  integer(kind=I32) :: error

  error=Read_Real_2d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real_2d

subroutine get_Real_3d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(out) :: val(:,:,:)
  integer(kind=I32) :: error

  error=Read_Real_3d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real_3d

subroutine get_Real_4d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(out) :: val(:,:,:,:)
  integer(kind=I32) :: error

  error=Read_Real_4d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real_4d

subroutine get_Real_5d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(out) :: val(:,:,:,:,:)
  integer(kind=I32) :: error

  error=Read_Real_5d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real_5d

subroutine get_Real_6d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(out) :: val(:,:,:,:,:,:)
  integer(kind=I32) :: error

  error=Read_Real_6d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real_6d

subroutine get_Int_Slab1d(self, offset, dshape, val)
  type(H5Dataset) :: self
  integer(kind=I32), intent(out) :: val(:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Read_Int_1dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Int_Slab1d

subroutine get_Int_Slab2d(self, offset, dshape, val)
  type(H5Dataset) :: self
  integer(kind=I32), intent(out) :: val(:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Read_Int_2dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Int_Slab2d

subroutine get_Int_Slab3d(self, offset, dshape, val)
  type(H5Dataset) :: self
  integer(kind=I32), intent(out) :: val(:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Read_Int_3dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Int_Slab3d

subroutine get_Int_Slab4d(self, offset, dshape, val)
  type(H5Dataset) :: self
  integer(kind=I32), intent(out) :: val(:,:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Read_Int_4dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Int_Slab4d

subroutine get_Int_Slab5d(self, offset, dshape, val)
  type(H5Dataset) :: self
  integer(kind=I32), intent(out) :: val(:,:,:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Read_Int_5dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Int_Slab5d

subroutine get_Real_Slab1d(self, offset, dshape, val)
  type(H5Dataset) :: self
  real(kind=SP), intent(out) :: val(:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Read_Real_1dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Real_Slab1d

subroutine get_Real_Slab2d(self, offset, dshape, val)
  type(H5Dataset) :: self
  real(kind=SP), intent(out) :: val(:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Read_Real_2dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Real_Slab2d

subroutine get_Real_Slab3d(self, offset, dshape, val)
  type(H5Dataset) :: self
  real(kind=SP), intent(out) :: val(:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Read_Real_3dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Real_Slab3d

subroutine get_Real_Slab4d(self, offset, dshape, val)
  type(H5Dataset) :: self
  real(kind=SP), intent(out) :: val(:,:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Read_Real_4dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Real_Slab4d

subroutine get_Real_Slab5d(self, offset, dshape, val)
  type(H5Dataset) :: self
  real(kind=SP), intent(out) :: val(:,:,:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Read_Real_5dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Real_Slab5d


subroutine setChunkSize(self, chunksize)
  type(H5Dataset) :: self
  integer(kind=I32) :: chunksize

  self%chunk_size=chunksize
end subroutine setChunkSize

subroutine setCompressionLevel(self, comp_level)
  type(H5Dataset) :: self
  integer(kind=I32) :: comp_level

  self%compression_level=comp_level
end subroutine setCompressionLevel

subroutine getDims(self, dims)
  type(H5Dataset) :: self
  integer, intent(out) :: dims(:)
  integer(kind=I32) :: error

  error = hdf_get_dims(self%parent_id,self%d_name,dims)
end subroutine getDims

subroutine set_Int8_1d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I8), intent(in) :: val(:)
  integer(kind=I32) :: error
  
  error = Create_Int8_1d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_1d

subroutine set_Int16_1d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I16), intent(in) :: val(:)
  integer(kind=I32) :: error
  
  error = Create_Int16_1d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_1d

subroutine set_Int32_1d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(in) :: val(:)
  integer(kind=I32) :: error
  
  error = Create_Int32_1d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_1d

subroutine set_Real32_1d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=SP), intent(in) :: val(:)
  integer(kind=I32) :: error
  
  error = Create_Real32_1d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_1d

subroutine set_Real64_1d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(in) :: val(:)
  integer(kind=I32) :: error
  
  error = Create_Real64_1d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_1d

subroutine set_Int8_2d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I8), intent(in) :: val(:,:)
  integer(kind=I32) :: error
  
  error = Create_Int8_2d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_2d

subroutine set_Int16_2d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I16), intent(in) :: val(:,:)
  integer(kind=I32) :: error
  
  error = Create_Int16_2d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_2d

subroutine set_Int32_2d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(in) :: val(:,:)
  integer(kind=I32) :: error
  
  error = Create_Int32_2d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_2d

subroutine set_Real32_2d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=SP), intent(in) :: val(:,:)
  integer(kind=I32) :: error
  
  error = Create_Real32_2d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_2d

subroutine set_Real64_2d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(in) :: val(:,:)
  integer(kind=I32) :: error
  
  error = Create_Real64_2d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_2d

subroutine set_Int8_3d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I8), intent(in) :: val(:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int8_3d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_3d

subroutine set_Int16_3d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I16), intent(in) :: val(:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int16_3d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_3d

subroutine set_Int32_3d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(in) :: val(:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int32_3d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_3d

subroutine set_Real32_3d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=SP), intent(in) :: val(:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Real32_3d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_3d

subroutine set_Real64_3d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(in) :: val(:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Real64_3d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_3d

subroutine set_Int8_4d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I8), intent(in) :: val(:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int8_4d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_4d

subroutine set_Int16_4d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I16), intent(in) :: val(:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int16_4d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_4d

subroutine set_Int32_4d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(in) :: val(:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int32_4d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_4d

subroutine set_Real32_4d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=SP), intent(in) :: val(:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Real32_4d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_4d

subroutine set_Real64_4d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(in) :: val(:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Real64_4d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_4d

subroutine set_Int8_5d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I8), intent(in) :: val(:,:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int8_5d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_5d

subroutine set_Int16_5d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I16), intent(in) :: val(:,:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int16_5d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_5d

subroutine set_Int32_5d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(in) :: val(:,:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int32_5d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_5d

subroutine set_Real32_5d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=SP), intent(in) :: val(:,:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Real32_5d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_5d

subroutine set_Real64_5d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(in) :: val(:,:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Real64_5d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_5d

subroutine set_Int8_6d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I8), intent(in) :: val(:,:,:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int8_6d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_6d

subroutine set_Int16_6d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I16), intent(in) :: val(:,:,:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int16_6d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_6d

subroutine set_Int32_6d(self, val)
  type(H5Dataset), intent(in) :: self
  integer(kind=I32), intent(in) :: val(:,:,:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Int32_6d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_6d

subroutine set_Real32_6d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=SP), intent(in) :: val(:,:,:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Real32_6d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_6d

subroutine set_Real64_6d(self, val)
  type(H5Dataset), intent(in) :: self
  real(kind=DP), intent(in) :: val(:,:,:,:,:,:)
  integer(kind=I32) :: error
  
  error = Create_Real64_6d_Dataset(self%parent_id, self%d_name, val, self%fill_value, self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_6d


end module H5_OO_mod
