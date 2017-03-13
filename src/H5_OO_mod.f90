module H5_OO_mod
use H5_Func_mod
use Types_mod

implicit none

  type, abstract :: H5Base

    integer(i32) :: id

    contains

      procedure(getNameInterface), deferred   :: getName
      procedure(getParentInterface), deferred :: getParent

  end type H5Base

  type, abstract, extends(H5Base) :: H5Attributable

    contains

  !   procedure, public  :: hasAttribute
  !   procedure, public  :: renameAttribute
  !   
  !   procedure, private :: getAttributableIds
  !   procedure, private :: setAttributeChar

      procedure, private :: get_Char_Attr0
      procedure, private :: get_Char_Attr1
      procedure, private :: get_Int_Attr0
      procedure, private :: get_Int_Attr1
      procedure, private :: get_Real32_Attr0
      procedure, private :: get_Real32_Attr1
      procedure, private :: get_Real64_Attr0
      procedure, private :: get_Real64_Attr1

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


     generic, public :: setAttribute => &
                    set_Int16_Attr0,    &
                    set_Int16_Attr1,    &
                    set_Int32_Attr0,    &
                    set_Int32_Attr1,    &
                    set_Real32_Attr0,   &
                    set_Real32_Attr1,   &
                    set_Real64_Attr0,   &
                    set_Real64_Attr1,   &
                    set_Char_Attr0,     &
                    set_Char_Attr1

     generic, public :: getAttribute => &
                    get_Char_Attr0,     &
                    get_Char_Attr1,     &
                    get_Int_Attr0,      &
                    get_Int_Attr1,      &
                    get_Real32_Attr0,   &
                    get_Real32_Attr1,   &
                    get_Real64_Attr0,   &
                    get_Real64_Attr1

  end type H5Attributable

  ! --------------------------------------------------------------------------------------


!  type att
!    contains
!      hdf_write_attribute => set
!      hdf_read_attribute => get
!  end type att

!  type dset
!    character(len=255) :: name
!    contains
!      hdf_write_dataset
!      hdf_read_dataset
!      Read_Slab
!      hdf_get_dims
!      hdf_get_rank
!  end type dset

!  type, extends(H5Attributable) :: H5Group
!    contains
!      init
!      hdf_create_group
!      hdf_open_group
!      hdf_close_group
!  end type H5Group

!  type, extends(H5Group) :: H5File
!    character(256) :: filename
!    contains
!    init
!    hdf_open_file
!    hdf_close_file

!  end type H5File

  ! abstract interfaces
  interface
     function getNameInterface(self)
       import H5Base
       class(H5Base), intent(in) :: self
       character(len=256)        :: getNameInterface
     end function getNameInterface

     function getParentInterface(self)
       import H5Base!, H5Group
       class(H5Base), intent(in) :: self
!       type(H5Group)             :: getParentInterface
     end function getParentInterface
  end interface


contains

!function newH5File( fname, state, mode )
!  type(h5file) :: newH5File
!  character(len=*), intent(in) :: filename        !< the HDF5 filename
!  character(len=*), optional, intent(in) :: state !< file state (OLD, NEW, REPLACE)
!  character(len=*), optional, intent(in) :: mode  !< file mode (READ, WRITE, READWRITE)
!  integer(kind=I32) :: error

!  newH5File%id = hdf_open_file(filename, state, mode)
!end function newH5File

!subroutine closeH5File( self )
!  type(h5file) :: self
!  integer(kind=I32) :: error

!  error = hdf_close_file(self%id)

!end subroutine closeH5File

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






























!Read_Att(file_id, a_name, val, d_name, d_idx, gr_id)
!Create_Attr(file_id, name, val, d_name, d_idx, gr_id)
!Read_Dset(loc_id, dset_name, data)
!Create_Dset(obj_id, d_name, val, fill_val, in_chunk_size, comp_level, extendable)
!Read_Slab(obj_id,dset_name,offset,dshape,data)




!hdf_create_group(loc_id, group_name)
!hdf_open_group(loc_id, group_name)
!hdf_close_group(grp_id)


!hdf_get_rank(loc_id, dset_name, d_rank)
!hdf_get_dims(loc_id, dset_name, dims)

!Get_Obj_Id(file_id, d_name, d_idx, gr_id, stat)










end module H5_OO_mod
