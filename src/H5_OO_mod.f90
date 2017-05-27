!#################################################################################################!
!BSD 3-Clause License
!
!Copyright (c) 2017, Ricardo Torres
!All rights reserved.
!
!Redistribution and use in source and binary forms, with or without
!modification, are permitted provided that the following conditions are met:
!
!* Redistributions of source code must retain the above copyright notice, this
!  list of conditions and the following disclaimer.

!* Redistributions in binary form must reproduce the above copyright notice,
!  this list of conditions and the following disclaimer in the documentation
!  and/or other materials provided with the distribution.

!* Neither the name of the copyright holder nor the names of its
!  contributors may be used to endorse or promote products derived from
!  this software without specific prior written permission.
!
!THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
!AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
!FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
!DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
!SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
!CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
!OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!#################################################################################################!

module H5_OO_mod
use H5_Func_mod
use Types_mod

implicit none

!#################################################################################################!
  type :: H5Attributable
    integer(I32) :: id
    contains

      procedure, public  :: Attr_exists
      procedure, public  :: getNumberAttrs
      procedure, public  :: getAttTypeSize
      procedure, public  :: getAttNameByIdx

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

      generic, public    :: setAttribute => &
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

      generic, public    :: getAttribute => &
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
      procedure, public :: openGroup

      procedure, public :: getNumObj
      procedure, public :: getObjNameByIdx
      procedure, public :: isDset
      procedure, public :: isGrp
  end type H5Group

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
    integer(kind=I32), private :: extendable
    contains
      procedure, public :: showstatus

      procedure, public :: setChunkSize
      procedure, public :: setCompressionLevel
      procedure, public :: setFillValue
      procedure, public :: setExtendable
      procedure, public :: getRank
      procedure, public :: getDims
      procedure, public :: getDTypeSize  !    H5T_NO_CLASS_F -1
                                         !    H5T_INTEGER_F   0
                                         !    H5T_FLOAT_F     1
                                         !    H5T_STRING_F    2
                                         !    H5T_BITFIELD_F  3
                                         !    H5T_OPAQUE_F    4
                                         !    H5T_COMPOUND_F  5
                                         !    H5T_REFERENCE_F 6
                                         !    H5T_ENUM_F      7
                                         !    H5T_VLEN_F      8
                                         !    H5T_ARRAY_F     9

      procedure, public :: setEmpty

      procedure, public :: defScale
      procedure, public :: setScale

      procedure, private :: set_Int8_1d
      procedure, private :: set_Int16_1d
      procedure, private :: set_Int32_1d
      procedure, private :: set_Real32_1d
      procedure, private :: set_Real64_1d
      procedure, private :: set_Int8_2d
      procedure, private :: set_Int16_2d
      procedure, private :: set_Int32_2d
      procedure, private :: set_Real32_2d
      procedure, private :: set_Real64_2d
      procedure, private :: set_Int8_3d
      procedure, private :: set_Int16_3d
      procedure, private :: set_Int32_3d
      procedure, private :: set_Real32_3d
      procedure, private :: set_Real64_3d
      procedure, private :: set_Int8_4d
      procedure, private :: set_Int16_4d
      procedure, private :: set_Int32_4d
      procedure, private :: set_Real32_4d
      procedure, private :: set_Real64_4d
      procedure, private :: set_Int8_5d
      procedure, private :: set_Int16_5d
      procedure, private :: set_Int32_5d
      procedure, private :: set_Real32_5d
      procedure, private :: set_Real64_5d
      procedure, private :: set_Int8_6d
      procedure, private :: set_Int16_6d
      procedure, private :: set_Int32_6d
      procedure, private :: set_Real32_6d
      procedure, private :: set_Real64_6d

      procedure, private :: get_Int_1d
      procedure, private :: get_Int_2d
      procedure, private :: get_Int_3d
      procedure, private :: get_Int_4d
      procedure, private :: get_Int_5d
      procedure, private :: get_Int_6d
      procedure, private :: get_Real32_1d
      procedure, private :: get_Real32_2d
      procedure, private :: get_Real32_3d
      procedure, private :: get_Real32_4d
      procedure, private :: get_Real32_5d
      procedure, private :: get_Real32_6d
      procedure, private :: get_Real64_1d
      procedure, private :: get_Real64_2d
      procedure, private :: get_Real64_3d
      procedure, private :: get_Real64_4d
      procedure, private :: get_Real64_5d
      procedure, private :: get_Real64_6d

      procedure, private :: get_Int_Slab1d
      procedure, private :: get_Int_Slab2d
      procedure, private :: get_Int_Slab3d
      procedure, private :: get_Int_Slab4d
      procedure, private :: get_Int_Slab5d
      procedure, private :: get_Real_Slab1d
      procedure, private :: get_Real_Slab2d
      procedure, private :: get_Real_Slab3d
      procedure, private :: get_Real_Slab4d
      procedure, private :: get_Real_Slab5d

      procedure, private :: Extend_Int8_1d
      procedure, private :: Extend_Int16_1d
      procedure, private :: Extend_Int32_1d
      procedure, private :: Extend_Real32_1d
      procedure, private :: Extend_Real64_1d
      procedure, private :: Extend_Int8_2d
      procedure, private :: Extend_Int16_2d
      procedure, private :: Extend_Int32_2d
      procedure, private :: Extend_Real32_2d

      procedure, private :: Extend_Real64_2d
      procedure, private :: Extend_Int8_3d
      procedure, private :: Extend_Int16_3d
      procedure, private :: Extend_Int32_3d
      procedure, private :: Extend_Real32_3d
      procedure, private :: Extend_Real64_3d
      procedure, private :: Extend_Int8_4d
      procedure, private :: Extend_Int16_4d
      procedure, private :: Extend_Int32_4d
      procedure, private :: Extend_Real32_4d
      procedure, private :: Extend_Real64_4d
      procedure, private :: Extend_Int8_5d
      procedure, private :: Extend_Int16_5d
      procedure, private :: Extend_Int32_5d
      procedure, private :: Extend_Real32_5d
      procedure, private :: Extend_Real64_5d
      procedure, private :: Extend_Int8_6d
      procedure, private :: Extend_Int16_6d
      procedure, private :: Extend_Int32_6d
      procedure, private :: Extend_Real32_6d
      procedure, private :: Extend_Real64_6d

      generic, public :: setDataset => &
                        set_Int8_1d,   &
                        set_Int16_1d,  &
                        set_Int32_1d,  &
                        set_Real32_1d, &
                        set_Real64_1d, &
                        set_Int8_2d,   &
                        set_Int16_2d,  &
                        set_Int32_2d,  &
                        set_Real32_2d, &
                        set_Real64_2d, &
                        set_Int8_3d,   &
                        set_Int16_3d,  &
                        set_Int32_3d,  &
                        set_Real32_3d, &
                        set_Real64_3d, &
                        set_Int8_4d,   &
                        set_Int16_4d,  &
                        set_Int32_4d,  &
                        set_Real32_4d, &
                        set_Real64_4d, &
                        set_Int8_5d,   &
                        set_Int16_5d,  &
                        set_Int32_5d,  &
                        set_Real32_5d, &
                        set_Real64_5d, &
                        set_Int8_6d,   &
                        set_Int16_6d,  &
                        set_Int32_6d,  &
                        set_Real32_6d, &
                        set_Real64_6d

      generic, public :: getDataset => &
                          get_Int_1d,  &
                          get_Int_2d,  &
                          get_Int_3d,  &
                          get_Int_4d,  &
                          get_Int_5d,  &
                          get_Int_6d,  &
                          get_Real32_1d, &
                          get_Real32_2d, &
                          get_Real32_3d, &
                          get_Real32_4d, &
                          get_Real32_5d, &
                          get_Real32_6d, &
                          get_Real64_1d, &
                          get_Real64_2d, &
                          get_Real64_3d, &
                          get_Real64_4d, &
                          get_Real64_5d, &
                          get_Real64_6d

      generic, public :: getBlock => &
                    get_Int_Slab1d,  &
                    get_Int_Slab2d,  &
                    get_Int_Slab3d,  &
                    get_Int_Slab4d,  &
                    get_Int_Slab5d,  &
                    get_Real_Slab1d, &
                    get_Real_Slab2d, &
                    get_Real_Slab3d, &
                    get_Real_Slab4d, &
                    get_Real_Slab5d

      generic, public :: extendDataset => &
                        Extend_Int8_1d,   &
                        Extend_Int16_1d,  &
                        Extend_Int32_1d,  &
                        Extend_Real32_1d, &
                        Extend_Real64_1d, &
                        Extend_Int8_2d,   &
                        Extend_Int16_2d,  &
                        Extend_Int32_2d,  &
                        Extend_Real32_2d, &
                        Extend_Real64_2d, &
                        Extend_Int8_3d,   &
                        Extend_Int16_3d,  &
                        Extend_Int32_3d,  &
                        Extend_Real32_3d, &
                        Extend_Real64_3d, &
                        Extend_Int8_4d,   &
                        Extend_Int16_4d,  &
                        Extend_Int32_4d,  &
                        Extend_Real32_4d, &
                        Extend_Real64_4d, &
                        Extend_Int8_5d,   &
                        Extend_Int16_5d,  &
                        Extend_Int32_5d,  &
                        Extend_Real32_5d, &
                        Extend_Real64_5d, &
                        Extend_Int8_6d,   &
                        Extend_Int16_6d,  &
                        Extend_Int32_6d,  &
                        Extend_Real32_6d, &
                        Extend_Real64_6d




  end type H5Dataset

  interface H5Dataset
    procedure newH5Dataset
  end interface H5Dataset

!#################################################################################################!

  contains
!#################################################################################################!
!###################################### Attribute Methods ########################################!
!#################################################################################################!
function Attr_exists(self, a_name)
  class(H5Attributable), intent(in) :: self
  character(len=*), intent (in):: a_name
  logical :: Attr_exists
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      Attr_exists = Ch_Attr_exist(dset_id,a_name)
      error = close_dset(dset_id)
    class default
      Attr_exists = Ch_Attr_exist(self%id,a_name)
  end select
end function Attr_exists

subroutine getAttNameByIdx(self, idx, a_name)
  class(H5Attributable), intent(in) :: self
  integer(kind=I32), intent(in) :: idx
  character(len=*), intent(out) :: a_name
  character(len=80) :: obj_name
  integer :: dset_id
  integer :: hdferr

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      hdferr = get_att_name_idx(dset_id, self%d_name, idx, a_name)
      hdferr = close_dset(dset_id)
    class default
      hdferr = get_obj_name(self%id, obj_name)
      hdferr = get_att_name_idx(self%id, trim(obj_name), idx, a_name)
  end select
end subroutine getAttNameByIdx

subroutine getNumberAttrs(self, n_attrs)
  class(H5Attributable), intent(in) :: self
  integer(kind=I32), intent(out) :: n_attrs
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      n_attrs = number_attrs(dset_id)
      error = close_dset(dset_id)
    class default
      n_attrs = number_attrs(self%id)
  end select
end subroutine getNumberAttrs

subroutine getAttTypeSize(self, a_name, att_type, att_type_size)
  class(H5Attributable), intent(in) :: self
  character(len=*), intent (in):: a_name
  integer(kind=I32), intent(out) :: att_type
  integer(kind=I64), intent(out) :: att_type_size
  integer(kind=I32) :: hdferr
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      call attr_type_size(dset_id, a_name, att_type, att_type_size, hdferr)
      hdferr = close_dset(dset_id)
    class default
      call attr_type_size(self%id, a_name, att_type, att_type_size, hdferr)
  end select
end subroutine getAttTypeSize

subroutine set_Int16_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I16), intent (in):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Create_Int16_Attr0(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Create_Int16_Attr0(self%id, a_name, val)
  end select
end subroutine set_Int16_Attr0

subroutine set_Int16_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I16), intent (in):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Create_Int16_Attr1(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Create_Int16_Attr1(self%id, a_name, val)
  end select
end subroutine set_Int16_Attr1

subroutine set_Int32_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I32), intent (in):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Create_Int32_Attr0(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Create_Int32_Attr0(self%id, a_name, val)
  end select
end subroutine set_Int32_Attr0

subroutine set_Int32_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I32), intent (in):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Create_Int32_Attr1(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Create_Int32_Attr1(self%id, a_name, val)
  end select
end subroutine set_Int32_Attr1

subroutine set_Real32_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=SP), intent (in):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Create_Real32_Attr0(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Create_Real32_Attr0(self%id, a_name, val)
  end select
end subroutine set_Real32_Attr0

subroutine set_Real32_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=SP), intent (in):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Create_Real32_Attr1(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Create_Real32_Attr1(self%id, a_name, val)
  end select
end subroutine set_Real32_Attr1

subroutine set_Real64_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=DP), intent (in):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Create_Real64_Attr0(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Create_Real64_Attr0(self%id, a_name, val)
  end select
end subroutine set_Real64_Attr0

subroutine set_Real64_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=DP), intent (in):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Create_Real64_Attr1(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Create_Real64_Attr1(self%id, a_name, val)
  end select
end subroutine set_Real64_Attr1

subroutine set_Char_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  character(len=*), intent (in):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Create_Char_Attr0(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Create_Char_Attr0(self%id, a_name, val)
  end select
end subroutine set_Char_Attr0

subroutine set_Char_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  character(len=*), intent (in):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Create_Char_Attr1(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Create_Char_Attr1(self%id, a_name, val)
  end select
end subroutine set_Char_Attr1

subroutine get_Char_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  character(len=*), intent (out):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Read_Char_Attr0(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Read_Char_Attr0(self%id, a_name, val)
  end select
end subroutine get_Char_Attr0

subroutine get_Char_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  character(len=*), intent (out):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Read_Char_Attr1(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Read_Char_Attr1(self%id, a_name, val)
  end select
end subroutine get_Char_Attr1

subroutine get_Int_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I32), intent (out):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Read_Int_Attr0(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Read_Int_Attr0(self%id, a_name, val)
  end select
end subroutine get_Int_Attr0

subroutine get_Int_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  integer(kind=I32), intent (out):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Read_Int_Attr1(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Read_Int_Attr1(self%id, a_name, val)
  end select
end subroutine get_Int_Attr1

subroutine get_Real32_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=SP), intent (out):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Read_Real32_Attr0(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Read_Real32_Attr0(self%id, a_name, val)
  end select
end subroutine get_Real32_Attr0

subroutine get_Real32_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=SP), intent (out):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Read_Real32_Attr1(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Read_Real32_Attr1(self%id, a_name, val)
  end select
end subroutine get_Real32_Attr1

subroutine get_Real64_Attr0(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=DP), intent (out):: val
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Read_Real64_Attr0(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Read_Real64_Attr0(self%id, a_name, val)
  end select
end subroutine get_Real64_Attr0

subroutine get_Real64_Attr1(self, a_name, val)
  class(H5Attributable), intent(in) :: self
  real(kind=DP), intent (out):: val(:)
  character(len=*), intent (in):: a_name
  integer(kind=I32) :: error
  integer(kind=I32) :: dset_id

  select type (self)
    class is (H5Dataset)
      dset_id = open_dset(self%parent_id,self%d_name)
      error = Read_Real64_Attr1(dset_id, a_name, val)
      error = close_dset(dset_id)
    class default
      error = Read_Real64_Attr1(self%id, a_name, val)
  end select
end subroutine get_Real64_Attr1

!#################################################################################################!
!######################################## Group Methods ##########################################!
!#################################################################################################!
subroutine openGroup(self, g_name, newGroup)
  class(H5Group) :: self
  type(H5Group), intent(out) :: newGroup
  character(len=*), intent(in) :: g_name

  newGroup%id = hdf_open_group(self%id, g_name)
end subroutine openGroup

subroutine setGroup(self, g_name, newGroup)
  class(H5Group) :: self
  character(len=*), intent(in) :: g_name
  type(H5Group), intent(out) :: newGroup

  newGroup%id=hdf_create_group(self%id, g_name)
end subroutine setGroup

subroutine closeGroup(self)
  class(H5Group) :: self
  integer(kind=I32) :: error

  error=hdf_close_group(self%id)
end subroutine closeGroup

subroutine getNumObj(self, nlinks)
  class(H5Group) :: self
  integer(kind=I32), intent(out) :: nlinks
  integer(kind=I32) :: error

  error=grp_num_of_obj(self%id, nlinks)
end subroutine getNumObj

subroutine getObjNameByIdx(self, idx, obj_name)
  class(H5Group) :: self
  integer(kind=I32), intent(in) :: idx
  character(len=*), intent(out) :: obj_name
  integer(kind=I32) :: hdferr

  hdferr = grp_obj_name_idx(self%id, idx, obj_name)
end subroutine getObjNameByIdx

function isDset(self, obj_name)
  class(H5Group) :: self
  character(len=*), intent(in) :: obj_name
  logical :: isDset
  integer(kind=I32) :: hdferr

  hdferr = obj_is_dset(self%id, obj_name, isDset)
end function isDset

function isGrp(self, obj_name)
  class(H5Group) :: self
  character(len=*), intent(in) :: obj_name
  logical :: isGrp
  integer(kind=I32) :: hdferr

  hdferr = obj_is_grp(self%id, obj_name, isGrp)
end function isGrp
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
  class(H5File), intent(in) :: self
  integer(kind=I32) :: error

  error = hdf_close_file(self%id)
end subroutine closeFile


!#################################################################################################!
!######################################## Dataset Methods ########################################!
!#################################################################################################!
function newH5Dataset(dset_name, parent_Group)
  type(H5Dataset) :: newH5Dataset
  character(len=*), intent(in) :: dset_name
  class(H5Group), intent(in) :: parent_Group

  newH5Dataset%d_name            = dset_name
  newH5Dataset%parent_id         = parent_Group%id
  newH5Dataset%compression_level = 9
  newH5Dataset%chunk_size        = 100
  newH5Dataset%extendable        = 0
  newH5Dataset%fill_value        = 0
end function newH5Dataset

subroutine setEmpty(self)
  class(H5Dataset), intent(inout) :: self

  self%id=Create_Empty_Dataset(self%parent_id,self%d_name)
end subroutine setEmpty

subroutine get_Int_1d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(out) :: val(:)

  self%id = Read_Int_1d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_1d

subroutine get_Int_2d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(out) :: val(:,:)

  self%id = Read_Int_2d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_2d

subroutine get_Int_3d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(out) :: val(:,:,:)

  self%id = Read_Int_3d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_3d

subroutine get_Int_4d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(out) :: val(:,:,:,:)

  self%id = Read_Int_4d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_4d

subroutine get_Int_5d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(out) :: val(:,:,:,:,:)

  self%id = Read_Int_5d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_5d

subroutine get_Int_6d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(out) :: val(:,:,:,:,:,:)

  self%id = Read_Int_6d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Int_6d

subroutine get_Real32_1d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(out) :: val(:)

  self%id = Read_Real32_1d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real32_1d

subroutine get_Real32_2d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(out) :: val(:,:)

  self%id = Read_Real32_2d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real32_2d

subroutine get_Real32_3d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(out) :: val(:,:,:)

  self%id = Read_Real32_3d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real32_3d

subroutine get_Real32_4d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(out) :: val(:,:,:,:)

  self%id = Read_Real32_4d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real32_4d

subroutine get_Real32_5d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(out) :: val(:,:,:,:,:)

  self%id = Read_Real32_5d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real32_5d

subroutine get_Real32_6d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(out) :: val(:,:,:,:,:,:)

  self%id = Read_Real32_6d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real32_6d

subroutine get_Real64_1d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(out) :: val(:)

  self%id = Read_Real64_1d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real64_1d

subroutine get_Real64_2d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(out) :: val(:,:)

  self%id = Read_Real64_2d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real64_2d

subroutine get_Real64_3d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(out) :: val(:,:,:)

  self%id = Read_Real64_3d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real64_3d

subroutine get_Real64_4d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(out) :: val(:,:,:,:)

  self%id = Read_Real64_4d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real64_4d

subroutine get_Real64_5d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(out) :: val(:,:,:,:,:)

  self%id = Read_Real64_5d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real64_5d

subroutine get_Real64_6d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(out) :: val(:,:,:,:,:,:)

  self%id = Read_Real64_6d_dataset(self%parent_id, self%d_name, val)
end subroutine get_Real64_6d

subroutine get_Int_Slab1d(self, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(out) :: val(:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)

  self%id = Read_Int_1dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Int_Slab1d

subroutine get_Int_Slab2d(self, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(out) :: val(:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)

  self%id = Read_Int_2dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Int_Slab2d

subroutine get_Int_Slab3d(self, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(out) :: val(:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)

  self%id = Read_Int_3dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Int_Slab3d

subroutine get_Int_Slab4d(self, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(out) :: val(:,:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)

  self%id = Read_Int_4dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Int_Slab4d

subroutine get_Int_Slab5d(self, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(out) :: val(:,:,:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)

  self%id = Read_Int_5dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Int_Slab5d

subroutine get_Real_Slab1d(self, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(out) :: val(:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)

  self%id = Read_Real_1dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Real_Slab1d

subroutine get_Real_Slab2d(self, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(out) :: val(:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)

  self%id = Read_Real_2dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Real_Slab2d

subroutine get_Real_Slab3d(self, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(out) :: val(:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)

  self%id = Read_Real_3dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Real_Slab3d

subroutine get_Real_Slab4d(self, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(out) :: val(:,:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)

  self%id = Read_Real_4dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Real_Slab4d

subroutine get_Real_Slab5d(self, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(out) :: val(:,:,:,:,:)
  integer(kind=I32), parameter :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)

  self%id = Read_Real_5dSlab(self%parent_id, self%d_name, offset, dshape, val)
end subroutine get_Real_Slab5d

subroutine showstatus(self)
  class(H5Dataset) :: self

  print*,self%parent_id
  print*,self%compression_level
  print*,self%chunk_size
  print*,trim(self%d_name)
  print*,self%id
end subroutine showstatus

subroutine setChunkSize(self, chunksize)
  class(H5Dataset) :: self
  integer(kind=I32) :: chunksize

  self%chunk_size=chunksize
end subroutine setChunkSize

subroutine setCompressionLevel(self, comp_level)
  class(H5Dataset) :: self
  integer(kind=I32) :: comp_level

  self%compression_level=comp_level
end subroutine setCompressionLevel

subroutine setFillValue(self, fillvalue)
  class(H5Dataset) :: self
  integer(kind=I32) :: fillvalue

  self%fill_value=fillvalue
end subroutine setFillValue

subroutine setExtendable(self,extdims)
  class(H5Dataset) :: self
  integer(kind=I32) :: extdims

  self%extendable=extdims
end subroutine setExtendable

subroutine getRank(self, d_rank)
  class(H5Dataset) :: self
  integer, intent(out) :: d_rank
  integer(kind=I32) :: error

  error = hdf_get_rank(self%parent_id,self%d_name,d_rank)
end subroutine getRank

subroutine getDims(self, dims)
  class(H5Dataset) :: self
  integer, intent(out) :: dims(:)
  integer(kind=I32) :: error

  error = hdf_get_dims(self%parent_id,self%d_name,dims)
end subroutine getDims

subroutine getDTypeSize(self,dset_type, dset_type_size)
  class(H5Dataset) :: self
  integer(kind=I32), intent(out) :: dset_type
  integer(kind=I64), intent(out) :: dset_type_size
  integer(kind=I32) :: error

  call get_dset_type(self%parent_id, self%d_name, dset_type, dset_type_size, error)
end subroutine getDTypeSize

subroutine set_Int8_1d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I8), intent(in) :: val(:)

  self%id = Create_Int8_1d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_1d

subroutine set_Int16_1d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I16), intent(in) :: val(:)

  self%id = Create_Int16_1d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_1d

subroutine set_Int32_1d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(in) :: val(:)

  self%id = Create_Int32_1d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_1d

subroutine set_Real32_1d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(in) :: val(:)

  self%id = Create_Real32_1d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_1d

subroutine set_Real64_1d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(in) :: val(:)

  self%id = Create_Real64_1d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_1d

subroutine set_Int8_2d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I8), intent(in) :: val(:,:)

  self%id = Create_Int8_2d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_2d

subroutine set_Int16_2d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I16), intent(in) :: val(:,:)

  self%id = Create_Int16_2d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_2d

subroutine set_Int32_2d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(in) :: val(:,:)

  self%id = Create_Int32_2d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_2d

subroutine set_Real32_2d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(in) :: val(:,:)

  self%id = Create_Real32_2d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_2d

subroutine set_Real64_2d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(in) :: val(:,:)

  self%id = Create_Real64_2d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_2d

subroutine set_Int8_3d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I8), intent(in) :: val(:,:,:)

  self%id = Create_Int8_3d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_3d

subroutine set_Int16_3d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I16), intent(in) :: val(:,:,:)

  self%id = Create_Int16_3d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_3d

subroutine set_Int32_3d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(in) :: val(:,:,:)

  self%id = Create_Int32_3d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_3d

subroutine set_Real32_3d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(in) :: val(:,:,:)

  self%id = Create_Real32_3d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_3d

subroutine set_Real64_3d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(in) :: val(:,:,:)

  self%id = Create_Real64_3d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_3d

subroutine set_Int8_4d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I8), intent(in) :: val(:,:,:,:)

  self%id = Create_Int8_4d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_4d

subroutine set_Int16_4d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I16), intent(in) :: val(:,:,:,:)

  self%id = Create_Int16_4d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_4d

subroutine set_Int32_4d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(in) :: val(:,:,:,:)

  self%id = Create_Int32_4d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_4d

subroutine set_Real32_4d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(in) :: val(:,:,:,:)

  self%id = Create_Real32_4d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_4d

subroutine set_Real64_4d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(in) :: val(:,:,:,:)

  self%id = Create_Real64_4d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_4d

subroutine set_Int8_5d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I8), intent(in) :: val(:,:,:,:,:)

  self%id = Create_Int8_5d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_5d

subroutine set_Int16_5d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I16), intent(in) :: val(:,:,:,:,:)

  self%id = Create_Int16_5d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_5d

subroutine set_Int32_5d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(in) :: val(:,:,:,:,:)

  self%id = Create_Int32_5d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_5d

subroutine set_Real32_5d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(in) :: val(:,:,:,:,:)

  self%id = Create_Real32_5d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_5d

subroutine set_Real64_5d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(in) :: val(:,:,:,:,:)

  self%id = Create_Real64_5d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_5d

subroutine set_Int8_6d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I8), intent(in) :: val(:,:,:,:,:,:)

  self%id = Create_Int8_6d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int8_6d

subroutine set_Int16_6d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I16), intent(in) :: val(:,:,:,:,:,:)

  self%id = Create_Int16_6d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int16_6d

subroutine set_Int32_6d(self, val)
  class(H5Dataset), intent(inout) :: self
  integer(kind=I32), intent(in) :: val(:,:,:,:,:,:)

  self%id = Create_Int32_6d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Int32_6d

subroutine set_Real32_6d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=SP), intent(in) :: val(:,:,:,:,:,:)

  self%id = Create_Real32_6d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real32_6d

subroutine set_Real64_6d(self, val)
  class(H5Dataset), intent(inout) :: self
  real(kind=DP), intent(in) :: val(:,:,:,:,:,:)

  self%id = Create_Real64_6d_Dataset(self%parent_id, self%d_name, val, self%fill_value, &
                                        self%chunk_size, self%compression_level, self%extendable)
end subroutine set_Real64_6d

subroutine Extend_Int8_1d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I8), intent(in) :: val(:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int8_1d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int8_1d

subroutine Extend_Int16_1d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I16), intent(in) :: val(:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int16_1d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int16_1d

subroutine Extend_Int32_1d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(in) :: val(:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int32_1d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int32_1d

subroutine Extend_Real32_1d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(in) :: val(:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real32_1d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real32_1d

subroutine Extend_Real64_1d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=DP), intent(in) :: val(:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real64_1d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real64_1d

subroutine Extend_Int8_2d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I8), intent(in) :: val(:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int8_2d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int8_2d

subroutine Extend_Int16_2d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I16), intent(in) :: val(:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int16_2d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int16_2d

subroutine Extend_Int32_2d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(in) :: val(:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int32_2d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int32_2d

subroutine Extend_Real32_2d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(in) :: val(:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real32_2d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real32_2d

subroutine Extend_Real64_2d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=DP), intent(in) :: val(:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real64_2d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real64_2d

subroutine Extend_Int8_3d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I8), intent(in) :: val(:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int8_3d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int8_3d

subroutine Extend_Int16_3d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I16), intent(in) :: val(:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int16_3d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int16_3d

subroutine Extend_Int32_3d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(in) :: val(:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int32_3d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int32_3d

subroutine Extend_Real32_3d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(in) :: val(:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real32_3d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real32_3d

subroutine Extend_Real64_3d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=DP), intent(in) :: val(:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real64_3d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real64_3d

subroutine Extend_Int8_4d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I8), intent(in) :: val(:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int8_4d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int8_4d

subroutine Extend_Int16_4d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I16), intent(in) :: val(:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int16_4d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int16_4d

subroutine Extend_Int32_4d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(in) :: val(:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int32_4d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int32_4d

subroutine Extend_Real32_4d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(in) :: val(:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real32_4d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real32_4d

subroutine Extend_Real64_4d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=DP), intent(in) :: val(:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real64_4d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real64_4d

subroutine Extend_Int8_5d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I8), intent(in) :: val(:,:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int8_5d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int8_5d

subroutine Extend_Int16_5d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I16), intent(in) :: val(:,:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int16_5d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int16_5d

subroutine Extend_Int32_5d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(in) :: val(:,:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int32_5d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int32_5d

subroutine Extend_Real32_5d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(in) :: val(:,:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real32_5d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real32_5d

subroutine Extend_Real64_5d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=DP), intent(in) :: val(:,:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real64_5d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real64_5d

subroutine Extend_Int8_6d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I8), intent(in) :: val(:,:,:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int8_6d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int8_6d

subroutine Extend_Int16_6d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I16), intent(in) :: val(:,:,:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int16_6d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int16_6d

subroutine Extend_Int32_6d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  integer(kind=I32), intent(in) :: val(:,:,:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Int32_6d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Int32_6d

subroutine Extend_Real32_6d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=SP), intent(in) :: val(:,:,:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real32_6d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real32_6d

subroutine Extend_Real64_6d(self, new_size, offset, dshape, val)
  class(H5Dataset) :: self
  real(kind=DP), intent(in) :: val(:,:,:,:,:,:)
  integer(kind=I32), parameter  :: D_RANK=rank(val)
  integer(kind=I64), intent(in) :: new_size(D_RANK)
  integer(kind=I64), intent(in) :: offset(D_RANK)
  integer(kind=I64), intent(in) :: dshape(D_RANK)
  integer(kind=I32) :: error

  error = Extend_Real64_6d_Dataset(self%parent_id, self%d_name, new_size, offset, dshape, val)
end subroutine Extend_Real64_6d

subroutine defScale(self,dim_name)
  class(H5Dataset) :: self
  character(len=*), intent(in), optional :: dim_name
  integer(kind=I32) :: ierr
  integer(kind=I32) :: dim_id, dset_id
  
  dim_id = open_dset(self%parent_id,self%d_name)
  if (present(dim_name)) then
    ierr = def_scale(dim_id,dim_name)
  else
    ierr = def_scale(dim_id)
  end if
end subroutine defScale

subroutine setScale(self,dim_dset,idx_dim)
  class(H5Dataset) :: self
  class(H5Dataset), intent(in) :: dim_dset
  integer(kind=I32), intent(in) :: idx_dim
  integer(kind=I32) :: ierr
  integer(kind=I32) :: dim_id, dset_id
  
  dset_id = open_dset(self%parent_id,self%d_name)
  dim_id = open_dset(dim_dset%parent_id,dim_dset%d_name)
  ierr = set_scale(dset_id,dim_id,idx_dim)
  ierr = close_dset(dim_id)
  ierr = close_dset(dset_id)
end subroutine setScale

end module H5_OO_mod
