module H5_OO_mod
use H5_Func_mod
use Types_mod

implicit none

type att
  contains
    hdf_write_attribute => set
    hdf_read_attribute => get
end type att

type dset
  type(att) :: att
  character(len=255) :: name
  contains
    hdf_write_dataset
    hdf_read_dataset
    Read_Slab
    hdf_get_dims
    hdf_get_rank
end type dset

type group
  integer(kind=4) :: id
  type(att) :: att
  type(dset):: dset
  contains
    init
    hdf_create_group
    hdf_open_group
    hdf_close_group
end type group

type, extends(group) :: h5file
  character(256) :: filename
  contains
  init
  hdf_open_file
  hdf_close_file

end type h5file


contains

function newH5File( fname, state, mode )
  type(h5file) :: newH5File
  character(len=*), intent(in) :: filename        !< the HDF5 filename
  character(len=*), optional, intent(in) :: state !< file state (OLD, NEW, REPLACE)
  character(len=*), optional, intent(in) :: mode  !< file mode (READ, WRITE, READWRITE)
  integer(kind=I32) :: error

  newH5File%id = hdf_open_file(filename, state, mode)
end function newH5File

subroutine closeH5File( self )
  type(h5file) :: self
  integer(kind=I32) :: error

  error = hdf_close_file(self%id)

end subroutine closeH5File



Read_Att(file_id, a_name, val, d_name, d_idx, gr_id)
Create_Attr(file_id, name, val, d_name, d_idx, gr_id)
Read_Dset(loc_id, dset_name, data)
Create_Dset(obj_id, d_name, val, fill_val, in_chunk_size, comp_level, extendable)
Read_Slab(obj_id,dset_name,offset,dshape,data)




hdf_create_group(loc_id, group_name)
hdf_open_group(loc_id, group_name)
hdf_close_group(grp_id)


hdf_get_rank(loc_id, dset_name, d_rank)
hdf_get_dims(loc_id, dset_name, dims)

Get_Obj_Id(file_id, d_name, d_idx, gr_id, stat)










end module H5_OO_mod
