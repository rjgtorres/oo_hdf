module H5_Func_mod
  use hdf5
  use Types_mod
  use Strings_Func_mod, only: To_upper
  implicit none
  integer, private, parameter :: LEN_STR_ATTR  = 80

  contains

  function hdf_open_file(filename, state, mode) result(file_id)
    integer(I32) :: file_id            !< HDF5 id of the file
    character(len=*), intent(in) :: filename        !< the HDF5 filename
    character(len=*), optional, intent(in) :: state !< file state (OLD, NEW, REPLACE)
    character(len=*), optional, intent(in) :: mode  !< file mode (READ, WRITE, READWRITE)
    integer(I32) :: hdferror
    character(len=16) :: state2, mode2

    ! open hdf5 interface
    call h5open_f(hdferror)

    ! set defaults
    state2 = 'NEW'
    if (present(state)) state2 = To_upper(state)
    mode2 = 'READWRITE'
    if (present(state)) mode2 = To_upper(mode)

    ! open/create hdf5 file
    if (state2 == 'OLD' .or. state2 == 'O') then
      if (mode2 == 'READ' .or. mode2 == 'R') then
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferror)
      elseif ( (mode2 == 'WRITE') .or. (mode2 == 'W') .or. &
           (mode2 == 'READWRITE') .or. (mode2 == 'RW') ) then
        call h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferror)
      else
        print*,"hdf_open: mode = "//trim(mode2)//" not supported." 
        print*,"Use READ, WRITE or READWRITE (R, W, RW)"
        stop
      end if
    elseif (state2 == 'NEW' .or. state2 == 'N') then
      call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror)
    elseif (state2 == 'REPLACE' .or. state2 == 'RP') then
      call h5fcreate_f(filename, H5F_ACC_EXCL_F, file_id, hdferror)
    else
      print*,"hdf_open: state = "//trim(state2)//" not supported." 
      print*,"Use OLD, NEW or REPLACE (O, N, RP)"
      stop
    end if

  end function hdf_open_file

  function hdf_close_file(file_id) result(hdferror)
    integer(I32), intent(in) :: file_id  !< file id to be closed
    integer :: hdferror

    call h5fclose_f(file_id, hdferror)
    call h5close_f(hdferror)
  end function hdf_close_file

  function hdf_create_group(loc_id, group_name) result(grp_id)
    integer(I32), intent(in) :: loc_id         !< location id where to put the group
    character(len=*), intent(in) :: group_name   !< name of the group
    integer(I32) :: grp_id
    integer(I32) :: hdferror

    call h5gcreate_f(loc_id, group_name, grp_id, hdferror)
  end function hdf_create_group
  
  function hdf_open_group(loc_id, group_name) result(grp_id)
    integer(I32), intent(in) :: loc_id         !< location id where to put the group
    character(len=*), intent(in) :: group_name   !< name of the group
    integer(I32) :: grp_id      !< id for the group
    integer :: hdferror

    call h5gopen_f(loc_id, group_name, grp_id, hdferror)
  end function hdf_open_group

  
  function hdf_close_group(grp_id) result(hdferror)
    integer(I32), intent(in) :: grp_id   !< id for the group
    integer :: hdferror

    call h5gclose_f(grp_id, hdferror)
  end function hdf_close_group


  function hdf_get_rank(loc_id, dset_name, rank) result(hdferror)
    integer(I32), intent(in) :: loc_id        !< location id
    character(len=*), intent(in) :: dset_name   !< dataset name
    integer, intent(out) :: rank                !< rank of the dataset
    integer(I32) :: dset_id, dspace_id
    integer :: hdferror

    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    call h5dget_space_f(dset_id, dspace_id, hdferror)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferror)

    call h5sclose_f(dspace_id, hdferror)  
    call h5dclose_f(dset_id, hdferror)
  end function hdf_get_rank


  function hdf_get_dims(loc_id, dset_name, dims) result(hdferror)
    integer(I32), intent(in) :: loc_id        !< location id
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(out) :: dims(:)             !< dimensions of the dataset
    integer(I32) :: dset_id, dspace_id
    integer :: rank
    integer(HSIZE_T) :: dset_dims(6), max_dims(6)
    integer :: hdferror

    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)
    call h5dget_space_f(dset_id, dspace_id, hdferror)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferror)
    call h5sget_simple_extent_dims_f(dspace_id, dset_dims(1:rank), max_dims(1:rank), hdferror)
    dims(1:rank) = int(dset_dims(1:rank))

    call h5sclose_f(dspace_id, hdferror)  
    call h5dclose_f(dset_id, hdferror)
  end function hdf_get_dims

    integer(I32) function Get_Obj_Id(file_id, d_name, d_idx, gr_id, stat)
      ! Returns an object identifier.
      ! If dataset name "d_name" or index "d_idx" is given, a
      ! dataset is returned from the  file and group with the
      ! respective identifiers "file_id" and "gr_id".
      ! If no input arguments for the dataset name and index are given,
      ! the group identifier is returned.
      ! If no group id is given, the root group ("/") is returned.
      integer , intent (in):: file_id
      character (len=*) ,optional, intent (in):: d_name
      character (len=LEN_STR_ATTR) :: buff_name
      integer(kind=I32) , optional, intent (in) :: d_idx
      integer, optional, intent (out) :: stat
      integer :: hdferr
      integer(kind=I32) :: obj_type 
      integer(I32), optional ,intent(in) :: gr_id
      integer(I32) :: buff_gr_id

      if(present(gr_id)) then
        buff_gr_id =  gr_id
      else
        ! Open root group:
        call H5gopen_f(file_id, "/", buff_gr_id, hdferr)
      end if

      if (.not. present(d_name)) then
        if (present(d_idx)) then
          call H5gget_obj_info_idx_f(file_id, "/" , d_idx, buff_name, obj_type, hdferr)
          call H5dopen_f(buff_gr_id, trim(buff_name), get_obj_id, hdferr)
        else
          get_obj_id = buff_gr_id
        end if
      else
        call H5dopen_f(buff_gr_id, trim(d_name), get_obj_id, hdferr)
      end if
      if (present(stat)) stat = hdferr
    end function Get_Obj_Id

    function Create_Int16_Attr0(file_id, name, val, d_name, d_idx, gr_id) result(stat)
        integer(kind=I16) , intent (in):: val
        integer , intent (in):: file_id
        character (len=*) , optional, intent (in):: name
        integer(HID_T) :: attr_id, space_id
        character (len=*) ,optional, intent (in):: d_name
        integer(kind=I32) , optional, intent (in) :: d_idx
        integer(HID_T), optional ,intent(in) :: gr_id
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: obj_id
        integer(HID_T)  :: type_id ! DataType identifier
        integer(SIZE_T) , parameter :: HDFSIZE = 2 ! DataType Size in Bytes
        integer(HSIZE_T):: adims(1) !Attribut Dimension

        obj_id = get_obj_id(file_id, d_name, d_idx, gr_id, hdferr)
        call H5tcopy_f(H5T_NATIVE_INTEGER_2, type_id, hdferr)
        adims = [0]
        call H5screate_f(H5S_SCALAR_F, space_id, hdferr)
        call H5tset_size_f(type_id, HDFSIZE, hdferr)
        call H5acreate_f(obj_id, name, type_id, space_id, attr_id, hdferr)
        call H5awrite_f(attr_id, H5T_NATIVE_INTEGER_4, int(val,kind=I32), adims, stat)
        call H5aclose_f(attr_id,hdferr)
    end function Create_Int16_Attr0

    function Create_Int16_Attr1(file_id, name, val, d_name, d_idx, gr_id) result(stat)
        integer(kind=I16) , intent (in):: val(:)
        integer , intent (in):: file_id
        character (len=*) , optional, intent (in):: name
        integer(HID_T) :: attr_id, space_id
        character (len=*) ,optional, intent (in):: d_name
        integer(kind=I32) , optional, intent (in) :: d_idx
        integer(HID_T), optional ,intent(in) :: gr_id
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: obj_id
        integer(HID_T)  :: type_id ! DataType identifier
        integer(SIZE_T) , parameter :: HDFSIZE = 2 ! DataType Size in Bytes
        integer(HSIZE_T):: adims(1) !Attribut Dimension
        integer(kind=I32), parameter :: RANK1=1

        obj_id = get_obj_id(file_id, d_name, d_idx, gr_id, hdferr)
        call H5tcopy_f(H5T_NATIVE_INTEGER_2, type_id, hdferr)
        adims = shape(val, kind=HID_T)
        if( adims(1)==1 ) then
          call H5screate_f(H5S_SCALAR_F, space_id, hdferr)
        else
          call H5screate_simple_f(RANK1, adims, space_id, hdferr)
        end if
        call H5tset_size_f(type_id, HDFSIZE, hdferr)
        call H5acreate_f(obj_id, name, type_id, space_id, attr_id, hdferr)
        call H5awrite_f(attr_id, H5T_NATIVE_INTEGER_4, int(val,kind=I32), adims, stat)
        call H5aclose_f(attr_id,hdferr)
    end function Create_Int16_Attr1

    function Create_Int32_Attr0(file_id, name, val, d_name, d_idx, gr_id) result(stat)
        integer(kind=I32) , intent (in):: val(:)
        integer , intent (in):: file_id
        character (len=*) , optional, intent (in):: name
        integer(HID_T) :: attr_id, space_id
        character (len=*) ,optional, intent (in):: d_name
        integer(kind=I32) , optional, intent (in) :: d_idx
        integer(HID_T), optional ,intent(in) :: gr_id
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: obj_id
        integer(HID_T)  :: type_id ! DataType identifier
        integer(SIZE_T) , parameter :: HDFSIZE = 4 ! DataType Size in Bytes
        integer(HSIZE_T):: adims(1) !Attribut Dimension

        obj_id = get_obj_id(file_id, d_name, d_idx, gr_id, hdferr)
        call H5tcopy_f(H5T_NATIVE_INTEGER_4, type_id, hdferr)
        adims = [0]
        call H5screate_f(H5S_SCALAR_F, space_id, hdferr)
        call H5tset_size_f(type_id, HDFSIZE, hdferr)
        call H5acreate_f(obj_id, name, type_id, space_id, attr_id, hdferr)
        call H5awrite_f(attr_id, H5T_NATIVE_INTEGER_4, val, adims, stat)
        call H5aclose_f(attr_id,hdferr)
    end function Create_Int32_Attr0

    function Create_Int32_Attr1(file_id, name, val, d_name, d_idx, gr_id) result(stat)
        integer(kind=HID_T) , intent (in):: val(:)
        integer , intent (in):: file_id
        character (len=*) , optional, intent (in):: name
        integer(HID_T) :: attr_id, space_id
        character (len=*) ,optional, intent (in):: d_name
        integer(kind=I32) , optional, intent (in) :: d_idx
        integer(HID_T), optional ,intent(in) :: gr_id
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: obj_id
        integer(HID_T)  :: type_id ! DataType identifier
        integer(SIZE_T) , parameter :: HDFSIZE = 4 ! DataType Size in Bytes
        integer(HSIZE_T):: adims(1) !Attribut Dimension
        integer(kind=I32), parameter :: RANK1=1

        obj_id = get_obj_id(file_id, d_name, d_idx, gr_id, hdferr)
        call H5tcopy_f(H5T_NATIVE_INTEGER_4, type_id, hdferr)
        adims = shape(val, kind=HID_T)
        if( adims(1)==1 ) then
          call H5screate_f(H5S_SCALAR_F, space_id, hdferr)
        else
          call H5screate_simple_f(RANK1, adims, space_id, hdferr)
        end if
        call H5tset_size_f(type_id, HDFSIZE, hdferr)
        call H5acreate_f(obj_id, name, type_id, space_id, attr_id, hdferr)
        call H5awrite_f(attr_id, H5T_NATIVE_INTEGER_4, val, adims, stat)
        call H5aclose_f(attr_id,hdferr)
    end function Create_Int32_Attr1

    function Create_Char_Attr0(file_id, a_name, val, d_name, d_idx, gr_id) result(stat)
        character(len=*), intent (in):: val
        integer , intent (in):: file_id
        character (len=*) , optional, intent (in):: a_name
        integer(HID_T) :: attr_id, space_id
        character (len=*) ,optional, intent (in):: d_name
        integer(kind=I32) , optional, intent (in) :: d_idx
        integer(HID_T), optional ,intent(in) :: gr_id
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: obj_id
        integer(HID_T)  :: type_id ! DataType identifier
        integer(SIZE_T) :: HDFSIZE 
        integer(HSIZE_T):: adims(1) !Attribut Dimension

        obj_id = get_obj_id(file_id, d_name, d_idx, gr_id, hdferr)
        call H5tcopy_f(H5T_NATIVE_CHARACTER, type_id, hdferr)
        adims = [0]
        call H5screate_f(H5S_SCALAR_F, space_id, hdferr)
        HDFSIZE=len(val)
        call H5tset_size_f(type_id, HDFSIZE, hdferr)
        call H5acreate_f(obj_id, a_name, type_id, space_id, attr_id, hdferr)
        call H5awrite_f(attr_id, type_id, trim(val), adims, stat)
        call H5aclose_f(attr_id,hdferr)
    end function Create_Char_Attr0

    function Create_Char_Attr1(file_id, a_name, val, d_name, d_idx, gr_id) result(stat)
        character(len=*), intent (in):: val(:)
        integer , intent (in):: file_id
        character (len=*) , optional, intent (in):: a_name
        integer(HID_T) :: attr_id, space_id
        character (len=*) ,optional, intent (in):: d_name
        integer(kind=I32) , optional, intent (in) :: d_idx
        integer(HID_T), optional ,intent(in) :: gr_id
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: obj_id
        integer(HID_T)  :: type_id ! DataType identifier
        integer(HSIZE_T):: adims(1) !Attribut Dimension
        integer(kind=SIZE_T) :: HDFSIZE
        integer(kind=I32), parameter :: RANK1=1

        obj_id = get_obj_id(file_id, d_name, d_idx, gr_id, hdferr)
        call H5tcopy_f(H5T_NATIVE_CHARACTER, type_id, hdferr)
        adims = shape(val, kind=HID_T)
        if( adims(1)==1 ) then
          call H5screate_f(H5S_SCALAR_F, space_id, hdferr)
        else
          call H5screate_simple_f(RANK1, adims, space_id, hdferr)
        end if
        HDFSIZE=len(val)
        call H5tset_size_f(type_id, HDFSIZE, hdferr)
        call H5acreate_f(obj_id, a_name, type_id, space_id, attr_id, hdferr)
        call H5awrite_f(attr_id, type_id, val, adims, stat)
        call H5aclose_f(attr_id,hdferr)
    end function Create_Char_Attr1

    function Create_Real32_Attr0(file_id, a_name, val, d_name, d_idx, gr_id) result(stat)
        ! Returns the exit status of the HDF5 API when writing an attribute.
        ! If dataset name "d_name" or index "d_idx" is given, a
        ! dataset attribute is set from the dataset in file
        ! and group with the respective identifiers "file_id" and "gr_id".
        ! If no input arguments for the dataset name and index are given,
        ! the attribute will be set at the given group in the file.
        ! If no group id is given, the dataset will be get from
        ! the root group ("/").
        real(kind=SP) , intent (in):: val
        integer , intent (in):: file_id
        character (len=*) , optional, intent (in):: a_name
        integer(HID_T) :: attr_id, space_id
        character (len=*) ,optional, intent (in):: d_name
        integer(kind=I32) , optional, intent (in) :: d_idx
        integer(HID_T), optional ,intent(in) :: gr_id
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: obj_id
        integer(HID_T)  :: type_id ! DataType identifier
        integer(SIZE_T) , parameter :: HDFSIZE = 4 ! DataType Size in Bytes
        integer(HSIZE_T):: adims(1) !Attribut Dimension

        obj_id = get_obj_id(file_id, d_name, d_idx, gr_id, hdferr)
        call H5tcopy_f(H5T_NATIVE_REAL_4, type_id, hdferr)
        adims = [0]
        call H5screate_f(H5S_SCALAR_F, space_id, hdferr)
        call H5tset_size_f(type_id, HDFSIZE, hdferr)
        call H5acreate_f(obj_id, a_name, type_id, space_id, attr_id, hdferr)
        call H5awrite_f(attr_id, H5T_NATIVE_REAL_4, val, adims, stat)
        call H5aclose_f(attr_id,hdferr)
    end function Create_Real32_Attr0

    function Create_Real32_Attr1(file_id, a_name, val, d_name, d_idx, gr_id) result(stat)
        ! Returns the exit status of the HDF5 API when writing an attribute.
        ! If dataset name "d_name" or index "d_idx" is given, a
        ! dataset attribute is set from the dataset in file
        ! and group with the respective identifiers "file_id" and "gr_id".
        ! If no input arguments for the dataset name and index are given,
        ! the attribute will be set at the given group in the file.
        ! If no group id is given, the dataset will be get from
        ! the root group ("/").
        real(kind=SP) , intent (in):: val(:)
        integer , intent (in):: file_id
        character (len=*) , optional, intent (in):: a_name
        integer(HID_T) :: attr_id, space_id
        character (len=*) ,optional, intent (in):: d_name
        integer(kind=I32) , optional, intent (in) :: d_idx
        integer(HID_T), optional ,intent(in) :: gr_id
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: obj_id
        integer(HID_T)  :: type_id ! DataType identifier
        integer(SIZE_T) , parameter :: HDFSIZE = 4 ! DataType Size in Bytes
        integer(HSIZE_T):: adims(1) !Attribut Dimension
        integer(kind=I32), parameter :: RANK1=1

        obj_id = get_obj_id(file_id, d_name, d_idx, gr_id, hdferr)
        call H5tcopy_f(H5T_NATIVE_REAL_4, type_id, hdferr)
        adims = shape(val, kind=HID_T)
        if( adims(1)==1 ) then
          call H5screate_f(H5S_SCALAR_F, space_id, hdferr)
        else
          call H5screate_simple_f(RANK1, adims, space_id, hdferr)
        end if
        call H5tset_size_f(type_id, HDFSIZE, hdferr)
        call H5acreate_f(obj_id, a_name, type_id, space_id, attr_id, hdferr)
        call H5awrite_f(attr_id, H5T_NATIVE_REAL_4, val, adims, stat)
        call H5aclose_f(attr_id,hdferr)
    end function Create_Real32_Attr1

    function Create_Real64_Attr0(file_id, name, val, d_name, d_idx, gr_id) result(stat)
        ! Returns the exit status of the HDF5 API when writing an attribute.
        ! If dataset name "d_name" or index "d_idx" is given, a
        ! dataset attribute is set from the dataset in file
        ! and group with the respective identifiers "file_id" and "gr_id".
        ! If no input arguments for the dataset name and index are given,
        ! the attribute will be set at the given group in the file.
        ! If no group id is given, the dataset will be get from
        ! the root group ("/").
        real(kind=DP), intent (in):: val(:)
        integer , intent (in):: file_id
        character (len=*) , optional, intent (in):: name
        integer(HID_T) :: attr_id, space_id
        character (len=*) ,optional, intent (in):: d_name
        integer(kind=I32) , optional, intent (in) :: d_idx
        integer(HID_T), optional ,intent(in) :: gr_id
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: obj_id
        integer(HID_T)  :: type_id ! DataType identifier
        integer(SIZE_T) , parameter :: HDFSIZE = 8 ! DataType Size in Bytes
        integer(HSIZE_T):: adims(1) !Attribut Dimension

        obj_id = get_obj_id(file_id, d_name, d_idx, gr_id, hdferr)
        call H5tcopy_f(H5T_NATIVE_REAL_8, type_id, hdferr)
        adims = [0]
        call H5screate_f(H5S_SCALAR_F, space_id, hdferr)
        call H5tset_size_f(type_id, HDFSIZE, hdferr)
        call H5acreate_f(obj_id, name, type_id, space_id, attr_id, hdferr)
        call H5awrite_f(attr_id, type_id, val(:), adims, stat)
        call H5aclose_f(attr_id,hdferr)
    end function Create_Real64_Attr0

    function Create_Real64_Attr1(file_id, name, val, d_name, d_idx, gr_id) result(stat)
        ! Returns the exit status of the HDF5 API when writing an attribute.
        ! If dataset name "d_name" or index "d_idx" is given, a
        ! dataset attribute is set from the dataset in file
        ! and group with the respective identifiers "file_id" and "gr_id".
        ! If no input arguments for the dataset name and index are given,
        ! the attribute will be set at the given group in the file.
        ! If no group id is given, the dataset will be get from
        ! the root group ("/").
        real(kind=DP), intent (in):: val(:)
        integer , intent (in):: file_id
        character (len=*) , optional, intent (in):: name
        integer(HID_T) :: attr_id, space_id
        character (len=*) ,optional, intent (in):: d_name
        integer(kind=I32) , optional, intent (in) :: d_idx
        integer(HID_T), optional ,intent(in) :: gr_id
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: obj_id
        integer(HID_T)  :: type_id ! DataType identifier
        integer(SIZE_T) , parameter :: HDFSIZE = 8 ! DataType Size in Bytes
        integer(HSIZE_T):: adims(1) !Attribut Dimension
        integer(kind=I32), parameter :: RANK1=1

        obj_id = get_obj_id(file_id, d_name, d_idx, gr_id, hdferr)
        call H5tcopy_f(H5T_NATIVE_REAL_8, type_id, hdferr)
        adims = shape(val, kind=HID_T)
        if( adims(1)==1 ) then
          call H5screate_f(H5S_SCALAR_F, space_id, hdferr)
        else
          call H5screate_simple_f(RANK1, adims, space_id, hdferr)
        end if
        call H5tset_size_f(type_id, HDFSIZE, hdferr)
        call H5acreate_f(obj_id, name, type_id, space_id, attr_id, hdferr)
        call H5awrite_f(attr_id, type_id, val(:), adims, stat)
        call H5aclose_f(attr_id,hdferr)
    end function Create_Real64_Attr1

    function Create_Empty_Dataset(obj_id, d_name) result(stat)
        ! Creates an empty dataset with the only purpose of an attributes
        ! storage. (Here used to hold the spatial reference system
        ! attributes, 'grid_maping'.)
        integer , intent (in):: obj_id
        integer(HID_T) :: dataset_id, space_id
        character (len=*) , intent (in):: d_name
        integer :: stat
        integer :: hdferr
        integer(HID_T)  :: type_id ! DataType identifier
        integer(HSIZE_T):: adims(1) ! Attribut Dimension
        adims = [0]
        call H5tcopy_f(H5T_NATIVE_CHARACTER, type_id, hdferr)
        call H5screate_simple_f(1, adims, space_id, hdferr)
        call H5dcreate_f(obj_id, d_name, type_id, space_id, dataset_id, stat)
        call H5dclose_f(dataset_id,hdferr)
        call H5tclose_f(type_id, hdferr)
        call H5sclose_f(space_id, hdferr)
    end function Create_Empty_Dataset

    function Read_Int_Slab(file_id,dset_name,rank,offset,dshape,int_array) result(ierr)
        ! Reads a section of a HDF5 dataset
        ! 
        implicit none
        integer, intent(in) :: file_id
        character(len=255), intent(in) :: dset_name
        integer, intent(in) :: rank
        integer(kind=I64), intent(in) :: offset(rank), dshape(rank)
        integer(kind=I64) :: offset_out(rank), dims(rank)
        integer :: ierr
        integer :: dset_id, dataspace, memspace
        integer(kind=I32), allocatable :: int_array(:,:)

        offset_out = [0,0]
        dims=dshape

        call H5dopen_f(file_id, dset_name, dset_id, ierr)
        call H5dget_space_f(dset_id, dataspace, ierr)
        call H5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, offset, dshape, ierr)

        call H5screate_simple_f(rank, dims, memspace, ierr)

        call H5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, offset_out, dshape, ierr)

        if (.not. allocated(int_array)) allocate(int_array(dims(1),dims(2)))

        call H5dread_f(dset_id, H5T_NATIVE_INTEGER, int_array, dims, ierr, memspace, dataspace)

        call H5sclose_f(dataspace, ierr)
        call H5sclose_f(memspace, ierr)
        call H5dclose_f(dset_id, ierr)
    end function Read_Int_Slab

    function Read_Real_Slab(file_id,dset_name,rank,offset,dshape,int_array) result(ierr)
        ! Reads a section of a HDF5 dataset
        ! 
        implicit none
        integer, intent(in) :: file_id
        character(len=255), intent(in) :: dset_name
        integer, intent(in) :: rank
        integer(kind=I64), intent(in) :: offset(rank), dshape(rank)
        integer(kind=I64) :: offset_out(rank), dims(rank)
        integer :: ierr
        integer :: dset_id, dataspace, memspace
        real(kind=SP), allocatable :: int_array(:,:)

        offset_out = [0,0]
        dims=dshape

        call H5dopen_f(file_id, dset_name, dset_id, ierr)
        call H5dget_space_f(dset_id, dataspace, ierr)
        call H5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, offset, dshape, ierr)

        call H5screate_simple_f(rank, dims, memspace, ierr)

        call H5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, offset_out, dshape, ierr)

        if (.not. allocated(int_array)) allocate(int_array(dims(1),dims(2)))

        call H5dread_f(dset_id, H5T_NATIVE_INTEGER, int_array, dims, ierr, memspace, dataspace)

        call H5sclose_f(dataspace, ierr)
        call H5sclose_f(memspace, ierr)
        call H5dclose_f(dset_id, ierr)
    end function Read_Real_Slab


end module H5_Func_mod
