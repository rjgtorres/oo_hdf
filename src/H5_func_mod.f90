module H5_Func_mod
  use hdf5
  use Types_mod
  use Strings_Func_mod, only: To_upper
  implicit none
  integer, private, parameter :: LEN_STR_ATTR  = 80

  contains


  function hdf_open_file(file_id, filename, state, mode)
    integer(I32), intent(out) :: file_id            !< HDF5 id of the file
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
    integer(I32), intent(out) :: grp_id
    integer(I32) :: hdferror

    call h5gcreate_f(loc_id, group_name, grp_id, hdferror)
    call h5gclose_f(grp_id, hdferror)
  end function hdf_create_group
!TODO
  
  !>  \brief Opens a group and returns the identifier
  subroutine hdf_open_group(loc_id, group_name, group_id)
    
    integer(HID_T), intent(in) :: loc_id         !< location id where to put the group
    character(len=*), intent(in) :: group_name   !< name of the group
    integer(HID_T), intent(out) :: group_id      !< id for the group
    
    integer :: hdferror

    write(*,'(A)') "->hdf_open_group"

    call h5gopen_f(loc_id, group_name, group_id, hdferror)
    !write(*,'(A20,I0)') "h5gcreate: ", hdferror
    
  end subroutine hdf_open_group

  
  !>  \brief Close a group by identifier
  subroutine hdf_close_group(group_id)

    integer(HID_T), intent(in) :: group_id   !< id for the group
    
    integer :: hdferror

    write(*,'(A)') "->hdf_close_group"

    call h5gclose_f(group_id, hdferror)
    !write(*,'(A20,I0)') "h5gclose: ", hdferror
    
  end subroutine hdf_close_group


  !>  \brief Get the rank of a dataset
  subroutine hdf_get_rank(loc_id, dset_name, rank)

    integer(HID_T), intent(in) :: loc_id        !< location id
    character(len=*), intent(in) :: dset_name   !< dataset name
    integer, intent(out) :: rank                !< rank of the dataset

    integer(HID_T) :: dset_id, dspace_id
    integer :: hdferror

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)

    ! get dataspace
    call h5dget_space_f(dset_id, dspace_id, hdferror)

    ! get rank (ndims)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferror)

    ! close id's
    call h5sclose_f(dspace_id, hdferror)  
    call h5dclose_f(dset_id, hdferror)
    
  end subroutine hdf_get_rank


  !>  \brief get the dimensions of a dataset
  subroutine hdf_get_dims(loc_id, dset_name, dims)

    integer(HID_T), intent(in) :: loc_id        !< location id
    character(len=*), intent(in) :: dset_name   !< name of dataset
    integer, intent(out) :: dims(:)             !< dimensions of the dataset

    integer(HID_T) :: dset_id, dspace_id
    integer :: rank
    integer(HSIZE_T) :: dset_dims(6), max_dims(6)
    integer :: hdferror

    ! open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, hdferror)

    ! get dataspace
    call h5dget_space_f(dset_id, dspace_id, hdferror)

    ! get rank (ndims)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferror)
    
    ! get rank (ndims)
    call h5sget_simple_extent_dims_f(dspace_id, dset_dims(1:rank), max_dims(1:rank), hdferror)
    dims(1:rank) = int(dset_dims(1:rank))

    ! close id's
    call h5sclose_f(dspace_id, hdferror)  
    call h5dclose_f(dset_id, hdferror)

  end subroutine hdf_get_dims



















end module H5_Func_mod
