program test_h5_funcs
  use H5_Func_mod
  implicit none

  integer(kind=4) :: file_id, gr_id, file2_id
  integer(kind=4) :: error
  character(len=10) :: chararr(2)
  integer(kind=4) :: read_a(19)
  integer(kind=4) :: value
  integer(kind=4) :: test_arr(4,3)
  integer(kind=4) :: i, j
  
  file_id=hdf_open_file("new_test_file.h5", "N", "W")

  file2_id=hdf_open_file("new2_test_file.h5", "N", "W")

  gr_id=hdf_create_group(file_id,'grupo1')

  error=Create_Int16_Attr0(file_id, 'att0', int(45,2))
  error=Create_Int16_Attr0(gr_id, 'attgr0', int(45,2))
  error=Create_Int32_Attr1(file_id, 'att1', int([4,4,2,5,4,6,3,1,1,5,5,2,4,2154545,6,2,4,7,6],4))
  
  error=Create_Char_Attr0(file_id,a_name='attchar',val='asdfasdf')
  chararr=''
  chararr(1)='alfa'
  chararr(2)='betas'
  error=Create_Char_Attr1(file_id,a_name='attchar3',val=chararr)

  do i=1,size(test_arr(:,1))
    do j=1,size(test_arr(1,:))
      test_arr(i,j)=i*j
    end do
  end do
  
  error=Create_Dset(gr_id,"2d_i32",test_arr)
  error=Create_Dset(gr_id,"2d_i16",int(test_arr,I16))
  error=Create_Dset(gr_id,"2d_i8",int(test_arr,I8))
  error=Create_Dset(gr_id,"2d_r32",real(test_arr,SP))
  error=Create_Dset(gr_id,"2d_r64",real(test_arr,DP))

  error=Create_Dset(gr_id,"1d_i32",test_arr(1,:))
  error=Create_Dset(gr_id,"1d_i16",int(test_arr(1,:),I16), extendable=1)
  error=Create_Dset(gr_id,"1d_i8",int(test_arr(1,:),I8))
  error=Create_Dset(gr_id,"1d_r32",real(test_arr(1,:),SP))
  error=Create_Dset(gr_id,"1d_r64",real(test_arr(1,:),DP))

  error=hdf_close_group(gr_id)

  error=hdf_close_file(file_id)

  error=Create_Char_Attr1(file2_id,a_name='attchar3',val=chararr)
error=hdf_close_file(file2_id)
  file_id=hdf_open_file("new_test_file.h5", "o", "r")
  gr_id=hdf_open_group(file_id, 'grupo1')
  chararr='-'
  error=Read_Att(file_id, a_name='attchar3', val=chararr)!, gr_id=gr_id)
  error=Read_Att(file_id, a_name='att1', val=read_a(:))
  error=Read_Att(gr_id, a_name='attgr0', val=value)

call hdf_close(error)

  print*,chararr
  print*,read_a
  print*,shape(read_a),size(read_a), rank(read_a)
  print*,value

  
end program test_h5_funcs
