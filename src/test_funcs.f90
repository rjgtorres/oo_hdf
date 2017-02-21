program test_h5_funcs
  use H5_Func_mod
  implicit none

  integer(kind=4) :: file_id, gr_id
  integer(kind=4) :: error
  character(len=10) :: chararr(2)
  
  file_id=hdf_open_file("new_test_file.h5", "N", "W")

  gr_id=hdf_create_group(file_id,'grupo1')
  print*,file_id,gr_id
  error=Create_Int16_Attr0(file_id, 'att0', int(45,2))
  print*,file_id,gr_id
  error=Create_Int16_Attr0(file_id, 'attgr0', int(45,2),gr_id=gr_id)
  print*,file_id,gr_id
  print*,error
  stop
  error=Create_Int32_Attr1(file_id, 'att1', int([4,4,2,5,4,6,3,1,1,5,5,2,4,2154545,6,2,4,7,6],4))
  
  error=Create_Char_Attr0(file_id,a_name='attchar',val='asdfasdf')
  chararr=''
  chararr(1)='alfa'
  chararr(2)='betas'
  error=Create_Char_Attr1(file_id,a_name='attchar3',val=chararr)
  
end program test_h5_funcs
