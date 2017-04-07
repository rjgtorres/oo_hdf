# oo_hdf
Object oriented fortran HDF5 module

This library aims to create a group of classes in fortran to interact with [HDF5](https://support.hdfgroup.org/HDF5/).

### Contents:
This library as two main files, one with high level hdf5 routines **(H5_func_mod.f90)**, the other with classes wrapping those routines **(H5_OO_mod.f90)**.

The function module was inspired, extends and has several of the functions present in [HDF5_utils](https://github.com/tiasus/HDF5_utils).

The classes module has the same base structure of [mo_netcdf](https://github.com/rjgtorres/mo_netcdf).

There are two main classes, *H5Group* and *H5Dataset*. The *H5File* is extended from the *H5Group*. All classes can read and write attributes from the *H5Attributable* class.

### Tested

This library was tested with versions:
 - **HDF5**: 1.8.11, 1.8.16
 - **gfortran**: 4.8.\*, 5.4.\* 

The following methods have been tested in several ways, most of them in production code:
 - All H5File
 - All H5Group
 - All H5Attributable
 - All H5Dataset, but:
     - getDataset only integer 8, 16, 32 and real 32 up to 3 dimensions (:,:,:)
     - setDataset with more than 4 dimensions is not tested
     - getBlock with more than 2 dimensions is not tested
     - extendDataset was only tested with int16 array of 3 and 4 dimensions

### TODO

 - [ ] One example of the definition of a netCDF file with this library.
 - [x] A way to write blocks of arrays.
 - [x] A way to define more than one dimension to be extensible.
 - [ ] Implement more functions of the HDF5 library.

### Interface
 #### H5File
  ```fortran 
  type(H5File) :: file
  
  file = H5File(' path of the file ', status, mode)
!status can be: (OLD, O, NEW, N, REPLACE, RP), not case sensitive
!mode can be: (READ, R, WRITE, W, READWRITE, RW), not case sensitive

``` 

  ```fortran 
  call file%closeFile()
  ``` 
 #### H5Group
  ```fortran 
  type(H5Group) :: newgroup
  type(H5Group) :: oldgroup
  ```
  This class must always be called from an object of the same class (H5Group or H5File)
  ```fortran
  call file%setGroup('name of the new group', newgroup)
  call newgroup%openGroup('name of an existing group', oldgroup)
  call oldgroup%closeGroup()
  ```
 #### H5Dataset
  ```fortran 
  type(H5Dataset) :: newdataset
  newdataset = H5Dataset('Name of the dataset', parent group)
  ```
Define the chunk size of the dataset:
  ```fortran 
  call newdataset%setChunkSize(chunksize)
  ! chunksize is an integer, if not defined is by default 100
  ```

Define the compression level of the dataset:
  ```fortran 
  call newdataset%setCompressionLevel(CompressionLevel)
  ! CompressionLevel is an integer that must vary between 0 and 9, if not defined is by default 9
  ```
Define the fill value of the dataset:
  ```fortran 
  call newdataset%setFillValue(fillvalue)
  ! fillvalue is an integer, if not defined is by default 0
  ```

Define the dataset is extendable in n  dimentions:
  ```fortran 
  call newdataset%setExtendable(n)
  ! n is an integer less or equal to the dataset dimensions
  ! the dataset gets extended from the last rank to the start
  ! if newdataset has 4 dimensions (:,:,:,:) and we call newdataset%setExtendable(2),
  ! the last two dimensions become extendable
  ```
Define an empty dataset:
  ```fortran 
  call newdataset%setEmpty()
  ```
Get the dataset rank:
  ```fortran 
  call newdataset%getRank(rank)
  !rank is an integer
  ```

Get the dataset dimensions:
  ```fortran 
  call newdataset%getDims(dims)
  !Dims must be an integer array with the dataset rank
  ```

Define dimension (scale) of an array
  ```fortran 
  type(H5Dataset) :: dim_dset
  call dim_dset%setDataset(x)
  call dim_dset%defScale(dimension_name)
  !dimension_name the name you wish to attribute to the scale, it is optional
  ```

Link a dimension (scale) to an array
  ```fortran 
  call newdataset%setScale(dim_dset,idx_dim)
  !dim_dset is the scale object you wish to link with your dataset
  !idx_dim is an integer with the rank of the dataset where you will link the scale
  ```

Read the entire dataset:
  ```fortran 
  call newdataset%getDataset(dset_array)
  !dset_array can be an array of 1 to 6 dimensions
  !dset_array can be an integer of 32 bits or a real of 64 bits
  !the dataset inside the file can be of any kind lower or equal of those.
  ```

Read a block of a dataset:
  ```fortran 
  call newdataset%getBlock(offset, shape, d_array)
  !offset is a one dimension integer of 64 bits array with the size of the rank of the dataset to read,
  !this array must have the starting points of where you will read the array
  !
  !shape is a one dimension integer of 64 bits array with the size of the rank of the dataset to read,
  !this array must have the size of the portion the array you want to read
  !
  !dset_array can be an array of 1 to 5 dimensions
  !dset_array can be an integer or a real of 32 bits
  ```

Write a Dataset:
  ```fortran 
  call newdataset%setDataset(dset_array)
  !dset_array can be an array of 1 to 6 dimensions
  !dset_array can be an integer of 8, 16 or 32 bits
  !dset_array can be a real of 32 or 64 bits
  ```

Extend a Dataset:
  ```fortran 
  call newdataset%extendDataset(new_size, offset, dshape, val)
  !dset_array can be an array of 1 to 6 dimensions
  !dset_array can be an integer of 8, 16 or 32 bits
  !dset_array can be a real of 32 or 64 bits
  !
  !new_size is a one dimension integer of 64 bits array with the size of the rank of the dataset to write,
!this array must have the new total shape of the array you want to extend
  !
  !offset is a one dimension integer of 64 bits array with the size of the rank of the dataset to write,
!this array must have the starting points of where you will start writing the array
!
!dshape is a one dimension integer of 64 bits array with the size of the rank of the dataset to extend,
!this array must have the size of the portion the array you want to write
!
  
  ```

#### H5Attributable
This class is only accessed from an object of class H5Group or H5Dataset.

Verify if an attribute exists:
  ```fortran 
  if ( newdataset%Attr_exists(a_name) ) then
        print*,a_name//' exists'
  end if
  !Attr_exists is a logical function
  !a_name is the name of the attribute to check
  ```

Read an attribute:
  ```fortran 
  call newdataset%getAttribute(a_name, val)
  call file%getAttribute(a_name, val)
  call newgroup%getAttribute(a_name, val)
  !a_name is the name of the attribute to read
  !val can be of type character, integer or real(32 or 64 bits)
  !val can be a scalar or an array of one dimension.
  ```
Write an attribute:
  ```fortran 
  call newdataset%setAttribute(a_name, val)
  call file%setAttribute(a_name, val)
  call newgroup%setAttribute(a_name, val)
  !a_name is the name of the attribute to read
  !val can be of type character, integer or real(32 or 64 bits)
  !val can be a scalar or an array of one dimension.
  ```

### Examples:
 - Write file, dataset and attribute:
 
 ```fortran

  type(H5File) :: f1
  type(H5Dataset) :: d1, d2
  type(H5Group) :: g1
 
  !Open a new, write only, file
  f1=H5File("new_test_file.h5", "N", "W")
  !write an attribute in /
  call f1%setAttribute('root_atribute',42)
  !initialize a dataset in /
  d1=H5Dataset('2d_i32',f1)
  !set the fillvalue of d1 dataset to -1
  call d1%setFillValue(-1)
  !define a new group in /
  call f1%setGroup('newgroup',g1)
  !initialize a dataset in group "newgroup"
  d2=H5Dataset('4d_r64',g1)

  do i=1,size(test_arr(:,1))
    do j=1,size(test_arr(1,:))
      test_arr(i,j)=i*j
    end do
  end do
  !write the contents of test_arr to d1 dataset in 8 bits precision
  call d1%setDataset(int(test_arr,1))
  !write an attribute in d1 dataset
  call d1%setAttribute('dataset_atribute',int(42,2))
  !write one array in double precision to d2 dataset
  call d2%setDataset(real([[1],[2],[4],[5]],DP))
  !write an attribute in group g1
  call g2%setAttribute('att1', int([4,4,2,5,4,6,3,1,1,5,5,2,4,2154545,6,2,4,7,6],4))
  !close hdf5 file
  call f1%closeFile()
 ```
 
  - Read the contents of a HDF5 file:
  
  ```fortran
  !Open an existing, read only, file
  f1=H5File("new_test_file.h5", "O", "R")
  !read and attribute in /
  call f1%getAttribute('thgttg',i)
  !open on group in /
  call f1%openGroup('newgroup',g1)
  !initialize a dataset in /
  d1=H5Dataset('2d_i32',f1)
  !read an attribute in d1 dataset
  call d1%getAttribute('att1',read_a)
  !initialize a dataset in group "newgroup"
  d2=H5Dataset('4d_r64',g1)
  !read the contents of d2 dataset
  call d2%getDataset(r8_dset)
  !close hdf5 file
  call f1%closeFile()
  
  ```
