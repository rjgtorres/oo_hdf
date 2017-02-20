module Types_mod
use, intrinsic :: iso_fortran_env
implicit none
public :: SP, DP, QP, I8, I16, I32, I64
integer, parameter :: SP  = REAL32   ! Single precision
integer, parameter :: DP  = REAL64   ! Double precision
integer, parameter :: QP  = REAL128  ! Quadruple precision

integer, parameter :: I8  = INT8
integer, parameter :: I16 = INT16
integer, parameter :: I32 = INT32
integer, parameter :: I64 = INT64

end module Types_mod
