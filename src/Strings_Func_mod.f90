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

module Strings_Func_mod
    use Types_mod
    implicit none

    contains
!##################################################################################################################################!
!------------------------------------------------------------------------
! ...Functions to change case of letters...
!------------------------------------------------------------------------
    elemental function To_upper(str)
      character(*), intent(in) :: str
      character(len=len(str))  :: To_upper
      integer :: i

      To_upper=str
      do i = 1, len(str)
        select case(str(i:i))
        case("a":"z")
          To_upper(i:i) = achar(iachar(str(i:i))-32)
        end select
      end do
    end function To_upper
!------------------------------------------------------------------------
    elemental function To_lower(str)
      character(*), intent(in) :: str
      character(len=len(str))  :: To_lower
      integer :: i

      To_lower=str
      do i = 1, len(str)
        select case(str(i:i))
        case("A":"Z")
          To_lower(i:i) = achar(iachar(str(i:i))+32)
        end select
      end do
    end function To_lower
!##################################################################################################################################!
!------------------------------------------------------------------------
! ...function N_Lines: Count number of lines of input file...
!------------------------------------------------------------------------
    integer(kind=I32) function N_Lines(filein)
      character(len=*), intent(in) :: filein
      integer(kind=I8)  :: io, nunit

      open (newunit=nunit,file=trim(filein),iostat=io)
      N_Lines=-1

      do while (io==0)
        N_Lines = N_Lines + 1
        read(nunit,*,iostat=io)
      end do

      close(nunit)
    end function N_Lines
!##################################################################################################################################!
!------------------------------------------------------------------------
! ...subroutine reportLog: Print a message to display...
!------------------------------------------------------------------------
    subroutine ReportLog(logmessage)
      character(len=*), intent(in)  :: logmessage
      write(*,*)trim(logmessage)
    end subroutine ReportLog
!##################################################################################################################################!
!------------------------------------------------------------------------
! ...subroutine Decomp_Names: decompose the filenamein array in fields...
!------------------------------------------------------------------------
    pure subroutine Decomp_Names(filenamein,field_names)
      character(len=*), intent(out)  :: field_names(:)    !string containing the names of the filenamein
      character(len=*),  intent(in)  :: filenamein        !input string to be decomposed
      integer(kind=I16)              :: reset             !position of first slash (/)
      integer(kind=I16), allocatable :: position_under(:) !positions of the found underscores
      integer(kind=I16)              :: name_len, name_spaces
      integer(kind=I16)              :: i
      !-----------------------------------------------------------------------------------------------

      name_spaces=size(field_names)
      allocate(position_under(name_spaces))
      reset=scan(trim(filenamein), '/',back=.true.)      !To find the last slash
      name_len=len(trim(filenamein))

      position_under(1) = scan(trim(filenamein(reset+1:name_len)), '_')
      position_under(1) = reset + position_under(1)
      if( name_spaces > 2 ) then
        do i=2,name_spaces-1
          position_under(i) = scan(trim(filenamein(sum(position_under(1:i-1))+1:name_len)), '_')
        end do
      end if
      position_under(name_spaces) = scan(trim(filenamein(sum(position_under(1:name_spaces-1))+1:name_len)), '.')
      if (position_under(name_spaces) == 0) then
          position_under(name_spaces) = len_trim(filenamein) - position_under(name_spaces-1)
      end if

      field_names(1)=filenamein(reset+1:position_under(1)-1)
      if( name_spaces > 1 ) then
        do i=2,name_spaces
          field_names(i)=filenamein(sum(position_under(1:i-1))+1:sum(position_under(1:i))-1)
        end do
      end if

      deallocate(position_under)
    end subroutine Decomp_Names
!##################################################################################################################################!
end module Strings_Func_mod
