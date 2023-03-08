program input
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none
  integer:: numberOfComponents, i, nr, nl, nc
  character, allocatable:: letter(:)
  character (len=200) :: line
  integer :: dat1, RetCode
nc=0; nl=0; nr=0
open(1, file="components", status="old")
numberOfComponents=0
read_loop: do
read (1, "(A)", end=100)  line
line=adjustl(line)
   if ( index (line, "!") /= 0 )  cycle
numberOfComponents= numberOfComponents+1
end do read_loop
100 CONTINUE
close(1)
print*,  "Number of components", numberOfComponents

allocate(letter(numberOfComponents))

i=0
open(1, file="components", status="old")
do while (i<numberOfComponents)
read (1, "(A)")  line
line=adjustl(line)
   if ( index (line, "!") /= 0 ) cycle
        read (line, *, end=101)  letter(1)
select case (letter(1))
case ("R")
nr=nr+1
case ("L")
nl=nl+1
case ("C")
nc=nc+1
end select
i=i+1
end do

print*, "       R      C       L"
print*, nr,nc,nl
101 CONTINUE
close(1)




!if () print*, "no"
 !     print*, "read error"
 !     exit read_loop
 !   end if
!    read (line, *) dat1
!end do read_loop
!enddo

!numberOfComponents=0
!print*, numberOfComponents
end program


