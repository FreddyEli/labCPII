program input
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none
        type voltage_source
                integer:: start
                integer:: end
                CHARACTER:: type
                REAL(dp):: amplitude
                REAL(dp):: freq
        end type voltage_source
        type current_source
                integer:: start
                integer:: end
                CHARACTER:: type
                REAL(dp):: amplitude
                REAL(dp):: freq
        end type current_source
        type resistor
                integer:: start
                integer:: end
                REAL(dp):: res
        end type resistor
        type inductor
                integer:: start
                integer:: end
                REAL(dp):: inductance
        end type inductor
        type capacitor
                integer:: start
                integer:: end
                real(dp):: capacitance
        end type capacitor

  integer:: Components, j, nr, nl, nc, nv, ni, err, istat
  integer:: Nodes
  type(voltage_source), allocatable:: v(:)
  type(current_source), allocatable:: i(:)
  type(resistor),  allocatable:: r(:)
  type(inductor),  allocatable:: l(:)
  type(capacitor), allocatable:: c(:)

open(100, file="main.log")
nc=0; nl=0; nr=0; nv=0; ni=0
Components=0
call SYSTEM('cd shell && ./sort.input && cd ..')

!Read number of components
open(1, file="./input/wc.input", status="old");read (1,*)  Components; close(1)
open(2, file="./input/wc.r", status="old"); read(2,*)  nr; close(2)
open(3, file="./input/wc.l", status="old"); read(3,*)  nl; close(3);
open(4, file="./input/wc.c", status="old"); read(4,*)  nc; close(4);
open(5, file="./input/wc.v", status="old"); read(5,*)  nv; close(5);
open(6, file="./input/wc.i", status="old"); read(6,*)  ni; close(6)

!Populate vectors
if ( nr /=0 ) then
    open(1, file="./input/components.r", status="old")
    allocate(r(nr), stat=err)
    if (err /= 0) print *, "array: Allocation request denied"
    do j = 1, nr
            read(1, *, iostat=istat) r(j)%start, r(j)%end, r(j)%res
            if (istat /= 0) stop "Read error in resitor unit"
    end do
    close(1)
end if
if (nl/=0) then
   open(2, file="./input/components.l", status="old")
    allocate(l(nl), stat=err)
    if (err /= 0) print *, "array: Allocation request denied"
    do j = 1, nl
            read(2, *, iostat=istat) l(j)%start, l(j)%end, l(j)%inductance
            if (istat /= 0) stop "Read error in inductor unit"
    end do
    close(2)
end if
if (nc/=0) then
   open(3, file="./input/components.c", status="old")
    allocate(c(nc), stat=err)
    if (err /= 0) print *, "array: Allocation request denied"
    do j = 1, nc
            read(3, *, iostat=istat) c(j)%start, c(j)%end, c(j)%capacitance
            if (istat /= 0) stop "Read error in capacitor unit"
    end do
    close(3)
end if
if (nv/=0) then
    open(4, file="./input/components.v", status="old")
    allocate(v(nv), stat=err)
    if (err /= 0) print *, "array: Allocation request denied"
    do j = 1, nv
        read(4, *, iostat=istat) v(j)%start, v(j)%end, v(j)%type, v(j)%freq 
        if (istat /= 0) stop "Read error in voltage source unit"
    end do
end if
close(4)
if (ni/=0) then
   open(5, file="./input/components.i", status="old")
    allocate(i(ni), stat=err)
    if (err /= 0) print *, "array: Allocation request denied"
    do j = 1, ni
            read(5, *, iostat=istat) i(j)%start, i(j)%end, i(j)%type, i(j)%amplitude, i(j)%freq
            if (istat /= 0) stop "Read error in current source unit"
    end do
    close(5)
end if




close(100)
end program


