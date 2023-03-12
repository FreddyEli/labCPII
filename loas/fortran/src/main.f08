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
                real(dp):: initial_charge
        end type capacitor

  integer:: Components, nr, nl, nc, nv, ni, err, istat
  integer:: Nodes, loops
  integer:: j,k
  type(voltage_source), allocatable:: v(:)
  type(current_source), allocatable:: i(:)
  type(resistor),  allocatable:: r(:)
  type(inductor),  allocatable:: l(:)
  type(capacitor), allocatable:: c(:)
  real:: cput
  integer, parameter:: n=100
  real(dp), PARAMETER:: t0=0D0, tf=3D0, dt=(tf-t0)/n
  !real(dp), :: voltage(0:n), current(0:n), time(0:n)
  real(dp):: time(0:n)
  real(dp):: k1, k2, k3, k4
  real(dp), allocatable:: voltage(:,:), current(:,:), charge(:,:)

open(100, file="main.log")
    !input
nc=0; nl=0; nr=0; nv=0; ni=0
Components=0; Nodes=0; loops=Components-Nodes-1
!Find number of Nodes
call SYSTEM('cd shell && ./sort.input && cd ..')

!Read number of components
open(1, file="./input/wc.input", status="old");read (1,*)  Components; close(1)
open(2, file="./input/wc.r", status="old"); read(2,*)  nr; close(2)
open(3, file="./input/wc.l", status="old"); read(3,*)  nl; close(3);
open(4, file="./input/wc.c", status="old"); read(4,*)  nc; close(4);
open(5, file="./input/wc.v", status="old"); read(5,*)  nv; close(5);
open(6, file="./input/wc.i", status="old"); read(6,*)  ni; close(6)
open(7, file="./input/wc.nodes", status="old"); read(7,*)  Nodes; close(7)

!Populate component vectors
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
            read(3, *, iostat=istat) c(j)%start, c(j)%end, c(j)%capacitance, c(j)%initial_charge
            if (istat /= 0) stop "Read error in capacitor unit"
    end do
    close(3)
end if
if (nv/=0) then
    open(4, file="./input/components.v", status="old")
    allocate(v(nv), stat=err)
    if (err /= 0) print *, "array: Allocation request denied"
    do j = 1, nv
        read(4, *, iostat=istat) v(j)%start, v(j)%end, v(j)%type, v(j)%amplitude, v(j)%freq
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
call CPU_TIME(cput); WRITE(100,*) cput, "Read components succesfully"
!Peamble finished 

!Populte time
time(0)=t0
time(n)=tf
do j = 1, n-1
time(j)=t0+j*dt
end do
allocate(voltage(0:n,Components))
allocate(current(0:n,Components))
allocate(current(0:n,nc))

!Now what to do 
!Warnings
if ( nv==0 .and. ni==0 .and. nc==0) then 
    call CPU_TIME(cput); WRITE(100,*) cput,"No source detected, nothing to do."; stop
else if (nv==0 .and. ni==0) then
    call CPU_TIME(cput); WRITE(100,*) cput,"No sources, but capacitor present. Be sure it's charged"
        if ( avg_v(c%initial_charge)/=0) then
            call CPU_TIME(cput); WRITE(100,*) cput, "Capacior(s) are not charged. Nothing to do."; stop
        end if

        if (nc==1 .and. nr==1) then 
            WRITE(100,*) "Resistors : ",nr
            WRITE(100,*) "Capacitors: ",nc
                !V=q/C
            voltage(0,1)=(c(1)%initial_charge)*(c(1)%capacitance)
            do j = 1, n
                k1=dt*rc_v(voltage(j-1,1),c(1)%capacitance,r(1)%res)
                k2=dt*rc_v(voltage(j-1,1),c(1)%capacitance,r(1)%res)

               voltage(j,1)=voltage(j-1,1)+(k1+k2+k3+k4)/6
              ! voltage(j,1)=source_v(v(1)%amplitude, v(1)%freq, time(j))
              ! voltage(j,2)=-voltage(j,1)
              ! current(j,1)=ohm_V(voltage(j,1),r(1)%res)
              ! current(j,2)=current(j,1)
            enddo
        endif
else if (nc/=0 .and. nr==0) then
    call CPU_TIME(cput); WRITE(100,*) cput,"Capacitor with no resistances"; stop
end if



if (nv/=0 .and. ni==0) then
    call CPU_TIME(cput); WRITE(100,*) cput,"Voltage driven circuit detected."

    if (nl==0 .and. nc==0) then 
    call CPU_TIME(cput); WRITE(100,*) cput,"Resistive circuit detected."
    WRITE(100,*) "Voltage sources:", nv
        if (nr==1) then 
            WRITE(100,*) "Resistors:", nr, "?"
            WRITE(100,*) "...   ok"
            do j = 1, nv
                if (v(j)%type=='D') v(j)%freq=0 
            end do
            !WRITE(100,*)  v(1)
            do j = 0, n
               voltage(j,1)=source_v(v(1)%amplitude, v(1)%freq, time(j))
               voltage(j,2)=-voltage(j,1)
               current(j,1)=ohm_V(voltage(j,1),r(1)%res)
               current(j,2)=current(j,1)
            enddo
        endif

    endif
else if (nv==0 .and. ni/=0 ) then 
    call CPU_TIME(cput); WRITE(100,*) cput,"Current driven circuit detected."
else if (nv==0 .and. ni==0 ) then 
    call CPU_TIME(cput); WRITE(100,*) cput,"Voltage and current driven circuit detected."
end if


open(unit=102, file='./results/voltages.dat')
open(unit=103, file='./results/currents.dat')
do k = 1, Components
do j = 0, n
    write(102,*) time(j), voltage(j,k)
    write(103,*) time(j), current(j,k)
end do
    write(102,*) " "
    write(103,*) " "
end do
close(102)
close(103)
close(100)

    contains
real(dp) function source_v(amp,freq,t) result(volts)
    real(dp), intent(in):: amp, freq, t
    real(dp), parameter:: pi=3.14159
    if (freq==0) then
        volts=amp
    else 
        volts=amp*sin(2*pi*freq*t)
    endif
end function

real(dp) function ohm_V(volts,res) result(amps)
    real(dp), intent(in):: volts, res
    amps=volts/res
end function

real(dp) function rc_v(vol, cap, res) result(volts)
    real(dp), intent(in):: vol, cap, res
volts=-vol/(res*cap)

end function

!Vector functions
real(dp) function avg_v(vector) result(avg)
    real(dp), intent(in):: vector(:)
    avg=sum(vector(:))/size(vector(:))
end function
end program


