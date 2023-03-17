program main
        use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
        implicit none
        type particle
                REAL(dp),ALLOCATABLE:: x(:)
                REAL(dp),ALLOCATABLE:: y(:)
                REAL(dp),ALLOCATABLE:: t(:)
                REAL(dp),ALLOCATABLE:: v(:)
        end type particle
        real(dp), parameter::a=-1D0 , b=1D0 , c=-1. , d=1.
        integer, parameter::m=100 , n=100. !both >=3 and 2k
        real(dp), parameter::h=(b-a)/n , k=(d-c)/m 
        real(dp), parameter::lam=h*h/(k*k) , mu=2*(1+lam) 
        real(dp), parameter::tol=1D-10 , it=100009
        real(dp):: x(0:n), y(0:m), z, nor
        real(dp):: w(0:n, 0:m), grad(0:n, 0:m, 2)
        integer:: i, j, l
        real(dp), parameter:: q=1D0, qm=1D0
        !initial position
        real(dp), parameter:: qix=0.5D0, qiy=0.5D0, qiv=0D0
        real(dp), parameter:: t0=0D0, tf=10D0
        integer, PARAMETER:: nt=100
        real(dp), parameter:: ht=(tf-t0)/nt
        type(particle), allocatable :: p(:)
        !real(dp):: particle(0:nt,0:nt,0:nt)
!boundaries are x(0), y(0), x(n), y(m)
ALLOCATE(p(0:nt))
p(0)%x=qix
p(0)%x=qix
p(0)%v=qiv
if ( qix<x(0) .or. qix>x(n) .or. qiy<y(0) .or. qiy>y(m) ) then
print*, "Particle out of bounds at step 0: time 0"
end if

p(0)%t=t0
do i=1,nt-1
p(i)%t=t0+i*ht
enddo
p(nt)%t=tf


contains
subroutine rk4(iox, ioy, ih)
    real(dp), intent(inout) :: iox, ioy
    real(dp), intent(in) :: ih
    real(dp) :: vx, vy
    real(dp):: xk1, xk2, xk3,xk4
    real(dp):: yk1, yk2, yk3,yk4
    real(dp):: vxk1, vxk2, vxk3,vxk4
    real(dp):: vyk1, vyk2, vyk3,vyk4
    real(dp):: px, py
    integer:: si, sj
CALL()
vxk1=force(iox)*ih

iox=iox+(xk1+2*xk2+2*xk3+xk4)/6
ioy=ioy+(yk1+2*yk2+2*yk3+yk4)/6
    
end subroutine rk4
real(dp) function forcex(inx) result(output)
    real(dp), intent(inout) :: inx
if (inx<x(0)) then 
print*, "Particle out of bounds. x>X(0)"
else

    
end if
    
end function force
!

end program
