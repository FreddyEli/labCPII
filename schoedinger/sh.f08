program main
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none
  integer,parameter :: n=400, niveles=4
  real(dp),parameter :: x0=0, xf=1, fx0=1, fxf=1
  integer::i,j
  real(dp)::x,h,a(n),b(n),c(n),d(n),l(n),u(n),z(n),w(0:n+1),int_y
  real(dp)::fs(0:n+1,0:niveles),ort(niveles,niveles),dummy,dummy2,sigma

h=(xf-x0)/(n+1)
w(0)=fx0
w(n+1)=fxf
open(101,file="sh_n.dat")
open(100,file="sh.dat")
open(99,file="sh_n2.dat")

     do j=1,niveles
x=x0+h
a(1)=2+h*h*q(j,x) 
b(1)=-1+h*p(x)/2 
d(1)=-h*h*r(x)+(1+h*p(x)/2)*fx0 

!%##for ,nner inner point
do i = 2,n-1
        x=x0+h*i
        a(i)=2+h*h*q(j,x) 
        b(i)=-1+h*p(x)/2 
        c(i)=-1-h*p(x)/2 
        d(i)=-h*h*r(x)
enddo

!##for last inner point
x=xf-h
a(n)=2+h*h*q(j,x) 
c(n)=-1-h*p(x)/2 
d(n)=-h*h*r(x)+(1-h*p(x)/2)*fxf 
!##vectors a,b,c,d are populated

!#Solving the matrix
l(1)=a(1)
u(1)=b(1)/a(1)
z(1)=d(1)/l(1)

do i = 2,n-1
        l(i)=a(i)-c(i)*u(i-1)
        u(i)=b(i)/l(i)
        z(i)=(d(i)-c(i)*z(i-1))/l(i)
enddo

l(n)=a(n)-c(n)*u(n-1)
z(n)=(d(n)-c(n)*z(n-1))/l(n)

do i = 1,n
        w(i)=z(i)-fx0
enddo

do i = n-1,1,-1
        w(i)=z(i)-u(i)*w(i+1)
enddo

do i=0,n+1
        if (j==1) then
                fs(i,0)= x0+i*h
        endif
        write(100,*) fs(i,0), w(i)
enddo
write(100,*)  " "
int_y=simple_integral(h,w)
int_y=int_y

do i=0,n+1
        fs(i,j)=w(i)/sqrt(int_y)
        write(101,*) fs(i,0), fs(i,j)
        write(99,*) fs(i,0), fs(i,j)*fs(i,j)
enddo
write(101,*)  " "
write(99,*)  " "

        enddo
close(99)
close(100)
close(101)

do i=1,niveles
        do j=1,niveles
                ort(i,j)=double_integral(h,fs(:,i),fs(:,j))
        enddo
enddo

open(102,file="sh.ortm")
do i=1,niveles
        write(102,*) ort(:,i)
enddo
close(102)

open(103,file="sh.stat")
do i=1,niveles
        dummy=expx(h,fs(:,0),fs(:,i))
        dummy2=expx2(h,fs(:,0),fs(:,i))
        sigma=sqrt(dummy2-dummy*dummy)
        write(103,*) "       <x> for level",i," is ", dummy
        write(103,*) "      <x2> for level",i," is ", dummy2
        write(103,*) "<x2>-<x>^2 for level",i," is ", dummy2-dummy*dummy
        write(103,*) "        SD for level",i," is ", sigma
        write(103,*) "Probability to to find electron between x+sigma and x-sigma"
        write(103,*)  prob(h,fs(:,0),fs(:,i), dummy-sigma , dummy+sigma)
        write(103,*) "Probability to to find electron between 0.45 and 0.55"
        write(103,*)  prob(h,fs(:,0),fs(:,i), 0.45D0, 0.55D0)
        write(103,*) "Probability to to find electron on the left side"
        write(103,*)  prob(h,fs(:,0),fs(:,i), 0D0, 0.5D0)
        write(103,*) "Probability to to find electron on the first quarter"
        write(103,*)  prob(h,fs(:,0),fs(:,i), 0D0, 0.25D0)
        write(103,*) " "
enddo
close(103)


contains
real(dp) function p(r) result(f)
  implicit none
  real(dp), intent(in) :: r
    f = 0D0
end function p

real(dp) function q(well_n, r) result(E)
  implicit none
  real(dp), intent(in) :: r
  integer, intent(in) :: well_n
  real(dp), parameter:: m=1D0,  pi=3.14159265
    E =-(well_n*pi*well_n*pi)
end function q

real(dp) function r(x) result(f)
  implicit none
  real(dp), intent(in) :: x
    f = 0D0
end function r

real(dp) function simple_integral(h,y) result(f)
  real(dp), intent(in) :: h, y(0:)
  integer ::i
    f=y(0)*y(0)
    do i=1, (size(y)-1)/2
         f=f+4*y(2*i-1)*y(2*i-1)
    enddo
    do i=2, (size(y)-1)/2-1
         f=f+2*y(2*i)*y(2*i)
    enddo
    f=f+y(size(y)-1)*y(size(y)-1)
    f=f*h/3
end function simple_integral

real(dp) function double_integral(h,y,w) result(f)
  real(dp), intent(in) :: h, y(0:), w(0:)
  integer ::i
    f=w(0)*y(0)
    do i=1, (size(y)-1)/2
         f=f+4*w(2*i-1)*y(2*i-1)
    enddo
    do i=2, (size(y)-1)/2-1
         f=f+2*w(2*i)*y(2*i)
    enddo
    f=f+w(size(y)-1)*y(size(y)-1)
    f=f*h/3
end function double_integral

real(dp) function prob(h,x, w,a,b) result(f)
  real(dp), intent(in) :: h, x(0:), w(0:), a, b
  integer :: xa(1), xb(1)
  integer ::i , ia , ib
    xa=minloc(abs(x-a))
    xb=minloc(abs(x-b))
    ia=xa(1)
    ib=xb(1)
    if (MOD(ib-ia,2)==0) then
        f=w(ia)*w(ia)
        do i=ia+1, ib-1, 2
             f=f+4*w(i)*w(i)
        enddo
        do i=ia+2, ib-1, 2
             f=f+2*w(i)*w(i)
        enddo
        f=f+w(ib)*w(ib)
        f=f*h/3
    else 
        ib=ib+1
        f=w(ia)*w(ia)
        do i=ia+1, ib-1, 2
             f=f+4*w(i)*w(i)
        enddo
        do i=ia+2, ib-1, 2
             f=f+2*w(i)*w(i)
        enddo
        f=f+w(ib)*w(ib)
        f=f*h/3
    endif
end function 

real(dp) function expx(h,x,w) result(f)
  real(dp), intent(in) :: h, x(0:), w(0:)
  integer ::i
    f=x(0)*w(0)*w(0)
    do i=1, (size(w)-1)/2
        f=f+4*x(2*i-1)*w(2*i-1)*w(2*i-1)
    enddo
    do i=2, (size(w)-1)/2-1
        f=f+2*w(2*i)*w(2*i)*x(2*i)
    enddo
    f=f+x(size(x)-1)*w(size(w)-1)*w(size(w)-1)
    f=f*h/3
end function 
        
real(dp) function expx2(h,x,w) result(f)
  real(dp), intent(in) :: h, x(0:), w(0:)
  integer ::i
    f=x(0)*x(0)*w(0)*w(0)
    do i=1, (size(w)-1)/2
        f=f+4*x(2*i-1)*x(2*i-1)*w(2*i-1)*w(2*i-1)
    enddo
    do i=2, (size(w)-1)/2-1
        f=f+2*w(2*i)*w(2*i)*x(2*i)*x(2*i)
    enddo
    f=f+x(size(x)-1)*x(size(x)-1)*w(size(w)-1)*w(size(w)-1)
    f=f*h/3
end function 

end program


