program main
!        use integrals
        use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
        implicit none
        integer,parameter :: n=100
        real(dp),parameter :: x0=0, xf=1, fx0=1, fxf=1
        integer::i,j
        real(dp)::x,h,a(n),b(n),c(n),d(n),l(n),u(n),z(n),w(0:n+1), int_y
        h=(xf-x0)/(n+1)
        w(0)=fx0
        w(n+1)=fxf
print*, size(w)
open(101,file="sh_n.dat")
open(100,file="sh.dat")
     do j=1,3
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
!#display(w)

do i = n-1,1,-1
w(i)=z(i)-u(i)*w(i+1)
enddo
do i=0,n+1
write(100,*) x0+i*h, w(i)
enddo
write(100,*)  " "
int_y=simple_integral(h,w)
int_y=int_y

do i=0,n+1
write(101,*) x0+i*h, w(i)/sqrt(int_y)
enddo
write(101,*)  " "
enddo
close(100)
close(101)

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
f=f*3*h/8
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
f=f*3*h/8
        end function double_integral

        
end program

