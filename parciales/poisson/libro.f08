program main
        use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
        implicit none
        real(dp), parameter::a=0 , b=2 , c=0 , d=1
        integer, parameter::m=5 , n=6. !both >=3
        real(dp), parameter::h=(b-a)/n , k=(d-c)/m 
        real(dp), parameter::lam=h*h/(k*k) , mu=2*(1+lam) 
        real(dp), parameter::tol=1D-10 , it=100
        real(dp):: x(n-1), y(m-1), z, nor
        real(dp):: w(n-1, m-1)
        integer:: i, j, l
open(100,file="libro.dat")
        do i= 1, n-1
x(i)=a+i*h
        enddo
        do i= 1, m-1
y(i)=c+i*k
        enddo
w=0
l=1
do while( l < it )
z=(-h*h*f(x(1),y(m-1))+g(a,y(m-1))+lam*g(x(1),d)+lam*w(1,m-2)+w(2,m-1))/mu
nor=abs(z-w(1,m-1))
w(1,m-1)=z

        do i= 2, n-2
z=(-h*h*f(x(i),y(m-1))+lam*g(x(i),d)+w(i-1,m-1)+w(i+1,m-1)+lam*w(i,m-2))/mu
if ( abs(w(i,m-1)-z)>nor ) then
        nor=abs(w(i,m-1)-z)
endif
w(i,m-1)=z
        enddo

z=(-h*h*f(x(n-1),y(m-1))+g(b,y(m-1))+lam*g(x(n-1),d)+w(n-2,m-1)+lam*w(n-1,m-2))/mu
if ( abs(w(n-1,m-1)-z)>nor ) then
        nor=abs(w(n-1,m-1)-z)
endif
w(n-1,m-1)=z

        do j= m-2,2,-1
        z=(-h*h*f(x(1),y(j))+g(a,y(j))+lam*w(1,j+1)+lam*w(1,j-1)+w(2,j))/mu
        if ( abs(w(1,j)-z)>nor ) then
                nor=abs(w(1,j)-z)
        endif
        w(1,j)=z

                do i= 2, n-2
        z=(-h*h*f(x(i),y(j))+w(i-1,j)+lam*w(i,j+1)+w(i+1,j)+lam*w(i,j-1))/mu
        if ( abs(w(i,j)-z)>nor ) then
                nor=abs(w(i,j)-z)
        endif
        w(i,j)=z
                enddo

        z=(-h*h*f(x(n-1),y(j))+g(b,y(j))+w(n-2,j)+lam*w(n-1,j+1)+lam*w(n-1,j-1))/mu
        if ( abs(w(n-1,j)-z)>nor ) then
                nor=abs(w(n-1,j)-z)
        endif
        w(n-1,j)=z
        enddo

z=(-h*h*f(x(1),y(1))+g(a,y(1))+lam*g(x(1),c)+lam*w(1,2)+w(2,1))/mu
if ( abs(w(1,1)-z)>nor ) then
nor=abs(z-w(1,1))
endif
w(1,1)=z

        do i= 2, n-2
z=(-h*h*f(x(i),y(1))+lam*g(x(i),c)+w(i-1,1)+lam*w(i,2)+w(i+1,1))/mu
if ( abs(w(i,1)-z)>nor ) then
        nor=abs(w(i,1)-z)
endif
w(i,1)=z
        enddo

z=(-h*h*f(x(n-1),y(1))+g(b,y(1))+lam*g(x(n-1),c)+w(n-2,1)+lam*w(n-1,2))/mu
if ( abs(w(n-1,1)-z)>nor ) then
        nor=abs(w(n-1,1)-z)
endif
w(n-1,1)=z

!Paso 17
if ( nor<tol ) then
        do i=1,n-1
        do j=1,m-1
                write(100,*)  x(i), y(j), w(i,j)
        enddo
        enddo
        go to 10
endif

l=l+1
enddo

print*, "Maximum number of iterations exceeded"
10 continue 
close(100)
contains

real(dp) function f(x,y) result(r)
  implicit none
  real(dp), intent(in) :: x,y
  r=x*exp(y)
end function f

real(dp) function g(x,y) result(f)
  implicit none
  real(dp), intent(in) :: x,y
  if ( x==0 ) then 
          f=0
  else if (x==2) then
          f=2*exp(y)
  endif
  if ( y==0 ) then 
          f=x
  else if ( y==1 ) then 
          f=exp(1.)*x
  endif
end function g

end program
