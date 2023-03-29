program main
        use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
        implicit none
        real(dp), parameter::a=-10D0 , b=10D0 , c=-10. , d=10.
        integer, parameter::m=150 , n=150. !both >=3 and 2k
        real(dp), parameter::h=(b-a)/n , k=(d-c)/m 
        real(dp), parameter::lam=h*h/(k*k) , mu=2*(1+lam) 
        real(dp), parameter::tol=1D-10 , it=100009
        !real(dp):: x(n-1), y(m-1), z, nor
        real(dp):: x(0:n), y(0:m), z, nor
        !real(dp):: w(n-1, m-1), grad(n-1, m-1, 2)
        real(dp):: w(0:n, 0:m), grad(0:n, 0:m, 2), norm
        integer:: i, j, l
open(99,file="libro.fuente")
open(100,file="libro.dat")
open(101,file="libro_grad.dat")
        do i= 0, n
x(i)=a+i*h
        enddo
        do i= 0, m
y(i)=c+i*k
        enddo
w=0D0
do i=0, n
w(i,0)= g(x(i),y(0))
w(i,m)= g(x(i),y(m))
enddo
do i=0, m
w(0,i)= g(x(0),y(i))
w(n,i)= g(x(n),y(i))
enddo
l=1
do while( l < it )
z=( -h*h*f(h,k,x(1),y(m-1))+g(a,y(m-1))+lam*g(x(1),d)+lam*w(1,m-2)+w(2,m-1) )/mu
nor=abs(z-w(1,m-1))
w(1,m-1)=z

        do i= 2, n-2
z=(-h*h*f(h,k,x(i),y(m-1))+lam*g(x(i),d)+w(i-1,m-1)+w(i+1,m-1)+lam*w(i,m-2))/mu
if ( abs(w(i,m-1)-z)>nor ) then
        nor=abs(w(i,m-1)-z)
endif
w(i,m-1)=z
        enddo

z=(-h*h*f(h,k,x(n-1),y(m-1))+g(b,y(m-1))+lam*g(x(n-1),d)+w(n-2,m-1)+lam*w(n-1,m-2))/mu
if ( abs(w(n-1,m-1)-z)>nor ) then
        nor=abs(w(n-1,m-1)-z)
endif
w(n-1,m-1)=z

        do j= 2, m-2
        z=(-h*h*f(h,k,x(1),y(j))+g(a,y(j))+lam*w(1,j+1)+lam*w(1,j-1)+w(2,j))/mu
        if ( abs(w(1,j)-z)>nor ) then
                nor=abs(w(1,j)-z)
        endif
        w(1,j)=z

                do i= 2, n-2
        z=(-h*h*f(h,k,x(i),y(j))+w(i-1,j)+lam*w(i,j+1)+w(i+1,j)+lam*w(i,j-1))/mu
        if ( abs(w(i,j)-z)>nor ) then
                nor=abs(w(i,j)-z)
        endif
        w(i,j)=z
                enddo

        z=(-h*h*f(h,k,x(n-1),y(j))+g(b,y(j))+w(n-2,j)+lam*w(n-1,j+1)+lam*w(n-1,j-1))/mu
        if ( abs(w(n-1,j)-z)>nor ) then
                nor=abs(w(n-1,j)-z)
        endif
        w(n-1,j)=z
        enddo

!14
z=(-h*h*f(h,k,x(1),y(1))+g(a,y(1))+lam*g(x(1),c)+lam*w(1,2)+w(2,1))/mu
if ( abs(w(1,1)-z)>nor ) then
nor=abs(z-w(1,1))
endif
w(1,1)=z

        do i= 2, n-2
z=(-h*h*f(h,k,x(i),y(1))+lam*g(x(i),c)+w(i-1,1)+lam*w(i,2)+w(i+1,1))/mu
if ( abs(w(i,1)-z)>nor ) then
        nor=abs(w(i,1)-z)
endif
w(i,1)=z
        enddo

z=(-h*h*f(h,k,x(n-1),y(1))+g(b,y(1))+lam*g(x(n-1),c)+w(n-2,1)+lam*w(n-1,2))/mu
if ( abs(w(n-1,1)-z)>nor ) then
        nor=abs(w(n-1,1)-z)
endif
w(n-1,1)=z

!Paso 17
if ( nor<=tol ) then
        do i=0,n
        do j=0,m
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

call gradiente(w,h,k,grad)
do i=0,n
do j=0,m
!norm= sqrt(grad(i,j,1)**2+grad(i,j,2)**2)
norm=1
write(101,*)  x(i), y(j), grad(i,j,1)/norm, grad(i,j,2)/norm
write(99,*)  x(i), y(j), f(h,k,x(i),y(j)), g(x(i),y(j))
enddo
enddo
close(99)
close(101)
contains

        !fuente
real(dp) function f(h,k,x,y) result(r)
  implicit none
  real(dp), intent(in) ::h,k,x,y
  if (abs(x-5D0)<=h/2 .and. abs(y-5D0)<=k/2) then         
          r=+10D0
  else if (abs(x+5D0)<=h/2 .and. abs(y+5D0)<=k/2) then 
          r=-10D0
      else
 r=0D0
 endif
end function f

!Frontera
real(dp) function g(x,y) result(f)
  implicit none
  real(dp), intent(in) :: x,y
!  if ( x==0 ) then 
!          f=0
!  else if (x==2) then
!          f=2*exp(y)
!  endif
 if ( x==0 ) then 
!          f=x
!  else if ( y==1 ) then 
          !f=exp(1.)*x
          f=0D0
  else
          f=0D0
 endif
end function g

subroutine gradiente(w,h,k,g)
  implicit none
  real(dp), intent(in):: w(0:,0:), h, k
!  real(dp):: g(0: size(w(:,0)-1), 0:size(w(0,:)-1) , 2)
  real(dp):: g(0:, 0:, :)
  integer::i, j, m, n

  n=size(w(:,0))-1
  m=size(w(0,:))-1
! Primeros puntos x
  do i = 0, 1
  do j = 0, m
g(i,j,1)=(-1.5*w(i,j)+2*w(i+1,j)-0.5*w(i+2,j))/h
  enddo
  enddo

! Primeros puntos y
  do i = 0, n
  do j = 0, 1
g(i,j,2)=(-1.5*w(i,j)+2*w(i,j+1)-0.5*w(i,j+2))/k
  enddo
  enddo

! Últimos puntos x
  do i = n, n-1, -1
  do j = 0, m
g(i,j,1)=(1.5*w(i,j)-2*w(i-1,j)+0.5*w(i-2,j))/h
  enddo
  enddo

! Últimos puntos y
  do i = 0, n
  do j = m, m-1, -1
g(i,j,2)=(1.5*w(i,j)-2*w(i,j-1)+0.5*w(i,j-2))/k
  enddo
  enddo

  !Púntos medios 
  do i = 2, n-2
  do j = 2, n-2
  g(i,j,1)=( w(i-2,j)/12 - 2*w(i-1,j)/3  - w(i+2,j)/12 + 2*w(i+1,j)/3 )/h
  g(i,j,2)=( w(i,j-2)/12 - 2*w(i,j-1)/3  - w(i,j+2)/12 + 2*w(i,j+1)/3 )/k
  enddo
  enddo
end subroutine

end program
