
program input
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none
  real(sp), parameter::R=1, C=0.2, L=0.25, E=5
  integer, parameter::n=100
  real(sp), parameter::t0=0D0, tf= 10D0, h=(tf-t0)/n
  real(sp):: t(0:n), i(0:n), q(0:n), v(0:n), k11, k12, k21, k22
  integer:: k

t(0)=t0
t(n)=tf
i(0)=0
q(0)=0
v(0)=q(0)/C

do k=0,n-1
k11=h*(-R*i(k)/L - q(k)/(L*C) +E/L)
k12=i(k)*h
k21=h*(-R*(i(k)+k11)/L - (q(k)+k12)/(L*C) +E/L)
k22=(i(k)+K11)*h

i(k+1)= i(k)+(k11+k21)/2
q(k+1)= q(k)+(k12+k22)/2
v(k+1)=q(k+1)/C
t(k+1)=t(k)+h
enddo
do k=0,n
write (1,*) t(k), q(k), i(k), v(k)
enddo
end program


