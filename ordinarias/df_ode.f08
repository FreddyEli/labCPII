program df_ode
integer, parameter :: np=9
real, parameter::x0=1, xf=2
real, parameter :: h=(xf-x0)/(np-1)
real, dimension(np-1, np-1):: A, L, U 
real, dimension(np-1)::x, y, f
integer:: i, j

do i =1, np-1
x(i)=x0+h*i
enddo
A=0
A(1,1)=2+h**2*q(x0)
A(1,2)=-1+h*p(x0)/2
do i=2, np-2
A(i,i-1)=-1.-h*p(x(i))/2
A(i,i)=2.-h**2*q(x(i))
A(i,i+1)=-1.-h*p(x(i))/2
enddo
A(np-1,np-2)=-1.+h*p(x(np-1))/2
A(np-1,np-1)=2.+h**2*q(x(np-1))

do i=1, np-1
print*, (A(i,j), j=1, np-1)
enddo
end program

real function p(x)
        real, intent(in) ::x
        p=2./x
        return
        end function

real function q(x)
        real, intent(in) ::x
        q=-2./(x**2)
        return
        end function
real function r(x)
        real, intent(in) ::x
        r=-sin(log(x))/x**2
        return
        end function
