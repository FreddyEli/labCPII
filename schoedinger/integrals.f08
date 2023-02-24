module integrals
        use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
        implicit none
        private
        public simple_integral, double_integral
        real(sp):: simple_integral, double_integral
        contains

        real(sp) function simple_integral(h,y) result(f)
        real(sp), intent(in) :: h, y(0:)
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

        real(sp) function double_integral(h,y,w) result(f)
        real(sp), intent(in) :: h, y(0:), w(0:)
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

        end module integrals
        
