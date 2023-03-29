program input
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none
        type generalComponent
                integer:: ID
                CHARACTER:: type
                integer:: start
                integer:: end
        end type generalComponent
!        type voltage_source
!                integer:: start
!                integer:: end
!                CHARACTER:: type
!                REAL(dp):: amplitude
!                REAL(dp):: freq
!        end type voltage_source
!        type current_source
!                integer:: start
!                integer:: end
!                CHARACTER:: type
!                REAL(dp):: amplitude
!                REAL(dp):: freq
!        end type current_source
!        type resistor
!                integer:: start
!                integer:: end
!                REAL(dp):: res
!        end type resistor
!        type inductor
!                integer:: start
!                integer:: end
!                REAL(dp):: inductance
!        end type inductor
!        type capacitor
!                integer:: start
!                integer:: end
!                real(dp):: capacitance
!                real(dp):: initial_charge
!        end type capacitor
!
!  type(voltage_source), allocatable:: v(:)
!  type(current_source), allocatable:: i(:)
!  type(resistor),  allocatable:: r(:)
!  type(inductor),  allocatable:: l(:)
!  type(capacitor), allocatable:: c(:)
!  integer, parameter:: n=100
!  real(dp), PARAMETER:: t0=0D0, tf=3D0, dt=(tf-t0)/n
!  !real(dp), :: voltage(0:n), current(0:n), time(0:n)
!  real(dp):: time(0:n)
!  real(dp):: k1, k2, k3, k4
!  real(dp), allocatable:: voltage(:,:), current(:,:), charge(:,:)

INTEGER:: nComps, nNodes, nLoops
INTEGER:: i,j
INTEGER, ALLOCATABLE:: adjacencyMat(:,:)
LOGICAL, ALLOCATABLE:: visited(:), completed(:) 
INTEGER, ALLOCATABLE:: stackSize(:), loopIndex(:), stack(:)
type(generalComponent), allocatable:: components(:)
INTEGER:: istat

open(100, file="main.log")
    !input
nComps=0; nNodes=0; nLoops=0
!Find number of Nodes
call SYSTEM('cd shell && ./sort.input && cd ..')

!Read number of components
open(1, file="./input/wc.input", status="old");read (1,*)  nComps; close(1)
open(7, file="./input/wc.nodes", status="old"); read(7,*)  nNodes; close(7)
print*,  "Number of componentes: ", nComps
print*,  "Number of nodes      : ", nNodes
print*,  "Teorethical loops    : ", nComps-(nNodes-1)

ALLOCATE(adjacencyMat(nNodes,nNodes))
adjacencyMat=0
ALLOCATE(visited(nNodes))
ALLOCATE(completed(nNodes))
ALLOCATE(stack(nNodes)) 
ALLOCATE(stackSize(nNodes))
ALLOCATE(loopIndex(nComps))
ALLOCATE(components(nComps))

open(1, file="./input/components.input", status="old")
do i = 1, nComps
        read(1, *, iostat=istat) components(i)%ID, components(i)%type, components(i)%start, components(i)%end
        if (istat /= 0) stop "Read error in components unit"
end do
close(1)

do i = 1, nNodes
    !nodes are components(i)%start, components(j)%end
adjacencyMat(components(i)%start,components(i)%end)= 1
adjacencyMat(components(i)%end,components(i)%start)= 1
end do

do i = 1, nNodes
    print*, adjacencyMat(i,:)
end do

do i = 1, nComps
   !Initialize arrays 
   visited=.FALSE.
   completed=.FALSE.
   stack=0
   stackSize=0

   !Start a new loop search from each unvisited node
   do j = 1, nNodes
   if (.not. visited(j)) then
       call dfs(i,j)
   endif
   end do
!Store loopo indices
if (stackSize(i) > 0) then
    nLoops= nLoops + 1
    loopIndex(1:stackSize(i)) = stack(1:stackSize(i))
end if
end do

print*, "Number of loops: ", nloops
print*, "Loop indices   :"
write(*,*) (loopIndex(i), i=1, nComps)
close(100)

contains
recursive SUBROUTINE dfs(node, comp)
    INTEGER :: node, comp
    INTEGER :: k, l

    visited(node) = .TRUE.
    stackSize(comp) = stackSize(comp) + 1
    stack(stackSize(comp)) = node

    ! Loop through all adjacent nodes
    DO k = 1, nNodes
        IF (adjacencyMat(node,k) == 1) THEN
            IF (k == stack(1)) THEN
                ! Found a loop!
                stackSize(comp) = 1
                RETURN
            ELSE IF (.NOT. visited(k)) THEN
                CALL dfs(k, comp)
                IF (stackSize(comp) == 1) RETURN
            END IF
        END IF
    END DO

    ! This node is done
    stackSize(comp) = stackSize(comp) - 1
    completed(node) = .TRUE.
END SUBROUTINE
end program 
