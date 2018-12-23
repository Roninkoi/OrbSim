! This is a render program to visualize the
! output produced by sim. Data is read in from
! stdin. The object with id 1 will be at the
! origin and id 2 is the one that is plotted.

module objects
  implicit none

  integer, parameter :: rk = 8
  real, parameter :: rr = 0.05
  type obj_t
     real :: x = 0.0
     real :: y = 0.0
     real :: z = 0.0

     real :: r = rr
  end type obj_t

  type(obj_t), allocatable :: objs(:)
contains
  subroutine appendr(a, v)
    real(rk), allocatable :: a(:)
    real(rk) :: v

    real(rk), allocatable :: b(:)
    integer :: n

    n = size(a)

    allocate(b(n))

    b = a
    deallocate(a)
    allocate(a(n + 1))

    a(1:n) = b
    a(n + 1) = v
  end subroutine appendr

  subroutine append(a, v)
    type(obj_t), allocatable :: a(:)
    type(obj_t) :: v

    type(obj_t), allocatable :: b(:)
    integer :: n

    n = size(a)

    allocate(b(n))

    b = a
    deallocate(a)
    allocate(a(n + 1))

    a(1:n) = b
    a(n + 1) = v
  end subroutine append
end module objects

program main
  use opengl_gl
  use opengl_glu
  use opengl_glut
  use gnufor2
  use objects

  implicit none

  integer :: i

  real(rk) :: scale
  real(rk), allocatable :: x(:)
  real(rk), allocatable :: y(:)

  allocate(objs(0))
  allocate(x(0))
  allocate(y(0))

  scale = 300000000000.0*4.5 ! scale to world space

  call glutinit()
  call glutinitwindowsize(800, 800)
  call glutinitdisplaymode(GLUT_DOUBLE+GLUT_RGB+GLUT_DEPTH)
  i = glutcreatewindow('sim')

  call gfxinit
  call glutdisplayfunc(render) ! callback functions
  call glutidlefunc(tick)
  call glutmainloop
contains
  subroutine tick()
    call readdata
    call update
    call render

    if (size(objs) >= 2) then ! add obj 2 to plot array
       call appendr(x, scale*real(objs(2)%x - 1.0*objs(1)%x, rk))
       call appendr(y, scale*real(objs(2)%y - 1.0*objs(1)%y, rk))
    end if
  end subroutine tick

  subroutine render()
    call glclear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT)
    call glcalllist(1)
    call glutswapbuffers
  end subroutine render

  subroutine update
    real :: cs
    real :: ss
    call glnewlist(1, GL_COMPILE)

    do i = 1, size(objs), 1 ! set up objects
       ss = sin(6.0*real(i) / size(objs)) + 0.5
       cs = cos(6.0*real(i) / size(objs)) + 0.5
       call glpushmatrix
       call glcolor3f(ss, cs, 1.0-(ss+cs))
       call gltranslatef(objs(i)%x - objs(1)%x, & ! position
            objs(i)%y - objs(1)%y, &
            objs(i)%z - objs(1)%z)
       call glutsolidsphere(real(objs(i)%r, gldouble), 10, 10)
       call glpopmatrix
    end do

    call glendlist
  end subroutine update

  subroutine gfxinit
    integer :: i

    call update

    call gldisable(GL_LIGHTING) ! gl calls
    call glenable(GL_DEPTH_TEST)
    call glmatrixmode(GL_PROJECTION)
    call gluperspective(40.0_gldouble, 1.0_gldouble, &
         1.0_gldouble, 100.0_gldouble)
    call glmatrixmode(GL_MODELVIEW)
    call glulookat( &
         0.0_gldouble, 0.0_gldouble, 5.0_gldouble, &
         0.0_gldouble, 0.0_gldouble, 0.0_gldouble, &
         0.0_gldouble, 1.0_gldouble, 1.0_gldouble)
    call gltranslatef(0.0, 0.0, -5.0)
  end subroutine gfxinit

  subroutine readdata()
    integer :: i, io, id = 1
    real :: t, x, y, z

    i = 1
    do while (i <= size(objs) .or. i < 2)
       read(*, *, iostat = io) id, t, x, y, z ! read data

       ! exit the program on error or end
       if (io /= 0) call quit

       if (id > size(objs)) then
          call append(objs, obj_t(0.0, 0.0, 0.0, rr))
       end if

       ! scale objs to fit screen
       objs(id)%x = x/scale
       objs(id)%y = y/scale
       print *, id, t, x, y
       i = i + 1
    end do
  end subroutine readdata

  subroutine quit
    call plot(x, y) ! create plot

    call glutleavemainloop
  end subroutine quit
end program main
