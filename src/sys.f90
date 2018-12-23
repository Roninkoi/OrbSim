module sys ! physical system
  use body

  implicit none

  type(body_t), allocatable :: obj(:)

  integer :: objnum = 1 ! number of objects

  integer(ik) :: s = 0 ! current step
  integer(ik) :: n = 0 ! number of steps
  real(rk) :: t = 0.0 ! current time
  real(rk) :: dt = 1000.0 ! timestep
  real(rk) :: tn = 1000000000.0 ! total time

  integer :: tp = 0 ! time parameters
  integer(ik) :: pm = 1000 ! print m
  integer(ik) :: wk = 100 ! write k
  integer :: mode = 0 ! simulation mode
contains
  subroutine s_init() ! initialize system
    integer :: i

    do i = 1, objnum, 1
       call b_init(obj(i), dt)
    end do
  end subroutine s_init

  subroutine s_tick()
    integer :: i, j

    do i = 1, objnum, 1
       do j = i + 1, objnum, 1
          call b_interacts(obj(i), obj(j)) ! gravitational
       end do
    end do

    do i = 1, objnum, 1
       call b_tick(obj(i), dt) ! update objects
    end do

    t = t + dt ! advance time
  end subroutine s_tick

  subroutine s_load(path) ! input file parser
    character(80) :: path
    integer :: id, io

    real(rk) :: m, r, x, y, z, vx, vy, vz

    open(9, file = path, status = "old", action = "read", iostat = io)

    ! simulation parameters
    read(9, *) objnum, mode, tp

    ! different cases for time parameters
    select case (tp)
    case(0) ! step, total
       read(9, *) dt, tn
       n = int(tn / dt)
    case(1) ! step, num
       read(9, *) dt, n
       tn = real(n, rk) * dt
    case(2) ! total, num
       read(9, *) tn, n
       dt = tn / real(n, rk)
    end select

    read(9, *) pm, wk ! print / write frequency

    allocate(obj(objnum))

    id = 1
    do while (id <= objnum)
       ! object parameters (mass, radius, x, v)
       read(9, *, iostat = io) m, r, x, y, z, vx, vy, vz

       if (io /= 0) exit ! if end or other

       obj(id)%m = m
       obj(id)%r = r

       obj(id)%x = vec3_t(x, y, z)

       obj(id)%v = vec3_t(vx, vy, vz)

       id = id + 1
    end do
  end subroutine s_load

  subroutine s_pinfo()
    integer :: i
    
    print *, "objnum: ", objnum
    print *, "time (s): ", t, " steps: ", s
    print *, "written: ", s / wk

    do i = 1, objnum, 1
       print *, "id: ", int(i, 1), " x (m): ", obj(i)%x%x, obj(i)%x%y, obj(i)%x%z
    end do
  end subroutine s_pinfo
end module sys
