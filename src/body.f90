module body ! physical body
  use integrator

  implicit none

  type, public :: body_t
     type(vec3_t) :: x = vec3_t(0.0, 0.0, 0.0) ! position
     type(vec3_t) :: x1 = vec3_t(0.0, 0.0, 0.0)
     type(vec3_t) :: x2 = vec3_t(0.0, 0.0, 0.0)
     type(vec3_t) :: v = vec3_t(0.0, 0.0, 0.0) ! velocity
     type(vec3_t) :: a = vec3_t(0.0, 0.0, 0.0) ! acceleration
     type(vec3_t) :: a1 = vec3_t(0.0, 0.0, 0.0)
     type(vec3_t) :: f = vec3_t(0.0, 0.0, 0.0) ! total forces

     real(rk) :: m = 1.0 ! mass
     real(rk) :: r = 1.0 ! radius
  end type body_t
contains
  subroutine b_interacts(this, another) ! interaction between bodies
    class(body_t) :: this
    class(body_t) :: another

    real(rk) :: r
    type(vec3_t) :: force

    ! distance
    r = vlength(this%x - another%x)

    ! Newton gravity
    force = normalize(another%x - this%x) ! unit vector r
    force = force * (gc * this%m * another%m) / (r**2)

    if (r < abs(this%r + another%r)) then ! bodies intersecting
       force = vec3_t(0.0, 0.0, 0.0) ! prevent singularity
    end if

    if (valid(vlength(force)) .or. .true.) then
       this%f = this%f + force ! newton III
       another%f = another%f - force
    end if
  end subroutine b_interacts

  subroutine b_tick(this, dt) ! one time cycle
    type(body_t) :: this
    real(rk) :: dt

    this%x2 = this%x1 ! save old
    this%x1 = this%x
    this%a1 = this%a

    ! Newton II
    this%a = this%f * (1.0 / this%m)

    ! integrate
    call vel_verlet3(this%x, this%v, this%a, this%a1, dt)

    ! reset force for next round
    this%f = vec3_t(0.0, 0.0, 0.0)
  end subroutine b_tick

  subroutine b_init(this, dt) ! initialize body
    type(body_t) :: this
    real(rk) :: dt
    
    !this%p = verlet_int0(this%p, this%p2, this%a, dt)
  end subroutine b_init
end module body
