module integrator ! integration routines
  use vec3
  
  implicit none
contains
  real function verlet_int0(x0, v0, d2x0, dt)
    real(rk), intent(in) :: x0, v0, d2x0, dt

    verlet_int0 = x0 + v0 * dt + 0.5 * d2x0 * dt**2
  end function verlet_int0

  real function verlet_int(xn, xnm1, d2xn, dt)
    real(rk), intent(in) :: xn, xnm1, d2xn, dt

    verlet_int = 2.0 * xn - xnm1 + d2xn * dt**2
  end function verlet_int

  subroutine vel_verlet(x, v, a, a1, dt) ! velocity verlet
    real(rk) :: x, a, a1, v, dt

    x = x + v * dt + 0.5 * a * dt**2
    v = v + 0.5 * (a1 + a) * dt
  end subroutine vel_verlet

  subroutine euler_int(x, v, a, dt) ! euler
    real(rk) :: x, a, v, dt

    x = x + v * dt
    v = v + a * dt
  end subroutine euler_int

  subroutine vel_verlet3(x, v, a, a1, dt) ! vec3 verlet
    type(vec3_t) :: x, a, a1, v
    real(rk) :: dt

    x = x + v * dt + mul(a, real(0.5, rk) * dt**2)
    v = v + mul((a1 + a), real(0.5, rk) * dt)
  end subroutine vel_verlet3
end module integrator
