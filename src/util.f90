module util ! utility programs
  implicit none

  integer, parameter :: rk = 8 ! real precision
  integer, parameter :: ik = 8 ! int precision

  ! constants
  real(rk) :: gc = 6.67408e-11 ! gravitational constant
contains
  logical function valid(v) ! is the number valid
    real(rk), intent(in) :: v

    valid = .not. isnan(v) ! nan check
    !valid = valid .and. (v /= v - real(1.0, rk)) ! inf check
  end function valid

  subroutine append(a, v) ! append real to array
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
  end subroutine append
end module util
