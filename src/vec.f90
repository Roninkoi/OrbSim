module vec3 ! vector math
  use util

  implicit none

  type vec3_t ! cartesian 3-vector type
     real(rk) :: x = 0.0 ! i-component
     real(rk) :: y = 0.0 ! j-component
     real(rk) :: z = 0.0 ! k-component
  end type vec3_t

  ! operator overloading for * and **
  interface operator (*) ! dot(a, b) = a * b
     procedure dot
  end interface operator (*)

  interface operator (**) ! cross(a, b) = a ** b
     procedure cross
  end interface operator (**)

  interface operator (+) ! vector addition
     procedure add
  end interface operator (+)

  interface operator (-)
     procedure sub
  end interface operator (-)

  interface operator (*) ! scalar scaling
     procedure mul
  end interface operator (*)

  interface operator (/)
     procedure div
  end interface operator (/)

  interface operator (.vcos.) ! cosine of vec angle
     procedure vcos
  end interface operator (.vcos.)
contains
  real(rk) function dot(a, b) ! vector dot product a . b
    type(vec3_t), intent(in) :: a, b

    dot = a%x * b%x + a%y * b%y + a%z * b%z
  end function dot

  type(vec3_t) function cross(a, b) ! vector cross product a x b
    type(vec3_t), intent(in) :: a, b

    cross%x = (a%y*b%z - a%z*b%y)
    cross%y = (a%z*b%x - a%x*b%z)
    cross%z = (a%x*b%y - a%y*b%x)
  end function cross

  type(vec3_t) function add(a, b) ! addition
    type(vec3_t), intent(in) :: a, b

    add%x = a%x + b%x
    add%y = a%y + b%y
    add%z = a%z + b%z
  end function add

  type(vec3_t) function sub(a, b) ! subtraction
    type(vec3_t), intent(in) :: a, b

    sub%x = a%x - b%x
    sub%y = a%y - b%y
    sub%z = a%z - b%z
  end function sub

  type(vec3_t) function mul(a, b) ! scaling by b
    type(vec3_t), intent(in) :: a
    real(rk), intent(in) :: b

    mul%x = a%x * b
    mul%y = a%y * b
    mul%z = a%z * b
  end function mul

  type(vec3_t) function div(a, b) ! scaling by 1/b
    type(vec3_t), intent(in) :: a
    real(rk), intent(in) :: b

    div%x = a%x / b
    div%y = a%y / b
    div%z = a%z / b
  end function div

  type(vec3_t) function normalize(a) ! unit vector
    type(vec3_t), intent(in) :: a

    normalize%x = a%x / vlength(a)
    normalize%y = a%y / vlength(a)
    normalize%z = a%z / vlength(a)
  end function normalize

  real(rk) function vlength(a) ! vector length |a|
    type(vec3_t), intent(in) :: a

    vlength = sqrt(a%x**2 + a%y**2 + a%z**2)
  end function vlength

  real(rk) function vcos(a, b) ! cosine (a . b) / (|a| |b|)
    type(vec3_t), intent(in) :: a, b

    vcos = (dot(a, b)) / (vlength(a) * vlength(b))
  end function vcos
end module vec3
