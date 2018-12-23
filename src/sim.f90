! sim - planetary motion simulator
! usage: sim <input file> <output file>
! simulation parameters are defined in the input
! file (see documentation) and object positions
! are written to the output file. The output can
! also be piped directly to the included render program.

program main ! main program
  use sys

  implicit none

  integer :: i

  character(80) :: inpath = ""
  character(80) :: outpath = ""

  ! get paths from arguments
  call get_command_argument(1, inpath)
  call get_command_argument(2, outpath)

  if (inpath == "") then
     print *, "Specify input file!"
     return
  end if

  call s_load(inpath) ! read input file
  call s_init() ! intialize objects in sys

  if (mode == 0) then
     if (outpath == "" .or. outpath == inpath) then
        print *, "Check output file!"
        return
     end if
     ! output file
     open(8, file = outpath, action = "write")

     print *, "Simulating..."
  end if

  s = 0
  do while (s < n) ! main simulation loop
     if (mod(s, pm) == 0 .and. mode == 0) then
        call pinfo() ! print sim info
     end if

     call s_tick() ! tick system

     if (mod(s, wk) == 0) then
        do i = 1, objnum, 1
           if (mode == 1) then
              ! print to stdout
              write(*, *) i, t, obj(i)%x%x, obj(i)%x%y, obj(i)%x%z
           else
              ! write to file
              write(8, *) i, t, obj(i)%x%x, obj(i)%x%y, obj(i)%x%z
           end if
        end do
     end if

     s = s + 1
  end do

  if (mode == 0) then
     print *, "Done!"
     close(8)
  end if
contains
  subroutine pinfo()
    print *, "Progress: ", int((real(s) / real(n)) * 100.0, 1), "%"
    call s_pinfo()
  end subroutine pinfo
end program main
