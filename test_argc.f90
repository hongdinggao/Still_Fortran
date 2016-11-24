PROGRAM test_get_command_argument
implicit none

integer :: i=0
character(len=32) :: arg

do
   CALL get_command_argument(i, arg)
   IF (LEN_TRIM(arg) == 0) EXIT
   WRITE (*,*) TRIM(arg)
   i = i+1
end do
END PROGRAM test_get_command_argument
