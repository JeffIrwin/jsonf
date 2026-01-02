
module jsonf__app

	use args_m
	use utils_m
	use jsonf
	implicit none

contains

subroutine app_echo_file(filename)
	character(len=*), intent(in) :: filename
	character(len=:), allocatable :: str
	type(json_t) :: json
	character(len=:), allocatable :: out_str

	write(*,*) "Reading JSON from file: "//quote(filename)
	call json%read_file(filename)

	!! TODO
	!call json%print("JSON content from file:")
	out_str = json%to_str()
	write(*,*) "JSON content from file:"//LINE_FEED//out_str

end subroutine app_echo_file

subroutine app_echo_str(str)
	character(len=*), intent(in) :: str
	type(json_t) :: json
	character(len=:), allocatable :: out_str

	write(*,*) "Reading JSON from string:"//LINE_FEED//"<<<"//str//">>>"
	call json%read_str(str)

	!! TODO
	call json%print("JSON content from string:")
	!out_str = json%to_str()
	!write(*, '(a)') "JSON content from string:"
	!write(*, "(a)") " "//out_str
	!print *, out_str
	call json%write("junk.json")

end subroutine app_echo_str

end module jsonf__app

program test

	use jsonf__app
	implicit none
	type(args_t) :: args

	args = parse_args()
	
	! TODO: version logging. Turn this off unless verbose option is given
	write(*,*) fg_bright_magenta//"Starting jsonf"//color_reset

	if (args%has_filename) then
		!write(*,*) "Reading JSON from file: "//args%filename
		call app_echo_file(args%filename)
	else if (args%has_str) then
		!write(*,*) "Reading JSON from string: "//LINE_FEED//args%str
		call app_echo_str(args%str)
	else
		! TODO: move this error to args. Log help msg
		call panic("no input JSON filename or string given")
	end if

	!write(*,*) "Finished jsonf"
	call jsonf_exit(EXIT_SUCCESS)

end program test

