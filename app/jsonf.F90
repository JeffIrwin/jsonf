
module jsonf__app

	use args_m
	use utils_m
	use jsonf
	implicit none

contains

subroutine app_read_file(filename)
	character(len=*), intent(in) :: filename
	character(len=:), allocatable :: str
	!type(parser_t) :: parser
	type(json_t) :: json

	write(*,*) "Reading JSON from file: "//quote(filename)
	call json%read_file(filename)

end subroutine app_read_file

subroutine app_read_str(str)
	character(len=*), intent(in) :: str
	!type(parser_t) :: parser
	type(json_t) :: json

	write(*,*) "Reading JSON from string:"//LINE_FEED//"<<<"//str//">>>"
	call json%read_str(str)

end subroutine app_read_str

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
		call app_read_file(args%filename)
	else if (args%has_str) then
		!write(*,*) "Reading JSON from string: "//LINE_FEED//args%str
		call app_read_str(args%str)
	else
		! TODO: move this error to args. Log help msg
		call panic("no input JSON filename or string given")
	end if

	!write(*,*) "Finished jsonf"
	call jsonf_exit(EXIT_SUCCESS)

end program test

