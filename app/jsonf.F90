
module jsonf__app

	use args_m
	use utils_m
	use jsonf
	implicit none

contains

subroutine app_echo_file(args)
	type(args_t), intent(in) :: args
	type(json_t) :: json

	write(*,*) "Reading JSON from file: "//quote(args%filename)
	json%compact = args%compact
	call json%read_file(args%filename)
	call json%print("JSON content from file:")

end subroutine app_echo_file

subroutine app_echo_str(args)
	type(args_t), intent(in) :: args
	type(json_t) :: json

	write(*,*) "Reading JSON from string:"//LINE_FEED//"<<<"//args%str//">>>"
	json%compact = args%compact
	call json%read_str(args%str)
	call json%print("JSON content from string:")
	!call json%write("junk.json")

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
		call app_echo_file(args)

	else if (args%has_str) then
		call app_echo_str(args)

	else
		! TODO: move this error to args. Log help msg
		call panic("no input JSON filename or string given")
	end if

	call jsonf_exit(EXIT_SUCCESS)

end program test

