
module jsonf__app

	use jsonf__args
	use jsonf__utils
	use jsonf
	implicit none

contains

subroutine args_to_json_config(args, json)
	type(args_t), intent(in) :: args
	type(json_t), intent(inout) :: json
	json%compact = args%compact
	json%allow_duplicate_keys = .not. args%no_dup  ! maybe rethink logical inversion here
	json%first_duplicate = args%first_duplicate
end subroutine

subroutine app_echo_file(args)
	type(args_t), intent(in) :: args
	type(json_t) :: json
	character(len=:), allocatable :: msg

	msg = ""
	if (.not. args%quiet) then
		write(*, "(a)") "Reading JSON from file: "//quote(args%filename)
		msg = "JSON content from file:"
	end if
	call args_to_json_config(args, json)

	if (args%tokens) then
		call print_file_tokens(args%filename)
		return
	end if

	call json%read_file(args%filename)
	call json%print(msg)

end subroutine app_echo_file

subroutine app_echo_str(args)
	type(args_t), intent(in) :: args
	type(json_t) :: json
	character(len=:), allocatable :: msg

	msg = ""
	if (.not. args%quiet) then
		write(*, "(a)") "Reading JSON from string:"//LINE_FEED//"<<<"//args%str//">>>"
		msg = "JSON content from string:"
	end if
	call args_to_json_config(args, json)

	if (args%tokens) then
		call print_str_tokens(args%str)
		return
	end if

	call json%read_str(args%str)
	call json%print(msg)
	!call json%write("junk.json")

end subroutine app_echo_str

end module jsonf__app

program main

	use jsonf__app
	implicit none
	type(args_t) :: args

	args = parse_args()
	
	if (.not. args%quiet) then
		write(*, "(a)") fg_bright_magenta//"jsonf "//get_jsonf_vers()//color_reset
	end if

	if (args%has_filename) then
		call app_echo_file(args)

	else if (args%has_str) then
		call app_echo_str(args)

	end if

	call jsonf_exit(EXIT_SUCCESS, args%quiet)

end program main

