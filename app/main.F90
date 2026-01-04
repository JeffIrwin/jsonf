
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
	json%first_duplicate = args%first_duplicate

	json%error_duplicate_keys  = args%error_dup
	json%warn_trailing_commas  = args%warn_commas
	json%error_trailing_commas = args%error_commas

	if (args%lint) then
		json%warn_trailing_commas  = .not. args%warn_no_commas
	end if

end subroutine

subroutine app_echo_file(args)
	type(args_t), intent(in) :: args
	!********
	character(len=:), allocatable :: msg
	type(json_t) :: json
	type(json_val_t) :: val

	msg = ""
	if (.not. (args%quiet .or. args%lint)) then
		write(*, "(a)") "Reading JSON from file: "//quote(args%filename)
		msg = "JSON content from file:"
	end if
	call args_to_json_config(args, json)

	if (args%tokens) then
		call print_file_tokens(args%filename)
		return
	end if

	call json%read_file(args%filename)
	if (args%lint) return

	call json%print(msg)

	if (args%has_pointer) then
		! TODO: probably don't want to always print json above, may be useful for now
		! for debugging
		print *, "pointer = "//quote(args%pointer)
		print *, "len(pointer) = ", len(args%pointer)

		val = json%get_val(args%pointer)
		!call copy_val(val, json%get_val(args%pointer))

		!print *, "val = "//val%to_str()
		!print *, "done get_val()"
		!print *, "printing val ..."
		print *, "val = "//val_to_str(json, val)

	end if

end subroutine app_echo_file

subroutine app_echo_str(args)
	type(args_t), intent(in) :: args
	type(json_t) :: json
	character(len=:), allocatable :: msg

	msg = ""
	if (.not. (args%quiet .or. args%lint)) then
		write(*, "(a)") "Reading JSON from string:"//LINE_FEED//"<<<"//args%str//">>>"
		msg = "JSON content from string:"
	end if
	call args_to_json_config(args, json)

	if (args%tokens) then
		call print_str_tokens(args%str)
		return
	end if

	call json%read_str(args%str)
	if (args%lint) return

	call json%print(msg)
	!call json%write("junk.json")

	! TODO: handle args%pointer logic here like app_echo_file()

end subroutine app_echo_str

end module jsonf__app

program main

	use jsonf__app
	implicit none
	type(args_t) :: args

	args = parse_args()
	
	if (.not. (args%quiet .or. args%lint)) then
		write(*, "(a)") fg_bright_magenta//"jsonf "//get_jsonf_vers()//color_reset
	end if

	if (args%has_filename) then
		call app_echo_file(args)

	else if (args%has_str) then
		call app_echo_str(args)

	end if

	call jsonf_exit(EXIT_SUCCESS, args%quiet .or. args%lint)

end program main

