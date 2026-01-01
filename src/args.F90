module args_m

	use utils_m
	implicit none

	type args_t

		logical :: &
			assert       = .false., &  ! assert stop on error?
			help         = .false., &
			has_str      = .false., &
			has_filename = .false.

		character(len=:), allocatable :: &
			filename, &
			str

	end type args_t

contains

!===============================================================================

function get_arg(i)
	integer, intent(in) :: i
	character(len=:), allocatable :: get_arg
	!********
	integer :: len_, io

	call get_command_argument(i, length = len_, status = io)
	if (io /= 0) then
		call panic("can't get length of command argument index "//to_str(i))
	end if
	!print *, "arg "//to_str(i)//" len_ = ", len_

	allocate(character(len = len_) :: get_arg)

	call get_command_argument(i, value = get_arg, status = io)
	if (io /= 0) then
		call panic("can't get value of command argument index "//to_str(i))
	end if
	print *, "arg "//to_str(i)//" = <", get_arg, ">"

end function get_arg

!===============================================================================

function parse_args() result(args)
	type(args_t) :: args
	!********
	character(len=:), allocatable :: arg
	integer :: i, nargs, ipos
	logical :: error = .false.
	type(str_vec_t) :: argv

	! Set defaults, if any
	! [none]

	! Get the cmd args as a vector of strings
	nargs = command_argument_count()
	argv = new_str_vec()
	do i = 1, nargs
		call argv%push(get_arg(i))
	end do
	!call print_str_vec("argv = ", argv)

	! Parse the args
	i = 0
	ipos = 0
	do while (i < nargs)
		i = i + 1
		arg = argv%vec(i)%str
		!print *, "arg = ", arg

		select case (arg)
		case ("-h", "-help", "--help")
			args%help = .true.

		case ("-a", "--assert")
			! TODO: do something with this. json-fortran has a "stop-on-error"
			! option which I would like to implement, default false and return
			! error codes but continue
			args%assert = .true.

		case ("-s", "--str", "--string")
			! TODO: fpm removes quotes from *inside* of cmd args, e.g.
			!
			!     '{"a": 69, "x": 420}'  ->  {a: 69, x: 420}
			!
			! Not sure if there's a good reason for it or if it's actually a
			! bug.  Of course you can install the exe directly and run it
			! outside of fpm and then there's no problem:
			!
			!     fpm install --prefix . && ./bin/jsonf -s '{"a123": 69, "x456": 420}'
			!
			i = i + 1
			args%has_str = .true.
			args%str = argv%vec(i)%str
			print *, "input string = ", args%str

		case default

			ipos = ipos + 1
			select case (ipos)
			case (1)
				args%has_filename = .true.
				args%filename = arg
			case default
				error = .true.
				write(*,*) ERROR_STR//'bad command argument "'//arg//'"'
			end select

			!! run.sh passes args to make as well as this program. don't
			!! error-out because we can't understand make's args
			!write(*,*) WARN_STR//'bad command argument "'//arg//'"'

		end select

	end do
	! TODO: error if filename *and* string are both given

	!********

	if (error .or. args%help) then

		write(*,*) fg_bold//"Usage:"//color_reset
		write(*,*) "    jsonf -h | --help"
		write(*,*) "    jsonf FILE.json"
		write(*,*) "    jsonf (-s | --string) STRING"
		write(*,*) "    jsonf -a | --assert"
		write(*,*)
		write(*,*) fg_bold//"Options:"//color_reset
		write(*,*) "    --help        Show this help"
		write(*,*) "    FILE.json     Input JSON filename"
		write(*,*) "    --string      Input JSON string"
		write(*,*) "    --assert      Abort if results do not match expected answers"
		write(*,*)

		if (error) call panic("")
		call jsonf_exit(EXIT_SUCCESS)
	end if

end function parse_args

!===============================================================================

end module args_m

