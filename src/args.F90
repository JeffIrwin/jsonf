module args_m

	use utils_m
	implicit none

	type args_t

		logical :: &
			test               = .false., &  ! use test input file? otherwise full input
			assert             = .false., &  ! assert correctness of results?
			part1              = .false., &  ! run part 1 only? default both parts
			part2              = .false., &
			help               = .false., &
			has_input_filename = .false.

		character(len=:), allocatable :: input_filename

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
	!print *, "arg "//to_str(i)//" = ", get_arg

end function get_arg

!===============================================================================

function parse_args() result(args)
	type(args_t) :: args
	!********
	character(len=:), allocatable :: arg
	integer :: i, nargs, ipos
	logical :: error = .false.
	type(str_vec_t) :: argv

	! Set defaults
	args%input_filename = "input.txt"

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

		case ("-t", "--test")
			args%test = .true.
			if (.not. args%has_input_filename) then
				args%input_filename = "test-input.txt"
			end if

		case ("-a", "--assert")
			args%assert = .true.

		case ("-1", "--part1")
			args%part1 = .true.

		case ("-2", "--part2")
			args%part2 = .true.

		case ("-i", "--input")
			i = i + 1
			args%has_input_filename = .true.
			args%input_filename = argv%vec(i)%str
			!print *, "input filename = ", args%input_filename

		case default
			! run.sh passes args to make as well as this program. don't
			! error-out because we can't understand make's args
			write(*,*) WARN_STR//'bad command argument "'//arg//'"'

		end select

	end do

	!********

	if (error .or. args%help) then

		write(*,*) fg_bold//"Usage:"//color_reset
		write(*,*) "    main -h | --help"
		write(*,*) "    main [-1 | --part1] [-2 | --part2]"
		write(*,*) "    main [-t | --test] [(-i|--input) file.txt]"
		write(*,*) "    main -a | --assert"
		write(*,*)
		write(*,*) fg_bold//"Options:"//color_reset
		write(*,*) "    --help        Show this help"
		write(*,*) "    --part1       Run part 1 only. Default both parts"
		write(*,*) "    --part2       Run part 2 only"
		write(*,*) "    --test        Use test-input.txt instead of input.txt"
		write(*,*) "    --input       Input data filename"
		write(*,*) "    --assert      Abort if results do not match expected answers"
		write(*,*)

		if (error) call panic("")
		call aoc_exit(EXIT_SUCCESS)
	end if

end function parse_args

!===============================================================================

end module args_m

