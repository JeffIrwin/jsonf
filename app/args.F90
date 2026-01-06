
module jsonf__args

	use jsonf__utils
	use jsonf
	implicit none

	type args_t

		logical :: &
			help            = .false., &
			version         = .false., &
			lint            = .false., &
			compact         = .false., &
			error_dup       = .false., &
			first_duplicate = .false., &
			warn_no_commas  = .false., &
			warn_commas     = .false., &
			error_commas    = .false., &
			warn_numbers    = .false., &
			error_numbers   = .false., &
			quiet           = .false., &
			tokens          = .false., &
			has_pointer     = .false., &
			has_str         = .false., &
			has_filename    = .false.

		character(len=:), allocatable :: &
			filename, &
			pointer, &
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
	!print *, "arg "//to_str(i)//" = <", get_arg, ">"

end function get_arg

!===============================================================================

function parse_args() result(args)
	type(args_t) :: args
	!********
	character(len=:), allocatable :: arg
	integer :: i, nargs, ipos
	logical :: error = .false.

	! Set defaults, if any
	! [none]

	! Parse the args
	i = 0
	ipos = 0
	nargs = command_argument_count()
	do while (i < nargs)
		i = i + 1
		arg = get_arg(i)
		!print *, "arg = ", arg

		select case (arg)
		case ("-h", "-help", "--help")
			args%help = .true.

		case ("--version")
			args%version = .true.

		case ("-l", "--lint")
			args%lint = .true.

		case ("-t", "--tokens")
			args%tokens = .true.

		case ("-q", "--quiet")
			args%quiet = .true.

		case ("-c", "--compact")
			args%compact = .true.

		! gcc-style warning/error options
		case ("-Wno-commas")
			args%warn_no_commas = .true.
		case ("-Wcommas")
			args%warn_commas = .true.
		case ("-Werror=commas")
			args%error_commas = .true.

		case ("-Wnumbers")
			args%warn_numbers = .true.
		case ("-Werror=numbers")
			args%error_numbers = .true.

		! TODO: add dup warning option. Maybe rename these flag(s) in gcc style?
		case ("-d", "--no-dup", "--no-duplicate-keys")
			args%error_dup = .true.

		case ("-f", "--first-dup", "--first-duplicate")
			args%first_duplicate = .true.

		case ("-p", "--pointer")
			i = i + 1
			args%has_pointer = .true.
			args%pointer = get_arg(i)

		case ("-s", "--str", "--string")
			! Beware, fpm removes quotes from *inside* of cmd args, e.g.
			!
			!     '{"a": 69, "x": 420}'  ->  {a: 69, x: 420}
			!
			! Of course you can install the exe directly and run it outside of
			! fpm and then there's no problem:
			!
			!     fpm install --prefix . && ./bin/jsonf -s '{"a123": 69, "x456": 420}'
			!
			! Another option is to escape the quotes in the fpm arg, but this is
			! inconsistent with escape rules outside of fpm:
			!
			!     fpm run -- -s '{\"a\": 69, \"b\": 420}'
			!
			i = i + 1
			args%has_str = .true.
			args%str = get_arg(i)
			!print *, "input string = ", args%str

		case default

			if (arg(1:1) == "-") then
				error = .true.
				write(*,*) ERROR_STR//'bad command argument "'//arg//'"'
				cycle
			end if

			ipos = ipos + 1
			select case (ipos)
			case (1)
				args%has_filename = .true.
				args%filename = arg
			case default
				error = .true.
				write(*,*) ERROR_STR//'bad positional command argument "'//arg//'"'
			end select

		end select

	end do
	if (.not. args%has_filename .and. .not. args%has_str &
		.and. .not. (args%help .or. args%version)) then

		error =.true.
		write(*,*) ERROR_STR//"no input JSON filename or string given"

	else if (args%has_filename .and. args%has_str) then
		error =.true.
		write(*,*) ERROR_STR//"both JSON filename and string given"
	end if

	!********

	if (error .or. args%help .or. args%version) then

		write(*, "(a)") fg_bright_magenta//"jsonf "//get_jsonf_vers()//color_reset
		if (args%version) call jsonf_exit(EXIT_SUCCESS, quiet = .true.)
		! Could add compiler details, build date, etc. like syntran

		write(*,*) fg_bold//"Usage:"//color_reset
		write(*,*) "    jsonf -h | --help"
		write(*,*) "    jsonf FILE.json [(-p|--pointer) POINTER]"
		write(*,*) "    jsonf (-s|--string) STRING [(-p|--pointer) POINTER]"
		write(*,*) "    jsonf -c|--compact"
		write(*,*) "    jsonf -d|--no-dup"
		write(*,*) "    jsonf -f|--first-dup"
		write(*,*) "    jsonf -l|--lint"
		write(*,*) "    jsonf -q|--quiet"
		write(*,*) "    jsonf -t|--tokens"
		write(*,*) "    jsonf --version"
		! TODO: document -Werror=commas, -Wcommas, etc.  This is getting long.
		! Maybe short help and long help options
		write(*,*)
		write(*,*) fg_bold//"Options:"//color_reset
		write(*,*) "    --help       Show this help"
		write(*,*) "    FILE.json    Input JSON filename"
		write(*,*) "    --string     Input JSON string"
		write(*,*) "    --pointer    JSON pointer path"
		write(*,*) "    --lint       Check JSON for syntax errors"
		write(*,*) "    --compact    Format compactly without whitespace"
		write(*,*) "    --first-dup  Keep first duplicate key, default last"
		write(*,*) "    --no-dup     Do not allow duplicate keys"
		write(*,*) "    --quiet      Decrease log verbosity"
		write(*,*) "    --tokens     Dump tokens without parsing JSON"
		write(*,*) "    --version    Show the jsonf version number"
		write(*,*)

		if (error) call panic("")
		call jsonf_exit(EXIT_SUCCESS)
	end if

end function parse_args

!===============================================================================

end module jsonf__args

