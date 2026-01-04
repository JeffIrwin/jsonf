
module jsonf__utils

	use iso_fortran_env
	implicit none

	double precision, parameter :: PI = 4.d0 * atan(1.d0)

	integer, parameter :: &
		EXIT_FAILURE = 1, &
		EXIT_SUCCESS = 0

	character, parameter :: &
			NULL_CHAR       = char( 0), &
			TAB             = char( 9), &
			LINE_FEED       = char(10), &
			VERT_TAB        = char(11), &
			CARRIAGE_RETURN = char(13), &
			ESC             = char(27)

	! TODO: make these variables, with colors disabled if output_unit is not tty
	! and an option to --force-color
	character(len = *), parameter :: &
			fg_bold               = ESC//"[;1m", &
			fg_yellow             = ESC//"[33m", &
			fg_bold_yellow        = ESC//"[33;1m", &
			fg_bright_red         = ESC//"[91m", &
			fg_bold_bright_red    = ESC//"[91;1m", &
			fg_bold_bright_yellow = ESC//"[93;1m", &
			fg_green              = ESC//"[32m", &
			fg_bright_green       = ESC//"[92m", &
			fg_bright_yellow      = ESC//"[93m", &
			fg_bright_blue        = ESC//"[94m", &
			fg_bright_magenta     = ESC//"[95m", &
			fg_bright_cyan        = ESC//"[96m", &
			fg_bright_white       = ESC//"[97m", &
			color_reset           = ESC//"[0m"

	character(len = *), parameter :: &
		ERROR_STR = fg_bold_bright_red   //"Error"  //fg_bold//": "//color_reset, &
		WARN_STR  = fg_bold_yellow//"Warning"//fg_bold//": "//color_reset

	!********

	interface to_str
		procedure :: i32_to_str
		procedure :: i64_to_str
	end interface to_str

	!********

	type str_t
		character(len = :), allocatable :: str
	end type str_t

	!********

	type str_builder_t
		! This is basically a dynamic char vector, but the type is a str and not
		! an actual array of single chars
		character(len = :), allocatable :: str
		integer(kind = 8) :: len, cap
		contains
			procedure :: &
				push => push_str_builder, &
				trim => trim_str_builder
	end type str_builder_t

	!********

	type i64_vec_t
		integer(kind=8), allocatable :: vec(:)
		integer(kind = 8) :: len, cap
		contains
			procedure :: push => push_i64
	end type i64_vec_t

	type str_vec_t
		type(str_t), allocatable :: vec(:)
		integer(kind = 8) :: len, cap
		contains
			procedure :: push => push_str
	end type str_vec_t

	!********

contains

!===============================================================================

function new_str_builder() result(sb)
	type(str_builder_t) :: sb
	sb%len = 0
	sb%cap = 16
	allocate(character(len = sb%cap) :: sb%str)
end function new_str_builder

!===============================================================================

function new_i64_vec() result(vec)
	type(i64_vec_t) :: vec
	vec%len = 0
	vec%cap = 2
	allocate(vec%vec( vec%cap ))
end function new_i64_vec

function new_str_vec() result(vec)
	type(str_vec_t) :: vec
	vec%len = 0
	vec%cap = 2
	allocate(vec%vec( vec%cap ))
end function new_str_vec

!===============================================================================

subroutine print_str_vec(msg, sv)
	character(len = *), intent(in) :: msg
	type(str_vec_t) :: sv
	!********
	integer(kind = 8) :: i
	integer, parameter :: unit_ = output_unit

	write(unit_, "(a)") " "//msg
	write(unit_, *) "["
	do i = 1, sv%len
		write(unit_, "(a)") '     "'//sv%vec(i)%str//'",'
	end do
	write(unit_, *) "]"
end subroutine print_str_vec

!===============================================================================

function trim_str_builder(sb) result(str)
	class(str_builder_t), intent(in) :: sb
	character(len = :), allocatable :: str
	str = sb%str(1: sb%len)
end function trim_str_builder

!===============================================================================

subroutine push_str_builder(sb, val)
	! Push onto a str builder
	class(str_builder_t), intent(inout) :: sb
	character(len=*), intent(in) :: val
	!********
	character(len = :), allocatable :: tmp
	integer(kind = 8) :: tmp_cap

	!print *, "pushing """//val//""""

	sb%len = sb%len + len(val)
	if (sb%len > sb%cap) then
		! Grow the buffer capacity
		tmp_cap = 2 * sb%len
		allocate(character(len = tmp_cap) :: tmp)
		tmp(1: sb%cap) = sb%str

		call move_alloc(tmp, sb%str)
		sb%cap = tmp_cap
	end if
	sb%str(sb%len - len(val) + 1: sb%len) = val

end subroutine push_str_builder

!===============================================================================

subroutine push_i64(vec, val)
	class(i64_vec_t), intent(inout) :: vec
	integer(kind=8), intent(in) :: val
	!********
	!type(str_t) :: val_str
	integer(kind=8), allocatable :: tmp(:)
	integer(kind=8) :: tmp_cap

	!print *, "pushing """//val//""""
	vec%len = vec%len + 1
	if (vec%len > vec%cap) then
		tmp_cap = 2 * vec%len
		allocate(tmp( tmp_cap ))
		tmp(1: vec%cap) = vec%vec

		call move_alloc(tmp, vec%vec)
		vec%cap = tmp_cap
	end if

	!val_str%str = val
	!vec%vec( vec%len ) = val_str
	vec%vec( vec%len ) = val

end subroutine push_i64

!===============================================================================

subroutine push_str(vec, val)
	class(str_vec_t), intent(inout) :: vec
	character(len = *), intent(in) :: val
	!********
	type(str_t) :: val_str
	type(str_t), allocatable :: tmp(:)
	integer(kind = 8) :: tmp_cap

	!print *, "pushing """//val//""""
	vec%len = vec%len + 1
	if (vec%len > vec%cap) then
		tmp_cap = 2 * vec%len
		allocate(tmp( tmp_cap ))
		tmp(1: vec%cap) = vec%vec

		call move_alloc(tmp, vec%vec)
		vec%cap = tmp_cap
	end if

	val_str%str = val
	vec%vec( vec%len ) = val_str

end subroutine push_str

!===============================================================================

function count_lines(filename) result(nline)
	character(len=*), intent(in) :: filename
	integer :: nline
	!********
	character :: c
	integer :: iu, io

	nline = 0
	open(newunit = iu, file = filename, action = "read")
	do
		read(iu, "(a)", iostat = io) c
		! TODO: test on file with blank lines. May need to check io /= IOSTAT_END
		if (io /= EXIT_SUCCESS) exit
		nline = nline + 1
	end do
	close(iu)

end function count_lines

!===============================================================================

function read_mat_char(filename) result(mat)
	character(len=*), intent(in) :: filename
	character, allocatable :: mat(:,:)
	!********
	integer :: nx, ny, iu, x, y
	character(len=:), allocatable :: line

	ny = count_lines(filename)
	open(newunit = iu, file = filename, action = "read")
	line = read_line(iu)
	nx = len(line)
	!print *, "nx = ", nx

	allocate(mat(nx, ny))
	!mat = "."
	do y = 1, ny
		do x = 1, nx
			mat(x,y) = line(x:x)
		end do

		line = read_line(iu)
	end do
	!print *, "mat = ", mat
	close(iu)

end function read_mat_char

!===============================================================================

function read_file(file, iostat) result(str)
	! Read all lines of a file into str
	character(len = *), intent(in) :: file
	integer, optional, intent(out) :: iostat
	character(len = :), allocatable :: str
	!********
	character :: c
	integer :: io, iu
	type(str_builder_t) :: sb  ! string builder

	! Open as binary to simplify newline handling
	open(file = file, newunit = iu, action = "read", access = "stream", iostat = io)
	if (io /= exit_success) then
		if (present(iostat)) iostat = io
		return
	end if

	! Read 1 character at a time until end
	io = EXIT_SUCCESS
	sb = new_str_builder()
	do
		read(iu, iostat = io) c
		if (io == IOSTAT_END) exit
		call sb%push(c)
	end do
	if (io == IOSTAT_END) io = EXIT_SUCCESS
	close(iu)
	str = sb%trim()

	!print *, "io = ", io
	!print *, 'str = '
	!print *, str

	if (present(iostat)) iostat = io

end function read_file

!===============================================================================

function read_line(iu, iostat) result(str)

	! c.f. aoc-2022/utils.f90 and syntran/src/utils.f90
	!
	! This version reads WITHOUT backspacing, so it works on stdin too

	integer, intent(in) :: iu
	integer, optional, intent(out) :: iostat

	character(len = :), allocatable :: str

	!********

	character :: c

	integer :: io

	type(str_builder_t) :: sb

	!print *, 'starting read_line()'

	! Read 1 character at a time until end
	sb = new_str_builder()
	do
		read(iu, '(a)', advance = 'no', iostat = io) c
		!print *, "io = ", io
		!print *, "c = """, c, """"

		if (io == IOSTAT_END) exit
		if (io == IOSTAT_EOR) exit

		! In syntran, calling readln() one more time after the initial EOF
		! causes an infinite loop for some reason without this
		if (io /= 0) exit

		!if (c == CARRIAGE_RETURN) exit
		!if (c == LINE_FEED) exit

		call sb%push(c)

	end do
	str = sb%trim()

	!print *, "sb  = ", sb%str( 1: sb%len )
	!print *, "str = ", str

	!if (io == IOSTAT_END .or. io == IOSTAT_EOR) io = 0
	if (io == IOSTAT_EOR) io = 0
	if (present(iostat)) iostat = io

end function read_line

!===============================================================================

function split(str, delims) result(strs)

	! This was translated from aoc-syntran and there are lots of off-by-one
	! differences going from syntran to fortran

	character(len = *), intent(in) :: str
	character(len = *), intent(in) :: delims
	type(str_vec_t) :: strs
	!********
	integer :: i, i0, n

	strs = new_str_vec()

	n = len(str)
	if (n == 0) return

	i = 1
	do while (i <= n)

		i0 = verify(str(i:n), delims) + i - 1
		if (i0 < i) i0 = n + 1

		i  = scan(str(i0:n), delims) + i0 - 1
		if (i < i0) i = n + 1

		if (i0 < i) call strs%push(str(i0: i - 1))
	end do

end function split

!===============================================================================
subroutine unit_test_split()
	character(len = :), allocatable :: str
	integer(kind = 8) :: i
	type(str_vec_t) :: strs
	str = "0,12,23,34,,7,,,,45,56,,1"
	strs = split(str, ",")
	!print *, "strs = ", strs%vec(:)%str
	!print *, "strs = ", [(strs%vec(i)%str, i = 1, strs%len)]
	print *, "strs = "//LINE_FEED//"["
	print "(a)", [(TAB//'"'//strs%vec(i)%str//'",', i = 1, strs%len)]
	print "(a)", "]"
	call exit(0)
end subroutine unit_test_split
!===============================================================================

function i32_to_str(int_) result(str)
	integer(kind = 4), intent(in) :: int_
	character(len = :), allocatable :: str
	character :: buffer*16
	write(buffer, "(i0)") int_
	str = trim(buffer)
end function i32_to_str

!===============================================================================

function i64_to_str(int_) result(str)
	integer(kind = 8), intent(in) :: int_
	character(len = :), allocatable :: str
	character :: buffer*16
	write(buffer, "(i0)") int_
	str = trim(buffer)
end function i64_to_str

!===============================================================================

integer function count_str_match(str, char_) result(n)
	! Count the number `n` of characters `char_` in string `str`
	character(len = *), intent(in) :: str
	character, intent(in) :: char_
	!********
	integer :: i
	n = 0
	do i = 1, len(str)
		if (str(i:i) == char_) n = n + 1
	end do
end function count_str_match

!===============================================================================

subroutine print_mat_f32(msg, a)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	!
	! TODO: optional output file unit arg
	character(len = *), intent(in) :: msg
	real, intent(in) :: a(:,:)
	!********
	integer :: i, j, m, n
	integer, parameter :: unit_ = output_unit
	m = size(a,1)
	n = size(a,2)
	write(unit_, "(a)") " " // msg
	do i = 1, m
		do j = 1, n
			write(unit_, "(es11.3)", advance = "no") a(i,j)
		end do
		write(unit_, *)
	end do

end subroutine print_mat_f32

!===============================================================================

subroutine print_mat_bool(msg, a)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	!
	! TODO: optional output file unit arg
	character(len = *), intent(in) :: msg
	logical, intent(in) :: a(:,:)
	!********
	integer :: i, j, m, n
	integer, parameter :: unit_ = output_unit
	m = size(a,1)
	n = size(a,2)
	write(unit_, "(a)") " " // msg
	do i = 1, m
		do j = 1, n
			write(unit_, "(l)", advance = "no") a(i,j)
		end do
		write(unit_, *)
	end do

end subroutine print_mat_bool

!===============================================================================

subroutine print_mat_i32(msg, a)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	!
	! TODO: optional output file unit arg
	character(len = *), intent(in) :: msg
	integer, intent(in) :: a(:,:)
	!********
	integer :: i, j, m, n
	integer, parameter :: unit_ = output_unit
	m = size(a,1)
	n = size(a,2)
	write(unit_, "(a)") " " // msg
	do i = 1, m
		do j = 1, n
			write(unit_, "(i6)", advance = "no") a(i,j)
		end do
		write(unit_, *)
	end do

end subroutine print_mat_i32

!===============================================================================

subroutine print_mat_i64(msg, a, width)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	!
	! TODO: optional int width arg, optional output file unit arg
	character(len = *), intent(in) :: msg
	integer(kind=8), intent(in) :: a(:,:)
	integer, optional, intent(in) :: width
	!********
	integer :: i, j, m, n, width_
	integer, parameter :: unit_ = output_unit

	! This could make two passes over the array to get the max width required,
	! or even max width per-column
	width_ = 6
	if (present(width)) width_ = width

	m = size(a,1)
	n = size(a,2)
	write(unit_, "(a)") " " // msg
	do i = 1, m
		do j = 1, n
			write(unit_, "(i"//to_str(width_)//")", advance = "no") a(i,j)
		end do
		write(unit_, *)
	end do

end subroutine print_mat_i64

!===============================================================================

subroutine print_mat_char(msg, a, transpose_)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	!
	! TODO: optional output file unit arg
	character(len = *), intent(in) :: msg
	character, intent(in) :: a(:,:)
	logical, optional, intent(in) :: transpose_
	!********
	integer :: i, j, m, n
	integer, parameter :: unit_ = output_unit
	logical :: transpose__
	transpose__ = .false.
	if (present(transpose_)) transpose__ = transpose_
	m = size(a,1)
	n = size(a,2)
	write(unit_, "(a)") " " // msg

	if (transpose__) then
		do i = 1, m
			! TODO: unroll inner loops for all print_mat*() fns
			do j = 1, n
				write(unit_, "(a)", advance = "no") a(i,j)
			end do
			write(unit_, *)
		end do
		return
	end if

	do j = 1, n
		do i = 1, m
			write(unit_, "(a)", advance = "no") a(i,j)
		end do
		write(unit_, *)
	end do

end subroutine print_mat_char

!===============================================================================

function mat_char_to_str(a, delim) result(str)
	! Convert a character matrix to a single string with a given delimiter
	! between lines
	character, intent(in) :: a(:,:), delim
	character(len=:), allocatable :: str
	!********
	integer :: i, j, m, n
	m = size(a,1)
	n = size(a,2)

	str = "" ! TODO: pre-allocate
	do j = 1, n
		do i = 1, m
			str = str // a(i,j)
		end do
		str = str // delim
	end do

end function mat_char_to_str

!===============================================================================

subroutine panic(msg)
	character(len = *), intent(in) :: msg
	! Write to ERROR_UNIT.  If a printing routine panics, it would otherwise
	! hang on recursive stdout
	if (msg /= "") write(ERROR_UNIT, "(a)") ERROR_STR//msg
	call jsonf_exit(EXIT_FAILURE)
end subroutine panic

!===============================================================================

subroutine jsonf_exit(exit_code, quiet)
	integer, intent(in) :: exit_code
	logical, optional, intent(in) :: quiet
	logical :: quiet_
	quiet_ = .false.
	if (present(quiet)) quiet_ = quiet
	if (exit_code == EXIT_SUCCESS .and. .not. quiet_) then
		write(*, "(a)") fg_green//"Finished jsonf"//color_reset
	end if
	call exit(exit_code)
end subroutine jsonf_exit

!===============================================================================

logical function is_str_eq(a, b)
	! Fortran considers spaces as insignificant in str comparisons, but no sane
	! language would allow that
	!
	! I guess this is an artifact of fixed-length strings being common in older
	! fortran code
	character(len = *), intent(in) :: a, b
	!is_str_eq = a == b  ! not what you expect!

	is_str_eq = &
		len(a) == len(b) .and. &
		    a  ==     b
end function is_str_eq

!===============================================================================

function read_i32(str) result(a)
	character(len=*), intent(in) :: str
	integer(kind=4) :: a
	read(str, *) a
end function read_i32

!===============================================================================

function read_i32_delims(str, delims) result(v)
	character(len=*), intent(in) :: str, delims
	integer, allocatable :: v(:)
	!********
	integer(kind=8) :: i, n
	type(str_vec_t) :: strs

	strs = split(str, delims)
	n = strs%len
	allocate(v(n))
	do i = 1, n
		v(i) = read_i32(strs%vec(i)%str)
	end do

end function read_i32_delims

!===============================================================================

function i32(i)
	! Cast i64 down to i32
	integer(kind=8), intent(in) :: i
	integer(kind=4) :: i32
	i32 = int(i, 4)
end function i32

!===============================================================================

function rm_char(str, char)
	character(len=*), intent(in) :: str
	character, intent(in) :: char
	character(len=:), allocatable :: rm_char
	!********
	integer :: i, j
	rm_char = str  ! over-allocate
	j = 0
	do i = 1, len(str)
		if (str(i:i) == char) cycle
		j = j + 1
		rm_char(j:j) = str(i:i)
	end do
	rm_char = rm_char(1:j)

end function rm_char

!===============================================================================

logical function ends_with(str, substr)
	character(len=*), intent(in) :: str, substr
	!print *, "ending = ", str(len(str) - len(substr) + 1:)
	ends_with = is_str_eq(substr, str(len(str) - len(substr) + 1:))
end function ends_with

!===============================================================================

function get_base_with_ext(filename) result(basename)
	! Get basename plus extension
	character(len = *), intent(in)  :: filename
	character(len = :), allocatable :: basename
	!********
	integer :: beg_, end_, i

	beg_ = 1
	end_ = len(filename)

	!print *, 'len = ', end_

	i = scan(filename, '/\', .true.)
	if (i /= 0) beg_ = i + 1

	basename = filename(beg_: end_)
	!print *, 'beg_, end_ = ', beg_, end_

end function get_base_with_ext

!===============================================================================

function get_dir(filename) result(dir)
	character(len = *), intent(in)  :: filename
	character(len = :), allocatable :: dir
	!********
	character(len = :), allocatable :: path
	integer :: beg_, end_, i

	!! Return the absolute path dir
	!path = fullpath(filename)

	! Return relative path or absolute, whichever way input filename is given
	path = filename

	beg_ = 1
	!end_ = len(path)
	end_ = 0

	!print *, 'len = ', end_

	i = scan(path, '/\', .true.)
	if (i /= 0) end_ = i

	dir = path(beg_: end_)
	!print *, 'beg_, end_ = ', beg_, end_

end function get_dir

!===============================================================================

logical function is_digit(c)
	character, intent(in) :: c
	is_digit = '0' <= c .and. c <= '9'
end function is_digit

!===============================================================================

logical function is_digit_under(c)
	character, intent(in) :: c
	is_digit_under = is_digit(c) .or. c == "_"
end function is_digit_under

!===============================================================================

logical function is_float(c)
	character, intent(in) :: c

	! Correctly tokenizing a float is actually tricky.  We can't just greedily
	! gobble up all the characters that match is_float().  We need to tokenize
	! this as a float:
	!
	!     1.234e+1
	!
	! But tokenize this as a binary expression adding two ints:
	!
	!     1+234
	!
	! The + or - can only appear immediately after d or e.  To complicate
	! matters, there could also be a variable identifier named "e".
	!
	! To correctly tokenize floats, the lexer uses is_float(), in conjunction
	! with is_sign() and is_expo() to ensure that sign characters within a float
	! token ONLY occur immediately after an exponent character. Note that sign
	! characters before a number are tokenized as a separate unary operator, not
	! as part of the number token.

	is_float = is_digit(c) .or. is_sign(c) .or. is_expo(c) .or. c == '.'

end function is_float

!===============================================================================

logical function is_float_under(c)
	character, intent(in) :: c
	is_float_under = is_float(c) .or. c == "_"
end function is_float_under

!===============================================================================

logical function is_sign(c)

	character, intent(in) :: c

	is_sign = c == '+' .or. c == '-'

end function is_sign

!===============================================================================

logical function is_expo(c)

	character, intent(in) :: c

	is_expo = c == 'd' .or. c == 'e' .or. c == 'D' .or. c == 'E'

end function is_expo

!===============================================================================

logical function is_letter(c)

	character, intent(in) :: c

	is_letter = ('a' <= c .and. c <= 'z') .or. ('A' <= c .and. c <= 'Z')

end function is_letter

!===============================================================================

logical function is_alphanum(c)

	character, intent(in) :: c

	is_alphanum = is_letter(c) .or. is_digit(c)

end function is_alphanum

!===============================================================================

logical function is_alphanum_under(c)

	character, intent(in) :: c

	is_alphanum_under = is_letter(c) .or. is_digit(c) .or. c == "_"

end function is_alphanum_under

!===============================================================================

logical function is_whitespace(c)

	character, intent(in) :: c

	is_whitespace = any(c == [TAB, LINE_FEED, VERT_TAB, CARRIAGE_RETURN, ' '])

end function is_whitespace

!===============================================================================

function quote(str)
	character(len=*), intent(in) :: str
	character(len=:), allocatable :: quote
	quote = '"'//str//'"'
end function quote

!===============================================================================

end module jsonf__utils

!===============================================================================

