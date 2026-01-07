
module jsonf

	use jsonf__utils
	implicit none

	! TODO:
	! - float improvements:
	!   * auto convert i64 overflows to f64?
	! - add a `strict` option (json_t member and cmd arg) which just turns on
	!   other options, e.g. error_trailing_commas, require leading digit before
	!   decimal point, etc.
	!   * name strict, pedantic, -Wall or -Werror
	!   * probably keep error_duplicate_keys orthogonal to this since standard
	!     allows dupes
	! - error handling
	!   * don't panic unless stop-on-error is requested
	! - get_val() improvements:
	!   * need a related `json%len(pointer)` method, or like json-fortran's
	!     %info(), to get len (n_children) of array (or object). maybe refactor
	!     get_val to take multiple opt out args or a struct to copy in the
	!     appropriate requested data
	!   * optional 'found' out-arg
	!   * name get_val() or just get() ?
	!   * add typed versions: get_i64(), get_bool(), etc.
	!   * throw error for bad type
	!   * get_vec_i64(), get_vec_bool(), etc.
	!   * i think json-fortran has something like get_matrix(). obviously there
	!     should be restrictions, like each sub-array must be the same size
	!   * Fortran does not really have null, so we will need is_null() instead
	!     of get_null()
	! - write details in readme
	! - expose less
	!   * move as many implementation details as possible into something like
	!     private.F90
	!   * other modules, e.g. utils, should default private
	!   * and/or, declare individual routines as `private`, though I'd prefer a
	!     separate file. time to split anyway
	! - test in other projects
	!   * aoc-fortran pips solver
	!   * ribbit?
	! - spellcheck for bad cmd args
	!   * bad keys done
	! - add other stream types
	!   * stdin
	!   * network? probably not
	! - ci/cd
	!   * test on windows, linux (macos?)
	!   * test with different fortran compilers
	!   * cmake
	! - benchmark performance
	! - unit tests must cover bad syntax -- it's important that they don't go
	!   into an infinite loop on anything like unterminated strs (i just did
	!   this :facepalm:)
	!   * invalid tokens
	!   * unterminated strings
	!   * invalid numbers, e.g. bad floats that look almost like a float
	! - cmd args:
	!   * stdin option, if no other opt given, or maybe with explicit ` - ` arg
	!   * hashed_order option
	!   * stop-on-error on "assert". need better error handling first
	! - check json-fortran, jq, and other similar projects for features to add
	! - test re-entry with re-using one object to load multiple JSON inputs in
	!   sequence. might find bugs with things that need to be deallocated first
	! - test large files
	! - test unicode in strings
	!   * note that hex literals can be part of a unicode sequence in JSON, but
	!     numeric literals are never hex, octal, or binary
	! - test escape sequences in strings
	!   * \", \\ done
	!   * \n, \t, \/, \b, \f, \r, \uXXXX TBD
	! - test comments -- nonstandard but use "#" or another char if requested by some option
	! - test diagnostics reporting line/column numbers

	integer, parameter :: &
		JSONF_MAJOR = 0, &  ! Note: update version in fpm.toml too
		JSONF_MINOR = 2, &
		JSONF_PATCH = 0

	integer, parameter :: DEBUG = 0

	type stream_t
		integer :: type
		integer :: unit
		integer(kind=8) :: pos
		character(len=:), allocatable :: str, src_file
		logical :: is_eof = .false.
		contains
			procedure :: get => get_stream_char
	end type stream_t

	type sca_t
		! Scalar value type -- primitive bool, int, float, str, or null, but
		! *not* arrays or objects
		!
		! TODO: equivalence union?
		integer :: type  ! TODO: is the sca_t%type used, or is the parent json_val_t%type sufficient?
		logical :: bool
		integer(kind=8) :: i64
		real(kind=8) :: f64
		character(len=:), allocatable :: str
	end type sca_t

	type json_val_t
		! JSON value type -- scalar, array, or object

		integer :: type
		type(sca_t) :: sca

		! Object members
		integer(kind=4) :: nkeys = 0
		type(str_t), allocatable :: keys(:)
		type(json_val_t), allocatable :: vals(:)
		integer(kind=4), allocatable :: idx(:)  ! for consistent output of object hashmap members

		! Array members
		integer(kind=4) :: narr = 0  ! not needed if we trim after reading? but that would block later post-read insertions
		type(json_val_t), allocatable :: arr(:)  ! could re-use vals(:) but this might be less error-prone

		! i32 should be ok for sizes and idx. If a JSON object has 2 billion
		! keys, it will be much more than 2 GB counting quotes and colons, even
		! without whitespace and 1-char keys. Non-duplicate keys would have to
		! be at least 4 or 5 chars
		!
		! Values, of course need to be i64
	end type json_val_t

	character(len=*), parameter :: INDENT_DEFAULT = "    "
	type json_t
		! JSON top-level type

		!********
		! Public members

		! Syntax strictness/permissiveness options
		logical :: &
			error_duplicate_keys  = .false., &
			error_trailing_commas = .false., &
			warn_trailing_commas  = .false., &
			error_numbers         = .false., &
			warn_numbers          = .false., &
			first_duplicate       = .false.

		! String, print, and write output formatting options
		character(len=:), allocatable :: indent
		logical :: compact = .false.
		logical :: hashed_order = .false.  ! if false, output in the same order as the input source

		logical :: lint = .false.

		!********
		! Private members
		type(json_val_t), private :: root
		integer, private :: indent_level = 0
		!********

		contains
			procedure :: &
				get_val   => get_val_json, &
				write     => write_json, &
				print     => print_json, &
				to_str    => json_to_str, &
				read_file => read_file_json, &
				read_str  => read_str_json, &
				parse     => parse_json
	end type json_t

	type token_t
		integer :: kind
		integer(kind=8) :: pos  ! TODO: unused?
		integer :: line, col
		character(len=:), allocatable :: text
		type(sca_t) :: sca
	end type token_t

	type lexer_t
		!character(len=:), allocatable :: src_file  ! already part of stream_t encapsed in lexer
		integer(kind=8)     :: pos  ! TODO: unused?
		integer             :: line, col
		type(str_vec_t)     :: diagnostics
		type(str_builder_t) :: line_str
		type(stream_t)      :: stream

		! Could add more lookaheads if needed, i.e. next_token and peek2_token
		character     :: current_char , previous_char
		type(token_t) :: current_token, previous_token

		logical :: &
			error_numbers         = .false., &
			warn_numbers          = .false.

		contains
			procedure :: &
				lex, &
				match => lexer_match, &
				next_char  => lexer_next_char, &
				next_token => lexer_next_token, &
				current_kind => lexer_current_kind, &
				finish_line  => lexer_finish_line
	end type lexer_t

	integer, parameter :: &
		F64_TYPE         = 26, &
		F64_TOKEN        = 25, &
		BOOL_TYPE        = 24, &
		BOOL_TOKEN       = 23, &
		NULL_TYPE        = 22, &
		NULL_TOKEN       = 21, &
		STR_STREAM       = 20, &
		FILE_STREAM      = 19, &
		ARR_TYPE         = 18, &
		OBJ_TYPE         = 17, &
		STR_TOKEN        = 16, &
		STR_TYPE         = 15, &
		LBRACE_TOKEN     = 14, &
		RBRACE_TOKEN     = 13, &
		LBRACKET_TOKEN   = 12, &
		RBRACKET_TOKEN   = 11, &
		COLON_TOKEN      = 10, &
		COMMA_TOKEN      = 9, &
		HASH_TOKEN       = 8, &
		MINUS_TOKEN      = 7, &
		PLUS_TOKEN       = 6, &
		I64_TYPE         = 5, &
		I64_TOKEN        = 4, &
		WHITESPACE_TOKEN = 3, &
		BAD_TOKEN        = 2, &
		EOF_TOKEN        = 1

contains

function get_jsonf_vers()
	character(len=:), allocatable :: get_jsonf_vers
	get_jsonf_vers = &
		to_str(JSONF_MAJOR) // "." // &
		to_str(JSONF_MINOR) // "." // &
		to_str(JSONF_PATCH)
end function get_jsonf_vers

subroutine lexer_next_char(lexer)
	class(lexer_t), intent(inout) :: lexer
	!********
	lexer%previous_char = lexer%current_char
	lexer%current_char =  lexer%stream%get()
	if (lexer%current_char == LINE_FEED) then
		lexer%line = lexer%line + 1
		lexer%col = 1
		lexer%line_str = new_str_builder()
	else
		lexer%col = lexer%col + 1
		call lexer%line_str%push(lexer%current_char)
	end if

	!print *, "char = ", quote(lexer%current_char)
	!print *, "line = ", lexer%line
	!print *, "col  = ", lexer%col
	!print *, ""
end subroutine lexer_next_char

subroutine lexer_finish_line(lexer)
	class(lexer_t), intent(inout) :: lexer
	! Read the rest of the line, for error messages
	!
	! TODO: save current stream position and restore it later, in case we want
	! to keep trying to parse?  Or can we just stop parsing after first error?
	do
		lexer%previous_char = lexer%current_char
		lexer%current_char =  lexer%stream%get()
		if (lexer%stream%is_eof) exit
		if (lexer%current_char == LINE_FEED) exit

		call lexer%line_str%push(lexer%current_char)
	end do
end subroutine lexer_finish_line

subroutine lexer_next_token(lexer)
	! Advance to the next non-whitespace token.  Comment skipping could also be
	! added here
	class(lexer_t), intent(inout) :: lexer
	!********
	lexer%previous_token = lexer%current_token
	lexer%current_token  = lexer%lex()
	do while (lexer%current_token%kind == WHITESPACE_TOKEN)
		lexer%current_token = lexer%lex()
		if (lexer%current_token%kind == EOF_TOKEN) exit
	end do
end subroutine lexer_next_token

integer function lexer_current_kind(lexer)
	class(lexer_t) :: lexer
	lexer_current_kind = lexer%current_token%kind
end function lexer_current_kind

function new_literal(type, bool, i64, f64, str) result(lit)
	integer, intent(in) :: type
	!********
	integer(kind=8) , intent(in), optional :: i64
	real   (kind=8) , intent(in), optional :: f64
	logical         , intent(in), optional :: bool
	character(len=*), intent(in), optional :: str
	type(sca_t) :: lit

	lit%type = type
	if (present(bool)) lit%bool = bool
	if (present(f64 )) lit%f64  = f64
	if (present(i64 )) lit%i64  = i64
	if (present(str )) lit%str  = str

end function new_literal

function lex(lexer) result(token)
	class(lexer_t) :: lexer
	type(token_t) :: token
	!********
	character(len=:), allocatable :: text, text_strip
	integer :: io
	integer(kind=8) :: start, end_, i64
	integer :: l0, c0
	logical :: float_, is_valid
	real(kind=8) :: f64
	type(str_builder_t) :: sb
	type(sca_t) :: sca

	if (DEBUG > 2) then
		write(*,*) "lex: pos = "//to_str(lexer%pos)
	end if

	start = lexer%pos  ! TODO: unused?
	l0 = lexer%line
	c0 = lexer%col
	if (lexer%stream%is_eof) then
		token = new_token(EOF_TOKEN, lexer%line, lexer%col, NULL_CHAR)
		return
	end if

	if (DEBUG > 2) write(*,*) "lex: current char = "//quote(lexer%current_char)

	if (is_whitespace(lexer%current_char)) then
		sb = new_str_builder()
		do while (is_whitespace(lexer%current_char))
			call sb%push(lexer%current_char)
			call lexer%next_char()
		end do
		text = sb%trim()
		token = new_token(WHITESPACE_TOKEN, l0, c0, text)
		return
	end if

	! Note, beware the overlap between is_float_under() and is_letter(), thus
	! the intertwined order dependence here.  Neither 'null', 'true', or 'false'
	! happen to start with any float, sign, or exponent ('dDeE') characters, so
	! this should be ok

	! TODO: don't allow underscores as thousands separators by default, but
	! optionally allow any custom separator
	if (is_float_under(lexer%current_char)) then
		! Numeric decimal integer or float

		! Check valid number format later
		!
		! You'll never have to parse arithmetic like `1+2` in json, unlike
		! syntran
		float_ = .false.
		sb = new_str_builder()
		if (is_sign(lexer%current_char)) then
			call sb%push(lexer%current_char)
			call lexer%next_char()
		end if
		do while (is_float_under(lexer%current_char))
			float_ = float_ .or. .not. is_digit_under(lexer%current_char)
			call sb%push(lexer%current_char)
			call lexer%next_char()
		end do
		end_ = lexer%pos

		text = sb%trim()
		text_strip = rm_char(text, "_")
		!print *, "text = ", text

		!print *, "lexer%error_numbers = ", lexer%error_numbers
		if (lexer%warn_numbers .or. lexer%error_numbers) then
			is_valid = is_valid_json_number(text_strip)
			!print *, "is_valid = ", is_valid
			if (.not. is_valid) then
				if (lexer%warn_numbers) then
					write(*, "(a)") WARN_STR//"invalid number "//quote(text)
				else if (lexer%error_numbers) then
					call panic("invalid number "//quote(text))
				end if
			end if
		end if

		if (float_) then
			read(text_strip, *, iostat = io) f64
			if (DEBUG > 0) write(*,*) "lex: parsed f64 = "//to_str(f64)
			if (io == exit_success) then
				sca   = new_literal(F64_TYPE, f64 = f64)
				token = new_token(F64_TOKEN, l0, c0, text, sca)
			else
				call panic("bad float number: "//text)
			end if
		else  ! i64
			read(text_strip, *, iostat = io) i64
			if (DEBUG > 0) write(*,*) "lex: parsed i64 = "//to_str(i64)
			if (io == exit_success) then
				sca   = new_literal(I64_TYPE, i64 = i64)
				token = new_token(I64_TOKEN, l0, c0, text, sca)
			else
				call panic("bad integer number: "//text)
			end if
		end if

		!print *, 'float text = ', quote(text)
		return
	end if

	if (lexer%current_char == '"') then
		! String literal
		call lexer%next_char()  ! skip opening quote
		start = lexer%pos

		sb = new_str_builder()
		do
			!print *, "lexer current = ", lexer%current_char

			! TODO: test str escape rules. Only 8 characters or a unicode sequence are allowed to follow a backslash
			if (lexer%current_char == "\") then
				call lexer%next_char()
				call sb%push(lexer%current_char)
				call lexer%next_char()
			end if

			if (lexer%current_char == '"' .or. lexer%stream%is_eof) then
				exit
			end if

			call sb%push(lexer%current_char)
			call lexer%next_char()
		end do
		text = sb%trim()

		if (lexer%stream%is_eof) then
			! Unterminated string
			token = new_token(BAD_TOKEN, l0, c0, text)
			call panic("unterminated string literal")  ! TODO: diagnostics
			return
		end if
		call lexer%next_char()

		sca = new_literal(STR_TYPE, str = text)
		if (DEBUG > 0) write(*,*) "lex: parsed string = "//quote(sca%str)
		token = new_token(STR_TOKEN, l0, c0, text, sca)

		return
	end if

	if (is_letter(lexer%current_char)) then
		! There are no variable identifiers in json, so that simplifies things
		sb = new_str_builder()
		do while (is_letter(lexer%current_char))
			call sb%push(lexer%current_char)
			call lexer%next_char()
		end do
		text = sb%trim()
		token = new_keyword_token(l0, c0, text)
		return
	end if

	select case (lexer%current_char)

	case ("{")
		token = new_token(LBRACE_TOKEN, l0, c0, lexer%current_char)
	case ("}")
		token = new_token(RBRACE_TOKEN, l0, c0, lexer%current_char)
	case ("[")
		token = new_token(LBRACKET_TOKEN, l0, c0, lexer%current_char)
	case ("]")
		token = new_token(RBRACKET_TOKEN, l0, c0, lexer%current_char)
	case (":")
		token = new_token(COLON_TOKEN, l0, c0, lexer%current_char)
	case (",")
		token = new_token(COMMA_TOKEN, l0, c0, lexer%current_char)

	! TODO: take user input instead of hard-coded comment char. Since it's a
	! variable it will have to be outside of select case which only works on
	! constant strings
	case ("#")
		token = new_token(HASH_TOKEN, l0, c0, lexer%current_char)

	case default
		!print *, 'bad token text = ', quote(lexer%current_char)

		token = new_token(BAD_TOKEN, l0, c0, lexer%current_char)
		!! TODO: implement span_t and diagnostics
		!span = new_span(lexer%pos, len(lexer%current_char))
		!call lexer%diagnostics%push( &
		!	err_unexpected_char(lexer%context, &
		!	span, lexer%current_char))
		call panic("unexpected character: "//quote(lexer%current_char))

	end select

	call lexer%next_char()

end function lex

logical function is_valid_json_number(str) result(is_valid)
	! Check the correct formatting of a `str` representing a potential JSON
	! number
	character(len=*), intent(in) :: str
	!********
	integer :: int_start, int_end, frac_start, frac_end, exp_start, exp_end
	logical :: has_frac, has_exp

	! JSON number grammar:
	!
	!     number = [ "-" ] int [ frac ] [ exp ]
	!     int   = "0" | digit1-9 *digit
	!     frac  = "." 1*digit
	!     exp   = ("e" | "E") [ "+" | "-" ] 1*digit
	!
	! Validate grammar with this process:
	!
	! - Strip optional leading -
	! - If integer part starts with 0, it must be exactly "0"
	! - If fractional part exists, require at least one digit
	! - If exponent exists, require digits after e/E

	!print *, "str = ", quote(str)
	is_valid = .true.

	int_start = 1
	if (str(1:1) == "-") int_start = 2

	int_end = verify(str(int_start+1:), DIGIT_CHARS) + int_start - 1
	if (int_end < int_start) int_end = len(str)
	!print *, "int_start, int_end = ", int_start, int_end
	!print *, "int part = ", quote(str(int_start: int_end))

	is_valid = int_start <= int_end
	if (.not. is_valid) return

	! - If integer part starts with 0, it must be exactly "0"
	if (str(int_start:int_start) == "0") then
		is_valid = int_start == int_end
		if (.not. is_valid) return
	end if

	is_valid = is_all_digits(str(int_start:int_end))
	if (.not. is_valid) return

	frac_start = int_end + 1

	! Get the exp_start now because it's the easiest way to get frac_end
	exp_start = scan(str(frac_start:), "eE") + frac_start - 1
	if (exp_start < frac_start) exp_start = len(str)+1

	frac_end = exp_start - 1
	has_frac = frac_start <= frac_end
	if (has_frac) then

		is_valid = frac_start-1 < int_end+1 .or. any(str(int_end+1: frac_start-1) == [".", "e", "E"])
		if (.not. is_valid) return

		if (str(frac_start:frac_start) == ".") frac_start = frac_start+1

		! - If fractional part exists, require at least one digit
		is_valid = contains(str(frac_end:frac_end), DIGIT_CHARS)
		if (.not. is_valid) return

		is_valid = is_all_digits(str(frac_start:frac_end))
		if (.not. is_valid) return
	end if

	has_exp = exp_start <= len(str)
	if (has_exp) then
		exp_end = len(str)

		! - If exponent exists, require digits after e/E
		is_valid = contains(str(exp_end:exp_end), DIGIT_CHARS)
		if (.not. is_valid) return

		! Skip [eE]
		exp_start = exp_start + 1

		! Skip [+-] if present
		if (contains("+-", str(exp_start:exp_start))) exp_start = exp_start + 1

		! Now we have just the digit part of the exponent, if it's formatted
		! correctly
		is_valid = is_all_digits(str(exp_start:exp_end))
		if (.not. is_valid) return

	end if

end function is_valid_json_number

function new_keyword_token(line, col, text) result(token)
	integer, intent(in) :: line, col
	character(len=*), intent(in) :: text
	type(token_t) :: token
	!********
	integer :: kind
	type(sca_t) :: sca

	select case (text)
	case ("true")
		kind = BOOL_TOKEN
		sca  = new_literal(BOOL_TYPE, bool = .true.)
	case ("false")
		kind = BOOL_TOKEN
		sca  = new_literal(BOOL_TYPE, bool = .false.)

	case ("null")
		kind = NULL_TOKEN
		sca  = new_literal(NULL_TYPE)

	case default
		kind = BAD_TOKEN
	end select

	token = new_token(kind, line, col, text, sca)

end function new_keyword_token

!function new_token(kind, pos, text, sca) result(token)
function new_token(kind, line, col, text, sca) result(token)
	integer, intent(in) :: kind
	integer, intent(in) :: line, col
	!integer(kind=8), intent(in) :: pos
	character(len=*), intent(in) :: text
	type(sca_t), intent(in), optional :: sca
	type(token_t) :: token

	token%kind = kind
	!token%pos  = pos
	token%line = line
	token%col  = col
	token%text = text
	if (present(sca)) token%sca = sca

end function new_token

function new_lexer(stream, json) result(lexer)
	type(stream_t) :: stream
	type(lexer_t) :: lexer
	type(json_t), optional :: json
	!********

	if (present(json)) then
		lexer%warn_numbers  = json%warn_numbers
		lexer%error_numbers = json%error_numbers
	end if

	lexer%line = 1
	lexer%pos = 1
	lexer%diagnostics = new_str_vec()
	lexer%stream = stream

	! Get the first char and token on construction instead of checking later if
	! we have them
	lexer%current_char = lexer%stream%get()
	lexer%col = 1
	if (lexer%current_char == LINE_FEED) then
		lexer%line = lexer%line + 1
	else
		lexer%line_str = new_str_builder()
		call lexer%line_str%push(lexer%current_char)
	end if

	lexer%current_token = lexer%lex()

	! Skip leading whitespace
	if (lexer%current_kind() == WHITESPACE_TOKEN) call lexer%next_token()

end function new_lexer

subroutine read_file_json(json, filename)
	class(json_t) :: json
	character(len=*), intent(in) :: filename
	!********
	type(stream_t) :: stream

	! Stream chars one at a time
	stream = new_file_stream(filename)
	call json%parse(stream)

end subroutine read_file_json

subroutine read_str_json(json, str)
	class(json_t), intent(inout) :: json
	character(len=*), intent(in) :: str
	!********
	type(stream_t) :: stream

	! We have the whole str, but treat it as a stream for consistency with file
	! streaming
	stream = new_str_stream(str)
	call json%parse(stream)

end subroutine read_str_json

character function get_stream_char(stream) result(c)
	class(stream_t) :: stream
	!********
	integer :: io
	select case (stream%type)
	case (FILE_STREAM)
		!print *, "reading stream unit "//to_str(stream%unit)
		read(stream%unit, iostat = io) c
		if (io == IOSTAT_END) then
			stream%is_eof = .true.
			c = NULL_CHAR
			return
		end if
		!print *, "c = ", quote(c)

	case (STR_STREAM)
		!print *, "getting str stream pos "//to_str(stream%pos)
		if (stream%pos > len(stream%str)) then
			stream%is_eof = .true.
			c = NULL_CHAR
			return
		end if
		c = stream%str(stream%pos:stream%pos)
		!print *, "c = ", quote(c)
		stream%pos = stream%pos + 1

	case default
		call panic("stream type not implemented")
	end select
end function get_stream_char

subroutine parse_json(json, stream)
	class(json_t), intent(inout) :: json
	type(stream_t) :: stream
	!********
	type(lexer_t) :: lexer

	lexer = new_lexer(stream, json)
	!write(*,*) "Parsing JSON tokens ..."
	call parse_val(json, lexer, json%root)

end subroutine parse_json

subroutine lexer_match(lexer, kind)
	class(lexer_t), intent(inout) :: lexer
	integer, intent(in) :: kind
	!********
	if (lexer%current_kind() == kind) then
		call lexer%next_token()
		return
	end if

	! Syntran just advances to the next token on mismatch. Does that have an advantage?
	call panic("expected token of kind "// &
		kind_name(kind)//", but got "// &
		kind_name(lexer%current_kind()))

end subroutine lexer_match

function escape(str)
	character(len=*), intent(in) :: str
	character(len=:), allocatable :: escape
	!********
	integer :: i
	type(str_builder_t) :: sb
	sb = new_str_builder()
	do i = 1, len(str)
		if (any(str(i:i) == ['"', '\'])) call sb%push("\")
		call sb%push(str(i:i))
	end do
	escape = sb%trim()
end function escape

! Do these need to be declared recursive? I remember certain fortran compilers being picky about it, even though it's only inderictly recursive.  Same with obj_to_str()
recursive function val_to_str(json, val) result(str)
	type(json_t), intent(inout) :: json
	type(json_val_t), intent(in) :: val
	character(len = :), allocatable :: str
	!********
	! Don't need a str_builder here since json_val_t is a single value

	if (.not. allocated(json%indent)) then
		! I would just initialize this in the type declaration but Fortran is redacted
		!
		! TODO: dry?
		json%indent = INDENT_DEFAULT
	end if

	select case (val%type)
	case (STR_TYPE)
		str = quote(escape(val%sca%str))
	case (F64_TYPE)
		str = to_str(val%sca%f64)
	case (I64_TYPE)
		str = to_str(val%sca%i64)
	case (BOOL_TYPE)
		str = to_str(val%sca%bool)

	case (NULL_TYPE)
		str = "null"

	case (OBJ_TYPE)
		str = obj_to_str(json, val)
	case (ARR_TYPE)
		str = arr_to_str(json, val)

	case default
		write(ERROR_UNIT, *) "type = ", val%type
		call panic("val_to_str: unknown type "//kind_name(val%type))
	end select
end function val_to_str

subroutine write_json(this, filename, unit_)
	class(json_t) :: this
	character(len=*), intent(in), optional :: filename
	integer, intent(in), optional :: unit_
	!********
	integer :: unit__
	if (present(unit_)) then
		unit__ = unit_
	else if (present(filename)) then
		open(newunit = unit__, file = filename, action = "write")
	else
		unit__ = OUTPUT_UNIT
	end if

	write(unit__, "(a)") json_to_str(this)

	if (present(filename)) then
		!print *, "Closing unit "//to_str(unit__)
		close(unit__)
	end if

end subroutine write_json

subroutine print_json(this, msg)
	class(json_t) :: this
	character(len=*), intent(in), optional :: msg
	!********
	if (present(msg)) then
		if (msg /= "") write(*, "(a)") msg
	end if
	call write_json(this)
end subroutine print_json

function json_to_str(this) result(str)
	class(json_t) :: this
	character(len = :), allocatable :: str
	!********
	if (.not. allocated(this%indent)) then
		! I would just initialize this in the type declaration but Fortran is redacted
		this%indent = INDENT_DEFAULT
	end if
	this%indent_level = 0
	str = val_to_str(this, this%root)
end function json_to_str

recursive function keyval_to_str(json, obj, i, indent, last) result(str)
	type(json_t), intent(inout) :: json
	type(json_val_t), intent(in) :: obj
	integer(kind=4), intent(in) :: i
	character(len=*), intent(in) :: indent
	character(len = :), allocatable :: str
	logical, intent(in) :: last
	!********
	character(len=:), allocatable :: key_str, val_str
	type(str_builder_t) :: sb

	!write(ERROR_UNIT, *) "key = "//quote(obj%keys(i)%str)

	sb = new_str_builder()
	key_str = quote(escape(obj%keys(i)%str))
	val_str = val_to_str(json, obj%vals(i))
	if (.not. json%compact) call sb%push(indent)
	call sb%push(key_str//":")
	if (.not. json%compact) call sb%push(" ")
	call sb%push(val_str)

	!! Output gets weirdly truncated if you do this without the helper variables,
	!! no idea why. Looks like a bug in str builder that doesn't copy/realloc
	!! correctly, but I'm pretty sure I've ruled that out.  This is way more
	!! readable with helper vars anyway
	!call sb%push(indent//quote(obj%keys(i)%str)//": "//val_to_str(json, obj%vals(i)))

	if (.not. last) call sb%push(",")
	if (.not. json%compact) call sb%push(LINE_FEED)
	str = sb%trim()

end function keyval_to_str

recursive function arr_to_str(json, arr) result(str)
	use jsonf__blarg
	use jsonf__sort
	type(json_t), intent(inout) :: json
	type(json_val_t), intent(in) :: arr
	character(len = :), allocatable :: str
	!********
	character(len=:), allocatable :: indent
	integer(kind=4) :: i
	type(str_builder_t) :: sb

	!! Be careful with debug logging. If you write directly to stdout, it hang forever for inadvertent recursive prints
	!write(ERROR_UNIT, *) "starting arr_to_str()"

	sb = new_str_builder()
	call sb%push("[")
	if (.not. json%compact) call sb%push(LINE_FEED)
	json%indent_level = json%indent_level + 1
	indent = repeat(json%indent, json%indent_level)

	do i = 1, arr%narr
		if (.not. json%compact) call sb%push(indent)
		call sb%push(val_to_str(json, arr%arr(i)))
		if (i < arr%narr) call sb%push(",")
		if (.not. json%compact) call sb%push(LINE_FEED)
	end do

	json%indent_level = json%indent_level - 1
	indent = repeat(json%indent, json%indent_level)
	if (.not. json%compact) call sb%push(indent)
	call sb%push("]")
	str = sb%trim()
end function arr_to_str

recursive function obj_to_str(json, obj) result(str)
	use jsonf__blarg
	use jsonf__sort
	type(json_t), intent(inout) :: json
	type(json_val_t), intent(in) :: obj
	character(len = :), allocatable :: str
	!********
	character(len=:), allocatable :: indent
	integer(kind=4) :: i, ii
	type(str_builder_t) :: sb

	!! Be careful with debug logging. If you write directly to stdout, it hang forever for inadvertent recursive prints
	!write(ERROR_UNIT, *) "starting obj_to_str()"
	!write(ERROR_UNIT, *) "in obj_to_str: idx = ", obj%idx(1: obj%nkeys)

	sb = new_str_builder()
	call sb%push("{")
	if (.not. json%compact) call sb%push(LINE_FEED)
	json%indent_level = json%indent_level + 1
	indent = repeat(json%indent, json%indent_level)

	if (json%hashed_order) then
		ii = 0
		do i = 1, size(obj%keys)
			if (allocated(obj%keys(i)%str)) then
				ii = ii + 1
				call sb%push(keyval_to_str(json, obj, i, indent, ii == obj%nkeys))
			end if
		end do
	else
		do ii = 1, obj%nkeys
			i = obj%idx(ii)
			call sb%push(keyval_to_str(json, obj, i, indent, ii == obj%nkeys))
		end do
	end if

	json%indent_level = json%indent_level - 1
	indent = repeat(json%indent, json%indent_level)
	if (.not. json%compact) call sb%push(indent)
	call sb%push("}")
	str = sb%trim()
end function obj_to_str

subroutine parse_val(json, lexer, val)
	type(json_t), intent(inout) :: json
	type(lexer_t), intent(inout) :: lexer
	type(json_val_t), intent(out) :: val
	!********
	select case (lexer%current_kind())
	case (STR_TOKEN)
		!print *, "value (string) = ", lexer%current_token%sca%str
		val%type = STR_TYPE
		val%sca = lexer%current_token%sca
		call lexer%next_token()

	case (F64_TOKEN)
		val%type = F64_TYPE
		! Note these technically copy more sca members than required, e.g. f64
		! doesn't need to initialize the sca%i64 member
		val%sca = lexer%current_token%sca
		call lexer%next_token()

	case (I64_TOKEN)
		val%type = I64_TYPE
		val%sca = lexer%current_token%sca
		call lexer%next_token()

	case (BOOL_TOKEN)
		val%type = BOOL_TYPE
		val%sca = lexer%current_token%sca
		call lexer%next_token()

	case (NULL_TOKEN)
		val%type = NULL_TYPE
		val%sca = lexer%current_token%sca
		call lexer%next_token()

	case (LBRACE_TOKEN)
		call parse_obj(json, lexer, val)
	case (LBRACKET_TOKEN)
		call parse_arr(json, lexer, val)

	case default
		!print *, "kind = ", lexer%current_kind()
		call panic("unexpected value type in object of kind " // &
			kind_name(lexer%current_kind()))
	end select
end subroutine parse_val

function get_val_json(json, ptr) result(val)
	! User-facing function
	class(json_t), intent(in) :: json
	character(len=*), intent(in) :: ptr  ! RFC 6901 path string
	type(json_val_t) :: val
	!********

	! If any backslash escape sequences should be processed in the ptr str, it
	! could be done here.  See e.g. the cases with expected results 5 and 6
	! ("/i\\j") in test_in9()

	call get_val_core(json%root, ptr, 1, val)

end function get_val_json

recursive subroutine get_val_core(val, ptr, i0, outval)
	! Private subroutine
	type(json_val_t), intent(in) :: val
	character(len=*), intent(in) :: ptr
	integer, intent(in) :: i0  ! index of last '/' separator in ptr path string
	type(json_val_t) :: outval
	!********
	character(len=:), allocatable :: key, closest
	integer :: i, j, k
	integer(kind=4) :: idx
	logical :: found

	if (i0 > len(ptr)) then
		! Base case: whole path has been walked

		! Hopefully this is a small copy, unless user is doing something weird
		! like querying the root
		!
		! Could even check type and warn if not scalar/primitive
		call copy_val(outval, val)
		return
	end if

	! Split by '/' before handling '~' escapes
	i = i0 + 1
	do
		if (i > len(ptr)) exit
		if (ptr(i:i) == "/") exit
		i = i + 1
	end do

	! TODO: test this RFC paragraph:
	!
	!   Evaluation of each reference token begins by decoding any escaped
	!   character sequence.  This is performed by first transforming any
	!   occurrence of the sequence '~1' to '/', and then transforming any
	!   occurrence of the sequence '~0' to '~'.  By performing the
	!   substitutions in this order, an implementation avoids the error of
	!   turning '~01' first into '~1' and then into '/', which would be
	!   incorrect (the string '~01' correctly becomes '~1' after
	!   transformation).

	key = ptr(i0+1: i-1)  ! over-allocate
	j = i0
	k = 1
	do while (j < i-1)
		j = j + 1
		if (j+1 <= len(ptr)) then
			if (ptr(j:j+1) == "~0") then
				key(k:k) = '~'
				k = k + 1
				j = j + 1
				cycle
			else if (ptr(j:j+1) == "~1") then
				key(k:k) = '/'
				k = k + 1
				j = j + 1
				cycle
			end if
		end if
		key(k:k) = ptr(j:j)
		k = k + 1
	end do
	key = key(1:k-1)
	!key = escape(key)
	!print *, "key = ", key

	select case (val%type)
	case (OBJ_TYPE)
		idx = get_map_idx(val, key, found)
		if (.not. found) then
			closest = get_closest_key(val, key)
			write(*, "(a)") ERROR_STR//"key "//quote(key_hi(key))//" not found"
			write(*, "(a)") "Did you mean "//quote(key_hi(closest))//"?"
			call panic("")
		end if
		call get_val_core(val%vals(idx), ptr, i, outval)

	case (ARR_TYPE)
		idx = read_i32(key) ! TODO: iostat
		!print *, "idx = ", idx
		if (idx < 0 .or. idx >= val%narr) then
			! TODO: print bounds, for consistency with printing closest key
			call panic("index "//to_str(idx)//" out of bounds")
		end if
		call get_val_core(val%arr(idx+1), ptr, i, outval)  ! convert 0-index to 1-index

	case default
		call panic("bad type in get_val_json()")
	end select

end subroutine get_val_core

function key_hi(str)
	! I'm setting up a syntax-highlighting framework here for later more general
	! color JSON output.  I'll probably want to color keys one way, string
	! values another way, numbers, etc.  Functions might (?) be easier to work with
	! than string constants when color can be turned off via cmd args
	character(len=*), intent(in) :: str
	character(len=:), allocatable :: key_hi
	key_hi = fg_cyan//str//color_reset
end function key_hi

subroutine parse_arr(json, lexer, arr)
	type(json_t), intent(inout) :: json
	type(lexer_t), intent(inout) :: lexer
	type(json_val_t), intent(out) :: arr
	!********
	integer, parameter :: INIT_SIZE = 2
	integer :: idx
	integer(kind=8) :: i, n, n0 ! TODO: kind?
	type(json_val_t) :: val
	type(json_val_t), allocatable :: old_arr(:)

	if (DEBUG > 0) print *, "Starting parse_arr()"

	if (DEBUG > 0) print *, "matching LBRACKET_TOKEN"
	call lexer%match(LBRACKET_TOKEN)
	arr%type = ARR_TYPE

	! Initialize array storage
	allocate(arr%arr(INIT_SIZE))
	arr%narr = 0

	idx = 0
	do
		if (lexer%current_kind() == RBRACKET_TOKEN) exit
		idx = idx + 1
		!print *, "idx = ", idx

		call parse_val(json, lexer, val)
		!print *, "val = ", val%to_str()

		if (.not. json%lint) then
			n0 = size(arr%arr)
			if (idx > n0) then
				! Resize array dynamically
				n = n0 * 2
				call move_alloc(arr%arr, old_arr)
				allocate(arr%arr(n))
				do i = 1, n0
					call move_val(old_arr(i), arr%arr(i))
				end do
				deallocate(old_arr)
			end if

			! Store the value. Could avoid temp `val` by resizing before parsing,
			! arrays are simpler than objects
			call move_val(val, arr%arr(idx))
		end if

		select case (lexer%current_kind())
		case (COMMA_TOKEN)
			call lexer%next_token()
		case (RBRACKET_TOKEN)
			! Do nothing
		case default
			call panic("expected `,` or `]` while parsing array")
		end select
		!if (lexer%current_kind() == COMMA_TOKEN) call lexer%next_token()
	end do
	arr%narr = idx
	!print *, "current  = ", kind_name(lexer%current_kind())
	!print *, "previous = ", kind_name(lexer%previous_token%kind)

	if (lexer%previous_token%kind == COMMA_TOKEN) then
		if (json%error_trailing_commas) then
			call panic("trailing comma in array")
		end if
		if (json%warn_trailing_commas) then
			write(*, "(a)") WARN_STR//"trailing comma in array"
		end if
	end if
	call lexer%match(RBRACKET_TOKEN)

	if (DEBUG > 0) then
		write(*,*) "Finished parse_arr(), narr = "//to_str(arr%narr)
	end if

end subroutine parse_arr

subroutine parse_obj(json, lexer, obj)
	type(json_t), intent(inout) :: json
	type(lexer_t), intent(inout) :: lexer
	type(json_val_t), intent(out) :: obj
	!********
	character(len=:), allocatable :: key
	integer, parameter :: INIT_SIZE = 2
	type(json_val_t) :: val

	if (DEBUG > 0) print *, "Starting parse_obj()"

	if (DEBUG > 0) print *, "matching LBRACE_TOKEN"
	call lexer%match(LBRACE_TOKEN)
	obj%type = OBJ_TYPE

	! Initialize hash map storage
	allocate(obj%keys(INIT_SIZE))
	allocate(obj%vals(INIT_SIZE))
	allocate(obj%idx  (INIT_SIZE))
	obj%nkeys = 0

	do
		if (lexer%current_kind() == RBRACE_TOKEN) exit

		call lexer%match(STR_TOKEN)
		key = lexer%previous_token%sca%str
		!print *, "key = ", key

		call lexer%match(COLON_TOKEN)
		call parse_val(json, lexer, val)
		!print *, "val = ", val%to_str()

		if (.not. json%lint) then
			! Store the key-value pair in the object
			call set_map(json, obj, key, val)
		end if

		select case (lexer%current_kind())
		case (COMMA_TOKEN)
			call lexer%next_token()
		case (RBRACE_TOKEN)
			! Do nothing
		case default
			!!print *, "pos = ", lexer%pos
			!!print *, "pos = ", lexer%stream%pos

			!!print *, "line = ", lexer%line
			!!print *, "col  = ", lexer%col
			!print *, "l0:c0 = ", to_str(lexer%previous_token%line)//":"//to_str(lexer%previous_token%col)
			!print *, "l :c  = ", to_str(lexer%current_token%line)//":"//to_str(lexer%current_token%col)
			!print *, "token text = ", quote(lexer%current_token%text)
			!print *, "token len  = ", len(lexer%current_token%text)
			!print *, "line.      = ", quote(lexer%line_str%str(1: lexer%line_str%len))

			!call lexer%finish_line()
			!print *, "line       = ", quote(lexer%line_str%str(1: lexer%line_str%len))

			call err_bad_obj_delim(lexer)

			call panic("expected `,` or `}` while parsing object, got token " &
				//quote(kind_name(lexer%current_kind())))
		end select
		!if (lexer%current_kind() == COMMA_TOKEN) call lexer%next_token()
	end do
	!print *, "current  = ", kind_name(lexer%current_kind())
	!print *, "previous = ", kind_name(lexer%previous_token%kind)

	if (lexer%previous_token%kind == COMMA_TOKEN) then
		if (json%error_trailing_commas) then
			call panic("trailing comma in object")
		end if
		if (json%warn_trailing_commas) then
			write(*, "(a)") WARN_STR//"trailing comma in object"
		end if
	end if
	call lexer%match(RBRACE_TOKEN)

	! We might be able to deallocate obj%idx(:) here if no duplicate keys were
	! found. However, memory savings aren't that much -- every key and val takes
	! up more memory than an idx. Also it would be fine with current read-once
	! architecture, but if we expose an API to modify JSON after reading, new
	! duplicate keys might get inserted even if the object originally had unique
	! keys

	if (DEBUG > 0) then
		write(*,*) "Finished parse_obj(), nkeys = "//to_str(obj%nkeys)
	end if

end subroutine parse_obj

subroutine err_bad_obj_delim(lexer)
	type(lexer_t), intent(inout) :: lexer
	!********
	character(len=:), allocatable :: err, underline
	character(len = :), allocatable :: str_i, spaces, fg1, rst, col, text
	integer :: i1(1), i, j, str_i_len, start, last, length, icol

	print *, ""
	print *, "Starting err_bad_obj_delim()"

	print *, "l0:c0 = ", to_str(lexer%previous_token%line)//":"//to_str(lexer%previous_token%col)
	print *, "l :c  = ", to_str(lexer%current_token%line)//":"//to_str(lexer%current_token%col)
	print *, "token text = ", quote(lexer%current_token%text)
	print *, "token len  = ", len(lexer%current_token%text)
	print *, "line.      = ", quote(lexer%line_str%str(1: lexer%line_str%len))

	call lexer%finish_line()
	print *, "line       = ", quote(lexer%line_str%str(1: lexer%line_str%len))

	err = ERROR_STR // &
		'expected "," or "}" while while parsing object, got token ' // &
		kind_name(lexer%current_kind())
		!quote(kind_name(lexer%current_kind()))

	print *, "err = "
	print *, err

	! Here's an example of a rust error message, from which I'm stealing UX:
	!
	! """
	!
	!    Compiling skillet v0.4.0 (C:\git\skillet)
	! error[E0433]: failed to resolve: use of undeclared crate or module `st`
	!  --> src\main.rs:6:5
	!   |
	! 6 | use st::path::PathBuf;
	!   |     ^^ use of undeclared crate or module `st`
	!   |
	! help: there is a crate or module with a similar name
	!   |
	! 6 | use std::path::PathBuf;
	!   |     ~~~
	!
	! """

	!!col = str(span%start - context%lines(i) + 1)
	!col = to_str(lexer%previous_token%col)
	icol = lexer%current_token%col
	col = to_str(icol)

	fg1 = fg_bright_cyan
	!fg1 = fg_bright_blue
	rst = color_reset
	!str_i = to_str(lexer%previous_token%line)
	str_i = to_str(lexer%current_token%line)

	!text = lexer%current_token%text
	text = lexer%line_str%trim()

	length = len(lexer%current_token%text)
	str_i_len = len(str_i)
	!print *, 'line # = ', i

	! Pad spaces the same length as the line number string
	spaces = repeat(' ', str_i_len + 2)

	print *, "text = ", text

	! TODO: convert tabs to spaces for easy, consistent alignment

	underline = LINE_FEED//fg1//spaces(2:)//"--> "//rst//lexer%stream%src_file &
		//":"//str_i//":"//col//LINE_FEED &
		//fg1//     spaces//"| "//LINE_FEED &
		//fg1//" "//str_i//" | "//rst//text//LINE_FEED &
		//fg1//     spaces//"| " &
		!//repeat(' ', max(span%start - context%lines(i), 0)) &
		//repeat(' ', max(icol-1, 0)) &
		//fg_bright_red//repeat('^', length)

	print *, "underline = "
	print "(a)", underline

!	underline = line_feed//fg1//spaces(2:)//"--> "//rst//context%src_file &
!		//":"//str_i//":"//col//line_feed &
!		//fg1//     spaces//"| "//line_feed &
!		//fg1//" "//str_i//" | "//rst//text//line_feed &
!		//fg1//     spaces//"| " &
!		//repeat(' ', max(span%start - context%lines(i), 0)) &
!		//fg_bright_red//repeat('^', length)

!********

!	err = err_prefix &
!		//'`&` reference to unexpected expression kind.  references can only ' &
!		//'be made to variable name expressions' &
!		//underline(context, span)//" non-name `&` ref"//color_reset

end subroutine err_bad_obj_delim

function get_closest_key(obj, key) result(closest)
	! Assuming `key` wasn't found in obj, get the most similarly-spelled one
	! that actually exists
	!
	! Convert to lowercase before comparing dist. Of course, use original case
	! for final returned "closest" value
	type(json_val_t), intent(in) :: obj
	character(len=*), intent(in) :: key
	character(len=:), allocatable :: closest
	!********
	character(len=:), allocatable :: key_low, obj_low
	integer :: i, dist, min_dist
	closest = ""
	min_dist = huge(min_dist)
	key_low = to_lower(key)
	do i = 1, size(obj%keys)
		if (.not. allocated(obj%keys(i)%str)) cycle
		obj_low = to_lower(obj%keys(i)%str)
		dist = levenshtein(key, obj_low)
		if (dist >= min_dist) cycle
		min_dist = dist
		closest = obj%keys(i)%str
	end do
end function get_closest_key

integer function get_map_idx(obj, key, found) result(idx)
	! Find the index of the key in its parent object.  Only return its index to
	! avoid a heavy value copy
	type(json_val_t), intent(in) :: obj
	character(len=*), intent(in) :: key
	logical, intent(out) :: found
	!********
	integer(kind=4) :: hash, n

	found = .false.
	hash = djb2_hash(key)
	n = size(obj%keys)
	idx = modulo(hash, n) + 1
	do
		if (.not. allocated(obj%keys(idx)%str)) then
			! Empty slot, key not found
			exit
		else if (is_str_eq(obj%keys(idx)%str, key)) then
			found = .true.
			exit
		else
			! Collision, try next index (linear probing)
			idx = modulo(idx, n) + 1
		end if
	end do

end function get_map_idx

subroutine set_map(json, obj, key, val)
	type(json_t), intent(inout) :: json
	type(json_val_t), intent(inout) :: obj
	character(len=*), intent(in) :: key
	type(json_val_t), intent(inout) :: val  ! intent out because it gets moved instead of copied
	!********
	integer(kind=8) :: i, ii, n, n0, old_nkeys ! TODO: kind 4?
	integer(kind=4), allocatable :: old_idx(:)
	type(str_t), allocatable :: old_keys(:)
	type(json_val_t), allocatable :: old_vals(:)

	n0 = size(obj%keys)
	if (obj%nkeys * 2 >= n0) then
		! Resize the entries array if load factor exceeds 0.5
		n = n0 * 2
		old_nkeys = obj%nkeys

		! Just manually manage idx growth if we have to do it here anyway.
		! Otherwise it could be an i64_vec_t
		call move_alloc(obj%keys, old_keys)
		call move_alloc(obj%vals, old_vals)
		call move_alloc(obj%idx, old_idx)

		allocate(obj%keys (n))
		allocate(obj%vals (n))
		allocate(obj%idx  (n))
		obj%nkeys = 0

		do ii = 1, old_nkeys
			i = old_idx(ii)
			call set_map_core(json, obj, old_keys(i)%str, old_vals(i))
		end do

		deallocate(old_idx)
		deallocate(old_vals)
		deallocate(old_keys)
	end if
	call set_map_core(json, obj, key, val)

end subroutine set_map

subroutine set_map_core(json, obj, key, val)
	type(json_t), intent(inout) :: json
	type(json_val_t), intent(inout) :: obj
	character(len=*), intent(in) :: key
	type(json_val_t), intent(inout) :: val
	!********
	integer(kind=4) :: hash, idx, n

	!print *, "set_map_core: key = "//quote(key)

	hash = djb2_hash(key)
	n = size(obj%keys)
	!print *, "n = ", n
	!print *, "hash = ", hash

	! TODO: my other map implementations need modulo instead of mod, which can
	! overflow become negative for long strs
	idx = modulo(hash, n) + 1
	do
		if (.not. allocated(obj%keys(idx)%str)) then
			! Empty slot found, insert new entry
			obj%keys(idx)%str = key
			if (DEBUG > 0) print *, "key = "//quote(obj%keys(idx)%str)
			call move_val(val, obj%vals(idx))
			obj%nkeys = obj%nkeys + 1
			obj%idx( obj%nkeys ) = idx  ! the first duplicate instance determines insertion index order
			!print *, "pushing idx ", idx
			exit
		else if (is_str_eq(obj%keys(idx)%str, key)) then
			! Key already exists, update value
			if (json%error_duplicate_keys) then
				call panic("duplicate key "//quote(key))
			end if
			if (.not. json%first_duplicate) then
				call move_val(val, obj%vals(idx))
			end if
			exit
		else
			! Collision, try next index (linear probing)
			idx = modulo(idx, n) + 1
		end if
	end do

end subroutine set_map_core

subroutine move_val(src, dst)
	! A copy constructor could be added if needed, but it's best to avoid for performance
	type(json_val_t), intent(inout) :: src
	type(json_val_t), intent(out) :: dst
	!********
	dst%type = src%type
	select case (src%type)
	case (OBJ_TYPE)
		dst%nkeys = src%nkeys
		call move_alloc(src%keys , dst%keys)
		call move_alloc(src%vals , dst%vals)
		call move_alloc(src%idx  , dst%idx)

	case (ARR_TYPE)
		dst%narr = src%narr
		call move_alloc(src%arr, dst%arr)

	case default
		! Lightweight scalars are actually just copied
		dst%sca = src%sca
	end select

end subroutine move_val

recursive subroutine copy_val(dst, src)
	! TODO: try to avoid
	class(json_val_t), intent(in) :: src
	class(json_val_t), intent(out) :: dst
	!********
	integer :: i

	dst%type = src%type
	select case (src%type)
	case (OBJ_TYPE)
		dst%nkeys = src%nkeys
		dst%keys  = src%keys
		dst%idx   = src%idx

		allocate(dst%vals( size(src%vals) ))
		do i = 1, size(src%vals)
			! Recurse
			if (.not. allocated(src%keys(i)%str)) cycle
			call copy_val(dst%vals(i), src%vals(i))
		end do

	case (ARR_TYPE)
		dst%narr = src%narr
		!allocate(dst%arr( size(src%arr) ))
		allocate(dst%arr( src%narr ))
		!do i = 1, size(src%arr)
		do i = 1, src%narr
			! Recurse
			call copy_val(dst%arr(i), src%arr(i))
		end do

	case default
		dst%sca = src%sca
	end select

end subroutine copy_val

function new_file_stream(filename) result(stream)
	character(len=*), intent(in) :: filename
	type(stream_t) :: stream
	!********
	integer :: io
	stream%type = FILE_STREAM
	stream%src_file = filename
	open(file = filename, newunit = stream%unit, action = "read", access = "stream", iostat = io)
	!print *, "opened stream unit "//to_str(stream%unit)
	if (io /= EXIT_SUCCESS) call panic("can't open file "//quote(filename))
end function new_file_stream

function new_str_stream(str) result(stream)
	character(len=*), intent(in) :: str
	type(stream_t) :: stream
	!********
	stream%type = STR_STREAM
	stream%str = str
	stream%pos = 1
	stream%src_file = "<STR_STREAM>"
end function new_str_stream

subroutine print_file_tokens(filename)
	character(len=*), intent(in) :: filename
	call print_stream_tokens(new_file_stream(filename))
end subroutine print_file_tokens

subroutine print_str_tokens(str)
	character(len=*), intent(in) :: str
	call print_stream_tokens(new_str_stream(str))
end subroutine print_str_tokens

subroutine print_stream_tokens(stream)
	type(stream_t) :: stream
	!********
	type(lexer_t) :: lexer
	type(str_builder_t) :: sb
	type(token_t) :: token

	! Note that any final trailing whitespace gets lumped in with EOF_TOKEN.
	! This is probably ok

	! TODO: refactor this as a string converter and add a test. Could be useful
	! for testing new stream types (e.g. stdin) or benchmarking performance of
	! streaming vs loading everything in memory up front
	sb = new_str_builder()
	call sb%push('tokens = '//LINE_FEED//'<<<'//LINE_FEED)
	lexer = new_lexer(stream)
	do
		token = lexer%current_token
		call sb%push("    " &
			//"<"//          token%text  //"> " &
			//"<"//kind_name(token%kind )//">"  &
			//LINE_FEED &
		)
		! This prints the eof token, whereas checking %is_eof exits one token earlier
		if (token%kind == EOF_TOKEN) exit
		call lexer%next_token()
	end do
	call sb%push(">>>"//LINE_FEED)
	write(*, "(a)") sb%trim()

end subroutine print_stream_tokens

function kind_name(kind)
	! TODO: consider auto-generating
	integer, intent(in) :: kind
	character(len = :), allocatable :: kind_name
	character(len = *), parameter :: names(*) = [ &
			"EOF_TOKEN        ", & ! 1
			"BAD_TOKEN        ", & ! 2
			"WHITESPACE_TOKEN ", & ! 3
			"I64_TOKEN        ", & ! 4
			"I64_TYPE         ", & ! 5
			"PLUS_TOKEN       ", & ! 6
			"MINUS_TOKEN      ", & ! 7
			"HASH_TOKEN       ", & ! 8
			"COMMA_TOKEN      ", & ! 9
			"COLON_TOKEN      ", & ! 10
			"RBRACKET_TOKEN   ", & ! 11
			"LBRACKET_TOKEN   ", & ! 12
			"RBRACE_TOKEN     ", & ! 13
			"LBRACE_TOKEN     ", & ! 14
			"STR_TYPE         ", & ! 15
			"STR_TOKEN        ", & ! 16
			"OBJ_TYPE         ", & ! 17
			"ARR_TYPE         ", & ! 18
			"FILE_STREAM      ", & ! 19
			"STR_STREAM       ", & ! 20
			"NULL_TOKEN       ", & ! 21
			"NULL_TYPE        ", & ! 22
			"BOOL_TOKEN       ", & ! 23
			"BOOL_TYPE        ", & ! 24
			"F64_TOKEN        ", & ! 25
			"F64_TYPE         ", & ! 26
			"unknown          "  & ! inf (trailing comma hack)
		]

	if (.not. (1 <= kind .and. kind <= size(names))) then
		kind_name = "unknown"
		return
	end if
	kind_name = trim(names(kind))

end function kind_name

function djb2_hash(str) result(hash)
	! DJB2 hash function implementation
	character(len=*), intent(in) :: str
	integer(kind=4) :: hash
	integer :: i

	hash = 5381
	do i = 1, len(str)
		hash = ((hash * 33) + iachar(str(i:i)))  ! hash * 33 + c
	end do
end function djb2_hash

integer function levenshtein(s, t)
	! Get the Levenshtein distance between two strings `s` and `t`
	use jsonf__blarg
	character(len=*), intent(in) :: s, t
	!********
	integer :: m, n, i, j, deletion_cost, insertion_cost, substitution_cost
	integer, allocatable :: v0(:), v1(:), tmp(:)

	m = len(s)
	n = len(t)
	v0 = range_i32(0, n)
	allocate(v1(n+1))

	do i = 1, m
		v1(1) = i+1
		do j = 1, n
			deletion_cost  = v0(j+1) + 1
			insertion_cost = v1(j) + 1
			if (s(i:i) == t(j:j)) then
				substitution_cost = v0(j)
			else
				substitution_cost = v0(j) + 1
			end if
			v1(j+1) = min(deletion_cost, insertion_cost, substitution_cost)
		end do

		! swap v0 with v1
		call move_alloc(v0, tmp)  !tmp=v0
		call move_alloc(v1, v0)   !v0=v1
		call move_alloc(tmp, v1)  !v1=tmp
	end do
	levenshtein = v0(n+1)

end function levenshtein

end module jsonf

