
module jsonf

	use utils_m
	implicit none

	! TODO:
	! - test object top-level json
	! - test array top-level json
	! - test heterogeneous arrays
	! - test `null` literal
	! - test nested objects/arrays
	! - test error handling: invalid tokens, unterminated strings, invalid numbers
	! - test large files
	! - test unicode in strings
	!   * note that hex literals can be part of a unicode sequence in JSON, but
	!     numeric literals are never hex, octal, or binary
	! - test escape sequences in strings
	! - test comments -- nonstandard but use "#" or another char if requested by some option
	! - test diagnostics reporting line/column numbers
	! - test pretty-printing output option
	! - test streaming parsing from file 1-char at a time

	integer, parameter :: DEBUG = 1

	type sca_t
		! Scalar value type -- primitive bool, int, float, str, or null, but
		! *not* arrays or objects
		integer :: type
		logical :: bool
		integer(kind=8) :: i64
		real(kind=8) :: f64
		character(len=:), allocatable :: str
	end type sca_t

	type val_t
		! JSON value type -- scalar, array, or object
		integer :: type
		type(sca_t) :: sca

		type(str_vec_t) :: key
		type(val_t), allocatable :: val(:)

		!type(arr_t) :: arr  ! TODO: arrays

		contains
			procedure :: &
				to_str => val_to_str
		!	procedure :: is_scalar => val_is_scalar
		!	procedure :: is_object => val_is_object
		!	procedure :: is_array  => val_is_array
	end type val_t

	type json_t
		! JSON top-level type
		!type(val_t), allocatable :: root
		type(val_t) :: root

		contains
			procedure :: &
				!to_str    => to_str_json, &
				read_file => read_file_json, &
				read_str  => read_str_json, &
				parse     => parse_json
	end type json_t

	type token_t
		integer :: kind
		integer(kind=8) :: pos
		character(len=:), allocatable :: text
		type(sca_t) :: sca
	end type token_t

	type token_vec_t
		type(token_t), allocatable :: vec(:)
		integer(kind=8) :: len_, cap
		contains
			procedure :: &
				push => push_token, &
				trim => trim_token_vec
	end type token_vec_t

	type lexer_t
		character(len=:), allocatable :: text
		integer(kind=8) :: pos
		type(str_vec_t) :: diagnostics
		contains
			procedure :: lex, current => lexer_current, peek => lexer_peek
	end type lexer_t

	integer, parameter :: &
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

character function lexer_current(lexer)
	class(lexer_t) :: lexer
	lexer_current = lexer%peek(0)
end function lexer_current

character function lexer_lookahead(lexer)
	class(lexer_t) :: lexer
	lexer_lookahead = lexer%peek(1)
end function lexer_lookahead

character function lexer_peek(lexer, offset)
	class(lexer_t) :: lexer
	integer, intent(in) :: offset
	!********
	integer(kind=8) :: pos
	pos = lexer%pos + offset
	if (pos < 1 .or. pos > len(lexer%text)) then
		lexer_peek = null_char
		return
	end if
	lexer_peek = lexer%text(pos: pos)
end function lexer_peek

function new_literal(type, bool, i64, f64, str_) result(lit)
	integer, intent(in) :: type
	!********
	integer(kind=8) , intent(in), optional :: i64
	real   (kind=8) , intent(in), optional :: f64
	logical         , intent(in), optional :: bool
	character(len=*), intent(in), optional :: str_
	type(sca_t) :: lit

	lit%type = type
	if (present(bool)) lit%bool    = bool
	if (present(f64 )) lit%f64     = f64
	if (present(i64 )) lit%i64     = i64
	if (present(str_)) lit%str = str_

end function new_literal

function lex(lexer) result(token)
	use utils_m
	class(lexer_t) :: lexer
	type(token_t) :: token
	!********
	character(len=:), allocatable :: text, text_strip
	integer :: io
	integer(kind=8) :: start, end_, i64
	logical :: float_
	type(str_builder_t) :: sb
	type(sca_t) :: sca

	if (DEBUG > 2) then
		write(*,*) "lex: pos = "//to_str(lexer%pos)
	end if

	if (lexer%pos > len(lexer%text)) then
		token = new_token(EOF_TOKEN, lexer%pos, null_char)
		return
	end if

	start = lexer%pos
	if (DEBUG > 2) write(*,*) "lex: current char = "//quote(lexer%current())

	if (is_whitespace(lexer%current())) then
		do while (is_whitespace(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)
		token = new_token(whitespace_token, start, text)
		return
	end if

	if (is_digit_under(lexer%current())) then
		! Numeric decimal integer or float

		float_ = .false.

		do while (is_float_under(lexer%current()))

			if (is_sign(lexer%current()) .and. .not. &
				is_expo(lexer%peek(-1))) exit

			float_ = float_ .or. .not. is_digit_under(lexer%current())

			lexer%pos = lexer%pos + 1
		end do
		end_ = lexer%pos

		text = lexer%text(start: end_ - 1)
		text_strip = rm_char(text, "_")

		if (float_) then
			call panic("float parsing not implemented yet")  ! TODO
		else  ! i64
			read(text_strip, *, iostat = io) i64
			if (DEBUG > 0) write(*,*) "lex: parsed i64 = "//to_str(i64)
			if (io == exit_success) then
				sca   = new_literal(I64_TYPE, i64 = i64)
				token = new_token(I64_TOKEN, start, text, sca)
			end if
		end if

		!print *, 'float text = ', quote(text)
		return
	end if

	if (lexer%current() == '"') then
		! String literal
		lexer%pos = lexer%pos + 1  ! Skip opening quote
		start = lexer%pos

		sb = new_str_builder()
		do
			! TODO: test str escape rules. Only 8 characters or a unicode sequence are allowed to follow a backslash
			if (lexer%current() == "\") then
				lexer%pos = lexer%pos + 1
			end if

			if (lexer%current() == '"') then
				lexer%pos = lexer%pos + 1
				exit
			end if

			call sb%push(lexer%current())
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos - 2)

		if (lexer%pos > len(lexer%text)) then
			! Unterminated string
			token = new_token(BAD_TOKEN, start, text)
			call panic("unterminated string literal")  ! TODO: diagnostics
			return
		end if

		sca = new_literal(STR_TYPE, str_ = sb%trim())
		if (DEBUG > 0) write(*,*) "lex: parsed string = "//quote(sca%str)
		token = new_token(STR_TOKEN, start, text, sca)

		return
	end if

	select case (lexer%current())
	case ("+")
		token = new_token(PLUS_TOKEN, lexer%pos, lexer%current())
	case ("-")
		token = new_token(MINUS_TOKEN, lexer%pos, lexer%current())
	case ("{")
		token = new_token(LBRACE_TOKEN, lexer%pos, lexer%current())
	case ("}")
		token = new_token(RBRACE_TOKEN, lexer%pos, lexer%current())
	case ("[")
		token = new_token(LBRACKET_TOKEN, lexer%pos, lexer%current())
	case ("]")
		token = new_token(RBRACKET_TOKEN, lexer%pos, lexer%current())
	case (":")
		token = new_token(COLON_TOKEN, lexer%pos, lexer%current())
	case (",")
		token = new_token(COMMA_TOKEN, lexer%pos, lexer%current())
	case ("#")
		token = new_token(HASH_TOKEN, lexer%pos, lexer%current())

	case default
		!print *, 'bad token text = ', quote(lexer%current())

		token = new_token(BAD_TOKEN, lexer%pos, lexer%current())
		!! TODO: implement span_t and diagnostics
		!span = new_span(lexer%pos, len(lexer%current()))
		!call lexer%diagnostics%push( &
		!	err_unexpected_char(lexer%context, &
		!	span, lexer%current()))
		call panic("unexpected character: "//quote(lexer%current()))

	end select

	lexer%pos = lexer%pos + 1

end function lex

function new_token(kind, pos, text, sca) result(token)
	integer :: kind
	integer(kind=8) :: pos
	character(len=*) :: text
	type(sca_t), optional :: sca
	type(token_t) :: token

	token%kind = kind
	token%pos  = pos
	token%text = text
	if (present(sca)) token%sca = sca

end function new_token

function new_token_vec() result(vec)
	type(token_vec_t) :: vec
	vec%len_ = 0
	vec%cap = 2  ! I think a small default makes sense here
	allocate(vec%vec( vec%cap ))
end function new_token_vec

function new_lexer(text, src_file) result(lexer)
	character(len=*) :: text, src_file
	type(lexer_t) :: lexer
	!********
	integer :: i, i0, nlines

	integer, allocatable :: lines(:)

	lexer%text = text
	lexer%pos  = 1
	lexer%diagnostics = new_str_vec()

	! Count lines
	nlines = 0
	i = 0
	do
		i = i + 1
		if (i > len(text)) exit

		if (i == len(text) .or. &
			text(i:i) == line_feed .or. &
			text(i:i) == carriage_return) then

			nlines = nlines + 1
		end if

	end do
	!print *, 'nlines = ', nlines

	allocate(lines(nlines + 1))

	! Get character indices for the start of each line and save them in lines(:)
	nlines = 0
	i = 0
	i0 = 0
	do
		i = i + 1
		if (i > len(text)) exit

		if (i == len(text) .or. &
			text(i:i) == line_feed .or. &
			text(i:i) == carriage_return) then

			nlines = nlines + 1
			lines(nlines) = i0 + 1
			i0 = i
		end if

	end do
	lines(nlines + 1) = len(text) + 1

	!print *, 'lines = ', lines

	if (DEBUG > 1) then
		write(*,*) 'lines = '
		do i = 1, nlines
			write(*, '(i5,a)') i, ' | '//text(lines(i): lines(i+1) - 2)
		end do
	end if

end function new_lexer

subroutine read_file_json(json, filename)
	class(json_t) :: json
	character(len=*), intent(in) :: filename
	!********
	character(len=:), allocatable :: str

	! TODO: stream chars one at a time
	str = read_file(filename)
	call json%parse(str)

end subroutine read_file_json

subroutine read_str_json(json, str)
	class(json_t), intent(inout) :: json
	character(len=*), intent(in) :: str
	!********
	call json%parse(str)
end subroutine read_str_json

subroutine parse_json(json, str)
	class(json_t), intent(inout) :: json
	character(len=*), intent(in) :: str
	!********
	integer(kind=8) :: pos
	type(lexer_t) :: lexer
	type(token_t) :: token
	type(token_vec_t) :: tokens

	! Lex and get an array of tokens
	!
	! TODO: can we make a streaming tokenizer to get one (or a few lookahead)
	! tokens at a time instead of making an array of all tokens up front?
	tokens = new_token_vec()
	lexer = new_lexer(str, "<dummy-filename>")
	do
		token = lexer%lex()

		if (token%kind /= WHITESPACE_TOKEN .and. &
		    token%kind /= BAD_TOKEN) then
			call tokens%push(token)
		end if

		if (token%kind == EOF_TOKEN) exit
	end do
	!tokens = tokens%vec(1: tokens%len_)  ! trim
	call tokens%trim()

	pos = 1
	!print *, 'tokens%len_ = ', tokens%len_

	if (DEBUG > 0) then
		write(*,*) tokens_to_str(tokens)
	end if

	write(*,*) "Parsing JSON tokens ..."

	! Maybe a parser type would be useful to encapsulate pos and diagnostics
	call parse_root(json%root, tokens, pos)

end subroutine parse_json

subroutine parse_root(json, tokens, pos)
	!class(json_t) :: json
	type(val_t), intent(out) :: json
	type(token_vec_t) :: tokens
	integer(kind=8), intent(inout) :: pos
	!********
	print *, "Starting parse_root()"
	print *, "pos = ", pos
	do
		print *, "tok kind = ", kind_name(tokens%vec(pos)%kind)

		select case (tokens%vec(pos)%kind)
		case (EOF_TOKEN)
			exit
		case (LBRACE_TOKEN)
			! Object
			!call panic("object parsing not implemented yet")  ! TODO
			call parse_obj(json, tokens, pos)
		case (LBRACKET_TOKEN)
			! Array
			call panic("array parsing not implemented yet")  ! TODO
		case default
			call panic("expected object or array at root")
		end select

	end do

end subroutine parse_root

subroutine match(tokens, pos, kind)
	! TODO: return a token as a fn? Might be useful to have the matched token's position in case of later diagnostics
	type(token_vec_t) :: tokens
	integer(kind=8), intent(inout) :: pos
	integer, intent(in) :: kind
	!********
	if (tokens%vec(pos)%kind == kind) then
		pos = pos + 1
		return
	end if

	! Syntran just advances to the next token on mismatch. Does that have an advantage?
	call panic("expected token of kind")

end subroutine match

function val_to_str(this) result(str_)
	class(val_t) :: this
	character(len = :), allocatable :: str_
	!********
	type(str_builder_t) :: sb
	sb = new_str_builder()
	select case (this%type)
	case (STR_TYPE)
		call sb%push("str: "//quote(this%sca%str))
	case (I64_TYPE)
		call sb%push("i64: "//to_str(this%sca%i64))
	case (OBJ_TYPE)
		call sb%push("object: {...}")
		! TODO: recurse
	case default
		call sb%push("val_to_str: unknown type "//kind_name(this%type))
	end select
	str_ = sb%trim()
end function val_to_str

subroutine parse_val(json, tokens, pos)
	type(val_t), intent(out) :: json
	type(token_vec_t) :: tokens
	integer(kind=8), intent(inout) :: pos
	!********
	select case (tokens%vec(pos)%kind)
	case (STR_TOKEN)
		!print *, "value (string) = ", tokens%vec(pos)%sca%str
		json%type = STR_TYPE
		json%sca = tokens%vec(pos)%sca
		pos = pos + 1
	case (I64_TOKEN)
		!print *, "value (i64) = ", tokens%vec(pos)%sca%i64
		json%type = I64_TYPE
		json%sca = tokens%vec(pos)%sca
		pos = pos + 1
	case default
		call panic("unexpected value type in object")
	end select
end subroutine parse_val

subroutine parse_obj(json, tokens, pos)
	type(val_t), intent(out) :: json
	type(token_vec_t) :: tokens
	integer(kind=8), intent(inout) :: pos
	!********
	character(len=:), allocatable :: key
	type(val_t) :: val

	print *, "Starting parse_obj()"
	print *, "pos = ", pos

	call match(tokens, pos, LBRACE_TOKEN)
	json%type = OBJ_TYPE

	do
		if (tokens%vec(pos)%kind == RBRACE_TOKEN) then
			! End of object
			pos = pos + 1
			exit
		end if

		call match(tokens, pos, STR_TOKEN)
		key = tokens%vec(pos-1)%sca%str
		print *, "key = ", key

		call match(tokens, pos, COLON_TOKEN)
		call parse_val(val, tokens, pos)
		print *, "val = ", val%to_str()

		! TODO: store key-value pair in json object

		! Expect comma or end of object
		select case (tokens%vec(pos)%kind)
		case (COMMA_TOKEN)
			! TODO: test trailing commas. Make them an error by default but provide option to allow
			pos = pos + 1
		case (RBRACE_TOKEN)
			! End of object
			pos = pos + 1
			exit
		case default
			call panic("expected ',' or '}' after key-value pair in object")
		end select
	end do

end subroutine parse_obj

subroutine trim_token_vec(this)
	class(token_vec_t) :: this
	!********
	type(token_t), allocatable :: tmp(:)
	this%vec = this%vec(1: this%len_)
	this%cap = this%len_
end subroutine trim_token_vec

subroutine push_token(vec, val)
	class(token_vec_t) :: vec
	type(token_t) :: val
	!********
	integer(kind=8) :: tmp_cap
	type(token_t), allocatable :: tmp(:)

	vec%len_ = vec%len_ + 1
	if (vec%len_ > vec%cap) then
		tmp_cap = 2 * vec%len_
		allocate(tmp( tmp_cap ))
		tmp(1: vec%cap) = vec%vec

		call move_alloc(tmp, vec%vec)
		vec%cap = tmp_cap
	end if
	vec%vec( vec%len_ ) = val

end subroutine push_token

module function tokens_to_str(tokens) result(str_)
	type(token_vec_t) :: tokens
	character(len = :), allocatable :: str_
	!********
	integer :: i
	type(str_builder_t) :: sb

	sb = new_str_builder()
	call sb%push('tokens = '//line_feed//'<<<'//line_feed)
	do i = 1, tokens%len_
		call sb%push("    " &
			//"<"//          tokens%vec(i)%text  //"> " &
			//"<"//kind_name(tokens%vec(i)%kind )//">"  &
			//line_feed &
		)
	end do
	call sb%push(">>>"//line_feed)
	str_ = sb%trim()

end function tokens_to_str

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
			"unknown          "  & ! inf (trailing comma hack)
		]

	if (.not. (1 <= kind .and. kind <= size(names))) then
		kind_name = "unknown"
		return
	end if
	kind_name = trim(names(kind))

end function kind_name

end module jsonf

