
module jsonf

	use utils_m
	implicit none

	! TODO:
	! - test object top-level json
	! - test stream parsing
	!   * lex 1 (or a couple lookahead) token(s) at a time
	!   * beware the peek(-1) token (i.e. previous) in float lexer
	!   * read 1 char from file at a time
	! - ci/cd
	!   * docker
	!   * github actions
	!   * test on windows, linux (macos?)
	!   * test with different fortran compilers
	!   * fpm and cmake
	!   * release and debug profiles/configurations
	! - test array top-level json
	!   * get file/token streaming down before starting array work
	! - test heterogeneous arrays
	! - test `null` literal
	! - test nested objects/arrays
	!   * test nested objs where inner obj has same keys as outer obj
	! - check json-fortran, jq, and other similar projects for features to add
	! - test re-entry with re-using one object to load multiple JSON inputs in
	!   sequence. might find bugs with things that need to be deallocated first
	! - test error handling: invalid tokens, unterminated strings, invalid numbers
	! - prune unused code copied from other template projects (blarg etc.)
	! - test large files
	! - test unicode in strings
	!   * note that hex literals can be part of a unicode sequence in JSON, but
	!     numeric literals are never hex, octal, or binary
	! - test escape sequences in strings
	! - test comments -- nonstandard but use "#" or another char if requested by some option
	! - test diagnostics reporting line/column numbers
	! - test pretty-printing output option

	integer, parameter :: DEBUG = 0

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

		! TODO: should maps also have an ordering index array, to preserve insertion order?  Not part of json standard but very helpful for consistent str/print output
		integer(kind=8) :: nkey = 0
		type(str_t), allocatable :: key(:)  ! TODO: rename to keys/vals? Singular seems confusing
		type(val_t), allocatable :: val(:)

		!type(arr_t) :: arr  ! TODO: arrays

		contains
			procedure :: &
				to_str => val_to_str
	end type val_t

	type json_t
		! JSON top-level type
		type(val_t) :: root

		contains
			procedure :: &
				write     => write_json, &
				print     => print_json, &
				to_str    => json_to_str, &
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

		! Could add more lookaheads if needed, i.e. next_token and peek2_token
		type(token_t) :: current_token, previous_token
		logical :: &
			has_current_token = .false., &
			has_previous_token = .false.

		contains
			procedure :: &
				lex, &
				next => lexer_next, &
				current => lexer_current, &
				current_kind => lexer_current_kind, &
				peek => lexer_peek
	end type lexer_t

	integer, parameter :: &
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

character function lexer_current(lexer)
	! Current char
	class(lexer_t) :: lexer
	lexer_current = lexer%peek(0)
end function lexer_current

subroutine lexer_next(lexer)
	! Advance to the next non-whitespace token
	class(lexer_t), intent(inout) :: lexer
	!********
	lexer%previous_token = lexer%current_token
	lexer%current_token = lexer%lex()
	do while (lexer%current_token%kind == WHITESPACE_TOKEN)
		lexer%current_token = lexer%lex()
		if (lexer%current_token%kind == EOF_TOKEN) exit
	end do
end subroutine lexer_next

integer function lexer_current_kind(lexer)
	class(lexer_t) :: lexer
	type(token_t) :: token

	if (lexer%has_current_token) then
		lexer%current_token = lexer%current_token
	else
		lexer%current_token = lexer%lex()
		lexer%has_current_token = .true.
	end if
	lexer_current_kind = lexer%current_token%kind

end function lexer_current_kind

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
				call sb%push(lexer%current())
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
	!print *, "str = "//LINE_FEED//str
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
	type(lexer_t) :: lexer
	type(token_t) :: token
	type(token_vec_t) :: tokens

	lexer = new_lexer(str, "<dummy-filename>")
	write(*,*) "Parsing JSON tokens ..."

	! Maybe a parser type would be useful to encapsulate diagnostics. Or just put them in the lexer to simplify things?
	call parse_root(lexer, json%root)

end subroutine parse_json

subroutine parse_root(lexer, json)
	type(lexer_t), intent(inout) :: lexer
	type(val_t), intent(out) :: json
	!********
	integer :: kind
	if (DEBUG > 0) print *, "Starting parse_root()"
	do
		kind = lexer%current_kind()
		if (DEBUG > 0) print *, "tok kind = ", kind_name(kind)

		select case (kind)
		case (EOF_TOKEN)
			exit
		case (LBRACE_TOKEN)
			call parse_obj(lexer, json)
		case (LBRACKET_TOKEN)
			! Array
			call panic("array parsing not implemented yet")  ! TODO
		case default
			call panic("expected object or array at root")
		end select
	end do
end subroutine parse_root

subroutine match(lexer, kind)
	! TODO: return a token as a fn? Might be useful to have the matched token's position in case of later diagnostics. Or could just use lexer%previous_token in caller
	type(lexer_t), intent(inout) :: lexer
	integer, intent(in) :: kind
	!********
	if (lexer%current_kind() == kind) then
		call lexer%next()
		return
	end if

	! Syntran just advances to the next token on mismatch. Does that have an advantage?
	call panic("expected token of kind "// &
		kind_name(kind)//", but got "// &
		kind_name(lexer%current_kind()))

end subroutine match

! Do these need to be declared recursive? I remember certain fortran compilers being picky about it, even though it's only inderictly recursive.  Same with obj_to_str()
recursive function val_to_str(this) result(str)
	class(val_t) :: this
	character(len = :), allocatable :: str
	!********
	! Don't need a str_builder here since val_t is a single value
	select case (this%type)
	case (STR_TYPE)
		str = quote(this%sca%str)
	case (I64_TYPE)
		str = to_str(this%sca%i64)
	case (OBJ_TYPE)
		str = obj_to_str(this)
	case default
		call panic("val_to_str: unknown type "//kind_name(this%type))
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
		!open(newunit = unit__, file = filename, action = "write", status = "replace")
		open(newunit = unit__, file = filename, action = "write")
	else
		unit__ = OUTPUT_UNIT
	end if
	write(unit__, "(a)") this%root%to_str()

	if (present(filename)) then
		print *, "Closing unit "//to_str(unit__)
		close(unit__)
	end if

end subroutine write_json

subroutine print_json(this, msg)
	class(json_t) :: this
	character(len=*), intent(in), optional :: msg
	!********
	if (present(msg)) then
		write(*, "(a)") msg
	end if
	call write_json(this)
end subroutine print_json

function json_to_str(this) result(str)
	class(json_t) :: this
	character(len = :), allocatable :: str
	!********
	str = this%root%to_str()
end function json_to_str

recursive function obj_to_str(this) result(str)
	! TODO: pass through a json_t object with indent level
	class(val_t) :: this
	character(len = :), allocatable :: str
	!********
	character(len=:), allocatable :: key_str, val_str
	integer(kind=8) :: i
	type(str_builder_t) :: sb

	!! Be careful with debug logging. If you write directly to stdout, it hang forever for inadvertent recursive prints
	!write(ERROR_UNIT, *) "starting obj_to_str()"

	! TODO: add up an incrementor and compare to nkey to decide when to omit last trailing comma

	sb = new_str_builder()
	call sb%push("{"//LINE_FEED)
	do i = 1, size(this%key)
		if (allocated(this%key(i)%str)) then
			!write(ERROR_UNIT, *) "key = "//quote(this%key(i)%str)

			key_str = quote(this%key(i)%str)  ! TODO: quote escaping
			val_str = this%val(i)%to_str()
			call sb%push(key_str//": "//val_str)
			call sb%push(","//LINE_FEED)
		end if
	end do
	call sb%push("}")
	str = sb%trim()
end function obj_to_str

subroutine parse_val(lexer, json)
	type(lexer_t), intent(inout) :: lexer
	type(val_t), intent(out) :: json
	!********
	select case (lexer%current_token%kind)
	case (STR_TOKEN)
		!print *, "value (string) = ", lexer%current_token%sca%str
		json%type = STR_TYPE
		json%sca = lexer%current_token%sca
		call lexer%next()
	case (I64_TOKEN)
		json%type = I64_TYPE
		json%sca = lexer%current_token%sca
		call lexer%next()
	case (LBRACE_TOKEN)
		call parse_obj(lexer, json)
	case (LBRACKET_TOKEN)
		call panic("array parsing not implemented yet")  ! TODO
		!call parse_arr(json, tokens, pos)
	case default
		call panic("unexpected value type in object")
	end select
end subroutine parse_val

subroutine parse_obj(lexer, json)
	type(lexer_t), intent(inout) :: lexer
	type(val_t), intent(out) :: json
	!********
	character(len=:), allocatable :: key
	type(val_t) :: val

	if (DEBUG > 0) print *, "Starting parse_obj()"

	! TODO: make `match()` a lexer class method
	if (DEBUG > 0) print *, "matching LBRACE_TOKEN"
	call match(lexer, LBRACE_TOKEN)
	json%type = OBJ_TYPE

	! Initialize hash map storage
	allocate(json%key(2))
	allocate(json%val(2))
	json%nkey = 0

	do
		if (lexer%current_kind() == RBRACE_TOKEN) then
			! End of object
			call lexer%next()
			exit
		end if

		call match(lexer, STR_TOKEN)
		key = lexer%previous_token%sca%str
		!print *, "key = ", key

		call match(lexer, COLON_TOKEN)
		call parse_val(lexer, val)
		!print *, "val = ", val%to_str()

		! Store key-value pair in json object
		call set_map(json, key, val)

		! Expect comma or end of object
		select case (lexer%current_kind())
		case (COMMA_TOKEN)
			! TODO: test trailing commas. Make them an error by default but provide option to allow
			call lexer%next()
		case (RBRACE_TOKEN)
			! TODO: do we really need exit condition at top and bottom of loop? Make sure we handle empty objects
			call lexer%next()
			exit
		case default
			call panic("expected ',' or '}' after key-value pair in object")
		end select
	end do

	if (DEBUG > 0) then
		write(*,*) "Finished parse_obj(), nkey = "//to_str(json%nkey)
		call print_map("obj =", json)
	end if

end subroutine parse_obj

subroutine print_map(prefix, json)
	character(len=*), intent(in) :: prefix
	type(val_t), intent(in) :: json
	!********
	integer(kind=8) :: i

	write(*,*) prefix
	do i = 1, size(json%key)
		if (allocated(json%key(i)%str)) then
			write(*,*) &
				"  key = "//quote(json%key(i)%str)// &
				", val = "//json%val(i)%to_str()
		end if
	end do

end subroutine print_map

subroutine set_map(json, key, val)
	type(val_t), intent(inout) :: json
	character(len=*), intent(in) :: key
	type(val_t), intent(inout) :: val
	!********
	integer(kind=8) :: i, n, n0
	type(str_t), allocatable :: old_keys(:)
	type(val_t), allocatable :: old_vals(:)

	n0 = size(json%key)
	if (json%nkey * 2 >= n0) then
		! Resize the entries array if load factor exceeds 0.5
		n = n0 * 2
		call move_alloc(json%key, old_keys)
		call move_alloc(json%val, old_vals)
		allocate(json%key(n))
		allocate(json%val(n))
		json%nkey = 0
		do i = 1, n0
			if (allocated(old_keys(i)%str)) then
				call set_map_core(json, old_keys(i)%str, old_vals(i))
			end if
		end do
		deallocate(old_keys)
		deallocate(old_vals)
	end if
	call set_map_core(json, key, val)

end subroutine set_map

subroutine set_map_core(json, key, val)
	type(val_t), intent(inout) :: json
	character(len=*), intent(in) :: key
	type(val_t), intent(inout) :: val
	!********
	integer(8) :: hash, idx, n

	!print *, "set_map_core: key = "//quote(key)

	hash = djb2_hash(key)
	n = size(json%key)
	!print *, "n = ", n
	!print *, "hash = ", hash

	! TODO: my other map implementations need modulo instead of mod
	idx = modulo(hash, n) + 1
	do
		if (.not. allocated(json%key(idx)%str)) then
			! Empty slot found, insert new entry
			json%key(idx)%str = key
			if (DEBUG > 0) print *, "key = "//quote(json%key(idx)%str)
			call move_val(val, json%val(idx))
			json%nkey = json%nkey + 1
			exit
		else if (is_str_eq(json%key(idx)%str, key)) then
			! Key already exists, update value
			!
			! TODO: ban duplicate keys by default, option to allow
			json%val(idx) = val
			exit
		else
			! Collision, try next index (linear probing)
			idx = mod(idx, n) + 1
		end if
	end do

end subroutine set_map_core

subroutine move_val(src, dst)
	! A copy constructor could be added if needed, but it's best to avoid for performance
	type(val_t), intent(out) :: dst
	type(val_t), intent(inout) :: src
	!********
	dst%type = src%type
	select case (src%type)
	case (OBJ_TYPE)
		dst%nkey = src%nkey
		call move_alloc(src%key, dst%key)
		call move_alloc(src%val, dst%val)
	case (ARR_TYPE)
		call panic("array move_val not implemented yet")  ! TODO
	case default
		dst%sca = src%sca
	end select

end subroutine move_val

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
			"ARR_TYPE         ", & ! 18
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
	integer(8) :: hash
	integer :: i

	hash = 5381
	do i = 1, len(str)
		hash = ((hash * 33) + iachar(str(i:i)))  ! hash * 33 + c
	end do
end function djb2_hash

end module jsonf

