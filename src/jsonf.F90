
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

	type stream_t
		integer :: type
		integer :: unit
		integer(kind=8) :: pos
		character(len=:), allocatable :: str
		logical :: is_eof = .false.
		contains
			procedure :: get => get_stream_char
	end type stream_t

	type sca_t
		! Scalar value type -- primitive bool, int, float, str, or null, but
		! *not* arrays or objects
		!
		! TODO: equivalence union?
		integer :: type
		logical :: bool
		integer(kind=8) :: i64
		real(kind=8) :: f64
		character(len=:), allocatable :: str
	end type sca_t

	type val_t
		! JSON value type -- scalar, array, or object
		!
		! TODO: maybe rename to json_val_t? It might end up user-facing
		integer :: type
		type(sca_t) :: sca

		! TODO: should maps also have an ordering index array, to preserve
		! insertion order?  Not part of json standard but very helpful for
		! consistent str/print output
		!
		! jq echos with consistent order
		integer(kind=8) :: nkeys = 0
		type(str_t), allocatable :: keys(:)
		type(val_t), allocatable :: vals(:)

		!type(arr_t) :: arr  ! TODO: arrays
	end type val_t

	character(len=*), parameter :: INDENT_DEFAULT = "    "
	type json_t
		! JSON top-level type
		type(val_t) :: root
		character(len=:), allocatable :: indent
		integer :: indent_level

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
		integer(kind=8) :: len, cap
		contains
			procedure :: &
				push => push_token, &
				trim => trim_token_vec
	end type token_vec_t

	type lexer_t
		integer(kind=8) :: pos
		integer         :: line  ! TODO: use this for diags. set and increment somewhere
		type(str_vec_t) :: diagnostics
		type(stream_t)  :: stream

		! Could add more lookaheads if needed, i.e. next_token and peek2_token
		character     :: current_char , previous_char
		type(token_t) :: current_token, previous_token

		contains
			procedure :: &
				lex, &
				match => lexer_match, &
				next_char  => lexer_next_char, &
				next_token => lexer_next_token, &
				current => lexer_current, &
				current_kind => lexer_current_kind
	end type lexer_t

	integer, parameter :: &
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

character function lexer_current(lexer)
	! Current char
	class(lexer_t) :: lexer

	! TODO: can we just do this once instead of checking a bool every time?
	! Same for token in lexer_current_kind()
	lexer_current = lexer%current_char

end function lexer_current

subroutine lexer_next_char(lexer)
	class(lexer_t), intent(inout) :: lexer
	!********
	lexer%previous_char = lexer%current_char
	lexer%current_char =  lexer%stream%get()
end subroutine lexer_next_char

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

	if (lexer%stream%is_eof) then
		token = new_token(EOF_TOKEN, lexer%pos, null_char)
		return
	end if

	start = lexer%pos
	if (DEBUG > 2) write(*,*) "lex: current char = "//quote(lexer%current())

	if (is_whitespace(lexer%current())) then
		sb = new_str_builder()
		do while (is_whitespace(lexer%current()))
			call sb%push(lexer%current())
			call lexer%next_char()
		end do
		text = sb%trim()
		token = new_token(whitespace_token, start, text)
		return
	end if

	! TODO: don't allow underscores as thousands separators by default, but
	! optionally allow any custom separator
	if (is_digit_under(lexer%current())) then
		! Numeric decimal integer or float

		float_ = .false.
		sb = new_str_builder()
		do while (is_float_under(lexer%current()))

			if (is_sign(lexer%current()) .and. .not. &
				is_expo(lexer%previous_char)) exit

			float_ = float_ .or. .not. is_digit_under(lexer%current())

			call sb%push(lexer%current())
			call lexer%next_char()
		end do
		end_ = lexer%pos

		text = sb%trim()
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
		call lexer%next_char()  ! skip opening quote
		start = lexer%pos

		sb = new_str_builder()
		do
			!print *, "lexer current = ", lexer%current()

			! TODO: test str escape rules. Only 8 characters or a unicode sequence are allowed to follow a backslash
			if (lexer%current() == "\") then
				call lexer%next_char()
				call sb%push(lexer%current())
				call lexer%next_char()
			end if

			if (lexer%current() == '"') then
				call lexer%next_char()
				exit
			end if

			call sb%push(lexer%current())
			call lexer%next_char()
		end do
		text = sb%trim()

		if (lexer%stream%is_eof) then
			! Unterminated string
			token = new_token(BAD_TOKEN, start, text)
			call panic("unterminated string literal")  ! TODO: diagnostics
			return
		end if

		sca = new_literal(STR_TYPE, str = text)
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

	call lexer%next_char()

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
	vec%len = 0
	vec%cap = 2  ! I think a small default makes sense here
	allocate(vec%vec( vec%cap ))
end function new_token_vec

function new_lexer(stream) result(lexer)
	type(stream_t) :: stream
	type(lexer_t) :: lexer
	!********
	lexer%pos = 1
	lexer%diagnostics = new_str_vec()
	lexer%stream = stream

	! Get the first char and token on construction instead of checking later if
	! we have them
	lexer%current_char = lexer%stream%get()
	lexer%current_token = lexer%lex()

end function new_lexer

subroutine read_file_json(json, filename)
	class(json_t) :: json
	character(len=*), intent(in) :: filename
	!********
	integer :: io
	type(stream_t) :: stream

	! Stream chars one at a time
	stream%type = FILE_STREAM
	open(file = filename, newunit = stream%unit, action = "read", access = "stream", iostat = io)
	!print *, "opened stream unit "//to_str(stream%unit)
	if (io /= EXIT_SUCCESS) call panic("can't open file "//quote(filename))

	call json%parse(stream)

end subroutine read_file_json

subroutine read_str_json(json, str)
	class(json_t), intent(inout) :: json
	character(len=*), intent(in) :: str
	!********
	type(stream_t) :: stream

	! We have the whole str, but treat it as a stream for consistency with file
	! streaming
	stream%type = STR_STREAM
	stream%str = str
	stream%pos = 1
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

	lexer = new_lexer(stream)
	!write(*,*) "Parsing JSON tokens ..."
	call parse_root(lexer, json%root)

end subroutine parse_json

subroutine parse_root(lexer, root)
	type(lexer_t), intent(inout) :: lexer
	type(val_t), intent(out) :: root
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
			call parse_obj(lexer, root)
		case (LBRACKET_TOKEN)
			! Array
			call panic("array parsing not implemented yet")  ! TODO
		case default
			call panic("expected object or array at root")
		end select
	end do
end subroutine parse_root

subroutine lexer_match(lexer, kind)
	! TODO: return a token as a fn? Might be useful to have the matched token's position in case of later diagnostics. Or could just use lexer%previous_token in caller
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

! Do these need to be declared recursive? I remember certain fortran compilers being picky about it, even though it's only inderictly recursive.  Same with obj_to_str()
recursive function val_to_str(json, val) result(str)
	type(json_t), intent(inout) :: json
	type(val_t), intent(in) :: val
	character(len = :), allocatable :: str
	!********
	! Don't need a str_builder here since val_t is a single value
	select case (val%type)
	case (STR_TYPE)
		str = quote(val%sca%str)
	case (I64_TYPE)
		str = to_str(val%sca%i64)
	case (OBJ_TYPE)
		str = obj_to_str(json, val)
	case default
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
		!open(newunit = unit__, file = filename, action = "write", status = "replace")
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
		write(*, "(a)") msg
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
	
recursive function obj_to_str(json, obj) result(str)
	type(json_t), intent(inout) :: json
	type(val_t), intent(in) :: obj
	character(len = :), allocatable :: str
	!********
	character(len=:), allocatable :: key_str, val_str, indent
	integer(kind=8) :: i
	type(str_builder_t) :: sb

	!! Be careful with debug logging. If you write directly to stdout, it hang forever for inadvertent recursive prints
	!write(ERROR_UNIT, *) "starting obj_to_str()"

	! TODO: add up an incrementor and compare to nkeys to decide when to omit last trailing comma

	sb = new_str_builder()
	call sb%push("{"//LINE_FEED)
	json%indent_level = json%indent_level + 1
	indent = repeat(json%indent, json%indent_level)
	do i = 1, size(obj%keys)
		if (allocated(obj%keys(i)%str)) then
			!write(ERROR_UNIT, *) "key = "//quote(obj%keys(i)%str)

			key_str = quote(obj%keys(i)%str)  ! TODO: quote escaping
			val_str = val_to_str(json, obj%vals(i))
			call sb%push(indent//key_str//": "//val_str)

			!! Output gets weirdly truncated if you do this without the helper variables, no idea why. Looks like a bug in str builder that doesn't copy/realloc correctly, but I'm pretty sure I've ruled that out.  This is way more readable with helper vars anyway
			!call sb%push(indent//quote(obj%keys(i)%str)//": "//val_to_str(json, obj%vals(i)))

			call sb%push(","//LINE_FEED)
		end if
	end do
	json%indent_level = json%indent_level - 1
	indent = repeat(json%indent, json%indent_level)
	call sb%push(indent//"}")
	str = sb%trim()
end function obj_to_str

subroutine parse_val(lexer, val)
	type(lexer_t), intent(inout) :: lexer
	type(val_t), intent(out) :: val
	!********
	select case (lexer%current_token%kind)
	case (STR_TOKEN)
		!print *, "value (string) = ", lexer%current_token%sca%str
		val%type = STR_TYPE
		val%sca = lexer%current_token%sca
		call lexer%next_token()
	case (I64_TOKEN)
		val%type = I64_TYPE
		val%sca = lexer%current_token%sca
		call lexer%next_token()
	case (LBRACE_TOKEN)
		call parse_obj(lexer, val)
	case (LBRACKET_TOKEN)
		call panic("array parsing not implemented yet")  ! TODO
		!call parse_arr(val, tokens, pos)
	case default
		call panic("unexpected value type in object")
	end select
end subroutine parse_val

subroutine parse_obj(lexer, obj)
	type(lexer_t), intent(inout) :: lexer
	type(val_t), intent(out) :: obj
	!********
	character(len=:), allocatable :: key
	type(val_t) :: val

	if (DEBUG > 0) print *, "Starting parse_obj()"

	if (DEBUG > 0) print *, "matching LBRACE_TOKEN"
	call lexer%match(LBRACE_TOKEN)
	obj%type = OBJ_TYPE

	! Initialize hash map storage
	allocate(obj%keys(2))
	allocate(obj%vals(2))
	obj%nkeys = 0

	do
		if (lexer%current_kind() == RBRACE_TOKEN) then
			! End of object
			call lexer%next_token()
			exit
		end if

		call lexer%match(STR_TOKEN)
		key = lexer%previous_token%sca%str
		!print *, "key = ", key

		call lexer%match(COLON_TOKEN)
		call parse_val(lexer, val)
		!print *, "val = ", val%to_str()

		! Store the key-value pair in the object
		call set_map(obj, key, val)

		! Expect comma or end of object
		select case (lexer%current_kind())
		case (COMMA_TOKEN)
			! TODO: test trailing commas. Make them an error by default but provide option to allow
			call lexer%next_token()
		case (RBRACE_TOKEN)
			! TODO: do we really need exit condition at top and bottom of loop? Make sure we handle empty objects
			call lexer%next_token()
			exit
		case default
			call panic("expected ',' or '}' after key-value pair in object")
		end select
	end do

	if (DEBUG > 0) then
		write(*,*) "Finished parse_obj(), nkeys = "//to_str(obj%nkeys)
	end if

end subroutine parse_obj

subroutine set_map(obj, key, val)
	type(val_t), intent(inout) :: obj
	character(len=*), intent(in) :: key
	type(val_t), intent(inout) :: val  ! intent out because it gets moved instead of copied
	!********
	integer(kind=8) :: i, n, n0
	type(str_t), allocatable :: old_keys(:)
	type(val_t), allocatable :: old_vals(:)

	n0 = size(obj%keys)
	if (obj%nkeys * 2 >= n0) then
		! Resize the entries array if load factor exceeds 0.5
		n = n0 * 2
		call move_alloc(obj%keys, old_keys)
		call move_alloc(obj%vals, old_vals)
		allocate(obj%keys(n))
		allocate(obj%vals(n))
		obj%nkeys = 0
		do i = 1, n0
			if (allocated(old_keys(i)%str)) then
				call set_map_core(obj, old_keys(i)%str, old_vals(i))
			end if
		end do
		deallocate(old_keys)
		deallocate(old_vals)
	end if
	call set_map_core(obj, key, val)

end subroutine set_map

subroutine set_map_core(obj, key, val)
	type(val_t), intent(inout) :: obj
	character(len=*), intent(in) :: key
	type(val_t), intent(inout) :: val
	!********
	integer(8) :: hash, idx, n

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
			exit
		else if (is_str_eq(obj%keys(idx)%str, key)) then
			! Key already exists, update value
			!
			! TODO: ban duplicate keys by default, option to allow. Maybe
			! include an option for first vs last dupe key to take precedence
			call move_val(val, obj%vals(idx))
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
		dst%nkeys = src%nkeys
		call move_alloc(src%keys, dst%keys)
		call move_alloc(src%vals, dst%vals)
	case (ARR_TYPE)
		call panic("array move_val not implemented yet")  ! TODO
	case default
		dst%sca = src%sca
	end select

end subroutine move_val

! TODO: token_vec is never used since token streaming was implemented. Delete
! these, but add a stream_tokens_to_str() fn in test, as a basic test, and for
! developing new stream types (e.g. stdin, network (?))
subroutine trim_token_vec(this)
	class(token_vec_t) :: this
	!********
	this%vec = this%vec(1: this%len)
	this%cap = this%len
end subroutine trim_token_vec

subroutine push_token(vec, val)
	class(token_vec_t) :: vec
	type(token_t) :: val
	!********
	integer(kind=8) :: tmp_cap
	type(token_t), allocatable :: tmp(:)

	vec%len = vec%len + 1
	if (vec%len > vec%cap) then
		tmp_cap = 2 * vec%len
		allocate(tmp( tmp_cap ))
		tmp(1: vec%cap) = vec%vec

		call move_alloc(tmp, vec%vec)
		vec%cap = tmp_cap
	end if
	vec%vec( vec%len ) = val

end subroutine push_token

module function tokens_to_str(tokens) result(str)
	! Unused, but potentially helpful for debugging
	type(token_vec_t) :: tokens
	character(len = :), allocatable :: str
	!********
	integer(kind=8) :: i
	type(str_builder_t) :: sb

	sb = new_str_builder()
	call sb%push('tokens = '//line_feed//'<<<'//line_feed)
	do i = 1, tokens%len
		call sb%push("    " &
			//"<"//          tokens%vec(i)%text  //"> " &
			//"<"//kind_name(tokens%vec(i)%kind )//">"  &
			//line_feed &
		)
	end do
	call sb%push(">>>"//line_feed)
	str = sb%trim()

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
			"FILE_STREAM      ", & ! 19
			"STR_STREAM       ", & ! 20
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

