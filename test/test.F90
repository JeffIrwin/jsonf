
module jsonf__test

	use jsonf__sort
	use jsonf__utils
	use jsonf
	implicit none

	interface is_sorted
		procedure :: is_sorted_i32
	end interface is_sorted

contains

! TODO: make a TEST_STR_EQ routine and macro to not just assert a bool, but also
! log the expected and actualy str values.  Could even underline the first
! different char and some surrounding context.  General-purpose diff is hard,
! but not necessary

#define TEST(val, msg, nfail, ntot) call test_(val, msg, nfail, ntot, __FILE__, __LINE__)
subroutine test_(val, msg, nfail, ntot, file, line)
	logical, intent(in) :: val
	character(len=*), intent(in) :: msg
	integer, intent(inout) :: nfail, ntot
	character(len=*), intent(in) :: file
	integer, intent(in) :: line

	ntot = ntot + 1
	if (.not. val) then
		write(*,*) fg_bold_bright_red // "Test failed: " // color_reset
		write(*,*) "    ", trim(msg)
		write(*,*) "    at ", trim(file), ":", to_str(line)
		nfail = nfail + 1
	end if
end subroutine test_

subroutine test_in1(nfail, ntot)
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: str, filename, str_out, expect
	type(json_t) :: json

	filename = "data/in1.json"
	write(*,*) "Unit testing file "//quote(filename)//" ..."

	! TODO: just use json%read_file() instead of manually loading whole str,
	! like in3 below

	str = read_file(filename)
	!print *, "str = "//LINE_FEED//str

	! Beware, this will break depending on the ordering, e.g. if djb2_hash() is
	! ever modified
	json%hashed_order = .true.

	call json%read_str(str)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = &
		'{' // LINE_FEED // &
		'    "c": "my str",' // LINE_FEED // &
		'    "a": 1,' // LINE_FEED // &
		'    "b": 2' // LINE_FEED // &
		'}'
	TEST(is_str_eq(str_out, expect), "test_in1 1", nfail, ntot)

	json%compact = .true.
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"c":"my str","a":1,"b":2}'
	TEST(is_str_eq(str_out, expect), "test_in1 2", nfail, ntot)

	! TODO: add tests with multiple indentation depths and custom indent strs

	! TODO: setting hashed_order true, reading json, and then changing it to
	! false before str conversion will cause problems.  Handle this case

	json%hashed_order = .false.
	json%compact = .true.
	str = read_file(filename)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"a":1,"b":2,"c":"my str"}'
	TEST(is_str_eq(str_out, expect), "test_in1 3", nfail, ntot)

end subroutine test_in1

subroutine test_in2(nfail, ntot)
	! Arrays
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: filename, str_out, expect
	type(json_t) :: json

	filename = "data/in2.json"
	write(*,*) "Unit testing file "//quote(filename)//" ..."

	json%compact = .true.
	call json%read_file(filename)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"a":1,"b":2,"c":"my str","d":[10,20,30]}'
	TEST(is_str_eq(str_out, expect), "test_in6 1", nfail, ntot)

end subroutine test_in2

subroutine test_in3(nfail, ntot)
	! Nested objects.  Inner objects have some of the same keys as the outer
	! objects, although these are of course not duplicate keys because they
	! exist at different levels
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: filename, str_out, expect
	type(json_t) :: json

	filename = "data/in3.json"
	write(*,*) "Unit testing file "//quote(filename)//" ..."

	json%compact = .true.
	call json%read_file(filename)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"a":1,"b":{"a":11,"b":{"a":111,"b":"here''s a \"string\"","c":333},"c":33,"d":44},"c":3,"d":4,"e":5}'
	TEST(is_str_eq(str_out, expect), "test_in3 1", nfail, ntot)

end subroutine test_in3

subroutine test_in4(nfail, ntot)
	! With duplicate keys, the last value is retained by default and its order is correct
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: filename, str_out, expect
	type(json_t) :: json

	filename = "data/in4.json"
	write(*,*) "Unit testing file "//quote(filename)//" ..."

	json%compact = .true.
	call json%read_file(filename)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"a":1,"b":9999,"c":3}'
	TEST(is_str_eq(str_out, expect), "test_in4 1", nfail, ntot)

	json%first_duplicate = .true.
	call json%read_file(filename)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"a":1,"b":2,"c":3}'
	TEST(is_str_eq(str_out, expect), "test_in4 2", nfail, ntot)

end subroutine test_in4

subroutine test_in5(nfail, ntot)
	! Duplicate keys
	!
	! Similar to in4 but with more source duplicates
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: filename, str_out, expect
	type(json_t) :: json

	filename = "data/in5.json"
	write(*,*) "Unit testing file "//quote(filename)//" ..."

	json%compact = .true.
	call json%read_file(filename)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"a":1111,"c":3333,"b":9999}'
	TEST(is_str_eq(str_out, expect), "test_in5 1", nfail, ntot)

	json%first_duplicate = .true.
	call json%read_file(filename)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"a":1,"c":20,"b":2}'
	TEST(is_str_eq(str_out, expect), "test_in5 2", nfail, ntot)

end subroutine test_in5

subroutine test_in6(nfail, ntot)
	! Duplicate keys
	!
	! A pathological case with 10 keys and 5 instances each, with randomly
	! shuffled orders after the first instance
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: filename, str_out, expect
	type(json_t) :: json

	filename = "data/in6.json"
	write(*,*) "Unit testing file "//quote(filename)//" ..."

	json%compact = .true.
	call json%read_file(filename)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"0":0,"a":1000,"b":2000,"c":3000,"d":4000,"e":5000,"f":6000,"g":7000,"h":8000,"i":9000}'
	TEST(is_str_eq(str_out, expect), "test_in6 1", nfail, ntot)

	!json%allow_duplicate_keys = .false.
	!call json%read_file(filename)  ! error

	json%first_duplicate = .true.
	call json%read_file(filename)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"0":404,"a":404,"b":404,"c":404,"d":404,"e":404,"f":404,"g":404,"h":404,"i":404}'
	TEST(is_str_eq(str_out, expect), "test_in6 2", nfail, ntot)

end subroutine test_in6

subroutine test_in7(nfail, ntot)
	! Get keys by "json pointer" path string -- RFC 6901
	!
	! Basic test
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: filename!, str_out
	integer :: expect_i32
	type(json_t) :: json
	type(json_val_t) :: val

	filename = "data/in7.json"
	write(*,*) "Unit testing file "//quote(filename)//" ..."

	call json%read_file(filename)
	!str_out = json%to_str()
	!print *, "str_out = ", str_out

	val = json%get_val('/foo')
	expect_i32 = 1337
	TEST(val%sca%i64 == expect_i32, "test_in7 1", nfail, ntot)

	val = json%get_val('/foo')
	expect_i32 = 1337
	TEST(val%sca%i64 == expect_i32, "test_in7 1", nfail, ntot)

	val = json%get_val('/bar')
	expect_i32 = 420
	TEST(val%sca%i64 == expect_i32, "test_in7 2", nfail, ntot)

	val = json%get_val('/baz')
	expect_i32 = 69
	TEST(val%sca%i64 == expect_i32, "test_in7 3", nfail, ntot)

end subroutine test_in7

subroutine test_in8(nfail, ntot)
	! Get keys by "json pointer" path string -- RFC 6901
	!
	! Nested objects, case-sensitive keys, empty string keys, space keys
	!
	! TODO: make a more complicated json pointer array test. in9 only covers a
	! single-level array
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: filename, expect_str!, str_out
	integer(kind=8) :: expect_i64
	type(json_t) :: json
	type(json_val_t) :: val

	filename = "data/in8.json"
	write(*,*) "Unit testing file "//quote(filename)//" ..."

	call json%read_file(filename)
	!str_out = json%to_str()
	!print *, "str_out = ", str_out

	val = json%get_val('/foo')
	expect_i64 = 1337
	TEST(val%sca%i64 == expect_i64, "test_in8 1", nfail, ntot)

	val = json%get_val('/bar/FOO')
	expect_i64 = 90210
	TEST(val%sca%i64 == expect_i64, "test_in8 2", nfail, ntot)

	val = json%get_val('/bar/FOO')
	expect_i64 = 90210
	TEST(val%sca%i64 == expect_i64, "test_in8 3", nfail, ntot)

	val = json%get_val('/bar/BAR')
	expect_i64 = 8675309
	TEST(val%sca%i64 == expect_i64, "test_in8 4", nfail, ntot)

	val = json%get_val('/baz/fOo')
	expect_i64 = 69
	TEST(val%sca%i64 == expect_i64, "test_in8 5", nfail, ntot)

	val = json%get_val('/baz/bar/_')
	expect_str = "very"
	TEST(val%sca%str == expect_str, "test_in8 6", nfail, ntot)

	! A space is a valid key
	val = json%get_val('/baz/bar/ ')
	expect_str = "nice"
	TEST(val%sca%str == expect_str, "test_in8 7", nfail, ntot)

	val = json%get_val('/baz/BaZ')
	expect_i64 = 420
	TEST(val%sca%i64 == expect_i64, "test_in8 8", nfail, ntot)

	! Empty space key(s) after last "/"
	val = json%get_val('/baz/bar/')
	expect_str = "kicks"
	TEST(val%sca%str == expect_str, "test_in8 9", nfail, ntot)

	val = json%get_val('/baz/foo//')
	expect_i64 = 42
	TEST(val%sca%i64 == expect_i64, "test_in8 10", nfail, ntot)

	! Empty keys in middle or start of path

	! This one is tricky in jq.  You have to quote the empty strings:
	!
	!     jq '.""."".a.b' data/in8.json
	!
	val = json%get_val('///a/b')
	expect_i64 = 80085
	TEST(val%sca%i64 == expect_i64, "test_in8 11", nfail, ntot)

	val = json%get_val('///a////c')
	expect_i64 = 5055034455_8
	TEST(val%sca%i64 == expect_i64, "test_in8 12", nfail, ntot)

end subroutine test_in8

subroutine test_in9(nfail, ntot)
	! Get keys by "json pointer" path string -- RFC 6901
	!
	! The example from the RFC
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: filename, str_out, expect_str
	integer(kind=8) :: expect_i64
	type(json_t) :: json
	type(json_val_t) :: val

	filename = "data/in9.json"
	write(*,*) "Unit testing file "//quote(filename)//" ..."

	call json%read_file(filename)
	!print *, "str_out 1 = ", str_out

	! Empty str means whole document.  This means leading '/' is not optional
	! for other cases
	!
	! Note, I can't figure out how to pass an empty string as a "-p" cmd
	! argument. Quoting should work, but it's getting padded somewhere
	! (fortran?)
	val = json%get_val('')
	json%compact = .true.
	str_out = val_to_str(json, val)
	expect_str = '{"foo":["bar","baz"],"":0,"a/b":1,"c%d":2,"e^f":3,"g|h":4,"i\\j":5,"k\"l":6," ":7,"m~n":8}'
	!print *, expect_str
	!print *, str_out
	TEST(str_out == expect_str, "test_in9 1", nfail, ntot)

	!    "/foo"       ["bar", "baz"]
	val = json%get_val('/foo')
	str_out = val_to_str(json, val)
	expect_str = '["bar","baz"]'
	TEST(str_out == expect_str, "test_in9 1.1", nfail, ntot)

	!    "/foo/0"     "bar"
	val = json%get_val('/foo/0')
	expect_str = 'bar'  ! note there are no inner quotes
	TEST(val%sca%str == expect_str, "test_in9 1.2", nfail, ntot)

	!    "/"          0
	val = json%get_val('/')
	expect_i64 = 0
	TEST(val%sca%i64 == expect_i64, "test_in9 2", nfail, ntot)

	!    "/a~1b"      1
	val = json%get_val('/a~1b')
	expect_i64 = 1
	TEST(val%sca%i64 == expect_i64, "test_in9 3", nfail, ntot)

	!    "/c%d"       2
	val = json%get_val('/c%d')
	expect_i64 = 2
	TEST(val%sca%i64 == expect_i64, "test_in9 4", nfail, ntot)

	!    "/e^f"       3
	val = json%get_val('/e^f')
	expect_i64 = 3
	TEST(val%sca%i64 == expect_i64, "test_in9 5", nfail, ntot)

	!    "/g|h"       4
	val = json%get_val('/g|h')
	expect_i64 = 4
	TEST(val%sca%i64 == expect_i64, "test_in9 6", nfail, ntot)

	! RFC:
	!
	!   Note that before processing a JSON string as a JSON Pointer,
	!   backslash escape sequences must be unescaped.
	!
	! Fortran strings don't require any escaping to begin with, so I have less
	! backslashes in these next two examples.  Not 100% sure if I'm reading the
	! standard correctly though

	!    "/i\\j"      5
	!val = json%get_val('/i\\j')  ! maybe?
	val = json%get_val('/i\j')
	expect_i64 = 5
	TEST(val%sca%i64 == expect_i64, "test_in9 7", nfail, ntot)

	!    "/k\"l"      6
	!val = json%get_val('/k\"l')  ! maybe?
	val = json%get_val('/k"l')
	expect_i64 = 6
	TEST(val%sca%i64 == expect_i64, "test_in9 8", nfail, ntot)

	!    "/ "         7
	val = json%get_val('/ ')
	expect_i64 = 7
	TEST(val%sca%i64 == expect_i64, "test_in9 9", nfail, ntot)

	!    "/m~0n"      8
	val = json%get_val('/m~0n')
	expect_i64 = 8
	TEST(val%sca%i64 == expect_i64, "test_in9 10", nfail, ntot)

end subroutine test_in9

logical function is_sorted_i32(v) result(sorted)
	integer, intent(in) :: v(:)
	integer :: n
	n = size(v)
	sorted = all(v(1:n-1) <= v(2:n))
end function is_sorted_i32

subroutine seed_rng_zeros()
	implicit none
	integer :: seed_size
	integer, allocatable :: seed(:)

	! Get the size of the seed array
	call random_seed(size=seed_size)

	! Allocate and initialize seed array with zeros
	allocate(seed(seed_size))
	seed = 0

	! Set the seed
	call random_seed(put=seed)

	! Clean up
	deallocate(seed)

end subroutine seed_rng_zeros

! TODO: blarg? utils maybe?
function rand_vec_i32(n, min_val, max_val) result(vec)
	integer, intent(in) :: n          ! Length of vector
	integer, intent(in) :: min_val    ! Minimum value (inclusive)
	integer, intent(in) :: max_val    ! Maximum value (inclusive)
	integer, dimension(n) :: vec      ! Output vector
	!********
	double precision :: rand_f64      ! Single random value
	integer :: i
	do i = 1, n
		! Generate random integers one at a time
		call random_number(rand_f64)
		vec(i) = int(min_val + floor(rand_f64 * (int(max_val,8) - min_val + 1), 8), 4)
	end do
end function rand_vec_i32

integer function rand_i32(min_val, max_val) result(r)
	! Random integer in inclusize bounds
	integer, intent(in) :: min_val, max_val
	double precision :: rand_f64      ! Single random value
	call random_number(rand_f64)
	r = int(min_val + floor(rand_f64 * (int(max_val,8) - min_val + 1), 8), 4)
end function rand_i32

function rand_str(len_) result(str)
	! Generate a random string of given length
	integer, intent(in) :: len_
	character(len=len_) :: str
	integer :: i, j
	character(len=*), parameter :: chars = &
		"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	do i = 1, len_
		j = rand_i32(1, len(chars))
		str(i:i) = chars(j:j)
	end do
end function rand_str

function rand_perm(n)
	! Fisher-Yates shuffle
	integer, intent(in) :: n
	integer, allocatable :: rand_perm(:)
	!********
	integer :: i, j

	rand_perm = [(i, i = 1, n)]  ! initially identity
	do i = 1, n-1
		j = rand_i32(i, n-1)
		rand_perm([i, j]) = rand_perm([j, i])
	end do

end function rand_perm

subroutine test_sort(nfail, ntot)
	integer, intent(inout) :: nfail, ntot
	!********
	integer :: i
	integer, allocatable :: v(:)

	write(*,*) "Unit testing sorting routines ..."

	v = [72, 16, 17, 3, 53, 99, 1]
	call sort(v)
	!print *, "v = ", v
	TEST(is_sorted(v), "test_sort 1", nfail, ntot)

	v = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3]
	call sort(v)
	!print *, "v = ", v
	TEST(is_sorted(v), "test_sort 2", nfail, ntot)

	! Fuzz tests

	! Large size and small range -- lots of duplicate values
	v = rand_vec_i32(100, 0, 10)
	call sort(v)
	!print *, "v = ", v
	TEST(is_sorted(v), "test_sort 3", nfail, ntot)

	! Small size and large range -- *probably* unique values
	v = rand_vec_i32(10, 0, 100)
	call sort(v)
	!print *, "v = ", v
	TEST(is_sorted(v), "test_sort 4", nfail, ntot)

	do i = 1, 100
		v = rand_vec_i32(200, -2000000000, 2000000000)
		call sort(v)
		!print *, "v = ", v
		TEST(is_sorted(v), "test_sort fuzz", nfail, ntot)

		v = rand_vec_i32(301, -200, 200)
		call sort(v)
		!print *, "v = ", v
		TEST(is_sorted(v), "test_sort fuzz", nfail, ntot)
	end do

end subroutine  test_sort

subroutine test_levenshtein(nfail, ntot)
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: a, b, edits
	integer :: i, n, dist, expect, expect_, reps, loc
	integer, allocatable :: locs(:)

	a = "sitting"
	b = "kitten"
	dist = levenshtein(a, b)
	TEST(dist == 3, "test_levenshtein 1", nfail, ntot)

	! Fuzz test.  This does not cover combinations of
	! insertions/deletions/substitutions, only each edit type in isolation
	do expect = 0, 10
	do n = 5, 25
	do reps = 1, 2
		if (expect > n) cycle  ! can't make more edits than str len
		a = rand_str(n)

		! Make `expect` edits to a
		edits = rand_str(expect)

		!********
		! Insertion: insert edits after random locations
		locs  = rand_vec_i32(expect, 0, n)
		b = a
		do i = 1, expect
			loc = locs(i)
			b = b(1: loc) // edits(i:i) // b(loc+1:)
		end do
		dist = levenshtein(a, b)
		TEST(dist == expect, "test_levenshtein insert fuzz", nfail, ntot)

		!********
		! Deletion: delete random chars
		b = a
		do i = 1, expect
			loc = rand_i32(1, len(b))
			b = b(1:loc-1) // b(loc+1:)
		end do
		TEST(dist == expect, "test_levenshtein delete fuzz", nfail, ntot)

		!********
		! Substitution: sub edits at random locs. Re-use `edits` str from insertion test
		!
		! It's tricky to generate the correct expected number of substitution
		! edits.  Consider the two strings where every single character has been
		! substituted:
		!
		!     a = "abcdefgh"
		!     b = "bcdefghi"
		!
		! The edit distance is of course not 8, because we can make an
		! equivalent edit in 2 steps: delete the first "a" and then insert an
		! "i"
		!
		! Hence, we multiply location by 2 to only change even indices
		b = a
		locs = rand_perm(n)  ! edits must be unique. use a slice of a random permutation
		expect_ = 0
		do i = 1, expect
			loc = 2 * locs(i)
			if (loc > len(b)) cycle
			if (a(loc:loc) == edits(i:i)) cycle  ! skip no-op substitutions
			expect_ = expect_ + 1
			b(loc:loc) = edits(i:i)
		end do
		dist = levenshtein(a, b)
		TEST(dist == expect_, "test_levenshtein substitution fuzz", nfail, ntot)

	end do
	end do
	end do

end subroutine test_levenshtein

subroutine test_float_jsons(nfail, ntot)
	! Float formatting makes it annoying to test str output, so this is a
	! separate test routine that relies on get_val()
	integer, intent(inout) :: nfail, ntot
	!********
	real(kind=8) :: expect
	real(kind=8), parameter :: TOL = 1.d-9
	type(json_t) :: json
	type(json_val_t) :: val

	write(*,*) "Unit testing floating point numbers ..."

	call json%read_str('1.2')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = 1.2d0
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 1", nfail, ntot)

	call json%read_str('3.4')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = 3.4d0
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 2", nfail, ntot)

	call json%read_str('{"foo": 5.6}')
	val = json%get_val('/foo')
	!print *, "val = ", val_to_str(json, val)
	expect = 5.6d0
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 3", nfail, ntot)

	call json%read_str('[true, 7.8, null]')
	val = json%get_val('/1')
	!print *, "val = ", val_to_str(json, val)
	expect = 7.8d0
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 4", nfail, ntot)

	! TODO: test floats in nested objects/arrays

	! Note, not strict JSON, disabled with -Werror=numbers
	call json%read_str('3.')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = 3.d0
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 5", nfail, ntot)

	! Note missing leading 0 is not supposed to be valid json
	call json%read_str('.4')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = .4d0
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 6", nfail, ntot)

	call json%read_str('.432')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = .432d0
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 6", nfail, ntot)

	call json%read_str('+.432')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = .432d0
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 6", nfail, ntot)

	call json%read_str('-.432')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = -.432d0
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 6", nfail, ntot)

	call json%read_str('327.')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = 327.d0
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 5", nfail, ntot)

	call json%read_str('327.d5')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = 327.d5
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 5", nfail, ntot)

	call json%read_str('326.e11')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = 326.d11
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 5", nfail, ntot)

	call json%read_str('327.D+5')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = 327.d+5
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 5", nfail, ntot)

	call json%read_str('326.E-11')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = 326.d-11
	TEST(abs(val%sca%f64 - expect) <= 1.d-20, "test_float_jsons 5", nfail, ntot)

	call json%read_str('-327.D+5')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = -327.d+5
	TEST(abs(val%sca%f64 - expect) <= TOL, "test_float_jsons 5", nfail, ntot)

	call json%read_str('-326.E-11')
	val = json%get_val('')
	!print *, "val = ", val_to_str(json, val)
	expect = -326.d-11
	TEST(abs(val%sca%f64 - expect) <= 1.d-20, "test_float_jsons 5", nfail, ntot)

	! i64 overflow silently promoted to f64
	call json%read_str('99999999999999999999')
	val = json%get_val('')
	TEST(val%sca%type == F64_TYPE, "i64 overflow -> f64 type", nfail, ntot)
	TEST(json%diagnostics%len == 0, "i64 overflow -> no error", nfail, ntot)
	TEST(abs(val%sca%f64 - 1.d20) <= TOL * 1.d20, "i64 overflow -> f64 value", nfail, ntot)

	call json%read_str('{"big": 99999999999999999999}')
	val = json%get_val('/big')
	TEST(val%sca%type == F64_TYPE, "i64 overflow in obj -> f64 type", nfail, ntot)
	TEST(json%diagnostics%len == 0, "i64 overflow in obj -> no error", nfail, ntot)
	TEST(abs(val%sca%f64 - 1.d20) <= TOL * 1.d20, "i64 overflow in obj -> f64 value", nfail, ntot)

end subroutine test_float_jsons

subroutine test_basic_jsons(nfail, ntot)
	integer, intent(inout) :: nfail, ntot
	!********
	character(len=:), allocatable :: str, str_out, expect
	type(json_t) :: json

	write(*,*) "Unit testing basic JSON strings ..."

	str = '{"a": 1}'
	call json%read_str(str)
	str_out = json%to_str()
	!print *, "str_out = ", str_out

	expect = &
		'{'//LINE_FEED// &
		'    "a": 1'//LINE_FEED// &
		'}'
	!print *, "expect = ", expect
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 1", nfail, ntot)

	json%compact = .true.
	str_out = json%to_str()
	expect = '{"a":1}'
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 2", nfail, ntot)

	! Null
	str = 'null'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 2.1", nfail, ntot)

	str = '{"a":null}'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 2.2", nfail, ntot)

	str = '[null]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 2.3", nfail, ntot)

	! Booleans
	str = 'true'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 2.4", nfail, ntot)

	str = 'false'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 2.5", nfail, ntot)

	str = '{"a":true}'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 2.6", nfail, ntot)

	str = '[false]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 2.7", nfail, ntot)

	! Integers
	str = '69'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 3", nfail, ntot)

	str = '0'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 3.1", nfail, ntot)

	str = '00'
	call json%read_str(str)
	str_out = json%to_str()
	expect = '0'
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 3.2", nfail, ntot)

	! Note, not strict JSON, disabled with -Werror=numbers
	str = '01'
	call json%read_str(str)
	str_out = json%to_str()
	expect = '1'
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 3.3", nfail, ntot)

	str = '-2'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 3.4", nfail, ntot)

	! Note, not strict JSON, disabled with -Werror=numbers
	str = '+2'
	call json%read_str(str)
	str_out = json%to_str()
	expect = '2'
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 3.5", nfail, ntot)

	str = '-234'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 3.6", nfail, ntot)

	! Note, not strict JSON, disabled with -Werror=numbers
	str = '+253'
	call json%read_str(str)
	str_out = json%to_str()
	expect = '253'
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 3.7", nfail, ntot)

	str = '420'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4", nfail, ntot)

	! Arrays
	str = '[0]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.1", nfail, ntot)

	str = '[10,20]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.2", nfail, ntot)

	! Heterogeneous arrays
	str = '[30,"hello"]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.3", nfail, ntot)

	str = '["world",40]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.4", nfail, ntot)

	str = '["world",null,40]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.4.1", nfail, ntot)

	str = '["world",null,40,true]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.4.2", nfail, ntot)

	! Nested arrays
	str = '[[]]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.5", nfail, ntot)

	str = '[[100],[67,64]]'  ! different sizes
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.5", nfail, ntot)

	str = '[[200,7],[67,64]]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.5", nfail, ntot)

	str = '[[300,8],[67,[64,35,404]]]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.5", nfail, ntot)

	str = '[[300,8],[67,[64,"my-str",404]]]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.6", nfail, ntot)

	str = '[[300,8,null,null],[67,[64,"my-str",null,404]]]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.6", nfail, ntot)

	str = '[[300,false,true,8,null,null],[67,[64,true,"my-str",null,404]]]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 4.7", nfail, ntot)

	str = '"my string"'
	!str = '"my string'  ! unterminated str
	!print *, "str = ", str
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 5", nfail, ntot)

	str = '"my \"escaped\" string"'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 6", nfail, ntot)

	str = '{"wierd-\"key":"my key has an escape"}'
	call json%read_str(str)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 7", nfail, ntot)

	str = '{"very \"wierd\" key":"my key has multiple escapes"}'
	call json%read_str(str)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 8", nfail, ntot)

	! Empty array
	str = '[]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 9", nfail, ntot)

	! Empty object
	str = '{}'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 9.1", nfail, ntot)

	str = '[{}]'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 9.2", nfail, ntot)

	! Trailing commas
	str = '{"a": 1,}'
	call json%read_str(str)
	str_out = json%to_str()
	expect = '{"a":1}'  ! comma is removed from output
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 10", nfail, ntot)

	str = '{"a": 1, "b": 2,}'
	call json%read_str(str)
	str_out = json%to_str()
	expect = '{"a":1,"b":2}'  ! last comma is removed from output
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 10", nfail, ntot)

	str = '{"a":1,"b":2,"c":null}'
	call json%read_str(str)
	str_out = json%to_str()
	expect = str
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 11", nfail, ntot)
	str = '{"a":1,"b":2,"c":null,}'
	call json%read_str(str)
	str_out = json%to_str()
	!expect = str  ! re-use last expect value
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 12", nfail, ntot)

end subroutine test_basic_jsons

subroutine test_num_jsons(nfail, ntot)
	! Test the JSON number validator
	integer, intent(inout) :: nfail, ntot
	!********
	integer :: i
	logical :: is_valid
	type(str_vec_t) :: str_vec

	!********
	! Valid numbers
	str_vec = new_str_vec()

	! ints
	call str_vec%push("0")
	call str_vec%push("-0")
	call str_vec%push("1")
	call str_vec%push("9")
	call str_vec%push("10")
	call str_vec%push("99")
	call str_vec%push("100")
	call str_vec%push("1234567890")
	call str_vec%push("-1")
	call str_vec%push("-9")
	call str_vec%push("-10")
	call str_vec%push("-1234567890")

	! fracs
	call str_vec%push("0.0")
	call str_vec%push("-0.0")
	call str_vec%push("0.1")
	call str_vec%push("1.0")
	call str_vec%push("1.5")
	call str_vec%push("10.01")
	call str_vec%push("123.456")
	call str_vec%push("-0.1")
	call str_vec%push("-1.0")
	call str_vec%push("-123.456")

	! exponents
	call str_vec%push("0e0")
	call str_vec%push("0E0")
	call str_vec%push("1e0")
	call str_vec%push("1E0")
	call str_vec%push("1e1")
	call str_vec%push("1e+1")
	call str_vec%push("1e-1")
	call str_vec%push("-1e1")
	call str_vec%push("-1e+1")
	call str_vec%push("-1e-1")
	call str_vec%push("123e456")
	call str_vec%push("123E456")
	call str_vec%push("0.1e1")
	call str_vec%push("1.23e-10")
	call str_vec%push("-1.23E+10")
	call str_vec%push("0.0e0")
	call str_vec%push("-0.0e0")
	call str_vec%push("1.0e10")
	call str_vec%push("-1.0e-10")
	call str_vec%push("123.456E789")

	do i = 1, i32(str_vec%len)
		is_valid = is_valid_json_number(str_vec%vec(i)%str)
		TEST(is_valid .eqv. .true., "test_num_jsons 1, i="//to_str(i), nfail, ntot)
	end do

	!********
	! Invalid numbers
	str_vec = new_str_vec()

	! leading zeros
	call str_vec%push("00")
	call str_vec%push("01")
	call str_vec%push("09")
	call str_vec%push("000")
	call str_vec%push("0123")
	call str_vec%push("-00")
	call str_vec%push("-01")
	call str_vec%push("-09")
	call str_vec%push("-0123")

	! invalid frac
	call str_vec%push("1.")
	call str_vec%push("0.")
	call str_vec%push("-1.")
	call str_vec%push(".1")
	call str_vec%push("-.1")
	call str_vec%push("1..0")
	call str_vec%push("1.0.0")

	! invalid exponents
	call str_vec%push("1e")
	call str_vec%push("1E")
	call str_vec%push("1e+")
	call str_vec%push("1e-")
	call str_vec%push("1e+-1")
	call str_vec%push("1e--1")
	call str_vec%push("1ee1")
	call str_vec%push("1E+E1")

	! leading zero
	call str_vec%push("01.0")
	call str_vec%push("01e1")
	call str_vec%push("01.0e1")
	call str_vec%push("-01.0")
	call str_vec%push("-01e1")

	! missing integer part
	call str_vec%push(".e1")
	call str_vec%push(".E1")
	call str_vec%push(".e+1")
	call str_vec%push(".e-1")

	! invalid chars
	call str_vec%push("+")
	call str_vec%push("-")
	call str_vec%push("--1")
	call str_vec%push("+-1")
	call str_vec%push("1-")
	call str_vec%push("1+")
	call str_vec%push("1a")
	call str_vec%push("a1")
	call str_vec%push("1_0")
	call str_vec%push("1,000")

	! whitespace
	call str_vec%push(" 1")
	call str_vec%push("1 ")
	call str_vec%push(" 1 ")
	call str_vec%push(LINE_FEED//"1")
	call str_vec%push("1"//LINE_FEED)

	! Non-decimals
	call str_vec%push("0x10")
	call str_vec%push("0X10")
	call str_vec%push("0b10")
	call str_vec%push("0o10")

	! Special values
	call str_vec%push("NaN")
	call str_vec%push("Infinity")
	call str_vec%push("-Infinity")
	call str_vec%push("inf")

	! Empty or partial
	call str_vec%push("")
	call str_vec%push(".")
	call str_vec%push("e10")
	call str_vec%push("-E1")

	do i = 1, i32(str_vec%len)
		is_valid = is_valid_json_number(str_vec%vec(i)%str)
		TEST(is_valid .eqv. .false., "test_num_jsons 2, i="//to_str(i), nfail, ntot)
	end do

end subroutine test_num_jsons

subroutine test_errs(nfail, ntot)
	integer, intent(inout) :: nfail, ntot
	!********
	type(json_t) :: json
	type(json_val_t) :: jval
	character(len=:), allocatable :: expect, place, under

	json%print_errors_immediately = .false.

	call json%read_str('{"a": 1  "b": 2}')
	expect = "missing comma or right-brace"  ! validate error message
	place  = "<STR_STREAM>:1:10"             ! validate line/column number reporting
	under  = "1m^^^"//ESC                    ! validate length of underline
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	call json%read_str(LINE_FEED//LINE_FEED//'{"a": 1, "b": 2')
	expect = "missing comma or right-brace"
	place  = "<STR_STREAM>:3:16"
	under  = "1m^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	call json%read_str('[0 2]')
	expect = "missing comma or right-bracket"
	place  = "<STR_STREAM>:1:4"
	under  = "1m^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	call json%read_str('[0, 2')
	expect = "missing comma or right-bracket"
	place  = "<STR_STREAM>:1:6"
	under  = "1m^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	json%error_numbers = .true.
	call json%read_str('[.0]')
	expect = "bad integer part"
	place  = "<STR_STREAM>:1:2"
	under  = "1m^^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	call json%read_str('[0.]')
	expect = "missing digits after decimal point"
	place  = "<STR_STREAM>:1:2"
	under  = "1m^^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	json%error_numbers = .false.
	call json%read_str('[0.0.]')
	expect = "bad floating-point number format"
	place  = "<STR_STREAM>:1:2"
	under  = "1m^^^^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	! i64 overflow is silently promoted to f64, not an error
	call json%read_str('[123456789123456789123]')
	TEST(json%diagnostics%len == 0, "i64 overflow in arr -> no error", nfail, ntot)

	call json%read_str('{"a": "hello}')
	expect = "unterminated string literal"
	place  = "<STR_STREAM>:1:7"
	under  = "1m^^^^^^^^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	call json%read_str('@')
	expect = "unexpected character"
	place  = "<STR_STREAM>:1:1"
	under  = "1m^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	call json%read_str('{1: 2}')
	expect = "but got"
	place  = "<STR_STREAM>:1:2"
	under  = "1m^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	json%error_trailing_commas = .true.
	call json%read_str('[1, 2,]')
	expect = "trailing comma in array"
	place  = "<STR_STREAM>:1:6"  ! points at the comma, not the closing bracket
	under  = "1m^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

	call json%read_str('{"a": 1,}')
	expect = "trailing comma in object"
	place  = "<STR_STREAM>:1:8"  ! points at the comma, not the closing brace
	under  = "1m^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)
	json%error_trailing_commas = .false.

	json%error_duplicate_keys = .true.
	call json%read_str('{"a": 1, "a": 2}')
	expect = "duplicate key"
	place  = "<STR_STREAM>:1:10"
	under  = "1m^^^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)
	json%error_duplicate_keys = .false.

	call json%read_str('{"a": 1}')
	jval = json%get_val("/b")
	expect = "not found"
	TEST(err_matches(json, expect), "diag: "//expect, nfail, ntot)
	expect = "Did you mean"
	TEST(err_matches(json, expect), "diag: "//expect, nfail, ntot)

	call json%read_str('[1, 2]')
	jval = json%get_val("/5")
	expect = "out of bounds"
	TEST(err_matches(json, expect), "diag: "//expect, nfail, ntot)

	call json%read_str('{}')  ! reset diagnostics with a clean parse
	call json%read_file("nonexistent_file.json")
	expect = "can't open file"
	TEST(err_matches(json, expect), "diag: "//expect, nfail, ntot)

	call json%read_str('[:]')
	expect = "where value expected"
	place  = "<STR_STREAM>:1:2"
	under  = "1m^"//ESC
	TEST(err_matches(json, expect), "diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "under: "//expect, nfail, ntot)

end subroutine test_errs

subroutine test_file_errs(nfail, ntot)
	integer, intent(inout) :: nfail, ntot
	!********
	type(json_t) :: json
	character(len=:), allocatable :: expect, place, under

	json%print_errors_immediately = .false.

	call json%read_file("data/errs/missing_comma.json")
	expect = "missing comma or right-brace"
	place  = "data/errs/missing_comma.json:3:5"
	TEST(err_matches(json, expect), "file diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "file place: "//expect, nfail, ntot)

	call json%read_file("data/errs/unterminated_str.json")
	expect = "unterminated string literal"
	place  = "data/errs/unterminated_str.json:2:10"
	TEST(err_matches(json, expect), "file diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "file place: "//expect, nfail, ntot)

	call json%read_file("data/errs/unexpected_char.json")
	expect = "unexpected character"
	place  = "data/errs/unexpected_char.json:3:5"
	TEST(err_matches(json, expect), "file diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "file place: "//expect, nfail, ntot)

	json%error_trailing_commas = .true.
	call json%read_file("data/errs/trailing_comma.json")
	expect = "trailing comma in array"
	place  = "data/errs/trailing_comma.json:3:6"  ! points at the comma
	TEST(err_matches(json, expect), "file diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "file place: "//expect, nfail, ntot)
	json%error_trailing_commas = .false.

	json%error_duplicate_keys = .true.
	call json%read_file("data/errs/duplicate_key.json")
	expect = "duplicate key"
	place  = "data/errs/duplicate_key.json:3:5"
	under  = "1m^^^"//ESC
	TEST(err_matches(json, expect), "file diag : "//expect, nfail, ntot)
	TEST(err_matches(json, place ), "file place: "//expect, nfail, ntot)
	TEST(err_matches(json, under ), "file under: "//expect, nfail, ntot)
	json%error_duplicate_keys = .false.

end subroutine test_file_errs

logical function err_matches(json, msg)
	! Check if a json's first error message matches a given msg string
	type(json_t), intent(in) :: json
	character(len=*), intent(in) :: msg
	err_matches = contains_substr(json%diagnostics%vec(1)%str, msg)
end function err_matches

subroutine test_get_api(nfail, ntot)
	! Test the new get_val() API improvements (Phase 1-6)
	integer, intent(inout) :: nfail, ntot
	!********
	type(json_t) :: json
	type(json_val_t) :: jval
	integer(kind=8) :: i64
	integer(kind=8), allocatable :: vec_i64(:), mat_i64(:,:)
	real(kind=8) :: f64
	real(kind=8), allocatable :: vec_f64(:), mat_f64(:,:)
	logical :: bool, found
	logical, allocatable :: vec_bool(:)
	character(len=:), allocatable :: str
	type(str_t), allocatable :: vec_str(:)
	real(kind=8), parameter :: TOL = 1.d-9

	json%print_errors_immediately = .false.

	write(*,*) "Unit testing get_val() API improvements ..."

	!===========================================================================
	! Phase 2: get() alias, found argument
	!===========================================================================

	call json%read_str('{"a": 1, "b": "hello", "c": null}')

	! get() is an alias for get_val()
	jval = json%get('/a')
	TEST(jval%sca%i64 == 1_8, "get alias works", nfail, ntot)

	! found=.true. when key exists
	jval = json%get_val('/a', found)
	TEST(found, "get_val found=.true. for existing key", nfail, ntot)

	! found=.false. and no error when key missing
	jval = json%get_val('/z', found)
	TEST(.not. found, "get_val found=.false. for missing key", nfail, ntot)
	TEST(json%diagnostics%len == 0, "get_val missing key: no diag w/ found", nfail, ntot)

	!===========================================================================
	! Phase 3: has(), is_null(), len()
	!===========================================================================

	call json%read_str('{"a": 1, "b": [10, 20, 30], "c": null, "d": {"x":1}}')

	TEST(json%has('/a'), "has: existing key", nfail, ntot)
	TEST(.not. json%has('/z'), "has: missing key", nfail, ntot)
	TEST(json%diagnostics%len == 0, "has: no error on missing key", nfail, ntot)

	TEST(json%is_null('/c'), "is_null: null value", nfail, ntot)
	TEST(.not. json%is_null('/a'), "is_null: non-null value", nfail, ntot)
	TEST(.not. json%is_null('/z'), "is_null: missing key returns false", nfail, ntot)

	TEST(json%len('/b') == 3, "len: array", nfail, ntot)
	TEST(json%len('/d') == 1, "len: object", nfail, ntot)
	TEST(json%len('/a') == 0, "len: scalar", nfail, ntot)
	TEST(json%len('/z') == 0, "len: missing key", nfail, ntot)

	!===========================================================================
	! Phase 4: typed scalar getters
	!===========================================================================

	call json%read_str('{"i": 42, "f": 3.14, "s": "hi", "b": true, "n": null}')

	! get_i64
	i64 = json%get_i64('/i')
	TEST(i64 == 42_8, "get_i64 happy path", nfail, ntot)

	! get_i64 from f64 (auto-convert)
	i64 = json%get_i64('/f')
	TEST(i64 == 3_8, "get_i64 from f64", nfail, ntot)

	! get_i64 missing key with found
	i64 = json%get_i64('/z', found)
	TEST(.not. found, "get_i64 missing key: found=.false.", nfail, ntot)
	TEST(i64 == 0_8, "get_i64 missing key: returns 0", nfail, ntot)

	! get_i64 type mismatch with found (no error)
	i64 = json%get_i64('/s', found)
	TEST(.not. found, "get_i64 type mismatch: found=.false.", nfail, ntot)
	TEST(json%diagnostics%len == 0, "get_i64 type mismatch: no error with found", nfail, ntot)

	! get_i64 type mismatch without found (pushes error)
	call json%read_str('{"s": "hello"}')
	i64 = json%get_i64('/s')
	TEST(err_matches(json, 'type mismatch'), "get_i64 type mismatch: error pushed", nfail, ntot)
	TEST(err_matches(json, 'expected i64'), "get_i64 type mismatch: error says i64", nfail, ntot)

	! get_f64
	call json%read_str('{"i": 10, "f": 2.5}')
	f64 = json%get_f64('/f')
	TEST(abs(f64 - 2.5d0) <= TOL, "get_f64 happy path", nfail, ntot)

	! get_f64 from i64 (auto-convert)
	f64 = json%get_f64('/i')
	TEST(abs(f64 - 10.d0) <= TOL, "get_f64 from i64", nfail, ntot)

	! get_f64 missing key with found
	f64 = json%get_f64('/z', found)
	TEST(.not. found, "get_f64 missing: found=.false.", nfail, ntot)

	! get_str
	call json%read_str('{"s": "world"}')
	str = json%get_str('/s')
	TEST(is_str_eq(str, "world"), "get_str happy path", nfail, ntot)

	str = json%get_str('/z', found)
	TEST(.not. found, "get_str missing: found=.false.", nfail, ntot)
	TEST(is_str_eq(str, ""), "get_str missing: returns empty string", nfail, ntot)

	! get_bool
	call json%read_str('{"t": true, "f": false}')
	bool = json%get_bool('/t')
	TEST(bool, "get_bool true", nfail, ntot)

	bool = json%get_bool('/f')
	TEST(.not. bool, "get_bool false", nfail, ntot)

	bool = json%get_bool('/z', found)
	TEST(.not. found, "get_bool missing: found=.false.", nfail, ntot)

	!===========================================================================
	! Phase 5: vector getters
	!===========================================================================

	! get_vec_i64 happy path
	call json%read_str('{"v": [10, 20, 30]}')
	vec_i64 = json%get_vec_i64('/v')
	TEST(size(vec_i64) == 3, "get_vec_i64 size", nfail, ntot)
	TEST(vec_i64(1) == 10_8, "get_vec_i64 element 1", nfail, ntot)
	TEST(vec_i64(2) == 20_8, "get_vec_i64 element 2", nfail, ntot)
	TEST(vec_i64(3) == 30_8, "get_vec_i64 element 3", nfail, ntot)

	! get_vec_i64 with i64/f64 mix (auto-convert)
	call json%read_str('[1, 2.0, 3]')
	vec_i64 = json%get_vec_i64('')
	TEST(size(vec_i64) == 3, "get_vec_i64 i64/f64 mix size", nfail, ntot)
	TEST(vec_i64(2) == 2_8, "get_vec_i64 f64 element auto-converted", nfail, ntot)

	! get_vec_i64 empty array
	call json%read_str('[]')
	vec_i64 = json%get_vec_i64('')
	TEST(size(vec_i64) == 0, "get_vec_i64 empty array", nfail, ntot)

	! get_vec_i64 missing key with found
	call json%read_str('{"a":1}')
	vec_i64 = json%get_vec_i64('/z', found)
	TEST(.not. found, "get_vec_i64 missing: found=.false.", nfail, ntot)

	! get_vec_i64 type mismatch with found (mixed types in array)
	call json%read_str('[1, "oops", 3]')
	vec_i64 = json%get_vec_i64('', found)
	TEST(.not. found, "get_vec_i64 mixed types: found=.false.", nfail, ntot)
	TEST(size(vec_i64) == 0, "get_vec_i64 mixed types: returns empty", nfail, ntot)

	! get_vec_f64
	call json%read_str('[1.1, 2.2, 3.3]')
	vec_f64 = json%get_vec_f64('')
	TEST(size(vec_f64) == 3, "get_vec_f64 size", nfail, ntot)
	TEST(abs(vec_f64(1) - 1.1d0) <= TOL, "get_vec_f64 element 1", nfail, ntot)
	TEST(abs(vec_f64(3) - 3.3d0) <= TOL, "get_vec_f64 element 3", nfail, ntot)

	! get_vec_f64 with i64 elements (auto-convert)
	call json%read_str('[1, 2, 3]')
	vec_f64 = json%get_vec_f64('')
	TEST(abs(vec_f64(1) - 1.d0) <= TOL, "get_vec_f64 from i64", nfail, ntot)

	! get_vec_bool
	call json%read_str('[true, false, true]')
	vec_bool = json%get_vec_bool('')
	TEST(size(vec_bool) == 3, "get_vec_bool size", nfail, ntot)
	TEST(vec_bool(1), "get_vec_bool element 1", nfail, ntot)
	TEST(.not. vec_bool(2), "get_vec_bool element 2", nfail, ntot)

	! get_vec_str
	call json%read_str('["hello", "world"]')
	vec_str = json%get_vec_str('')
	TEST(size(vec_str) == 2, "get_vec_str size", nfail, ntot)
	TEST(is_str_eq(vec_str(1)%str, "hello"), "get_vec_str element 1", nfail, ntot)
	TEST(is_str_eq(vec_str(2)%str, "world"), "get_vec_str element 2", nfail, ntot)

	! get_vec_str missing key with found
	call json%read_str('{"a":1}')
	vec_str = json%get_vec_str('/z', found)
	TEST(.not. found, "get_vec_str missing: found=.false.", nfail, ntot)

	!===========================================================================
	! Phase 6: matrix getters
	!===========================================================================

	! get_mat_i64 happy path
	call json%read_str('[[1,2,3],[4,5,6]]')
	mat_i64 = json%get_mat_i64('')
	TEST(size(mat_i64, 1) == 2, "get_mat_i64 nrows", nfail, ntot)
	TEST(size(mat_i64, 2) == 3, "get_mat_i64 ncols", nfail, ntot)
	TEST(mat_i64(1,1) == 1_8, "get_mat_i64 (1,1)", nfail, ntot)
	TEST(mat_i64(1,3) == 3_8, "get_mat_i64 (1,3)", nfail, ntot)
	TEST(mat_i64(2,1) == 4_8, "get_mat_i64 (2,1)", nfail, ntot)
	TEST(mat_i64(2,3) == 6_8, "get_mat_i64 (2,3)", nfail, ntot)

	! get_mat_i64 empty outer array
	call json%read_str('[]')
	mat_i64 = json%get_mat_i64('', found)
	TEST(found, "get_mat_i64 empty array: found=.true.", nfail, ntot)
	TEST(size(mat_i64) == 0, "get_mat_i64 empty array: size 0", nfail, ntot)

	! get_mat_i64 non-uniform rows: found=.false.
	call json%read_str('[[1,2],[3,4,5]]')
	mat_i64 = json%get_mat_i64('', found)
	TEST(.not. found, "get_mat_i64 non-uniform rows: found=.false.", nfail, ntot)

	! get_mat_f64 happy path
	call json%read_str('[[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]')
	mat_f64 = json%get_mat_f64('')
	TEST(size(mat_f64, 1) == 3, "get_mat_f64 nrows", nfail, ntot)
	TEST(size(mat_f64, 2) == 2, "get_mat_f64 ncols", nfail, ntot)
	TEST(abs(mat_f64(1,1) - 1.d0) <= TOL, "get_mat_f64 (1,1)", nfail, ntot)
	TEST(abs(mat_f64(3,2) - 6.d0) <= TOL, "get_mat_f64 (3,2)", nfail, ntot)

	! get_mat_f64 i64 elements (auto-convert)
	call json%read_str('[[1,2],[3,4]]')
	mat_f64 = json%get_mat_f64('')
	TEST(abs(mat_f64(2,2) - 4.d0) <= TOL, "get_mat_f64 from i64", nfail, ntot)

	! get_mat_f64 missing with found
	call json%read_str('{"a":1}')
	mat_f64 = json%get_mat_f64('/z', found)
	TEST(.not. found, "get_mat_f64 missing: found=.false.", nfail, ntot)

end subroutine test_get_api

end module jsonf__test

program test

	use jsonf__test
	implicit none
	integer :: nfail, ntot

	write(*,*) fg_bright_magenta//"Starting jsonf unit tests"//color_reset
	nfail = 0
	ntot = 0

	! Do consistent fuzz tests every time
	call seed_rng_zeros()

	call test_sort(nfail, ntot)
	call test_levenshtein(nfail, ntot)
	call test_num_jsons(nfail, ntot)
	call test_basic_jsons(nfail, ntot)
	call test_float_jsons(nfail, ntot)
	call test_in1(nfail, ntot)
	call test_in2(nfail, ntot)
	call test_in3(nfail, ntot)
	call test_in4(nfail, ntot)
	call test_in5(nfail, ntot)
	call test_in6(nfail, ntot)
	call test_in7(nfail, ntot)
	call test_in8(nfail, ntot)
	call test_in9(nfail, ntot)
	call test_errs(nfail, ntot)
	call test_file_errs(nfail, ntot)
	call test_get_api(nfail, ntot)

	if (nfail == 0) then
		write(*, "(a,i0,a)") fg_bold // fg_green // " All ", ntot, " tests passed " // color_reset
	else
		write(*, "(a,i0,a,i0,a)") fg_bold_bright_red // " Error: ", nfail, "/", ntot, " tests failed " // color_reset
	end if
	if (nfail /= 0) call panic("")
	call jsonf_exit(EXIT_SUCCESS)

end program test

