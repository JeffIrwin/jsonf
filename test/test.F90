
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
	double precision :: rand_val      ! Single random value
	integer :: i
	do i = 1, n
		! Generate random integers one at a time
		call random_number(rand_val)
		vec(i) = int(min_val + floor(rand_val * (int(max_val,8) - min_val + 1), 8), 4)
	end do
end function rand_vec_i32

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

subroutine test_float_jsons(nfail, ntot)
	! Float formatting makes it annoying to test str output, so this is a
	! separate test routine that relies on get_val()
	integer, intent(inout) :: nfail, ntot
	!********
	real(kind=8) :: expect
	real(kind=8), parameter :: TOL = 1.d-9
	type(json_t) :: json
	type(json_val_t) :: val

	write(*,*) "Unit testing basic JSON strings ..."

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

	if (nfail == 0) then
		write(*, "(a,i0,a)") fg_bold // fg_green // " All ", ntot, " tests passed " // color_reset
	else
		write(*, "(a,i0,a,i0,a)") fg_bold_bright_red // " Error: ", nfail, "/", ntot, " tests failed " // color_reset
	end if
	if (nfail /= 0) call panic("")
	call jsonf_exit(EXIT_SUCCESS)

end program test

