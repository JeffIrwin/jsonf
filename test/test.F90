
module jsonf__test

	use utils_m
	use jsonf
	implicit none

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
	json%hashed_order = .true.
	call json%read_str(str)
	str_out = json%to_str()
	!print *, "str_out = ", str_out

	! TODO: this will break depending on the ordering, e.g. if djb2_hash() is
	! ever modified
	expect = &
		'{' // LINE_FEED // &
		'    "c": "my str",' // LINE_FEED // &
		'    "a": 1,' // LINE_FEED // &
		'    "b": 2,' // LINE_FEED // &
		'}'
	TEST(is_str_eq(str_out, expect), "test_in1 1", nfail, ntot)

	json%compact = .true.
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"c":"my str","a":1,"b":2,}'
	TEST(is_str_eq(str_out, expect), "test_in1 2", nfail, ntot)

	! TODO: add tests with multiple indentation depths and custom indent strs

	! TODO: setting hashed_order true, reading json, and then changing it to
	! false before str conversion will cause problems.  Handle this case

	json%hashed_order = .false.
	json%compact = .true.
	str = read_file(filename)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = '{"a":1,"b":2,"c":"my str",}'
	TEST(is_str_eq(str_out, expect), "test_in1 3", nfail, ntot)

end subroutine test_in1

subroutine test_in3(nfail, ntot)
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
	expect = '{"a":1,"b":{"a":11,"b":{"a":111,"b":"here''s a "string"","c":333,},"c":33,"d":44,},"c":3,"d":4,"e":5,}'
	TEST(is_str_eq(str_out, expect), "test_in1 2", nfail, ntot)

end subroutine test_in3

subroutine test_in4(nfail, ntot)
	! With duplicate keys, the last value is retained and its order is correct
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
	expect = '{"a":1,"c":3,"b":9999,}'
	TEST(is_str_eq(str_out, expect), "test_in1 2", nfail, ntot)

end subroutine test_in4

subroutine test_in5(nfail, ntot)
	! With duplicate keys, the last value is retained and its order is correct
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
	expect = '{"a":1111,"c":3333,"b":9999,}'
	TEST(is_str_eq(str_out, expect), "test_in1 2", nfail, ntot)

end subroutine test_in5

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

	! TODO: everything will break after trailing commas are removed
	expect = &
		'{'//LINE_FEED// &
		'    "a": 1,'//LINE_FEED// &
		'}'
	!print *, "expect = ", expect
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 1", nfail, ntot)

	json%compact = .true.
	str_out = json%to_str()
	expect = '{"a":1,}'
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 2", nfail, ntot)

end subroutine test_basic_jsons

end module jsonf__test

program test

	use jsonf__test
	implicit none
	integer :: nfail, ntot
	
	write(*,*) fg_bright_magenta//"Starting jsonf unit tests"//color_reset
	nfail = 0
	ntot = 0

	call test_basic_jsons(nfail, ntot)
	call test_in1(nfail, ntot)
	call test_in3(nfail, ntot)
	call test_in4(nfail, ntot)
	call test_in5(nfail, ntot)

	if (nfail == 0) then
		write(*, "(a,i0,a)") fg_bold // fg_green // " All ", ntot, " tests passed " // color_reset
	else
		write(*, "(a,i0,a,i0,a)") fg_bold_bright_red // " Error: ", nfail, "/", ntot, " tests failed " // color_reset
	end if
	if (nfail /= 0) call panic("")
	call jsonf_exit(EXIT_SUCCESS)

end program test

