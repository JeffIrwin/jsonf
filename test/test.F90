
module jsonf__test

	use utils_m
	use jsonf
	implicit none

contains

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

	str = read_file(filename)
	!print *, "str = "//LINE_FEED//str
	call json%read_str(str)
	str_out = json%to_str()
	!print *, "str_out = ", str_out
	expect = &
		'{' // LINE_FEED // &
		'    "c": "my str",' // LINE_FEED // &
		'    "a": 1,' // LINE_FEED // &
		'    "b": 2,' // LINE_FEED // &
		'}'
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 1", nfail, ntot)

end subroutine test_in1

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
		'    "a": 1,'//LINE_FEED// &
		'}'
	!print *, "expect = ", expect
	TEST(is_str_eq(str_out, expect), "test_basic_jsons 1", nfail, ntot)

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

	if (nfail == 0) then
		write(*, "(a,i0,a)") fg_bold // fg_green // " All ", ntot, " tests passed " // color_reset
	else
		write(*, "(a,i0,a,i0,a)") fg_bold_bright_red // " Error: ", nfail, "/", ntot, " tests failed " // color_reset
	end if
	if (nfail /= 0) call panic("")
	call jsonf_exit(EXIT_SUCCESS)

end program test

