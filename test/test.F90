
module jsonf__test

	use utils_m
	use jsonf
	implicit none

contains

subroutine test_in1()
	character(len=:), allocatable :: str, filename
	type(json_t) :: json

	write(*,*) "Starting test_in1()"

	filename = "data/in1.json"
	str = read_file(filename)
	print *, "str = "//LINE_FEED//str
	call json%read_str(str)

end subroutine test_in1

subroutine test_basic_jsons()
	character(len=:), allocatable :: str, str_out, expect
	type(json_t) :: json

	str = '{"a": 1}'
	call json%read_str(str)
	str_out = json%to_str()
	print *, "str_out = ", str_out
	expect = &
		'{'//LINE_FEED// &
		'    "a": 1,'//LINE_FEED// &
		'}'
	print *, "expect = ", expect
	if (.not. is_str_eq(str_out, expect)) then
		call panic("test_basic_jsons test 1 failed")
	end if

end subroutine test_basic_jsons

end module jsonf__test

program test

	use jsonf__test
	implicit none
	
	write(*,*) "Starting jsonf test"

	call test_basic_jsons()
	call test_in1()

	write(*,*) "Finished jsonf test"

end program test

