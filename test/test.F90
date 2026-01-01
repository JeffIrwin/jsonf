
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

end module jsonf__test

program test

	use jsonf__test
	implicit none
	
	write(*,*) "Starting jsonf test"

	call test_in1()

	write(*,*) "Finished jsonf test"

end program test

