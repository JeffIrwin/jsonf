program example
	use jsonf
	implicit none

	type(json_t) :: j
	integer(kind=8) :: len_
	integer(int64) :: start, finish, count_rate

	call system_clock(start, count_rate)
	call j%read_file("scratch/objects.json")
	if (.not. j%is_ok) then
		call j%print_errors()
		stop 1
	end if
	call system_clock(finish)
	write(*,'(A30,1X,F7.4,1X,A)') 'jsonf : ', (finish-start)/real(count_rate,real64), ' seconds'

	len_ = j%len("/")
	print *, "len_('/') = ", len_

	len_ = j%len("/user_0000")
	print *, "len_('/user_0000') = ", len_

end program example
