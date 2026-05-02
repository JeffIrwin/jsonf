program example
	use jsonf
	implicit none

	type(json_t) :: j
	integer(kind=8) :: len_
	integer(int64) :: start, finish, count_rate
	real(kind=8), allocatable :: coords1(:)!, coord_mat1(:,:)

	call system_clock(start, count_rate)
	call j%read_file("scratch/canada.json")
	if (.not. j%is_ok) then
		call j%print_errors()
		stop 1
	end if
	call system_clock(finish)
	write(*,'(A30,1X,F7.4,1X,A)') 'jsonf : ', (finish-start)/real(count_rate,real64), ' seconds'

	len_ = j%len("/features/0/geometry/coordinates")
	print *, "len_ = ", len_

	coords1 = j%get_vec_f64("/features/0/geometry/coordinates/0/0")
	print *, "coords1 = ", coords1

	!coord_mat1 = j%get_mat_f64("/features/0/geometry/coordinates/0")
	!print *, "coord_mat1 = ", coord_mat1

end program example
