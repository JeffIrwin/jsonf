program example
	use jsonf
	implicit none

	type(json_t) :: j
	integer(kind=8) :: age, val
	real(kind=8), allocatable :: scores(:)
	logical :: found

	call j%read_str('{"name": "Alice", "age": 30, "scores": [95.0, 87.0, 92.0]}')
	if (.not. j%is_ok) then
		call j%print_errors()
		stop 1
	end if

	age = j%get_i64('/age')
	print *, 'age:', age    ! 30

	scores = j%get_vec_f64('/scores')
	print *, 'scores:', scores    ! 95.0 87.0 92.0

	val = j%get_i64('/missing', found)
	if (.not. found) print *, 'key not found'
end program example
