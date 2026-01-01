
module sort_m

	use blarg_m
	use utils_m
	implicit none

	interface sort
		! Sort in/out array in-place
		procedure :: sort_i32
		procedure :: sort_i64
	end interface sort

	interface sort64
		! Sort in/out array in-place where array size may be > 2 billion
		!procedure :: sort64_i32
		procedure :: sort64_i64
	end interface sort64

	interface sort_index
		! Return an index array without modifying arg
		procedure :: sort_index_i32
		procedure :: sort_index_i64
	end interface sort_index

	interface sort_index64
		! Return a 64-bit index array without modifying arg
		!procedure :: sort_index64_i32
		procedure :: sort_index64_i64
	end interface sort_index64

	private :: partition_i32
	private :: partition_i64
	private :: partition64_i64

	private :: sort_index_i32_sub
	private :: sort_index_i64_sub
	private :: sort_index64_i64_sub

contains

!===============================================================================

subroutine sort_i32(a)
	integer(kind=4), intent(inout) :: a(:)
	!********
	integer(kind=4), allocatable :: idx(:)
	idx = sort_index(a)
	a = a(idx)
end subroutine sort_i32

function sort_index_i32(a) result(idx)
	integer(kind=4), intent(in) :: a(:)
	integer(kind=4), allocatable :: idx(:)
	!********
	integer(kind=4) :: n32
	integer(kind=8) :: n

	n = size(a)
	if (n > huge(n32)) then
		call panic("array size `"//to_str(n)//"` is greater than max " // &
			"32-bit int.  Use sort_index64() instead")
	end if
	n32 = i32(n)
	idx = range_i32(n32)
	call sort_index_i32_sub(a, idx, 1, n32)

end function sort_index_i32

!===============================================================================

recursive subroutine sort_index_i32_sub(a, idx, lo, hi)
	! Quicksort an array `a` and return the sort permutation idx
	!
	! See also:  https://github.com/JeffIrwin/aoc-2022/blob/381da3c2d468b4e2a9d4bb1068d84a9e8ae6bec6/2022/23/main.f90#L114

	integer(kind=4), intent(in) :: a(:)
	integer(kind=4) :: idx(:)
	integer(kind=4), intent(in) :: lo, hi
	!********
	integer(kind=4) :: p

	if (lo >= hi .or. lo < 1) return
	p = partition_i32(a, idx, lo, hi)

	call sort_index_i32_sub(a, idx, lo, p - 1)
	call sort_index_i32_sub(a, idx, p + 1, hi)

end subroutine sort_index_i32_sub

!===============================================================================

function partition_i32(a, idx, lo, hi) result(ans)

	integer(kind=4), intent(in) :: a(:)
	integer(kind=4), intent(inout) :: idx(:)
	integer(kind=4) :: ans
	!********
	integer(kind=4) :: pivot
	integer(kind=4) :: lo, hi, i, j, mid

	! Median of three pivot
	mid = (lo + hi) / 2
	if (a(idx(mid)) < a(idx(lo))) then
		idx([lo, mid]) = idx([mid, lo])
	else if (a(idx(hi)) < a(idx(lo))) then
		idx([lo, hi]) = idx([hi, lo])
	else if (a(idx(mid)) < a(idx(hi))) then
		idx([mid, hi]) = idx([hi, mid])
	end if
	pivot = a(idx(hi))

	i = lo - 1
	do j = lo, hi - 1
		if (a(idx(j)) <= pivot) then
			i = i + 1
			idx([i, j]) = idx([j, i])
		end if
	end do

	i = i + 1
	idx([i, hi]) = idx([hi, i])
	ans = i

end function partition_i32

!===============================================================================

subroutine sort64_i64(a)
	integer(kind=8), intent(inout) :: a(:)
	!********
	integer(kind=8), allocatable :: idx(:)
	idx = sort_index64(a)
	a = a(idx)
end subroutine sort64_i64

function sort_index64_i64(a) result(idx)
	integer(kind=8), intent(in) :: a(:)
	integer(kind=8), allocatable :: idx(:)
	!********
	integer(kind=8) :: n

	n = size(a)
	idx = range_i64(n)
	call sort_index64_i64_sub(a, idx, 1_8, n)

end function sort_index64_i64

!===============================================================================

recursive subroutine sort_index64_i64_sub(a, idx, lo, hi)
	! Quicksort an array `a` and return the sort permutation idx
	!
	! See also:  https://github.com/JeffIrwin/aoc-2022/blob/381da3c2d468b4e2a9d4bb1068d84a9e8ae6bec6/2022/23/main.f90#L114

	integer(kind=8), intent(in) :: a(:)
	integer(kind=8) :: idx(:)
	integer(kind=8), intent(in) :: lo, hi
	!********
	integer(kind=8) :: p

	if (lo >= hi .or. lo < 1) return
	p = partition64_i64(a, idx, lo, hi)

	call sort_index64_i64_sub(a, idx, lo, p - 1)
	call sort_index64_i64_sub(a, idx, p + 1, hi)

end subroutine sort_index64_i64_sub

!===============================================================================

function partition64_i64(a, idx, lo, hi) result(ans)

	integer(kind=8), intent(in) :: a(:)
	integer(kind=8), intent(inout) :: idx(:)
	integer(kind=8) :: ans
	!********
	integer(kind=8) :: pivot
	integer(kind=8) :: lo, hi, i, j, mid

	! Median of three pivot
	mid = (lo + hi) / 2
	if (a(idx(mid)) < a(idx(lo))) then
		idx([lo, mid]) = idx([mid, lo])
	else if (a(idx(hi)) < a(idx(lo))) then
		idx([lo, hi]) = idx([hi, lo])
	else if (a(idx(mid)) < a(idx(hi))) then
		idx([mid, hi]) = idx([hi, mid])
	end if
	pivot = a(idx(hi))

	i = lo - 1
	do j = lo, hi - 1
		if (a(idx(j)) <= pivot) then
			i = i + 1
			idx([i, j]) = idx([j, i])
		end if
	end do

	i = i + 1
	idx([i, hi]) = idx([hi, i])
	ans = i

end function partition64_i64

!===============================================================================

subroutine sort_i64(a)
	integer(kind=8), intent(inout) :: a(:)
	!********
	integer(kind=4), allocatable :: idx(:)
	idx = sort_index(a)
	a = a(idx)
end subroutine sort_i64

function sort_index_i64(a) result(idx)
	integer(kind=8), intent(in) :: a(:)
	integer(kind=4), allocatable :: idx(:)
	!********
	integer(kind=4) :: n32
	integer(kind=8) :: n

	n = size(a)
	!print *, "huge(n32) = ", huge(n32)
	if (n > huge(n32)) then
		call panic("array size `"//to_str(n)//"` is greater than max " // &
			"32-bit int.  Use sort_index64() instead")
	end if
	n32 = i32(n)
	idx = range_i32(n32)
	call sort_index_i64_sub(a, idx, 1, n32)

end function sort_index_i64

!===============================================================================

recursive subroutine sort_index_i64_sub(a, idx, lo, hi)
	! Quicksort an array `a` and return the sort permutation idx
	!
	! See also:  https://github.com/JeffIrwin/aoc-2022/blob/381da3c2d468b4e2a9d4bb1068d84a9e8ae6bec6/2022/23/main.f90#L114

	integer(kind=8), intent(in) :: a(:)
	integer(kind=4) :: idx(:)
	integer(kind=4), intent(in) :: lo, hi
	!********
	integer(kind=4) :: p

	if (lo >= hi .or. lo < 1) return
	p = partition_i64(a, idx, lo, hi)

	call sort_index_i64_sub(a, idx, lo, p - 1)
	call sort_index_i64_sub(a, idx, p + 1, hi)

end subroutine sort_index_i64_sub

!===============================================================================

function partition_i64(a, idx, lo, hi) result(ans)

	integer(kind=8), intent(in) :: a(:)
	integer(kind=4), intent(inout) :: idx(:)
	integer(kind=4) :: ans
	!********
	integer(kind=8) :: pivot
	integer(kind=4) :: lo, hi, i, j, mid

	! Median of three pivot
	mid = (lo + hi) / 2
	if (a(idx(mid)) < a(idx(lo))) then
		idx([lo, mid]) = idx([mid, lo])
	else if (a(idx(hi)) < a(idx(lo))) then
		idx([lo, hi]) = idx([hi, lo])
	else if (a(idx(mid)) < a(idx(hi))) then
		idx([mid, hi]) = idx([hi, mid])
	end if
	pivot = a(idx(hi))

	i = lo - 1
	do j = lo, hi - 1
		if (a(idx(j)) <= pivot) then
			i = i + 1
			idx([i, j]) = idx([j, i])
		end if
	end do

	i = i + 1
	idx([i, hi]) = idx([hi, i])
	ans = i

end function partition_i64

!===============================================================================

end module sort_m

