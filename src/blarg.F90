
!! The numa version of this file used PANIC as a macro which is non-terminating
!! if optional iostat arg is present.  Maybe useful in a lib like numa, but not
!! here in AOC
!#include "panic.F90"

!****************

!> Basic Linear Algebra Routine Group
!>
!> > blarg:  (informal) Expressing frustration or disappointment.
!> >
!> > Blarg! I'm sick of this.
!>
!> Ported from numerical-analysis:
!>
!>     https://github.com/JeffIrwin/numerical-analysis/blob/main/src/blarg.F90
!>
module jsonf__blarg

	use jsonf__utils
	implicit none

	interface outer_product
		procedure :: outer_product_c64
		procedure :: outer_product_f64
	end interface outer_product

	interface zeros_f32
		procedure :: zeros_vec_f32
		procedure :: zeros_mat_f32
	end interface zeros_f32

	interface zeros_f64
		procedure :: zeros_vec_f64
		procedure :: zeros_mat_f64
	end interface zeros_f64

	interface zeros_i32
		procedure :: zeros_vec_i32
		procedure :: zeros_mat_i32
	end interface zeros_i32

	interface zeros_i64
		procedure :: zeros_vec_i64
		procedure :: zeros_mat_i64
	end interface zeros_i64

	interface ones_i32
		procedure :: ones_vec_i32
		procedure :: ones_mat_i32
	end interface ones_i32

	interface ones_f64
		procedure :: ones_vec_f64
		procedure :: ones_mat_f64
	end interface ones_f64

	interface falses
		procedure :: falses_vec
		procedure :: falses_mat
	end interface falses
	interface trues
		procedure :: trues_vec
		procedure :: trues_mat
	end interface trues

	interface diag
		procedure :: diag_set
		procedure :: diag_get
		procedure :: diag_get_c64
	end interface diag

	interface triu
		procedure :: triu_c64
		procedure :: triu_f64
	end interface triu

	interface hstack
		procedure :: hstack_mat_mat
		procedure :: hstack_mat_vec
		procedure :: hstack_mat_vec_i32
	end interface hstack

	interface vstack
		procedure :: vstack_mat_mat
		procedure :: vstack_mat_vec
	end interface vstack

	interface mask_to_index
		procedure :: mask_to_index_vec
		procedure :: mask_to_index_mat
	end interface mask_to_index

	interface range_f64
		procedure :: range_f64_count
		procedure :: range_f64_step
	end interface range_f64
	interface range_i32
		procedure :: range_i32_start_stop
		procedure :: range_i32_step
		procedure :: range_i32_stop
	end interface range_i32
	interface range_i64
		procedure :: range_i64_start_stop
		procedure :: range_i64_step
		procedure :: range_i64_stop
	end interface range_i64

contains

!===============================================================================

function outer_product_f64(a, b) result(c)
	double precision, intent(in) :: a(:), b(:)
	double precision, allocatable :: c(:,:)

	integer :: i, j, na, nb
	na = size(a)
	nb = size(b)

	allocate(c(na, nb))
	do j = 1, nb
	do i = 1, na
		c(i,j) = a(i) * b(j)
	end do
	end do

end function outer_product_f64

function outer_product_c64(a, b) result(c)
	double complex, intent(in) :: a(:), b(:)
	double complex, allocatable :: c(:,:)

	integer :: i, j, na, nb
	na = size(a)
	nb = size(b)

	allocate(c(na, nb))
	do j = 1, nb
	do i = 1, na
		!c(i,j) = a(i) * conjg(b(j))
		c(i,j) = a(i) * b(j)
	end do
	end do

end function outer_product_c64

!********

function eye(n)
	! n x n identity matrix

	integer, intent(in) :: n
	double precision, allocatable :: eye(:,:)

	integer :: i, j
	allocate(eye(n, n))
	do i = 1, n
		do j = 1, n
			if (i == j) then
				eye(i,j) = 1.d0
			else
				eye(i,j) = 0.d0
			end if
		end do
	end do

end function eye

!****************

double precision function norm2c(v)

	double complex, intent(in) :: v(:)

	! It's weird that Fortran's intrinsic norm2() can't handle complex args.
	! Intel MKL has dznrm2() which is equivalent
	!
	! Note that dot_product() already conjugates one of the arguments, so there
	! is no need for additional conjugation

	!print *, "v = ", v
	norm2c = dble(sqrt(dot_product(v, v)))

end function norm2c

!===============================================================================

function zeros_mat_f64(m, n) result(a)
	! m x n matrix of 0
	integer, intent(in) :: m, n
	double precision, allocatable :: a(:,:)
	allocate(a(m, n))
	a = 0
end function zeros_mat_f64

function zeros_vec_f64(n) result(a)
	! Size n vector of 0
	integer, intent(in) :: n
	double precision, allocatable :: a(:)
	allocate(a(n))
	a = 0
end function zeros_vec_f64

function zeros_mat_f32(m, n) result(a)
	! m x n matrix of 0
	integer, intent(in) :: m, n
	real, allocatable :: a(:,:)
	allocate(a(m, n))
	a = 0
end function zeros_mat_f32

function zeros_vec_f32(n) result(a)
	! Size n vector of 0
	integer, intent(in) :: n
	real, allocatable :: a(:)
	allocate(a(n))
	a = 0
end function zeros_vec_f32

function ones_mat_f64(m, n) result(a)
	! m x n matrix of 0
	integer, intent(in) :: m, n
	double precision, allocatable :: a(:,:)
	allocate(a(m, n))
	a = 1
end function ones_mat_f64

function ones_vec_f64(n) result(a)
	! Size n vector of 0
	integer, intent(in) :: n
	double precision, allocatable :: a(:)
	allocate(a(n))
	a = 1
end function ones_vec_f64

function ones_mat_i32(m, n) result(a)
	! m x n matrix of 0
	integer, intent(in) :: m, n
	integer, allocatable :: a(:,:)
	allocate(a(m, n))
	a = 1
end function ones_mat_i32

function ones_vec_i32(n) result(a)
	! Size n vector of 0
	integer, intent(in) :: n
	integer, allocatable :: a(:)
	allocate(a(n))
	a = 1
end function ones_vec_i32

function zeros_mat_i32(m, n) result(a)
	! m x n matrix of 0
	integer, intent(in) :: m, n
	integer, allocatable :: a(:,:)
	allocate(a(m, n))
	a = 0
end function zeros_mat_i32

function zeros_vec_i32(n) result(a)
	! Size n vector of 0
	integer, intent(in) :: n
	integer, allocatable :: a(:)
	allocate(a(n))
	a = 0
end function zeros_vec_i32

function zeros_mat_i64(m, n) result(a)
	! m x n matrix of 0
	integer, intent(in) :: m, n
	integer(kind=8), allocatable :: a(:,:)
	allocate(a(m, n))
	a = 0
end function zeros_mat_i64

function zeros_vec_i64(n) result(a)
	! Size n vector of 0
	integer, intent(in) :: n
	integer(kind=8), allocatable :: a(:)
	allocate(a(n))
	a = 0
end function zeros_vec_i64

function falses_vec(n) result(a)
	! Size n vector of .false.
	integer, intent(in) :: n
	logical, allocatable :: a(:)
	allocate(a(n))
	a = .false.
end function falses_vec

function falses_mat(m, n) result(a)
	! Size n vector of .false.
	integer, intent(in) :: m, n
	logical, allocatable :: a(:,:)
	allocate(a(m, n))
	a = .false.
end function falses_mat

function trues_vec(n) result(a)
	! Size n vector of .true.
	integer, intent(in) :: n
	logical, allocatable :: a(:)
	allocate(a(n))
	a = .true.
end function trues_vec

function trues_mat(m, n) result(a)
	! Size n vector of .true.
	integer, intent(in) :: m, n
	logical, allocatable :: a(:,:)
	allocate(a(m, n))
	a = .true.
end function trues_mat

!********

function diag_set(v) result(d)
	! Spread a diagonal vector `v` into a matrix
	double precision, intent(in) :: v(:)
	double precision, allocatable :: d(:,:)
	!********
	integer :: i, n

	n = size(v)
	d = zeros_f64(n, n)
	do i = 1, n
		d(i,i) = v(i)
	end do

end function diag_set

function diag_get(a) result(v)
	! Get the diagonal vector `v` from a matrix `a`
	double precision, intent(in) :: a(:,:)
	double precision, allocatable :: v(:)
	!********
	integer :: i, n

	n = min(size(a,1), size(a,2))
	allocate(v(n))
	do i = 1, n
		v(i) = a(i,i)
	end do

end function diag_get

function diag_get_c64(a) result(v)
	! Get the diagonal vector `v` from a matrix `a`
	double complex, intent(in) :: a(:,:)
	double complex, allocatable :: v(:)
	!********
	integer :: i, n

	n = min(size(a,1), size(a,2))
	allocate(v(n))
	do i = 1, n
		v(i) = a(i,i)
	end do

end function diag_get_c64

!===============================================================================

function triu_f64(a) result(r)
	! Explicitly get R by zeroing lower triangle
	double precision, intent(in) :: a(:,:)
	double precision, allocatable :: r(:,:)
	!********
	integer :: i, n

	n = min(size(a,1), size(a,2))
	r = a(:n, :n)
	do i = 1, n
		r(i+1:, i) = 0
	end do

end function triu_f64

!********

function triu_c64(a) result(r)
	! Explicitly get R (U) by zeroing lower triangle
	double complex, intent(in) :: a(:,:)
	double complex, allocatable :: r(:,:)
	!********
	integer :: i

	r = a
	do i = 1, size(a, 1)
		r(i+1:, i) = 0
	end do

end function triu_c64

!===============================================================================

function vstack_mat_vec(a, b, iostat) result(c)
	double precision, intent(in) :: a(:,:), b(:)
	double precision, allocatable :: c(:,:)
	integer, optional, intent(out) :: iostat
	!********
	character(len = :), allocatable :: msg
	integer :: na
	integer, parameter :: nb = 1

	if (present(iostat)) iostat = 0

	! Sizes must match, unless at least one of the arrays is empty
	if (size(a,2) /= size(b) .and. size(a) > 0 .and. size(b) > 0) then
		msg = "size(a,2) does not match size(b) in vstack_mat_vec()"
		!call PANIC(msg, present(iostat))
		call panic(msg)
		iostat = 1
		return
	end if

	na = size(a, 1)
	allocate(c(na+nb, size(a,2)))
	if (size(a,2) <= 0) return

	if (na > 0) c(1:na, :) = a
	if (nb > 0) c(na+1, :) = b

end function vstack_mat_vec

!********

function vstack_mat_mat(a, b, iostat) result(c)
	double precision, intent(in) :: a(:,:), b(:,:)
	double precision, allocatable :: c(:,:)
	integer, optional, intent(out) :: iostat
	!********
	character(len = :), allocatable :: msg
	integer :: na, nb

	if (present(iostat)) iostat = 0
	if (size(a,2) /= size(b,2) .and. size(a) > 0 .and. size(b) > 0) then
		msg = "size(a,2) does not match size(b,2) in vstack_mat_mat()"
		!call PANIC(msg, present(iostat))
		call panic(msg)
		iostat = 1
		return
	end if

	na = size(a, 1)
	nb = size(b, 1)

	allocate(c(na+nb, size(a,2)))
	if (size(a,2) <= 0) return

	if (na > 0) c(1:na , :) = a
	if (nb > 0) c(na+1:, :) = b

end function vstack_mat_mat

!===============================================================================

function hstack_mat_mat(a, b, iostat) result(c)
	! `hstack` and `vstack` are named after corresponding numpy functions
	double precision, intent(in) :: a(:,:), b(:,:)
	double precision, allocatable :: c(:,:)
	integer, optional, intent(out) :: iostat
	!********
	character(len = :), allocatable :: msg
	integer :: na, nb

	if (present(iostat)) iostat = 0
	if (size(a,1) /= size(b,1) .and. size(a) > 0 .and. size(b) > 0) then
		msg = "size(a,1) does not match size(b,1) in hstack_mat_mat()"
		!call PANIC(msg, present(iostat))
		call panic(msg)
		iostat = 1
		return
	end if

	na = size(a, 2)
	nb = size(b, 2)
	allocate(c(size(a,1), na+nb))
	if (size(a,1) <= 0) return

	if (na > 0) c(:, 1: na) = a
	if (nb > 0) c(:, na+1:) = b

end function hstack_mat_mat

!===============================================================================

function hstack_mat_vec(a, b, iostat) result(c)
	double precision, intent(in) :: a(:,:), b(:)
	double precision, allocatable :: c(:,:)
	integer, optional, intent(out) :: iostat
	!********
	character(len = :), allocatable :: msg
	integer :: na, nb

	if (present(iostat)) iostat = 0
	if (size(a,1) /= size(b) .and. size(a) > 0 .and. size(b) > 0) then
		msg = "size(a,1) does not match size(b) in hstack_mat_vec()"
		!call PANIC(msg, present(iostat))
		call panic(msg)
		iostat = 1
		return
	end if

	na = size(a, 2)
	nb = 1

	allocate(c(size(a,1), na+nb))
	if (size(a,1) <= 0) return

	if (na > 0) c(:, 1: na) = a
	if (nb > 0) c(:, na+1 ) = b

end function hstack_mat_vec

!===============================================================================

function hstack_mat_vec_i32(a, b, iostat) result(c)
	integer, intent(in) :: a(:,:), b(:)
	integer, allocatable :: c(:,:)
	integer, optional, intent(out) :: iostat
	!********
	character(len = :), allocatable :: msg
	integer :: na, nb

	if (present(iostat)) iostat = 0
	if (size(a,1) /= size(b) .and. size(a) > 0 .and. size(b) > 0) then
		msg = "size(a,1) does not match size(b) in hstack_mat_vec_i32()"
		!call PANIC(msg, present(iostat))
		call panic(msg)
		iostat = 1
		return
	end if

	na = size(a, 2)
	nb = 1

	allocate(c(size(a,1), na+nb))
	if (size(a,1) <= 0) return

	if (na > 0) c(:, 1: na) = a
	if (nb > 0) c(:, na+1 ) = b

end function hstack_mat_vec_i32

!===============================================================================

function mask_to_index_vec(mask) result(indices)
	! Convert a logical mask array to an index array `indices`
	!
	! For example:
	!
	!     mask_to_index([.true., .false., .false, .true.])
	!     ==
	!     [1, 4]
	!
	! This is useful if you actually need the index array.  If you just want the
	! result of indexing the parent array by the returned index array, you can
	! just use built-in pack(parent, mask) instead

	logical, intent(in) :: mask(:)
	integer, allocatable :: indices(:)
	!********
	integer :: i, j

	! Overallocate and then trim.  Could make two passes, explicitly or
	! implicitly using count()
	allocate(indices(size(mask)))
	i = 0
	do j = 1, size(mask)
		if (.not. (mask(j))) cycle
		i = i + 1
		indices(i) = j
	end do
	indices = indices(:i)  ! trim

end function mask_to_index_vec

!===============================================================================

function mask_to_index_mat(mask) result(indices)
	! Convert a logical mask array to an index array `indices`
	!
	! For example:
	!
	!     mask_to_index([.true., .false., .false, .true.])
	!     ==
	!     [1, 4]
	!
	! This is useful if you actually need the index array.  If you just want the
	! result of indexing the parent array by the returned index array, you can
	! just use built-in pack(parent, mask) instead

	logical, intent(in) :: mask(:,:)
	integer, allocatable :: indices(:,:)
	!********
	integer :: i, j, k

	! Overallocate and then trim.  Could make two passes, explicitly or
	! implicitly using count()
	allocate( indices(2, size(mask)) )
	i = 0
	do j = 1, size(mask,2)
	do k = 1, size(mask,1)
		if (.not. (mask(k,j))) cycle
		i = i + 1
		indices(:,i) = [k, j]
	end do
	end do
	indices = indices(:, :i)  ! trim

end function mask_to_index_mat

!===============================================================================

function reverse(a) result(r)
	integer, intent(in) :: a(:)
	integer, allocatable :: r(:)
	r = a(range_i32(size(a), 1, -1))
end function reverse

!===============================================================================

!> Return evenly spaced integers over a specified interval
!>
!> Following the Fortran convention, the stop_ bound is inclusive.  Compare the
!> similar functions linspace in MATLAB and arange in numpy
function range_i32_step(start_, stop_, step_) result(range_)
	! I settled on the `range` name just because it's shorter than `linspace`
	integer, intent(in) :: start_, stop_, step_
	integer, allocatable :: range_(:)
	!********
	integer :: i
	range_ = [(i, i = start_, stop_, step_)]
end function range_i32_step

!> Return evenly spaced integers over a specified interval
!>
!> Following the Fortran convention, the stop_ bound is inclusive.  Compare the
!> similar functions linspace in MATLAB and arange in numpy
function range_i32_start_stop(start_, stop_) result(range_)
	! I settled on the `range` name just because it's shorter than `linspace`
	integer, intent(in) :: start_, stop_
	integer, allocatable :: range_(:)
	!********
	integer, parameter :: step_ = 1
	range_ = range_i32_step(start_, stop_, step_)
end function range_i32_start_stop

!> Return evenly spaced integers over a specified interval
!>
!> Following the Fortran convention, the stop_ bound is inclusive.  Compare the
!> similar functions linspace in MATLAB and arange in numpy
function range_i32_stop(stop_) result(range_)
	! I settled on the `range` name just because it's shorter than `linspace`
	integer, intent(in) :: stop_
	integer, allocatable :: range_(:)
	!********
	integer, parameter :: start_ = 1, step_ = 1
	range_ = range_i32_step(start_, stop_, step_)
end function range_i32_stop

!> Return evenly spaced integers over a specified interval
!>
!> Following the Fortran convention, the stop_ bound is inclusive.  Compare the
!> similar functions linspace in MATLAB and arange in numpy
function range_i64_step(start_, stop_, step_) result(range_)
	! I settled on the `range` name just because it's shorter than `linspace`
	integer(kind=8), intent(in) :: start_, stop_, step_
	integer(kind=8), allocatable :: range_(:)
	!********
	integer(kind=8) :: i
	range_ = [(i, i = start_, stop_, step_)]
end function range_i64_step

!> Return evenly spaced integers over a specified interval
!>
!> Following the Fortran convention, the stop_ bound is inclusive.  Compare the
!> similar functions linspace in MATLAB and arange in numpy
function range_i64_start_stop(start_, stop_) result(range_)
	! I settled on the `range` name just because it's shorter than `linspace`
	integer(kind=8), intent(in) :: start_, stop_
	integer(kind=8), allocatable :: range_(:)
	!********
	integer(kind=8), parameter :: step_ = 1
	range_ = range_i64_step(start_, stop_, step_)
end function range_i64_start_stop

!> Return evenly spaced integers over a specified interval
!>
!> Following the Fortran convention, the stop_ bound is inclusive.  Compare the
!> similar functions linspace in MATLAB and arange in numpy
function range_i64_stop(stop_) result(range_)
	! I settled on the `range` name just because it's shorter than `linspace`
	integer(kind=8), intent(in) :: stop_
	integer(kind=8), allocatable :: range_(:)
	!********
	integer(kind=8), parameter :: start_ = 1, step_ = 1
	range_ = range_i64_step(start_, stop_, step_)
end function range_i64_stop

!********

!> Return evenly spaced doubles over a specified interval
!>
!> Following the Fortran convention, the stop_ bound is inclusive.  Compare the
!> similar functions linspace in MATLAB and arange in numpy
function range_f64_step(start_, stop_, step_) result(range_)
	! Might also want a range_f64() with start, step, and count (but no stop).
	! Will either need a non-overloaded name or a weird arg order
	double precision, intent(in) :: start_, stop_, step_
	double precision, allocatable :: range_(:)
	!********
	integer :: count_
	count_ = ceiling((stop_ - start_) / step_) + 1
	range_ = start_ + step_ * range_i32(0, count_ - 1)
	if (range_(count_) > stop_) then
		range_ = range_(: count_ - 1)
	end if
end function range_f64_step

!> Return evenly spaced doubles over a specified interval
!>
!> Following the Fortran convention, the stop_ bound is inclusive.  Compare the
!> similar functions linspace in MATLAB and arange in numpy
function range_f64_count(start_, stop_, count_) result(range_)
	double precision, intent(in) :: start_, stop_
	integer, intent(in) :: count_
	double precision, allocatable :: range_(:)
	!********
	double precision :: step_
	step_ = (stop_ - start_) / (count_ - 1)
	range_ = start_ + step_ * range_i32(0, count_ - 1)
end function range_f64_count

!===============================================================================

end module jsonf__blarg

