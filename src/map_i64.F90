module map_i64_m
	use utils_m
	implicit none

	private :: map_entry_i64_t, set_map_i64_core

	private :: djb2_hash  ! ok to expose this if needed. could be moved to utils

	!********

	type map_entry_i64_t
		character(len=:), allocatable :: key
		integer(kind=8) :: val
	end type map_entry_i64_t

	!********
	type map_i64_t
		type(map_entry_i64_t), allocatable :: entries(:)
		integer(kind=8) :: len_ = 0
		contains
			procedure :: set => set_map_i64
			procedure :: get => get_map_i64
			procedure, private :: set_core => set_map_i64_core
	end type map_i64_t

contains

	function djb2_hash(str) result(hash)
		! DJB2 hash function implementation
		character(len=*), intent(in) :: str
		integer(8) :: hash
		integer :: i

		hash = 5381
		do i = 1, len(str)
			hash = ((hash * 33) + iachar(str(i:i)))  ! hash * 33 + c
		end do
	end function djb2_hash

	function new_map_i64(cap) result(map)
		! Create a new empty map
		type(map_i64_t) :: map
		integer(8), intent(in), optional :: cap
		integer(8) :: cap_
		map%len_ = 0
		cap_ = 256
		if (present(cap)) then
			cap_ = cap
		end if
		allocate(map%entries(cap_))
	end function new_map_i64

	recursive subroutine set_map_i64(this, key, val)
		! Set a key-value pair in the map
		class(map_i64_t), intent(inout) :: this
		character(len=*), intent(in) :: key
		integer(kind=8), intent(in) :: val
		!********
		integer(kind=8) :: i, n, n0
		type(map_entry_i64_t), allocatable :: old_entries(:)

		n0 = size(this%entries)
		if (this%len_ * 2 >= n0) then
			! Resize the entries array if load factor exceeds 0.5
			n = n0 * 2
			call move_alloc(this%entries, old_entries)
			allocate(this%entries(n))
			this%len_ = 0
			do i = 1, n0
				if (allocated(old_entries(i)%key)) then
					call this%set_core(old_entries(i)%key, old_entries(i)%val)
				end if
			end do
			deallocate(old_entries)
		end if

		call this%set_core(key, val)

	end subroutine set_map_i64

	subroutine set_map_i64_core(this, key, val)
		! This fn just sets without resizing
		class(map_i64_t), intent(inout) :: this
		character(len=*), intent(in) :: key
		integer(kind=8), intent(in) :: val
		!********
		integer(8) :: hash, idx, n

		hash = djb2_hash(key)
		n = size(this%entries)
		idx = mod(hash, n) + 1
		do
			if (.not. allocated(this%entries(idx)%key)) then
				! Empty slot found, insert new entry
				this%entries(idx)%key = key
				this%entries(idx)%val = val
				this%len_ = this%len_ + 1
				exit
			else if (is_str_eq(this%entries(idx)%key, key)) then
				! Key already exists, update value
				this%entries(idx)%val = val
				exit
			else
				! Collision, try next index (linear probing)
				idx = mod(idx, n) + 1
			end if
		end do

	end subroutine set_map_i64_core

	function get_map_i64(this, key, found) result(val)
		! Get a value by key from the map
		class(map_i64_t), intent(in) :: this
		character(len=*), intent(in) :: key
		logical, intent(out), optional :: found
		integer(kind=8) :: val
		integer(8) :: hash, idx
		logical :: found_
		found_ = .false.
		val = 0  ! Default value if not found
		hash = djb2_hash(key)
		idx = mod(hash, size(this%entries)) + 1
		do
			if (.not. allocated(this%entries(idx)%key)) then
				! Empty slot, key not found
				exit
			else if (is_str_eq(this%entries(idx)%key, key)) then
				! Key found
				val = this%entries(idx)%val
				found_ = .true.
				exit
			else
				! Collision, try next index (linear probing)
				idx = mod(idx, size(this%entries)) + 1
			end if
		end do
		if (present(found)) found = found_
	end function get_map_i64

end module map_i64_m

