use #[Int, Bool, Ordered] from: Core

class VoidPtr of Ordered is native[repr: `voidptr`] {
	on [offset: (Int)] (VoidPtr) is native `ptr_add`

	on [copy: length (Int) to: dest (VoidPtr)] is native `ptr_copy_to`
	;on [copy: length (Int)] (VoidPtr) is inline => return this[new: length]

	on [move: length (Int) to: dest (VoidPtr)] is native `ptr_move_to`
	
	on [compare: length (Int) with: other (VoidPtr)] (Int32) is native `ptr_cmp_with`
	
	operator `++` (VoidPtr) => return this + 1
	operator `--` (VoidPtr) => return this - 1


	operator `+` [offset (Int)] (This) is native `ptr_add`
	operator `-` [offset (Int)] (This) is native `ptr_sub`
	
	operator `<` [other (VoidPtr)] (Bool) => return this[UInt64] < other[UInt64]
	operator `<=` [other (VoidPtr)] (Bool) => return this[UInt64] <= other[UInt64]

	operator `>` [other (VoidPtr)] (Bool) => return this[UInt64] > other[UInt64]
	operator `>=` [other (VoidPtr)] (Bool) => return this[UInt64] >= other[UInt64]


	on [UInt64] is native `ptr_addr`
}