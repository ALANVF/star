use #[Int, Bool, Ordered, Comparable] from: Core

type T ;if T != Void
class Ptr[T] of Ordered is native[repr: `ptr` elem: T] {
	on [new] (This) is static {
		return This[new: 0]
	}

	on [pointTo: value (T)] (This) is static {
		return This[new: 1]
		-> deref = value
	}


	init [new: length (Int)] is native `ptr_new`


	on [new: length (Int)] (This) {
		my result = This[new: length]
		
		this[copy: length to: result]

		return result
	}
	

	on [deref] (T) is getter is native `ptr_get_deref`
	on [deref: (T)] is setter is native `ptr_set_deref`

	on [at: (Int)] (T) is native `ptr_get_at`
	on [at: (Int) set: (T)] is setter is native `ptr_set_at`

	on [offset: (Int)] (Ptr[T]) is native `ptr_add`

	on [resized: length (Int)] (Ptr[T]) is native `ptr_resized`
	
	on [copy: length (Int) to: dest (Ptr[T])] is native `ptr_copy_to`
	on [copy: length (Int)] (Ptr[T]) is inline => return this[new: length]

	on [move: length (Int) to: dest (Ptr[T])] is native `ptr_move_to`
	
	on [compare: length (Int) with: other (Ptr[T])] (Int32) is native `ptr_cmp_with`
	
	on [fill: length (Int) with: value (T)] is native `ptr_fill_with`

	on [next] (Ptr[T]) => return this + 1
	on [previous] (Ptr[T]) => return this - 1


	operator `+` [offset (Int)] (This) is native `ptr_add`
	operator `-` [offset (Int)] (This) is native `ptr_sub`
	
	operator `<` [other (Ptr[T])] (Bool) => return this[UInt64] < other[UInt64]
	operator `<=` [other (Ptr[T])] (Bool) => return this[UInt64] <= other[UInt64]

	operator `>` [other (Ptr[T])] (Bool) => return this[UInt64] > other[UInt64]
	operator `>=` [other (Ptr[T])] (Bool) => return this[UInt64] >= other[UInt64]


	;on [Ptr[Void]] is native `cast_ptr_vptr`
	on [UInt64] is native `ptr_addr`
}

;[class Ptr[Void] of Comparable is native[repr: `voidptr`] {
	operator `<` [other (Ptr[Void])] (Bool) {
		return this[UInt64] < other[UInt64]
	}

	operator `<=` [other (Ptr[Void])] (Bool) {
		return this[UInt64] <= other[UInt64]
	}

	operator `>` [other (Ptr[Void])] (Bool) {
		return this[UInt64] > other[UInt64]
	}

	operator `>=` [other (Ptr[Void])] (Bool) {
		return this[UInt64] >= other[UInt64]
	}
	

	type T if T != Void
	on [Ptr[T]] is native `cast_vptr_ptr`
	on [UInt64] is native `ptr_addr`
}]