class Int64 of Core.Ordered is native[repr: `int` bits: 64 signed: false] is strong {
	on [next] (This) is native `u64_succ`
	on [previous] (This) is native `u64_pred`
	

	operator `?` (Core.Bool) is native `u64_truthy`
	operator `-` (This) is native `u64_neg`
	operator `~` (This) is native `u64_compl`
	
	operator `+` [int (This)] (This) is native `u64_add`
	operator `-` [int (This)] (This) is native `u64_sub`
	operator `*` [int (This)] (This) is native `u64_mult`
	operator `**` [int (This)] (This) is native `u64_pow`
	operator `/` [int (This)] (Core.Dec) is native `u64_div`
	operator `//` [int (This)] (This) is native `u64_idiv`
	operator `%` [int (This)] (This) is native `u64_mod`
	operator `%%` [int (This)] (Core.Bool) is native `u64_mod0`
	operator `&` [int (This)] (This) is native `u64_and`
	operator `|` [int (This)] (This) is native `u64_or`
	operator `^` [int (This)] (This) is native `u64_xor`
	operator `<<` [int (This)] (This) is native `u64_shl`
	operator `>>` [int (This)] (This) is native `u64_shr`
	operator `?=` [int (This)] (Core.Bool) is native `u64_eq`
	operator `!=` [int (This)] (Core.Bool) is native `u64_ne`
	operator `>` [int (This)] (Core.Bool) is native `u64_gt`
	operator `>=` [int (This)] (Core.Bool) is native `u64_ge`
	operator `<` [int (This)] (Core.Bool) is native `u64_lt`
	operator `<=` [int (This)] (Core.Bool) is native `u64_le`

	
	on [Core.Bool] is native `cast_u64_bool`
	on [Int8] is native `cast_u64_i8`
	on [UInt8] is native `cast_u64_u8`
	on [Int16] is native `cast_u64_i16`
	on [UInt16] is native `cast_u64_u16`
	on [Int32] is native `cast_u64_i32`
	on [UInt32] is native `cast_u64_u32`
	on [Int64] is native `cast_u64_i64`
	on [Core.Dec] is native `cast_u64_d64`
	on [Core.Str] is native `cast_u64_str`
	type T
	on [Ptr[T]] is native `cast_u64_ptr`
}