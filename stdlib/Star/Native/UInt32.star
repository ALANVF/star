class UInt32 of Core.Ordered is native[repr: `int` bits: 32 signed: false] {
	on [next] (This) is native `u32_succ`
	on [previous] (This) is native `u32_pred`
	

	operator `?` (Core.Bool) is native `u32_truthy`
	operator `-` (This) is native `u32_neg`
	operator `~` (This) is native `u32_compl`
	
	operator `+` [int (This)] (This) is native `u32_add`
	operator `-` [int (This)] (This) is native `u32_sub`
	operator `*` [int (This)] (This) is native `u32_mult`
	operator `**` [int (This)] (This) is native `u32_pow`
	operator `/` [int (This)] (Core.Dec) is native `u32_div`
	operator `//` [int (This)] (This) is native `u32_idiv`
	operator `%` [int (This)] (This) is native `u32_mod`
	operator `%%` [int (This)] (Core.Bool) is native `u32_mod0`
	operator `&` [int (This)] (This) is native `u32_and`
	operator `|` [int (This)] (This) is native `u32_or`
	operator `^` [int (This)] (This) is native `u32_xor`
	operator `<<` [int (This)] (This) is native `u32_shl`
	operator `>>` [int (This)] (This) is native `u32_shr`
	operator `?=` [int (This)] (Core.Bool) is native `u32_eq`
	operator `!=` [int (This)] (Core.Bool) is native `u32_ne`
	operator `>` [int (This)] (Core.Bool) is native `u32_gt`
	operator `>=` [int (This)] (Core.Bool) is native `u32_ge`
	operator `<` [int (This)] (Core.Bool) is native `u32_lt`
	operator `<=` [int (This)] (Core.Bool) is native `u32_le`

	
	on [Core.Bool] is native `cast_u32_bool`
	on [Int8] is native `cast_u32_i8`
	on [UInt8] is native `cast_u32_u8`
	on [Int16] is native `cast_u32_i16`
	on [UInt16] is native `cast_u32_u16`
	on [Int32] is native `cast_u32_i32`
	on [Int64] is native `cast_u32_i64`
	on [UInt64] is native `cast_u32_u64`
	on [Core.Dec] is native `cast_u32_d64`
	on [Core.Str] is native `cast_u32_str`
}