class Int32 of Core.Ordered is native[repr: `int` bits: 32 signed: true] is strong {
	operator `?` (Core.Bool) is native `i32_truthy`
	operator `-` (This) is native `i32_neg`
	operator `~` (This) is native `i32_compl`
	operator `++` (This) is native `i32_succ`
	operator `--` (This) is native `i32_pred`
	
	operator `+` [int (This)] (This) is native `i32_add`
	operator `-` [int (This)] (This) is native `i32_sub`
	operator `*` [int (This)] (This) is native `i32_mult`
	operator `**` [int (This)] (This) is native `i32_pow`
	operator `/` [int (This)] (Core.Dec) is native `i32_div`
	operator `//` [int (This)] (This) is native `i32_idiv`
	operator `%` [int (This)] (This) is native `i32_mod`
	operator `%%` [int (This)] (Core.Bool) is native `i32_mod0`
	operator `&` [int (This)] (This) is native `i32_and`
	operator `|` [int (This)] (This) is native `i32_or`
	operator `^` [int (This)] (This) is native `i32_xor`
	operator `<<` [int (This)] (This) is native `i32_shl`
	operator `>>` [int (This)] (This) is native `i32_shr`
	operator `?=` [int (This)] (Core.Bool) is native `i32_eq`
	operator `!=` [int (This)] (Core.Bool) is native `i32_ne`
	operator `>` [int (This)] (Core.Bool) is native `i32_gt`
	operator `>=` [int (This)] (Core.Bool) is native `i32_ge`
	operator `<` [int (This)] (Core.Bool) is native `i32_lt`
	operator `<=` [int (This)] (Core.Bool) is native `i32_le`

	
	on [Core.Bool] is native `cast_i32_bool`
	on [Int8] is native `cast_i32_i8`
	on [UInt8] is native `cast_i32_u8`
	on [Int16] is native `cast_i32_i16`
	on [UInt16] is native `cast_i32_u16`
	on [UInt32] is native `cast_i32_u32`
	on [Int64] is native `cast_i32_i64`
	on [UInt64] is native `cast_i32_u64`
	on [Core.Dec] is native `cast_i32_d64`
	on [Core.Str] is native `cast_i32_str`
}