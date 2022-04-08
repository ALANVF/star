class Int64 of Core.Ordered is native[repr: `int` bits: 64 signed: true] is strong {
	operator `?` (Core.Bool) is native `i64_truthy`
	operator `-` (This) is native `i64_neg`
	operator `~` (This) is native `i64_compl`
	operator `++` (This) is native `i64_succ`
	operator `--` (This) is native `i64_pred`
	
	operator `+` [int (This)] (This) is native `i64_add`
	operator `-` [int (This)] (This) is native `i64_sub`
	operator `*` [int (This)] (This) is native `i64_mult`
	operator `**` [int (This)] (This) is native `i64_pow`
	operator `/` [int (This)] (Core.Dec) is native `i64_div`
	operator `//` [int (This)] (This) is native `i64_idiv`
	operator `%` [int (This)] (This) is native `i64_mod`
	operator `%%` [int (This)] (Core.Bool) is native `i64_mod0`
	operator `&` [int (This)] (This) is native `i64_and`
	operator `|` [int (This)] (This) is native `i64_or`
	operator `^` [int (This)] (This) is native `i64_xor`
	operator `<<` [int (This)] (This) is native `i64_shl`
	operator `>>` [int (This)] (This) is native `i64_shr`
	operator `?=` [int (This)] (Core.Bool) is native `i64_eq`
	operator `!=` [int (This)] (Core.Bool) is native `i64_ne`
	operator `>` [int (This)] (Core.Bool) is native `i64_gt`
	operator `>=` [int (This)] (Core.Bool) is native `i64_ge`
	operator `<` [int (This)] (Core.Bool) is native `i64_lt`
	operator `<=` [int (This)] (Core.Bool) is native `i64_le`

	
	on [Core.Bool] is native `cast_i64_bool`
	on [Int8] is native `cast_i64_i8`
	on [UInt8] is native `cast_i64_u8`
	on [Int16] is native `cast_i64_i16`
	on [UInt16] is native `cast_i64_u16`
	on [Int32] is native `cast_i64_i32`
	on [UInt32] is native `cast_i64_u32`
	on [UInt64] is native `cast_i64_u64`
	on [Core.Dec] is native `cast_i64_d64`
	on [Core.Str] is native `cast_i64_str`
}