class Int16 of Core.Ordered is native[repr: `int` bits: 16 signed: true] is strong {
	on [next] (This) is native `i16_succ`
	on [previous] (This) is native `i16_pred`

	
	operator `?` (Core.Bool) is native `i16_truthy`
	operator `-` (This) is native `i16_neg`
	operator `~` (This) is native `i16_compl`
	
	operator `+` [int (This)] (This) is native `i16_add`
	operator `-` [int (This)] (This) is native `i16_sub`
	operator `*` [int (This)] (This) is native `i16_mult`
	operator `**` [int (This)] (This) is native `i16_pow`
	operator `/` [int (This)] (Core.Dec) is native `i16_div`
	operator `//` [int (This)] (This) is native `i16_idiv`
	operator `%` [int (This)] (This) is native `i16_mod`
	operator `%%` [int (This)] (Core.Bool) is native `i16_mod0`
	operator `&` [int (This)] (This) is native `i16_and`
	operator `|` [int (This)] (This) is native `i16_or`
	operator `^` [int (This)] (This) is native `i16_xor`
	operator `<<` [int (This)] (This) is native `i16_shl`
	operator `>>` [int (This)] (This) is native `i16_shr`
	operator `?=` [int (This)] (Core.Bool) is native `i16_eq`
	operator `!=` [int (This)] (Core.Bool) is native `i16_ne`
	operator `>` [int (This)] (Core.Bool) is native `i16_gt`
	operator `>=` [int (This)] (Core.Bool) is native `i16_ge`
	operator `<` [int (This)] (Core.Bool) is native `i16_lt`
	operator `<=` [int (This)] (Core.Bool) is native `i16_le`


	on [Core.Bool] is native `cast_i16_bool`
	on [Int8] is native `cast_i16_i8`
	on [UInt8] is native `cast_i16_u8`
	on [UInt16] is native `cast_i16_u16`
	on [Int32] is native `cast_i16_i32`
	on [UInt32] is native `cast_i16_u32`
	on [Int64] is native `cast_i16_i64`
	on [UInt64] is native `cast_i16_u64`
	on [Core.Dec] is native `cast_i16_d64`
	on [Core.Str] is native `cast_i16_str`
}