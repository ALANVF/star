class Float32 of Core.Ordered is native[repr: `float` bits: 32] is strong {
	;== Constants

	on [nan] (This) is static is getter is native `f32_nan`
	on [inf] (This) is static is getter is native `f32_inf`
	on [neg_inf] (This) is static is getter is native `f32_neg_inf`
	
	on [pi] (This) is static is getter is native `f32_pi`
	on [e] (This) is static is getter is native `f32_e`


	;== Math

	on [isNaN] (Core.Bool) is native `f32_is_nan`
	on [sign] (This) is native `f32_sign`
	on [abs] (This) is native `f32_abs`
	on [sqrt] (This) is native `f32_sqrt`
	on [exp] (This) is native `f32_exp`
	on [sin] (This) is native `f32_sin`
	on [cos] (This) is native `f32_cos`
	on [tan] (This) is native `f32_tan`
	on [asin] (This) is native `f32_asin`
	on [acos] (This) is native `f32_acos`
	on [atan] (This) is native `f32_atan`
	on [floor] (This) is native `f32_floor`
	on [ceiling] (This) is native `f32_ceil`
	on [truncate] (This) is native `f32_trunc`
	on [round] (This) is native `f32_round`
	on [ln] (This) is native `f32_ln`
	on [log] (This) is native `f32_log`
	
	
	;== Operators

	operator `?` (Core.Bool) is native `f32_truthy`
	operator `-` (This) is native `f32_neg`
	operator `++` (This) is native `f32_succ`
	operator `--` (This) is native `f32_pred`

	operator `+` [other (This)] (This) is native `f32_add`
	operator `-` [other (This)] (This) is native `f32_sub`
	operator `*` [other (This)] (This) is native `f32_mult`
	operator `**` [other (This)] (This) is native `f32_pow`
	operator `/` [other (This)] (This) is native `f32_div`
	operator `//` [other (This)] (Core.Int) is native `f32_idiv`
	operator `%` [other (This)] (This) is native `f32_mod`
	operator `%%` [other (This)] (Core.Bool) is native `f32_mod0`
	operator `?=` [other (This)] (Core.Bool) is native `f32_eq`
	operator `!=` [other (This)] (Core.Bool) is native `f32_ne`
	operator `>` [other (This)] (Core.Bool) is native `f32_gt`
	operator `>=` [other (This)] (Core.Bool) is native `f32_ge`
	operator `<` [other (This)] (Core.Bool) is native `f32_lt`
	operator `<=` [other (This)] (Core.Bool) is native `f32_le`



	;== Converting
	
	on [Int8] is native `cast_f32_i8`
	on [UInt8] is native `cast_f32_u8`
	on [Int16] is native `cast_f32_i16`
	on [UInt16] is native `cast_f32_u16`
	on [Int32] is native `cast_f32_i32`
	on [UInt32] is native `cast_f32_u32`
	on [Int64] is native `cast_f32_i64`
	on [UInt64] is native `cast_f32_u64`
	on [Float64] is native `cast_f32_f64`
	on [Core.Int] is native `cast_f32_i32`
	on [Core.Dec] is native `cast_f32_d64`
	on [Core.Str] is native `cast_f32_str`
}