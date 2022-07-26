class Float64 of Core.Ordered is native[repr: `float` bits: 64] is strong {
	;== Constants

	on [nan] (This) is static is getter is native `f64_nan`
	on [inf] (This) is static is getter is native `f64_inf`
	on [neg_inf] (This) is static is getter is native `f64_neg_inf`

	on [pi] (This) is static is getter is native `f64_pi`
	on [e] (This) is static is getter is native `f64_e`


	;== Math

	on [isNaN] (Core.Bool) is native `f64_is_nan`
	on [sign] (This) is native `f64_sign`
	on [abs] (This) is native `f64_abs`
	on [sqrt] (This) is native `f64_sqrt`
	on [exp] (This) is native `f64_exp`
	on [sin] (This) is native `f64_sin`
	on [cos] (This) is native `f64_cos`
	on [tan] (This) is native `f64_tan`
	on [asin] (This) is native `f64_asin`
	on [acos] (This) is native `f64_acos`
	on [atan] (This) is native `f64_atan`
	on [floor] (This) is native `f64_floor`
	on [ceiling] (This) is native `f64_ceil`
	on [truncate] (This) is native `f64_trunc`
	on [round] (This) is native `f64_round`
	on [ln] (This) is native `f64_ln`
	on [log] (This) is native `f64_log`
	
	
	;== Operators

	operator `?` (Core.Bool) is native `f64_truthy`
	operator `-` (This) is native `f64_neg`
	operator `++` (This) is native `f64_succ`
	operator `--` (This) is native `f64_pred`

	operator `+` [other (This)] (This) is native `f64_add`
	operator `-` [other (This)] (This) is native `f64_sub`
	operator `*` [other (This)] (This) is native `f64_mult`
	operator `**` [other (This)] (This) is native `f64_pow`
	operator `/` [other (This)] (This) is native `f64_div`
	operator `//` [other (This)] (Core.Int) is native `f64_idiv`
	operator `%` [other (This)] (This) is native `f64_mod`
	operator `%%` [other (This)] (Core.Bool) is native `f64_mod0`
	operator `?=` [other (This)] (Core.Bool) is native `f64_eq`
	operator `!=` [other (This)] (Core.Bool) is native `f64_ne`
	operator `>` [other (This)] (Core.Bool) is native `f64_gt`
	operator `>=` [other (This)] (Core.Bool) is native `f64_ge`
	operator `<` [other (This)] (Core.Bool) is native `f64_lt`
	operator `<=` [other (This)] (Core.Bool) is native `f64_le`



	;== Converting
	
	on [Int8] is native `cast_f64_i8`
	on [UInt8] is native `cast_f64_u8`
	on [Int16] is native `cast_f64_i16`
	on [UInt16] is native `cast_f64_u16`
	on [Int32] is native `cast_f64_i32`
	on [UInt32] is native `cast_f64_u32`
	on [Int64] is native `cast_f64_i64`
	on [UInt64] is native `cast_f64_u64`
	on [Float32] is native `cast_f64_f32`
	on [Core.Int] is native `cast_f64_i32`
	on [Core.Dec] is native `cast_f64_d64`
	on [Core.Str] is native `cast_f64_str`
}