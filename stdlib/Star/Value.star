protocol Value {
	init [new]

	on [new] (This)

	operator `?` (Core.Bool) is native `value_truthy`
	
	operator `?=` [value (This)] (Core.Bool) is native `value_eq`
	
	operator `!=` [value (This)] (Core.Bool) {
		return !(this ?= value)
	}

	on [Core.Str] is native `cast_value_str`
}