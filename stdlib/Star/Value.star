protocol Value {
	init [new]

	on [new] (This)

	operator `?` (Core.Bool) is native `value_truthy`
	
	operator `?=` [value (This)] (Core.Bool) is native `value_eq`
	
	operator `!=` [value (This)] (Core.Bool) => return !(this ?= value)

	on [Core.Str] is native `cast_value_str`
}

protocol MultiKind {
	operator `?` (Core.Bool) is native `multikind_truthy`

	operator `&` [other (This)] (Core.Bool) is native `multikind_has`
	operator `|` [other (This)] (This) is native `multikind_include`
	operator `^` [other (This)] (This) is native `multikind_exclude`
}