type T
kind Set[T] {
	; ...

	on [new] (This) is static

	on [contains: value (T)] (Bool)

	on [add: value (T)] (This)

	on [remove: value (T)] (This)

	; ...

	operator `+` [other (This)] (This)

	; ...
}