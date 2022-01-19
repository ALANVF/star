type T
kind Set[T] of Iterable[T] {
	; ...

	on [new] (This) is static

	on [contains: value (T)] (Bool)
	
	on [containsAny: set (This)] (Bool)
	on [containsAny: values (Iterable[T])] (Bool)

	on [add: value (T)] (This)

	on [remove: value (T)] (This)

	; ...

	operator `+` [value (T)] (This)

	operator `|` [other (This)] (This)

	; ...

	on [Iterator[T]]

	; ...
}