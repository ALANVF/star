type T
protocol Collection[T] of Iterable[T] {
	;== Sizing
	
	on [length] (Int) is getter
	
	
	;== Removing values
	
	on [remove: value (T)] (Bool)
	on [remove: value (T) times: (Int)] (Int)
	on [removeAll: value (T)] (Int)
	
	on [removeValues: values (Iterable[T])] (Bool)
	on [removeValues: (Iterable[T]) times: (Int)] (This) => return this[This :removeValues :times]
	on [removeAllValues: (Iterable[T])] (This) => return this[This :removeAllValues]
	
	on [removeWhere: func (Func[Bool, T])] (Bool)
	on [removeWhere: (Func[Bool, T]) times: (Int)] (This) => return this[This :removeWhere :times]
	on [removeAllWhere: (Func[Bool, T])] (This) => return this[This :removeAllWhere]
}

type T
type C of Collection[T]
category C for Collection[T] {
	;== Removing values
	
	on [removeValues: values (Iterable[T])] (C)
	on [removeValues: values (Iterable[T]) times: (Int)] (C)
	on [removeAllValues: values (Iterable[T])] (C)
	
	on [removeWhere: func (Func[Bool, T]) times: (Int)] (C)
	on [removeAllWhere: func (Func[Bool, T])] (C)
}