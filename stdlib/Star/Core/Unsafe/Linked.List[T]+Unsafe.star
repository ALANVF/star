type T
category Unsafe for Linked.List[T] {
	;== Removing values

	on [removeAt: index (Int)] (T) {
		my link = this[linkAt: index]
		-> prev = link.next
		
		length--

		return link.value
	}
}