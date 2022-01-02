type T
class Box[T] is sealed {
	my value (T)

	init [new: value (T)] {
		_.value = value
	}
}