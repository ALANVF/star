protocol Slot of Printable {
	my name (Str) is getter
	my isParent (Bool) = false

	on [isArg] (Bool) is getter {
		return false
	}

	on [contents] (Object) is getter
	on [contents: (Object)] is setter

	on [clone] (This)
}

category Slot for Array[Slot] {
	on [cloneSlots] (This) {
		return this[collect: $0[clone]]
	}
}