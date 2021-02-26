protocol Printable {
	on [printString] (Str) {
		return this[printString: ""]
	}

	on [printString: prefix (Str)] (Str) is hidden
}

category Printable for Array[Printable] {
	on [printString] (Str) {
		return this[Printable printString: ""]
	}

	on [printString: prefix (Str)] (Str) is hidden {
		return this[collect: $0[printString: prefix]][joinWith: ", "]
	}
}