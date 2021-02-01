protocol Num of Ordered ;[is strong] {
	on [sign] (This)
	on [abs] (This)
	on [rootOf: root (Num)] (This)
	on [exp] (This)
	on [sin] (This)
	on [cos] (This)
	on [tan] (This)
	on [asin] (This)
	on [acos] (This)
	on [atan] (This)
	on [floor] (This)
	on [ceiling] (This)
	on [round] (This)
	on [round: digits (Int)] (This)
	on [ln] (This)
	on [log] (This)
	on [log: base (Int)] (This)

	on [next] (This)
	on [previous] (This)

	on [min: other (This)] (This) {
		return [this < other yes: this no: other]
	}

	on [max: other (This)] (This) {
		return [this > other yes: this no: other]
	}

	on [min: (This) max: (This)] (This) {
		return [this < min yes: min no: [this > max yes: max no: this]]
	}

	; gcd
	; lcm


	operator `-` (This)

	operator `+` [other (This)] (This)
	operator `-` [other (This)] (This)
	operator `*` [other (This)] (This)
	operator `**` [other (This)] (This)
	operator `/` [other (This)] (This)
	operator `//` [other (This)] (Int)
	operator `%` [other (This)] (This)
	operator `%%` [other (This)] (Bool)
	operator `?=` [other (This)] (Bool)
	operator `!=` [other (This)] (Bool)
	operator `>` [other (This)] (Bool)
	operator `>=` [other (This)] (Bool)
	operator `<` [other (This)] (Bool)
	operator `<=` [other (This)] (Bool)
}