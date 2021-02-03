protocol Num of Ordered ;[is strong] {
	on [sign] (This)
	on [abs] (This)
	on [sqrt] (Num)
	on [rootOf: root (Num)] (Num)
	on [exp] (Num)
	on [sin] (Num)
	on [cos] (Num)
	on [tan] (Num)
	on [asin] (Num)
	on [acos] (Num)
	on [atan] (Num)
	on [floor] (Num)
	on [ceiling] (Num)
	on [round] (Num)
	on [round: digits (Int)] (Num)
	on [truncate] (Num)
	on [ln] (Num)
	on [log] (Num)
	on [log: base (Int)] (Num)

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


	operator `?` (Bool)
	operator `-` (This)

	operator `+` [other (This)] (This)
	operator `-` [other (This)] (This)
	operator `*` [other (This)] (This)
	operator `**` [other (This)] (This)
	operator `/` [other (This)] (Num)
	operator `//` [other (This)] (Int)
	operator `%` [other (This)] (This)
	operator `%%` [other (This)] (Bool)
}