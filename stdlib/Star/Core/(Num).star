protocol Num of Ordered {
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

	; gcd
	; lcm

	on [to: (This)] (Range[This]) is inline {
		return Range[This][from: this :to]
	}

	on [to: (This) by: (This)] (Range[This]) is inline {
		return Range[This][from: this :to :by]
	}

	on [upto: (This)] (Range[This]) is inline {
		return Range[This][from: this :upto]
	}

	on [upto: (This) by: (This)] (Range[This]) is inline {
		return Range[This][from: this :upto :by]
	}

	on [downto: (This)] (Range[This]) is inline {
		return Range[This][from: this :downto]
	}

	on [downto: (This) by: (This)] (Range[This]) is inline {
		return Range[This][from: this :downto :by]
	}


	operator `?` (Bool)
	operator `-` (This)

	operator `+` [other (This)] (This),  operator `+` [other (Num)] (Num)
	operator `-` [other (This)] (This),  operator `-` [other (Num)] (Num)
	operator `*` [other (This)] (This),  operator `*` [other (Num)] (Num)
	operator `**` [other (This)] (This), operator `**` [other (Num)] (Num)
	operator `/` [other (Num)] (Num)
	operator `//` [other (Num)] (Int)
	operator `%` [other (This)] (This),  operator `%` [other (Num)] (Num)
	operator `%%` [other (Num)] (Bool)

	operator `?=` [int (Num)] (Bool)
	operator `!=` [int (Num)] (Bool)
	operator `>` [int (Num)] (Bool)
	operator `>=` [int (Num)] (Bool)
	operator `<` [int (Num)] (Bool)
	operator `<=` [int (Num)] (Bool)

	on [Int]
	on [Dec]
}