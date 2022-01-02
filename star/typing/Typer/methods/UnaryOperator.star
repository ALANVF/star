class UnaryOperator of Operator {
	kind Op (Str) {
		has incr => "++"
		has decr => "--"
		has neg => "-"
		has not => "!"
		has compl => "~"
		has truthy => "?"
	}

	
	my op (Op) is getter
}