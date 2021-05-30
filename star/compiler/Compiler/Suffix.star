kind Suffix {
	has incr => "++"
	has decr => "--"
	has spread => "..."
	
	on [form] (Str) {
		return this[Str]
	}
}