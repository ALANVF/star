kind Attrs (Str) is flags {
	has none
	has friend => "friend"
	has inline => "inline"
	has virtual => "virtual"
	has constexpr => "constexpr"
	has consteval => "consteval"
	has static => "static"
	has implicit => "implicit"
	has explicit => "explicit"
	has mutable => "mutable"
	has extern => "extern"
	
	on [form] (Str) {
		return this[Array[Str]][joinWith: " "]
	}
	
	on [formLeading] (Str) {
		return this[Array[Str]][collect: "\(Str$.0) "][join]
	}
	
	on [formTrailing] (Str) {
		return this[Array[Str]][collect: " \(Str$.0)"][join]
	}
}