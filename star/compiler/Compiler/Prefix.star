kind Prefix {
	has [pos]
	has [neg]
	has [compl]
	has [not]
	has [addr]
	has [deref]
	has [incr]
	has [decr]
	has [scope]
	has [userDefined: sym (Str)]
	
	on [form] (Str) {
		match this {
			at This[pos] => return "+"
			at This[neg] => return "-"
			at This[compl] => return "~"
			at This[not] => return "!"
			at This[addr] => return "&"
			at This[deref] => return "*"
			at This[incr] => return "++"
			at This[decr] => return "--"
			at This[scope] => return "::"
			at This[userDefined: my sym] => "\(sym)"
		}
	}
}