alias TypePath (Array[Tuple[Str, Maybe[Array[Type]]]]) {
	on [named: name (Str)] (This) is static is inline {
		return This #[#{name, Maybe[none]}]
	}
	
	on [named: name (Str) of: args (Array[Type])] (This) is static is inline {
		return This #[#{name, Maybe[the: args]}]
	}
	
	on [form] (Str) {
		return this[collect: {
			match $.0 {
				at #{my name, Maybe[none]} => return "\(name)"
				at #{my name, Maybe[the: my args]} {
					return "\(name)<\(args[collect: $0[form]][joinWith: ", "])>"
				}
			}
		}][joinWith: "::"]
	}
}