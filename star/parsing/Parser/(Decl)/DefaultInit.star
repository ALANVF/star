class DefaultInit of EmptyMethod {
	alias Attrs = EmptyMethod.Attrs
	
	on [displayName] (Str) is getter {
		match attrs at Attrs[isStatic: _] & _ {
			return "static initializer"
		} else {
			return "default initializer"
		}
	}
}