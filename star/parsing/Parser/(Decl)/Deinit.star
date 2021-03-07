class Deinit of EmptyMethod {
	alias Attrs = EmptyMethod.Attrs

	on [displayName] (Str) is getter {
		match attrs at Attrs[isStatic: _] & _ {
			return "static deinitializer"
		} else {
			return "default deinitializer"
		}
	}
}