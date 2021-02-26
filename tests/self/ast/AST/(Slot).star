protocol Slot {
	my name (Str)
	
	on [isParent] (Bool) is getter {
		return false
	}

	on [isArg] (Bool) is getter {
		return false
	}

	on [isMutable] (Bool) is getter {
		return false
	}
}