class MultiInit of Init {
	my typevars (TypeVars) is getter = TypeVars #()
	
	my params (MultiParams) = MultiParams #[]
	my fuzzyName (Str)
	my isUnordered (Bool) = false
}