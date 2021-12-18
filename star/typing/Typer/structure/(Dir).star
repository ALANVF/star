protocol Dir of TypeLookup {
	my name (Str) is getter
	my path (Str) is getter
	my units (Array[Unit]) = #[]
	my files (Array[File]) = #[]
}