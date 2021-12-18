protocol HasErrors {
	my errors (Array[Diagnostic]) is getter = #[]
	
	on [hasErrors] (Bool) => return errors?
	on [allErrors] (Array[Diagnostic]) => return errors[new]
}