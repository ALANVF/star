protocol HasErrors {
	my errors (Array[Error]) is getter = #[]
	
	on [hasErrors] (Bool) => return errors?
	on [allErrors] (Array[Error]) => return errors[new]
}