class Diagnostic {
	my severity (Severity)
	my code (Maybe[Str])
	my message (Maybe[Str])
	my info (Array[Info])
	
	
	init [severity: (Severity) code: (Str) message: (Str) info: (Array[Info])] {
		this.severity = severity
		this.code = Maybe[the: code]
		this.message = Maybe[the: message]
		this.info = info
	}
	
	init [severity: (Severity) code: (Str) info: (Array[Info])] {
		this.severity = severity
		this.code = Maybe[the: code]
		this.message = Maybe[none]
		this.info = info
	}
	
	init [severity: (Severity) message: (Str) info: (Array[Info])] {
		this.severity = severity
		this.code = Maybe[none]
		this.message = Maybe[the: message]
		this.info = info
	}
	
	init [severity: (Severity) info: (Array[Info])] {
		this.severity = severity
		this.code = Maybe[none]
		this.message = Maybe[none]
		this.info = info
	}
}