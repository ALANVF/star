kind Info {
	has [span: (Span) message: (Maybe[Str]) priority: (Priority)]
	has [message: (Str)]
	
	on [span: (Span) message: (Str) priority: (Priority)] (This) is static is inline {
		return This[:span message: Maybe[the: message] :priority]
	}
	
	on [span: (Span) priority: (Priority)] (This) is static is inline {
		return This[:span message: Maybe[none] :priority]
	}
}