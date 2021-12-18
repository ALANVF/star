module Error {
	on [invalidTypePath: path (TypePath)] (Diagnostic)

	on [invalidTypeLookup: type (Type)] (Diagnostic)

	on [invalidTypeApply: span (Span) because: reason (Str)] (Diagnostic)
}