protocol Decl of HasErrors {
	my span (Span) is getter

	on [declName] (Str) is getter
}