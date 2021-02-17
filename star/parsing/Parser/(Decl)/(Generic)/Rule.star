kind Rule {
	has [left: (Type) eq: (Span) right: (Type)]
	has [left: (Type) ne: (Span) right: (Type)]
	has [left: (Type) of: (Span) right: (Type)]
	has [left: (Rule) and: (Span) right: (Rule)]
	has [left: (Rule) or: (Span) right: (Rule)]
	has [left: (Rule) xor: (Span) right: (Rule)]
	has [left: (Rule) nor: (Span) right: (Rule)]
	has [not: (Span) right: (Rule)]
	has [begin: (Span) paren: (Rule) end: (Span)]
}