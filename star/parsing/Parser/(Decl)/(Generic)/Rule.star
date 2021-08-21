kind Cmp {
	has lt
	has le
	has gt
	has ge
}

kind Rule {
	has [not: (Span) type: (Type)]
	has [type: (Type) exists: (Span)]
	
	has [left: (Type) eq: chain (Array[Tuple[Span, Type]])]
	has [left: (Type) ne: chain (Array[Tuple[Span, Type]])]
	has [left: (Type) of: chain (Array[Tuple[Span, Type]])]
	
	has [left: (Type) cmp: chain (Array[Tuple[Span, Cmp, Type]])]
	
	has [left: (Rule) and: (Span) right: (Rule)]
	has [left: (Rule) or: (Span) right: (Rule)]
	has [left: (Rule) xor: (Span) right: (Rule)]
	has [left: (Rule) nor: (Span) right: (Rule)]
	
	has [not: (Span) cond: (Rule)]
	has [begin: (Span) paren: (Rule) end: (Span)]
}