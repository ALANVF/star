kind Type {
	has [leading: (Array[Span]) segs: (Array[Seg])]
	has [blank: (Span)]
	has [blank: (Span) params: (Params)]

	on [span] (Span) is getter {
		match this {
			at Type[leading: #[] segs: my segs] => return Span[from: segs[at: 0].span to: segs[at: -1].span]
			at Type[leading: my leading segs: my segs] => return Span[from: leading[at: 0] to: segs[at: -1].span]
			at Type[blank: my span] => return span
			at Type[blank: my from params: Delims[end: my to]] => return Span[:from :to]
		}
	}

	on [simpleName] (Str) is getter {
		match this {
			at Type[leading: my leading segs: my segs] => return ("_." * leading.length) + segs[collect: $0[simpleName]][joinWith: "."]
			at Type[blank: _] => return "_"
			at Type[blank: _ params: my params] => return "_[\("..., " * (params.length - 1))...]"
		}
	}
}