kind Seg {
	has [name: (Ident)]
	has [name: (Ident) params: (Params)]

	on [name] (Ident) is getter {
		match this {
			at Seg[name: my name] => return name
			at Seg[name: my name params: _] => return name
		}
	}

	on [params] (Maybe[Params]) is getter {
		match this {
			at Seg[name: _] => return Maybe[none]
			at Seg[name: _ params: my params] => return Maybe[the: params]
		}
	}

	on [span] (Span) is getter {
		match this {
			at Seg[name: Ident[span: my span]] => return span
			at Seg[name: Ident[span: my from] params: Delims[end: my to]] => return Span[:from :to]
		}
	}

	on [simpleName] (Str) is getter {
		match this {
			at Seg[name: my name] => return name[new]
			at Seg[name: my name params: my params] => return "\(name)[\("..., " * (params.length - 1))...]"
		}
	}
}