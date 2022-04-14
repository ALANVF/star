kind Seg {
	has [name: (Ident)]
	has [name: (Ident) args: (Args)]

	on [name] (Ident) is getter {
		match this {
			at Seg[name: my name] => return name
			at Seg[name: my name args: _] => return name
		}
	}

	on [args] (Maybe[Args]) is getter {
		match this {
			at Seg[name: _] => return Maybe[none]
			at Seg[name: _ args: my args] => return Maybe[the: args]
		}
	}

	on [span] (Span) is getter {
		match this {
			at Seg[name: Ident[span: my span]] => return span
			at Seg[name: Ident[span: my from] args: Delims[end: my to]] => return Span[:from :to]
		}
	}

	on [simpleName] (Str) is getter {
		match this {
			at Seg[name: Ident[name: my name]] => return name[new]
			at Seg[name: Ident[name: my name] args: my args] => return "\(name)[\("..., " * (args.of.length - 1))...]"
		}
	}
}