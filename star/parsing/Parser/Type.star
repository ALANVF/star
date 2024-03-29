kind Type {
	has [leading: (Array[Span]) segs: (Array[Seg])]
	has [blank: (Span)]
	has [blank: (Span) args: (Args)]

	on [span] (Span) is getter {
		match this {
			at Type[leading: #[] segs: my segs] => return Span[from: segs[at: 0].span to: segs[at: -1].span]
			at Type[leading: my leading segs: my segs] => return Span[from: leading[at: 0] to: segs[at: -1].span]
			at Type[blank: my span] => return span
			at Type[blank: my from args: Delims[end: my to]] => return Span[:from :to]
		}
	}

	on [simpleName] (Str) is getter {
		match this {
			at Type[leading: my leading segs: my segs] => return ("_." * leading.length) + segs[collect: Seg$0[simpleName]][joinWith: "."]
			at Type[blank: _] => return "_"
			at Type[blank: _ args: my args] => return "_[\("..., " * (args.of.length - 1))...]"
		}
	}

	on [depth] (Int) is getter {
		match this {
			at Type[leading: my leading segs: _] => return leading.length
			else => return 0
		}
	}
}