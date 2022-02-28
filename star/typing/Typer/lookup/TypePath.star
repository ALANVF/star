use #[
	Ident
	Delims
	;Type.Seg
] from: Parser
use Seg from: Parser.Type

alias TypePath (Parser.Type) {
	on [toLookupPath: lookup (TypeLookup)] (Tuple[Int, LookupPath]) {
		match this {
			at This[blank: _] || This[blank: _ args: _] => throw "error!"
			at This[leading: my leading segs: my segs] {
				return #{
					leading.length
					segs[TypePath mapSegs: lookup]
				}
			}
		}
	}

	on [toType: source (TypeLookup)] (Type) {
		match this {
			at This[blank: my span] {
				return Type[blank: span]
			}

			at This[blank: my span args: Delims[of: my args]] {
				return Type[
					span: Maybe[the: this.span]
					type: Type[blank: span]
					args: args[collect: Parser.Type$0[TypePath][toType: source]]
				]
			}

			at This[leading: my leading segs: my segs] {
				return Type[
					span: Maybe[the: this.span]
					depth: leading.length
					lookup: segs[TypePath mapSegs: source]
					:source
				]
			}
		}
	}
}

category TypePath for Array[Seg] {
	on [mapSegs: source (TypeLookup)] (LookupPath) {
		return this[LookupPath collect: {|seg (Seg)|
			match this {
				at Seg[name: Ident #{my span, my name}] {
					return #{span, name, #[]}
				}

				at Seg[name: Ident #{my span, my name} args: Delims[of: my args]] {
					return #{
						span
						name
						args[collect: source[makeTypePath: Parser.Type$.0[TypePath]]]
					}
				}
			}
		}]
	}
}