alias LookupSeg = Tuple[
	Maybe[Span]
	Str
	Array[Type]
]

alias LookupPath (Array[LookupSeg]) {
	on [simpleName] (Str) is getter {
		return this[collect: {|seg|
			match seg {
				at #{_, my name, #[]} => return name
				at #{_, my name, my args} {
					return ""
					-> [add: name]
					-> [add: "["]
					-> [add: args[collect: $0.simpleName][joinWith: ", "]]
					-> [add: "]"]
				}
			}
		}][joinWith: "."]
	}

	on [span] (Span) is getter {
		for #{my span, _, _} in: this {
			match span at Maybe[the: my span'] {
				return span'
			}
		}

		throw "Cannot get the span of type `\(this.simpleName)`!"
	}
}