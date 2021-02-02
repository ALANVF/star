kind Alignment {
	has left
	has center
	has right
}

kind TextAttrs is flags {
	has [default] ;-- First case is always the default in order to avoid confusion (although it can be whatever you want)
	has [bold]
	has [italic]
	has [underlined]
	has [colored: (Str)]
	has [aligned: (Alignment)]
	has [font: (Str) size: (Int)]
}

module Main {
	on [main] {
		my attrs = TextAttrs[bold] | TextAttrs[colored: "green"]

		match attrs {
			at TextAttrs[colored: "red"] & _ => Core[say: "It's colored red"]
			at TextAttrs[aligned: Alignment.left] => Core[say: "It's only aligned to the left"]
			at TextAttrs[colored: my color] & my attrs' {
				Core[say: "It's not colored red, but actually \(color)"]
				Core[say: {
					if attrs' & TextAttrs[bold] {
						return "Bold"
					} else {
						return "Not bold"
					}
				}]
			}
		}

		;=> "It's not colored red, but actually green"
		;=> "Bold"
	}
}