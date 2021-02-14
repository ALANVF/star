module TaggedKind {
	kind A {
		has [a]
		has [b: (This)]
		has [c: (Int) d: (Int)]
	}

	kind B of A {
		has [e]
	}

	kind C {
		my a (Str)

		has [b: (Str)] {
			a = b
		}

		has [c] {
			a = "c"
		}
	}

	kind D is flags {
		has [a]
		has [b: (Int)]
	}
}