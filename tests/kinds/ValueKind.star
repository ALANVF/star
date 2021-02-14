module ValueKind {
	kind A {
		has a
		has b
		has c
	}

	kind B {
		has a => 0
		has b => 1
		has c => 2
	}

	kind C (Int) {
		has a
		has b
		has c

		on [Str] {
			match this {
				at C.a => return "a"
				at C.b => return "b"
				at C.c => return "c"
			}
		}
	}

	kind D (Int) {
		has a => 0
		has b => 1
		has c => 2
	}

	kind Parent {
		has a
	}

	kind Child of Parent {
		has b
	}
}