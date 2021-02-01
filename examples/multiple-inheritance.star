class A {
	on [method] (Int) {
		return 1
	}
}

class B {
	on [method] (Int) {
		return 2
	}
}

class C of A, B {}

module Main {
	on [main] {
		my c = C[new]

		Core
		-> [say: c[method]]   ;=> 2
		-> [say: c[A method]] ;=> 1
		-> [say: c[B method]] ;=> 2
	}
}
