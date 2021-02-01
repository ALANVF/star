module Main {
	on [main] {
		;-- while loop
		my i = 0
		while i < 5 {
			Core[say: i]
			i++
		}

		;-- do-while loop
		do {
			Core[say: i]
			i--
		} while i != 0

		;-- for loop
		for my i in: #[1, 2, 3] {
			Core[say: i]
		}

		for my i from: 1 to: 3 {
			Core[say: i]
		} ;=> 1, 2, 3

		for my i after: 1 to: 3 {
			Core[say: i]
		} ;=> 2, 3

		for my i from: 1 upto: 3 {
			Core[say: i]
		} ;=> 1, 2

		for my i after: 1 upto: 3 {
			Core[say: i]
		} ;=> 2

		for my i from: 3 downto: 1 {
			Core[say: i]
		} ;=> 3, 2, 1

		for my i after: 3 downto: 1 {
			Core[say: i]
		} ;=> 2, 1

		for my k, my v in: #("a" => 1, "b" => 2, "c" => 3) {
			Core[say: "\(k): \(v)"]
		}
	}
}