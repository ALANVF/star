module Main {
	on [main] {
		for my i from: 1 to: 100 {
			case {
				at i %% 15 => Core[say: "FizzBuzz"]
				at i %% 3  => Core[say: "Fizz"]
				at i %% 5  => Core[say: "Buzz"]
				else       => Core[say: i]
			}
		}
	}
}
