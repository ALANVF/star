module Main {
	on [fact: n (Int)] (Int) {
		if n ?= 0 {
			return 1
		} else {
			return n * Main[fact: n - 1]
		}
	}
	
	on [main] {
		Core[say: Main[fact: 5]] ;=> 120
	}
}
