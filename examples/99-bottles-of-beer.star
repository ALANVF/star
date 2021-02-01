module Main {
	on [main] {
		for my i from: 99 downto: 0 {
			Core[say: "\(i) bottle(s) of beer on the wall, \(i) bottle(s) of beer."]
			my n = [i ?= 0 yes: "no more" no: "\(i - 1)"]
			Core[say: "Take one down, pass it around", "\(n) bottle(s) of beer."]
		}
	}
}