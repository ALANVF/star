class Point {
	my x = 0
	my y = 0

	on [Str] {
		return "Point x: \(x), y: \(y)"
	}
}

class Circle of Point {
	my r = 0

	on [Str] {
		return "Circle x: \(x), y: \(y), radius: \(r)"
	}
}

module Main {
	on [main] {
		my p = Point[new]
		my c = Circle[new]

		Core[say: p] ;=> "Point x: 0, y: 0"
		Core[say: c] ;=> "Circle x: 0, y: 0, radius: 0"
	}
}
