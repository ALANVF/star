class Point {
	my x (Int)
	my y (Int)

	init [new] {
		x = 0
		y = 0
	}

	operator `-` (Point) {
		return Point[x: -x y: -y]
	}

	operator `?=` [other (Point)] (Bool) {
		return x ?= other.x && y ?= other.y
	}

	on [Str] {
		return "(\(x), \(y))"
	}
}