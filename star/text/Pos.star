class Pos of Comparable {
	; these should probably be immutable huh
	my line = 0
	my column = 0

	; `<=>` operator doesn't exist yet
	on [compare: other (Pos)] (Int) {
		match line - other.line {
			at 0       => return column - other.column
			at my diff => return diff
		}
	}

	operator `>` [other (Pos)] (Bool) {
		return this[compare: other] > 0
	}

	operator `<` [other (Pos)] (Bool) {
		return this[compare: other] > 0
	}
	
	operator `++` (Pos) {
		return this[advance]
	}
	
	operator `--` (Pos) {
		return this[advance: -1]
	}

	on [advance: amount (Int) = 1] (Pos) {
		return Pos[:line column: column + amount]
	}

	on [newline] (Pos) {
		return Pos[line: line + 1]
	}

	on [Str] {
		return "line \(line + 1), column \(column + 1)"
	}
}