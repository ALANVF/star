class Span {
	my source (Maybe[SourceFile])
	my start (Pos)
	my end (Pos)

	init [new] {
		source = Maybe[none]
		start = Pos[new]
		end = Pos[new]
	}
	
	init [new: source (SourceFile)] {
		this.source = Maybe[the: source]
		start = Pos[new]
		end = Pos[new]
	}

	init [start: (Pos) end: (Pos)] {
		source = Maybe[none]
		this.start = start
		this.end = end
	}
	
	init [start: (Pos) end: (Pos) source: (SourceFile)] {
		this.source = Maybe[the: source]
		this.start = start
		this.end = end
	}

	init [at: pos (Pos)] {
		source = Maybe[none]
		start = pos
		end = pos[advance]
	}
	
	init [at: pos (Pos) source: (SourceFile)] {
		this.source = Maybe[the: source]
		start = pos
		end = pos[advance]
	}

	init [start: (Pos) length: (Int)] {
		source = Maybe[none]
		this.start = start
		end = start[advance: length]
	}
	
	init [start: (Pos) length: (Int) source: (SourceFile)] {
		this.source = Maybe[the: source]
		this.start = start
		end = start[advance: length]
	}

	init [from: (Span) to: (Span)] {
		if from.source != to.source {
			throw "The two spans originate from different sources!"
		}

		source = from.source
		start = from.start
		end = to.end
	}

	on [contains: pos (Pos)] (Bool) {
		return start <= pos < end
	}

	on [intersects: other (Span)] (Bool) {
		return start >= other.end !! other.start >= end
	}
}