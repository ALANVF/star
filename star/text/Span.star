class Span {
	my source (Maybe[SourceFile])
	my begin (Pos)
	my end (Pos)

	init [new] {
		source = Maybe[none]
		begin = Pos[new]
		end = Pos[new]
	}
	
	init [new: source (SourceFile)] {
		this.source = Maybe[the: source]
		begin = Pos[new]
		end = Pos[new]
	}

	init [begin: (Pos) end: (Pos)] {
		source = Maybe[none]
		this.begin = begin
		this.end = end
	}
	
	init [begin: (Pos) end: (Pos) source: (SourceFile)] {
		this.source = Maybe[the: source]
		this.begin = begin
		this.end = end
	}

	init [at: pos (Pos)] {
		source = Maybe[none]
		begin = pos
		end = pos[advance]
	}
	
	init [at: pos (Pos) source: (SourceFile)] {
		this.source = Maybe[the: source]
		begin = pos
		end = pos[advance]
	}

	init [begin: (Pos) length: (Int)] {
		source = Maybe[none]
		this.begin = begin
		end = begin[advance: length]
	}
	
	init [begin: (Pos) length: (Int) source: (SourceFile)] {
		this.source = Maybe[the: source]
		this.begin = begin
		end = begin[advance: length]
	}

	init [from: (Span) to: (Span)] {
		if from.source != to.source => throw "The two spans originate from different sources!"

		source = from.source
		begin = from.begin
		end = to.end
	}

	on [contains: pos (Pos)] (Bool) {
		return begin <= pos < end
	}

	on [intersects: other (Span)] (Bool) {
		return begin >= other.end !! other.begin >= end
	}
	
	operator `|` [other (Span)] (Span) is inline {
		return Span[from: this to: other]
	}
	
	on [isMultiline] (Bool) {
		return begin.line != end.line
	}
	
	on [first] (Span) {
		return Span[at: begin :source]
	}
	
	on [last] (Span) {
		return Span[at: end :source]
	}
	
	on [next] (Span) {
		return this[new]
		-> begin++
		-> end++
	}
	
	on [previous] (Span) {
		return this[new]
		-> begin--
		-> end--
	}
}