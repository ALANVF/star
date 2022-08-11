package text;

//@:struct
@:publicFields
class Span {
	var source: Null<SourceFile>;
	final start: Pos;
	final end: Pos;

	function new(start, end, ?source) {
		this.source = source;
		this.start = start;
		this.end = end;
	}

	static function empty(?source) {
		return new Span(new Pos(0, 0), new Pos(0, 0), source);
	}

	static function at(pos, ?source) {
		return new Span(pos, pos.advance(), source);
	}

	static function length(start, length, ?source) {
		return new Span(start, start.advance(length), source);
	}

	static function range(from: Span, to: Span) {
		if(from.source != to.source) {
			throw "The two spans originate from different sources!";
		}
		
		return new Span(from.start, to.end, from.source);
	}

	function contains(pos) {
		return start <= pos && pos < end;
	}

	function intersects(other: Span) {
		return !(start >= other.end || other.start >= end);
	}

	function display() {
		return (source?.path ?? "(Unknown)")+':${start.line + 1}:${start.column}';
	}

	/*function toString() {
		return '{start: $start, end: $end}';
	}*/

	inline function union(other: Span) {
		return Span.range(this, other);
	}
}