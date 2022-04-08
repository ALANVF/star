package codegen;

@:build(util.Overload.build())
@:publicFields class GenCtx {
	var outer: Null<GenCtx>;
	var anons: UInt;
	var labels: UInt;
	var loopStack: List<String>;
	var thisStack: List2<String, TypeRef>;

	function new(outer: Null<GenCtx> = null, anons: UInt = 0, labels: UInt = 0, loopStack: List<String> = Nil, thisStack: List2<String, TypeRef> = Nil2) {
		this.outer = outer;
		this.anons = anons;
		this.labels = labels;
		this.loopStack = loopStack;
		this.thisStack = thisStack;
	}

	function inner() {
		return new GenCtx(this, anons, labels, loopStack, thisStack);
	}

	function anon() {
		return '`${anons++}';
	}

	function label() {
		return '`${labels++}';
	}

	function loop(label: Null<String>) {
		label._andOr(label => {
			loopStack = loopStack.prepend(label);
			return label;
		}, {
			final lbl = this.label();
			loopStack = loopStack.prepend(lbl);
			return lbl;
		});
	}

	function addThis(name: String, typeref: TypeRef) {
		return new GenCtx(this, anons, labels, loopStack, Cons2(name, typeref, thisStack));
	}

	overload function getThis() {
		return thisStack._match(
			at([]) => null,
			at([[name, typeref], ..._]) => tuple(name, typeref)
		);
	}

	overload function getThis(depth: UInt) {
		var stack = thisStack;
		for(_ in 0...depth) stack._match(
			at(Nil2) => return null, // error...?
			at(Cons2(_, _, rest)) => {
				stack = rest;
			}
		);
		return stack._match(
			at(Nil2) => null,
			at(Cons2(name, typeref, _)) => tuple(name, typeref)
		);
	}

	overload function getLoop() {
		return loopStack._match(
			at([]) => null,
			at([label, ..._]) => label
		);
	}

	overload function getLoop(depth: UInt) {
		if(depth == 0) throw "bad";
		depth--;
		
		var stack = loopStack;
		for(_ in 0...depth) stack._match(
			at([]) => return null, // error...?
			at([_, ...rest]) => {
				stack = rest;
			}
		);
		return stack._match(
			at([]) => null,
			at([label, ..._]) => label
		);
	}

	function popLoop(label: Null<String>) {
		loopStack = loopStack.tail();
		if(label == null) {
			labels--;
		}
	}
}