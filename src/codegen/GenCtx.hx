package codegen;

@:build(util.Overload.build())
@:publicFields class GenCtx {
	var outer: Null<GenCtx>;
	var locals: UInt;
	var labels: UInt;
	var localsMap: Map<String, LocalID>;
	var labelsMap: Map<String, LabelID>;
	var loopStack: List<LabelID>;
	var thisStack: List2<LocalID, TypeRef>;

	function new(
		outer: Null<GenCtx> = null,
		locals: UInt = 0,
		labels: UInt = 0,
		localsMap: Null<Map<String, LocalID>> = null,
		labelsMap: Null<Map<String, LabelID>> = null,
		loopStack: List<LabelID> = Nil,
		thisStack: List2<LocalID, TypeRef> = Nil2
	) {
		this.outer = outer;
		this.locals = locals;
		this.labels = labels;
		this.localsMap = localsMap ?? new Map();
		this.labelsMap = labelsMap ?? new Map();
		this.loopStack = loopStack;
		this.thisStack = thisStack;
	}

	function inner() {
		return new GenCtx(this, locals, labels, localsMap.copy(), labelsMap.copy(), loopStack, thisStack);
	}

	function anon() {
		return locals++;
	}

	function local(name: String) {
		return localsMap[name].nonNull();
	}

	function newLocal(name: String) {
		localsMap[name] = locals;
		return locals++;
	}

	overload function label() {
		return labels++;
	}

	overload function label(label: String) {
		return labelsMap[label].nonNull();
	}

	function loop(label: Null<String>) {
		label._andOr(label => {
			final id = labels++;
			labelsMap[label] = id;
			loopStack = loopStack.prepend(id);
			return id;
		}, {
			final lbl = this.label();
			loopStack = loopStack.prepend(lbl);
			return lbl;
		});
	}

	function addThis(id: LocalID, typeref: TypeRef) {
		return new GenCtx(this, locals, labels, localsMap.copy(), labelsMap.copy(), loopStack, Cons2(id, typeref, thisStack));
	}

	overload function getThis() {
		return thisStack._match(
			at([]) => null,
			at([[id, typeref], ..._]) => tuple(id, typeref)
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
			at(Cons2(id, typeref, _)) => tuple(id, typeref)
		);
	}

	overload function getLoop() {
		return loopStack._match(
			at([]) => null,
			at([id, ..._]) => id
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
			at([id, ..._]) => id
		);
	}

	function popLoop(label: Null<String>) {
		label._andOr(label => {
			if(labelsMap[label] == loopStack.head()) {
				labels--;
			}

			labelsMap.remove(label);
		}, {
			labels--;
		});

		loopStack = loopStack.tail();
	}
}