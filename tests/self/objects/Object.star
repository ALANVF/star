class Object of Printable {
	my slots (Array[Slot])
	my exprs (Array[Expr]) = #[]

	on [clone] (Object) {
		return Object[
			slots: slots[Slot cloneSlots]
			:exprs
		]
	}

	on [addSlot: newSlot (Slot)] {
		slots[add: newSlot]
	}

	on [addOrReplaceSlot: newSlot (Slot)] {
		match slots[maybeFindIndex: $0.name ?= newSlot.name] at Maybe[the: my index] {
			slots[at: index] = newSlot
		} else {
			slots[add: newSlot]
		}
	}

	on [getSlot: name (Str)] (Maybe[Slot]) {
		return slots[maybeFind: $0.name ?= name]
	}

	on [hasSlot: name (Str)] (Bool) {
		return slots[any: $0.name ?= name]
	}

	on [numArgSlots] (Int) {
		return slots[countIf: $0.isArg]
	}

	;on [nlrClosure: ifError (Func[Void])] (Func[Void, Object, Func[Void, Str]])
}