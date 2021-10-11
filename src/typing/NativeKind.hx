package typing;

@:using(typing.NativeKind)
enum NativeKind {
	NVoid;
	NBool;
	NInt8;
	NUInt8;
	NInt16;
	NUInt16;
	NInt32;
	NUInt32;
	NInt64;
	NUInt64;
	NDec32;
	NDec64;
	NVoidPtr;
	NPtr(t: Type);
}


function matches(self: NativeKind, other: NativeKind) return self._match(
	at(NPtr(t)) => other._match(
		at(NPtr(t2)) => t.hasParentType(t2) || t.hasChildType(t2),
		_ => false
	),
	_ => self == other
);