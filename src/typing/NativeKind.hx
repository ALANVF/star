package typing;

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