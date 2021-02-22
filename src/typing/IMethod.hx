package typing;

typedef IMethod = IAnyMethod & {
	var ret: Option<Type>;
	var isMain: Bool;
	var isGetter: Bool;
	var isSetter: Bool;
	var isInline: Bool;
}