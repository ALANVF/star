package typing;

interface IMethod extends IAnyMethod {
	var ret: Option<Type>;
	var isMain: Bool;
	var isGetter: Bool;
	var isSetter: Bool;
	var isUnordered: Bool;
	var isInline: Bool;
}