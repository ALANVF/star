package typing;

abstract class Method extends AnyMethod implements IMethod {
	var ret: Option<Type>;
	var isMain: Bool = false;
	var isGetter: Bool = false;
	var isSetter: Bool = false;
	var isUnordered: Bool = false;
	var isInline: Bool = false;
}