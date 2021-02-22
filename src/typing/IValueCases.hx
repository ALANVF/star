package typing;

typedef IValueCases = ITypeDecl & {
	final valueCases: Array<ValueCase>;
}