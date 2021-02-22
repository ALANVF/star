package typing;

typedef ITaggedCases = ITypeDecl & {
	final taggedCases: Array<TaggedCase>;
}