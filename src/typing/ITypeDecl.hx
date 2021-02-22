package typing;

import parsing.ast.Ident;

typedef ITypeDecl = ILookupType & IDecl & {
	final lookup: ILookupType;
	final name: Ident;
	var params: Option<Array<Type>>;
}