package typing;

import parsing.ast.Ident;

interface ITypeDecl extends ILookupType extends IDecl {
	final lookup: ILookupType;
	final name: Ident;
	var params: Option<Array<Type>>;
}