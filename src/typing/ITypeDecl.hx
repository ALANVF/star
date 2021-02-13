package typing;

import text.Span;
import parsing.ast.Ident;

interface ITypeDecl extends ILookupType {
	final lookup: ILookupType;
	final span: Span;
	final name: Ident;
	var params: Option<Array<Type>>;
}