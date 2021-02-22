package typing;

import text.Span;

typedef ILookupType = {
	//function lookupTypePath(lookup: ILookupType, path: TypePath, isBase: Bool = false): Option<Type>;
	
	function makeTypePath(path: TypePath): Type;
}