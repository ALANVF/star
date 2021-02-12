package typing;

import text.Span;

interface ILookupType {
	//function lookupType(span: Option<Span>, name: String, params: Option<Array<Type>>): Option<Type>;
	/*function addType(span: Option<Span>, name: String, params: Option<Array<Type>>, type: Type): Type;
	function lookupOrAddType(span: Option<Span>, name: String, params: Option<Array<Type>>): Type;

	function lookupTypePath(path: TypePath): Option<Type>;
	function addTypePath(path: TypePath, type: Type): Type;
	function lookupOrAddTypePath(path: TypePath): Type;*/

	function lookupTypePath(path: TypePath, isBase: Bool = false): Option<Type>;
}