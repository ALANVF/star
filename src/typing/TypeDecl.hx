package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;

@:build(util.Auto.build({keepInit: true}))
@:autoBuild(util.Auto.build())
abstract class TypeDecl {
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType;
	@:ignore final generics = new MultiMap<String, Generic>();
	final span: Span;
	final name: Ident;
	var params: Option<Array<Type>>;
	var hidden: Option<Option<Type>> = None;
	final friends: Array<Type> = [];
	@:ignore var thisType: Type;

	function new() {
		thisType = new Type(TConcrete(this));
	}
	
	abstract function declName(): String;

	function findLocalType(typeName: String) {
		return switch generics.find(typeName) {
			case None: None;
			case Some([type]): Some(type.thisType);
			case Some(found): Some(new Type(TMulti(found.map(g -> g.thisType))));
		}
	}

	function findType(typeName: String) {
		if(typeName == "This") {
			return Some(new Type(TThis(None, this)));
		} else {
			return switch this.findLocalType(typeName) {
				case t = Some(_): t;
				case None: None;
			}
		}
	}

	function makeTypePath(path) {
		return new Type(TPath(path, this));
	}

	function hasErrors() {
		return errors.length != 0 || generics.allValues().some(g -> g.hasErrors());
	}

	function allErrors() {
		var result = errors;

		for(generic in generics) result = result.concat(generic.allErrors());

		return result;
	}
}