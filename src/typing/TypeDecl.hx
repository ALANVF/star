package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;

@:build(util.Auto.build({keepInit: true}))
@:autoBuild(util.Auto.build())
abstract class TypeDecl {
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType;
	final generics: Array<Generic>;
	@:ignore final genericTypes: Array<Type>;
	final span: Span;
	final name: Ident;
	var params: Option<Array<Type>>;
	var hidden: Option<Option<Type>> = None;
	final friends: Array<Type> = [];
	@:ignore var thisType: Type;

	function new() {
		genericTypes = generics.map(g -> new Type(TGeneric(g)));
		thisType = new Type(TConcrete(this));
	}
	
	abstract function declName(): String;

	function findLocalType(typeName: String) {
		final found = [for(i => generic in generics) {
			if(generic.name.name == typeName) {
				genericTypes[i];
			}
		}];

		return switch found {
			case []: None;
			case [type]: Some(type);
			default: Some(new Type(TMulti(found)));
		}
	}

	function findType(typeName: String) {
		/*if(typeName == name.name) {
			return Some(thisType);
		} else*/ if(typeName == "This") {
			return Some(new Type(TThis(None, this)));
		} else {
			return this.findLocalType(typeName);
		}
	}

	function makeTypePath(path) {
		return new Type(TPath(path, this));
	}

	function hasErrors() {
		return errors.length != 0 || generics.some(g -> g.hasErrors());
	}

	function allErrors() {
		var result = errors;

		for(generic in generics) result = result.concat(generic.allErrors());

		return result;
	}
}