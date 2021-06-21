package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;
import Util.match;
import typing.Traits;

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

	function findType(path: List<String>, absolute = false, cache: List<{}> = Nil) {
		if(absolute) {
			if(cache.contains(this)) {
				return None;
			} else {
				cache = cache.prepend(this);
			}
		}

		return if(absolute) {
			match(path,
				at(["This"]) => Some(new Type(TThis(None, this))),
				at([typeName]) => switch generics.find(typeName) {
					case None: lookup.findType(path, true, cache);
					case Some([type]): Some(type.thisType);
					case Some(found): Some(new Type(TMulti(found.map(g -> g.thisType))));
				},
				_ => if(absolute) lookup.findType(path, true, cache) else None
			);
		} else {
			None;
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