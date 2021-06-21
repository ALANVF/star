package typing;

import typing.Traits;

abstract class Kind extends Namespace {
	final parents: Array<Type> = [];
	final methods: Array<Method> = [];
	final operators: Array<Operator> = [];
	var deinit: Option<Deinit> = None;
	var isFlags: Bool = false;
	var isStrong: Bool = false;
	var isUncounted: Bool = false;
	var sealed: Option<Option<Type>> = None;

	static function fromAST(decl: ILookupType, ast: parsing.ast.decls.Kind): Kind {
		final cases = ast.body.of.filterMap(d -> switch d {
			case DCase(c): c;
			default: null;
		});

		return if(cases.length != 0 && cases.every(c -> c.kind.match(Scalar(_, _)))) {
			ValueKind.fromAST(decl, ast);
		} else {
			TaggedKind.fromAST(decl, ast);
		}
	}

	override function hasErrors() {
		return super.hasErrors() || methods.some(m -> m.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(method in methods) result = result.concat(method.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}

	inline function declName() {
		return "kind";
	}
}