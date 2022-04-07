package typing;

import typing.Traits;

abstract class Kind extends ClassLike {
	var deinit: Option<Deinit> = None;
	var _isFlags: Bool = false;
	var _isStrong: Bool = false;
	var _isUncounted: Bool = false;

	static function fromAST(decl: ITypeLookup, ast: parsing.ast.decls.Kind): Kind {
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
		return super.hasErrors()
			|| methods.some(m -> m.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(method in methods) result = result.concat(method.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}

	function declName() {
		return "kind";
	}


	// Attributes

	override function isFlags() {
		return _isFlags || super.isFlags();
	}

	override function isStrong() {
		return _isStrong || super.isStrong();
	}

	override function isUncounted() {
		return _isUncounted || super.isUncounted();
	}


	// Method lookup

	// TODO: add multi-kind stuff
	override function defaultUnaryOp(ctx: Ctx, op: UnaryOp, from: AnyTypeDecl): Null<UnaryOpKind> {
		return Pass2.STD_Value.findUnaryOp(ctx, op, from);
	}


	// TODO: add multi-kind stuff
	override function defaultBinaryOp(ctx: Ctx, op: BinaryOp, from: Type): Array<BinaryOpKind> {
		return Pass2.STD_Value.findBinaryOp(ctx, op, from);
	}
}