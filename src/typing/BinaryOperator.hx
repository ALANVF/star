package typing;

import reporting.Diagnostic;
import parsing.ast.Ident;

enum abstract BinaryOp(Int) {
	var Plus;
	var Minus;
	var Times;
	var Pow;
	var Div;
	var IntDiv;
	var Mod;
	var IsMod;
	var BitAnd;
	var BitOr;
	var BitXor;
	var Shl;
	var Shr;
	var Eq;
	var Ne;
	var Gt;
	var Ge;
	var Lt;
	var Le;

	static final SYMBOLS = ["+", "-", "*", "**", "/", "//", "%", "%%", "&", "|", "^", "<<", ">>", "?=", "!=", ">", ">=", "<", "<="];

	public inline function symbol() {
		return SYMBOLS[this];
	}
}

class BinaryOperator extends Operator {
	final generics: Array<Generic>;
	final op: BinaryOp;
	final paramName: Ident;
	final paramType: Type;

	static function fromAST(decl, op, paramName, paramType, ast: parsing.ast.decls.Operator) {
		return new BinaryOperator({
			decl: decl,
			generics: ast.generics.mapArray(Generic.fromAST.bind(decl, _)),
			span: ast.span,
			op: op,
			opSpan: ast.symbolSpan,
			paramName: paramName,
			paramType: decl.makeTypePath(paramType),
			ret: ast.ret.map(ret -> decl.makeTypePath(ret)),
			body: ast.body.map(body -> body.stmts)
		});
	}

	override function hasErrors() {
		return super.hasErrors() || generics.some(g -> g.hasErrors());
	}

	override function allErrors() {
		return super.allErrors().concat(generics.flatMap(g -> g.allErrors()));
	}

	inline function opName() {
		return op.symbol();
	}
}