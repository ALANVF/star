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
	var And;
	var Or;
	var Xor;
	var Nor;

	static final SYMBOLS = ["+", "-", "*", "**", "/", "//", "%", "%%", "&", "|", "^", "<<", ">>", "?=", "!=", ">", ">=", "<", "<=", "&&", "||", "^^", "!!"];

	public inline function symbol() {
		return SYMBOLS[this];
	}
}

class BinaryOperator extends Operator {
	@:ignore final generics = new MultiMap<String, Generic>();
	final op: BinaryOp;
	final paramName: Ident;
	final paramType: Type;

	static function fromAST(decl, op, paramName, paramType, ast: parsing.ast.decls.Operator) {
		final oper = new BinaryOperator({
			decl: decl,
			span: ast.span,
			op: op,
			opSpan: ast.symbolSpan,
			paramName: paramName,
			paramType: decl.makeTypePath(paramType),
			ret: ast.ret.map(ret -> decl.makeTypePath(ret)),
			body: ast.body.map(body -> body.stmts)
		});

		for(generic in ast.generics.mapArray(Generic.fromAST.bind(decl, _))) {
			oper.generics.add(generic.name.name, generic);
		}

		return oper;
	}

	override function hasErrors() {
		return super.hasErrors() || generics.allValues().some(g -> g.hasErrors());
	}

	override function allErrors() {
		return super.allErrors().concat(generics.allValues().flatMap(g -> g.allErrors()));
	}

	inline function opName() {
		return op.symbol();
	}
}