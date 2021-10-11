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
	@ignore final typevars = new MultiMap<String, TypeVar>();
	final op: BinaryOp;
	final paramName: Ident;
	var paramType: Type;

	static function fromAST(decl, op, paramName, paramType, ast: parsing.ast.decls.Operator) {
		final oper = new BinaryOperator({
			decl: decl,
			span: ast.span,
			op: op,
			opSpan: ast.symbolSpan,
			paramName: paramName,
			paramType: null, // hack for partial initialization
			ret: null,       // hack for partial initialization
			body: ast.body.map(body -> body.stmts())
		});

		oper.paramType = oper.makeTypePath(paramType);
		oper.ret = ast.ret.map(ret -> oper.makeTypePath(ret));

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(oper, a))) {
			oper.typevars.add(typevar.name.name, typevar);
		}

		return oper;
	}

	function methodName() {
		return opName();
	}

	override function hasErrors() {
		return super.hasErrors()
			|| typevars.allValues().some(g -> g.hasErrors());
	}

	override function allErrors() {
		return super.allErrors().concat(typevars.allValues().flatMap(g -> g.allErrors()));
	}

	inline function opName() {
		return op.symbol();
	}


	override function findType(path: LookupPath, search: Search, from: Null<Traits.ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		return BaseMethod._findType(this, path, depth);
	}
}