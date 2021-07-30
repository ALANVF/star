package compiler.nim;

import compiler.nim.Expr;
import compiler.nim.AnyType;

@:publicFields
class Wrap {
	static final TAB = "    ";

	static function wrapImmediate(expr: Expr) return expr._match(
		at(
			( (ESym(_) ... EParenList(_))
			| EPrefix(PAt, _)
			| EDeref(_)
			| (EIndex(_, _) ... EExcl(_, _))
			| ECall(_, _, _, null | false)
			| EMethod(_, _, _, _, null | false)
			| EInit(_, _)
		)) => expr,
		
		at(EType(t)) => EType(wrapImmediateType(t)),
		
		_ => EParen(expr)
	);

	static function wrapInfix(?op: Infix, expr: Expr, left = false) return Util._match([op, expr],
		at([IAssignOp(_), _] | [_,
			( (ESym(_) ... EParenList(_))
			| EPrefix(_, _)
			| EDeref(_)
			| (EIndex(_, _) ... EExcl(_, _))
			| ECall(_, _, _, null | false)
			| EMethod(_, _, _, _, null | false)
			| EInit(_, _)
		)]) => expr,
		
		at([_,
			( EBlock(_, _)
			| ECall(_, _, _, true)
			| EMethod(_, _, _, _, true)
			| (EIf(_, _, _, _, _) ... EStmtList(_))
		)], when(!left)) => expr,
		
		at([_, EType(t)]) => EType(wrapInfixType(t, left)),
		
		_ => EParen(expr)
	);

	static function wrapImmediateType(type: Type): Type return type._match(
		at(
			( (TInt ... TPointer)
			| (TRange(_) ... TNamedTuple(_))
			| (TRootObj ... TPath(_))
			| TVoid
			| TParen(_)
			| TGeneric(_, _)
		)) => type,
		
		at(TExpr(e)) => TExpr(wrapImmediate(e)),
		
		_ => TParen(type)
	);

	static function wrapInfixType<T>(type: AnyType<T>, left = false): AnyType<T> return type._match(
		at(
			( (TInt ... TPointer)
			| (TRange(_) ... TNamedTuple(_))
			| (TRootObj ... TPath(_))
			| TVoid
			| TParen(_)
			| TGeneric(_, _)
		)) => type,
		
		at(TDistinct(_) | TType(_) | TRef(_) | TPtr(_), when(!left)) => type,
		
		at(TEnum(_) | TObject(_, _, _) | TConcept(_, _, _)) => throw "???",
		
		at(TExpr(e)) => TExpr(wrapInfix(e, left)),
		
		_ => TParen((untyped type : Type))
	);
}