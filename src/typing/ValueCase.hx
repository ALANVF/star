package typing;

import reporting.Severity;
import errors.Error;
import parsing.ast.Expr;
import text.Span;
import parsing.ast.Ident;
import typing.Traits;

@:build(util.Auto.build())
class ValueCase implements IErrors {
	final errors: Array<Error> = [];
	final decl: AnyTypeDecl;
	final span: Span;
	final name: Ident;
	var value: Option<Expr>;
	@ignore var typedValue: Null<TExpr> = null;

	static function fromAST(decl, ast: parsing.ast.decls.Case) {
		switch ast.kind {
			case Scalar(name, value):
				final valueCase = new ValueCase({
					decl: decl,
					span: ast.span,
					name: name,
					value: value
				});

				switch ast.init {
					case None:
					case Some({begin: begin, end: end}):
						valueCase.errors.push(Type_NoValueCaseInit(
							name.name,
							Span.range(ast.span, name.span),
							Span.range(begin, end)
						));
				}

				return valueCase;
			
			default: throw "Error!";
		}
	}

	function declName() {
		return "value case";
	}

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}
}