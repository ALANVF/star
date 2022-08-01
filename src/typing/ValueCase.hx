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
	var decl: AnyTypeDecl;
	var span: Span;
	var name: Ident;
	var value: Null<Expr>;
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

				ast.init._and(init => {
					valueCase.errors.push(Type_NoValueCaseInit(
						name.name,
						Span.range(ast.span, name.span),
						Span.range(init.begin, init.end)
					));
				});

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