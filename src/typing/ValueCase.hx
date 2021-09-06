package typing;

import reporting.Severity;
import reporting.Diagnostic;
import parsing.ast.Expr;
import text.Span;
import parsing.ast.Ident;
import typing.Traits;

@:build(util.Auto.build())
class ValueCase implements IErrors {
	final errors: Array<Diagnostic> = [];
	final decl: IValueCases;
	final span: Span;
	final name: Ident;
	var value: Option<Expr>;

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
						valueCase.errors.push(new Diagnostic({
							severity: Severity.ERROR,
							message: "Invalid value case",
							info: [
								Spanned({
									span: Span.range(begin, end),
									message: "Value cases may not have an initializer",
									isPrimary: true
								}),
								Spanned({
									span: Span.range(ast.span, name.span),
									message: 'For value case `${name.name}`',
									isSecondary: true
								})
							]
						}));
				}

				return valueCase;
			
			default: throw "Error!";
		}
	}

	inline function declName() {
		return "value case";
	}

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}
}