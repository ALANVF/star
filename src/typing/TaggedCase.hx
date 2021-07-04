package typing;

import parsing.ast.Message;
import parsing.ast.Stmt;
import reporting.Severity;
import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;
import typing.Traits;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
class TaggedCase {
	final errors: Array<Diagnostic> = [];
	final decl: ITaggedCases;
	final span: Span;
	final assoc: Option<Message<parsing.ast.Type>>;
	final init: Option<Array<Stmt>> = None;

	static function fromAST(decl, ast: parsing.ast.decls.Case): TaggedCase {
		switch ast.kind {
			case Tagged({of: Single(name)}, assoc):
				return new SingleTaggedCase({
					decl: decl,
					span: ast.span,
					name: name,
					assoc: assoc,
					init: ast.init.map(i -> i.stmts)
				});
			
			case Tagged({of: Multi(params)}, assoc):
				return new MultiTaggedCase({
					decl: decl,
					span: ast.span,
					params: params.map(p -> {
						final type = decl.makeTypePath(p.type);
						return switch [p.label, p.name] {
							case [Some(l), Some(n)]: {label: l, name: n, type: type};
							case [Some(l), None]: {label: l, name: l, type: type};
							case [None, Some(n)]: {label: new Ident(n.span, "_"), name: n, type: type};
							case [None, None]:
								final span = {
									final s = p.type.span();
									Span.at(s.start, s.source.toNull());
								};
								final ident = new Ident(span, "_");
								{label: ident, name: ident, type: type};
						}
					}),
					assoc: assoc,
					init: ast.init.map(i -> i.stmts)
				});
			
			default: throw "Error!";
		}
	}

	inline function declName() {
		return "tagged case";
	}

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}
}