package typing;

import parsing.ast.Stmt;
import reporting.Severity;
import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
class TaggedCase implements IDecl {
	final errors: Array<Diagnostic> = [];
	final decl: ITaggedCases;
	final span: Span;
	final init: Option<Array<Stmt>> = None;

	static function fromAST(decl, ast: parsing.ast.decls.Case): TaggedCase {
		switch ast.kind {
			case Tagged({of: Single(name)}):
				return new SingleTaggedCase({
					decl: decl,
					span: ast.span,
					name: name,
					init: ast.init.map(i -> i.stmts)
				});
			
			case Tagged({of: Multi(params)}):
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
								final span = switch p.type {
									case Nil: throw "Error!";
									case Cons(Named(span2, _, _) | Blank(span2, _), _):
										Span.at(span2.start.advance(-1), span2.source.toNull());
								};
								final ident = new Ident(span, "_");
								{label: ident, name: ident, type: type};
						}
					}),
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