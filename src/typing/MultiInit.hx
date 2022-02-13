package typing;

import parsing.ast.Expr;
import text.Span;
import parsing.ast.Ident;
import typing.Traits;

class MultiInit extends Init {
	@ignore final typevars = new MultiMap<String, TypeVar>();
	var params: MultiParams = [];
	var fuzzyName: String;
	var isUnordered: Bool = false;

	static function fromAST(decl: AnyTypeDecl, ast: parsing.ast.decls.Init): MultiInit {
		final init = new MultiInit({
			decl: decl,
			span: ast.span,
			params: null,    // hack for partial initialization
			fuzzyName: null, // hack for partial initialization
			body: ast.body.toNull()._and(body => body.stmts())
		});

		final params = switch ast.spec.of {
			case Multi(params2): params2.map(p -> {
				final type = init.makeTypePath(p.type);
				return switch [p.label, p.name] {
					case [Some(l), Some(n)]: {label: l, name: n, type: type, value: p.value.toNull()};
					case [Some(l), None]: {label: l, name: l, type: type, value: p.value.toNull()};
					case [None, Some(n)]: {label: new Ident(n.span, "_"), name: n, type: type, value: p.value.toNull()};
					case [None, None]:
						final span = {
							final s = p.type.span();
							Span.at(s.start, s.source.toNull());
						};
						final ident = new Ident(span, "_");
						{label: ident, name: ident, type: type, value: p.value.toNull()};
				}
			});
			default: throw "Error!";
		};

		init.params = params;
		init.fuzzyName = params.map(p -> p.label.name + ":").join(" ");


		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(init, a))) {
			init.typevars.add(typevar.name.name, typevar);
		}
		
		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(init.hidden != null): init.errors.push(Type_DuplicateAttribute(init, init.fuzzyName, "hidden", span));
			case IsHidden(None): init.hidden = None;
			case IsHidden(Some(outsideOf)): init.hidden = Some(decl.makeTypePath(outsideOf));

			case IsNoinherit: init.noInherit = true;

			case IsUnordered: init.isUnordered = true;

			case IsNative(_) if(init.native != null): init.errors.push(Type_DuplicateAttribute(init, init.fuzzyName, "native", span));
			case IsNative(sym): init.native = sym;

			case IsAsm: init.isAsm = true;
			
			case IsMacro: init.isMacro = true;
		}

		return init;
	}


	override function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		return BaseMethod._findType(this, path, from, depth);
	}

	function methodName() {
		return fuzzyName.replaceAll(" ", "");
	}
}