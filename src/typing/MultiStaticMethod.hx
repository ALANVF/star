package typing;

import text.Span;
import parsing.ast.Expr;
import parsing.ast.Ident;
import typing.Traits;

class MultiStaticMethod extends StaticMethod {
	@ignore final typevars = new MultiMap<String, TypeVar>();
	var params: MultiParams = [];
	var fuzzyName: String;
	var isUnordered: Bool = false;

	static function fromAST(decl: AnyTypeDecl, ast: parsing.ast.decls.Method) {
		final method = new MultiStaticMethod({
			decl: decl,
			span: ast.span,
			params: null,    // hack for partial initialization
			fuzzyName: null, // hack for partial initialization
			ret: null,       // hack for partial initialization
			body: ast.body.map(body -> body.stmts())
		});

		final params = switch ast.spec.of {
			case Multi(params2): params2.map(p -> {
				final type = method.makeTypePath(p.type);
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

		method.params = params;
		method.fuzzyName = params.map(p -> p.label.name + ":").join(" ");
		method.ret = ast.ret.map(ret -> method.makeTypePath(ret));

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(method, a))) {
			method.typevars.add(typevar.name.name, typevar);
		}

		for(attr => span in ast.attrs) switch attr {
			case IsStatic:
			
			case IsHidden(_) if(method.hidden.isSome()): method.errors.push(Errors.duplicateAttribute(method, method.fuzzyName, "hidden", span));
			case IsHidden(None): method.hidden = Some(None);
			case IsHidden(Some(outsideOf)): method.hidden = Some(Some(decl.makeTypePath(outsideOf)));

			case IsMain: method.isMain = true;

			case IsNoinherit: method.noInherit = true;

			case IsGetter: method.isGetter = true;

			case IsSetter: method.isSetter = true;

			case IsUnordered: method.isUnordered = true;

			case IsNative(_) if(method.native.isSome()): method.errors.push(Errors.duplicateAttribute(method, method.fuzzyName, "native", span));
			case IsNative(sym): method.native = Some(sym);

			case IsInline: method.isInline = true;

			case IsAsm: method.isAsm = true;

			case IsMacro: method.isMacro = true;
		}

		return method;
	}


	override function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		return BaseMethod._findType(this, path, from, depth);
	}

	function methodName() {
		return fuzzyName.replaceAll(" ", "");
	}
}