package typing;

class CastMethod extends Method {
	@ignore final typevars = new MultiMap<String, TypeVar>();
	var type: Type;

	static function fromAST(decl, ast: parsing.ast.decls.Method) {
		final method = new CastMethod({
			decl: decl,
			span: ast.span,
			type: null, // hack for partial initialization
			ret: None,
			body: ast.body.map(body -> body.stmts())
		});

		method.type = switch ast.spec.of {
			case Cast(type): method.makeTypePath(type);
			default: throw "Error!";
		};

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(method, a))) {
			method.typevars.add(typevar.name.name, typevar);
		}

		final typeName = method.type.simpleName();

		for(attr => span in ast.attrs) switch attr {
			case IsStatic: method.errors.push(Errors.invalidAttribute(method, typeName, "static", span));
			
			case IsHidden(_) if(method.hidden.isSome()): method.errors.push(Errors.duplicateAttribute(method, typeName, "hidden", span));
			case IsHidden(None): method.hidden = Some(None);
			case IsHidden(Some(outsideOf)): method.hidden = Some(Some(decl.makeTypePath(outsideOf)));

			case IsMain: method.errors.push(Errors.invalidAttribute(method, typeName, "main", span));

			case IsNoinherit: method.noInherit = true;

			case IsGetter: method.errors.push(Errors.invalidAttribute(method, typeName, "getter", span));

			case IsSetter: method.errors.push(Errors.invalidAttribute(method, typeName, "setter", span));

			case IsUnordered: method.errors.push(Errors.invalidAttribute(method, typeName, "unordered", span));

			case IsNative(_) if(method.native.isSome()): method.errors.push(Errors.duplicateAttribute(method, typeName, "native", span));
			case IsNative(sym): method.native = Some(sym);

			case IsInline: method.isInline = true;

			case IsAsm: method.isAsm = true;

			case IsMacro: method.isMacro = true;
		}

		return method;
	}

	override function findType(path: LookupPath, absolute = true, cache: List<{}> = Nil): Option<Type> {
		return path._match(
			at([[span, typeName, args], ...rest]) => {
				final res: Option<Type> = switch typevars.find(typeName) {
					case None: return decl.findType(path, true, cache);
					case Some([type]): switch [args, type.params] {
						case [[], _]: Some({t: type.thisType.t, span: span}); // should probably curry parametrics but eh
						case [_, []]:
							// should this check for type aliases?
							errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
							None;
						case [_, params]:
							if(args.length > params.length) {
								errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
								None;
							} else if(args.length < params.length) {
								errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
								None;
							} else {
								Some({t: TApplied(type.thisType, args), span: span});
							}
					}
					case Some(found):
						if(args.length == 0) {
							Some({t: TMulti(found.map(t -> t.thisType)), span: span});
						} else switch found.filter(t -> t.params.length == args.length).map(g -> g.thisType) {
							case []:
								errors.push(Errors.invalidTypeApply(span, "No candidate matches the type arguments"));
								None;
							case [type]: Some({t: TApplied(type, args), span: span});
							case types: Some({t: TMulti(types), span: span});
						}
				};

				switch [rest, res] {
					case [Nil3, _]: res;
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
					case [_, None]: res;
				}
			},
			_ => if(absolute) decl.findType(path, true, cache) else None
		);
	}

	function methodName() {
		return type.simpleName();
	}

	override function hasErrors() {
		return super.hasErrors()
			|| typevars.allValues().some(g -> g.hasErrors());
	}

	override function allErrors() {
		return super.allErrors().concat(typevars.allValues().flatMap(g -> g.allErrors()));
	}
}