package typing;

@:structInit
class ValueKind extends Kind {
	var repr: Option<Type> = None;
	final valueCases: Array<ValueCase> = [];

	static function fromAST(lookup, ast: parsing.ast.decls.Kind) {
		final kind: ValueKind = {
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: []
		};

		kind.initThisType();

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(kind, a))) {
			kind.typevars.add(typevar.name.name, typevar);
		}

		ast.params._and(params => {
			kind.params = params.of.map(param -> kind.makeTypePath(param));
		});

		ast.repr._and(repr => {
			kind.repr = Some(kind.makeTypePath(repr));
		});

		ast.parents._and(parents => {
			for(parent in parents.parents) {
				kind.parents.push(kind.makeTypePath(parent));
			}
		});

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(kind.hidden != null): kind.errors.push(Type_DuplicateAttribute(kind, ast.name.name, "hidden", span));
			case IsHidden(None): kind.hidden = None;
			case IsHidden(Some(outsideOf)): kind.hidden = Some(kind.makeTypePath(outsideOf));

			case IsFriend(_) if(kind.friends.length != 0): kind.errors.push(Type_DuplicateAttribute(kind, ast.name.name, "friend", span));
			case IsFriend(One(friend)): kind.friends.push(kind.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) kind.friends.push(kind.makeTypePath(friend));

			case IsSealed(_) if(kind.sealed.isSome()): kind.errors.push(Type_DuplicateAttribute(kind, ast.name.name, "sealed", span));
			case IsSealed(None): kind.sealed = Some(None);
			case IsSealed(Some(outsideOf)): kind.sealed = Some(Some(kind.makeTypePath(outsideOf)));

			case IsFlags: kind._isFlags = true;

			case IsStrong: kind._isStrong = true;

			case IsUncounted: kind._isUncounted = true;
		}

		for(decl in ast.body.of) switch decl {
			case DMember(m) if(m.attrs.exists(IsStatic)): kind.staticMembers.push(Member.fromAST(kind, m));

			case DCase(c = {kind: Scalar(_, _)}): kind.valueCases.push(ValueCase.fromAST(kind, c));

			case DModule(m): kind.addTypeDecl(Module.fromAST(kind, m));

			case DClass(c): kind.addTypeDecl(Class.fromAST(kind, c));

			case DProtocol(p): kind.addTypeDecl(Protocol.fromAST(kind, p));

			case DKind(k): kind.addTypeDecl(Kind.fromAST(kind, k));
			
			case DAlias(a): kind.addTypeDecl(Alias.fromAST(kind, a));

			case DCategory(c): kind.categories.push(Category.fromAST(kind, c));

			case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(kind, m).forEach(x -> kind.staticMethods.push(x));
			case DMethod(m): kind.methods.push(Method.fromAST(kind, m));

			case DOperator(o): Operator.fromAST(kind, o).forEach(x -> kind.operators.push(x));

			case DDefaultInit(_) if(kind.staticInit.isSome()): kind.errors.push(Type_DuplicateDecl(kind, decl));
			case DDefaultInit(i): kind.staticInit = Some(StaticInit.fromAST(kind, i));
			
			case DDeinit(d) if(kind.staticDeinit.isSome()): kind.staticDeinit = Some(StaticDeinit.fromAST(kind, d));
			case DDeinit(d): kind.deinit = Some(Deinit.fromAST(kind, d));
			
			default: kind.errors.push(Type_UnexpectedDecl(kind, decl));
		}

		return kind;
	}

	override function hasErrors() {
		return super.hasErrors()
			|| valueCases.some(c -> c.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(valueCase in valueCases) result = result.concat(valueCase.allErrors());
		
		return result;
	}


	// Cases

	override function allValueCases(): Array<ValueCase> {
		var res = [];

		for(parent in parents) {
			res = res.concat(parent.allValueCases());
		}

		for(ref in refinees) {
			res = res.concat(ref.allValueCases());
		}

		res = res.concat(valueCases);

		return res;
	}


	// Method lookup

	override function findSingleStatic(ctx: Ctx, name: String, from: Type, getter = false, cache: TypeCache = Nil): Null<SingleStaticKind> {
		if(cache.contains(thisType)) return null;
		
		for(vcase in valueCases) {
			if(vcase.name.name == name) {
				return SSValueCase(vcase);
			}
		}

		return super.findSingleStatic(ctx, name, from, getter, cache);
	}


	override function findCast(ctx: Ctx, target: Type, from: Type, cache: TypeCache = Nil): Array<CastKind> {
		return super.findCast(ctx, target, from, cache).concat(repr._match(
			at(Some(r), when(!_isFlags && r.strictUnifyWithType(target) != null)) => [CUpcast(target)],
			at(Some(r), when(_isFlags)) => target._match(
				at({t: TApplied(a, [p])}) => {
					if(a.hasParentType(Pass2.STD_Array) && r.strictUnifyWithType(p) != null) {
						[CUpcast(target)];
					} else {
						[];
					}
				},
				_ => []
			),
			_ => []
		));
	}


	override function findBinaryOp(ctx: Ctx, op: BinaryOp, from: Type, cache: TypeCache = Nil) {
		final res = super.findBinaryOp(ctx, op, from, cache);

		if(_isFlags) {
			return res.concat(Pass2.STD_MultiKind.findBinaryOp(ctx, op, from, cache));
		} else {
			return res;
		}
	}
}