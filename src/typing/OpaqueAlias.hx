package typing;

import typing.Traits;

class OpaqueAlias extends Alias {
	final staticMethods: Array<StaticMethod> = [];
	final methods: Array<Method> = [];
	final operators: Array<Operator> = [];

	static function fromAST(lookup, ast: parsing.ast.decls.Alias) {
		final alias = new OpaqueAlias({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: []
		});

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(alias, a))) {
			alias.typevars.add(typevar.name.name, typevar);
		}

		final body = switch ast.kind {
			case Opaque(body): body;
			default: throw "Error!";
		};

		ast.params._and(params => {
			alias.params = params.of.map(param -> alias.makeTypePath(param));
		});

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(alias.hidden != null): alias.errors.push(Type_DuplicateAttribute(alias, ast.name.name, "hidden", span));
			case IsHidden(None): alias.hidden = None;
			case IsHidden(Some(outsideOf)): alias.hidden = Some(alias.makeTypePath(outsideOf));

			case IsFriend(_) if(alias.friends.length != 0): alias.errors.push(Type_DuplicateAttribute(alias, ast.name.name, "friend", span));
			case IsFriend(One(friend)): alias.friends.push(alias.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) alias.friends.push(alias.makeTypePath(friend));
			
			case IsNoinherit: alias.errors.push(Type_InvalidAttribute(alias, ast.name.name, "noinherit", span));
		}

		if(body.isSome()) {
			for(decl in body.value().of) switch decl {
				case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(alias, m).forEach(x -> alias.staticMethods.push(x));
				case DMethod(m): alias.methods.push(Method.fromAST(alias, m));
	
				case DOperator(o): Operator.fromAST(alias, o).forEach(x -> alias.operators.push(x));
	
				default: alias.errors.push(Type_UnexpectedDecl(alias, decl));
			}
		}

		return alias;
	}

	override function hasErrors() {
		return super.hasErrors()
			|| staticMethods.some(m -> m.hasErrors())
			|| methods.some(m -> m.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();

		for(method in staticMethods) result = result.concat(method.allErrors());
		for(method in methods) result = result.concat(method.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}
	
	override function declName() {
		return "opaque alias";
	}


	// Method lookup

	override function findSingleStatic(ctx: Ctx, name: String, from: Type, getter = false, cache: TypeCache = Nil): Null<SingleStaticKind> {
		if(cache.contains(thisType)) return null;
		
		for(mth in staticMethods) mth._match(
			at(sm is SingleStaticMethod) => {
				if(sm.name.name == name && (!getter || sm.isGetter) && from.canSeeMethod(sm)) {
					return SSMethod(sm);
				}
			},
			at(mm is MultiStaticMethod) => if(!getter) {
				if(mm.params[0].label.name == name && mm.params.every(p -> p.value != null)) {
					return SSMultiMethod(mm);
				}
			},
			_ => {}
		);

		return null;
	}


	override function findMultiStatic(ctx: Ctx, names: Array<String>, from: AnyTypeDecl, setter = false, cache: TypeCache = Nil) {
		if(cache.contains(thisType)) return [];
		
		final candidates: Array<MultiStaticKind> = [];

		if(setter) {
			throw "todo";
		} else {
			for(mth in staticMethods) mth._match(
				at(mm is MultiStaticMethod) => if(from.canSeeMethod(mm))
					mm.params.matchesNames(names)._match(
						at(Yes) => candidates.push(MSMethod(mm)),
						at(Partial(indexes)) => candidates.push(MSMethod(mm, indexes)),
						at(No) => {}
					),
				_ => {}
			);
		}

		return candidates;
	}


	override function findSingleInst(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false, cache: TypeCache = Nil): Null<SingleInstKind> {
		if(cache.contains(thisType)) return null;

		for(mth in methods) mth._match(
			at(sm is SingleMethod) => {
				if(sm.name.name == name && (!getter || sm.isGetter) && from.canSeeMethod(sm)) {
					return SIMethod(sm);
				}
			},
			at(mm is MultiMethod) => if(!getter) {
				if(mm.params[0].label.name == name && mm.params.every(p -> p.value != null)) {
					return SIMultiMethod(mm);
				}
			},
			_ => {}
		);
		
		for(refinee in refinees) {
			refinee.findSingleInst(ctx, name, from, getter, cache)._match(
				at(si!) => return si,
				_ => {}
			);
		}
		
		return null;
	}


	override function findMultiInst(ctx: Ctx, names: Array<String>, from: Type, setter = false, cache: TypeCache = Nil) {
		if(cache.contains(thisType)) return [];
		
		final candidates: Array<MultiInstKind> = [];

		if(setter) {
			switch names {
				case [name]: for(mth in methods) mth._match(
					at(mm is MultiMethod) => if(mm.isSetter) mm.params._match(
						at([{label: {name: l}}], when(l == name && from.canSeeMethod(mm))) => {
							candidates.push(MIMethod(mm));
						},
						_ => {}
					),
					_ => {}
				);

				default: for(mth in methods) mth._match(
					at(mm is MultiMethod) => if(mm.isSetter && from.canSeeMethod(mm))
						mm.params.matchesNames(names, true)._match(
							at(Yes) => candidates.push(MIMethod(mm)),
							at(Partial(indexes)) => candidates.push(MIMethod(mm, indexes)),
							at(No) => {}
						),
					_ => {}
				);
			}
		} else {
			for(mth in methods) mth._match(
				at(mm is MultiMethod) => if(from.canSeeMethod(mm))
					mm.params.matchesNames(names, mm.isSetter)._match(
						at(Yes) => candidates.push(MIMethod(mm)),
						at(Partial(indexes)) => candidates.push(MIMethod(mm, indexes)),
						at(No) => {}
					),
				_ => {}
			);
		}

		for(refinee in refinees) {
			candidates.pushAll(refinee.findMultiInst(ctx, names, from, setter, cache));
		}

		return candidates;
	}


	override function findCast(ctx: Ctx, target: Type, from: AnyTypeDecl, cache: TypeCache = Nil) {
		if(cache.contains(thisType)) return [];

		final candidates: Array<CastKind> = [];

		for(mth in methods) mth._match(
			at(cm is CastMethod) => {
				if(cm.type.hasChildType(target) && from.canSeeMethod(cm)) {
					candidates.push(CMethod(cm));
				}
			},
			_ => {}
		);

		for(refinee in refinees) {
			candidates.pushAll(refinee.findCast(ctx, target, from, cache));
		}
		
		return candidates;
	}


	override function findUnaryOp(ctx: Ctx, op: UnaryOp, from: AnyTypeDecl, cache: TypeCache = Nil): Null<UnaryOpKind> {
		if(cache.contains(thisType)) return null;

		for(oper in operators) oper._match(
			at(unary is UnaryOperator) => {
				if(unary.op == op && from.canSeeMethod(unary)) {
					return UOMethod(unary);
				}
			},
			_ => {}
		);

		for(refinee in refinees) {
			refinee.findUnaryOp(ctx, op, from, cache)._match(
				at(uo!) => return uo,
				_ => {}
			);
		}

		return null;
	}

	
	override function findBinaryOp(ctx: Ctx, op: BinaryOp, from: Type, cache: TypeCache = Nil) {
		final candidates: Array<BinaryOpKind> = [];

		for(oper in operators) oper._match(
			at(binary is BinaryOperator) => {
				if(binary.op == op && from.canSeeMethod(binary)) {
					candidates.push(BOMethod(binary));
				}
			},
			_ => {}
		);

		for(refinee in refinees) {
			candidates.pushAll(refinee.findBinaryOp(ctx, op, from, cache));
		}

		return candidates;
	}
}