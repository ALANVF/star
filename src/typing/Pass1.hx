package typing;

import typing.Traits;


/* This pass resolves basic types and stuff that aren't inside an expr, block, or statement.
 * Basic types are types that can be resolved from a simple type path, or are already concrete:
 * - TPath
 * - TLookup (sometimes)
 * - TModular (TODO)
 * - TConcrete, which stays concrete
 * - TApplied
 *   - If there are generic arguments (TApplied), they are ignored when doing lookup
 *     except when checking for arity if the base type is concrete (although the args themselves are still resolved).
 */


@:publicFields class Pass1 {
// dummy comment to fix formatting in vscode


static function resolveDirContents(dir: Dir) {
	for(f in dir.files) resolveFileContents(f);
	for(u in dir.units) resolveUnitContents(u);
}


static function resolveProjectContents(proj: Project) {
	resolveDirContents(proj);
	proj.main.forEach(m -> resolveFileContents(m));
}


static function resolveUnitContents(unit: Unit) {
	unit.primary.forEach(p -> resolveFileContents(p));
	resolveDirContents(unit);
}


static function resolveFileContents(file: File) {
	for(imp in file.imports) {
		switch imp {
			case {as: Some(_)}:
				file.errors.push(Errors.notYetImplemented(imp.span));
				continue;
			
			case {from: None}:
				var types: Array<TypePath>;
				switch imp.spec {
					case UType(type):
						types = [type];
					
					case UTypes(_types):
						types = _types;
					
					default:
						file.errors.push(Errors.notYetImplemented(imp.span));
						continue;
				}

				for(type in types) switch file.findType((type : TypePath).toLookupPath(file), Start, null, 0, Nil) {
					case Some(t):
						resolveBasicType(file, t);
						file.imported.push({from: t, types: []});
					
					case None:
						file.errors.push(Errors.invalidTypeLookup(type.span()));
				}

			case {from: Some(UType(_, type))}:
				var from: ILookupType;
				switch file.findType((type : TypePath).toLookupPath(file), Start, null, 0, Nil) {
					case Some(t):
						resolveBasicType(file, t);
						from = t;
					
					case None:
						file.errors.push(Errors.invalidTypeLookup(type.span()));
						continue;
				}

				var types: Array<TypePath>;
				switch imp.spec {
					case UType(type):
						types = [type];
					
					case UTypes(_types):
						types = _types;
					
					default:
						file.errors.push(Errors.notYetImplemented(imp.span));
						continue;
				}

				final imported = [];
				for(type in types) {
					final lookup = (type : TypePath).toLookupPath(from);
					
					//trace(lookup, lookup.span().display());
					switch from.findType(lookup, Start, null, 0, from == file ? Nil : List.of(file)) {
						case Some(t):
							resolveBasicType(from, t);
							
							// TODO: fix. should import `A.B` as `B`, rec errors instead
							if(!lookup.match(Cons3(_,_,_,Nil3))) {
								final realFrom = {
									function loop(ty: Type): ILookupType return ty.t._match(
										at(TPath(_, _, source)) => source,
										at(TLookup(base, _, _)) => base,
										at(TConcrete(decl)) => decl.lookup,
										at(TThis(source)) => source.lookup,
										at(TBlank) => throw "bad",
										at(TMulti(types)) => loop(types[0]),
										at(TApplied(type, _)) => loop(type),
										at(TTypeVar(typevar)) => typevar.lookup,
										at(TModular(type, _)) => loop(type)
									);

									loop(t);
								};
								//trace(realFrom, t.fullName());
								file.imported.push({from: realFrom, types: [t]});
							} else {
								imported.push(t);
							}
						
						case None:
							file.errors.push(Errors.invalidTypeLookup(type.span()));
					}
				}

				file.imported.push({from: from, types: imported});

			default:
				file.errors.push(Errors.notYetImplemented(imp.span));
				continue;
		}
	}

	for(decl in file.decls) resolveDecl(decl);
	for(cat in file.categories) resolveCategory(cat);
}


static function resolveBasicType(source: ILookupType, type: Type, cache: List<{}> = Nil) {
	type.t._match(
		at(TPath(depth, path, src)) => {
			source.findType(path, Start, null, depth, cache)._match(
				at(Some(ty)) => {
					type.t = ty.t;
				},
				at(None) => {
					if(source != src) src.findType(path, Start, null, depth, cache)._match(
						at(Some(ty)) => {
							type.t = ty.t;
						},
						at(None) => src._match(
							at(e is IErrors) => {
								e.errors.push(Errors.invalidTypeLookup(
									type.span.nonNull(),
									"Type does not exist!"
								));
								return;
							},
							_ => throw "I don't know where this error came from!"
						)
					); else {
						source._match(
							at(e is IErrors) => {
								e.errors.push(Errors.invalidTypeLookup(
									type.span.nonNull(),
									"Type does not exist!"
								));
								return;
							},
							_ => throw "I don't know where this error came from!"
						);
					}
				}
			);

			path.forEach((_, _, ps) -> if(ps.length != 0) {
				for(p in ps) {
					resolveBasicType(source, p);
				}
			});
		},

		at(TLookup(base, path, src)) => {
			resolveBasicType(src, base);

			base.t._match(
				at(TConcrete(c) /*| TModular(_, _)*/) => {
					cache = cache.prepend(base);
					switch c.findType(path, Inside, null, 0, Nil) {
						case Some(found):
							type.t = found.t;
						
						case None: if(source != src) {
							/*src._match(
								at(e is IErrors) => e.errors.pop(),
								_ => {}
							);*/

							switch source.findType(path, Start, null, 0, cache.prepend(source)) {
								case Some(found):
									type.t = found.t;
								
								case None: source._match(
									at(e is IErrors) => {
										e.errors.push(Errors.invalidTypeLookup(
											type.span.nonNull(),
											"Type does not exist!"
										));
									},
									_ => throw "I don't know where this error came from!"
								);
							}
						} else source._match(
							at(e is IErrors) => {
								e.errors.push(Errors.invalidTypeLookup(
									type.span.nonNull(),
									"Type does not exist!"
								));
							},
							_ => throw "I don't know where this error came from!"
						);
					}
				},
				_ => {}
			);
		},

		at(TApplied(base, params)) => {
			resolveBasicType(source, base);
			for(p in params) resolveBasicType(source, p);
		},

		_ => {}
	);
}


static function resolveDecl(decl: TypeDecl) {
	decl.friends.forEach(f -> resolveBasicType(decl, f));
	decl.hidden.forEach(h -> h.forEach(t -> resolveBasicType(decl, t)));

	for(typevar in decl.typevars) resolveTypeVar(typevar);
	decl.params.forEach(p -> resolveBasicType(decl, p));

	decl._match(
		at(ns is Namespace) => {
			ns.sealed.forEach(s -> s.forEach(t -> resolveBasicType(decl, t)));
			ns.parents.forEach(p -> resolveBasicType(decl, p));
		},
		_ => {}
	);

	decl._match(
		at(vkind is ValueKind) => vkind.repr.forEach(r -> resolveBasicType(decl, r)),
		at(alias is StrongAlias) => resolveBasicType(decl, alias.type),
		at(alias is DirectAlias) => resolveBasicType(decl, alias.type),
		at({native: Some(NPtr(t))} is Class) => resolveBasicType(decl, t),
		_ => {}
	);

	decl._match(
		at(ns is Namespace) => {
			for(decl2 in ns.decls) resolveDecl(decl2);
			for(cat in ns.categories) resolveCategory(cat);
		},
		_ => {}
	);

	decl._match(
		at(ns is Namespace) => {
			for(m in ns.staticMembers) resolveMember(m);
			for(m in ns.staticMethods) resolveStaticMethod(m);
			ns._match(
				at({members: members, methods: methods, inits: inits, operators: ops} is Class
				| ({members: members, methods: methods, inits: inits, operators: ops} is Protocol)) => {
					for(m in members) resolveMember(m);
					for(m in methods) resolveMethod(m);
					for(i in inits) resolveInit(i);
					for(o in ops) resolveOperator(o);
				},
				at(tkind is TaggedKind) => {
					for(m in tkind.members) resolveMember(m);
					for(m in tkind.methods) resolveMethod(m);
					for(o in tkind.operators) resolveOperator(o);
					for(c in tkind.taggedCases) resolveTaggedCase(c);
				},
				at(vkind is ValueKind) => {
					for(m in vkind.methods) resolveMethod(m);
					for(o in vkind.operators) resolveOperator(o);
				},
				_ => {}
			);
		},
		at({staticMethods: smethods, methods: methods, operators: ops} is StrongAlias
		| ({staticMethods: smethods, methods: methods, operators: ops} is OpaqueAlias)) => {
			for(m in smethods) resolveStaticMethod(m);
			for(m in methods) resolveMethod(m);
			for(o in ops) resolveOperator(o);
		},
		_ => {}
	);

	decl.refinements.forEach(r -> resolveDecl(r));
}


static function resolveCategory(category: Category) {
	category.friends.forEach(f -> resolveBasicType(category, f));
	category.hidden.forEach(h -> h.forEach(t -> resolveBasicType(category, t)));

	for(typevar in category.typevars) resolveTypeVar(typevar);
	
	resolveBasicType(category, category.path);

	category.type.forEach(t -> resolveBasicType(category, t));

	for(m in category.staticMembers) resolveMember(m);
	for(m in category.staticMethods) resolveStaticMethod(m);
	for(m in category.methods) resolveMethod(m);
	for(i in category.inits) resolveInit(i);
	for(o in category.operators) resolveOperator(o);
}


static function resolveTypeVar(typevar: TypeVar) {
	typevar.params.forEach(p -> resolveBasicType(typevar.lookup, p));
	typevar.parents.forEach(p -> resolveBasicType(typevar.lookup, p));
	
	typevar.native._match(
		at(Some(NPtr(t))) => resolveBasicType(typevar.lookup, t),
		_ => {}
	);

	typevar.rule.forEach(r -> resolveTypeRule(typevar.lookup, r));
	
	for(cat in typevar.categories) resolveCategory(cat);

	for(m in typevar.staticMembers) resolveMember(m);
	for(m in typevar.staticMethods) resolveStaticMethod(m);
	for(m in typevar.members) resolveMember(m);
	for(m in typevar.methods) resolveMethod(m);
	for(i in typevar.inits) resolveInit(i);
	for(o in typevar.operators) resolveOperator(o);
	for(c in typevar.taggedCases) resolveTaggedCase(c);
}


static function resolveTypeRule(lookup: ILookupType, rule: TypeRule) rule._match(
	at(Negate(t) | Exists(t)) => resolveBasicType(lookup, t),
	at(Eq(l, r) | Of(l, r) | Lt(l, r) | Le(l, r)) => {
		resolveBasicType(lookup, l);
		resolveBasicType(lookup, r);
	},

	at(Not(r)) => resolveTypeRule(lookup, r),
	at(All(rs) | Any(rs) | One(rs)) => rs.forEach(r -> resolveTypeRule(lookup, r))
);


static function resolveMethod(method: Method) {
	method.hidden.forEach(h -> h.forEach(t -> resolveBasicType(method, t)));

	method._match(
		at(multi is MultiMethod) => {
			for(typevar in multi.typevars) resolveTypeVar(typevar);
			for(param in multi.params) resolveBasicType(method, param.type);
		},
		at(cmethod is CastMethod) => {
			for(typevar in cmethod.typevars) resolveTypeVar(typevar);
			resolveBasicType(method, cmethod.type);
			return;
		},
		_ => {}
	);

	method.ret.forEach(r -> resolveBasicType(method, r));
}


static function resolveStaticMethod(method: StaticMethod) {
	method.hidden.forEach(h -> h.forEach(t -> resolveBasicType(method, t)));

	method._match(
		at(multi is MultiStaticMethod) => {
			for(typevar in multi.typevars) resolveTypeVar(typevar);
			for(param in multi.params) resolveBasicType(method, param.type);
		},
		_ => {}
	);

	method.ret.forEach(r -> resolveBasicType(method, r));
}


static function resolveInit(init: Init) {
	init.hidden.forEach(h -> h.forEach(t -> resolveBasicType(init, t)));

	init._match(
		at(multi is MultiInit) => {
			for(typevar in multi.typevars) resolveTypeVar(typevar);
			for(param in multi.params) resolveBasicType(init, param.type);
		},
		_ => {}
	);
}


static function resolveOperator(op: Operator) {
	op.hidden.forEach(h -> h.forEach(t -> resolveBasicType(op, t)));

	op._match(
		at(binop is BinaryOperator) => {
			for(typevar in binop.typevars) resolveTypeVar(typevar);
			resolveBasicType(op, binop.paramType);
		},
		_ => {}
	);

	op.ret.forEach(r -> resolveBasicType(op, r));
}


static function resolveMember(member: Member) {
	member.hidden.forEach(h -> h.forEach(t -> resolveBasicType(member.lookup, t)));
	member.type.forEach(t -> resolveBasicType(member.lookup, t));
}


static function resolveTaggedCase(tcase: TaggedCase) tcase._match(
	at({params: params} is MultiTaggedCase) => {
		for(param in params) resolveBasicType(tcase.decl, param.type);
	},
	_ => {}
);


}