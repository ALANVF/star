package typing;

import typing.Traits;


/* This pass resolves basic types and stuff that aren't inside an expr, block, or statement.
 * Basic types are types that can be resolved from a simple type path, or are already concrete:
 * - TPath
 * - TLookup (sometimes)
 * - TModular (TODO)
 * - TConcrete, which stays concrete
 * If there are generic arguments (TApplied), they are ignored when doing lookup
 * except when checking for arity if the base type is concrete (although the args themselves are still resolved).
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
		var from: ILookupType;
		switch imp {
			case {as: Some(_)}:
				file.errors.push(Errors.notYetImplemented(imp.span));
				continue;
			
			case {from: None}:
				from = file;

			case {from: Some(UType(_, type))}:
				switch file.findType((type : TypePath).toLookupPath(file), true, Nil) {
					case Some(t):
						resolveBasicType(file, t);
						from = t;
					
					case None:
						file.errors.push(Errors.invalidTypeLookup(type.span()));
						continue;
				}

			default:
				file.errors.push(Errors.notYetImplemented(imp.span));
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

		for(type in types) switch from.findType((type : TypePath).toLookupPath(from), true, from == file ? Nil : List.of(file)) {
			case Some(t):
				resolveBasicType(from, t);
				file.imported.push(t);
			
			case None:
				file.errors.push(Errors.invalidTypeLookup(type.span()));
		}
	}

	for(decl in file.decls) resolveDecl(decl);
	for(cat in file.categories) resolveCategory(cat);
}


static function resolveBasicType(
	source: ILookupType,
	type: Type,
	cache: List<{}> = Nil
) {
	type.t._match(
		at(TPath(depth, lookup, src)) => {
			for(_ in 0...depth) {
				switch src.findType(lookup, true, cache) {
					case Some(found):
						cache = cache.prepend(found);
					
					case None: if(source != src) {
						switch source.findType(lookup, true, cache.prepend(source)) {
							case Some(found):
								cache = cache.prepend(found);
							
							case None: source._match(
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
					} else source._match(
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

			switch src.findType(lookup, true, cache) {
				case Some(found):
					type.t = found.t;
				
				case None: if(source != src) {
					switch source.findType(lookup, true, cache.prepend(source)) {
						case Some(found):
							type.t = found.t;
						
						case None: {
							// a very lazy way of incrementally looking up the type path. works for unknown reasons
							var cur = source;
							var status = false;
							var first = true;
							final orig = lookup;
							while(true) {
								cache = cache.prepend(cur);
								switch lookup {
									case Nil3:
										if(lookup == Nil3) status = true;
										break;
									case Cons3(a, b, c, r):
										final l = lookup;
										lookup = r;
										switch cur.findType(List3.of([a, b, c]), first, cache) {
											case Some(f):
												if(cur == f) break;
												cur = f;
												if(lookup == Nil3) {
													status = true;
													break;
												}
											case None:
												lookup = l;
												break;
										}
								}
								first = false;
							}

							if(status) {
								lookup = orig;
							} source._match(
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
				} else {
					// a very lazy way of incrementally looking up the type path. works for unknown reasons
					var cur = source;
					var status = false;
					var first = true;
					final orig = lookup;
					while(true) {
						cache = cache.prepend(cur);
						switch lookup {
							case Nil3:
								if(lookup == Nil3) status = true;
								break;
							case Cons3(a, b, c, r):
								final l = lookup;
								lookup = r;
								switch cur.findType(List3.of([a, b, c]), first, cache) {
									case Some(f):
										if(cur == f) break;
										cur = f;
										if(lookup == Nil3) {
											status = true;
											break;
										}
									case None:
										lookup = l;
										break;
								}
						}
						first = false;
					}

					if(status) {
						lookup = orig;
					} src._match(
						at(e is IErrors) => {
							e.errors.push(Errors.invalidTypeLookup(
								type.span.nonNull(),
								"Type does not exist! "+lookup.simpleName()+" "+cur._match(
									at(t is Type) => t.simpleName(),
									_ => Std.string(cur)
								)
							));
							return;
						},
						_ => throw "I don't know where this error came from!"
					);
				}
			}

			lookup.forEach((_, _, ps) -> if(ps.length != 0) {
				for(p in ps) {
					resolveBasicType(src, p);
				}
			});
		},

		at(TLookup(base, lookup, src)) => {
			resolveBasicType(src, base);

			base.t._match(
				at(TConcrete(c) /*| TModular(_, _)*/) => {
					cache = cache.prepend(base);
					switch c.findType(lookup, false, Nil) {
						case Some(found):
							type.t = found.t;
						
						case None: if(source != src) {
							/*src._match(
								at(e is IErrors) => e.errors.pop(),
								_ => {}
							);*/

							switch source.findType(lookup, true, cache.prepend(source)) {
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