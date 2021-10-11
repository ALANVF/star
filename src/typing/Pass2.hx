package typing;

import typing.EmptyMethod;
import parsing.ast.Ident;
import text.Span;
import reporting.Diagnostic;
import typing.Traits;
import Util.detuple2;
import parsing.ast.Expr as UExpr;
import parsing.ast.Stmt as UStmt;
import parsing.ast.Type as UType;
import parsing.ast.Message as UMessage;
import parsing.ast.Cascade as UCascade;
import parsing.ast.Block as UBlock;


/* This pass does all the things, including typing statements and expressions (well... most of them).
 *
 * TODO:
 * - typed patterns / destructuring
 * - other typed exprs (operators, members, etc)
 * - enforced visibility
 * - hardcode some macro stuff (will be implemented correctly in bootstrapped compiler)
 * 
 * TODO (...eventually):
 * - complex typevars/typeclasses (nested type conditions, structural reqs, existential types, curried HKTs, and anything else that might also exist)
 * - unique typevar/typeclass instances
 * - very specialized type refinement/overloading
 * - type inference for type arguments (mainly for refined/overloaded types)
 * 
 */


enum Where {
	WEmptyMethod(m: EmptyMethod);
	WMethod(m: AnyMethod);
	WDecl(d: TypeDecl);
	WCategory(c: Category);
	WBlock;
	WPattern;
	WObjCascadeBlock(t: Null<Type>);
	WTypeCascade;
}

@:publicFields @:structInit class Ctx {
	var where: Where;
	var outer: Null<Ctx> = null;
	var thisType: Type;
	var locals: Map<String, Local> = [];
	var labels: Map<String, TStmt> = [];

	var typeDecl(get, never): ITypeDecl; private inline function get_typeDecl(): ITypeDecl return where._match(
		at(WDecl(decl)) => decl,
		at(WCategory(cat)) => cat,
		at(WObjCascadeBlock(_) | WBlock | WPattern) => outer._match(
			at(ctx!) => ctx.typeDecl,
			_ => throw "bad"
		),
		_ => {
			function loop(type: Type) return type.t._match(
				at(TConcrete(decl)) => decl,
				at(TApplied({t: TConcrete(decl)}, _)) => decl,
				at(TModular(ty, _)) => loop(ty),
				at(TMulti(types)) => Type.leastSpecific(types)._match(
					at([]) => throw "bad",
					at([ty]) => loop(ty),
					at(tys) => throw "todo"
				),
				//case TApplied()
				_ => throw "bad "+type
			);

			loop(thisType);
		}
	);

	function innerDecl(decl: TypeDecl): Ctx {
		return {
			where: WDecl(decl),
			outer: this,
			thisType: decl.thisType
		};
	}

	function innerCategory(cat: Category): Ctx {
		return {
			where: WCategory(cat),
			outer: this,
			thisType: cat.thisType
		};
	}

	function innerEmptyMethod(method: EmptyMethod): Ctx {
		return {
			where: WEmptyMethod(method),
			outer: this,
			thisType: thisType
		};
	}

	function innerMethod(method: AnyMethod): Ctx {
		return {
			where: WMethod(method),
			outer: this,
			thisType: thisType
		};
	}

	function innerBlock(): Ctx {
		return {
			where: WBlock,
			outer: this,
			thisType: thisType
		};
	}

	function innerPattern(): Ctx {
		return {
			where: WPattern,
			outer: this,
			thisType: thisType
		};
	}

	function innerCascade(?t: Type): Ctx {
		return {
			where: WObjCascadeBlock(t),
			outer: this,
			thisType: t._or(thisType) // TODO: fix
		};
	}

	function addError(diag: Diagnostic) {
		switch where {
			case WMethod(method): method.errors.push(diag);
			case WDecl(decl): decl.errors.push(diag);
			default: throw "bad";
		}
	}

	function findLabel(label: String): Null<TStmt> {
		return labels[label]._or(
			outer._and(o => o.findLabel(label))
		);
	}

	function findLocal(name: String, depth = 0): Null<Local> {
		return locals[name]._match(
			at(loc!, when(depth == 0)) => loc,
			_ => where._match(
				at(WObjCascadeBlock(t!)) => t.findSingleInst(name, outer.typeDecl, true)._match(
					at(SIMember(mem), when(depth == 0)) => new LocalField(mem, mem.name.name, mem.type.toNull(), null),
					_ => outer.findLocal(name, depth)
				),
				at(WObjCascadeBlock(_)) => throw "todo",
				at(WTypeCascade) => throw "todo",
				at(WBlock) => outer.findLocal(name, depth),
				at(WPattern) => {
					// TODO
					outer.findLocal(name, depth);
				},
				at(WDecl(_)) => throw ":thonk:",
				at(WCategory(_)) => throw "bad",
				at(WEmptyMethod((_ : BaseMethod) => mth) | WMethod(mth)) => {
					function loop(type: Type, cache: List<Type>): Null<Local> return cache.contains(type) ? null : type.t._match(
						// TODO: fix
						at(TApplied({t: TConcrete(decl)}, _) | TConcrete(decl)) => decl._match(
							at(ns is Namespace) => {
								ns.staticMembers.find(mem -> mem.name.name == name)
								._andOr(
									mem => (locals[name] = new LocalField(
										mem,
										mem.name.name,
										mem.type.toNull(),
										mem.value.map(v -> Pass2.typeExpr(this, v)).toNull()
									)),
									{
										var value: Null<Local> = ns._match(
											at({members: ms} is ClassLike) => mth._match(
												at(_ is StaticMethod | _ is StaticInit | _ is StaticDeinit) => null,
												_ => ms.find(mem -> mem.name.name == name)._and(
													mem => (locals[name] = new LocalField(
														mem,
														mem.name.name,
														mem.type.toNull(),
														mem.value.map(v -> Pass2.typeExpr(this, v)).toNull()
													))
												)
											),
											_ => null
										);
										
										if(value == null) for(parent in ns.parents) switch parent.t {
											case TConcrete(declp) | TApplied({t: TConcrete(declp)}, _):
												final v = loop(declp.thisType, cache.prepend(type));
												if(v != null) {
													value = v;
													break;
												}
											
											case TMulti(types):
												for(type in types) switch type.t {
													case TConcrete(declp) | TApplied({t: TConcrete(declp)}, _):
														final v = loop(declp.thisType, cache.prepend(type));
														if(v != null) {
															value = v;
															break;
														}
													
													default: throw "bad";
												}
	
												if(value != null) break;
	
											default: throw "bad";
										}
	
										if(value == null && ns.params.length != 0) {
											ns.lookup.findType(List3.of([ns.name.span, ns.name.name, ns.params]), Inside, ns, 0, List.of(type))._match(
												at(Some({t: TConcrete(decl2) | TApplied({t: TConcrete(decl2)}, _)})) => {
													value = loop(decl2.thisType, cache.prepend(type));
												},
												at(Some({t: TMulti(types)})) => for(ty in types) loop(ty, cache.prepend(type))._match(
													at(local!) => {
														value = local;
														break;
													},
													_ => {}
												),
												_ => {}
											);
										}
	
										value;
									}
								);
							},
							_ => throw "todo"
						),
						at(TModular(ty, _)) => loop(ty, cache),
						at(TMulti(types)) => Type.leastSpecific(types)._match(
							at([]) => throw "bad",
							at([ty]) => loop(ty, cache.prepend(type)),
							at(tys) => throw "todo"
						),
						_ => throw "bad"
					);

					loop(thisType, Nil);
				}
			)
		);
	}

	function getType(path: TypePath): Type {
		where._match(
			at(WObjCascadeBlock(t!)) => t.findType(path.toLookupPath(thisType), Start, typeDecl, path.leadingCount())._match(
				at(found = Some(_)) => found,
				at(None) => return outer.getType(path)
			),
			at(WMethod(m)) => m.findType(path.toLookupPath(thisType), Start, typeDecl, path.leadingCount()),
			at(WDecl(d)) => d.findType(path.toLookupPath(thisType), Start, typeDecl, path.leadingCount()),
			_ => return outer.getType(path)
		)._match(
			at(Some(t)) => return t,
			at(None) => outer._match(
				at(ctx!) => return ctx.getType(path),
				_ => throw 'error: unknown type `${path.simpleName()}` at ${path.span().display()}'
			)
		);
	}

	function allowsThis() {
		return switch where {
			case WDecl(d): !(d is Module);
			case WCategory(c): true;// meh
			case WEmptyMethod(m): !(m is StaticInit || m is StaticDeinit) && outer.allowsThis();
			case WMethod(m): !(m is StaticMethod) && outer.allowsThis();
			case WBlock | WPattern: outer.allowsThis();
			case WObjCascadeBlock(_): true;
			case WTypeCascade: outer.allowsThis();
		};
	}

	function description() {
		return where._match(
			at(WEmptyMethod(m)) => m.declName(),
			at(WMethod(m)) => m.declName() + " ["+m.methodName()+"] for "+outer.description(),
			at(WDecl(decl)) => decl.declName() + " " + decl.fullName(),
			at(WCategory(_)) => throw "bad",
			at(WBlock) => "{ ... } in " + {
				final decl = this.typeDecl;
				decl.declName() + " " + decl.fullName();
			},
			at(WPattern) => "pattern ... in " + {
				final decl = this.typeDecl;
				decl.declName() + " " + decl.fullName();
			},
			at(WObjCascadeBlock(_)) => throw "todo",
			at(WTypeCascade) => throw "todo"
		);
	}
}

@:publicFields abstract class Local {
	var name: String;
	var type: Null<Type>;
	var expr: Null<TExpr>;
}

class LocalVar extends Local {
	var def: TExpr;
	var defPaths:
		Array<
			Array<
				Either<
					TStmt,
					TExpr
				>
			>
		> = [];
	
	function new(def: TExpr) {
		this.def = def;
		switch def.e {
			case EVarDecl(_, type, value):
				this.type = type;
				this.expr = value;
				if(value != null) defPaths.push([Right(def)]);
			
			default: throw "error!";
		};
	}
}

class LocalParam extends Local {
	function new(name, type, expr) {
		this.name = name;
		this.type = type;
		this.expr = expr;
	}
}

class LocalField extends Local {
	var member: Member;
	function new(member, name, type, expr) {
		this.member = member;
		this.name = name;
		this.type = type;
		this.expr = expr;
	}
}


var STD_Value: TypeDecl;
var STD_Void: TypeDecl;
var STD_Int: Type;
var STD_Dec: Type;
var STD_Char: Type;
var STD_Bool: Type;
var STD_Str: Type;
var STD_Array: Type;
var STD_Dict: Type;
var STD_Tuple: Type;
var STD_Func: Type;
var STD_Iterable: Type;
var STD_Iterator: Type;

function initSTD(std: Project) {
	final t: Type = {t: TBlank};
	
	STD_Value = switch std.findType(List3.of([null, "Star", []], [null, "Value", []]), Inside, null).value().t {
		case TConcrete(decl) | TModular({t: TConcrete(decl)}, _): decl;
		default: throw "internal error: Star.Value should be a concrete type!";
	};
	STD_Void = switch std.findType(List3.of([null, "Star", []], [null, "Void", []]), Inside, null).value().t {
		case TConcrete(decl) | TModular({t: TConcrete(decl)}, _): decl;
		default: throw "internal error: Star.Value should be a concrete type!";
	};
	STD_Int = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Int", []]), Inside, null).value();
	STD_Dec = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Dec", []]), Inside, null).value();
	STD_Char = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Char", []]), Inside, null).value();
	STD_Bool = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Bool", []]), Inside, null).value();
	STD_Str = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Str", []]), Inside, null).value();
	STD_Array = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Array", [t]]), Inside, null).value();
	STD_Dict = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Dict", [t, t]]), Inside, null).value();
	STD_Tuple = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Tuple", []]), Inside, null).value();
	STD_Func = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Func", []]), Inside, null).value();
	STD_Iterable = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Iterable", []]), Inside, null).value();
	STD_Iterator = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Iterator", []]), Inside, null).value();
}


@:publicFields class Pass2 {
// dummy comment to fix formatting in vscode


static function resolveDir(dir: Dir) {
	for(f in dir.files) resolveFile(f);
	for(u in dir.units) resolveUnit(u);
}


static function resolveProject(proj: Project) {
	resolveDir(proj);
	proj.main.forEach(m -> resolveFile(m));
}


static function resolveUnit(unit: Unit) {
	unit.primary.forEach(p -> resolveFile(p));
	resolveDir(unit);
}


static function resolveFile(file: File) {
	for(decl in file.decls) resolveDecl({
		where: WDecl(decl),
		thisType: decl.thisType
	}, decl);
	for(cat in file.categories) resolveCategory({
		where: WCategory(cat),
		thisType: cat.thisType
	}, cat);
}

static function resolveDecl(ctx: Ctx, decl: TypeDecl) {
	//for(typevar in decl.typevars) resolveTypeVar(typevar);
	//decl.params.forEach(p -> resolveBasicType(decl, p));

	decl.buildRefinements();
	
	decl._match(
		at(ns is Namespace) => {
			for(decl2 in ns.decls) resolveDecl(ctx.innerDecl(decl2), decl2);
			for(cat in ns.categories) resolveCategory(ctx.innerCategory(cat), cat);
		},
		_ => {}
	);

	decl._match(
		at(ns is Namespace) => {
			for(m in ns.staticMembers) resolveMember(m);
			for(m in ns.staticMethods) resolveStaticMethod(ctx, m);
			ns.staticInit.forEach(i -> resolveEmptyMethod(ctx, i));
			ns.staticDeinit.forEach(i -> resolveEmptyMethod(ctx, i));

			ns._match(
				at({members: members, methods: methods, inits: inits, operators: ops} is Class
				| ({members: members, methods: methods, inits: inits, operators: ops} is Protocol)) => {
					for(m in members) resolveMember(m);
					for(m in methods) resolveMethod(ctx, m);
					for(i in inits) resolveInit(ctx, i);
					for(o in ops) resolveOperator(o);
				},
				at(tkind is TaggedKind) => {
					for(m in tkind.members) resolveMember(m);
					for(m in tkind.methods) resolveMethod(ctx, m);
					for(o in tkind.operators) resolveOperator(o);
					for(c in tkind.taggedCases) resolveTaggedCase(c);
				},
				at(vkind is ValueKind) => {
					for(m in vkind.methods) resolveMethod(ctx, m);
					for(o in vkind.operators) resolveOperator(o);
				},
				_ => {}
			);
		},
		at({staticMethods: smethods, methods: methods, operators: ops} is StrongAlias
		| ({staticMethods: smethods, methods: methods, operators: ops} is OpaqueAlias)) => {
			for(m in smethods) resolveStaticMethod(ctx, m);
			for(m in methods) resolveMethod(ctx, m);
			for(o in ops) resolveOperator(o);
		},
		_ => {}
	);

	//decl.refinements.forEach(r -> resolveDecl(r));
}


static function resolveCategory(ctx: Ctx, category: Category) {
	//category.friends.forEach(f -> resolveBasicType(category, f));
	//category.hidden.forEach(h -> h.forEach(t -> resolveBasicType(category, t)));

	//for(typevar in category.typevars) resolveTypeVar(typevar);
	
	//resolveBasicType(category, category.path);

	//category.type.forEach(t -> resolveBasicType(category, t));

	for(m in category.staticMembers) resolveMember(m);
	for(m in category.staticMethods) resolveStaticMethod(ctx, m);
	for(m in category.methods) resolveMethod(ctx, m);
	for(i in category.inits) resolveInit(ctx, i);
	for(o in category.operators) resolveOperator(o);
}


static function resolveTypeVar(typevar: TypeVar) {
	//typevar.params.forEach(p -> resolveBasicType(typevar.lookup, p));
	//typevar.parents.forEach(p -> resolveBasicType(typevar.lookup, p));
	
	/*typevar.native._match(
		at(Some(NPtr(t))) => resolveBasicType(typevar.lookup, t),
		_ => {}
	);*/

	//typevar.rule.forEach(r -> resolveTypeRule(typevar.lookup, r));
	
	//for(cat in typevar.categories) resolveCategory(cat);

	for(m in typevar.staticMembers) resolveMember(m);
	//for(m in typevar.staticMethods) resolveStaticMethod(m);
	for(m in typevar.members) resolveMember(m);
	//for(m in typevar.methods) resolveMethod(m);
	//for(i in typevar.inits) resolveInit(i);
	for(o in typevar.operators) resolveOperator(o);
	for(c in typevar.taggedCases) resolveTaggedCase(c);
}


/*static function resolveTypeRule(lookup: ILookupType, rule: TypeRule) rule._match(
	at(Negate(t) | Exists(t)) => resolveBasicType(lookup, t),
	at(Eq(l, r) | Of(l, r) | Lt(l, r) | Le(l, r)) => {
		resolveBasicType(lookup, l);
		resolveBasicType(lookup, r);
	},

	at(Not(r)) => resolveTypeRule(lookup, r),
	at(All(rs) | Any(rs) | One(rs)) => rs.forEach(r -> resolveTypeRule(lookup, r))
);*/


static function resolveEmptyMethod(ctx: Ctx, method: EmptyMethod) {
	final methodCtx = ctx.innerEmptyMethod(method);
	final bodyCtx = methodCtx.innerBlock();
	final tstmts = method.body.map(stmt -> typeStmt(bodyCtx, stmt));
	method.typedBody = tstmts;
}


static function resolveMethod(ctx: Ctx, method: Method) {
	if(method.isMacro) return;

	final methodCtx = ctx.innerMethod(method);

	method._match(
		at(multi is MultiMethod) => {
			for(param in multi.params) {
				final name = param.name.name;
				if(name == "_") {
					continue;
				} else if(methodCtx.locals.exists(name)) {
					throw 'duplicate param `$name` in ${methodCtx.description()}!';
				} else {
					methodCtx.locals[name] = new LocalParam(
						name,
						param.type,
						param.value._and(v =>
							assignType(methodCtx, typeExpr(methodCtx, v), param.type)
						)
					);
				}
			}
		},
		_ => {}
	);

	switch method.body {
		case None:
		case Some(body):
			final bodyCtx = methodCtx.innerBlock();
			final tstmts = body.map(stmt -> typeStmt(bodyCtx, stmt));
			method.typedBody = tstmts;
	}
}


static function resolveStaticMethod(ctx: Ctx, method: StaticMethod) {
	if(method.isMacro) return;

	final methodCtx = ctx.innerMethod(method);

	method._match(
		at(multi is MultiStaticMethod) => {
			for(param in multi.params) {
				final name = param.name.name;
				if(name == "_") {
					continue;
				} else if(methodCtx.locals.exists(name)) {
					throw 'duplicate param `$name` in ${methodCtx.description()}!';
				} else {
					methodCtx.locals[name] = new LocalParam(
						name,
						param.type,
						param.value._and(v =>
							assignType(methodCtx, typeExpr(methodCtx, v), param.type)
						)
					);
				}
			}
		},
		_ => {}
	);

	switch method.body {
		case None:
		case Some(body):
			final bodyCtx = methodCtx.innerBlock();
			final tstmts = body.map(stmt -> typeStmt(bodyCtx, stmt));
			method.typedBody = tstmts;
	}
}


static function resolveInit(ctx: Ctx, init: Init) {
	if(init.isMacro) return;

	final initCtx = ctx.innerMethod(init);

	init._match(
		at(multi is MultiInit) => {
			for(param in multi.params) {
				final name = param.name.name;
				if(name == "_") {
					continue;
				} else if(initCtx.locals.exists(name)) {
					throw 'duplicate param `$name` in ${initCtx.description()}!';
				} else {
					initCtx.locals[name] = new LocalParam(
						name,
						param.type,
						param.value._and(v =>
							assignType(initCtx, typeExpr(initCtx, v), param.type)
						)
					);
				}
			}
		},
		_ => {}
	);

	switch init.body {
		case None:
		case Some(body):
			final bodyCtx = initCtx.innerBlock();
			final tstmts = body.map(stmt -> typeStmt(bodyCtx, stmt));
			init.typedBody = tstmts;
	}
}


static function resolveOperator(op: Operator) {
	//op.hidden.forEach(h -> h.forEach(t -> resolveBasicType(op, t)));

	op._match(
		at(binop is BinaryOperator) => {
			for(typevar in binop.typevars) resolveTypeVar(typevar);
			//resolveBasicType(op, binop.paramType);
		},
		_ => {}
	);

	//op.ret.forEach(r -> resolveBasicType(op, r));
}


static function resolveMember(member: Member) {
	//member.hidden.forEach(h -> h.forEach(t -> resolveBasicType(member.lookup, t)));
	//member.type.forEach(t -> resolveBasicType(member.lookup, t));
}


static function resolveTaggedCase(tcase: TaggedCase) tcase._match(
	at({params: params} is MultiTaggedCase) => {
		//for(param in params) resolveBasicType(tcase.decl, param.type);
	},
	_ => {}
);


static function assignType(ctx: Ctx, expr: TExpr, type: Type): TExpr {
	expr.t = type;
	return expr;
}

static function typeExpr(ctx: Ctx, expr: UExpr): TExpr {
	final res: TExpr = switch expr {
		case EName(span, name):
			ctx.findLocal(name)._match(
				at(local!) => { e: EName(name, local), t: local.type },
				_ => throw 'field or variable `$name` does not exist! ${span.display()}'
			);
		
		case ETag(_, name, expr2):
			final texpr = typeExpr(ctx, expr2);
			{
				e: ETag(name, texpr),
				t: switch name {
					case "init_this": STD_Void.thisType;
					case "inline": texpr.t;
					default: null;
				}
			};

		case EInt(_, int, exp): { e: EInt(int, exp.toNull()), t: STD_Int };
		case EDec(_, int, dec, exp): { e: EDec(int, dec, exp.toNull()), t: STD_Dec };
		case EChar(_, char): { e: EChar(char), t: STD_Char };
		case EStr(_, parts): { e: EStr(parts.map(p -> switch p {
			case PCode(code): TExpr.StrPart.PCode(typeExpr(ctx, code));
			case PStr(str): TExpr.StrPart.PStr(str);
		})), t: STD_Str };
		case EBool(_, bool): { e: EBool(bool), t: STD_Bool };
		case EArray(_, values, _): { e: EArray(typeExprs(ctx, values)) };
		case EHash(_, pairs, _): { e: EHash(pairs.map(p -> new Tuple2(typeExpr(ctx, p.k), typeExpr(ctx, p.v)))) };
		case ETuple(_, values, _): { e: ETuple(typeExprs(ctx, values)) };
		case EThis(s):
			if(ctx.allowsThis()) { e: EThis, t: ctx.thisType};
			else throw 'error: `this` is not allowed in a static context ${s.display()}';
		case EWildcard(s):
			if(ctx.where.match(WPattern)) { e: EWildcard, t: {t: TBlank} }
			else throw 'error: wildcard is not allowed outside of a pattern ${s.display()}';
		//EFunc
		case EAnonArg(_, depth, nth): { e: EAnonArg(depth, nth) };
		case ELiteralCtor(type, literal):
			final t = ctx.getType(type);
			{ e: ELiteralCtor(t, typeExpr(ctx, literal)), t: t };
		
		case EParen(_, exprs, _): { e: EParen(typeExprs(ctx, exprs)) };
		case EBlock(blk): { e: EBlock(typeBlock(ctx, blk)) };

		case ETypeMessage(type, begin, msg, _):
			final t = ctx.getType(type);
			msg._match(
				at(Single(None, _, name)) => {
					t.findSingleStatic(name, ctx.typeDecl)._match(
						at(kind!) => {
							e: ETypeMessage(t, Single(kind)),
							t: switch kind {
								case SSInit(_) | SSTaggedCase(_) | SSTaggedCaseAlias(_) | SSValueCase(_): t;
								default: null;
							}
						},
						_ => throw 'error: type `${t.fullName()}` does not respond to method `[$name]`! ${begin.display()}'
					);
				},
				at(Single(Some(cat), _, name)) => throw "todo",
				
				at(Multi(None, labels)) => {
					detuple2(@var names, @var args, getNamesArgs(ctx, labels));
					
					t.findMultiStatic(names, ctx.typeDecl)._match(
						at([]) => throw 'error: type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]`! ${begin.display()}',
						at(kinds) => {
							e: ETypeMessage(t, Multi(kinds, names, args)),
							t: switch kinds {
								case [MSInit(_) | MSTaggedCase(_) | MSTaggedCaseAlias(_)]: t;
								case _ if(kinds.every(k -> k.match(MSInit(_) | MSTaggedCase(_) | MSTaggedCaseAlias(_)))): t;
								default: null;
							}
						}
					);
				},
				at(Multi(Some(cat), labels)) => {
					final tcat: Type = ctx.getType(cat)._match(
						at({t: TThis(source)}) => source._match(
							at(td is TypeDecl) => { t: TConcrete(td) },
							at(tv is TypeVar) => { t: TTypeVar(tv) },
							at(c is Category) => c.type.orElseDo(c.lookup._match(
								at(td is TypeDecl) => { t: TConcrete(td) },
								at(tv is TypeVar) => { t: TTypeVar(tv) },
								_ => throw "bad"
							)),
							_ => throw "bad"
						),
						at(c) => c
					);
					detuple2(@var names, @var args, getNamesArgs(ctx, labels));

					var categories = t.t._match(
						at(TThis(td is TypeDecl)) => td.thisType,
						_ => t
					).findThisCategory(tcat, ctx.typeDecl).unique();
					categories._match(
						at([]) => throw 'error: type `${t.fullName()}` does not have the category `${tcat.fullName()}`!',
						at([found]) => found.findMultiStatic(names, ctx.typeDecl)._match(
							at([]) => throw 'error: type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in category `${tcat.fullName()}`! ${begin.display()}',
							at(kinds) => {
								e: ETypeMessage(t, Multi(kinds, names, args)),
								t: null
							}
						),
						at(found) => Type.mostSpecificBy(
							Type.reduceOverloadsBy(
								found
									.map(f -> {cat: f, mth: f.findMultiStatic(names, ctx.typeDecl)})
									.filter(l -> l.mth.length != 0),
								f -> f.cat.thisType.getMostSpecific()
							),
							f -> f.mth[0]._match(
								at(MSMethod((_ : AnyMethod) => m, _) | MSInit(m, _)) => m.decl.thisType.getMostSpecific(),
								at(MSMember(m)) => (cast m.lookup : Traits.ITypeDecl).thisType.getMostSpecific(),
								_ => throw "bad"
							)
						)._match(
							at([kinds]) => {
								e: ETypeMessage(t, Multi(kinds.mth, names, args)),
								t: null
							},
							at([]) => throw 'error: type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${begin.display()}',
							at(kinds) => if(kinds.every(k -> k.mth.length == 0)) {
								throw 'error: type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${begin.display()}';
							} else {
								throw "todo ";
							}
						)
					);
				}
			);
		
		// ...
		
		case EObjMessage(obj, begin, msg, _):
			final tobj = typeExpr(ctx, obj);
			tobj.t._match(
				at(t!) => msg._match(
					at(Single(None, _, name)) => {
						t.findSingleInst(name, ctx.typeDecl)._match(
							at(kind!) => {
								e: EObjMessage(tobj, Single(kind)),
								t: kind._match(
									at(SIMethod({ret: Some(ret)})) => ret.t._match(
										at(TThis(source), when(source.hasChildType(t))) => t.t._match(
											at(TConcrete(decl)) => { t: TThis(decl) },
											at(TThis(source2)) => { t: TThis(source2) },
											at(TApplied({t: TConcrete(decl)}, args)) => t,
											_ => throw "todo (?) "+t.span._and(s=>s.display())
										),
										_ => null
									),
									_ => null
								)
							},
							_ => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]`!'
						);
					},
					at(Single(Some(cat = TSegs(Nil, Cons(NameParams(_, "Super", {of: [parent]}), Nil))), _, name)) => {
						final tparent: Type = ctx.getType(parent)._match(
							at({t: TMulti(types), span: s}) => switch Type.leastSpecific(types) {
								case []: throw "bad";
								case [p]: p;
								case ps: {t: TMulti(ps), span: s};
							},
							at(p) => p
						);

						if(tparent.hasChildType(t)) {
							tparent.findSingleInst(name, ctx.typeDecl)._match(
								at(kind!) => {
									e: EObjMessage(tobj, Super(tparent, Single(kind))),
									t: kind._match(
										at(SIMethod({ret: Some(ret)})) => ret.t._match(
											at(TThis(source), when(source.hasChildType(t))) => t.t._match(
												at(TConcrete(decl)) => { t: TThis(decl) },
												at(TThis(source2)) => { t: TThis(source2) },
												_ => throw "todo (?)"+t.span._and(s=>s.display())
											),
											_ => null
										),
										_ => null
									)
								},
								_ => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]`! ${cat.span().display()}'
							);
						} else {
							throw 'error: value of type `${t.fullName()}` does not have supertype `${tparent.fullName()}`! ${cat.span().display()}';
						}
					},
					at(Single(Some(cat), _, name)) => {
						final tcat: Type = ctx.getType(cat)._match(
							at({t: TThis(source)}) => source._match(
								at(td is TypeDecl) => { t: TConcrete(td) },
								at(tv is TypeVar) => { t: TTypeVar(tv) },
								at(c is Category) => c.type.orElseDo(c.lookup._match(
									at(td is TypeDecl) => { t: TConcrete(td) },
									at(tv is TypeVar) => { t: TTypeVar(tv) },
									_ => throw "bad"
								)),
								_ => throw "bad"
							),
							at(c) => c
						);
						var categories = t.t._match(
							at(TThis(td is TypeDecl)) => td.thisType,
							_ => t
						).findThisCategory(tcat, ctx.typeDecl).unique();
						categories._match(
							at([]) => throw 'error: value of type `${t.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
							at([found]) => found.findSingleInst(name, ctx.typeDecl)._match(
								at(kind!) => {
									e: EObjMessage(tobj, Single(kind)),
									t: null
								},
								_ => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in category `${tcat.fullName()}`! ${begin.display()}'
							),
							at(found) => found.filterMap(f -> f.findSingleInst(name, ctx.typeDecl))._match(
								at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`!',
								at([kind!]) => {
									e: EObjMessage(tobj, Single(kind)),
									t: null
								},
								at(kinds) => throw "todo"
							)
						);
					},
					
					at(Multi(None, labels)) => {
						detuple2(@var names, @var args, getNamesArgs(ctx, labels));

						t.findMultiInst(names, ctx.typeDecl)._match(
							at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]`! ${begin.display()}',
							at(kinds) => {
								e: EObjMessage(tobj, Multi(kinds, names, args)),
								t: null
							}
						);
					},
					at(Multi(Some(TSegs(Nil, Cons(NameParams(_, "Super", {of: [parent]}), Nil))), labels)) => {
						final tparent = ctx.getType(parent);

						if(tparent.hasChildType(t)) {
							detuple2(@var names, @var args, getNamesArgs(ctx, labels));

							tparent.findMultiInst(names, ctx.typeDecl)._match(
								at([]) => throw 'error: value of type `${t.fullName()}` does not have a supertype `${tparent.fullName()}` that responds to method `[${names.joinMap(" ", n -> '$n:')}]`! ${begin.display()}',
								at(kinds) => {
									e: EObjMessage(tobj, Super(tparent, Multi(kinds, names, args))),
									t: null
								}
							);
						} else {
							throw 'error: value of type `${t.fullName()}` does not have supertype `${tparent.fullName()}`!';
						}
					},
					at(Multi(Some(cat), labels)) => {
						final tcat: Type = ctx.getType(cat)._match(
							at({t: TThis(source)}) => source._match(
								at(td is TypeDecl) => { t: TConcrete(td) },
								at(tv is TypeVar) => { t: TTypeVar(tv) },
								at(c is Category) => c.type.orElseDo(c.lookup._match(
									at(td is TypeDecl) => { t: TConcrete(td) },
									at(tv is TypeVar) => { t: TTypeVar(tv) },
									_ => throw "bad"
								)),
								_ => throw "bad"
							),
							at(c) => c
						);
						detuple2(@var names, @var args, getNamesArgs(ctx, labels));

						var categories = t.findThisCategory(tcat, ctx.typeDecl).unique();
						categories._match(
							at([]) => throw 'error: value of type `${t.fullName()}` does not have the category `${tcat.fullName()}`! ${begin.display()}',
							at([found]) => found.findMultiInst(names, ctx.typeDecl)._match(
								at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in category `${tcat.fullName()}`! ${begin.display()}',
								at(kinds) => {
									e: EObjMessage(tobj, Multi(kinds, names, args)),
									t: null
								}
							),
							at(found) => Type.mostSpecificBy(
								Type.reduceOverloadsBy(
									found
										.map(f -> {cat: f, mth: f.findMultiInst(names, ctx.typeDecl)})
										.filter(l -> l.mth.length != 0),
									f -> f.cat.thisType.getMostSpecific()
								),
								f -> f.mth[0]._match(
									at(MIMethod(m, _)) => m.decl.thisType.getMostSpecific(),
									at(MIMember(m)) => (cast m.lookup : Traits.ITypeDecl).thisType.getMostSpecific()
								)
							)._match(
								at([kinds]) => {
									e: EObjMessage(tobj, Multi(kinds.mth, names, args)),
									t: null
								},
								at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${begin.display()}',
								at(kinds) => if(kinds.every(k -> k.mth.length == 0)) {
									throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${begin.display()}';
								} else {
									throw "todo ["+Type.mostSpecificBy(kinds, f->f.cat.thisType).map(f->f.cat.fullName()+"#"+f.mth).join(", ")+"]";
								}
							)
						);
					},

					at(Cast(None, type)) => {
						final target = ctx.getType(type).getLeastSpecific();
						t.findCast(target, ctx.typeDecl)._match(
							at([]) => {
								throw 'error: value of type `${t.fullName()}` cannot be cast to type `${target.fullName()}`! ${begin.display()}';
							},
							at(casts) => {
								e: EObjMessage(tobj, Cast(target, casts)),
								t: target
							}
						);
					},
					at(Cast(Some(cat = TSegs(Nil, Cons(NameParams(_, "Super", {of: [parent]}), Nil))), type)) => {
						final tparent: Type = ctx.getType(parent)._match(
							at({t: TMulti(types), span: s}) => switch Type.leastSpecific(types) {
								case []: throw "bad";
								case [p]: p;
								case ps: {t: TMulti(ps), span: s};
							},
							at(p) => p
						);

						if(tparent.hasChildType(t)) {
							final target = ctx.getType(type).getLeastSpecific();
							tparent.findCast(target, ctx.typeDecl)._match(
								at([]) => {
									throw 'error: value of type `${t.fullName()}` does not have a supertype `${tparent.fullName()}` that can be cast to type `${target.fullName()}`! ${begin.display()}';
								},
								at(casts) => {
									e: EObjMessage(tobj, Super(tparent, Cast(target, casts))),
									t: target
								}
							);
						} else {
							throw 'error: value of type `${t.fullName()}` does not have supertype `${tparent.fullName()}`! ${cat.span().display()}';
						}
					},
					at(Cast(Some(cat), type)) => {
						final target = ctx.getType(type).getLeastSpecific();
						final tcat: Type = ctx.getType(cat)._match(
							at({t: TThis(source)}) => source._match(
								at(td is TypeDecl) => { t: TConcrete(td) },
								at(tv is TypeVar) => { t: TTypeVar(tv) },
								at(c is Category) => c.type.orElseDo(c.lookup._match(
									at(td is TypeDecl) => { t: TConcrete(td) },
									at(tv is TypeVar) => { t: TTypeVar(tv) },
									_ => throw "bad"
								)),
								_ => throw "bad"
							),
							at(c) => c
						);
						var categories = t.t._match(
							at(TThis(td is TypeDecl)) => td.thisType,
							_ => t
						).findThisCategory(tcat, ctx.typeDecl).unique();
						categories._match(
							at([]) => throw 'error: value of type `${t.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
							at([found]) => found.findCast(target, ctx.typeDecl)._match(
								at([]) => {
									throw 'error: value of type `${t.fullName()}` cannot be cast to type `${target.fullName()}` in category `${found.fullName()}`! ${begin.display()}';
								},
								at(casts) => {
									e: EObjMessage(tobj, Cast(target, casts)),
									t: target
								}
							),
							at(found) => found.filterMap(f -> f.findCast(target, ctx.typeDecl))._match(
								at([]) => throw 'error: value of type `${t.fullName()}` cannot be cast to type `${target.fullName()}` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${begin.display()}',
								at([kind!]) => {
									e: EObjMessage(tobj, Cast(target, kind)),
									t: target
								},
								at(kinds) => throw "todo"
							)
						);
					}
				),
				_ => { e: EObjMessage(tobj, Lazy(typeMessage(ctx, msg))) }
			);
		
		case EObjCascade(obj, cascades):
			final tobj = typeExpr(ctx, obj);
			{
				e: EObjCascade(tobj, cascades.map(c -> typeObjCascade(ctx, tobj.t, c))),
				t: tobj.t
			};
		
		case EObjMember(EWildcard(_), {name: name}):
			ctx.findLocal(name, 1)._match(
				at(local!) => { e: EName(name, local), t: local.type },
				_ => throw 'error: field or variable `_.$name` does not exist!'
			);
		// TODO: _._.var, ...
		case EObjMember(obj, {span: s, name: name}):
			final tobj = typeExpr(ctx, obj);
			tobj.t._match(
				at(t!) => t.findSingleInst(name, ctx.typeDecl, true)._match(
					at(kind!) => {
						e: EObjMember(tobj, kind),
						t: null
					},
					_ => throw 'error: value of type ${t.fullName()} does not have member/getter `$name`! ${s.display()}'
				),
				_ => { e: EObjLazyMember(tobj, name) }
			);
		
		// ...

		// TEMP
		//case EPrefix(_, PNeg, EInt(_, int, exp)): { e: EInt(-int, exp.toNull()), t: STD_Int };
		case EPrefix(_, op, right):
			// TODO
			{ e: EPrefix(op, typeExpr(ctx, right)) };
		
		case ESuffix(left, _, op):
			// TODO
			{ e: ESuffix(typeExpr(ctx, left), op) };
		
		
		// hacky but eh
		case EInfix(EObjMessage(obj, begin, Multi(cat, labels), end), span, Assign(None), right):
			typeExpr(ctx, EObjMessage(obj, begin, Multi(cat, labels.concat([Named(span, "=", right)])), end));
		
		case EInfix(left, _, op, right):
			// TODO
			{ e: EInfix(typeExpr(ctx, left), op, typeExpr(ctx, right)) };

		// ...
		
		case EVarDecl(_, {name: name}, type, Some(EObjCascade(obj, cascades))):
			if(ctx.locals.exists(name)) trace('warning: shadowing local variable `$name`');
			
			final tobj = typeExpr(ctx, obj);
			final t = {
				final tt = type.toNull()._and(ty => ctx.getType(ty));
				tobj.t._match(
					at(ot!) => tt._match(
						at(et!) => et.strictUnifyWithType(ot)._match(
							at(ut!) => ut,
							_ => throw 'error: local variable declared to be of type `${et.fullName()}`, but provided value was of type `${ot.fullName()}` instead!'
						),
						_ => ot
					),
					_ => tt
				);
			};

			final tempCtx = ctx.innerBlock();
			tempCtx.locals[name] = new LocalVar({ e: EVarDecl(name, t, tobj) });
			
			final texpr: TExpr = {
				e: EObjCascade(tobj, cascades.map(c -> typeObjCascade(tempCtx, tobj.t, c))),
				t: tobj.t
			};
			final te: TExpr = { e: EVarDecl(name, t, texpr) };

			ctx.locals[name] = new LocalVar(te);
			te;

		case EVarDecl(_, {name: name}, type, expr):
			if(ctx.locals.exists(name)) trace('warning: shadowing local variable `$name`');
			final t = type.toNull()._and(ty => ctx.getType(ty));
			final te: TExpr = { e: EVarDecl(name, t, expr.toNull()._and(e => typeExpr(ctx, e))) };
			ctx.locals[name] = new LocalVar(te);
			te;
		
		// TEMP
		case EType(type):
			{ e: EPatternType(ctx.getType(type)), t: null };

		default: throw "todo! "+ctx.description()+" "+expr;
	};

	res.orig = expr;

	return res;
}

static function typeExprs(ctx: Ctx, exprs: Array<UExpr>): TExprs {
	return cast exprs.map(e -> typeExpr(ctx, e));
}

static function getNamesArgs(ctx: Ctx, labels: Array<parsing.ast.Message.Label>) {
	final names = new Array<String>();
	final args = new Array<TExpr>();

	for(i => l in labels) switch l {
		case Named(_, n, e):
			names[i] = n;
			args[i] = typeExpr(ctx, e);

		case Punned(s, n):
			names[i] = n;
			args[i] = typeExpr(ctx, EName(s, n));
		
		case Anon(e):
			names[i] = "_";
			args[i] = typeExpr(ctx, e);
	}

	return new Tuple2(names, args);
}

static function typeMessage(ctx: Ctx, msg: UMessage<UExpr>): Message<TExpr> {
	return switch msg {
		case Single(cat, _, name): Single(cat.toNull()._and(c => ctx.getType(c)), name);

		case Multi(cat, labels):
			detuple2(@var names, @var args, getNamesArgs(ctx, labels));
			Multi(cat.toNull()._and(c => ctx.getType(c)), names, args);

		case Cast(cat, type): Cast(cat.toNull()._and(c => ctx.getType(c)), ctx.getType(type));
	}
}

static function typeObjCascade(ctx: Ctx, type: Null<Type>, cascade: UCascade<UExpr>): ObjCascade {
	final cascadeCtx = ctx.innerCascade();
	return {
		ctx: ctx,
		t: null,
		depth: cascade.depth,
		kind: type._match(
			at(t!) => switch cascade.kind {
				case Member({name: name}): t.findSingleInst(name, ctx.typeDecl, true)._match(
					at(kind!) => Member(Single(kind)),
					_ => throw 'error: value of type `${t.fullName()}` does not have member/getter `$name`!'
				);

				case Message(msg): Message(msg._match(
					at(Single(None, _, name)) => t.findSingleInst(name, ctx.typeDecl)._match(
						at(kind!) => Single(kind),
						_ => throw 'error: value of type ${t.fullName()} does not respond to method `[$name]`!'
					),
					at(Single(Some(cat), _, name)) => {
						final tcat: Type = ctx.getType(cat)._match(
							at({t: TThis(source)}) => source._match(
								at(td is TypeDecl) => { t: TConcrete(td) },
								at(tv is TypeVar) => { t: TTypeVar(tv) },
								at(c is Category) => c.type.orElseDo(c.lookup._match(
									at(td is TypeDecl) => { t: TConcrete(td) },
									at(tv is TypeVar) => { t: TTypeVar(tv) },
									_ => throw "bad"
								)),
								_ => throw "bad"
							),
							at(c) => c
						);
						
						var categories = t.t._match(
							at(TThis(td is TypeDecl)) => td.thisType,
							_ => t
						).findThisCategory(tcat, ctx.typeDecl).unique();
						categories._match(
							at([]) => throw 'error: value of type `${t.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
							at([found]) => found.findSingleInst(name, ctx.typeDecl)._match(
								at(kind!) => Single(kind),
								_ => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in category `${tcat.fullName()}`!'
							),
							at(found) => found.filterMap(f -> f.findSingleInst(name, ctx.typeDecl))._match(
								at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`!',
								at([kind!]) => Single(kind),
								at(kinds) => throw "todo"
							)
						);
					},

					at(Multi(None, labels)) => {
						detuple2(@var names, @var args, getNamesArgs(ctx, labels));
						t.findMultiInst(names, ctx.typeDecl)._match(
							at([]) => throw 'error: type ${t.fullName()} does not respond to method `[${names.joinMap(" ", n -> '$n:')}]`!',
							at(kinds) => Multi(kinds, names, args)
						);
					},
					at(Multi(Some(cat), labels)) => {
						final tcat: Type = ctx.getType(cat)._match(
							at({t: TThis(source)}) => source._match(
								at(td is TypeDecl) => { t: TConcrete(td) },
								at(tv is TypeVar) => { t: TTypeVar(tv) },
								at(c is Category) => c.type.orElseDo(c.lookup._match(
									at(td is TypeDecl) => { t: TConcrete(td) },
									at(tv is TypeVar) => { t: TTypeVar(tv) },
									_ => throw "bad"
								)),
								_ => throw "bad"
							),
							at(c) => c
						);
						detuple2(@var names, @var args, getNamesArgs(ctx, labels));

						var categories = t.t._match(
							at(TThis(td is TypeDecl)) => td.thisType,
							_ => t
						).findThisCategory(tcat, ctx.typeDecl).unique();
						categories._match(
							at([]) => throw 'error: value of type `${t.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
							at([found]) => found.findMultiInst(names, ctx.typeDecl)._match(
								at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in category `${tcat.fullName()}`! ${cat.span().display()}',
								at(kinds) => Multi(kinds, names, args)
							),
							at(found) => Type.mostSpecificBy(
								found
									.map(f -> {cat: f, mth: f.findMultiInst(names, ctx.typeDecl)})
									.filter(l -> l.mth.length != 0),
								f -> f.cat.type.value()
							)._match(
								at([kinds]) => Multi(kinds.mth, names, args),
								at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${cat.span().display()}',
								at(kinds) => if(kinds.every(k -> k.mth.length == 0)) {
									throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${cat.span().display()}';
								} else {
									throw "todo ["+kinds.map(f->f.cat.fullName()+"#"+f.mth).join(", ")+"]";
								}
							)
						);
					},

					at(Cast(None, ty)) => {
						final target = ctx.getType(ty);
						t.findCast(target, ctx.typeDecl)._match(
							at([]) => throw 'error: value of type ${t.fullName()} cannot be cast to type `${target.fullName()}`!',
							at(casts) => Cast(target, casts)
						);
					},
					at(Cast(Some(cat), ty)) => throw "todo"
				));


				case AssignMember({name: name}, _, None, rhs): t.findMultiInst([name], ctx.typeDecl, true)._match(
					at(kinds!) => Member(Multi(kinds, [name], [typeExpr(ctx, rhs)])),
					_ => throw 'error: value of type `${t.fullName()}` does not have member/setter `$name`!'
				);
				case AssignMember({name: name}, _, Some(op), rhs): throw "todo";
				
				case AssignMessage(msg, _, None, rhs): throw "todo";
				case AssignMessage(msg, _, Some(op), rhs): throw "todo";


				case StepMember({name: name}, _, step): throw "todo!";

				case StepMessage(msg, _, step): throw "todo!";
				
				case Block(blk):
					final blkCtx = ctx.innerCascade(t);
					Block(blkCtx, typeBlock(blkCtx, blk));
			},
			_ => Lazy(switch cascade.kind {
				case Member(mem): Member(mem.name);
				case Message(msg): Message(typeMessage(ctx, msg));
				case AssignMember(mem, _, op, rhs): AssignMember(mem.name, op.toNull(), typeExpr(ctx, rhs));
				case AssignMessage(msg, _, op, rhs): AssignMessage(typeMessage(ctx, msg), op.toNull(), typeExpr(ctx, rhs));
				case StepMember(mem, _, step): StepMember(mem.name, step);
				case StepMessage(msg, _, step): StepMessage(typeMessage(ctx, msg), step);
				case Block(blk): Block(typeBlock(cascadeCtx, blk));
			})
		),
		nested: cascade.nested.map(c -> typeObjCascade(cascadeCtx, null, c))
	};
}


static function typeStmt(ctx: Ctx, stmt: UStmt): TStmt {
	var stmtLabel: Null<String> = null;
	final res: TStmt = {
		s: switch stmt {
			case SExpr(expr):
				SExpr(typeExpr(ctx, expr));
			
			case SIf(_, cond, thenBlk, elseBlk):
				SIf(typeExpr(ctx, cond), typeBlock(ctx, thenBlk), elseBlk._and(b => typeBlock(ctx, b._2)));
			
			case SCase(_, cases, otherwise, _):
				SCase(
					cases.map(c -> {
						cond: typeExpr(ctx, c.cond),
						then: typeThen(ctx, c.then)
					}),
					otherwise._and(o => typeThen(ctx, o._2))
				);
			
			case SMatch(_, value, _, cases, otherwise, _):
				final tvalue = typeExpr(ctx, value);
				SMatch(
					tvalue,
					cases.map(c -> {
						final patternCtx = ctx.innerPattern();
						{
							pattern: typeExpr(patternCtx, c.pattern),
							cond: c.when.toNull()._and(w => typeExpr(patternCtx, w._2)),
							then: typeThen(patternCtx.innerBlock(), c.then)
						};
					}),
					otherwise._and(o => typeThen(ctx.innerBlock(), o._2))
				);

			case SShortMatch(_, value, _, pattern, cond, thenBlk, elseBlk):
				final tvalue = typeExpr(ctx, value);
				final patternCtx = ctx.innerPattern();
				SMatchAt(
					tvalue,
					typeExpr(patternCtx, pattern),
					cond._and(c => typeExpr(patternCtx, c._2)),
					typeBlock(patternCtx, thenBlk),
					elseBlk._and(e => typeBlock(patternCtx, e._2))
				);

			case SWhile(_, cond, label, blk):
				SWhile(
					typeExpr(ctx, cond),
					stmtLabel = getLabel(ctx, label),
					typeBlock(ctx, blk)
				);
			
			case SDoWhile(_, label, blk, _, cond):
				SDoWhile(
					typeBlock(ctx, blk),
					stmtLabel = getLabel(ctx, label),
					typeExpr(ctx, cond)
				);
			
			case SForIn(_, lvar, lvar2, _, inExpr, cond, label, block):
				// TODO
				final forCtx = ctx.innerPattern();
				SForIn(
					typeExpr(forCtx, lvar),
					lvar2._and(l => typeExpr(forCtx, l)),
					typeExpr(ctx.innerBlock(), inExpr),
					cond._and(c => typeExpr(forCtx.innerBlock(), c._2)),
					label._and(l => l._2.name),
					typeBlock(forCtx.innerBlock(), block)
				);

			case SForRange(_, lvar, _, startK, startE, _, stopK, stopE, step, cond, label, block):
				final forCtx = ctx.innerBlock();
				final tlvar: Null<TExpr> = lvar._match(
					at(EWildcard(_)) => null,
					at(EName(_, _) | EVarDecl(_, _, _, _)
					| ETypeMember(_, _) | EObjMember(_, _)) => typeExpr(forCtx, lvar),
					_ => throw 'for loop pattern should ideally be some sort of variable'
				);
				final tstart = typeExpr(ctx.innerBlock(), startE);
				final tstop = typeExpr(ctx.innerBlock(), stopE);
				final tstep = step._and(s => typeExpr(forCtx, s._2));
				final tcond = cond._and(c => typeExpr(forCtx, c._2));
				
				final loopt: Null<Type> = Util._match([tstart.t, tstop.t],
					at([null, null]) => null,
					at([null, t!!]) => { tstart.t = t; t; },
					at([t!!, null]) => { tstop.t = t; t; },
					at([t1!!, t2!!]) => t1.strictUnifyWithType(t2)._match(
						at(t!) => t,
						_ => throw 'error: loop bounds of types `${t1.fullName()}` and `${t2.fullName()}` are not compatible!'
					)
				);

				Util._match([loopt, tlvar], at([lt!, tv!]) => {
					(tv: TExpr);
					tv.t._match(
						at(t!) => if(!t.hasChildType(lt)) {
							throw 'error: loop bound of type `${t.fullName()}` is not compatible with loop variable of type `${lt.fullName()}`';
						},
						_ => tv.e._match(
							at(EVarDecl(name, null, _)) => {
								forCtx.findLocal(name).type = loopt;
							},
							at(EVarDecl(name, _, _)) => {
								final local = forCtx.findLocal(name);
								if(!local.type.hasChildType(lt)) {
									throw 'error: loop bound of type `${local.type.fullName()}` is not compatible with loop variable of type `${lt.fullName()}`';
								}
							},
							_ => { tv.t = lt; }
						)
					);
				}, _ => {});
				
				SForRange(
					tlvar,
					new Tuple2(startK, tstart),
					new Tuple2(stopK, tstop),
					tstep,
					tcond,
					label._and(l => l._2.name),
					typeBlock(forCtx.innerBlock(), block)
				);

			case SDo(_, label, blk):
				SDo(
					stmtLabel = getLabel(ctx, label),
					typeBlock(ctx, blk)
				);
			
			case SReturn(_, value):
				SReturn(
					value.map(v -> typeExpr(ctx, v)).toNull()
				);
			
			case SBreak(_, None): SBreak(null);
			case SBreak(_, Some({_2: Left(depth)})): SBreak(Left(depth));
			case SBreak(_, Some({_2: Right(label)})):
				if(ctx.findLabel(label) != null) throw 'label `$label` does not exist!';
				SBreak(Right(label));
			
			case SNext(_, None): SNext(null);
			case SNext(_, Some({_2: Left(depth)})): SNext(Left(depth));
			case SNext(_, Some({_2: Right(label)})):
				if(ctx.findLabel(label) != null) throw 'label `$label` does not exist!';
				SNext(Right(label));
			
			case SThrow(span, value):
				SThrow(span, typeExpr(ctx, value));
			
			case STry(_, block, _, cases, otherwise, _):
				STry(
					typeBlock(ctx.innerBlock(), block),
					cases.map(c -> {
						final patternCtx = ctx.innerPattern();
						{
							pattern: typeExpr(patternCtx, c.pattern),
							cond: c.when.toNull()._and(w => typeExpr(patternCtx, w._2)),
							then: typeThen(patternCtx, c.then)
						};
					}),
					otherwise._and(o => typeThen(ctx.innerBlock(), o._2))
				);
			
			default: throw "todo! "+ctx.description()+" "+stmt;
		},
		orig: stmt
	};

	stmtLabel._and(l => {
		if(ctx.labels.exists(l)) trace('warning: shadowing label `$l`');
		ctx.labels[l] = res;
	});

	return res;
}

static function typeBlock(ctx: Ctx, blk: UBlock): TStmts {
	final newCtx = ctx.innerBlock();
	return cast blk.stmts.map(stmt -> typeStmt(newCtx, stmt));
}

static function typeThen(ctx: Ctx, then: parsing.ast.Stmt.Then): TStmts {
	final newCtx = ctx.innerBlock();
	return cast switch then {
		case ThenBlock(blk): blk.stmts.map(stmt -> typeStmt(newCtx, stmt));
		case ThenStmt(_, stmt): [typeStmt(newCtx, stmt)];
	}
}

static function getLabel(ctx: Ctx, label: Option<Tuple2<Span, Ident>>) {
	return switch label {
		case None: null;
		case Some({_2: {name: name}}): name;
	}
}


}