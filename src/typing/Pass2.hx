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

using typing.MultiStaticKind;
using typing.MultiInstKind;


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
	
	function new(ctx: Ctx, span: Span, def: TExpr) {
		this.ctx = ctx;
		this.def = def;
		switch def.e {
			case EVarDecl(name, type, value):
				this.span = span;
				this.name = name;
				this.type = type._or(value._and(v => v.t));
				this.expr = value;
				if(value != null) defPaths.push([Right(def)]);
			
			default: throw "error!";
		};
	}
}

class LocalParam extends Local {
	function new(ctx, span, name, type, expr) {
		this.ctx = ctx;
		this.span = span;
		this.name = name;
		this.type = type;
		this.expr = expr;
	}
}

class LocalField extends Local {
	var member: Member;
	function new(ctx: Ctx, member: Member, name, type: Null<Type>, expr) {
		this.ctx = ctx;
		this.member = member;
		this.span = member.span;
		this.name = name;
		this.type = type._or(member.type);
		this.expr = expr;
	}
}


var STD_Value: TypeDecl;
var STD_MultiKind: TypeDecl;
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
	final t: Type = {t: TBlank, span: null};
	
	STD_Value = std.findType(List3.of([null, "Star", []], [null, "Value", []]), Inside, null)._match(
		at({t: TConcrete(decl) | TModular({t: TConcrete(decl)}, _)}) => decl,
		_ => throw "internal error: Star.Value should be a concrete type!"
	);
	STD_MultiKind = std.findType(List3.of([null, "Star", []], [null, "MultiKind", []]), Inside, null)._match(
		at({t: TConcrete(decl) | TModular({t: TConcrete(decl)}, _)}) => decl,
		_ => throw "internal error: Star.MultiKind should be a concrete type!"
	);
	STD_Void = std.findType(List3.of([null, "Star", []], [null, "Void", []]), Inside, null)._match(
		at({t: TConcrete(decl) | TModular({t: TConcrete(decl)}, _)}) => decl,
		_ => throw "internal error: Star.Value should be a concrete type!"
	);
	STD_Int = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Int", []]), Inside, null).nonNull();
	STD_Dec = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Dec", []]), Inside, null).nonNull();
	STD_Char = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Char", []]), Inside, null).nonNull();
	STD_Bool = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Bool", []]), Inside, null).nonNull();
	STD_Str = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Str", []]), Inside, null).nonNull();
	STD_Array = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Array", []]), Inside, null).nonNull();
	STD_Dict = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Dict", []]), Inside, null).nonNull();
	STD_Tuple = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Tuple", []]), Inside, null).nonNull();
	STD_Func = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Func", []]), Inside, null).nonNull();
	STD_Iterable = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Iterable", []]), Inside, null).nonNull();
	STD_Iterator = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Iterator", []]), Inside, null).nonNull();
}


@:publicFields class Pass2 {
// dummy comment to fix formatting in vscode


static function buildDirRefinements(dir: Dir) {
	for(file in dir.files) buildFileRefinements(file);
	for(unit in dir.units) buildUnitRefinements(unit);
}

static function buildProjectRefinements(proj: Project) {
	buildDirRefinements(proj);
	proj.main.forEach(m -> buildFileRefinements(m));
}


static function buildUnitRefinements(unit: Unit) {
	unit.primary.forEach(p -> buildFileRefinements(p));
	buildDirRefinements(unit);
}

static function buildFileRefinements(file: File) {
	for(decl in file.decls) decl.buildRefinements();
}


static function resolveDir(dir: Dir) {
	for(f in dir.files) resolveFile(f);
	for(u in dir.units) resolveUnit(u);
}


static function resolveProject(proj: Project) {
	buildProjectRefinements(proj);
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
	//decl.buildRefinements();
	
	decl._match(
		at(ns is Namespace) => {
			for(i in 0...ns.parents.length) {
				ns.parents[i] = ns.parents[i].simplify();
			}
			for(decl2 in ns.decls) decl2.buildRefinements();
			for(decl2 in ns.decls) resolveDecl(ctx.innerDecl(decl2), decl2);
			for(cat in ns.categories) resolveCategory(ctx.innerCategory(cat), cat);
		},
		_ => {}
	);

	decl._match(
		at(ns is Namespace) => {
			for(m in ns.staticMembers) resolveMember(ctx, m);
			for(m in ns.staticMethods) resolveStaticMethod(ctx, m);
			ns.staticInit.forEach(i -> resolveEmptyMethod(ctx, i));
			ns.staticDeinit.forEach(i -> resolveEmptyMethod(ctx, i));

			ns._match(
				at({members: members, methods: methods, inits: inits, operators: ops} is Class
				| ({members: members, methods: methods, inits: inits, operators: ops} is Protocol)) => {
					for(m in members) resolveMember(ctx, m);
					for(m in methods) resolveMethod(ctx, m);
					for(i in inits) resolveInit(ctx, i);
					for(o in ops) resolveOperator(ctx, o);
				},
				at(tkind is TaggedKind) => {
					for(m in tkind.members) resolveMember(ctx, m);
					for(m in tkind.methods) resolveMethod(ctx, m);
					for(o in tkind.operators) resolveOperator(ctx, o);
					for(c in tkind.taggedCases) resolveTaggedCase(ctx, c);
				},
				at(vkind is ValueKind) => {
					for(c in vkind.valueCases) resolveValueCase(ctx, c);
					for(m in vkind.methods) resolveMethod(ctx, m);
					for(o in vkind.operators) resolveOperator(ctx, o);
				},
				_ => {}
			);
		},
		at(sa is StrongAlias) => {
			sa.type = sa.type.simplify();
			for(m in sa.staticMembers) resolveMember(ctx, m);
			for(m in sa.staticMethods) resolveStaticMethod(ctx, m);
			for(m in sa.members) resolveMember(ctx, m);
			for(m in sa.methods) resolveMethod(ctx, m);
			for(o in sa.operators) resolveOperator(ctx, o);
		},
		at(oa is OpaqueAlias) => {
			for(m in oa.staticMethods) resolveStaticMethod(ctx, m);
			for(m in oa.methods) resolveMethod(ctx, m);
			for(o in oa.operators) resolveOperator(ctx, o);
		},
		_ => {}
	);

	//decl.refinements.forEach(r -> resolveDecl(r));
}


static function resolveCategory(ctx: Ctx, category: Category) {
	for(m in category.staticMembers) resolveMember(ctx, m);
	for(m in category.staticMethods) resolveStaticMethod(ctx, m);
	for(m in category.methods) resolveMethod(ctx, m);
	for(i in category.inits) resolveInit(ctx, i);
	for(o in category.operators) resolveOperator(ctx, o);
}


static function resolveTypeVar(ctx: Ctx, typevar: TypeVar) {
	//typevar.params.forEach(p -> resolveBasicType(typevar.lookup, p));
	//typevar.parents.forEach(p -> resolveBasicType(typevar.lookup, p));
	
	/*typevar.native._match(
		at(Some(NPtr(t))) => resolveBasicType(typevar.lookup, t),
		_ => {}
	);*/

	//typevar.rule.forEach(r -> resolveTypeRule(typevar.lookup, r));
	
	//for(cat in typevar.categories) resolveCategory(cat);

	//for(m in typevar.staticMembers) resolveMember(m);
	//for(m in typevar.staticMethods) resolveStaticMethod(m);
	//for(m in typevar.members) resolveMember(m);
	//for(m in typevar.methods) resolveMethod(m);
	//for(i in typevar.inits) resolveInit(i);
	//for(o in typevar.operators) resolveOperator(o);
	for(c in typevar.valueCases) resolveValueCase(ctx, c);
	for(c in typevar.taggedCases) resolveTaggedCase(ctx, c);
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
				param.type = param.type.simplify();

				final span = param.name.span;
				final name = param.name.name;
				if(name == "_") {
					continue;
				} else methodCtx.locals[name]._match(
					at(local!) => {
						ctx.addError(Errors.duplicateParam(multi, name, local.span, span));
					},
					_ => {
						methodCtx.locals[name] = new LocalParam(
							methodCtx,
							span,
							name,
							param.type,
							param.value._and(v =>
								assignType(methodCtx, typeExpr(methodCtx, v), param.type)
							)
						);
					}
				);
			}
		},
		_ => {}
	);

	method.ret._and(ret => method.ret = ret.simplify());

	method.body._and(body => {
		final bodyCtx = methodCtx.innerBlock();
		final tstmts = body.map(stmt -> typeStmt(bodyCtx, stmt));
		method.typedBody = tstmts;
	});
}


static function resolveStaticMethod(ctx: Ctx, method: StaticMethod) {
	if(method.isMacro) return;

	final methodCtx = ctx.innerMethod(method);

	method._match(
		at(multi is MultiStaticMethod) => {
			for(param in multi.params) {
				param.type = param.type.simplify();

				final span = param.name.span;
				final name = param.name.name;
				if(name == "_") {
					continue;
				} else methodCtx.locals[name]._match(
					at(local!) => {
						ctx.addError(Errors.duplicateParam(multi, name, local.span, span));
					},
					_ => {
						methodCtx.locals[name] = new LocalParam(
							methodCtx,
							span,
							name,
							param.type,
							param.value._and(v =>
								assignType(methodCtx, typeExpr(methodCtx, v), param.type)
							)
						);
					}
				);
			}
		},
		_ => {}
	);

	method.ret._and(ret => method.ret = ret.simplify());

	method.body._and(body => {
		final bodyCtx = methodCtx.innerBlock();
		final tstmts = body.map(stmt -> typeStmt(bodyCtx, stmt));
		method.typedBody = tstmts;
	});
}


static function resolveInit(ctx: Ctx, init: Init) {
	if(init.isMacro) return;

	final initCtx = ctx.innerMethod(init);

	init._match(
		at(multi is MultiInit) => {
			for(param in multi.params) {
				param.type = param.type.simplify();
				
				final span = param.name.span;
				final name = param.name.name;
				if(name == "_") {
					continue;
				} else initCtx.locals[name]._match(
					at(local!) => {
						ctx.addError(Errors.duplicateParam(multi, name, local.span, span));
					},
					_ => {
						initCtx.locals[name] = new LocalParam(
							initCtx,
							span,
							name,
							param.type,
							param.value._and(v =>
								assignType(initCtx, typeExpr(initCtx, v), param.type)
							)
						);
					}
				);
			}
		},
		_ => {}
	);

	init.body._and(body => {
		final bodyCtx = initCtx.innerBlock();
		final tstmts = body.map(stmt -> typeStmt(bodyCtx, stmt));
		init.typedBody = tstmts;
	});
}


static function resolveOperator(ctx: Ctx, op: Operator) {
	if(op.isMacro) return;

	final opCtx = ctx.innerMethod(op);

	op._match(
		at(binop is BinaryOperator) => {
			binop.paramType = binop.paramType.simplify();

			final span = binop.paramName.span;
			final name = binop.paramName.name;

			opCtx.locals[name] = new LocalParam(
				opCtx,
				span,
				name,
				binop.paramType,
				null
			);
		},
		_ => {}
	);

	op.ret._and(ret => op.ret = ret.simplify());

	op.body._and(body => {
		final bodyCtx = opCtx.innerBlock();
		final tstmts = body.map(stmt -> typeStmt(bodyCtx, stmt));
		op.typedBody = tstmts;
	});
}


static function resolveMember(ctx: Ctx, member: Member) {
	member.type._match(
		at(null) => if(!member.isStatic) ctx.typeDecl._match(
			at(ns is Namespace) => {
				ns.parents.filterMap(p -> p.instMembers(ns).find(m -> m.name.name == member.name.name)._match(
					at(mem!) => {parent: p, mem: mem},
					_ => null
				))._match(
					at([]) => {},
					at([{parent: p, mem: m}]) => member.type = m.type._and(t => t.getFrom(p)),
					at(ms) => throw 'error: member `${member.name.name}` is ambiguous because it\'s shared by multiple parents!'
				);
			},
			_ => {}
		),
		at(type!!) => type.t = type.simplify().t
	);

	member.value._and(value => {
		final texpr = typeExpr(ctx.innerMember(member), value);
		if(member.type == null) {
			member.type = texpr.t;
		}
		member.typedValue = texpr;
	});
}


static function resolveValueCase(ctx: Ctx, vcase: ValueCase) {
	vcase.value.toNull()._and(value => {
		vcase.typedValue = typeExpr(ctx, value);
	});
}

static function resolveTaggedCase(ctx: Ctx, tcase: TaggedCase) {
	tcase._match(
		at({params: params} is MultiTaggedCase) => {
			for(param in params) {
				param.type = param.type.simplify();
			}
		},
		_ => {}
	);

	final caseCtx = ctx.innerTaggedCase(tcase);

	tcase.assoc.toNull()._and(assoc => {
		tcase.typedAssoc = typeTMessage(caseCtx, assoc);
	});

	tcase.init.toNull()._and(init => {
		final initCtx = caseCtx.innerBlock();
		tcase.typedInit = init.map(s -> typeStmt(initCtx, s));
	});
}


static function assignType(ctx: Ctx, expr: TExpr, type: Type): TExpr {
	expr.t = type;
	return expr;
}

static function invalidExpr(): TExpr {
	return { e: EInvalid, t: null };
}

static function typeExpr(ctx: Ctx, expr: UExpr): TExpr {
	final res: TExpr = expr._match(
		at(EName(span, name)) =>
			ctx.findLocal(name)._match(
				at(local!) => { e: EName(name, local), t: local.type },
				_ => {
					ctx.addError(Errors.unknownFieldOrVar(ctx, name, span));
					invalidExpr();
				}
			),
		
		at(ETag(s, name, expr2)) => {
			final texpr = typeExpr(ctx, expr2);
			switch name {
				case "init_this": {
					e: switch texpr.e {
						case ETypeMessage(type, msg): EInitThis(type, msg);
						default: throw 'error: invalid expression for `#init_this` tag! ${s.display()}';
					},
					t: STD_Void.thisType
				};
				case "inline": {
					e: EInline(texpr),
					t: texpr.t
				};
				case "asm": texpr;
				case "kind_id": {
					e: EKindId(texpr),
					t: null
				};
				case "kind_slot": {
					e: switch texpr.e {
						case ETuple([tobj, {e: EInt(i)}]): EKindSlot(tobj, i);
						default: throw 'error: invalid expression for `#kind_slot` tag! ${s.display()}';
					},
					t: null
				};
				default:
					trace('warning: unknown tag `#$name` ${s.display()}\n');
					{ e: ETag(name, texpr), t: null };
			}
		},

		at(EInt(_, int, exp)) => { e: EInt(int, exp.toNull()), t: STD_Int },
		at(EDec(_, int, dec, exp)) => { e: EDec(int, dec, exp.toNull()), t: STD_Dec },
		at(EChar(_, char)) => { e: EChar(char), t: STD_Char },
		at(EStr(_, parts)) => { e: EStr(parts.map(p -> switch p {
			case PCode(code): TExpr.StrPart.PCode(typeExpr(ctx, code));
			case PStr(str): TExpr.StrPart.PStr(str);
		})), t: STD_Str },
		at(EBool(_, bool)) => { e: EBool(bool), t: STD_Bool },
		at(EArray(_begin, values, _)) => {
			final tvalues = typeExprs(ctx, values);
			{
				e: EArray(tvalues),
				t: tvalues.filterMap(te -> te.t)._match(
					at([]) => STD_Array,
					at([t]) => {t: TApplied(STD_Array, [t]), span: _begin},
					at(ts) => {
						final et = ts.reduce((t1, t2) -> t1.strictUnifyWithType(t2)._match(
							at(t!) => t,
							_ => {
								//ctx.addError(Errors...)
								throw "todo";
							}
						));
						{t: TApplied(STD_Array, [et]), span: _begin};
					}
				)
			};
		},
		at(EHash(_, pairs, _)) => { e: EHash(pairs.map(p -> new Tuple2(typeExpr(ctx, p.k), typeExpr(ctx, p.v)))) },
		at(ETuple(_begin, values, _)) => {
			final tvalues = typeExprs(ctx, values);
			final ttypes = tvalues.filterMap(tv -> tv.t);
			{
				e: ETuple(tvalues),
				t: if(ttypes.length == tvalues.length) {
					{t: TApplied(STD_Tuple, ttypes), span: _begin};
				} else {
					null;
				}
			};
		},
		at(EThis(span)) => {
			if(ctx.allowsThis()) { e: EThis, t: {t: {
				final decl = ctx.typeDecl;
				if(decl.thisType.t != ctx.thisType.t) ctx.thisType.t
				else decl is Category ? decl.thisType.t : TThis(decl); // hacky thingy because categories are dumb
			}, span: span} };
			else {
				ctx.addError(Errors.thisNotAllowed(ctx, span));
				invalidExpr();
			}
		},
		at(EWildcard(s)) => {
			if(ctx.isPattern()) { e: EWildcard, t: null/*{t: TBlank}*/ }
			else throw 'error: wildcard is not allowed outside of a pattern ${s.display()}';
		},
		at(EFunc(begin, params, ret, body, end)) => {
			// TODO
			return invalidExpr();
		},
		at(EAnonArg(_, depth, nth)) => { e: EAnonArg(depth, nth) },
		at(ELiteralCtor(type, literal)) => {
			final t = ctx.getType(type)._or(return invalidExpr());
			{ e: ELiteralCtor(t, typeExpr(ctx, literal)), t: t };
		},
		
		at(EParen(_, exprs, _)) => {
			final texprs = typeExprs(ctx, exprs);
			{ e: EParen(texprs), t: texprs.last().t };
		},
		at(EBlock(blk)) => { e: EBlock(typeBlock(ctx, blk)) },

		at(ETypeMessage(type, begin, msg, _)) => {
			final t = ctx.getType(type)._or(return invalidExpr());
			msg._match(
				at(Single(None, span, name)) => {
					t.findSingleStatic(ctx, name, ctx.typeDecl)._match(
						at(kind!) => {
							e: ETypeMessage(t, Single(kind)),
							t: kind._match(
								at(SSInit(_) | SSTaggedCase(_) | SSTaggedCaseAlias(_) | SSValueCase(_)) => t,
								at(SSMethod({ret: ret!})) => ret.t._match(
									at(TThis(source), when(source.hasChildType(t))) => t.t._match(
										at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
										at(TThis(source2)) => { t: TThis(source2), span: ret.span },
										at(TApplied({t: TConcrete(decl)}, args)) => t,
										at(TTypeVar(_)) => throw "todo (?) "+t.span._and(s=>s.display()),
										_ => t
									),
									at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
									_ => ret
								),
								at(SSMember(mem)) => mem.type, // TODO: solve in ctx
								_ => null
							)
						},
						_ => {
							ctx.addError(Errors.unknownMethod(ctx, true, t, name, span));
							invalidExpr();
						}
					);
				},
				at(Single(Some(cat), _, name)) => throw "todo",
				
				at(Multi(None, labels)) => {
					detuple2(@var names, @var args, getNamesArgs(ctx, labels));
					
					t.findMultiStatic(ctx, names, ctx.typeDecl).unique().reduceBySender()._match(
						at([]) => {
							ctx.addError(Errors.unknownMethod(ctx, true, t, names, labels[0].span()));
							invalidExpr();
						},
						at(kinds) => {
							e: ETypeMessage(t, Multi(kinds, names, args)),
							t: kinds._match(
								at([MSInit(_) | MSMemberwiseInit(_) | MSTaggedCase(_) | MSTaggedCaseAlias(_)]) => t,
								at(_, when(kinds.every(k -> k.match(MSInit(_) | MSMemberwiseInit(_) | MSTaggedCase(_) | MSTaggedCaseAlias(_))))) => t,
								_ => kinds.filterMap(kind ->
									(kind._match(
										at(MSMethod({ret: ret!})) => ret.t._match(
											at(TThis(source), when(source.hasChildType(t))) => t.t._match(
												at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
												at(TThis(source2)) => { t: TThis(source2), span: ret.span },
												at(TApplied({t: TConcrete(decl)}, args)) => t,
												at(TTypeVar(_)) => throw "todo (?) "+t.span._and(s=>s.display()),
												_ => t
											),
											at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
											_ => ret
										),
										at(MSInit(_) | MSMemberwiseInit(_) | MSTaggedCase(_) | MSTaggedCaseAlias(_)) => t,
										_ => null
									) : Null<Type>)
								).unique()._match(
									at([]) => null,
									at([ret]) => ret,
									at(rets) => {t: TMulti(rets), span: null}
								)
							)
						}
					);
				},
				at(Multi(Some(cat), labels)) => {
					final tcat: Type = ctx.getType(cat)._or(return invalidExpr())._match(
						at({t: TThis(source), span: span}) => source._match(
							at(td is TypeDecl) => { t: TConcrete(td), span: span },
							at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
							at(c is Category) => c.type.orElseDo(c.lookup._match(
								at(td is TypeDecl) => { t: TConcrete(td), span: span },
								at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
								_ => throw "bad"
							)),
							_ => throw "bad"
						),
						at(c) => c
					);
					detuple2(@var names, @var args, getNamesArgs(ctx, labels));

					var categories = t.t._match(
						at(TThis(td is TypeDecl)) => ({t: td.thisType.t, span: t.span} : Type),
						_ => t
					).findThisCategory(ctx, tcat, ctx.typeDecl).unique();
					categories._match(
						at([]) => throw 'error: type `${t.fullName()}` does not have the category `${tcat.fullName()}`!',
						at([found]) => found.findMultiStatic(ctx, names, ctx.typeDecl).unique().reduceBySender()._match(
							at([]) => throw 'error: type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in category `${tcat.fullName()}`! ${begin.display()}',
							at(kinds) => {
								e: ETypeMessage(t, Multi(kinds, names, args)),
								t: kinds.filterMap(kind ->
									(kind._match(
										at(MSMethod({ret: ret!})) => ret.t._match(
											at(TThis(source), when(source.hasChildType(t))) => t.t._match(
												at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
												at(TThis(source2)) => { t: TThis(source2), span: ret.span },
												at(TApplied({t: TConcrete(decl)}, args)) => t,
												at(TTypeVar(_)) => throw "todo (?) "+t.span._and(s=>s.display()),
												_ => t
											),
											at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
											_ => ret
										),
										at(MSInit(_) | MSMemberwiseInit(_) | MSTaggedCase(_) | MSTaggedCaseAlias(_)) => t,
										_ => null
									) : Null<Type>)
								).unique()._match(
									at([]) => null,
									at([ret]) => ret,
									at(rets) => {t: TMulti(rets), span: null}
								)
							}
						),
						at(found) => Type.mostSpecificBy(
							Type.reduceOverloadsBy(
								found
									.map(f -> {cat: f, mth: f.findMultiStatic(ctx, names, ctx.typeDecl).unique().reduceBySender()})
									.filter(l -> l.mth.length != 0),
								f -> f.cat.thisType.getMostSpecific()
							),
							f -> f.mth[0]._match(
								at(MSMethod((_ : AnyMethod) => m, _) | MSInit(m, _)) => m.decl.thisType.getMostSpecific(),
								at(MSMember(m)) => m.decl.thisType.getMostSpecific(),
								_ => throw "bad"
							)
						)._match(
							at([kinds]) => {
								e: ETypeMessage(t, Multi(kinds.mth, names, args)),
								t: kinds.mth.filterMap(kind ->
									(kind._match(
										at(MSMethod({ret: ret!})) => ret.t._match(
											at(TThis(source), when(source.hasChildType(t))) => t.t._match(
												at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
												at(TThis(source2)) => { t: TThis(source2), span: ret.span },
												at(TApplied({t: TConcrete(decl)}, args)) => t,
												at(TTypeVar(_)) => throw "todo (?) "+t.span._and(s=>s.display()),
												_ => t
											),
											at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
											_ => ret
										),
										at(MSInit(_) | MSMemberwiseInit(_) | MSTaggedCase(_) | MSTaggedCaseAlias(_)) => t,
										_ => null
									) : Null<Type>)
								).unique()._match(
									at([]) => null,
									at([ret]) => ret,
									at(rets) => {t: TMulti(rets), span: null}
								)
							},
							at([]) => throw 'error: type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${begin.display()}',
							at(kinds) => if(kinds.every(k -> k.mth.length == 0)) {
								throw 'error: type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${begin.display()}';
							} else {
								throw "todo";
							}
						)
					);
				}
			);
		},
		
		at(ETypeMember(type, {name: name, span: s})) => {
			final t = ctx.getType(type)._or(return invalidExpr());
			t.findSingleStatic(ctx, name, ctx.typeDecl, true)._match(
				at(kind!) => {
					e: ETypeMember(t, kind),
					t: kind._match(
						at(SSMethod({ret: ret!})) => ret.t._match(
							at(TThis(source), when(source.hasChildType(t))) => t.t._match(
								at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
								at(TThis(source2)) => { t: TThis(source2), span: ret.span },
								at(TApplied({t: TConcrete(decl)}, args)) => t,
								at(TTypeVar(_)) => throw "todo (?) "+t.span._and(s=>s.display()),
								_ => t
							),
							at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
							_ => ret
						),
						at(SSMember(mem)) => mem.type, // TODO: solve in ctx
						at(SSTaggedCase(_) | SSTaggedCaseAlias(_) | SSValueCase(_)) => t,
						_ => null
					)
				},
				_ => {
					ctx.addError(Errors.unknownGetter(ctx, true, t, name, s));
					invalidExpr();
				}
			);
		},
		
		// ...
		
		at(EObjMessage(obj, begin, msg, end)) => {
			final tobj = typeExpr(ctx, obj);
			tobj.t._match(
				at(t!) => msg._match(
					at(Single(None, span, name)) => {
						t.t._match(
							at(TThis(td is TypeDecl)) => td.thisType,
							_ => t
						).findSingleInst(ctx, name, ctx.typeDecl)._match(
							at(kind!) => {
								e: EObjMessage(tobj, Single(kind)),
								t: (kind._match(
									at(SIMethod({ret: ret!})) => ret.t._match(
										at(TThis(source), when(source.hasChildType(t))) => t.t._match(
											at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
											at(TThis(source2)) => { t: TThis(source2), span: ret.span },
											at(TApplied({t: TConcrete(decl)}, args)) => t,
											at(TTypeVar(_)) => throw "todo (?) "+t.span._and(s=>s.display()),
											_ => t
										),
										at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
										_ => ret
									),
									at(SIMember(mem)) => mem.type, // TODO: solve in ctx
									_ => null
								) : Null<Type>)._and(ret => ret.getFrom(t))
							},
							_ => {
								ctx.addError(Errors.unknownMethod(ctx, false, t, name, span));
								invalidExpr();
							}
						);
					},
					at(Single(Some(cat = TSegs(Nil, Cons(NameParams(_, "Super", {of: [parent]}), Nil))), _, name)) => {
						final tparent: Type = ctx.getType(parent)._or(return invalidExpr())/*._match(
							at({t: TMulti(types), span: s}) => switch Type.leastSpecific(types) {
								case []: throw "bad";
								case [p]: p;
								case ps: {t: TMulti(ps), span: s};
							},
							at(p) => p
						)*/;
						//trace(tparent.fullName(), t.fullName(), tparent.hasChildType(t), t.hasParentType(tparent));
						//if(tparent.hasChildType(t)) {
						if(t.hasParentType(tparent)||tparent.hasChildType(t)) {
							/*tparent.t._match(
								at(TApplied({t: TConcrete(decl)}, _)) => trace(decl.fullName()),
								_ => {}
							);*/
							//trace(tparent.t);
							tparent.findSingleInst(ctx, name, ctx.typeDecl)._match(
								at(kind!) => {
									e: EObjMessage(tobj, Super(tparent, Single(kind))),
									t: (kind._match(
										at(SIMethod({ret: ret!})) => ret.t._match(
											at(TThis(source), when(source.hasChildType(t))) => t.t._match(
												at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
												at(TThis(source2)) => { t: TThis(source2), span: ret.span },
												at(TApplied({t: TConcrete(decl)}, args)) => t,
												at(TTypeVar(_)) => throw "todo (?) "+t.span._and(s=>s.display()),
												_ => t
											),
											at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
											_ => ret
										),
										at(SIMember(mem)) => mem.type, // TODO: solve in ctx
										_ => null
									) : Null<Type>)._and(ret => ret.getFrom(t))
								},
								_ => throw 'error: value of type `${t.fullName()}` does not have a supertype `${tparent.fullName()}` that responds to method `[$name]`! ${cat.span().display()}'
							);
						} else {
							throw 'error: value of type `${t.fullName()}` does not have supertype `${tparent.fullName()}`! ${cat.span().display()}';
						}
					},
					at(Single(Some(cat), _, name)) => {
						final tcat: Type = ctx.getType(cat)._or(return invalidExpr())._match(
							at({t: TThis(source), span: span}) => source._match(
								at(td is TypeDecl) => { t: TConcrete(td), span: span },
								at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
								at(c is Category) => c.type.orElseDo(c.lookup._match(
									at(td is TypeDecl) => { t: TConcrete(td), span: span },
									at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
									_ => throw "bad"
								)),
								_ => throw "bad"
							),
							at(c) => c
						);
						var categories = t.t._match(
							at(TThis(td is TypeDecl)) => ({t: td.thisType.t, span: t.span} : Type),
							_ => t
						).findThisCategory(ctx, tcat, ctx.typeDecl).concat(
							tcat.findCategory(ctx, tcat, t, ctx.typeDecl)
						).unique();
						categories._match(
							at([]) => {
								ctx.addError(Errors.unknownCategory(ctx, false, t, tcat, cat.span()));
								invalidExpr();
							}, //throw 'error: value of type `${t.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
							at([found]) => found.findSingleInst(ctx, name, ctx.typeDecl)._match(
								at(kind!) => {
									e: EObjMessage(tobj, Single(kind)),
									t: (kind._match(
										at(SIMethod({ret: ret!})) => ret.t._match(
											at(TThis(source), when(source.hasChildType(t))) => t.t._match(
												at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
												at(TThis(source2)) => { t: TThis(source2), span: ret.span },
												at(TApplied({t: TConcrete(decl)}, args)) => t,
												at(TTypeVar(_)) => throw "todo (?) "+t.span._and(s=>s.display()),
												_ => t
											),
											at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
											_ => ret
										),
										at(SIMember(mem)) => mem.type, // TODO: solve in ctx
										_ => null
									) : Null<Type>)._and(ret => ret.getFrom(t))
								},
								_ => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in category `${tcat.fullName()}`! ${begin.display()}'
							),
							at(found) => found.filterMap(f -> f.findSingleInst(ctx, name, ctx.typeDecl))._match(
								at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`!',
								at([kind!]) => {
									e: EObjMessage(tobj, Single(kind)),
									t: (kind._match(
										at(SIMethod({ret: ret!})) => ret.t._match(
											at(TThis(source), when(source.hasChildType(t))) => t.t._match(
												at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
												at(TThis(source2)) => { t: TThis(source2), span: ret.span },
												at(TApplied({t: TConcrete(decl)}, args)) => t,
												at(TTypeVar(_)) => throw "todo (?) "+t.span._and(s=>s.display()),
												_ => t
											),
											at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
											_ => ret
										),
										at(SIMember(mem)) => mem.type, // TODO: solve in ctx
										_ => null
									) : Null<Type>)._and(ret => ret.getFrom(t))
								},
								at(kinds) => throw "todo"
							)
						);
					},
					
					at(Multi(None, labels)) => {
						detuple2(@var names, @var args, getNamesArgs(ctx, labels));

						t.t._match(
							at(TThis(td is TypeDecl)) => ({t: td.thisType.t, span: t.span} : Type),
							_ => t
						).findMultiInst(ctx, names, ctx.typeDecl)._match(
							at([]) => {
								ctx.addError(Errors.unknownMethod(ctx, false, t, names, labels[0].span()));
								invalidExpr();
							},
							at(kinds) => kinds.reduceOverloads(t, args)._match(
								at([]) => {
									// TODO: replace with custom error w/ typed sig
									ctx.addError(Errors.unknownMethod(ctx, false, t, names, args, labels[0].span()));
									invalidExpr();
								},
								at(overloads) => {
									e: EObjMessage(tobj, Multi(overloads.map(ov -> ov.kind), names, args)),
									t: overloads.map(ov -> ov.ret).unique()._match(
										at([]) => null,
										at([ret]) => ret,
										at(rets) => {t: TMulti(rets), span: null}
									)
								}
							)/*{
								e: EObjMessage(tobj, Multi(kinds, names, args)),
								t: kinds.filterMap(kind ->
									(kind._match(
										at(MIMethod({ret: ret!})) => ret.t._match(
											at(TThis(source), when(source.hasChildType(t))) => t.t._match(
												at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
												at(TThis(source2)) => { t: TThis(source2), span: ret.span },
												at(TApplied({t: TConcrete(decl)}, args)) => t,
												at(TTypeVar(_)) => throw "todo (?) "+t.span._and(s=>s.display()),
												_ => t
											),
											at(TApplied(_, _) | TTypeVar(_)) => ret.getFrom(t),
											_ => ret
										),
										_ => null
									) : Null<Type>)._and(ret => ret.getFrom(t))
								).unique()._match(
									at([]) => null,
									at([ret]) => ret,
									at(rets) => {t: TMulti(rets), span: null}
								)
							}*/
						);
					},
					at(Multi(Some(TSegs(Nil, Cons(NameParams(_, "Super", {of: [parent]}), Nil))), labels)) => {
						final tparent = ctx.getType(parent)._or(return invalidExpr()).simplify();

						if(tparent.hasChildType(t)) {
							detuple2(@var names, @var args, getNamesArgs(ctx, labels));

							tparent.findMultiInst(ctx, names, ctx.typeDecl).reduceBySender()._match(
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
						final tcat: Type = ctx.getType(cat)._or(return invalidExpr())._match(
							at({t: TThis(source), span: s}) => source._match(
								at(td is TypeDecl) => { t: TConcrete(td), span: s },
								at(tv is TypeVar) => { t: TTypeVar(tv), span: s },
								at(c is Category) => c.type.orElseDo(c.lookup._match(
									at(td is TypeDecl) => { t: TConcrete(td), span: s },
									at(tv is TypeVar) => { t: TTypeVar(tv), span: s },
									_ => throw "bad"
								)),
								_ => throw "bad"
							),
							at(c) => c
						);
						detuple2(@var names, @var args, getNamesArgs(ctx, labels));

						var categories = t.findThisCategory(ctx, tcat, ctx.typeDecl).concat(
							tcat.findCategory(ctx, tcat, t, ctx.typeDecl)
						).unique().filter(c -> {
							if(t.hasParentType(c.thisType) && c.thisType.hasChildType(t)) {
								true;
							} else {
								//trace(t.fullName(), c.fullName(), t.hasParentType(c.thisType), c.thisType.hasChildType(t));
								t.hasParentType(c.thisType);
							}
						}); // BUG: hasParentType has false positives?!?!!??!?!?!!?!?! probably related to refinements
						categories._match(
							at([]) => {
								ctx.addError(Errors.unknownCategory(ctx, false, t, tcat, cat.span()));
								invalidExpr();
							},
							at([found]) => found.findMultiInst(ctx, names, ctx.typeDecl).reduceBySender()._match(
								at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in category `${tcat.fullName()}`! ${begin.display()}',
								at(kinds) => {
									e: EObjMessage(tobj, Multi(kinds, names, args)),
									t: null
								}
							),
							_ => Type.mostSpecificBy(
								Type.reduceOverloadsBy(
									categories
										.map(f -> {cat: f, mth: f.findMultiInst(ctx, names, ctx.typeDecl).reduceBySender()})
										.filter(l -> l.mth.length != 0),
									f -> f.cat.thisType.getMostSpecific()
								),
								f -> f.mth[0]._match(
									at(MIMethod(m, _)) => m.decl.thisType.getMostSpecific(),
									at(MIMember(m)) => m.decl.thisType.getMostSpecific(),
									at(MIFromTypevar(tv, _, _, _)) => tv.thisType.getMostSpecific()
								)
							)._match(
								at([]) => {
									ctx.addError(Errors.unknownMethod(ctx, false, t, names, begin, categories));
									invalidExpr();
								},
								at([kinds]) => {
									e: EObjMessage(tobj, Multi(kinds.mth, names, args)),
									t: null
								},
								at(kinds) => if(kinds.every(k -> k.mth.length == 0)) {
									throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${categories.map(f -> f.fullName()).join(", ")}`! ${begin.display()}';
								} else {
									throw "todo ["+Type.mostSpecificBy(kinds, f->f.cat.thisType).map(f->f.cat.fullName()+"#"+f.mth).join(", ")+"]";
								}
							)
						);
					},

					at(Cast(None, type)) => {
						final target = ctx.getType(type)._or(return invalidExpr()).getLeastSpecific();
						t.findCast(ctx, target, ctx.typeDecl)._match(
							at([]) => {
								//throw 'error: value of type `${t.fullName()}` cannot be cast to type `${target.fullName()}`! ${begin.display()}';
								ctx.addError(Errors.unknownCast(ctx, t, target, begin.union(end)));
								invalidExpr();
							},
							at(casts) => {
								e: EObjMessage(tobj, Cast(target, casts.unique())),
								t: target
							}
						);
					},
					at(Cast(Some(cat = TSegs(Nil, Cons(NameParams(_, "Super", {of: [parent]}), Nil))), type)) => {
						final tparent: Type = ctx.getType(parent)._or(return invalidExpr()).simplify();

						if(tparent.hasChildType(t)) {
							final target = ctx.getType(type)._or(return invalidExpr()).getLeastSpecific();
							tparent.findCast(ctx, target, ctx.typeDecl)._match(
								at([]) => {
									throw 'error: value of type `${t.fullName()}` does not have a supertype `${tparent.fullName()}` that can be cast to type `${target.fullName()}`! ${begin.display()}';
								},
								at(casts) => {
									e: EObjMessage(tobj, Super(tparent, Cast(target, casts.unique()))),
									t: target
								}
							);
						} else {
							throw 'error: value of type `${t.fullName()}` does not have supertype `${tparent.fullName()}`! ${cat.span().display()}';
						}
					},
					at(Cast(Some(cat), type)) => {
						final target = ctx.getType(type)._or(return invalidExpr()).getLeastSpecific();
						final tcat: Type = ctx.getType(cat)._or(return invalidExpr())._match(
							at({t: TThis(source), span: span}) => source._match(
								at(td is TypeDecl) => { t: TConcrete(td), span: span },
								at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
								at(c is Category) => c.type.orElseDo(c.lookup._match(
									at(td is TypeDecl) => { t: TConcrete(td), span: span },
									at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
									_ => throw "bad"
								)),
								_ => throw "bad"
							),
							at(c) => c
						);
						var categories = t.t._match(
							at(TThis(td is TypeDecl)) => td.thisType,
							_ => t
						).findThisCategory(ctx, tcat, ctx.typeDecl).concat(
							tcat.findCategory(ctx, tcat, t, ctx.typeDecl)
						).unique();
						categories._match(
							at([]) => throw 'error: value of type `${t.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
							at([found]) => found.findCast(ctx, target, ctx.typeDecl)._match(
								at([]) => {
									throw 'error: value of type `${t.fullName()}` cannot be cast to type `${target.fullName()}` in category `${found.fullName()}`! ${begin.display()}';
								},
								at(casts) => {
									e: EObjMessage(tobj, Cast(target, casts)),
									t: target
								}
							),
							at(found) => found
								.filterMap(f -> f.findCast(ctx, target, ctx.typeDecl).unique())
								.filter(c -> c.length != 0)
							._match(
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
		},
		
		at(EObjCascade(obj, cascades)) => {
			final tobj = typeExpr(ctx, obj);
			{
				e: EObjCascade(tobj, cascades.filterMap(c -> typeObjCascade(ctx, tobj.t, c))),
				t: tobj.t
			};
		},
		
		at(EObjMember(EWildcard(span1), {name: name, span: span2})) =>
			ctx.findLocal(name, 1)._match(
				at(local!) => { e: EName(name, local), t: local.type },
				_ => {
					ctx.addError(Errors.unknownFieldOrVar(ctx, '_.$name', span1.union(span2)));
					invalidExpr();
				}
			),
		// TODO: _._.var, ...
		at(EObjMember(obj, {span: s, name: name})) => {
			final tobj = typeExpr(ctx, obj);
			tobj.t._match(
				at(t!) => {
					t = t.getIn(ctx);
					t.findSingleInst(ctx, name, ctx.typeDecl, true)._match(
						at(kind!) => {
							e: EObjMember(tobj, kind),
							t: kind._match(
								at(SIMethod(m)) => m.ret.nonNull(),
								at(SIMultiMethod(m)) => m.ret.nonNull(),
								at(SIMember(m)) => m.type,
								at(SIFromTypevar(_, _, _, _)) => null
							)._and(ret => ret.getFrom(t).getIn(ctx))
						},
						_ => {
							ctx.addError(Errors.unknownGetter(ctx, false, t, name, s));
							invalidExpr();
						}
					);
				},
				_ => { e: EObjLazyMember(tobj, name) }
			);
		},
		
		//case EPrefix(_, PNeg, EInt(_, int, exp)): { e: EInt(-int, exp.toNull()), t: STD_Int };
		at(EPrefix(span, op, right)) => {
			final rhs = typeExpr(ctx, right);
			rhs.t._match(
				at(t!) => {
					t = t.getIn(ctx);
					final op2: UnaryOp = op._match(
						at(PIncr) => Incr,
						at(PDecr) => Decr,
						at(PNeg) => Neg,
						at(PNot) => Not,
						at(PCompl) => Compl,
						at(PSpread) => if(ctx.isPattern()) {
							return { e: ELazyPrefix(op, rhs) };
						} else {
							throw "todo!";
						}
					);
					
					t.findUnaryOp(ctx, op2, ctx.typeDecl)._match(
						at(kind!) => {
							e: EPrefix(kind, rhs),
							t: kind.digForMethod().ret.nonNull().simplify().getFrom(t)//.getIn(ctx)
						},
						_ => if(ctx.isPattern()) {
							{ e: ELazyPrefix(op, rhs) };
						} else {
							ctx.addError(Errors.unknownMethod(ctx, t, op2, span));
							invalidExpr();
						}
					);
				},
				_ => { e: ELazyPrefix(op, rhs) }
			);
		},
		
		at(ESuffix(left, span, op)) => {
			final lhs = typeExpr(ctx, left);
			lhs.t._match(
				at(t!) => {
					final op2: UnaryOp = op._match(
						at(SIncr) => Incr,
						at(SDecr) => Decr,
						at(STruthy) => if(ctx.isPattern()) {
							return { e: ELazySuffix(lhs, op) };
						} else {
							Truthy;
						}
					);
					
					t.findUnaryOp(ctx, op2, ctx.typeDecl)._match(
						at(kind!) => {
							e: ESuffix(lhs, kind),
							t: kind.digForMethod().ret.nonNull().simplify().getFrom(t)
						},
						_ => if(ctx.isPattern()) {
							{ e: ELazySuffix(lhs, op) };
						} else {
							ctx.addError(Errors.unknownMethod(ctx, t, op2, span));
							invalidExpr();
						}
					);
				},
				_ => { e: ELazySuffix(lhs, op) }
			);
		},
		
		
		// TODO: make this better
		
		at(EInfix(EObjMessage(obj, begin, msg, end), span, Assign(None), right)) =>
			msg._match(
				at(Single(cat, span2, name)) =>
					typeExpr(ctx, EObjMessage(obj, begin, Multi(cat, [Named(Span.range(span, span2), name, right)]), end)),

				// hacky but eh
				at(Multi(cat, labels)) =>
					typeExpr(ctx, EObjMessage(obj, begin, Multi(cat, labels.concat([Named(span, "=", right)])), end)),
				
				at(Cast(_, _)) => throw "bad"
			),
		
		at(EInfix(left = EObjMessage(obj, begin, msg, end), span, Assign(Some(op)), right)) =>
			typeExpr(ctx, EObjMessage(
				obj,
				begin,
				msg._match(
					at(Single(cat, span2, name)) =>
						Multi(cat, [Named(span2, name, EInfix(left, span, op, right))]),
					
					at(Multi(cat, labels)) =>
						Multi(cat, labels.concat([Named(span, "=", EInfix(left, span, op, right))])),
					
					at(Cast(_, _)) => throw "bad"
				),
				end
			)),
		
		//case EInfix(left = EObjMember(obj, {span: span1, name: name}), span2, Assign(None), right):

		// TODO: bad
		at(EInfix(left = EName(span1, name), span2, Assign(assign), right)) =>
			ctx.findLocal(name)._match(
				at(local!) => {
					local._match(
						at({member: {isReadonly: true}} is LocalField) => {
							throw 'error: field `$name` is readonly and cannot be assigned! ${span1.display()}';
						},
						_ => {}
					);

					{
						e: assign._match(
							at(None) => {
								final tvalue = typeExpr(ctx, right);
								//trace(local.name, local.type._and(t => t.fullName()), tvalue.t._and(t => t.fullName()), span2.display());
								local._match(
									at(lvar is LocalVar) => {
										//if(local.expr == null) local.expr = tvalue;
										tvalue.t._and(rt => {
											rt = rt.simplify();
											lvar.type._andOr(lt => {
												lt = lt.simplify().getFrom(ctx.thisType);
												lt.strictUnifyWithType(rt)._match(
													at(t!) => {
														//trace(t);
													},
													_ => if(!ctx.isPattern()) {
														ctx.addError(Errors.localVarTypeMismatch(ctx, name, lt, rt, lvar.span, span1));
														return invalidExpr();
													}
												);
											}, {
												lvar.type = rt;
											});
										});
									},
									_ => {}
								);
								ESetName(name, local, tvalue);
							},
							at(Some(op)) => {
								final tvalue = typeExpr(ctx, EInfix(left, span2, op, right));
								if(local is LocalVar && local.expr == null) {
									throw 'error: variable `$name` is used before being assigned! ${span1.display()}';
								}
								ESetName(name, local, tvalue);
							}
						),
						t: local.type
					};
				},
				_ => {
					ctx.addError(Errors.unknownFieldOrVar(ctx, name, span1));
					invalidExpr();
				}
			),

		
		at(EInfix(left, span, op=Assign(_), right)) => {
			final tleft = typeExpr(ctx, left);
			final tright = typeExpr(ctx, right);
			{ e: ELazyInfix(tleft, op, tright) };
		},
		
		at(EInfix(left1, span1, op1 = (TExpr.Infix.Eq ... Le),
			second=EInfix(left2, span2, op2 = (TExpr.Infix.Eq ... Le), right))
		) => {
			// TODO: make this better
			typeExpr(ctx,
				EInfix(
					EInfix(left1, span1, op1, left2),
					span1,
					And,
					second
				)
			);
		},
		
		at(EInfix(left, span, op, right)) => {
			final tleft = typeExpr(ctx, left);
			final tright = typeExpr(ctx, right);
			final op2 = BinaryOp.fromInfix(op);

			tleft.t._match(
				at(lt!, when(!ctx.isPattern())) => {
					lt = lt.getIn(ctx);
					var found = lt.findBinaryOp(ctx, op2, ctx.typeDecl);
					final oldFound = found;
					final rt = tright.t._match(
						at(rt0!) => {
							rt0 = rt0.getIn(ctx);
							found = found.filter(k -> k.digForMethod()
								.paramType.getIn(ctx).getFrom(lt).hasChildType(rt0) // TODO: change to use hasStrictChildType when it's finished
							);
							found = Type.reduceOverloadsBy(found, k -> k.digForMethod()
								.paramType.getIn(ctx).getFrom(lt) // TODO: change to use hasStrictChildType when it's finished
							);
							rt0;
						},
						_ => null
					);
					found._match(
						at([]) => {
							(oldFound.some(k -> k.match(BOMethod(_) | BOFromTypevar(_, _, BOMethod(_)))) ? rt : null)._match(
								at(rt0!) => ctx.addError(Errors.unknownMethod(ctx, lt, rt0, op2, span)),
								_ => ctx.addError(Errors.unknownMethod(ctx, lt, op2, span))
							);
							invalidExpr();
						},
						at(kinds) => {
							e: EInfix(tleft, kinds, tright),
							t: kinds.filterMap(kind ->
								(kind.digForMethod()._match(
									at({ret: ret!}) => ret.t._match(
										at(TThis(source), when(source.hasChildType(lt))) => lt.t._match(
											at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
											at(TThis(source2)) => { t: TThis(source2), span: ret.span },
											at(TApplied({t: TConcrete(decl)}, args)) => lt,
											//at(TTypeVar(_)) => throw "todo (?) "+lt.span._and(s=>s.display()),
											_ => lt
										),
										at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
										_ => ret
									),
									_ => null // TODO
								) : Null<Type>)._and(ret => ret.getFrom(lt))
							).unique()._match(
								at([]) => null,
								at([ret]) => ret,
								at(rets) => {t: TMulti(rets), span: null}
							)
						}
					);
				},
				_ => { e: ELazyInfix(tleft, op, tright) }
			);
		},

		// ...
		
		at(EVarDecl(_, {name: name, span: span}, type, Some(EObjCascade(obj, cascades)))) => {
			ctx.findLocal(name)._and(local =>
				if(!(local is LocalField || (ctx.isPattern() && local.ctx.isPattern() && ctx == local.ctx))) {
					ctx.addError(Errors.shadowedLocalVar(ctx, name, local.span, span));
				}
			);
			
			final tobj = typeExpr(ctx, obj);
			final t = {
				final tt = type.toNull()._and(ty => ctx.getType(ty)._or(return invalidExpr()));
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
			tempCtx.locals[name] = new LocalVar(tempCtx, span, { e: EVarDecl(name, t, tobj), t: t });
			
			final texpr: TExpr = {
				e: EObjCascade(tobj, cascades.filterMap(c -> typeObjCascade(tempCtx, tobj.t, c))),
				t: tobj.t
			};
			final te: TExpr = { e: EVarDecl(name, t, texpr), t: t };

			final local = new LocalVar(ctx, span, te);
			ctx.locals[name] = local;
			if(te.t == null) te.t = local.type;
			te;
		},

		at(EVarDecl(_, {name: name, span: span}, type, expr)) => {
			ctx.findLocal(name)._and(local =>
				if(!(local is LocalField || (ctx.isPattern() && local.ctx.isPattern() && ctx == local.ctx))) {
					ctx.addError(Errors.shadowedLocalVar(ctx, name, local.span, span));
				}
			);
			final t = type.toNull()._and(ty => ctx.getType(ty)._or(return invalidExpr()));
			final te: TExpr = { e: EVarDecl(name, t, expr.toNull()._and(e => typeExpr(ctx, e))), t: t };
			final local = new LocalVar(ctx, span, te);
			ctx.locals[name] = local;
			if(te.t == null) te.t = local.type;
			te;
		},
		
		// TEMP
		at(EType(type)) => {
			final t = ctx.getType(type)._or(return invalidExpr());
			if(ctx.isPattern()) {
				{ e: EPatternType(t), t: null };
			} else {
				throw 'error: type `${t.fullName()}` cannot be used as a value! ${type.span().display()}';
			}
		},

		_ => throw "todo! "+ctx.description()+" "+expr
	);

	res.orig = expr;
	res.t._and(t => {
		res.t = t.simplify();

		//Main.typesDumper.dump(res.t, Nil);
		//Main.typesDumper.writeChar('\n'.code);
	});

	return res;
}

static function typeExprs(ctx: Ctx, exprs: Array<UExpr>): TExprs {
	return cast exprs.map(e -> typeExpr(ctx, e));
}

static function getNamesArgs(ctx: Ctx, labels: Array<parsing.ast.Message.Label>) {
	final names = new Array<String>();
	final args = new Array<TExpr>();

	for(l in labels) l._match(
		at(Named(_, n, e)) => {
			names.push(n);
			args.push(typeExpr(ctx, e));
		},

		at(Punned(s, n = "this")) => {
			names.push(n);
			args.push(typeExpr(ctx, ctx.findLocal("this") != null ? EName(s, n) : EThis(s)));
		},
		at(Punned(s, n)) => {
			names.push(n);
			args.push(typeExpr(ctx, EName(s, n)));
		},
		
		at(Anon(e)) => {
			names.push("_");
			args.push(typeExpr(ctx, e));
		}
	);

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

static function typeTMessage(ctx: Ctx, msg: UMessage<UType>): Message<Type> {
	return switch msg {
		case Single(cat, _, name): Single(cat.toNull()._and(c => ctx.getType(c)), name);

		case Multi(cat, labels):
			detuple2(@var names, @var args, getNamesArgs(ctx, labels));
			Multi(cat.toNull()._and(c => ctx.getType(c)), names, args);
	}
}

static function typeObjCascade(ctx: Ctx, type: Null<Type>, cascade: UCascade<UExpr>): Null<ObjCascade> {
	final cascadeCtx = ctx.innerCascade();
	return {
		ctx: ctx,
		t: null,
		depth: cascade.depth,
		kind: type._match(
			at(t!) => switch cascade.kind {
				case Member({name: name}): t.findSingleInst(ctx, name, ctx.typeDecl, true)._match(
					at(kind!) => Member(Single(kind)),
					_ => throw 'error: value of type `${t.fullName()}` does not have member/getter `$name`!'
				);

				case Message(msg): Message(msg._match(
					at(Single(None, _, name)) => t.findSingleInst(ctx, name, ctx.typeDecl)._match(
						at(kind!) => Single(kind),
						_ => throw 'error: value of type ${t.fullName()} does not respond to method `[$name]`!'
					),
					at(Single(Some(cat), _, name)) => {
						final tcat: Type = ctx.getType(cat)._or(return null)._match(
							at({t: TThis(source), span: span}) => source._match(
								at(td is TypeDecl) => { t: TConcrete(td), span: span },
								at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
								at(c is Category) => c.type.orElseDo(c.lookup._match(
									at(td is TypeDecl) => { t: TConcrete(td), span: span },
									at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
									_ => throw "bad"
								)),
								_ => throw "bad"
							),
							at(c) => c
						);
						
						var categories = t.t._match(
							at(TThis(td is TypeDecl)) => ({t: td.thisType.t, span: t.span} : Type),
							_ => t
						).findThisCategory(ctx, tcat, ctx.typeDecl).unique();
						categories._match(
							at([]) => throw 'error: value of type `${t.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
							at([found]) => found.findSingleInst(ctx, name, ctx.typeDecl)._match(
								at(kind!) => Single(kind),
								_ => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in category `${tcat.fullName()}`!'
							),
							at(found) => found.filterMap(f -> f.findSingleInst(ctx, name, ctx.typeDecl))._match(
								at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`!',
								at([kind!]) => Single(kind),
								at(kinds) => throw "todo"
							)
						);
					},

					at(Multi(None, labels)) => {
						detuple2(@var names, @var args, getNamesArgs(ctx, labels));
						t.findMultiInst(ctx, names, ctx.typeDecl)._match(
							at([]) => {
								ctx.addError(Errors.unknownMethod(ctx, false, t, names, labels[0].span()));
								return null;
							},
							at(kinds) => Multi(kinds, names, args)
						);
					},
					at(Multi(Some(cat), labels)) => {
						final tcat: Type = ctx.getType(cat)._or(return null)._match(
							at({t: TThis(source), span: span}) => source._match(
								at(td is TypeDecl) => { t: TConcrete(td), span: span },
								at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
								at(c is Category) => c.type.orElseDo(c.lookup._match(
									at(td is TypeDecl) => { t: TConcrete(td), span: span },
									at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
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
						).findThisCategory(ctx, tcat, ctx.typeDecl).unique();
						categories._match(
							at([]) => throw 'error: value of type `${t.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
							at([found]) => found.findMultiInst(ctx, names, ctx.typeDecl)._match(
								at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in category `${tcat.fullName()}`! ${cat.span().display()}',
								at(kinds) => Multi(kinds, names, args)
							),
							at(found) => Type.mostSpecificBy(
								found
									.map(f -> {cat: f, mth: f.findMultiInst(ctx, names, ctx.typeDecl)})
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
						final target = ctx.getType(ty)._or(return null);
						t.findCast(ctx, target, ctx.typeDecl)._match(
							at([]) => throw 'error: value of type ${t.fullName()} cannot be cast to type `${target.fullName()}`!',
							at(casts) => Cast(target, casts)
						);
					},
					at(Cast(Some(cat), ty)) => throw "todo"
				));


				case AssignMember({name: name}, _, None, rhs): t.findMultiInst(ctx, [name], ctx.typeDecl, true)._match(
					at(kinds!) => Member(Multi(kinds, [name], [typeExpr(ctx, rhs)])),
					_ => throw 'error: value of type `${t.fullName()}` does not have member/setter `$name`!'
				);
				case AssignMember({name: name}, _, Some(op), rhs): throw "todo";
				
				case AssignMessage(msg, _, None, rhs): msg._match(
					at(Single(cat, _, name)) => throw "todo",
					at(Multi(None, labels)) => {
						detuple2(@var names, @var args, getNamesArgs(ctx, labels));
						names = names.concat(["="]);
						t.findMultiInst(ctx, names, ctx.typeDecl)._match(
							at([]) => throw 'error: value of type ${t.fullName()} does not respond to method `[${names.joinMap(" ", n -> '$n:')}]`!',
							at(kinds) => {
								final trhs = typeExpr(ctx, rhs);
								AssignMessage(Multi(kinds, names, args.concat([trhs])), null, trhs);
							}
						);
					},
					at(Multi(Some(cat), labels)) => throw "todo",
					at(Cast(_, _)) => throw "bad"
				);
				case AssignMessage(msg, _, Some(op), rhs): throw "todo";


				case StepMember({name: name}, span, step):
					t.findMultiInst(ctx, [name], ctx.typeDecl, true)._match(
						at([]) => throw "bad",
						at([setKind]) => t.findSingleInst(ctx, name, ctx.typeDecl, true)._match(
							at(getKind!) => {
								final memt = getKind._match(
									at(SIMethod((_ : Method) => m) | SIMultiMethod(m)) => m.ret.nonNull(),
									at(SIMember(m)) => m.type.nonNull(),
									_ => throw "todo"
								);
								final op: UnaryOp = step._match(at(Incr) => Incr, at(Decr) => Decr);
								memt.findUnaryOp(ctx, op, ctx.typeDecl)._match(
									at(opKind!) => StepMember(setKind, getKind, opKind),
									_ => {
										ctx.addError(Errors.unknownMethod(ctx, memt, op, span));
										return null;
									}
								);
							},
							_ => throw "bad"
						),
						at(setKinds) => throw "todo"
					);

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
		nested: cascade.nested.filterMap(c -> typeObjCascade(cascadeCtx, null, c))
	};
}


static function typeStmt(ctx: Ctx, stmt: UStmt): TStmt {
	var stmtLabel: Null<String> = null;
	final res: TStmt = {
		s: switch stmt {
			case SExpr(expr):
				SExpr(typeExpr(ctx, expr));
			
			case SIf(_, cond, thenBlk, elseBlk):
				SIf(
					shouldBeLogical(ctx, typeExpr(ctx, cond)),
					typeBlock(ctx, thenBlk),
					elseBlk._and(b => typeBlock(ctx, b._2))
				);
			
			case SCase(_, cases, otherwise, _):
				SCase(
					cases.map(c -> {
						cond: shouldBeLogical(ctx, typeExpr(ctx, c.cond)),
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
							cond: c.when.toNull()._and(w => shouldBeLogical(patternCtx, typeExpr(patternCtx, w._2))),
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
					cond._and(c => shouldBeLogical(patternCtx, typeExpr(patternCtx, c._2))),
					typeBlock(patternCtx, thenBlk),
					elseBlk._and(e => typeBlock(ctx, e._2))
				);

			case SWhile(_, cond, label, blk):
				SWhile(
					shouldBeLogical(ctx, typeExpr(ctx, cond)),
					stmtLabel = getLabel(ctx, label),
					typeBlock(ctx, blk)
				);
			
			case SDoWhile(_, label, blk, _, cond):
				SDoWhile(
					typeBlock(ctx, blk),
					stmtLabel = getLabel(ctx, label),
					shouldBeLogical(ctx, typeExpr(ctx, cond))
				);
			
			case SForIn(_, lvar, lvar2, _, inExpr, cond, label, block):
				// TODO
				final forCtx = ctx.innerPattern();
				SForIn(
					typeExpr(forCtx, lvar),
					lvar2._and(l => typeExpr(forCtx, l)),
					typeExpr(ctx.innerBlock(), inExpr),
					cond._and(c => {
						final condCtx = forCtx.innerBlock();
						shouldBeLogical(condCtx, typeExpr(condCtx, c._2));
					}),
					label._and(l => l._2.name),
					typeBlock(forCtx.innerBlock(), block)
				);

			case SForRange(span, lvar, _, startK, startE, _, stopK, stopE, step, cond, label, block):
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
				final tcond = cond._and(c => {
					final condCtx = forCtx.innerBlock();
					shouldBeLogical(condCtx, typeExpr(condCtx, c._2));
				});
				
				final loopt: Null<Type> = Util._match([tstart.t, tstop.t],
					at([null, null]) => null,
					at([null, t!!]) => { tstart.t = t; t; },
					at([t!!, null]) => { tstop.t = t; t; },
					at([t1!!, t2!!]) => t1.strictUnifyWithType(t2)._match(
						at(t!) => t,
						_ => throw 'error: loop bounds of types `${t1.fullName()}` and `${t2.fullName()}` are not compatible! ${span.display()}'
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
							cond: c.when.toNull()._and(w => shouldBeLogical(ctx, typeExpr(patternCtx, w._2))),
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

static function shouldBeLogical(ctx: Ctx, texpr: TExpr) {
	texpr.t._match(
		at(t!) => if(!t.isNative(NBool)) {
			ctx.addError(Errors.expectedLogicalValue(ctx, t, texpr.orig.mainSpan()));
		},
		_ => {}
	);
	return texpr;
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