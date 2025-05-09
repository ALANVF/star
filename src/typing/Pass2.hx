package typing;

import typing.EmptyMethod;
import parsing.ast.Ident;
import text.Span;
import Util.detuple;
import parsing.ast.Expr as UExpr;
import parsing.ast.Stmt as UStmt;
import parsing.ast.Type as UType;
import parsing.ast.Message as UMessage;
import parsing.ast.Cascade as UCascade;
import parsing.ast.Block as UBlock;

using typing.CastKind;
using typing.MultiStaticKind;
using typing.MultiInstKind;
using typing.BinaryOpKind;


/* This pass does all the things, including typing statements and expressions (well... most of them).
 *
 * TODO:
 * - enforced visibility
 * - hardcode some macro stuff (will be implemented correctly in bootstrapped compiler)
 * - finish type inference for type arguments (mainly for refined/overloaded types)
 * 
 * TODO (...eventually):
 * - complex typevars/typeclasses (nested type conditions, structural reqs, existential types, curried HKTs, and anything else that might also exist)
 * - unique typevar/typeclass instances
 * - very specialized type refinement/overloading
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
				this.type = type ?? value?.t;
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

class LocalBinding extends Local {
	function new(ctx, span, name, type) {
		this.ctx = ctx;
		this.span = span;
		this.name = name;
		this.type = type;
		this.expr = null;
	}
}

class LocalField extends Local {
	var member: Member;
	function new(ctx: Ctx, member: Member, name, type: Null<Type>, expr) {
		this.ctx = ctx;
		this.member = member;
		this.span = member.span;
		this.name = name;
		this.type = type ?? member.type;
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
var STD_Func0: TypeDecl;
var STD_Func1: TypeDecl;
var STD_Func2: TypeDecl;
var STD_Func3: TypeDecl;
var STD_Iterable: Type;
var STD_Iterable1: TypeDecl;
var STD_Iterable2: TypeDecl;
var STD_Iterator: Type;
var STD_Iterator1: TypeDecl;
var STD_Iterator2: TypeDecl;

function initSTD(std: Project) {
	STD_Value = std.findType(List3.of([null, "Star", []], [null, "Value", []]), Inside, null).getTypeDecl();
	STD_MultiKind = std.findType(List3.of([null, "Star", []], [null, "MultiKind", []]), Inside, null).getTypeDecl();
	STD_Void = std.findType(List3.of([null, "Star", []], [null, "Void", []]), Inside, null).getTypeDecl();
	STD_Int = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Int", []]), Inside, null).nonNull();
	STD_Dec = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Dec", []]), Inside, null).nonNull();
	STD_Char = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Char", []]), Inside, null).nonNull();
	STD_Bool = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Bool", []]), Inside, null).nonNull();
	STD_Str = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Str", []]), Inside, null).nonNull();
	STD_Array = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Array", []]), Inside, null).nonNull();
	STD_Dict = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Dict", []]), Inside, null).nonNull();
	STD_Tuple = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Tuple", []]), Inside, null).nonNull();
	STD_Func = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Func", []]), Inside, null).nonNull();
	STD_Func._match(
		at({t: TMulti(types)}) => {
			// TODO: make this better

			STD_Func0 = types.findMap(t -> {
				final decl = t.getTypeDecl();
				if(decl.params.length == 1) decl else null;
			}) ?? throw "internal error: bad Func type!";
			
			STD_Func1 = types.findMap(t -> {
				final decl = t.getTypeDecl();
				if(decl.params.length == 2) decl else null;
			}) ?? throw "internal error: bad Func type!";

			STD_Func2 = types.findMap(t -> {
				final decl = t.getTypeDecl();
				if(decl.params.length == 3) decl else null;
			}) ?? throw "internal error: bad Func type!";

			STD_Func3 = types.findMap(t -> {
				final decl = t.getTypeDecl();
				if(decl.params.length == 4) decl else null;
			}) ?? throw "internal error: bad Func type!";
		},
		_ => throw "internal error: bad Iterable type!"
	);
	STD_Iterable = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Iterable", []]), Inside, null).nonNull();
	STD_Iterable._match(
		at({t: TMulti([
			{t: TConcrete(decl1) | TModular({t: TConcrete(decl1)}, _)},
			{t: TConcrete(decl2) | TModular({t: TConcrete(decl2)}, _)}
		])}) => {
			if(decl1.params.length == 1 && decl2.params.length == 2) {
				STD_Iterable1 = decl1;
				STD_Iterable2 = decl2;
			} else if(decl1.params.length == 2 && decl2.params.length == 1) {
				STD_Iterable1 = decl2;
				STD_Iterable2 = decl1;
			} else {
				throw "internal error: bad Iterable type!";
			}
		},
		_ => throw "internal error: bad Iterable type!"
	);
	STD_Iterator = std.findType(List3.of([null, "Star", []], [null, "Core", []], [null, "Iterator", []]), Inside, null).nonNull();
	STD_Iterator._match(
		at({t: TMulti([
			{t: TConcrete(decl1) | TModular({t: TConcrete(decl1)}, _)},
			{t: TConcrete(decl2) | TModular({t: TConcrete(decl2)}, _)}
		])}) => {
			if(decl1.params.length == 1 && decl2.params.length == 2) {
				STD_Iterator1 = decl1;
				STD_Iterator2 = decl2;
			} else if(decl1.params.length == 2 && decl2.params.length == 1) {
				STD_Iterator1 = decl2;
				STD_Iterator2 = decl1;
			} else {
				throw "internal error: bad Iterator type!";
			}
		},
		_ => throw "internal error: bad Iterator type!"
	);
}

typedef PatternBindings = Map<String, Tuple2<Span, Type>>;


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
	for(decl in file.sortedDecls) decl.buildRefinements();
}


static function resolveDirMembers(dir: Dir) {
	for(f in dir.files) resolveFileMembers(f);
	for(u in dir.units) resolveUnitMembers(u);
}

static function resolveProjectMembers(proj: Project) {
	resolveDirMembers(proj);
	proj.main.forEach(m -> resolveFileMembers(m));
}

static function resolveUnitMembers(unit: Unit) {
	resolveDirMembers(unit);
	unit.primary.forEach(p -> resolveFileMembers(p));
}

static function resolveFileMembers(file: File) {
	// TODO: make this better
	for(decl in file.sortedDecls) resolveDeclMembers({
		where: WDecl(decl),
		thisType: decl.thisType
	}, decl);
}

static function resolveDeclMembers(ctx: Ctx, decl: TypeDecl) {
	decl._match(
		at(ns is Namespace) => {
			for(decl2 in ns.sortedDecls) resolveDeclMembers(ctx.innerDecl(decl2), decl2);
			//for(m in ns.staticMembers) resolveMember(ctx, m);

			ns._match(
				at(cl is ClassLike) => {
					for(m in cl.members) resolveMember(ctx, m);
				},
				_ => {}
			);
		},
		at(sa is StrongAlias) => {
			//for(m in sa.staticMembers) resolveMember(ctx, m);
			for(m in sa.members) resolveMember(ctx, m);
		},
		_ => {}
	);
}


static function resolveDir(dir: Dir) {
	for(f in dir.files) resolveFile(f);
	for(u in dir.units) resolveUnit(u);
}


static function resolveProject(proj: Project) {
	buildProjectRefinements(proj);
	resolveProjectMembers(proj);
	resolveDir(proj);
	proj.main.forEach(m -> resolveFile(m));
}


static function resolveUnit(unit: Unit) {
	unit.primary.forEach(p -> resolveFile(p));
	resolveDir(unit);
}


static function resolveFile(file: File) {
	// TODO: make this better
	for(decl in file.sortedDecls) resolveDecl({
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
			for(decl2 in ns.sortedDecls) decl2.buildRefinements();
			for(decl2 in ns.sortedDecls) resolveDecl(ctx.innerDecl(decl2), decl2);
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
		at(da is DirectAlias) => {
			da.type = da.type.simplify();
		},
		at(sa is StrongAlias) => {
			sa.type = sa.type.simplify();
			for(m in sa.staticMembers) resolveMember(ctx, m);
			for(m in sa.staticMethods) resolveStaticMethod(ctx, m);
			for(m in sa.members) resolveMember(ctx, m);
			for(m in sa.methods) resolveMethod(ctx, m);
			for(o in sa.operators) resolveOperator(ctx, o);
			sa.staticInit.forEach(i -> resolveEmptyMethod(ctx, i));
			sa.staticDeinit.forEach(i -> resolveEmptyMethod(ctx, i));
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
	category.thisType = category.thisType.simplify();
	for(m in category.staticMembers) resolveMember(ctx, m);
	for(m in category.staticMethods) resolveStaticMethod(ctx, m);
	for(m in category.methods) resolveMethod(ctx, m);
	for(i in category.inits) resolveInit(ctx, i);
	for(o in category.operators) resolveOperator(ctx, o);
	category.staticInit.forEach(i -> resolveEmptyMethod(ctx, i));
	category.staticDeinit.forEach(i -> resolveEmptyMethod(ctx, i));
}


static function resolveEmptyMethod(ctx: Ctx, method: EmptyMethod) {
	// TODO: improve this
	final oldLocals = ctx.locals;
	ctx.locals = ctx.locals.copy();

	final methodCtx = ctx.innerEmptyMethod(method);
	final bodyCtx = methodCtx.innerBlock();
	final tstmts = method.body.map(stmt -> typeStmt(bodyCtx, stmt));
	method.typedBody = tstmts;

	ctx.locals = oldLocals;
}


static function resolveMethod(ctx: Ctx, method: Method) {
	if(method.isMacro) return;

	// TODO: improve this
	final oldLocals = ctx.locals;
	ctx.locals = ctx.locals.copy();

	final methodCtx = ctx.innerMethod(method);

	method._match(
		at({typevars: tvars} is MultiMethod
		| {typevars: tvars} is CastMethod) => {
			for(tvar in tvars.allValues()) {
				tvar.rule._and(rule => {
					rule.trackEffectsIn(ctx)._and(effects => {//trace(effects.adds, effects.removes);
						methodCtx.effects += effects;
					});
				});
			}
		},
		_ => {}
	);

	method._match(
		at(multi is MultiMethod) => {
			for(param in multi.params) {
				param.type = param.type.simplify();

				final span = param.name.span;
				final name = param.name.name;
				if(name == "_") {
					continue;
				} else methodCtx.locals[name]._andOr(local => {
					ctx.addError(Type_DuplicateParam(multi, name, local.span, span));
				}, {
					methodCtx.locals[name] = new LocalParam(
						methodCtx,
						span,
						name,
						param.type,
						param.value._and(v =>
							param.tvalue = assignType(methodCtx, typeExpr(methodCtx, v), param.type)
						)
					);
				});
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

	ctx.locals = oldLocals;
}


static function resolveStaticMethod(ctx: Ctx, method: StaticMethod) {
	if(method.isMacro) return;

	// TODO: improve this
	final oldLocals = ctx.locals;
	ctx.locals = ctx.locals.copy();

	final methodCtx = ctx.innerMethod(method);

	method._match(
		at(multi is MultiStaticMethod) => {
			for(param in multi.params) {
				param.type = param.type.simplify();

				final span = param.name.span;
				final name = param.name.name;
				if(name == "_") {
					continue;
				} else methodCtx.locals[name]._andOr(local => {
					ctx.addError(Type_DuplicateParam(multi, name, local.span, span));
				}, {
					methodCtx.locals[name] = new LocalParam(
						methodCtx,
						span,
						name,
						param.type,
						param.value._and(v =>
							param.tvalue = assignType(methodCtx, typeExpr(methodCtx, v), param.type)
						)
					);
				});
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

	ctx.locals = oldLocals;
}


static function resolveInit(ctx: Ctx, init: Init) {
	if(init.isMacro) return;

	// TODO: improve this
	final oldLocals = ctx.locals;
	ctx.locals = ctx.locals.copy();

	final initCtx = ctx.innerMethod(init);

	init._match(
		at(multi is MultiInit) => {
			for(param in multi.params) {
				param.type = param.type.simplify();
				
				final span = param.name.span;
				final name = param.name.name;
				if(name == "_") {
					continue;
				} else initCtx.locals[name]._andOr(local => {
					ctx.addError(Type_DuplicateParam(multi, name, local.span, span));
				}, {
					initCtx.locals[name] = new LocalParam(
						initCtx,
						span,
						name,
						param.type,
						param.value._and(v =>
							param.tvalue = assignType(initCtx, typeExpr(initCtx, v), param.type)
						)
					);
				});
			}
		},
		_ => {}
	);

	init.body._and(body => {
		final bodyCtx = initCtx.innerBlock();
		final tstmts = body.map(stmt -> typeStmt(bodyCtx, stmt));
		init.typedBody = tstmts;
	});

	ctx.locals = oldLocals;
}


static function resolveOperator(ctx: Ctx, op: Operator) {
	if(op.isMacro) return;

	// TODO: improve this
	final oldLocals = ctx.locals;
	ctx.locals = ctx.locals.copy();

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

	ctx.locals = oldLocals;
}


static function resolveMember(ctx: Ctx, member: Member) {
	// TODO: improve this
	final oldLocals = ctx.locals;
	ctx.locals = ctx.locals.copy();

	member.type._andOr(
		type => {
			member.type = {t: type.simplify().t, span: type.span};
		},
		ctx.typeDecl.findInstMember(ctx, member.name.name, member.isStatic, true)._and(kind => {
			member.type = kind.retType();
		})
	);

	member.value._and(value => {
		final texpr = typeExpr(ctx.innerMember(member), value);
		if(member.type == null) {
			member.type = texpr.t;
		}
		member.typedValue = texpr;
	});

	ctx.locals = oldLocals;
}


static function resolveValueCase(ctx: Ctx, vcase: ValueCase) {
	vcase.value._and(value => {
		vcase.typedValue = typeExpr(ctx, value);
	});
}

static function resolveTaggedCase(ctx: Ctx, tcase: TaggedCase) {
	// TODO: improve this
	final oldLocals = ctx.locals;
	ctx.locals = ctx.locals.copy();

	final caseCtx = ctx.innerTaggedCase(tcase);

	tcase._match(
		at({params: params} is MultiTaggedCase) => {
			for(param in params) {
				param.type = param.type.simplify();
				param.value._and(v =>
					param.tvalue = assignType(caseCtx, typeExpr(caseCtx, v), param.type)
				);
			}
		},
		_ => {}
	);

	tcase.assoc._and(assoc => {
		tcase.typedAssoc = typeTMessage(caseCtx, assoc);
	});

	tcase.init._and(init => {
		final initCtx = caseCtx.innerBlock();

		tcase._match(
			at(tcase is MultiTaggedCase) => {
				for(param in tcase.params) {
					param.type = param.type.simplify();
	
					final span = param.name.span;
					final name = param.name.name;
					if(name == "_") {
						continue;
					} else initCtx.locals[name]._andOr(local => {
						ctx.addError(Type_DuplicateCaseParam(tcase, name, local.span, span));
					}, {
						initCtx.locals[name] = new LocalParam(
							initCtx,
							span,
							name,
							param.type,
							param.tvalue
						);
					});
				}
			},
			_ => {}
		);

		tcase.typedInit = init.map(s -> typeStmt(initCtx, s));
	});

	ctx.locals = oldLocals;
}


static inline function assignType(ctx: Ctx, expr: TExpr, type: Type): TExpr {
	expr.t = type;
	return expr;
}

static function invalidExpr(): TExpr {
	return { e: EInvalid, t: null };
}

static function typeExpr(ctx: Ctx, expr: UExpr): TExpr {
	final res: TExpr = expr._match(
		at(EName(span, name)) =>
			ctx.findLocal(name)._andOr(
				local => { e: EName(name, local), t: local.type },
				{
					ctx.addError(Type_UnknownFieldOrVar(ctx, name, span));
					invalidExpr();
				}
			),
		
		at(ETag(s, "kind_slot", EObjMessage(ETuple(_, [obj, EInt(_, i, _)], _), _, Cast(_, type), _))) => {
			final tobj = typeExpr(ctx, obj);
			final ttype = ctx.getType(type) ?? return invalidExpr();
			{
				e: EKindSlot(tobj, i),
				t: ttype
			};
		},
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
					t: {t: STD_Int.t, span: s}
				};
				case "kind_slot": {
					// already handled earlier. anything else is invalid
					throw 'error: invalid expression for `#kind_slot` tag! ${s.display()}';
				}
				default:
					trace('warning: unknown tag `#$name` ${s.display()}\n');
					{ e: ETag(name, texpr), t: null };
			}
		},

		at(EInt(_, int, exp)) => { e: EInt(int, exp), t: exp == null ? STD_Int : STD_Dec },
		at(EDec(_, int, dec, exp)) => { e: EDec(int, dec, exp), t: STD_Dec },
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
						final et = ts.reduce((t1, t2) -> t1.strictUnifyWithType(t2) ?? {
							//ctx.addError(Errors...)
							throw "todo";
						});
						{t: TApplied(STD_Array, [et]), span: _begin};
					}
				)
			};
		},
		at(EHash(_begin, pairs, _)) => {
			final tpairs = pairs.map(p -> tuple(typeExpr(ctx, p.k), typeExpr(ctx, p.v)));
			if(tpairs.length == 0) {
				{ e: EHash(tpairs), t: STD_Dict };
			} else {
				var tkey: Null<Type> = null;
				var tvalue: Null<Type> = null;
				for(p in tpairs) { detuple(@var [k, v] = p);
					tkey._andOr(tk1 => {
						k.t._and(tk2 => {
							tk1.strictUnifyWithType(tk2)._andOr(t => {
								tkey = t;
							}, {
								//ctx.addError(Errors...)
								throw "todo";
							});
						});
					}, {
						tkey = k.t;
					});
					tvalue._andOr(tv1 => {
						v.t._and(tv2 => {
							tv1.strictUnifyWithType(tv2)._andOr(t => {
								tvalue = t;
							}, {
								//ctx.addError(Errors...)
								throw "todo";
							});
						});
					}, {
						tvalue = v.t;
					});
				}
				
				Util._match([tkey, tvalue],
					at([tk!, tv!]) => {
						{ e: EHash(tpairs), t: {t: TApplied(STD_Dict, [tk, tv]), span: _begin} };
					},
					at([_, _]) => {
						trace("???");
						{ e: EHash(tpairs), t: STD_Dict };
					}
				);
			}
		},
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
			if(ctx.allowsThis()) {
				e: EThis,
				t: {t: {
					final decl = ctx.typeDecl;
					if(decl.thisType.t != ctx.thisType.t) ctx.thisType.t
					else decl is Category ? decl.thisType.t : TThis(decl); // hacky thingy because categories are dumb
				}, span: span}
			} else {
				ctx.addError(Type_ThisNotAllowed(ctx, span));
				invalidExpr();
			}
		},
		at(EWildcard(s)) => {
			if(ctx.isPattern()) { e: EWildcard, t: null/*{t: TBlank}*/ }
			else throw 'error: wildcard is not allowed outside of a pattern ${s.display()}';
		},
		at(EFunc(begin, params, ret, body, end)) => {
			// TODO

			if(params.every(p -> p.type != null)) {
				final closureCtx = ctx.innerBlockExpr();
				final params2: Array<{name: String, ?type: Type}> = [];
				final atypes: Array<Type> = [];
				for(p in params) {
					final t = ctx.getType(p.type) ?? return invalidExpr();
					final name = p.name.name;
					params2.push({name: name, type: t});
					atypes.push(t);
					closureCtx.locals[name] = new LocalParam(closureCtx, p.name.span, name, t, null);
				}
				// ignore ret for now
				final tstmts = body.map(stmt -> typeStmt(closureCtx, stmt));
				final res = getReturnType(closureCtx, tstmts);
				for(span in res.incomplete) {
					trace("warning: this branch does not return a value "+span.display());
				}
				final tret = res._match(
					at({complete: true, spans: spans, ret: ret!}) => ret,
					at({complete: true, spans: spans, ret: null}) => {
						trace("warning: could not infer return type of block expression "+(spans.length == 0 ? end : spans.last()).display());
						null;
					},
					at({complete: false, spans: spans, ret: ret!}) => {
						trace("warning: not all paths of block expression return a value "+(spans.length == 0 ? end : spans.last()).display());
						ret;
					},
					at({complete: false, spans: spans}) => {
						trace("warning: could not infer return type of incomplete block expression "+(spans.last() ?? begin).display());
						null;
					},
					at({complete: true, spans: spans}) => {
						trace("???????");
						null;
					}
				);
				{
					e: EFunc(params2, null, tstmts),
					t: tret._and(tr => {
						final r = STD_Func.applyArgs([tr].concat(atypes));
						//trace(r.fullName());
						r;
					})
				};
			} else {
				return invalidExpr();
			}
		},
		at(EAnonFunc(depth, nparams, types, expr)) => {
			// TODO
			final exprSpan = expr.mainSpan();
			final anonCtx = ctx.innerBlockExpr();
			final params: Array<{name: String, ?type: Type}> = [];
			for(i in 0...nparams) {
				final name = '$depth@$i';
				final type = types?.get(i)._and(t => {
					ctx.getType(t) ?? return invalidExpr();
				});
				params.push({name: name, type: type});
				anonCtx.locals[name] = new LocalParam(anonCtx, exprSpan, name, type, null);
			}
			final tstmts = [
				typeStmt(anonCtx, SReturn(exprSpan, expr))
			];
			{
				e: EFunc(params, null, tstmts),
				t: nparams._match(
					at(1) => getReturnType(anonCtx, tstmts).ret._andOr(
						ret => {t: TMulti([
								STD_Func1.applyArgs([ret, params[0]?.type ?? STD_Func1.params[1]]).nonNull(),
								STD_Func2.applyArgs([ret, params[0]?.type ?? STD_Func2.params[1], params[1]?.type ?? STD_Func2.params[2]]).nonNull()
							]), span: null},
						{t: TMulti([ STD_Func1.applyArgs(STD_Func1.params).nonNull(), STD_Func2.applyArgs(STD_Func2.params).nonNull() ]), span: null}
					),
					at(2) => getReturnType(anonCtx, tstmts).ret._andOr(
						ret => STD_Func2.applyArgs([ret, params[0]?.type ?? STD_Func2.params[1], params[1]?.type ?? STD_Func2.params[2]]),
						STD_Func2.applyArgs(STD_Func2.params)
					),
					at(3) => getReturnType(anonCtx, tstmts).ret._andOr(
						ret => STD_Func3.applyArgs([ret, STD_Func3.params[1], STD_Func3.params[2], STD_Func3.params[3]]),
						STD_Func3.applyArgs(STD_Func3.params)
					),
					_ => {
						trace("todo!");
						null;
					}
				)
			};
		},
		//at(EAnonArg(_, depth, nth)) => { e: EAnonArg(depth, nth) },
		at(EAnonArg(s, _, _)) => {
			throw "why is this here?!?! "+s.display();
		},
		at(ELiteralCtor(type, literal)) => {
			final t = ctx.getType(type) ?? return invalidExpr();
			{ e: ELiteralCtor(t, typeExpr(ctx, literal)), t: t };
		},
		
		at(EParen(_, exprs, _)) => {
			final texprs = typeExprs(ctx, exprs);
			{ e: EParen(texprs), t: texprs.last().t };
		},
		at(EBlock(blk)) => {
			final tstmts = typeBlockExpr(ctx, blk);
			final res = getReturnType(ctx, tstmts);
			for(span in res.incomplete) {
				trace("warning: this branch does not return a value "+span.display());
			}
			{
				e: EBlock(tstmts),
				t: res._match(
					at({complete: true, spans: spans, ret: ret!}) => ret,
					at({complete: true, spans: spans, ret: null}) => {
						trace("warning: could not infer return type of block expression "+(spans.length == 0 ? blk.end : spans.last()).display());
						null;
					},
					at({complete: false, spans: spans, ret: ret!}) => {
						trace("warning: not all paths of block expression return a value "+(spans.length == 0 ? blk.end : spans.last()).display());
						ret;
					},
					at({complete: false, spans: spans}) => {
						trace("warning: could not infer return type of incomplete block expression "+(spans.last() ?? blk.begin).display());
						null;
					},
					at({complete: true, spans: spans}) => {
						trace("???????");
						null;
					}
				)
			};
		},

		at(ETypeMessage(type, begin, msg, end)) => {
			final t = (ctx.getType(type) ?? return invalidExpr()).fullSimplify();
			sendTypeMessage(ctx, t, begin, end, msg)._match(
				at(null) => invalidExpr(),
				at(tuple(msg2, ret)) => {e: ETypeMessage(t, msg2), t: ret.getFrom(t)}
			);
		},

		at(ETypeCascade(type, cascades)) => {
			final t = ctx.getType(type) ?? return invalidExpr();
			{
				e: ETypeCascade(t, cascades.filterMap(c -> typeTypeCascade(ctx, t, c))),
				t: {t: STD_Void.thisType.t, span: type.span() }
			};
		},
		
		at(ETypeMember(type, {name: name, span: s})) => {
			final t = ctx.getType(type) ?? return invalidExpr();
			t.findSingleStatic(ctx, name, ctx.typeDecl.thisType, true)._andOr(
				kind => {
					e: ETypeMember(t, kind),
					t: kind.retType()?.getFrom(t)
				},
				{
					ctx.addError(Type_UnknownGetter(ctx, Static, t, name, s));
					invalidExpr();
				}
			);
		},
		
		
		at(EObjMessage(obj, begin, msg, end)) => {
			final tobj = typeExpr(ctx, obj);
			tobj.t._andOr(
				t => sendObjMessage(ctx, t, begin, end, msg)._match(
					at(null) => invalidExpr(),
					at(tuple(msg2, ret)) => {
						{ e: EObjMessage(tobj, msg2), t: ret?.simplify() };
					}
				),
				{ e: EObjMessage(tobj, Lazy(typeMessage(ctx, msg))) }
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
			ctx.findLocal(name, 1)._andOr(
				local => { e: EName(name, local), t: local.type },
				{
					ctx.addError(Type_UnknownFieldOrVar(ctx, '_.$name', span1.union(span2)));
					invalidExpr();
				}
			),
		// TODO: _._.var, ...
		at(EObjMember(obj, {span: s, name: name})) => {
			final tobj = typeExpr(ctx, obj);
			tobj.t._andOr(t => {
				t = t.getIn(ctx);
				t.findSingleInst(ctx, name, ctx.typeDecl, true)._andOr(
					kind => {
						e: EObjMember(tobj, kind),
						t: kind.retType()?.getFrom(t)
					},
					{
						ctx.addError(Type_UnknownGetter(ctx, Instance, t, name, s));
						invalidExpr();
					}
				);
			}, { e: EObjLazyMember(tobj, name) });
		},
		
		//case EPrefix(_, PNeg, EInt(_, int, exp)): { e: EInt(-int, exp.toNull()), t: STD_Int };
		at(EPrefix(span, op, right)) => {
			final rhs = typeExpr(
				ctx,
				if(op.match(PIncr | PDecr) && right.match(
					EName(_, _)
					| ETypeMessage(_, _, _, _)
					| ETypeMember(_, _)
					| EObjMessage(_, _, _, _)
					| EObjMember(_, _)
				))
					EInfix(right, span, Assign(null), right)
				else
					right
			);
			rhs.t._andOr(t => {
				//t = t.getIn(ctx);
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
					at(kind!) => if(op.match(PIncr | PDecr)) rhs.e._match(
						at(ESetName(name, loc, value)) => {
							e: ESetName(name, loc, {
								e: EPrefix(kind, {
									e: EName(name, loc),
									t: loc.type,
									orig: right
								}),
								t: kind.retType().getFrom(t),
								orig: expr
							}),
							t: rhs.t
						},
						at(ETypeMessage(type, msg)) => {
							e: {
								function loop(msg: TypeMessage) msg._match(
									at(Single(_)) => throw "bad",
									at(Multi(candidates, labels, args)) => {
										args.setLast({
											e: EPrefix(kind, args.last()),
											t: kind.retType().getFrom(t),
											orig: args.last().orig
										});
									},
									at(Super(_, msg2)) => loop(msg2)
								);
								loop(msg);
								rhs.e;
							},
							t: rhs.t
						},
						at(EObjMessage(expr, msg)) => {
							e: {
								function loop(msg: ObjMessage) msg._match(
									at(Multi(candidates, labels, args)) => {
										args.setLast({
											e: EPrefix(kind, args.last()),
											t: kind.retType().getFrom(t),
											orig: args.last().orig
										});
									},
									at(Super(_, msg2)) => loop(msg2),
									_ => throw "bad"
								);
								loop(msg);
								rhs.e;
							},
							t: rhs.t
						},
						_ => {
							e: EPrefix(kind, rhs),
							t: kind.retType().getFrom(t)
						}
					) else {
						e: EPrefix(kind, rhs),
						t: kind.retType().getFrom(t)
					},
					_ => if(ctx.isPattern()) {
						{ e: ELazyPrefix(op, rhs) };
					} else {
						ctx.addError(Type_UnknownMethod(ctx, t, Unary(op2), span));
						invalidExpr();
					}
				);
			}, { e: ELazyPrefix(op, rhs) });
		},
		
		at(ESuffix(left, span, op)) => {
			final lhs = typeExpr(ctx, op.match(SIncr | SDecr) ? EInfix(left, span, Assign(null), left) : left);
			lhs.t._match(
				at(t!) => {
					final op2: UnaryOp = op._match(
						at(SIncr) => Incr,
						at(SDecr) => Decr,
						at(STruthy) => /*if(ctx.isPattern()) {
							return { e: ELazySuffix(lhs, op) };
						} else {*/
							Truthy//;
						//}
					);
					
					t.findUnaryOp(ctx, op2, ctx.typeDecl)._andOr(kind => {
						if(op.match(SIncr | SDecr)) lhs.e._match(
							at(ESetName(name, loc, value)) => {
								e: ESetName(name, loc, {
									e: ESuffix({
										e: EName(name, loc),
										t: loc.type,
										orig: left
									}, kind),
									t: kind.digForMethod().ret.nonNull().simplify().getFrom(t),
									orig: expr
								}),
								t: lhs.t
							},
							at(ETypeMessage(type, msg)) => {
								e: {
									function loop(msg: TypeMessage) msg._match(
										at(Single(_)) => throw "bad",
										at(Multi(candidates, labels, args)) => {
											args.setLast({
												e: ESuffix(args.last(), kind),
												t: kind.digForMethod().ret.nonNull().simplify().getFrom(t),
												orig: args.last().orig
											});
										},
										at(Super(_, msg2)) => loop(msg2)
									);
									loop(msg);
									lhs.e;
								},
								t: lhs.t
							},
							at(EObjMessage(expr, msg)) => {
								e: {
									function loop(msg: ObjMessage) msg._match(
										at(Multi(candidates, labels, args)) => {
											args.setLast({
												e: ESuffix(args.last(), kind),
												t: kind.digForMethod().ret.nonNull().simplify().getFrom(t),
												orig: args.last().orig
											});
										},
										at(Super(_, msg2)) => loop(msg2),
										_ => throw "bad"
									);
									loop(msg);
									lhs.e;
								},
								t: lhs.t
							},
							_ => {
								e: ESuffix(lhs, kind),
								t: kind.digForMethod().ret.nonNull().simplify().getFrom(t)
							}
						) else {
							e: ESuffix(lhs, kind),
							t: kind.digForMethod().ret.nonNull().simplify().getFrom(t)
						}
					}, {
						if(ctx.isPattern()) {
							{ e: ELazySuffix(lhs, op) };
						} else {
							ctx.addError(Type_UnknownMethod(ctx, t, Unary(op2), span));
							invalidExpr();
						}
					});
				},
				_ => { e: ELazySuffix(lhs, op) }
			);
		},
		
		
		// TODO: make this better
		
		at(EInfix(EObjMessage(obj, begin, msg, end), span, Assign(null), right)) =>
			msg._match(
				at(Single(cat, span2, name)) =>
					typeExpr(ctx, EObjMessage(obj, begin, Multi(cat, [Named(Span.range(span, span2), name, right)]), end)),

				// hacky but eh
				at(Multi(cat, labels)) =>
					typeExpr(ctx, EObjMessage(obj, begin, Multi(cat, labels.concat([Named(span, "=", right)])), end)),
				
				at(Cast(_, _)) => throw "bad"
			),
		
		at(EInfix(left = EObjMessage(obj, begin, msg, end), span, Assign(op!!), right)) =>
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
		

		at(EInfix(left = EObjMember(EWildcard(span0), {name: name, span: span1}), span2, Assign(op), right)) =>
			typeLocalAssign(ctx, ctx.findLocal(name, 1), left, name, 1, span0.union(span1), span2, op, right),
		// TODO: _._.var, ...

		at(EInfix(left = EName(span1, name), span2, Assign(op), right)) =>
			typeLocalAssign(ctx, ctx.findLocal(name), left, name, 0, span1, span2, op, right),
		
		
		// TODO: make this better

		at(EInfix(EObjMember(obj, {name: name, span: span1}), span2, Assign(null), right)) => {
			final tobj = typeExpr(ctx, obj);
			tobj.t._andOr(t => {
				final tright = typeExpr(ctx, right);
				t = t.getIn(ctx);
				t.findMultiInst(ctx, [name], ctx.typeDecl.thisType, true)._match(
					at([]) => {
						ctx.addError(Type_UnknownSetter(ctx, Instance, t, name, span1));
						invalidExpr();
					},
					at(kinds) => kinds.reduceOverloads(ctx, t, [tright])._match(
						at([]) => {
							ctx.addError(Type_UnknownSetter(ctx, Instance, t, name, span1, tright));
							invalidExpr();
						},
						at(overloads) => {
							e: EObjMessage(tobj, Multi(overloads.simplify(), [name], [tright])),
							t: tright.t
						}
					)
				);
			}, {
				throw "todo at "+span1.display();
			});
		},

		at(EInfix(left = EObjMember(obj, ident), span, Assign(op!!), right)) => {
			typeExpr(ctx, EInfix(
				left,
				span,
				Assign(null),
				EInfix(
					left,
					span,
					op,
					right
				)
			));
		},

		
		// TEMP left pattern
		at(EInfix(left=ETuple(_,_,_)|EArray(_,_,_)|ELiteralCtor(_,_), span, Assign(null), right)) => {
			final rhs = typeExpr(ctx, right);
			final lhs = typePattern(ctx, rhs.t.nonNull(), left);
			buildPatternBindings(ctx, lhs);
			{ e: EDestructure(lhs, rhs), t: rhs.t };
		},
		at(EInfix(left, span, op=Assign(_), right)) => {
			final tleft = typeExpr(ctx, left);
			final tright = typeExpr(ctx, right);
			{ e: ELazyInfix(tleft, op, tright) };
		},
		
		at(EInfix(
			first = EInfix(
				left, span1, op1 = (TExpr.Infix.Eq ... Le), right1
			),
			span2, op2 = (TExpr.Infix.Eq ... Le),
			right2
		)) => {
			// TODO: make this better
			typeExpr(ctx,
				EInfix(
					first,
					span1,
					And,
					EInfix(right1, span2, op2, right2)
				)
			);
		},
		
		at(EInfix(left, span, op, right)) => {
			final tleft = typeExpr(ctx, left);
			final tright = typeExpr(ctx, right);
			final op2 = BinaryOp.fromInfix(op);

			tleft.t._match(
				at(lt!) => {
					lt.t._match(
						at(TThis(td is TypeDecl)) => ({t: td.thisType.t, span: lt.span} : Type),
						_ => lt
					).findBinaryOp(ctx, op2, ctx.typeDecl.thisType)._match(
						at([]) => {
							ctx.addError(Type_UnknownMethod(ctx, lt, Binary(op2, null), span));
							invalidExpr();
						},
						at(kinds) => kinds.reduceOverloads(lt, tright)._match(
							at([]) => {
								ctx.addError(Type_UnknownMethod(ctx, lt, Binary(op2, tright.t), span));
								invalidExpr();
							},
							at(overloads) => {
								e: EInfix(tleft, overloads.simplify(), tright),
								t: overloads.map(ov -> {
									final res = ov.ret.getFrom(lt);
									ov.tctx._andOr(
										tctx => res.getInTCtx(tctx),
										res
									);
								}).unique()._match(
									at([]) => null,
									at([ret]) => ret,
									at(rets) => {t: TMulti(rets), span: null}
								)
							}
						)
					);
				},
				_ => { e: ELazyInfix(tleft, op, tright) }
			);
		},

		// ...
		
		at(EVarDecl(_, {name: name, span: span}, type, EObjCascade(obj, cascades))) => {
			ctx.findLocal(name)._and(local =>
				if(!(local is LocalField || (ctx.isPattern() && local.ctx.isPattern() && ctx == local.ctx))) {
					ctx.addError(Type_ShadowedLocalVar(ctx, name, local.span, span));
				}
			);
			
			final tobj = typeExpr(ctx, obj);
			final t = {
				final tt = type._and(ty => ctx.getType(ty) ?? return invalidExpr());
				tobj.t._andOr(
					ot => tt._andOr(
						et => et.strictUnifyWithType(ot)
							?? throw 'error: local variable declared to be of type `${et.fullName()}`, but provided value was of type `${ot.fullName()}` instead!',
						ot
					),
					tt
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
			te.t ??= local.type;
			te;
		},

		at(EVarDecl(_, {name: name, span: span}, type, expr)) => {
			ctx.findLocal(name)._and(local =>
				if(!(local is LocalField || (ctx.isPattern() && local.ctx.isPattern() && ctx == local.ctx))) {
					ctx.addError(Type_ShadowedLocalVar(ctx, name, local.span, span));
				}
			);
			final t = type._and(ty => ctx.getType(ty) ?? return invalidExpr());
			final te: TExpr = { e: EVarDecl(name, t, expr._and(e => typeExpr(ctx, e))), t: t };
			final local = new LocalVar(ctx, span, te);
			ctx.locals[name] = local;
			te.t ??= local.type;
			te;
		},
		
		// TEMP
		at(EType(type)) => {
			final t = ctx.getType(type) ?? return invalidExpr();
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

static function typeLocalAssign(
	ctx: Ctx,
	found: Null<Local>,
	left: UExpr,
	name: String, depth: Int,
	span1: Span,
	span2: Span,
	assign: Null<TExpr.AssignInfix>,
	right: UExpr
): TExpr {
	// TODO: bad
	return found._andOr(local => {
		local._match(
			at({member: {isReadonly: true}} is LocalField) => {
				if(!ctx.canAssignReadonlyField()) {
					if(depth > 0) for(_ in 0...depth) name = '_.$name';
					throw 'error: field `$name` is readonly and cannot be assigned! ${span1.display()}';
				}
			},
			_ => {}
		);

		{
			e: assign._match(
				at(null) => {
					final tvalue = typeExpr(ctx, right);
					//trace(local.name, local.type?.fullName(), tvalue.t?.fullName(), span2.display());
					local._match(
						at(lvar is LocalVar | lvar is LocalBinding) => {
							//if(local.expr == null) local.expr = tvalue;
							tvalue.t._and(rt => {
								rt = rt.simplify();
								lvar.type._andOr(lt => {
									lt = lt.simplify().getIn(ctx);//.getFrom(ctx.thisType);
									lt.strictUnifyWithType(rt)._andOr(t => {
										//trace(t);
									}, if(!ctx.isPattern()) {
										ctx.addError(Type_LocalVarTypeMismatch(ctx, name, lt, rt, lvar.span, span1));
										return invalidExpr();
									});
								}, {
									lvar.type = rt;
								});
							});
						},
						_ => {}
					);

					// Basic type inference
					// TODO: work on this, likely doesn't work in more complex scenarios
					if(tvalue.t?.t.match(TMulti(_)) && local.type != null) {
						tvalue.e._match(
							at(ETypeMessage(type, msg)) => {
								if(type.t.match(TMulti(_))) {
									tvalue.e = ETypeMessage({
										t: local.type?.t,
										span: type.span
									}, msg);
								}
							},
							_ => {}
						);
			
						tvalue.t = {
							t: local.type?.t,
							span: tvalue.t.span
						};
					}
					
					ESetName(name, local, tvalue);
				},
				at(op!!) => {
					final tvalue = typeExpr(ctx, EInfix(left, span2, op, right));
					if((local is LocalVar || local is LocalBinding) && local.expr == null) {
						if(depth > 0) for(_ in 0...depth) name = '_.$name';
						throw 'error: variable `$name` is used before being assigned! ${span1.display()}';
					}
					ESetName(name, local, tvalue);
				}
			),
			t: local.type
		};
	}, {
		ctx.addError(Type_UnknownFieldOrVar(ctx, name, span1));
		invalidExpr();
	});
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

	return tuple(names, args);
}

static function getNamesUntypedArgs(ctx: Ctx, labels: Array<parsing.ast.Message.Label>) {
	final names = new Array<String>();
	final args = new Array<UExpr>();

	for(l in labels) l._match(
		at(Named(_, n, e)) => {
			names.push(n);
			args.push(e);
		},

		at(Punned(s, n = "this")) => {
			names.push(n);
			args.push(ctx.findLocal("this") != null ? EName(s, n) : EThis(s));
		},
		at(Punned(s, n)) => {
			names.push(n);
			args.push(EName(s, n));
		},
		
		at(Anon(e)) => {
			names.push("_");
			args.push(e);
		}
	);

	return tuple(names, args);
}

static function sendTypeMessage(ctx: Ctx, t: Type, begin: Span, end: Span, msg: UMessage<UType>): Null<Tuple2<TypeMessage, Null<Type>>> {
	return msg._match(
		at(Single(null, span, name)) => {
			t.findSingleStatic(ctx, name, ctx.typeDecl.thisType)._andOr(
				kind => tuple(
					Single(kind),
					kind.retType()?.getFrom(t)
				),
				{
					ctx.addError(Type_UnknownMethod(ctx, t, Single(Static, name), span));
					null;
				}
			);
		},
		at(Single(cat = TSegs(Nil, Cons(NameParams(_, "Super", {of: [parent]}), Nil)), _, name)) => {
			final tparent: Type = ctx.getType(parent) ?? return null;
			if(t.hasParentType(tparent)||tparent.hasChildType(t)) {
				tparent.findSingleStatic(ctx, name, ctx.typeDecl.thisType)._andOr(
					kind => tuple(
						Super(tparent, Single(kind)),
						(kind._match(
							at(SSMethod({ret: ret!})) => ret.t._match(
								at(TThis(source), when(source.hasChildType(t))) => t.t._match(
									at(TConcrete(decl)) => { t: TThis(decl), span: ret.span },
									at(TThis(source2)) => { t: TThis(source2), span: ret.span },
									at(TApplied({t: TConcrete(decl)}, args)) => t,
									at(TTypeVar(_)) => throw "todo (?) "+t.span?.display(),
									_ => t
								),
								at(TApplied(_, _) | TTypeVar(_)) => null, // TODO
								_ => ret
							),
							at(SSMember(mem)) => mem.type, // TODO: solve in ctx
							_ => null
						) : Null<Type>)?.getFrom(t)
					),
					throw 'error: type `${t.fullName()}` does not have a supertype `${tparent.fullName()}` that responds to method `[$name]`! ${cat.span().display()}'
				);
			} else {
				throw 'error: type `${t.fullName()}` does not have supertype `${tparent.fullName()}`! ${cat.span().display()}';
			}
		},
		at(Single(cat!!, _, name)) => {
			final tcat: Type = (ctx.getType(cat) ?? return null)._match(
				at({t: TThis(source), span: span}) => source._match(
					at(td is TypeDecl) => { t: TConcrete(td), span: span },
					at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
					at(c is Category) => c.type ?? c.lookup._match(
						at(td is TypeDecl) => { t: TConcrete(td), span: span },
						at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
						_ => throw "bad"
					),
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
					ctx.addError(Type_UnknownCategory(ctx, Static, t, tcat, cat.span()));
					null;
				},
				at([found]) => found.findSingleStatic(ctx, name, ctx.typeDecl.thisType)._andOr(
					kind => tuple(
						Single(kind),
						kind._match(
							at(SSMethod({ret: null, span: s}) | SSMultiMethod({ret: null, span: s})) =>
								{t: TConcrete(STD_Void), span: s},
							at(SSMethod({ret: ret!!}) | SSMultiMethod({ret: ret!!})) => ret.getFrom(t), // TODO: solve in method ctx
							at(SSMember({type: null})) => null,
							at(SSMember({type: type!!})) => type.getFrom(t),
							at(SSFromTypevar(tvar, _, _, kind2)) => null, // TODO
							_ => t
						)
					),
					throw 'error: type `${t.fullName()}` does not respond to method `[$name]` in category `${tcat.fullName()}`! ${begin.display()}'
				),
				at(found) => found.filterMap(f -> f.findSingleStatic(ctx, name, ctx.typeDecl.thisType))._match(
					at([]) => throw 'error: type `${t.fullName()}` does not respond to method `[$name]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`!',
					at([kind!]) => tuple(
						Single(kind),
						kind._match(
							at(SSMethod({ret: null, span: s}) | SSMultiMethod({ret: null, span: s})) =>
								{t: TConcrete(STD_Void), span: s},
							at(SSMethod({ret: ret!!}) | SSMultiMethod({ret: ret!!})) => ret.getFrom(t), // TODO: solve in method ctx
							at(SSMember({type: null})) => null,
							at(SSMember({type: type!!})) => type.getFrom(t),
							at(SSFromTypevar(tvar, _, _, kind2)) => null, // TODO
							_ => t
						)
					),
					at(kinds) => throw "todo"
				)
			);
		},
		
		at(Multi(null, labels)) => {
			detuple(@final [names, args] = getNamesArgs(ctx, labels));
			
			t.findMultiStatic(ctx, names, ctx.typeDecl.thisType).unique()/*.reduceBySender()*/._match(
				at([]) => {
					ctx.addError(Type_UnknownMethod(ctx, t, Multi(Static, names), labels[0].span()));
					null;
				},
				at(kinds) => kinds.reduceOverloads(t, args)._match(
					at([]) => {
						ctx.addError(Type_UnknownMethod(ctx, t, Multi(Static, names, args), labels[0].span()));
						null;
					},
					at(overloads) => tuple(
						Multi(overloads.simplify(), names, args),
						overloads.retType(t)
					)
				)
			);
		},
		// TODO: Super
		at(Multi(cat!!, labels)) => {
			final tcat: Type = (ctx.getType(cat) ?? return null)._match(
				at({t: TThis(source), span: span}) => source._match(
					at(td is TypeDecl) => { t: TConcrete(td), span: span },
					at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
					at(c is Category) => c.type ?? c.lookup._match(
						at(td is TypeDecl) => { t: TConcrete(td), span: span },
						at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
						_ => throw "bad"
					),
					_ => throw "bad"
				),
				at(c) => c
			);
			detuple(@final [names, args] = getNamesArgs(ctx, labels));
			
			var categories = t.t._match(
				at(TThis(td is TypeDecl)) => ({t: td.thisType.t, span: t.span} : Type),
				_ => t
			).findThisCategory(ctx, tcat, ctx.typeDecl).unique();
			categories._match(
				at([]) => throw 'error: type `${t.fullName()}` does not have the category `${tcat.fullName()}`!',
				at([found]) => found.findMultiStatic(ctx, names, ctx.typeDecl.thisType).unique().reduceBySender()._match(
					at([]) => throw 'error: type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in category `${tcat.fullName()}`! ${begin.display()}',
					at(kinds) => {
						kinds.reduceOverloads(t, args)._match(
							at([]) => {
								ctx.addError(Type_UnknownMethod(ctx, t, Multi(Static, names, args), labels[0].span(), [found]));
								null;
							},
							at(overloads) => {
								if(found.path.hasTypevars()) {
									final ctctx: TypeVarCtx = [];
									tcat.bindTo(found.path, ctctx);
									trace(found.path.fullName(), ctctx.display());
									if(ctctx.size() > 0) {
										for(ov in overloads) {
											ov.tctx._andOr(tctx => {
												for(tv => ty in ctctx) {
													tctx[tv] = ty;
												}
											}, {
												ov.tctx = ctctx.copy();
											});
											ov.ret = ov.ret.getInTCtx(ctctx);
										}
									}
								}

								tuple(
									Multi(overloads.simplify(), names, args),
									overloads.retType(t)
								);
							}
						);
					}
				),
				at(found) => Type.mostSpecificBy(
					Type.reduceOverloadsBy(
						found
							.map(f -> {cat: f, mth: f.findMultiStatic(ctx, names, ctx.typeDecl.thisType).unique().reduceBySender()})
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
						kinds.mth.reduceOverloads(t, args)._match(
							at([]) => {
								ctx.addError(Type_UnknownMethod(ctx, t, Multi(Static, names, args), labels[0].span(), found));
								null;
							},
							at(overloads) => {
								final cat = kinds.cat;
								if(true||cat.path.hasTypevars()) {
									final ctctx: TypeVarCtx = [];
									tcat.bindTo(cat.path, ctctx);
									
									if(ctctx.size() > 0) {
										for(ov in overloads) {
											ov.tctx._andOr(tctx => {
												for(tv => ty in ctctx) {
													tctx[tv] = ty;
												}
											}, {
												ov.tctx = ctctx.copy();
											});
											ov.ret = ov.ret.getInTCtx(ctctx);
										}
									}
								}

								tuple(
									Multi(overloads.simplify(), names, args),
									overloads.retType(t)
								);
							}
						);
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
}

static function sendObjMessage(ctx: Ctx, t: Type, begin: Span, end: Span, msg: UMessage<UExpr>): Null<Tuple2<ObjMessage, Null<Type>>> {
	return msg._match(
		at(Single(null, span, name)) => {
			t.t._match(
				at(TThis(td is TypeDecl)) => td.thisType,
				_ => t
			).findSingleInst(ctx, name, ctx.typeDecl)._match(
				at(kind!) => tuple(
					Single(kind),
					kind.retType()?.getFrom(t)
				),
				_ => {
					ctx.addError(Type_UnknownMethod(ctx, t, Single(Instance, name), span));
					null;
				}
			);
		},
		at(Single(cat = TSegs(Nil, Cons(NameParams(_, "Super", {of: [parent]}), Nil)), _, name)) => {
			final tparent: Type = ctx.getType(parent) ?? return null;
			if(t.hasParentType(tparent)||tparent.hasChildType(t)) {
				tparent.findSingleInst(ctx, name, ctx.typeDecl)._match(
					at(kind!) => tuple(
						Super(tparent, Single(kind)),
						kind.retType()?.getFrom(t)
					),
					_ => throw 'error: value of type `${t.fullName()}` does not have a supertype `${tparent.fullName()}` that responds to method `[$name]`! ${cat.span().display()}'
				);
			} else {
				throw 'error: value of type `${t.fullName()}` does not have supertype `${tparent.fullName()}`! ${cat.span().display()}';
			}
		},
		at(Single(cat!!, _, name)) => {
			final tcat: Type = (ctx.getType(cat) ?? return null)._match(
				/*at({t: TThis(source), span: span}) => source._match(
					at(td is TypeDecl) => { t: TConcrete(td), span: span },
					at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
					at(c is Category) => c.type ?? c.lookup._match(
						at(td is TypeDecl) => { t: TConcrete(td), span: span },
						at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
						_ => throw "bad"
					),
					_ => throw "bad"
				),*/
				at({t: TThis(source)}) => source.thisType.simplify(),
				at(c) => c.simplify()
			);
			var categories = t.t._match(
				at(TThis(td is TypeDecl)) => ({t: td.thisType.t, span: t.span} : Type),
				_ => t
			).findThisCategory(ctx, tcat, ctx.typeDecl).concat(
				tcat.findCategory(ctx, tcat, t, ctx.typeDecl)
			).unique().filter(c -> c.path.simplify().hasChildType(tcat));
			categories._match(
				at([]) => {
					ctx.addError(Type_UnknownCategory(ctx, Instance, t, tcat, cat.span()));
					null;
				},
				at([found]) => found.findSingleInst(ctx, name, ctx.typeDecl)._match(
					at(kind!) => tuple(
						Single(kind),
						kind.retType()?.getFrom(t)
					),
					_ => {
						trace(found.fullName());
						trace(tcat.fullName());
						trace(t.fullName());
						throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in category `${tcat.fullName()}`! ${begin.display()}';
					}
				),
				at(found) => found.filterMap(f -> f.findSingleInst(ctx, name, ctx.typeDecl))._match(
					at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`!',
					at([kind!]) => tuple(
						Single(kind),
						kind.retType()?.getFrom(t)
					),
					at(kinds) => SingleInstKind.reduceCategoryCalls(kinds)._match(
						at(null) => throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`!',
						at(kind!!) => {
							tuple(
								Single(kind),
								kind.retType()?.getFrom(t)
							);
						}
					)
				)
			);
		},
		
		at(Multi(null, labels)) => {
			detuple(@final [names, args] = getNamesArgs(ctx, labels));

			t.t._match(
				at(TThis(td is TypeDecl)) => ({t: td.thisType.t, span: t.span} : Type),
				_ => t
			).findMultiInst(ctx, names, ctx.typeDecl.thisType)._match(
				at([]) => {
					ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names), labels[0].span()));
					null;
				},
				at(kinds) => kinds.reduceOverloads(ctx, t, args)._match(
					at([]) => {
						ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names, args), labels[0].span()));
						null;
					},
					at(overloads) => tuple(
						Multi(overloads.simplify(), names, args),
						overloads.retType(t)
					)
				)
			);
		},
		at(Multi(TSegs(Nil, Cons(NameParams(_, "Super", {of: [parent]}), Nil)), labels)) => {
			final tparent = ctx.getType(parent) ?? return null.simplify();

			if(tparent.hasChildType(t)) {
				detuple(@final [names, args] = getNamesArgs(ctx, labels));

				tparent.findMultiInst(ctx, names, ctx.typeDecl.thisType).reduceBySender()._match(
					at([]) => throw 'error: value of type `${t.fullName()}` does not have a supertype `${tparent.fullName()}` that responds to method `[${names.joinMap(" ", n -> '$n:')}]`! ${begin.display()}',
					at(kinds) => {
						kinds.reduceOverloads(ctx, tparent, args)._match(
							at([]) => {
								ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names, args), labels[0].span(), null, tparent));
								null;
							},
							at(overloads) => tuple(
								Super(tparent, Multi(overloads.simplify(), names, args)),
								// TODO
								overloads.retType(t)
							)
						);
					}
				);
			} else {
				throw 'error: value of type `${t.fullName()}` does not have supertype `${tparent.fullName()}`!';
			}
		},
		at(Multi(cat!!, labels)) => {
			final tcat: Type = (ctx.getType(cat) ?? return null)._match(
				/*at({t: TThis(source), span: s}) => source._match(
					at(td is TypeDecl) => { t: TConcrete(td), span: s },
					at(tv is TypeVar) => { t: TTypeVar(tv), span: s },
					at(c is Category) => c.type ?? c.lookup._match(
						at(td is TypeDecl) => { t: TConcrete(td), span: s },
						at(tv is TypeVar) => { t: TTypeVar(tv), span: s },
						_ => throw "bad"
					),
					_ => throw "bad"
				),*/
				//at({t: TThis(source)}) => source.thisType.simplify(),
				at(c) => c.simplify()
			);
			detuple(@final [names, args] = getNamesArgs(ctx, labels));

			var categories = t.t._match(
				at(TThis(td is TypeDecl)) => td.thisType,
				_ => t
			).findThisCategory(ctx, tcat, ctx.typeDecl).concat(
				tcat.findCategory(ctx, tcat, t, ctx.typeDecl)
			).unique().filter(c -> c.path.simplify().hasChildType(tcat))/*.filter(c -> {
				if(t.hasParentType(c.thisType) && c.thisType.hasChildType(t)) {
					true;
				} else {
					//trace(t.fullName(), c.fullName(), t.hasParentType(c.thisType), c.thisType.hasChildType(t));
					t.hasParentType(c.thisType);
				}
			})*/; // BUG: hasParentType has false positives?!?!!??!?!?!!?!?! probably related to refinements
			//trace(t.fullName(), tcat.fullName(), categories.map(c->c.fullName()));
			//trace(categories.map(c->c.fullName()), begin.display());
			categories._match(
				at([]) => {
					trace(1);
					ctx.addError(Type_UnknownCategory(ctx, Instance, t, tcat, cat.span()));
					null;
				},
				at([found]) => found.findMultiInst(ctx, names, ctx.typeDecl.thisType).reduceCategoryCalls()._match(
					at([]) => {
						trace(2);
						ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names), labels[0].span(), categories));
						null;
					},
					at(kinds) => kinds.reduceOverloads(ctx, t, args)._match(
						at([]) => {
							trace(3);
							ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names, args), labels[0].span(), categories));
							null;
						},
						at(overloads) => {
							if(found.path.hasTypevars()) {
								final ctctx: TypeVarCtx = [];
								tcat.bindTo(found.path, ctctx);
								
								if(ctctx.size() > 0) {
									for(ov in overloads) {
										ov.tctx._andOr(tctx => {
											for(tv => ty in ctctx) {
												tctx[tv] = ty;
											}
										}, {
											ov.tctx = ctctx.copy();
										});
										ov.ret = ov.ret.getInTCtx(ctctx);
									}
								}
							}

							tuple(
								Multi(overloads.simplify(), names, args),
								overloads.map(ov -> {
									final res = ov.ret.getFrom(t);
									final tctx: TypeVarCtx = ov.tctx ?? new TypeVarCtx();
									/*trace(
										tctx.display(),
										t.bindTo(ov.kind.getMethodOwner(), tctx)._and(ty=>ty.fullName()),
										tctx.display()
									);*/
									t.bindTo(ov.kind.getMethodOwner(), tctx);
									res.getInTCtx(tctx).getFrom(t.getInTCtx(tctx));
								}).unique()._match(
									at([]) => null,
									at([ret]) => ret,
									at(rets) => {t: TMulti(rets), span: null}
								)
							);
						}
					)
				)/*found.findMultiInst(ctx, names, ctx.typeDecl).reduceBySender()._match(
					at([]) => {
						ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names), labels[0].span(), categories));
						null;
					},
					at(kinds) => tuple(
						Multi(kinds, names, args),
						null
					)
				)*/,
				_ => Type.mostSpecificBy(
					Type.reduceOverloadsBy(
						categories
							.map(f -> {cat: f, mth: f.findMultiInst(ctx, names, ctx.typeDecl.thisType)})
							.filter(l -> l.mth.length != 0),
						f -> f.cat.thisType.getMostSpecific()
					),
					f -> {
						function loop(kind: MultiInstKind) return kind._match(
							at(MIMethod(m, _)) => m.decl.thisType.getMostSpecific(),
							at(MIMember(m)) => m.decl.thisType.getMostSpecific(),
							at(MIFromTypevar(tv, _, _, _)) => tv.thisType.getMostSpecific(),
							at(MIFromParent(parent, kind2 = MIFromParent(_, _))) => loop(kind2).getFrom(parent.simplify()),
							at(MIFromParent(parent, _)) => parent.simplify()
						);
						loop(f.mth[0]);
					}
				)._match(
					at([]) => {
						ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names), begin, categories));
						null;
					},
					at([kinds]) => kinds.mth.reduceOverloads(ctx, t, args)._match(
						at([]) => {
							ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names, args), labels[0].span(), categories));
							null;
						},
						at(overloads) => {
							final cat = kinds.cat;
							if(cat.path.hasTypevars()) {
								final ctctx: TypeVarCtx = [];
								tcat.bindTo(cat.path, ctctx);
								
								if(ctctx.size() > 0) {
									for(ov in overloads) {
										ov.tctx._andOr(tctx => {
											for(tv => ty in ctctx) {
												tctx[tv] = ty;
											}
										}, {
											ov.tctx = ctctx.copy();
										});
										ov.ret = ov.ret.getInTCtx(ctctx);
									}
								}
							}

							tuple(
								Multi(overloads.simplify(), names, args),
								overloads.map(ov -> {
									var res = ov.ret.getFrom(t);
									var tcat = kinds.cat.thisType.getLeastSpecific().simplify();
									final tctx:TypeVarCtx = ov.tctx ?? new TypeVarCtx();
									res = res.getInTCtx(tctx);
									//trace(t.fullName());
									//trace(tcat.fullName());
									var t2=t.bindTo(tcat, tctx);
									t2._match(
										at(null) => {},//null,
										at(res={t: TInstance(decl, params, tctx2)}) => {
											final rs = [for(ref in decl.refinees) {
												ref.applyArgs(params).nonNull()._match(
													at({t: TInstance(_, _, tctx3)}) => tctx3,
													_ => throw "bad"
												);
											}];

											for(r in rs) {
												//trace(r.display());
												for(k => v in r) {
													tctx2[k] = v;
												}
											}
											for(k => v in tctx) {
												var v2 = v.getInTCtx(tctx2);
												//for(r in rs) v2 = v2.getInTCtx(r);
												tctx[k] = v2;
											}
											for(k=>v in tctx2) {
												tctx[k]=v;
											}
											//res.fullName() + " " + tctx2.display();
										},
										at(res) => {}//res.fullName()
									);
									
									//res = res.getInTCtx(tctx);
									t2._and(ty2 => {
										tcat = tcat.getFrom(ty2);
										res = res.getFrom(ty2).getFrom(tcat);
									});
									res=res.getFrom(t.getInTCtx(tctx));
									res.t._match(
										at(TTypeVar(tv)) => {
											tctx[tv]._and(t => {
												t.t._match(
													at(TTypeVar({lookup: _ is Category})) => {
														trace("?!?!?!?!?!?!?!");
													},
													_ => {}
												);
												//trace("!!!", t);
												res = t;
											});
										},
										_ => {}
									);
									//trace(res.fullName());
									//Sys.println("");
									res;
								}).unique()._match(
									at([]) => null,
									at([ret]) => ret,
									at(rets) => {t: TMulti(rets), span: null}
								)
							);
						}
					),
					at(kinds) => if(kinds.every(k -> k.mth.length == 0)) {
						throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${categories.map(f -> f.fullName()).join(", ")}`! ${begin.display()}';
					} else {
						throw "todo ["+Type.mostSpecificBy(kinds, f->f.cat.thisType).map(f->f.cat.fullName()+"#"+f.mth).join(", ")+"]";
					}
				)
			);
		},

		at(Cast(null, type)) => {
			final target = (ctx.getType(type) ?? return null).getLeastSpecific();
			t.findCast(ctx, target, ctx.typeDecl.thisType)._match(
				at([]) => {
					ctx.addError(Type_UnknownCast(ctx, t, target, begin.union(end)));
					null;
				},
				at(casts) => tuple(
					Cast(target, casts.unique().reduceOverloads(t, target)),
					target
				)
			);
		},
		at(Cast(cat = TSegs(Nil, Cons(NameParams(_, "Super", {of: [parent]}), Nil)), type)) => {
			final tparent: Type = (ctx.getType(parent) ?? return null).simplify();

			if(tparent.hasChildType(t)) {
				final target = (ctx.getType(type) ?? return null).getLeastSpecific();
				tparent.findCast(ctx, target, ctx.typeDecl.thisType)._match(
					at([]) => {
						throw 'error: value of type `${t.fullName()}` does not have a supertype `${tparent.fullName()}` that can be cast to type `${target.fullName()}`! ${begin.display()}';
					},
					at(casts) => tuple(
						Super(tparent, Cast(target, casts.unique().reduceOverloads(t, target))),
						target
					)
				);
			} else {
				throw 'error: value of type `${t.fullName()}` does not have supertype `${tparent.fullName()}`! ${cat.span().display()}';
			}
		},
		at(Cast(cat!!, type)) => {
			final target = (ctx.getType(type) ?? return null).getLeastSpecific();
			final tcat: Type = (ctx.getType(cat) ?? return null)._match(
				/*at({t: TThis(source), span: span}) => source._match(
					at(td is TypeDecl) => { t: TConcrete(td), span: span },
					at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
					at(c is Category) => c.type ?? c.lookup._match(
						at(td is TypeDecl) => { t: TConcrete(td), span: span },
						at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
						_ => throw "bad"
					),
					_ => throw "bad"
				),*/
				at({t: TThis(source)}) => source.thisType.simplify(),
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
				at([found]) => found.findCast(ctx, target, ctx.typeDecl.thisType)._match(
					at([]) => {
						throw 'error: value of type `${t.fullName()}` cannot be cast to type `${target.fullName()}` in category `${found.fullName()}`! ${begin.display()}';
					},
					at(casts) => tuple(
						Cast(target, casts),
						target
					)
				),
				at(found) => found
					.filterMap(f -> f.findCast(ctx, target, ctx.typeDecl.thisType).unique().reduceOverloads(t, target))
					.filter(c -> c.length != 0)
				._match(
					at([]) => throw 'error: value of type `${t.fullName()}` cannot be cast to type `${target.fullName()}` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${begin.display()}',
					at([kind!]) => tuple(
						Cast(target, kind),
						target
					),
					at(kinds) => throw "todo"
				)
			);
		}
	);
}

static function typeMessage(ctx: Ctx, msg: UMessage<UExpr>): Message<TExpr> {
	return switch msg {
		case Single(cat, _, name): Single(cat._and(c => ctx.getType(c)), name);

		case Multi(cat, labels):
			detuple(@final [names, args] = getNamesArgs(ctx, labels));
			Multi(cat._and(c => ctx.getType(c)), names, args);

		case Cast(cat, type): Cast(cat._and(c => ctx.getType(c)), ctx.getType(type));
	}
}

static function typeTMessage(ctx: Ctx, msg: UMessage<UType>): Message<Type> {
	return switch msg {
		case Single(cat, _, name): Single(cat._and(c => ctx.getType(c)), name);

		case Multi(cat, labels):
			detuple(@final [names, args] = getNamesArgs(ctx, labels));
			Multi(cat._and(c => ctx.getType(c)), names, args);
	}
}

static function typeTypeCascade(ctx: Ctx, type: Type, cascade: UCascade<UType>): Null<TypeCascade> {
	final cascadeCtx = ctx.innerTypeCascade(type);

	var ret: Null<Type> = null;
	final kind: TypeCascade.TypeCascadeKind =
		cascade.kind._match(
			at(Member({name: name})) => type.findSingleStatic(ctx, name, ctx.typeDecl.thisType, true)._match(
				at(kind!) => {
					ret = kind.retType()?.getFrom(type);
					Member(Single(kind));
				},
				_ => throw 'error: value of type `${type.fullName()}` does not have member/getter `$name`!'
			),
			
			// TODO: replace with sendObjMessage
			at(Message(msg)) => Message(msg._match(
				at(Single(null, _, name)) => type.findSingleStatic(ctx, name, ctx.typeDecl.thisType)._andOr(
					kind => {
						ret = kind.retType()?.getFrom(type);
						Single(kind);
					},
					throw 'error: value of type ${type.fullName()} does not respond to method `[$name]`!'
				),
				at(Single(cat!!, span, name)) => {
					final tcat: Type = (ctx.getType(cat) ?? return null)._match(
						at({t: TThis(source), span: span}) => source._match(
							at(td is TypeDecl) => { t: TConcrete(td), span: span },
							at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
							at(c is Category) => c.type ?? c.lookup._match(
								at(td is TypeDecl) => { t: TConcrete(td), span: span },
								at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
								_ => throw "bad"
							),
							_ => throw "bad"
						),
						at(c) => c
					);
					
					var categories = type.t._match(
						at(TThis(td is TypeDecl)) => ({t: td.thisType.t, span: type.span} : Type),
						_ => type
					).findThisCategory(ctx, tcat, ctx.typeDecl).unique();
					categories._match(
						at([]) => throw 'error: value of type `${type.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
						at([found]) => found.findSingleStatic(ctx, name, ctx.typeDecl.thisType)._match(
							at(kind!) => {
								ret = kind.retType()?.getFrom(type);
								Single(kind);
							},
							_ => throw 'error: value of type `${type.fullName()}` does not respond to method `[$name]` in category `${tcat.fullName()}`!'
						),
						at(found) => found.filterMap(f -> f.findSingleStatic(ctx, name, ctx.typeDecl.thisType))._match(
							at([]) => {
								ctx.addError(Type_UnknownMethod(ctx, type, Single(Static, name), span, found));
								null;
							},//throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`!',
							at([kind!]) => {
								ret = kind.retType()?.getFrom(type);
								Single(kind);
							},
							at(kinds) => throw "todo"
						)
					);
				},

				at(Multi(null, labels)) => {
					detuple(@final [names, args] = getNamesArgs(ctx, labels));
					type.findMultiStatic(ctx, names, ctx.typeDecl.thisType)._match(
						at([]) => {
							ctx.addError(Type_UnknownMethod(ctx, type, Multi(Static, names), labels[0].span()));
							return null;
						},
						at(kinds) => kinds.reduceOverloads(type, args)._match(
							at([]) => {
								ctx.addError(Type_UnknownMethod(ctx, type, Multi(Static, names, args), labels[0].span()));
								return null;
							},
							at(overloads) => {
								ret = overloads.retType(type);
								Multi(overloads.simplify(), names, args);
							}
						)
					);
				},
				// TODO: ret type for multi category calls
				at(Multi(cat!!, labels)) => {
					final tcat: Type = (ctx.getType(cat) ?? return null)._match(
						at({t: TThis(source), span: span}) => source._match(
							at(td is TypeDecl) => { t: TConcrete(td), span: span },
							at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
							at(c is Category) => c.type ?? c.lookup._match(
								at(td is TypeDecl) => { t: TConcrete(td), span: span },
								at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
								_ => throw "bad"
							),
							_ => throw "bad"
						),
						at(c) => c
					);
					detuple(@var [names, args] = getNamesArgs(ctx, labels));

					var categories = type.t._match(
						at(TThis(td is TypeDecl)) => td.thisType,
						_ => type
					).findThisCategory(ctx, tcat, ctx.typeDecl).unique();
					categories._match(
						at([]) => throw 'error: value of type `${type.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
						at([found]) => found.findMultiStatic(ctx, names, ctx.typeDecl.thisType)._match(
							at([]) => throw 'error: value of type `${type.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in category `${tcat.fullName()}`! ${cat.span().display()}',
							at(kinds) => kinds.reduceOverloads(type, args)._match(
								at([]) => {
									ctx.addError(Type_UnknownMethod(ctx, type, Multi(Static, names, args), labels[0].span(), categories));
									return null;
								},
								at(overloads) => Multi(overloads.simplify(), names, args)
							)
						),
						at(found) => Type.mostSpecificBy(
							found
								.map(f -> {cat: f, mth: f.findMultiStatic(ctx, names, ctx.typeDecl.thisType)})
								.filter(l -> l.mth.length != 0),
							f -> f.cat.type.nonNull()
						)._match(
							at([kinds]) => kinds.mth.reduceOverloads(type, args)._match(
								at([]) => {
									ctx.addError(Type_UnknownMethod(ctx, type, Multi(Static, names, args), labels[0].span(), categories));
									return null;
								},
								at(overloads) => Multi(overloads.simplify(), names, args)
							),
							at([]) => throw 'error: value of type `${type.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${cat.span().display()}',
							at(kinds) => if(kinds.every(k -> k.mth.length == 0)) {
								throw 'error: value of type `${type.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${cat.span().display()}';
							} else {
								throw "todo ["+kinds.map(f->f.cat.fullName()+"#"+f.mth).join(", ")+"]";
							}
						)
					);
				}
			)),


			at(AssignMember({name: name, span: span}, _, null, rhs)) => type.findMultiStatic(ctx, [name], ctx.typeDecl.thisType, true)._andOr(
				kinds => {
					final trhs = typeExpr(ctx, rhs);
					kinds.reduceOverloads(type, [trhs])._match(
						at([]) => {
							ctx.addError(Type_UnknownSetter(ctx, Static, type, name, span, trhs));
							return null;
						},
						at(overloads) => {
							Member(Multi(overloads.simplify(), [name], [trhs]));
						}
					);
				},
				throw 'error: value of type `${type.fullName()}` does not have member/setter `$name`!'
			),
			at(AssignMember({name: name}, _, op!!, rhs)) => throw "todo",
			
			at(AssignMessage(msg, _, null, rhs)) => msg._match(
				at(Single(cat, _, name)) => throw "todo",
				at(Multi(null, labels)) => {
					detuple(@var [names, args] = getNamesArgs(ctx, labels));
					names = names.concat(["="]);
					type.findMultiStatic(ctx, names, ctx.typeDecl.thisType)._match(
						at([]) => throw 'error: value of type ${type.fullName()} does not respond to method `[${names.joinMap(" ", n -> '$n:')}]`!',
						at(kinds) => {
							final trhs = typeExpr(ctx, rhs);
							final args2 = args.concat([trhs]);
							kinds.reduceOverloads(type, args2)._match(
								at([]) => {
									ctx.addError(Type_UnknownMethod(ctx, type, Multi(Static, names, args2), labels[0].span()));
									return null;
								},
								at(overloads) => {
									AssignMessage(Multi(overloads.simplify(), names, args2), null, trhs);
								}
							);
						}
					);
				},
				at(Multi(cat!!, labels)) => throw "todo"
			),
			at(AssignMessage(msg, _, op!!, rhs)) => throw "todo",


			at(StepMember({name: name}, span, step)) =>
				type.findMultiStatic(ctx, [name], ctx.typeDecl.thisType, true)._match(
					at([]) => throw "bad",
					at([setKind]) => type.findSingleStatic(ctx, name, ctx.typeDecl.thisType, true)._andOr(
						getKind => {
							final memt = getKind._match(
								at(SSMethod((_ : StaticMethod) => m) | SSMultiMethod(m)) => m.ret.nonNull(),
								at(SSMember(m)) => m.type.nonNull(),
								_ => throw "todo"
							);
							final op: UnaryOp = step._match(at(Incr) => Incr, at(Decr) => Decr);
							memt.findUnaryOp(ctx, op, ctx.typeDecl)._match(
								at(opKind!) => StepMember(setKind, getKind, opKind),
								_ => {
									ctx.addError(Type_UnknownMethod(ctx, memt, Unary(op), span));
									return null;
								}
							);
						},
						throw "bad"
					),
					at(setKinds) => throw "todo"
				),

			at(StepMessage(msg, _, step)) => throw "todo!",
			
			at(Block(blk)) => {
				final blkCtx = cascadeCtx.innerBlock();
				Block(blkCtx, typeBlock(blkCtx, blk));
			}
		);

	return {
		ctx: ctx,
		t: ret,
		depth: cascade.depth,
		kind: kind,
		nested: cascade.nested.filterMap(c -> typeObjCascade(cascadeCtx, ret, c))
	};
}

static function typeObjCascade(ctx: Ctx, type: Null<Type>, cascade: UCascade<UExpr>): Null<ObjCascade> {
	final cascadeCtx = ctx.innerCascade(type);

	var ret: Null<Type> = null;
	final kind: ObjCascade.ObjCascadeKind = type._match(
		at(t!) => cascade.kind._match(
			at(Member({name: name})) => t.findSingleInst(ctx, name, ctx.typeDecl, true)._andOr(
				kind => {
					ret = kind.retType()?.getFrom(t);
					Member(Single(kind));
				},
				throw 'error: value of type `${t.fullName()}` does not have member/getter `$name`!'
			),
			
			// TODO: replace with sendObjMessage
			at(Message(msg)) => Message(msg._match(
				at(Single(null, _, name)) => t.findSingleInst(ctx, name, ctx.typeDecl)._andOr(
					kind => {
						ret = kind.retType()?.getFrom(t);
						Single(kind);
					},
					throw 'error: value of type ${t.fullName()} does not respond to method `[$name]`!'
				),
				at(Single(cat!!, span, name)) => {
					final tcat: Type = (ctx.getType(cat) ?? return null)._match(
						at({t: TThis(source), span: span}) => source._match(
							at(td is TypeDecl) => { t: TConcrete(td), span: span },
							at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
							at(c is Category) => c.type ?? c.lookup._match(
								at(td is TypeDecl) => { t: TConcrete(td), span: span },
								at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
								_ => throw "bad"
							),
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
						at([found]) => found.findSingleInst(ctx, name, ctx.typeDecl)._andOr(
							kind => {
								ret = kind.retType()?.getFrom(t);
								Single(kind);
							},
							throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in category `${tcat.fullName()}`!'
						),
						at(found) => found.filterMap(f -> f.findSingleInst(ctx, name, ctx.typeDecl))._match(
							at([]) => {
								ctx.addError(Type_UnknownMethod(ctx, t, Single(Instance, name), span, found));
								null;
							},//throw 'error: value of type `${t.fullName()}` does not respond to method `[$name]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`!',
							at([kind!]) => {
								ret = kind.retType()?.getFrom(t);
								Single(kind);
							},
							at(kinds) => throw "todo"
						)
					);
				},

				at(Multi(null, labels)) => {
					detuple(@final [names, args] = getNamesArgs(ctx, labels));
					t.findMultiInst(ctx, names, ctx.typeDecl.thisType)._match(
						at([]) => {
							ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names), labels[0].span()));
							return null;
						},
						at(kinds) => kinds.reduceOverloads(ctx, t, args)._match(
							at([]) => {
								ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names, args), labels[0].span()));
								return null;
							},
							at(overloads) => {
								ret = overloads.retType(t);
								Multi(overloads.simplify(), names, args);
							}
						)
					);
				},
				// TODO: ret type for multi category calls
				at(Multi(cat!!, labels)) => {
					final tcat: Type = (ctx.getType(cat) ?? return null)._match(
						at({t: TThis(source), span: span}) => source._match(
							at(td is TypeDecl) => { t: TConcrete(td), span: span },
							at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
							at(c is Category) => c.type ?? c.lookup._match(
								at(td is TypeDecl) => { t: TConcrete(td), span: span },
								at(tv is TypeVar) => { t: TTypeVar(tv), span: span },
								_ => throw "bad"
							),
							_ => throw "bad"
						),
						at(c) => c
					);
					detuple(@final [names, args] = getNamesArgs(ctx, labels));

					var categories = t.t._match(
						at(TThis(td is TypeDecl)) => td.thisType,
						_ => t
					).findThisCategory(ctx, tcat, ctx.typeDecl).unique();
					categories._match(
						at([]) => throw 'error: value of type `${t.fullName()}` does not have the category `${tcat.fullName()}`! ${cat.span().display()}',
						at([found]) => found.findMultiInst(ctx, names, ctx.typeDecl.thisType)._match(
							at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in category `${tcat.fullName()}`! ${cat.span().display()}',
							at(kinds) => kinds.reduceOverloads(ctx, t, args)._match(
								at([]) => {
									ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names, args), labels[0].span(), categories));
									return null;
								},
								at(overloads) => Multi(overloads.simplify(), names, args)
							)
						),
						at(found) => Type.mostSpecificBy(
							found
								.map(f -> {cat: f, mth: f.findMultiInst(ctx, names, ctx.typeDecl.thisType)})
								.filter(l -> l.mth.length != 0),
							f -> f.cat.type.nonNull()
						)._match(
							at([kinds]) => kinds.mth.reduceOverloads(ctx, t, args)._match(
								at([]) => {
									ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names, args), labels[0].span(), categories));
									return null;
								},
								at(overloads) => Multi(overloads.simplify(), names, args)
							),
							at([]) => throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${cat.span().display()}',
							at(kinds) => if(kinds.every(k -> k.mth.length == 0)) {
								throw 'error: value of type `${t.fullName()}` does not respond to method `[${names.joinMap(" ", n -> '$n:')}]` in any categories of: `${found.map(f -> f.fullName()).join(", ")}`! ${cat.span().display()}';
							} else {
								throw "todo ["+kinds.map(f->f.cat.fullName()+"#"+f.mth).join(", ")+"]";
							}
						)
					);
				},

				at(Cast(null, ty)) => {
					final target = ctx.getType(ty) ?? return null;
					t.findCast(ctx, target, ctx.typeDecl.thisType)._match(
						at([]) => throw 'error: value of type ${t.fullName()} cannot be cast to type `${target.fullName()}`!',
						at(casts) => {
							ret = target;
							Cast(target, casts);
						}
					);
				},
				at(Cast(cat!!, ty)) => throw "todo"
			)),


			at(AssignMember({name: name, span: span}, _, null, rhs)) => t.findMultiInst(ctx, [name], ctx.typeDecl.thisType, true)._andOr(
				kinds => {
					final trhs = typeExpr(ctx, rhs);
					kinds.reduceOverloads(ctx, t, [trhs])._match(
						at([]) => {
							ctx.addError(Type_UnknownSetter(ctx, Instance, t, name, span, trhs));
							return null;
						},
						at(overloads) => {
							Member(Multi(overloads.simplify(), [name], [trhs]));
						}
					);
				},
				throw 'error: value of type `${t.fullName()}` does not have member/setter `$name`!'
			),
			at(AssignMember({name: name}, _, op!!, rhs)) => throw "todo",
			
			at(AssignMessage(msg, _, null, rhs)) => msg._match(
				at(Single(cat, _, name)) => throw "todo",
				at(Multi(null, labels)) => {
					detuple(@var [names, args] = getNamesArgs(ctx, labels));
					names = names.concat(["="]);
					t.findMultiInst(ctx, names, ctx.typeDecl.thisType)._match(
						at([]) => throw 'error: value of type ${t.fullName()} does not respond to method `[${names.joinMap(" ", n -> '$n:')}]`!',
						at(kinds) => {
							final trhs = typeExpr(ctx, rhs);
							final args2 = args.concat([trhs]);
							kinds.reduceOverloads(ctx, t, args2)._match(
								at([]) => {
									ctx.addError(Type_UnknownMethod(ctx, t, Multi(Instance, names, args2), labels[0].span()));
									return null;
								},
								at(overloads) => {
									AssignMessage(Multi(overloads.simplify(), names, args2), null, trhs);
								}
							);
						}
					);
				},
				at(Multi(cat!!, labels)) => throw "todo",
				at(Cast(_, _)) => throw "bad"
			),
			at(AssignMessage(msg, _, op!!, rhs)) => throw "todo",


			at(StepMember({name: name}, span, step)) =>
				t.findMultiInst(ctx, [name], ctx.typeDecl.thisType, true)._match(
					at([]) => throw "bad",
					at([setKind]) => t.findSingleInst(ctx, name, ctx.typeDecl, true)._match(
						at(getKind!) => {
							final memt = getKind._match(
								at(SIMethod((_ : Method) => m) | SIMultiMethod(m)) => m.ret.nonNull(),
								at(SIMember(m)) => m.type.nonNull(),
								_ => throw "todo"
							);
							final op: UnaryOp = step._match(at(Incr) => Incr, at(Decr) => Decr);
							memt.findUnaryOp(ctx, op, ctx.typeDecl)._andOr(
								opKind => StepMember(setKind, getKind, opKind),
								{
									ctx.addError(Type_UnknownMethod(ctx, memt, Unary(op), span));
									return null;
								}
							);
						},
						_ => throw "bad"
					),
					at(setKinds) => throw "todo"
				),

			at(StepMessage(msg, _, step)) => throw "todo!",
			
			at(Block(blk)) => {
				final blkCtx = ctx.innerCascade(t);
				Block(blkCtx, typeBlock(blkCtx, blk));
			}
		),
		_ => Lazy(switch cascade.kind {
			case Member(mem): Member(mem.name);
			case Message(msg): Message(typeMessage(ctx, msg));
			case AssignMember(mem, _, op, rhs): AssignMember(mem.name, op, typeExpr(ctx, rhs));
			case AssignMessage(msg, _, op, rhs): AssignMessage(typeMessage(ctx, msg), op, typeExpr(ctx, rhs));
			case StepMember(mem, _, step): StepMember(mem.name, step);
			case StepMessage(msg, _, step): StepMessage(typeMessage(ctx, msg), step);
			case Block(blk): Block(typeBlock(cascadeCtx, blk));
		})
	);

	return {
		ctx: ctx,
		t: ret,
		depth: cascade.depth,
		kind: kind,
		nested: cascade.nested.filterMap(c -> typeObjCascade(cascadeCtx, ret, c))
	};
}


static function typePattern(ctx: Ctx, expectType: Type, expr: UExpr): Pattern {
	final pattern: Pattern.PatternKind = expr._match(
		at(EArray(_begin, values, _)) => {
			final elemType = expectType.iterElemType() ?? {
				trace("error: cannot use array pattern on type `"+expectType.fullName()+"`!");
				({t: TBlank, span: _begin}:Type);
			};

			PArray([for(value in values) {
				value._match(
					at(EPrefix(_, PSpread, right)) => {
						final p = typePattern(ctx, expectType, right);
						({p: PSpread(p), t: p.t, orig: value}:Pattern);
					},
					at(ESuffix(left, _, STruthy)) => {
						final p = typePattern(ctx, elemType, left);
						({p: POptional(p), t: p.t, orig: value}:Pattern);
					},
					_ => {
						typePattern(ctx, elemType, value);
					}
				);
			}]);
		},
		
		at(ETuple(_begin, values, _)) => {
			if(expectType.hasParentType(STD_Tuple) || STD_Tuple.hasChildType(expectType) || STD_Tuple.strictUnifyWithType(expectType)!=null) {
				final members = expectType.instMembers(ctx.typeDecl);
				if(members.length == values.length) {
					PTuple([
						for(i => mem in members)
							typePattern(ctx, mem.type.nonNull().getFrom(expectType), values[i])
					]);
				} else {
					throw 'bad ${expectType.fullName()} ${members.length} ${values.length} at '+_begin.display();
				}
			} else {
				throw 'bad ${expectType.fullName()} ${STD_Tuple.strictUnifyWithType(expectType)} at '+_begin.display();
			}
		},
		
		at(EWildcard(_)) => PIgnore,
		
		at(ELiteralCtor(type, literal)) => {
			literal._match(
				at(EArray(_, values, _)) => throw "todo",
				at(ETuple(_, values, _)) => {
					final ttype = ctx.getType(type).nonNull();
					final members = ttype.instMembers(ctx.typeDecl);
					if(members.length == values.length) {
						PTypeTuple(ttype, [
							for(i => mem in members)
								typePattern(ctx, mem.type.nonNull().getFrom(ttype), values[i])
						]);
					} else {
						throw "bad";
					}
				},
				_ => PExpr(typeExpr(ctx, expr))
			);
		},
		
		at(EParen(_, [expr1], _)) => return typePattern(ctx, expectType, expr1),
		
		at(ETypeMessage(type, _, msg, _)) => {
			final ttype = ctx.getType(type).nonNull();
			msg._match(
				at(Single(null, _, name)) => {
					ttype.findSingleStatic(ctx, name, ctx.typeDecl.thisType)._match(
						at(SSValueCase(vcase)) => PTypeValueCase(ttype, vcase),
						at(SSTaggedCase(tcase)) => PTypeTaggedCaseSingle(ttype, tcase),
						at(SSTaggedCaseAlias(tcase is SingleTaggedCase)) =>
							PTypeTaggedCaseSingle(ttype, tcase),
						at(SSTaggedCaseAlias(tcase is MultiTaggedCase)) =>
							PTypeTaggedCaseMulti(ttype, tcase,
								[for(p in tcase.params) ({p: PIgnore, t: p.type.getFrom(expectType)}:Pattern)]),
						at(SSFromTypevar(tvar, _, _, kind)) => throw "todo",
						_ => PExpr(typeExpr(ctx, expr))
					);
				},
				at(Multi(null, labels)) => {
					detuple(@var [names, args] = getNamesUntypedArgs(ctx, labels));
					ttype.findMultiStatic(ctx, names, ctx.typeDecl.thisType)._match(
						at([MSMemberwiseInit(ms)]) => {
							// TODO: fix this to work with generic subtypes
							PTypeMembers(ttype, [
								for(i => m in ms)
									tuple(m, typePattern(ctx, m.type.nonNull().getFrom(ttype).getFrom(expectType), args[i]))
							]);
						},
						at([MSTaggedCase([], tcase, [])]) => {
							final args2 = new Array<Pattern>();
							for(i => p in tcase.params) {
								args2.push(typePattern(ctx, p.type.getFrom(expectType), args[i]));
							}

							PTypeTaggedCaseMulti(
								if(ttype.hasParentType(expectType)) {t: expectType.t, span: ttype.span} else ttype,
								tcase,
								args2
							);
						},
						at([MSTaggedCase(ms1, tcase, ms2)]) => {
							final ms3: Array<Tuple2<Member, Pattern>> = [];
							var i = 0;
							for(mem in ms1) {
								ms3.push(tuple(mem, typePattern(ctx, mem.type.nonNull().getFrom(expectType), args[i])));
								i++;
							}

							final args2 = new Array<Pattern>();
							for(p in tcase.params) {
								args2.push(typePattern(ctx, p.type.getFrom(expectType), args[i]));
								i++;
							}
							
							for(mem in ms2) {
								ms3.push(tuple(mem, typePattern(ctx, mem.type.nonNull().getFrom(expectType), args[i])));
								i++;
							}

							PTypeTaggedCaseMembersMulti(ttype, tcase, args2, ms3);
						},
						at([MSTaggedCaseAlias(tcase)]) => {
							throw "todo!";
						},
						at([MSFromTypevar(tvar, _, _, kind)]) => throw "todo",
						_ => PExpr(typeExpr(ctx, expr))
					);
				},
				_ => PExpr(typeExpr(ctx, expr))
			);
		},
		
		at(ETypeMember(type, {name: name})) => {
			final ttype = ctx.getType(type).nonNull();
			ttype.findSingleStatic(ctx, name, ctx.typeDecl.thisType, true)._match(
				at(SSValueCase(vcase)) => PTypeValueCase(ttype, vcase),
				_ => PExpr(typeExpr(ctx, expr))
			);
		},
		
		at(EObjMessage(EWildcard(_), begin, msg, end)) => {
			sendObjMessage(ctx, expectType, begin, end, msg)._match(
				at(null) => {
					PIgnore;
				},
				at(tuple(msg2, type)) => {
					return {
						p: PExtractMessage(msg2),
						t: type,
						orig: expr
					};
				}
			);
		},
		
		at(EPrefix(_, PNot, right)) => PNot(typePattern(ctx, expectType, right)),
		at(EPrefix(_, PCompl, right)) => {
			final pright = typePattern(ctx, {t: TBlank, span: null}, right);
			if(pright.isMultiKind()) {
				PComplement(pright);
			} else {
				PExpr(typeExpr(ctx, expr));
			}
		},
		at(EPrefix(span, PSpread, right)) => {
			ctx.addError(Type_ArrayPatternNotAllowed(ctx, span));
			PIgnore;
		},
		
		at(ESuffix(left, span, STruthy)) => {
			ctx.addError(Type_ArrayPatternNotAllowed(ctx, span));
			PIgnore;
		},
		
		at(EInfix(EInfix(left, _, op1 = Lt | Le, middle), _, op2 = Lt | Le, right)) => {
			final tleft = typePattern(ctx, expectType, left);
			PBoundsMinMax(
				tleft,
				op1.match(Lt) ? Exclusive : Inclusive,
				typePattern(ctx, tleft.t ?? expectType, middle),
				typePattern(ctx, expectType, right),
				op2.match(Lt) ? Exclusive : Inclusive
			);
		},
		at(EInfix(EInfix(left, _, op1 = Gt | Ge, middle), _, op2 = Gt | Ge, right)) =>
			PBoundsMinMax(
				typePattern(ctx, expectType, right),
				op2.match(Gt) ? Exclusive : Inclusive,
				typePattern(ctx, expectType, middle),
				typePattern(ctx, expectType, left),
				op1.match(Gt) ? Exclusive : Inclusive
			),
		at(EInfix(EInfix(_, span1, Gt | Ge, _), _, Lt | Le, _)
		| EInfix(EInfix(_, span1, Lt | Le, _), _, Gt | Ge, _)) => {
			throw "invalid ordering!";
			PIgnore;
		},
		at(EInfix(left = EWildcard(_) | EVarDecl(_, _, _, _), _, op = Lt | Le, right)) =>
			PBoundsMax(
				typePattern(ctx, expectType, left),
				typePattern(ctx, expectType, right),
				op.match(Lt) ? Exclusive : Inclusive
			),
		at(EInfix(left = EWildcard(_) | EVarDecl(_, _, _, _), _, op = Gt | Ge, right)) =>
			PBoundsMin(
				typePattern(ctx, expectType, right),
				op.match(Gt) ? Exclusive : Inclusive,
				typePattern(ctx, expectType, left)
			),
		at(EInfix(left, _, op = Lt | Le, right = EWildcard(_) | EVarDecl(_, _, _, _))) =>
			PBoundsMin(
				typePattern(ctx, expectType, left),
				op.match(Lt) ? Exclusive : Inclusive,
				typePattern(ctx, expectType, right)
			),
		at(EInfix(left, _, op = Gt | Ge, right = EWildcard(_) | EVarDecl(_, _, _, _))) =>
			PBoundsMax(
				typePattern(ctx, expectType, right),
				typePattern(ctx, expectType, left),
				op.match(Gt) ? Exclusive : Inclusive
			),
		// TODO: extractor stuff w/ ?= and !=
		at(EInfix(left, _, op = (TExpr.Infix.And ... Nor), right)) => {
			// TODO: ideally this should eval left-to-right
			final patterns: Array<Pattern> = [typePattern(ctx, expectType, right)];

			while(true) left._match(
				at(EInfix(left2, _, _ == op => true, right2)) => {
					patterns.push(typePattern(ctx, expectType, right2));
					left = left2;
				},
				_ => {
					patterns.push(typePattern(ctx, expectType, left));
					patterns.reverse();
					break;
				}
			);
			
			op._match(
				at(And) => PAll(patterns),
				at(Or) => PAny(patterns),
				at(Xor) => POne(patterns),
				at(Nor) => PNone(patterns),
				_ => throw "bad"
			);
		},
		at(EInfix(left, _, BitAnd, right = EWildcard(_) | EVarDecl(_, _, _, _))) => {
			// TODO: correctly implement chained extraction (e.g `a & b & _`)
			PExtractFrom(
				typePattern(ctx, expectType, left),
				typePattern(ctx, expectType, right)
			);
		},
		// maybe add a detailed error for `a | b`...?
		at(EInfix(left = EWildcard(_) | EVarDecl(_, _, _, _), _, BitXor, right)) => {
			// TODO: correctly implement chained exclusion (e.g `_ ^ a ^ b`)
			PExcludeFrom(
				typePattern(ctx, expectType, right),
				typePattern(ctx, expectType, left)
			);
		},
		at(EInfix(left, _, Assign(null), right)) => {
			final rhs = typePattern(ctx, expectType, right);
			final lhs = typePattern(ctx, rhs.t.nonNull(), left);
			PAssignPattern(lhs, rhs);
		},
		
		at(EVarDecl(_, name, null, null)) => PMy(name.name),
		at(EVarDecl(_, name, type!!, null)) => PMyType(name.name, ctx.getType(type).nonNull()),
		at(EVarDecl(s, name, type!!, value!!)) => {
			final tvalue = typePattern(ctx, expectType, value);
			final lhs = typePattern(ctx, tvalue.t, EVarDecl(s, name, type, null));
			PAssignPattern(lhs, tvalue);
		},
		
		at(EType(type)) => PType(ctx.getType(type).nonNull()),
		
		_ => PExpr(typeExpr(ctx, expr))
	);

	final actualType = pattern._match(
		at(PExpr({t: t!})) => t,
		//at(PExtractor(extractor))
		at(PMyType(_, type)) => type,
		at(PType(type)) => type,
		at(PAssignPattern(lhs, _)) => lhs.t,
		at(PTypeArray(type, _) | PTypeTuple(type, _)) => type,
		at(PTypeMembers(type, members)) => type.getFrom(expectType),
		_ => expectType
	);

	return {
		p: pattern,
		t: actualType,
		orig: expr
	}
}

static final EMPTY_BINDINGS = new PatternBindings();
static function _buildPatternBindings(ctx: Ctx, pattern: Pattern): PatternBindings {
	inline function _buildEach<T>(res: PatternBindings, offset: Int, ps: Array<T>, f: (T) -> Pattern) {
		for(i in offset...ps.length) {
			final res2 = _buildPatternBindings(ctx, f(ps[i]));
			if(res2.size() > 0) for(name => pair in res2) {
				if(res.exists(name)) {
					trace(name, res[name]._1.display(), pair._1.display());
					ctx.addError(Type_DuplicateBinding(ctx, name, res[name]._1, pair._1));
					continue;
				}
				res[name] = pair;
			}
		}
	}
	inline function buildEach<T>(ps: Array<T>, f: (T) -> Pattern) {
		var res = _buildPatternBindings(ctx, f(ps[0]));
		if(res.size() == 0) res = res.copy();
		_buildEach(res, 1, ps, f);
		return res;
	}

	return pattern.p._match(
		at(PMy(name)) => [name => tuple(pattern.orig.nonNull().mainSpan(), pattern.t.nonNull())],
		at(PMyType(name, t)) => [name => tuple(pattern.orig.nonNull().mainSpan(), t)],
		// TODO: PAll should have its own thing
		at(PAll(ps) | PAny(ps) | POne(ps)) => {
			var res = _buildPatternBindings(ctx, ps[0]);
			var size = res.size();

			for(i in 1...ps.length) {
				final res2 = _buildPatternBindings(ctx, ps[i]);
				if(res2.size() != size) throw "incomplete bindings";
				// ignore types for now
				if(size > 0) for(name => _ in res2) {
					if(!res.exists(name)) throw "incomplete bindings";
				}
			}

			res;
		},
		
		at(PBoundsMin(_, _, p) | PBoundsMax(p, _, _) | PBoundsMinMax(_, _, p, _, _)
		| PExtractFrom(_, p) | PExcludeFrom(_, p)
		| PSpread(p)) => _buildPatternBindings(ctx, p),

		at(PAssignPattern(p1, p2)) => {
			final res = _buildPatternBindings(ctx, p1);
			for(name => pair in _buildPatternBindings(ctx, p2)) {
				if(res.exists(name)) {
					trace(name, res[name]._1.display(), pair._1.display());
					ctx.addError(Type_DuplicateBinding(ctx, name, res[name]._1, pair._1));
					continue;
				}
				res[name] = pair;
			}
			res;
		},

		at(PArray(ps) | PTypeArray(_, ps) | PTuple(ps) | PTypeTuple(_, ps)
		| PTypeTaggedCaseMulti(_, _, ps), when(ps.length > 0)) => {
			buildEach(ps, p -> p);
		},

		at(PTypeMembers(_, members) | PTypeTaggedCaseMembersSingle(_, _, members)) => {
			buildEach(members, p -> p._2);
		},

		at(PTypeTaggedCaseMembersMulti(_, _, ps, members)) => {
			final res = buildEach(ps, p -> p);
			_buildEach(res, 0, members, p -> p._2);
			res;
		},
		
		_ => EMPTY_BINDINGS
	);
}
static function buildPatternBindings(ctx: Ctx, pattern: Pattern) {
	for(name => p in _buildPatternBindings(ctx, pattern)) {
		/*if(ctx.findLocal(name) != null) {
			throw 'rebound variable `$name`! at ${p._1.display()}';
		} else {
			ctx.locals[name] = new LocalBinding(ctx, p._1, name, p._2);
		}*/
		ctx.findLocal(name)._and(local => {
			if(!(local is LocalField)) {
				ctx.addError(Type_ShadowedLocalVar(ctx, name, local.span, p._1));
			}
		});
		ctx.locals[name] = new LocalBinding(ctx, p._1, name, p._2);
	}
}


static function checkBadThenStmt(ctx: Ctx, then: parsing.ast.Stmt.Then) {
	then._match(
		at(ThenStmt(span, SExpr(EBlock(_)))) => {
			ctx.addError(Type_PossiblyUnintendedArrowBlock(ctx, span));
		},
		_ => {}
	);
}

static function typeStmt(ctx: Ctx, stmt: UStmt): TStmt {
	var stmtLabel: Null<String> = null;
	final res: TStmt = {
		s: switch stmt {
			case SExpr(expr):
				SExpr(typeExpr(ctx, expr));
			
			case SIf(_, cond, then, elseBlk):
				checkBadThenStmt(ctx, then);
				SIf(
					shouldBeLogical(ctx, typeExpr(ctx, cond)),
					typeThen(ctx, then),
					elseBlk._and(b => typeBlock(ctx, b._2))
				);
			
			case SCase(_, cases, otherwise, _):
				SCase(
					cases.map(c -> {
						cond: shouldBeLogical(ctx, typeExpr(ctx, c.cond)),
						then: typeThen(ctx, {
							checkBadThenStmt(ctx, c.then);
							c.then;
						})
					}),
					otherwise._and(o => {
						checkBadThenStmt(ctx, o._2);
						typeThen(ctx, o._2);
					})
				);
			
			case SMatch(_, value, _, cases, otherwise, _):
				final tvalue = typeExpr(ctx, value);
				SMatch(
					tvalue,
					cases.map(c -> {
						final patternCtx = ctx.innerPattern();
						final tpattern = typePattern(patternCtx, tvalue.t ?? {
							trace("NO PATTERN TYPE: "+Std.string(tvalue.e)+" "+c.span.display());
							{t: TBlank,span:c.span};
						}, c.pattern);
						buildPatternBindings(patternCtx, tpattern);
						checkBadThenStmt(ctx, c.then);
						{
							pattern: tpattern,
							cond: c.when._and(w => shouldBeLogical(patternCtx, typeExpr(patternCtx, w._2))),
							then: typeThen(patternCtx.innerBlock(), c.then)
						};
					}),
					otherwise._and(o => {
						checkBadThenStmt(ctx, o._2);
						typeThen(ctx.innerBlock(), o._2);
					})
				);

			case SShortMatch(_, value, _, pattern, cond, then, elseBlk):
				final tvalue = typeExpr(ctx, value);
				//trace(tvalue.t.fullName(),tvalue.orig.mainSpan().display());
				final patternCtx = ctx.innerPattern();
				final tpattern = typePattern(patternCtx, tvalue.t ?? {
					//trace(ctx.outer.outer.outer.locals);
					trace("NO PATTERN: "+Std.string(value)+" "+value.mainSpan().display());
					{t: TBlank, span: value.mainSpan()};
				}, pattern);
				buildPatternBindings(patternCtx, tpattern);
				checkBadThenStmt(ctx, then);
				SMatchAt(
					tvalue,
					tpattern,
					cond._and(c => shouldBeLogical(patternCtx, typeExpr(patternCtx, c._2))),
					typeThen(patternCtx, then),
					elseBlk._and(e => typeBlock(ctx, e._2))
				);

			case SWhile(_, cond, label, body):
				checkBadThenStmt(ctx, body);
				SWhile(
					shouldBeLogical(ctx, typeExpr(ctx, cond)),
					stmtLabel = label?._2.name,
					typeThen(ctx, body)
				);
			
			case SDoWhile(_, label, blk, _, cond):
				SDoWhile(
					typeBlock(ctx, blk),
					stmtLabel = label?._2.name,
					shouldBeLogical(ctx, typeExpr(ctx, cond))
				);
			
			case SForIn(_, lvar, lvar2, _, inExpr, cond, label, body):
				final forCtx = ctx.innerPattern();

				final texpr = typeExpr(ctx, inExpr);

				final t = texpr.t ?? throw "!!! "+inExpr.mainSpan().display();
				
				var lpat1: Pattern;
				var lpat2: Null<Pattern>;
				lvar2._match(
					at(null) => {
						t.iterElemType()._andOr(et => {
							et = et.getFrom(t);
							lpat1 = typePattern(forCtx, et, lvar);
							lpat2 = null;
						}, {
							throw "error! "+lvar.mainSpan().display();
						});
					},
					at(vvar!!) => {
						t.iterAssocType()._andOr(
							a => { detuple(@var [kt, vt] = a);
								kt = kt.getFrom(t);
								vt = vt.getFrom(t);
								lpat1 = typePattern(forCtx, kt, lvar);
								lpat2 = typePattern(forCtx, vt, vvar);
							},
							t.iterElemType()._andOr(et => {
								et = et.getFrom(t);
								lpat1 = typePattern(forCtx, STD_Int, lvar);
								lpat2 = typePattern(forCtx, et, vvar);
							}, {
								throw "error!";
							})
						);
					}
				);

				buildPatternBindings(forCtx, lpat1);
				lpat2._and(p => buildPatternBindings(forCtx, p));

				checkBadThenStmt(ctx, body);

				SForIn(
					lpat1,
					lpat2,
					texpr,
					cond._and(c => {
						final condCtx = forCtx.innerBlock();
						shouldBeLogical(condCtx, typeExpr(condCtx, c._2));
					}),
					label?._2.name,
					typeThen(forCtx, body)
				);

			case SForRange(span, lvar, _, startK, startE, _, stopK, stopE, step, cond, label, body):
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

				checkBadThenStmt(ctx, body);
				
				SForRange(
					tlvar,
					tuple(startK, tstart),
					tuple(stopK, tstop),
					tstep,
					tcond,
					label?._2.name,
					typeThen(forCtx, body)
				);
			
			case SRecurse(_, lvars, label, body):
				final tlvars = [];
				final tlvars2 = [];
				final recCtx = ctx.innerRecurse(tlvars2);
				for(lvar in lvars) tlvars.push(typeExpr(recCtx, lvar));
				for(tlvar in tlvars) tlvars2.push(tlvar.e._match(
					at(EName(name, local)) => tuple(name, local),
					at(EVarDecl(name, null, _)) => tuple(name, recCtx.findLocal(name)),
					at(ESetName(name, local, _)) => tuple(name, local),
					// TODO: foo.bar = 0
					_ => throw "bad"
				));
				SRecurse(
					tlvars,
					label?._2.name,
					typeThen(recCtx, body)
				);

			case SDo(_, label, blk):
				SDo(
					stmtLabel = label?._2.name,
					typeBlock(ctx, blk)
				);
			
			case SReturn(s, value):
				SReturn(
					value._and(v => {
						final tv = typeExpr(ctx, v);

						ctx.expectedReturnType._andOr(ret => {
							final t = tv.t._andOr(tvt => {
								tvt.fullSimplify().unifyWithType(ret.fullSimplify())._andOr(ut => {
									if(ut.span == null) ut.span = tvt.span;
									tv.t = ut;
									ut;
								}, {
									//throw 'Error: expression type `${tvt.fullSimplify().fullName()}` does not match return type `${ret.fullSimplify().fullName()}`! ${s.display()}';
									ctx.addError(Type_DoesNotMatchReturnType(ctx, ret.fullSimplify(), tvt.fullSimplify(), s));
									tvt.fullSimplify();
								});
							}, {
								//trace("???", tv.e, tv.orig.mainSpan().display());
								tv.t = ret;
								ret;
							});

							tv;
						}, {
							tv;
						});
					})
				);
			
			case SBreak(_, null): SBreak(null);
			case SBreak(_, {_2: Left(depth)}): SBreak(Left(depth));
			case SBreak(_, {_2: Right(label)}):
				if(ctx.findLabel(label) != null) throw 'label `$label` does not exist!';
				SBreak(Right(label));
			
			/*case SNext(_, null, with): SNext(null, with._and(w => typeExprs(ctx, w)));
			case SNext(_, {_2: Left(depth)}, with): SNext(Left(depth), with._and(w => typeExprs(ctx, w)));
			case SNext(_, {_2: Right(label)}, with):
				if(ctx.findLabel(label) != null) throw 'label `$label` does not exist!';
				SNext(Right(label), with._and(w => typeExprs(ctx, w)));*/
			case SNext(_, label, with):
				final lvars = ctx.tryGetRecurse();
				final twith = with._and(w => typeExprs(ctx, w));
				switch label {
					case null: SNext(null, lvars, twith);
					case {_2: Left(depth)}: SNext(Left(depth), lvars, twith);
					case {_2: Right(label)}:
						if(ctx.findLabel(label) != null) throw 'label `$label` does not exist!';
						SNext(Right(label), lvars, twith);
				}

			
			case SThrow(span, value):
				SThrow(span, typeExpr(ctx, value));
			
			case STry(_, block, _, cases, otherwise, _):
				STry(
					typeBlock(ctx.innerBlock(), block),
					cases.map(c -> {
						final patternCtx = ctx.innerPattern();
						checkBadThenStmt(ctx, c.then);
						
						final tpattern = typePattern(patternCtx, /*TODO*/{t: TBlank, span: null}, c.pattern);
						buildPatternBindings(patternCtx, tpattern);

						{
							pattern: tpattern,
							cond: c.when._and(w => shouldBeLogical(ctx, typeExpr(patternCtx, w._2))),
							then: typeThen(patternCtx, c.then)
						};
					}),
					otherwise._and(o => {
						checkBadThenStmt(ctx, o._2);
						typeThen(ctx.innerBlock(), o._2);
					})
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
	texpr.t._and(t => {
		if(!t.isNative(NBool)) {
			ctx.addError(Type_ExpectedLogicalValue(ctx, t, texpr.orig.mainSpan()));
		}
	});
	return texpr;
}

static function typeBlock(ctx: Ctx, blk: UBlock): TStmts {
	final newCtx = ctx.innerBlock();
	return cast blk.stmts.map(stmt -> typeStmt(newCtx, stmt));
}

static function typeBlockExpr(ctx: Ctx, blk: UBlock): TStmts {
	final newCtx = ctx.innerBlockExpr();
	return cast blk.stmts.map(stmt -> typeStmt(newCtx, stmt));
}

static function typeThen(ctx: Ctx, then: parsing.ast.Stmt.Then): TStmts {
	final newCtx = ctx.innerBlock();
	return cast switch then {
		case ThenBlock(blk): blk.stmts.map(stmt -> typeStmt(newCtx, stmt));
		case ThenStmt(_, stmt): [typeStmt(newCtx, stmt)];
	}
}

// TODO: add logic for fancy control flow (break, etc) and impossible/invalid returns
// maybe also separate unknown result from no result?
static function getReturnType(ctx: Ctx, tstmts: TStmts): {complete: Bool, spans: Array<Span>, incomplete: Array<Span>, ret: Null<Type>} {
	var isComplete = false;
	var rets = new Array<Tuple2<Array<Span>, Type>>();
	var incomplete = new Array<Span>();

	//if(tstmts.length > 0) {
	final lastStmt = tstmts.last();
	for(tstmt in tstmts) {
		final isLast = (tstmt == lastStmt);
		tstmt.s._match(
			at(SIf(_, then, null)) => {
				getReturnType(ctx, then)._and(res => {
					res.ret._and(ret => rets.push(tuple(res.spans, res.ret)));
				});
			},
			at(SIf(_, then, orelse!!)) => {
				final res1 = getReturnType(ctx, then);
				final res2 = getReturnType(ctx, orelse);

				if(!isComplete) isComplete = res1.complete && res2.complete;
				Util._match([res1.complete, res1.ret, res2.complete, res2.ret],
					at([true, r1!, true, r2!]) => {
						r1.strictUnifyWithType(r2)._andOr(
							r => rets.push(tuple(res1.spans.concat(res2.spans), r)),
							throw "error! "+r1.fullName()+" "+r2.fullName()
						);
					},
					at([_, r1, _, r2]) => {
						r1._and(ret => rets.push(tuple(res1.spans, ret)));
						r2._and(ret => rets.push(tuple(res2.spans, ret)));
					}
				);
			},
			at(SCase(cases, orelse)) => {
				var allComplete = true;
				for(c in cases) {
					final res = getReturnType(ctx, c.then);
					
					if(allComplete) allComplete = res.complete;
					res.ret._and(ret => rets.push(tuple(res.spans, ret)));
				}
				orelse._and(oe => {
					final res = getReturnType(ctx, oe);
					
					if(!isComplete) isComplete = allComplete && res.complete;
					res.ret._and(ret => rets.push(tuple(res.spans, ret)));
				});
			},
			// TODO: add completion logic for matching on non-refutable patterns
			at(SMatch(_, cases, orelse)) => {
				var allComplete = true;
				for(i => c in cases) {
					final res = getReturnType(ctx, c.then);
					
					if(allComplete) allComplete = res.complete;
					res.ret._andOr(ret => {
						rets.push(tuple(res.spans, ret));
						if(isLast && !res.complete) incomplete.pushAll(res.incomplete);
					}, {
						if(isLast && !res.complete)
							incomplete.push(tstmt.orig._match(
								at(SMatch(_, _, _, ucases, _, _)) => {
									ucases[i].span;
								},
								_ => throw "bad"
							));
					});
				}
				if(!isComplete) isComplete = allComplete;
				orelse._and(oe => {
					final res = getReturnType(ctx, oe);
					
					if(!isComplete) isComplete = allComplete && res.complete;
					res.ret._and(ret => rets.push(tuple(res.spans, ret)));
				});
			},
			at(SMatchAt(_, _, _, then, null)) => {
				getReturnType(ctx, then)._and(res => {
					res.ret._and(ret => rets.push(tuple(res.spans, ret)));
				});
			},
			at(SMatchAt(_, _, _, then, orelse!!)) => {
				final res1 = getReturnType(ctx, then);
				final res2 = getReturnType(ctx, orelse);

				if(!isComplete) isComplete = res1.complete && res2.complete;
				Util._match([res1.complete, res1.ret, res2.complete, res2.ret],
					at([true, r1!, true, r2!]) => {
						r1.strictUnifyWithType(r2)._andOr(
							r => rets.push(tuple(res1.spans.concat(res2.spans), r)),
							throw "error!"
						);
					},
					at([_, r1, _, r2]) => {
						r1._and(ret => rets.push(tuple(res1.spans, ret)));
						r2._and(ret => rets.push(tuple(res2.spans, ret)));
					}
				);
			},
			at(SWhile(_, _, body) | SForIn(_, _, _, _, _, body)) => {
				getReturnType(ctx, body)._and(res => {
					res.ret._and(ret => rets.push(tuple(res.spans, ret)));
				});
			},
			at(SDoWhile(body, _, _) | SDo(_, body)) => {
				getReturnType(ctx, body)._and(res => {
					if(!isComplete) isComplete = res.complete;
					res.ret._and(ret => rets.push(tuple(res.spans, ret)));
				});
			},
			at(SForRange(_, _, _, _, _, _, body)) => {
				getReturnType(ctx, body)._and(res => {
					res.ret._and(ret => rets.push(tuple(res.spans, ret)));
				});
			},
			at(SReturn(null)) => {
				if(!isComplete) isComplete = true;
				rets.push(tuple(
					[tstmt.orig._match(
						at(SReturn(span, _)) => span,
						_ => throw "bad"
					)],
					STD_Void.thisType
				));
			},
			at(SReturn(value!!)) => {
				if(!isComplete) isComplete = true;
				value.t._andOr(
					t => rets.push(tuple(
						[tstmt.orig._match(
							at(SReturn(span, _)) => span,
							_ => throw "bad"
						)],
						t
					)),
					trace("cannot return value of unknown type! "+value.orig.nonNull().mainSpan().display())
				);
			},
			at(SThrow(_, _)) => {
				if(!isComplete) isComplete = true;
			},
			at(STry(_, cases, orelse)) => {
				var allComplete = true;
				for(c in cases) {
					final res = getReturnType(ctx, c.then);
					
					if(allComplete) allComplete = res.complete;
					res.ret._and(ret => rets.push(tuple(res.spans, ret)));
				}
				if(!isComplete) isComplete = allComplete;
				orelse._and(oe => {
					final res = getReturnType(ctx, oe);
					
					if(!isComplete) isComplete = allComplete && res.complete;
					res.ret._and(ret => rets.push(tuple(res.spans, ret)));
				});
			},
			_ => {}
		);
	}

	final res = rets.reduce((r1, r2) -> (r1._2.unifyWithType(r2._2) ?? r1._2.bindTo(r2._2, []))._andOr(
		t => tuple(r1._1.concat(r2._1), t),
		{
			//ctx.addError(null);
			throw r1._1[0].display()+" "+r2._1[0].display();
			r1;
		}
	));
	return {
		complete: isComplete,
		spans: res?._1 ?? [],
		incomplete: incomplete,
		ret: res?._2
	};
}


}