package codegen;

import text.Pos;
import typing.*;

@:generic
@:publicFields class Entries<I: Int, K: {}, V: { var id: I; }> {
	final map = new Map<K, I>();
	final list = new Array<V>();

	function new() {}

	function add(k: K, v: V) {
		if(map.exists(k)) return;
		final id: I = cast list.push(v);
		v.id = id-1;
		map[k] = id-1;
	}

	inline function has(k: K) {
		return map.exists(k);
	}

	function find(k: K) {
		return map[k]._and(i => list[i]);
	}

	function findIndex(k: K) {
		return map[k];
	}

	function get(k: K) {
		return map[k]._andOr(
			i => list[i],
			throw "Entry not found!"
		);
	}

	function getIndex(k: K): I {
		return map[k] ?? throw "Entry not found!";
	}
}


@:structInit @:publicFields
class TypeVarEntry {
	@:optional var id: TVarID;
	var name: String;
	var parents: Array<TypeRef>;
}

typedef TypeVarEntries = Entries<TVarID, TypeVar, TypeVarEntry>;


@:publicFields
abstract class AnyMethodEntry {
	var id: MethodID;
	var body: Opcodes;

	function new() {}
}

class SingleMethodEntry extends AnyMethodEntry {
	var name: String;
}

class MultiMethodEntry extends AnyMethodEntry {
	var typevars = new TypeVarEntries();
	var params: Array<TypeRef>;
	var name: String;
}

class CastMethodEntry extends AnyMethodEntry {
	var typevars = new TypeVarEntries();
	var type: TypeRef;
}

class BinaryMethodEntry extends AnyMethodEntry {
	var typevars = new TypeVarEntries();
	var param: TypeRef;
	var name: String;
}

class UnaryMethodEntry extends AnyMethodEntry {
	var name: String;
}


typedef MethodEntries<K: AnyMethod, V: AnyMethodEntry> = Entries<MethodID, K, V>;

typedef StaticSingleMethods = Entries<MethodID, SingleStaticMethod, SingleMethodEntry>;
typedef StaticMultiMethods = Entries<MethodID, MultiStaticMethod, MultiMethodEntry>;
typedef InstSingleMethods = Entries<MethodID, SingleMethod, SingleMethodEntry>;
typedef InstMultiMethods = Entries<MethodID, MultiMethod, MultiMethodEntry>;
typedef CastMethods = Entries<MethodID, CastMethod, CastMethodEntry>;
typedef BinaryMethods = Entries<MethodID, BinaryOperator, BinaryMethodEntry>;
typedef UnaryMethods = Entries<MethodID, UnaryOperator, UnaryMethodEntry>;

typedef SingleInits = Entries<InitID, SingleInit, SingleMethodEntry>;
typedef MultiInits = Entries<InitID, MultiInit, MultiMethodEntry>;


@:structInit @:publicFields
class MemberEntry {
	@:optional var id: MemberID;
	final type: TypeRef;
}

@:structInit @:publicFields
class TaggedCaseEntry {
	@:optional var id: KindTag;
	var name: String;
	var slots: Null<Array<TypeRef>>;
	@:optional var defaultInit: Null<Opcodes>;
}

@:structInit @:publicFields
class ValueCaseEntry {
	@:optional var id: KindTag;
	var name: String;
	@:optional var valueInit: Null<Opcodes>;
}

typedef MemberEntries = Entries<MemberID, Member, MemberEntry>;


@:publicFields
abstract class TypeDeclEntry {
	var id: TypeID;
	var name: String;
	final typevars = new TypeVarEntries();

	final staticSingleMethods = new StaticSingleMethods();
	final staticMultiMethods = new StaticMultiMethods();
	
	function new() {}

	function addStaticMember(mem: Member, e: MemberEntry) {}
	function addInstMember(mem: Member, e: MemberEntry) {}
	function addSingleInit(init: SingleInit, e: SingleMethodEntry) {}
	function addMultiInit(init: MultiInit, e: MultiMethodEntry) {}
	function addStaticSingle(mth: SingleStaticMethod, e: SingleMethodEntry) staticSingleMethods.add(mth, e);
	function addStaticMulti(mth: MultiStaticMethod, e: MultiMethodEntry) staticMultiMethods.add(mth, e);
	function addInstSingle(mth: SingleMethod, e: SingleMethodEntry) {}
	function addInstMulti(mth: MultiMethod, e: MultiMethodEntry) {}
	function addCast(mth: CastMethod, e: CastMethodEntry) {}
	function addBinary(mth: BinaryOperator, e: BinaryMethodEntry) {}
	function addUnary(mth: UnaryOperator, e: UnaryMethodEntry) {}
}

class CategoryEntry extends TypeDeclEntry {
	var pathType: TypeRef;
	var forType: TypeRef;
	
	final staticMembers = new MemberEntries();

	var staticInit: Null<Opcodes>;
	
	final singleInits = new SingleInits();
	final multiInits = new MultiInits();
	
	final instSingleMethods = new InstSingleMethods();
	final instMultiMethods = new InstMultiMethods();
	final castMethods = new CastMethods();
	final binaryMethods = new BinaryMethods();
	final unaryMethods = new UnaryMethods();

	var staticDeinit: Null<Opcodes>;
	
	override function addStaticMember(mem: Member, e: MemberEntry) staticMembers.add(mem, e);
	override function addSingleInit(init: SingleInit, e: SingleMethodEntry) singleInits.add(init, e);
	override function addMultiInit(init: MultiInit, e: MultiMethodEntry) multiInits.add(init, e);
	override function addInstSingle(mth: SingleMethod, e: SingleMethodEntry) instSingleMethods.add(mth, e);
	override function addInstMulti(mth: MultiMethod, e: MultiMethodEntry) instMultiMethods.add(mth, e);
	override function addCast(mth: CastMethod, e: CastMethodEntry) castMethods.add(mth, e);
	override function addBinary(mth: BinaryOperator, e: BinaryMethodEntry) binaryMethods.add(mth, e);
	override function addUnary(mth: UnaryOperator, e: UnaryMethodEntry) unaryMethods.add(mth, e);
}

class OpaqueEntry extends TypeDeclEntry {
	final instSingleMethods = new InstSingleMethods();
	final instMultiMethods = new InstMultiMethods();
	final castMethods = new CastMethods();
	final binaryMethods = new BinaryMethods();
	final unaryMethods = new UnaryMethods();

	override function addInstSingle(mth: SingleMethod, e: SingleMethodEntry) instSingleMethods.add(mth, e);
	override function addInstMulti(mth: MultiMethod, e: MultiMethodEntry) instMultiMethods.add(mth, e);
	override function addCast(mth: CastMethod, e: CastMethodEntry) castMethods.add(mth, e);
	override function addBinary(mth: BinaryOperator, e: BinaryMethodEntry) binaryMethods.add(mth, e);
	override function addUnary(mth: UnaryOperator, e: UnaryMethodEntry) unaryMethods.add(mth, e);
}

class NewtypeEntry extends TypeDeclEntry {
	var base: TypeRef;
	var noInherit: Bool;

	final staticMembers = new MemberEntries();

	var staticInit: Null<Opcodes>;

	final instSingleMethods = new InstSingleMethods();
	final instMultiMethods = new InstMultiMethods();
	final castMethods = new CastMethods();
	final binaryMethods = new BinaryMethods();
	final unaryMethods = new UnaryMethods();

	var staticDeinit: Null<Opcodes>;

	override function addStaticMember(mem: Member, e: MemberEntry) staticMembers.add(mem, e);
	override function addInstSingle(mth: SingleMethod, e: SingleMethodEntry) instSingleMethods.add(mth, e);
	override function addInstMulti(mth: MultiMethod, e: MultiMethodEntry) instMultiMethods.add(mth, e);
	override function addCast(mth: CastMethod, e: CastMethodEntry) castMethods.add(mth, e);
	override function addBinary(mth: BinaryOperator, e: BinaryMethodEntry) binaryMethods.add(mth, e);
	override function addUnary(mth: UnaryOperator, e: UnaryMethodEntry) unaryMethods.add(mth, e);
}

class ModuleEntry extends TypeDeclEntry {
	var parents: Array<TypeRef>;

	final staticMembers = new MemberEntries();

	var staticInit: Null<Opcodes>;

	var staticDeinit: Null<Opcodes>;

	override function addStaticMember(mem: Member, e: MemberEntry) staticMembers.add(mem, e);
}

class ClassLikeEntry extends TypeDeclEntry {
	var parents: Array<TypeRef>;

	final staticMembers = new MemberEntries();
	final instMembers = new MemberEntries();

	var staticInit: Null<Opcodes>;
	var defaultInit: Null<Opcodes>;

	final instSingleMethods = new InstSingleMethods();
	final instMultiMethods = new InstMultiMethods();
	final castMethods = new CastMethods();
	final binaryMethods = new BinaryMethods();
	final unaryMethods = new UnaryMethods();

	final instSingleMethodVTable = new Map<TypeID, InstSingleMethods>();
	final instMultiMethodVTable = new Map<TypeID, InstMultiMethods>();
	final castMethodVTable = new Map<TypeID, CastMethods>();
	final binaryMethodVTable = new Map<TypeID, BinaryMethods>();
	final unaryMethodVTable = new Map<TypeID, UnaryMethods>();

	var deinit: Null<Opcodes>;
	var staticDeinit: Null<Opcodes>;

	override function addStaticMember(mem: Member, e: MemberEntry) staticMembers.add(mem, e);
	override function addInstMember(mem: Member, e: MemberEntry) instMembers.add(mem, e);
	override function addInstSingle(mth: SingleMethod, e: SingleMethodEntry) instSingleMethods.add(mth, e);
	override function addInstMulti(mth: MultiMethod, e: MultiMethodEntry) instMultiMethods.add(mth, e);
	override function addCast(mth: CastMethod, e: CastMethodEntry) castMethods.add(mth, e);
	override function addBinary(mth: BinaryOperator, e: BinaryMethodEntry) binaryMethods.add(mth, e);
	override function addUnary(mth: UnaryOperator, e: UnaryMethodEntry) unaryMethods.add(mth, e);
}

class ClassEntry extends ClassLikeEntry {
	final singleInits = new SingleInits();
	final multiInits = new MultiInits();

	override function addSingleInit(init: SingleInit, e: SingleMethodEntry) singleInits.add(init, e);
	override function addMultiInit(init: MultiInit, e: MultiMethodEntry) multiInits.add(init, e);
}

class ProtocolEntry extends ClassLikeEntry {
	final singleInits = new SingleInits();
	final multiInits = new MultiInits();

	override function addSingleInit(init: SingleInit, e: SingleMethodEntry) singleInits.add(init, e);
	override function addMultiInit(init: MultiInit, e: MultiMethodEntry) multiInits.add(init, e);
}

interface ITaggedCaseEntries {
	final taggedCases: Entries<KindTag, TaggedCase, TaggedCaseEntry>;
}

class TaggedKindEntry extends ClassLikeEntry implements ITaggedCaseEntries {
	var isFlags: Bool;

	final taggedCases = new Entries<KindTag, TaggedCase, TaggedCaseEntry>();
}

// Ignore instMembers and defaultInit
class ValueKindEntry extends ClassLikeEntry {
	var isFlags: Bool;
	var base: Null<TypeRef>;

	final valueCases = new Entries<KindTag, ValueCase, ValueCaseEntry>();

	override function addInstMember(mem: Member, e: MemberEntry) {}
}


class TypeVarEntryMappings extends TypeDeclEntry implements ITaggedCaseEntries {
	final staticMembers = new MemberEntries();
	final instMembers = new MemberEntries();

	final taggedCases = new Entries<KindTag, TaggedCase, TaggedCaseEntry>();
	final valueCases = new Entries<KindTag, ValueCase, ValueCaseEntry>();

	final singleInits = new SingleInits();
	final multiInits = new MultiInits();

	final instSingleMethods = new InstSingleMethods();
	final instMultiMethods = new InstMultiMethods();
	final castMethods = new CastMethods();
	final binaryMethods = new BinaryMethods();
	final unaryMethods = new UnaryMethods();

	override function addStaticMember(mem: Member, e: MemberEntry) staticMembers.add(mem, e);
	override function addInstMember(mem: Member, e: MemberEntry) instMembers.add(mem, e);
	override function addSingleInit(init: SingleInit, e: SingleMethodEntry) singleInits.add(init, e);
	override function addMultiInit(init: MultiInit, e: MultiMethodEntry) multiInits.add(init, e);
	override function addInstSingle(mth: SingleMethod, e: SingleMethodEntry) instSingleMethods.add(mth, e);
	override function addInstMulti(mth: MultiMethod, e: MultiMethodEntry) instMultiMethods.add(mth, e);
	override function addCast(mth: CastMethod, e: CastMethodEntry) castMethods.add(mth, e);
	override function addBinary(mth: BinaryOperator, e: BinaryMethodEntry) binaryMethods.add(mth, e);
	override function addUnary(mth: UnaryOperator, e: UnaryMethodEntry) unaryMethods.add(mth, e);
}


@:build(util.Overload.build())
@:publicFields class World2 {
	final typeDecls = new Entries<TypeID, AnyTypeDecl, TypeDeclEntry>();
	final methods = new Map<AnyMethod, Tuple2<TypeDeclEntry, AnyMethodEntry>>(); // TODO: improve this
	final typevars = new Map<TypeVar, TypeVarEntryMappings>();
	
	final declTVars = new Map<TypeVar, Tuple2<TypeDeclEntry, TypeVarEntry>>();
	final methodTVars = new Map<TypeVar, Tuple2<AnyMethodEntry, TypeVarEntry>>();


	final singleInits = new Map<SingleInit, Tuple2<TypeDeclEntry, SingleMethodEntry>>();
	final multiInits = new Map<MultiInit, Tuple2<TypeDeclEntry, MultiMethodEntry>>();

	final singleStaticMethods = new Map<SingleStaticMethod, Tuple2<TypeDeclEntry, SingleMethodEntry>>();
	final multiStaticMethods = new Map<MultiStaticMethod, Tuple2<TypeDeclEntry, MultiMethodEntry>>();
	
	final singleInstMethods = new Map<SingleMethod, Tuple2<TypeDeclEntry, SingleMethodEntry>>();
	final multiInstMethods = new Map<MultiMethod, Tuple2<TypeDeclEntry, MultiMethodEntry>>();
	final castMethods = new Map<CastMethod, Tuple2<TypeDeclEntry, CastMethodEntry>>();
	final unaryMethods = new Map<UnaryOperator, Tuple2<TypeDeclEntry, UnaryMethodEntry>>();
	final binaryMethods = new Map<BinaryOperator, Tuple2<TypeDeclEntry, BinaryMethodEntry>>();

	final valueCases = new Map<ValueCase, Tuple2<ValueKindEntry, ValueCaseEntry>>();
	final taggedCases = new Map<TaggedCase, Tuple2<TypeDeclEntry/* & ITaggedCaseEntries*/, TaggedCaseEntry>>();

	final staticMembers = new Map<Member, Tuple2<TypeDeclEntry, MemberEntry>>();
	final instMembers = new Map<Member, Tuple2<TypeDeclEntry, MemberEntry>>();

	function new() {}

	
	overload function getID(decl: AnyTypeDecl) {
		return decl._match(
			at(da is DirectAlias) => this.getID(da.type),
			_ => this.get(decl).id
		);
	}

	overload function get(decl: AnyTypeDecl): TypeDeclEntry {
		if(typeDecls.has(decl)) {
			return typeDecls.get(decl);
		} else {
			return this.add(decl);
		}
	}


	function addDVar(e: TypeDeclEntry, tvar: TypeVar) {
		final te: TypeVarEntry = {
			name: tvar.name.name,
			parents: tvar.parents.map(p -> this.getTypeRef(p))
		};

		e.typevars.add(tvar, te);
		declTVars[tvar] = tuple(e, te);
	}


	private function compileStaticInit(staticMemberInits: Array<Tuple2<MemberEntry, TExpr>>, staticInit: Null<StaticInit>): Null<Opcodes> {
		if(staticMemberInits.length > 0) {
			final genCtx = new GenCtx();
			final inits = staticMemberInits.flatMap(smi -> {
				final ops = CodeGen.compile(genCtx, smi._2);
				ops.push(OSetStaticField(smi._1.id));
				ops;
			});

			return staticInit._andOr(si => {
				inits.concat(CodeGen.compile(genCtx, si.typedBody));
			}, {
				inits;
			});
		} else {
			return null;
		}
	}

	private function compileDefaultInit(instMemberInits: Array<Tuple2<MemberEntry, TExpr>>, defaultInit: Null<DefaultInit>): Null<Opcodes> {
		if(instMemberInits.length > 0) {
			final genCtx = new GenCtx();
			final inits = instMemberInits.flatMap(imi -> {
				final ops = CodeGen.compile(genCtx, imi._2);
				ops.push(OSetField(imi._1.id));
				ops;
			});

			return defaultInit._andOr(si => {
				inits.concat(CodeGen.compile(genCtx, si.typedBody));
			}, {
				inits;
			});
		} else {
			return null;
		}
	}

	private function compileDeinit(deinit: Null<Deinit>): Null<Opcodes> {
		return deinit._and(d => CodeGen.compile(new GenCtx(), d.typedBody));
	}

	private function compileStaticDeinit(staticDeinit: Null<StaticDeinit>): Null<Opcodes> {
		return staticDeinit._and(sd => CodeGen.compile(new GenCtx(), sd.typedBody));
	}
	
	overload function add(decl: AnyTypeDecl): TypeDeclEntry {
		if(typeDecls.has(decl)) {
			return typeDecls.get(decl);
		} else {
			function initEntry(e: TypeDeclEntry) {
				typeDecls.add(decl, e);
				e.name = Type.getFullPath(decl).value(); // TODO: eqv of Type#simpleName
				
				final tvars = decl._match(
					at(td is TypeDecl) => td.typevars,
					at(cat is Category) => cat.typevars,
					_ => throw "bad"
				);
				if(tvars.size > 0) {
					for(tvar in tvars.allValues()) {
						this.addDVar(e, tvar);
					}
				}
			}

			return decl._match(
				at(cat is Category) => {
					final e = new CategoryEntry();
					initEntry(e);
					
					e.pathType = this.getTypeRef(cat.path);
					e.forType = this.getTypeRef(cat.thisType);

					final staticMemberInits = new Array<Tuple2<MemberEntry, TExpr>>();
					for(mem in cat.staticMembers) {
						final me = this.addStatic(e, mem);
						mem.typedValue._and(tv => {
							staticMemberInits.push(tuple(me, tv));
						});
					}

					for(sm in cat.staticMethods) this.add(e, sm);
					for(im in cat.methods) this.add(e, im);
					for(op in cat.operators) this.add(e, op);

					e.staticInit = compileStaticInit(staticMemberInits, cat.staticInit.toNull());

					e.staticDeinit = compileStaticDeinit(cat.staticDeinit.toNull());

					e;
				},
				at(oa is OpaqueAlias) => {
					final e = new OpaqueEntry();
					initEntry(e);

					for(sm in oa.staticMethods) this.add(e, sm);
					for(im in oa.methods) this.add(e, im);
					for(op in oa.operators) this.add(e, op);

					e;
				},
				at(sa is StrongAlias) => {
					final e = new NewtypeEntry();
					initEntry(e);

					e.base = this.getTypeRef(sa.type);
					e.noInherit = sa.noInherit;

					final staticMemberInits = new Array<Tuple2<MemberEntry, TExpr>>();
					for(mem in sa.staticMembers) {
						final me = this.addStatic(e, mem);
						mem.typedValue._and(tv => {
							staticMemberInits.push(tuple(me, tv));
						});
					}

					for(sm in sa.staticMethods) this.add(e, sm);
					for(im in sa.methods) this.add(e, im);
					for(op in sa.operators) this.add(e, op);

					e.staticInit = compileStaticInit(staticMemberInits, sa.staticInit.toNull());

					e.staticDeinit = compileStaticDeinit(sa.staticDeinit.toNull());

					e;
				},
				at(m is Module) => {
					final e = new ModuleEntry();
					initEntry(e);

					e.parents = m.parents.map(p -> this.getTypeRef(p));

					final staticMemberInits = new Array<Tuple2<MemberEntry, TExpr>>();
					for(mem in m.staticMembers) {
						final me = this.addStatic(e, mem);
						mem.typedValue._and(tv => {
							staticMemberInits.push(tuple(me, tv));
						});
					}

					for(sm in m.staticMethods) this.add(e, sm);

					e.staticInit = compileStaticInit(staticMemberInits, m.staticInit.toNull());

					e.staticDeinit = compileStaticDeinit(m.staticDeinit.toNull());

					e;
				},
				at(c is Class) => {
					final e = new ClassEntry();
					initEntry(e);

					e.parents = c.parents.map(p -> this.getTypeRef(p));

					final staticMemberInits = new Array<Tuple2<MemberEntry, TExpr>>();
					for(mem in c.staticMembers) { // TODO: get inherited members
						final me = this.addStatic(e, mem);
						mem.typedValue._and(tv => {
							staticMemberInits.push(tuple(me, tv));
						});
					}

					final instMemberInits = new Array<Tuple2<MemberEntry, TExpr>>();
					for(mem in c.instMembers(c)) {
						final me = this.addInst(e, mem);
						mem.typedValue._and(tv => {
							instMemberInits.push(tuple(me, tv));
						});
					}
					
					// TODO
					for(parent in c.parents) {
						parent.getTypeDecl()._match(
							at(cl is ClassLike) => {
								/*cl._match(
									at({inits: inits} is Class | {inits: inits} is Protocol) => {
										for(it in inits) if(it.typedBody!=null) this.add(e, it);
									},
									_ => {}
								);*/
								cl._match(
									at({inits: inits} is Class | {inits: inits} is Protocol) => {
										for(init in inits) if(init.typedBody!=null) {
											final i = {
												init._match(
													at(init is SingleInit) => {
														final i: SingleInit = untyped $new(SingleInit);
														i.decl = c;
														i.span = init.span;
														i.name = init.name;
														i.typedBody = [{
															s: SExpr({
																e: EInitThis(parent, Single(SSInit(init))),
																t: Pass2.STD_Void.thisType
															})
														}];
														i.isMacro = init.isMacro;
														i;
													},
													at(init is MultiInit) => {
														final i: MultiInit = untyped $new(MultiInit);
														i.decl = c;
														i.span = init.span;
														i.typevars = init.typevars;
														i.params = init.params;
														i.fuzzyName = init.fuzzyName;
														final ctx: Ctx = {
															where: WMethod(init),
															outer: {
																where: WDecl(cl),
																thisType: parent
															},
															thisType: parent
														};
														for(param in init.params) {
															param.type = param.type.simplify();
											
															final span = param.name.span;
															final name = param.name.name;
															if(name == "_") {
																continue;
															} else ctx.locals[name]._andOr(local => {
																ctx.addError(Type_DuplicateParam(init, name, local.span, span));
															}, {
																ctx.locals[name] = new Pass2.LocalParam(
																	ctx,
																	span,
																	name,
																	param.type,
																	param.value._and(v =>
																		param.tvalue = Pass2.assignType(ctx, Pass2.typeExpr(ctx, v), param.type)
																	)
																);
															});
														}
														i.typedBody = [{
															s: SExpr({
																e: EInitThis(parent, Multi(
																	[{
																		kind: MSInit(init),
																		tctx: {
																			null; // TODO
																		}
																	}],
																	init.params.map(p -> p.label.name),
																	init.params.map(p -> ({
																		e: EName(p.name.name, ctx.locals[p.name.name]),
																		t: ctx.locals[p.name.name].type,
																		orig: EName(init.span, p.name.name)
																	}:TExpr))
																)),
																t: Pass2.STD_Void.thisType
															})
														}];
														
														i;
													},
													_ => throw "bad"
												);
											};
		
											this.add(e, i);
										}
									},
									_ => {}
								);
								for(sm in cl.staticMethods) if(sm.typedBody!=null) {
									final m = {
										sm._match(
											at(sm is SingleStaticMethod) => {
												final m: SingleStaticMethod = untyped $new(SingleStaticMethod);
												m.decl = c;
												m.span = sm.span;
												m.name = sm.name;
												m.typedBody = [{
													s: SExpr({
														e: ETypeMessage(parent, Single(SSMethod(sm))),
														t: Pass2.STD_Void.thisType
													})
												}];
												m.ret = sm.ret;
												m.isMain = sm.isMain;
												m.isGetter = sm.isGetter;
												m.isSetter = sm.isSetter;
												m.isInline = sm.isInline;
												m.isMacro = sm.isMacro;
												m;
											},
											at(sm is MultiStaticMethod) => {
												final m: MultiStaticMethod = untyped $new(MultiStaticMethod);
												m.decl = c;
												m.span = sm.span;
												m.typevars = sm.typevars;
												m.params = sm.params;
												m.fuzzyName = sm.fuzzyName;
												final ctx: Ctx = {
													where: WMethod(sm),
													outer: {
														where: WDecl(cl),
														thisType: parent
													},
													thisType: parent
												};
												for(param in sm.params) {
													param.type = param.type.simplify();
									
													final span = param.name.span;
													final name = param.name.name;
													if(name == "_") {
														continue;
													} else ctx.locals[name]._andOr(local => {
														ctx.addError(Type_DuplicateParam(sm, name, local.span, span));
													}, {
														ctx.locals[name] = new Pass2.LocalParam(
															ctx,
															span,
															name,
															param.type,
															param.value._and(v =>
																param.tvalue = Pass2.assignType(ctx, Pass2.typeExpr(ctx, v), param.type)
															)
														);
													});
												}
												m.typedBody = [{
													s: SExpr({
														e: ETypeMessage(parent, Multi(
															[{
																kind: MSMethod(sm),
																tctx: {
																	null; // TODO
																}
															}],
															sm.params.map(p -> p.label.name),
															sm.params.map(p -> ({
																e: EName(p.name.name, ctx.locals[p.name.name]),
																t: ctx.locals[p.name.name].type
															}:TExpr))
														)),
														t: Pass2.STD_Void.thisType
													})
												}];
												m.ret = sm.ret;
												m.isMain = sm.isMain;
												m.isGetter = sm.isGetter;
												m.isSetter = sm.isSetter;
												m.isInline = sm.isInline;
												m.isMacro = sm.isMacro;

												m;
											},
											_ => throw "bad"
										);
									};

									this.add(e, m);
								}
								for(im in cl.methods) if(im.typedBody!=null) {
									final m = {
										im._match(
											at(im is SingleMethod) => {
												final m: SingleMethod = untyped $new(SingleMethod);
												m.decl = c;
												m.span = im.span;
												m.name = im.name;
												m.typedBody = im.typedBody;
												m.ret = im.ret;
												m.isMain = im.isMain;
												m.isGetter = im.isGetter;
												m.isSetter = im.isSetter;
												m.isInline = im.isInline;
												m.isMacro = im.isMacro;
												m;
											},
											at(im is MultiMethod) => {
												final m: MultiMethod = untyped $new(MultiMethod);
												m.decl = c;
												m.span = im.span;
												m.typevars = im.typevars;
												m.params = im.params;
												m.fuzzyName = im.fuzzyName;
												m.typedBody = im.typedBody;
												m.ret = im.ret;
												m.isMain = im.isMain;
												m.isGetter = im.isGetter;
												m.isSetter = im.isSetter;
												m.isInline = im.isInline;
												m.isMacro = im.isMacro;

												m;
											},
											at(im is CastMethod) => {
												final m: CastMethod = untyped $new(CastMethod);
												m.decl = c;
												m.span = im.span;
												m.typevars = im.typevars;
												m.type = im.type;
												m.typedBody = im.typedBody;
												m.isMain = im.isMain;
												m.isGetter = im.isGetter;
												m.isSetter = im.isSetter;
												m.isInline = im.isInline;
												m.isMacro = im.isMacro;
												m;
											},
											_ => throw "bad"
										);
									};

									final me = this.add(e, m);

									final clid = this.getID(cl);
									/*im._match(
										at(m is SingleMethod) => {
											if(!e.instSingleMethodVTable.exists(clid)) {
												e.instSingleMethodVTable[clid] = new InstSingleMethods();
											}
											e.instSingleMethodVTable[clid].add(m, cast me);
										},
										at(m is MultiMethod) => {
											if(!e.instMultiMethodVTable.exists(clid)) {
												e.instMultiMethodVTable[clid] = new InstMultiMethods();
											}
											e.instMultiMethodVTable[clid].add(m, cast me);
										},
										at(m is CastMethod) => {
											if(!e.castMethodVTable.exists(clid)) {
												e.castMethodVTable[clid] = new CastMethods();
											}
											e.castMethodVTable[clid].add(m, cast me);
										},
										_ => throw "bad"
									);*/
								}
								for(op in cl.operators) if(op.typedBody!=null) this.add(e, op);
							},
							_ => {}
						);
					}
					for(it in c.inits) this.add(e, it);
					for(sm in c.staticMethods) this.add(e, sm);
					for(im in c.methods) this.add(e, im);
					for(op in c.operators) this.add(e, op);

					e.staticInit = compileStaticInit(staticMemberInits, c.staticInit.toNull());
					e.defaultInit = compileDefaultInit(instMemberInits, c.defaultInit.toNull());

					e.deinit = compileDeinit(c.deinit.toNull());
					e.staticDeinit = compileStaticDeinit(c.staticDeinit.toNull());

					e;
				},
				at(p is Protocol) => {
					final e = new ProtocolEntry();
					initEntry(e);
					if(e.instMembers.map.size() > 0) {
						trace(typeDecls.has(decl), typeDecls.list.contains(e));
					}

					e.parents = p.parents.map(p -> this.getTypeRef(p));

					final staticMemberInits = new Array<Tuple2<MemberEntry, TExpr>>();
					for(mem in p.staticMembers) {
						final me = this.addStatic(e, mem);
						mem.typedValue._and(tv => {
							staticMemberInits.push(tuple(me, tv));
						});
					}

					final instMemberInits = new Array<Tuple2<MemberEntry, TExpr>>();
					for(mem in p.instMembers(p)) {
						final me = this.addInst(e, mem);
						mem.typedValue._and(tv => {
							instMemberInits.push(tuple(me, tv));
						});
					}
					
					// TODO
					for(parent in p.parents) {
						parent.getTypeDecl()._match(
							at(cl is ClassLike) => {
								cl._match(
									at({inits: inits} is Class | {inits: inits} is Protocol) => {
										for(it in inits) if(it.typedBody!=null) this.add(e, it);
									},
									_ => {}
								);
								for(sm in cl.staticMethods) if(sm.typedBody!=null) {
									final m = {
										sm._match(
											at(sm is SingleStaticMethod) => {
												final m: SingleStaticMethod = untyped $new(SingleStaticMethod);
												m.decl = p;
												m.span = sm.span;
												m.name = sm.name;
												m.typedBody = [{
													s: SExpr({
														e: ETypeMessage(parent, Single(SSMethod(sm))),
														t: Pass2.STD_Void.thisType
													})
												}];
												m.ret = sm.ret;
												m.isMain = sm.isMain;
												m.isGetter = sm.isGetter;
												m.isSetter = sm.isSetter;
												m.isInline = sm.isInline;
												m.isMacro = sm.isMacro;
												m;
											},
											at(sm is MultiStaticMethod) => {
												final m: MultiStaticMethod = untyped $new(MultiStaticMethod);
												m.decl = p;
												m.span = sm.span;
												m.typevars = sm.typevars;
												m.params = sm.params;
												m.fuzzyName = sm.fuzzyName;
												final ctx: Ctx = {
													where: WMethod(sm),
													outer: {
														where: WDecl(cl),
														thisType: parent
													},
													thisType: parent
												};
												for(param in sm.params) {
													param.type = param.type.simplify();
									
													final span = param.name.span;
													final name = param.name.name;
													if(name == "_") {
														continue;
													} else ctx.locals[name]._andOr(local => {
														ctx.addError(Type_DuplicateParam(sm, name, local.span, span));
													}, {
														ctx.locals[name] = new Pass2.LocalParam(
															ctx,
															span,
															name,
															param.type,
															param.value._and(v =>
																param.tvalue = Pass2.assignType(ctx, Pass2.typeExpr(ctx, v), param.type)
															)
														);
													});
												}
												m.typedBody = [{
													s: SExpr({
														e: ETypeMessage(parent, Multi(
															[{
																kind: MSMethod(sm),
																tctx: {
																	null; // TODO
																}
															}],
															sm.params.map(p -> p.label.name),
															sm.params.map(p -> ({
																e: EName(p.name.name, ctx.locals[p.name.name]),
																t: ctx.locals[p.name.name].type
															}:TExpr))
														)),
														t: Pass2.STD_Void.thisType
													})
												}];
												m.ret = sm.ret;
												m.isMain = sm.isMain;
												m.isGetter = sm.isGetter;
												m.isSetter = sm.isSetter;
												m.isInline = sm.isInline;
												m.isMacro = sm.isMacro;

												m;
											},
											_ => throw "bad"
										);
									};

									this.add(e, m);
								}
								for(im in cl.methods) if(im.typedBody!=null) {
									final m = {
										im._match(
											at(im is SingleMethod) => {
												final m: SingleMethod = untyped $new(SingleMethod);
												m.decl = p;
												m.span = im.span;
												m.name = im.name;
												m.typedBody = im.typedBody;
												m.ret = im.ret;
												m.isMain = im.isMain;
												m.isGetter = im.isGetter;
												m.isSetter = im.isSetter;
												m.isInline = im.isInline;
												m.isMacro = im.isMacro;
												m;
											},
											at(im is MultiMethod) => {
												final m: MultiMethod = untyped $new(MultiMethod);
												m.decl = p;
												m.span = im.span;
												m.typevars = im.typevars;
												m.params = im.params;
												m.fuzzyName = im.fuzzyName;
												m.typedBody = im.typedBody;
												m.ret = im.ret;
												m.isMain = im.isMain;
												m.isGetter = im.isGetter;
												m.isSetter = im.isSetter;
												m.isInline = im.isInline;
												m.isMacro = im.isMacro;

												m;
											},
											at(im is CastMethod) => {
												final m: CastMethod = untyped $new(CastMethod);
												m.decl = p;
												m.span = im.span;
												m.typevars = im.typevars;
												m.type = im.type.getFrom(parent);
												m.typedBody = im.typedBody;
												m.isMain = im.isMain;
												m.isGetter = im.isGetter;
												m.isSetter = im.isSetter;
												m.isInline = im.isInline;
												m.isMacro = im.isMacro;
												m;
											},
											_ => throw "bad"
										);
									};

									/*final me =*/ this.add(e, m);

									/*final clid = this.getID(cl);
									im._match(
										at(m is SingleMethod) => {
											if(!e.instSingleMethodVTable.exists(clid)) {
												e.instSingleMethodVTable[clid] = new InstSingleMethods();
											}
											e.instSingleMethodVTable[clid].add(m, cast me);
										},
										at(m is MultiMethod) => {
											if(!e.instMultiMethodVTable.exists(clid)) {
												e.instMultiMethodVTable[clid] = new InstMultiMethods();
											}
											e.instMultiMethodVTable[clid].add(m, cast me);
										},
										at(m is CastMethod) => {
											if(!e.castMethodVTable.exists(clid)) {
												e.castMethodVTable[clid] = new CastMethods();
											}
											e.castMethodVTable[clid].add(m, cast me);
										},
										_ => throw "bad"
									);*/
								}
								for(op in cl.operators) if(op.typedBody!=null) {
									final o = op._match(
										at(op is BinaryOperator) => {
											final o: BinaryOperator = untyped $new(BinaryOperator);
											o.decl = op.decl;
											o.span = op.span;
											o.typevars = op.typevars;
											o.opSpan = op.opSpan;
											o.op = op.op;
											o.paramName = op.paramName;
											o.paramType = op.paramType.getFrom(parent);
											o.typedBody = op.typedBody;
											o.ret = op.ret;
											o.isInline = op.isInline;
											o.isMacro = op.isMacro;
											o;
										},
										at(op is UnaryOperator) => {
											final o: UnaryOperator = untyped $new(UnaryOperator);
											o.decl = op.decl;
											o.span = op.span;
											o.opSpan = op.opSpan;
											o.op = op.op;
											o.typedBody = op.typedBody;
											o.ret = op.ret;
											o.isInline = op.isInline;
											o.isMacro = op.isMacro;
											o;
										},
										_ => throw "bad"
									);

									this.add(e, o);
								}
							},
							_ => {}
						);
					}
					for(it in p.inits) this.add(e, it);
					for(sm in p.staticMethods) this.add(e, sm);
					for(im in p.methods) this.add(e, im);
					for(op in p.operators) this.add(e, op);

					e.staticInit = compileStaticInit(staticMemberInits, p.staticInit.toNull());
					e.defaultInit = compileDefaultInit(instMemberInits, p.defaultInit.toNull());

					e.deinit = compileDeinit(p.deinit.toNull());
					e.staticDeinit = compileStaticDeinit(p.staticDeinit.toNull());

					e;
				},
				at(tk is TaggedKind) => {
					final e = new TaggedKindEntry();
					initEntry(e);

					e.parents = tk.parents.map(p -> this.getTypeRef(p));
					e.isFlags = tk._isFlags;

					final staticMemberInits = new Array<Tuple2<MemberEntry, TExpr>>();
					for(mem in tk.staticMembers) {
						final me = this.addStatic(e, mem);
						mem.typedValue._and(tv => {
							staticMemberInits.push(tuple(me, tv));
						});
					}

					final instMemberInits = new Array<Tuple2<MemberEntry, TExpr>>();
					for(mem in tk.instMembers(tk)) {
						final me = this.addInst(e, mem);
						mem.typedValue._and(tv => {
							instMemberInits.push(tuple(me, tv));
						});
					}

					for(tc in tk.taggedCases) {
						if(tc.typedAssoc != null) throw "NYI";

						final te = tc._match(
							at(stc is SingleTaggedCase) => {
								({
									name: stc.name.name,
									slots: null
								} : TaggedCaseEntry);
							},
							at(mtc is MultiTaggedCase) => {
								({
									name: mtc.params.map(p -> p.label.name + ":").join(" "),
									slots: mtc.params.map(p -> this.getTypeRef(p.type))
								} : TaggedCaseEntry);
							},
							_ => throw "bad"
						);
						taggedCases[tc] = tuple((cast e : TypeDeclEntry), te);
						e.taggedCases.add(tc, te);
						te.defaultInit = tc.typedInit._and(i => CodeGen.compile(new GenCtx(), i));
					}
					for(sm in tk.staticMethods) this.add(e, sm);
					for(im in tk.methods) this.add(e, im);
					for(op in tk.operators) this.add(e, op);

					e.staticInit = compileStaticInit(staticMemberInits, tk.staticInit.toNull());
					e.defaultInit = compileDefaultInit(instMemberInits, tk.defaultInit.toNull());

					e.deinit = compileDeinit(tk.deinit.toNull());
					e.staticDeinit = compileStaticDeinit(tk.staticDeinit.toNull());

					e;
				},
				at(vk is ValueKind) => {
					final e = new ValueKindEntry();
					initEntry(e);

					e.parents = vk.parents.map(p -> this.getTypeRef(p));
					e.isFlags = vk._isFlags;
					e.base = vk.repr.toNull()._and(t => this.getTypeRef(t));

					final staticMemberInits = new Array<Tuple2<MemberEntry, TExpr>>();
					for(mem in vk.staticMembers) {
						final me = this.addStatic(e, mem);
						mem.typedValue._and(tv => {
							staticMemberInits.push(tuple(me, tv));
						});
					}

					for(vc in vk.valueCases) {
						final ve: ValueCaseEntry = {
							name: vc.name.name
						};
						valueCases[vc] = tuple(e, ve);
						e.valueCases.add(vc, ve);
						ve.valueInit = vc.typedValue._and(v => {
							final r = CodeGen.compile(new GenCtx(), v);
							r.push(ORet);
							r;
						});
					}
					for(sm in vk.staticMethods) this.add(e, sm);
					for(im in vk.methods) this.add(e, im);
					for(op in vk.operators) this.add(e, op);

					e.staticInit = compileStaticInit(staticMemberInits, vk.staticInit.toNull());

					e.deinit = compileDeinit(vk.deinit.toNull());
					e.staticDeinit = compileStaticDeinit(vk.staticDeinit.toNull());

					e;
				},
				at(tvar is TypeVar) => {
					typevars[tvar]._and(e => return e);

					final e = new TypeVarEntryMappings();
					typevars[tvar] = e;

					// TODO: don't duplicate code
					for(tc in tvar.taggedCases) {
						if(tc.typedAssoc != null) throw "NYI";

						final te = tc._match(
							at(stc is SingleTaggedCase) => {
								({
									name: stc.name.name,
									slots: null
								} : TaggedCaseEntry);
							},
							at(mtc is MultiTaggedCase) => {
								({
									name: mtc.params.map(p -> p.label.name + ":").join(" "),
									slots: mtc.params.map(p -> this.getTypeRef(p.type))
								} : TaggedCaseEntry);
							},
							_ => throw "bad"
						);
						taggedCases[tc] = tuple((e : TypeDeclEntry), te);
						e.taggedCases.add(tc, te);
						te.defaultInit = tc.typedInit._and(i => CodeGen.compile(new GenCtx(), i));
					}
					for(vc in tvar.valueCases) {
						final ve: ValueCaseEntry = {
							name: vc.name.name
						};
						valueCases[vc] = cast tuple(e, ve);
						e.valueCases.add(vc, ve);
						ve.valueInit = vc.typedValue._and(v => {
							final r = CodeGen.compile(new GenCtx(), v);
							r.push(ORet);
							r;
						});
					}
					for(mem in tvar.staticMembers) this.addStatic(e, mem);
					for(mem in tvar.members) this.addInst(e, mem);
					for(it in tvar.inits) this.add(e, it);
					for(sm in tvar.staticMethods) this.add(e, sm);
					for(im in tvar.methods) this.add(e, im);
					for(op in tvar.operators) this.add(e, op);

					e;
				},
				_ => throw "bad"
			);
		}
	}


	overload function getID(tcase: TaggedCase) {
		return this.get(tcase)._2.id;
	}

	overload function getID(vcase: ValueCase) {
		return valueCases[vcase]._2.id;
	}
	
	
	overload function get(tcase: TaggedCase) {
		return taggedCases[tcase] ?? {
			final d = this.get(tcase.decl);
			final e = (cast d : ITaggedCaseEntries).taggedCases.get(tcase);
			final t = tuple(d, e);
			taggedCases[tcase] = t;
			t;
		};
	}


	@:generic
	function addMVar<M: AnyMethodEntry & { var typevars: TypeVarEntries; }>(e: M, tvar: TypeVar) {
		final te: TypeVarEntry = {
			name: tvar.name.name,
			parents: tvar.parents.map(p -> this.getTypeRef(p))
		};

		e.typevars.add(tvar, te);
		methodTVars[tvar] = tuple((e : AnyMethodEntry), te);
	}
	
	@:generic
	private inline function buildMVars<M: AnyMethodEntry & { var typevars: TypeVarEntries; }>(e: M, tvars: MultiMap<String, TypeVar>) {
		if(tvars.size > 0) {
			for(tvar in tvars.allValues()) {
				this.addMVar(e, tvar);
			}
		}
	}
	overload function add(decl: TypeDeclEntry, mth: AnyMethod): AnyMethodEntry {
		final entry = mth._match(
			at(si is SingleInit) => {
				singleInits[si]._and(e => return e._2);

				final e = new SingleMethodEntry();
				decl.addSingleInit(si, e);
				singleInits[si] = tuple(decl, e);

				e.name = si.name.name;

				e.body = si.typedBody._andOr(tb => {
					CodeGen.compile(new GenCtx(), tb);
				}, {
					si.native._andOr(nat => nat._match(
						at(None) => throw "bad",
						at(Some({name: native})) => {
							final ops: Opcodes = [];
							if(native == "ptr_new") {
								final elemType = si.decl.getNative()._match(
									at(null) => throw "bad",
									at(NPtr(elem)) => elem,
									_ => throw "bad"
								);
								final elemRef = this.getTypeRef(elemType);
								ops.push(ONewPtr(elemRef));
							} else {
								ops.push(ONative(native));
							}
							ops.push(ORet);
							ops;
						}
					), {
						[OStr(""), OThrow(mth.span.display())];
					});
				});

				e;
			},
			at(mi is MultiInit) => {
				if(mi.isMacro) return null; // BAD
				multiInits[mi]._and(e => return e._2);

				final e = new MultiMethodEntry();
				decl.addMultiInit(mi, e);
				multiInits[mi] = tuple(decl, e);

				buildMVars(e, mi.typevars);
				e.params = mi.params.map(p -> this.getTypeRef(p.type));
				e.name = mi.fuzzyName;

				e.body = mi.typedBody._andOr(tb => {
					final genCtx = new GenCtx();
					for(param in mi.params) {
						genCtx.newLocal(param.name.name);
					}
					CodeGen.compile(genCtx, tb);
				}, {
					mi.native._andOr(nat => nat._match(
						at(None) => throw "bad",
						at(Some({name: native})) => {
							final ops: Opcodes = [];
							for(id in 0...mi.params.length) {
								ops.push(OGetLocal(id + 1));
							}
							if(native == "ptr_new") {
								final elemType = mi.decl.getNative()._match(
									at(null) => throw "bad",
									at(NPtr(elem)) => elem,
									_ => throw "bad"
								);
								final elemRef = this.getTypeRef(elemType);
								ops.push(ONewPtr(elemRef));
							} else {
								ops.push(ONative(native));
							}
							ops.push(ORet);
							ops;
						}
					), {
						[OStr(""), OThrow(mth.span.display())];
					});
				});

				e;
			},
			at(ss is SingleStaticMethod) => {
				singleStaticMethods[ss]._and(e => return e._2);

				final e = new SingleMethodEntry();
				decl.addStaticSingle(ss, e);
				singleStaticMethods[ss] = tuple(decl, e);

				e.name = ss.name.name;

				e.body = ss.typedBody._andOr(tb => {
					CodeGen.compile(new GenCtx(), tb);
				}, {
					ss.native._andOr(nat => nat._match(
						at(None) => throw "bad",
						at(Some({name: native})) => {
							final ops: Opcodes = [ONative(native)];
							if(ss.ret._andOr(ret => !ret.isNative(NVoid), false)) {
								ops.push(ORet);
							} else {
								ops.push(ORetVoid);
							}
							ops;
						}
					), {
						[OStr(""), OThrow(mth.span.display())];
					});
				});

				e;
			},
			at(ms is MultiStaticMethod) => {
				multiStaticMethods[ms]._and(e => return e._2);

				final e = new MultiMethodEntry();
				decl.addStaticMulti(ms, e);
				multiStaticMethods[ms] = tuple(decl, e);

				buildMVars(e, ms.typevars);
				e.params = ms.params.map(p -> this.getTypeRef(p.type));
				e.name = ms.fuzzyName;

				e.body = ms.typedBody._andOr(tb => {
					final genCtx = new GenCtx();
					for(param in ms.params) {
						genCtx.newLocal(param.name.name);
					}
					CodeGen.compile(genCtx, tb);
				}, {
					ms.native._andOr(nat => nat._match(
						at(None) => throw "bad",
						at(Some({name: native})) => {
							final ops: Opcodes = [];
							for(id in 0...ms.params.length) {
								ops.push(OGetLocal(id + 1));
							}
							ops.push(ONative(native));
							if(ms.ret._andOr(ret => !ret.isNative(NVoid), false)) {
								ops.push(ORet);
							} else {
								ops.push(ORetVoid);
							}
							ops;
						}
					), {
						[OStr(""), OThrow(mth.span.display())];
					});
				});

				e;
			},
			at(si is SingleMethod) => {
				singleInstMethods[si]._and(e => return e._2);

				final e = new SingleMethodEntry();
				decl.addInstSingle(si, e);
				singleInstMethods[si] = tuple(decl, e);

				e.name = si.name.name;

				e.body = si.typedBody._andOr(tb => {
					CodeGen.compile(new GenCtx(), tb);
				}, {
					si.native._andOr(nat => nat._match(
						at(None) => throw "bad",
						at(Some({name: native})) => {
							final ops: Opcodes = [OThis, ONative(native)];
							if(si.ret._andOr(ret => !ret.isNative(NVoid), false)) {
								ops.push(ORet);
							} else {
								ops.push(ORetVoid);
							}
							ops;
						}
					), {
						[OStr(""), OThrow(mth.span.display())];
					});
				});

				e;
			},
			at(mi is MultiMethod) => {
				if(mi.isMacro) return null; // BAD
				multiInstMethods[mi]._and(e => return e._2);

				final e = new MultiMethodEntry();
				decl.addInstMulti(mi, e);
				multiInstMethods[mi] = tuple(decl, e);

				buildMVars(e, mi.typevars);
				e.params = mi.params.map(p -> this.getTypeRef(p.type));
				e.name = mi.fuzzyName;

				e.body = mi.typedBody._andOr(tb => {
					final genCtx = new GenCtx();
					for(param in mi.params) {
						genCtx.newLocal(param.name.name);
					}
					CodeGen.compile(genCtx, tb);
				}, {
					mi.native._andOr(nat => nat._match(
						at(None) => throw "bad",
						at(Some({name: native})) => {
							final ops: Opcodes = [OThis];
							for(id in 0...mi.params.length) {
								ops.push(OGetLocal(id + 1));
							}
							ops.push(ONative(native));
							if(mi.ret._andOr(ret => !ret.isNative(NVoid), false)) {
								ops.push(ORet);
							} else {
								ops.push(ORetVoid);
							}
							ops;
						}
					), {
						[OStr(""), OThrow(mth.span.display())];
					});
				});

				e;
			},
			at(cm is CastMethod) => {
				castMethods[cm]._and(e => return e._2);

				final e = new CastMethodEntry();
				decl.addCast(cm, e);
				castMethods[cm] = tuple(decl, e);

				buildMVars(e, cm.typevars);
				e.type = this.getTypeRef(cm.type);
				
				e.body = cm.typedBody._andOr(tb => {
					CodeGen.compile(new GenCtx(), tb);
				}, {
					cm.native._andOr(nat => nat._match(
						at(None) => throw "bad",
						at(Some({name: native})) => {
							final ops: Opcodes = [OThis, {
								if(native == "cast_u64_ptr") {
									final elemType = cm.type.getNative()._match(
										at(null) => throw "bad",
										at(NPtr(elem)) => elem,
										_ => throw "bad"
									);
									final elemRef = this.getTypeRef(elemType);
									OPtrFromAddr(elemRef);
								} else {
									ONative(native);
								}
							}];
							ops.push(ORet);
							ops;
						}
					), {
						[OStr(""), OThrow(mth.span.display())];
					});
				});

				e;
			},
			at(bo is BinaryOperator) => {
				binaryMethods[bo]._and(e => return e._2);

				final e = new BinaryMethodEntry();
				decl.addBinary(bo, e);
				binaryMethods[bo] = tuple(decl, e);

				buildMVars(e, bo.typevars);
				e.param = this.getTypeRef(bo.paramType);
				e.name = bo.opName();

				e.body = bo.typedBody._andOr(tb => {
					final genCtx = new GenCtx();
					genCtx.newLocal(bo.paramName.name);
					CodeGen.compile(genCtx, tb);
				}, {
					bo.native._andOr(nat => nat._match(
						at(None) => throw "bad",
						at(Some({name: native})) => {
							[
								OThis,
								OGetLocal(1),
								ONative(native),
								ORet
							];
						}
					), {
						[OStr(""), OThrow(mth.span.display())];
					});
				});

				e;
			},
			at(uo is UnaryOperator) => {
				unaryMethods[uo]._and(e => return e._2);

				final e = new UnaryMethodEntry();
				decl.addUnary(uo, e);
				unaryMethods[uo] = tuple(decl, e);

				e.name = uo.opName();

				e.body = uo.typedBody._andOr(tb => {
					CodeGen.compile(new GenCtx(), tb);
				}, {
					uo.native._andOr(nat => nat._match(
						at(None) => throw "bad",
						at(Some({name: native})) => {
							[
								OThis,
								ONative(native),
								ORet
							];
						}
					), {
						[OStr(""), OThrow(mth.span.display())];
					});
				});

				e;
			},
			_ => throw "bad"
		);

		methods[mth] = tuple(decl, entry);

		return entry;
	}


	overload function getID(mth: AnyMethod) {
		if(methods.exists(mth)) {
			return methods[mth]._2.id;
		} else {
			return this.add(this.get(mth.decl), mth).id;
		}
	}

	overload function get(mth: AnyMethod) {
		if(methods.exists(mth)) {
			return methods[mth];
		} else {
			final decl = this.get(mth.decl);
			return tuple(decl, this.add(decl, mth));
		}
	}


	function addStatic(decl: TypeDeclEntry, mem: Member) {
		staticMembers[mem]._and(e => return e._2);

		mem.refinee._and(ref => mem = ref);

		final e: MemberEntry = {
			type: this.getTypeRef(mem.type)
		};

		decl.addStaticMember(mem, e);
		staticMembers[mem] = tuple(decl, e);

		return e;
	}

	function addInst(decl: TypeDeclEntry, mem: Member) {
		instMembers[mem]._and(e => return e._2);

		mem.refinee._and(ref => mem = ref);

		final e: MemberEntry = {
			type: this.getTypeRef(mem.type)
		};

		decl.addInstMember(mem, e);
		instMembers[mem] = tuple(decl, e);

		return e;
	}

	
	function getStaticID(mem: Member) {
		if(staticMembers.exists(mem)) {
			return staticMembers[mem]._2.id;
		} else {
			return this.addStatic(this.get(mem.decl), mem).id;
		}
	}
	
	function getInstID(mem: Member) {
		if(instMembers.exists(mem)) {
			return instMembers[mem]._2.id;
		} else {
			return this.addInst(this.get(mem.decl), mem).id;
		}
	}


	private final dynDVarIDs = new Map<TypeVar, TVarID>();
	function getDVarID(tvar: TypeVar) {
		if(declTVars.exists(tvar)) {
			return declTVars[tvar]._2.id;
		} else {
			// pls make this actually work I hate this pls
			return dynDVarIDs[tvar] ?? {
				final id = (cast tvar.lookup : {typevars: MultiMap<String, TypeVar>}).typevars.allValues()
					.sorted((tv1, tv2) -> tv1.span.start.compare(tv2.span.start)) // BAD
					.indexOf(tvar);
				dynDVarIDs[tvar] = id;
				id;
			};
		}
	}

	private final dynMVarIDs = new Map<TypeVar, TVarID>();
	function getMVarID(tvar: TypeVar) {
		// BAD: DO NOT RELY ON methodTVars IDS
		//if(methodTVars.exists(tvar)) {
		//	return methodTVars[tvar]._2.id;
		//} else {
			// pls make this actually work I hate this pls
			return dynMVarIDs[tvar] ?? {
				final id = (cast tvar.lookup : {typevars: MultiMap<String, TypeVar>}).typevars.allValues()
					.sorted((tv1, tv2) -> tv1.span.start.compare(tv2.span.start)) // BAD
					.indexOf(tvar);
				dynMVarIDs[tvar] = id;
				id;
			};
		//}
	}

	function getTVar(tvar: TypeVar): TVar {
		return tvar.lookup._match(
			at(_ is AnyTypeDecl) => VDecl(this.getDVarID(tvar)),
			at(_ is AnyMethod) => VMethod(this.getMVarID(tvar)),
			_ => throw "bad"
		);
	}


	function getTypeRef(type: Type): TypeRef {
		return type.t._match(
			at(TPath(_, _, _) | TLookup(_, _, _)) => throw "bad",
			at(TConcrete(decl)) => TDecl(this.getID(decl)),
			at(TInstance(decl, params, ctx)) =>
				TInst(
					this.getID(decl),
					[for(tv => t in ctx) this.getDVarID(tv) => this.getTypeRef(t)]
				),
			at(TThis(source)) => TThis,
			at(TBlank) => this.getTypeRef(Pass2.STD_Value.thisType), // TODO: BAD
			at(TApplied({t: TThis(source)}, args)) => this.getTypeRef(source.applyArgs(args).nonNull()), // TODO
			at(TApplied({t: TMulti(_)}, _)) => this.getTypeRef(type.getMostSpecific().simplify()), // TODO
			at(TApplied(_, _)) => this.getTypeRef(type.simplify()), // TODO
			at(TTypeVar(tvar)) => TTypeVar(this.getTVar(tvar)),
			at(TModular(type2, _)) => this.getTypeRef(type2),
			_ => throw "bad! "+type.fullName()+" "+type.span._andOr(s => s.display(), "???")
		);
	}

	overload function getID(type: Type) {
		return this.getTypeRef(type)._match(
			at(TDecl(id) | TInst(id, _)) => id,
			_ => throw "bad "+type.span.display()
		);
	}
}