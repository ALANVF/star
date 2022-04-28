package codegen;

import typing.*;

@:generic
@:publicFields class AccessMap<T: {}, Id: Int> {
	final accessList: Array<T> = [];
	final accessMap: Map<T, Id> = [];

	public function new() {}

	function getID(access: T): Id {
		accessMap[access]._match(
			at(null) => {
				final id = (cast accessList.push(access) : Id);
				accessMap[access] = id;
				return id;
			},
			at(id!!) => {
				return id;
			}
		);
	}

	function getAccess(id: Id): T {
		return accessList[id];
	}
}

typedef MethodMap<T: AnyMethod> = AccessMap<T, MethodID>;

typedef MemberMap = AccessMap<Member, MemberID>;

typedef DeclMap = AccessMap<TypeDecl, TypeID>;

typedef TVarMap = AccessMap<TypeVar, TVarID>;

typedef CaseMap<T: {}> = AccessMap<T, KindTag>;

@:build(util.Overload.build())
@:publicFields class World {
	final typeDecls = new DeclMap();
	final declTVars = new Map<AnyTypeDecl, TVarMap>();
	final methodTVars = new Map<AnyMethod, TVarMap>();

	final singleInits = new MethodMap<SingleInit>();
	final multiInits = new MethodMap<MultiInit>();
	
	final singleStaticMethods = new MethodMap<SingleStaticMethod>();
	final multiStaticMethods = new MethodMap<MultiStaticMethod>();
	
	final singleInstMethods = new MethodMap<SingleMethod>();
	final multiInstMethods = new MethodMap<MultiMethod>();
	final castMethods = new MethodMap<CastMethod>();
	final unaryOps = new MethodMap<UnaryOperator>();
	final binaryOps = new MethodMap<BinaryOperator>();

	final valueCases = new Map<AnyTypeDecl, Array<ValueCase>>();
	final taggedCases = new Map<AnyTypeDecl, Array<TaggedCase>>();

	final staticMembers = new Map<AnyTypeDecl, MemberMap>();
	final instMembers = new Map<AnyTypeDecl, MemberMap>();

	public function new() {}

	overload inline function getID(decl: TypeDecl) {
		return typeDecls.getID(decl);
	}

	overload function getTVar(tvar: TypeVar): TVar {
		return tvar.lookup._match(
			at(decl is AnyTypeDecl) => VDecl(this._getDVarID(tvar, decl)),
			at(mth is AnyMethod) => VMethod(this._getMVarID(tvar, mth)),
			_ => throw "bad"
		);
	}

	inline function getDVarID(tvar: TypeVar) {
		final decl = cast(tvar.lookup, AnyTypeDecl);
		return _getDVarID(tvar, decl);
	}
	private function _getDVarID(tvar: TypeVar, decl: AnyTypeDecl) {
		final tvarMap = declTVars[decl]._match(
			at(null) => {
				final res = new TVarMap();
				declTVars[decl] = res;
				res;
			},
			at(map!!) => map
		);
		return tvarMap.getID(tvar);
	}

	inline function getMVarID(tvar: TypeVar) {
		final mth = cast(tvar.lookup, AnyMethod);
		return _getMVarID(tvar, mth);
	}
	private function _getMVarID(tvar: TypeVar, mth: AnyMethod) {
		final tvarMap = methodTVars[mth]._match(
			at(null) => {
				final res = new TVarMap();
				methodTVars[mth] = res;
				res;
			},
			at(map!!) => map
		);
		return tvarMap.getID(tvar);
	}

	overload function getID(tvar: TypeVar) {
		return tvar.lookup._match(
			at(decl is AnyTypeDecl) => this._getDVarID(tvar, decl),
			at(mth is AnyMethod) => this._getMVarID(tvar, mth),
			_ => throw "bad"
		);
	}

	overload function getID(init: Init) return init._match(
		at(si is SingleInit) => getID(si),
		at(mi is MultiInit) => getID(mi),
		_ => throw "bad"
	);
	overload inline function getID(init: SingleInit) {
		return singleInits.getID(init);
	}
	overload inline function getID(init: MultiInit) {
		return multiInits.getID(init);
	}
	
	overload function getID(mth: StaticMethod) return mth._match(
		at(sm is SingleStaticMethod) => getID(sm),
		at(mm is MultiStaticMethod) => getID(mm),
		_ => throw "bad"
	);
	overload inline function getID(mth: SingleStaticMethod) {
		return singleStaticMethods.getID(mth);
	}
	overload inline function getID(mth: MultiStaticMethod) {
		return multiStaticMethods.getID(mth);
	}
	
	overload function getID(mth: Method) return mth._match(
		at(sm is SingleMethod) => getID(sm),
		at(mm is MultiMethod) => getID(mm),
		at(cm is CastMethod) => getID(cm),
		_ => throw "bad"
	);
	overload inline function getID(mth: SingleMethod) {
		return singleInstMethods.getID(mth);
	}
	overload inline function getID(mth: MultiMethod) {
		return multiInstMethods.getID(mth);
	}
	overload inline function getID(mth: CastMethod) {
		return castMethods.getID(mth);
	}

	overload function getID(oper: Operator) return oper._match(
		at(uo is UnaryOperator) => unaryOps.getID(uo),
		at(bo is BinaryOperator) => binaryOps.getID(bo),
		_ => throw "bad"
	);
	overload inline function getID(oper: UnaryOperator) {
		return unaryOps.getID(oper);
	}
	overload inline function getID(oper: BinaryOperator) {
		return binaryOps.getID(oper);
	}

	overload function getID(vcase: ValueCase): KindTag {
		final decl = vcase.decl;
		return valueCases[decl]._andOr(cases => {
			cases.indexOf(vcase);
		}, {
			final cases = decl.allValueCases();
			valueCases[decl] = cases;
			return cases.indexOf(vcase);
		});
	}
	overload function getID(tcase: TaggedCase): KindTag {
		final decl = tcase.decl;
		return taggedCases[decl]._andOr(cases => {
			cases.indexOf(tcase);
		}, {
			final cases = decl.allTaggedCases();
			taggedCases[decl] = cases;
			return cases.indexOf(tcase);
		});
	}

	overload function getStaticID(mem: Member) {
		mem.refinee._and(ref => {
			mem = ref;
		});
		
		final decl = mem.decl;
		final staticMap = staticMembers[decl]._match(
			at(null) => {
				final res = new MemberMap();
				staticMembers[decl] = res;
				res;
			},
			at(map!!) => map
		);

		return staticMap.getID(mem);
	}

	overload function getInstID(mem: Member) {
		mem.refinee._and(ref => {
			mem = ref;
		});

		final decl = mem.decl;
		final instMap = instMembers[decl]._match(
			at(null) => {
				final res = new MemberMap();
				instMembers[decl] = res;
				res;
			},
			at(map!!) => map
		);

		return instMap.getID(mem);
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
			at(TApplied({t: TThis(source)}, args)) => this.getTypeRef(source.applyArgs(args).nonNull()), // TODO
			at(TApplied({t: TMulti(_)}, _)) => this.getTypeRef(type.getMostSpecific().simplify()), // TODO
			at(TApplied(_, _)) => this.getTypeRef(type.simplify()), // TODO
			at(TTypeVar(tvar)) => TTypeVar(this.getTVar(tvar)),
			at(TModular(type2, _)) => this.getTypeRef(type2),
			_ => throw "bad! "+type.fullName()+" "+type.span._andOr(s => s.display(), "???")
		);
	}
}