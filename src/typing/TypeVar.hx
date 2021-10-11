package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;
import typing.Traits;

using typing.TypeRule.TypeRuleTools;

// Dear god what have I gotten myself into

@:build(util.Auto.build({keepInit: true}))
class TypeVar implements IErrors {
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType & ITypeVars;
	final span: Span;
	final name: Ident;
	var params: Array<Type>;
	var parents: Array<Type>;
	var rule: Option<TypeRule>;
	var defaultInit: Option<DefaultInit> = None;
	var deinit: Option<Deinit> = None;
	final inits: Array<Init> = [];
	final members: Array<Member> = [];
	final methods: Array<Method> = [];
	final operators: Array<Operator> = [];
	var staticDeinit: Option<StaticDeinit> = None;
	var staticInit: Option<StaticInit> = None;
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	final taggedCases: Array<TaggedCase> = [];
	final valueCases: Array<ValueCase> = [];
	final categories: Array<Category> = [];
	var native: Option<NativeKind> = None;
	var isFlags: Bool = false;
	var isStrong: Bool = false;
	var isUncounted: Bool = false;
	@ignore var thisType: Type;

	function new() {
		thisType = new Type(TTypeVar(this));
	}

	static function fromAST(lookup: ILookupType & ITypeVars, ast: parsing.ast.decls.GenericParam): TypeVar {
		final typevar = new TypeVar({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: null,  // hack for partial initialization
			parents: null, // hack for partial initialization
			rule: ast.rule.map(r -> TypeRule.fromAST(lookup, r.rule))
		});

		typevar.params = ast.params.doOrElse(p => p.of.map(x -> typevar.makeTypePath(x)), []);
		typevar.parents = ast.parents.doOrElse(p => p.parents.map(x -> typevar.makeTypePath(x)), []);
		
		for(attr => span in ast.attrs) switch attr {
			case IsNative(_, _, _) if(typevar.native.isSome()): typevar.errors.push(Errors.duplicateAttribute(typevar, ast.name.name, "native", span));
			case IsNative(_, [{label: {name: "repr"}, expr: ELitsym(_, repr)}], _): switch repr {
				case "void": typevar.native = Some(NVoid);
				case "bool": typevar.native = Some(NBool);
				case "voidptr": typevar.native = Some(NVoidPtr);
				default: typevar.errors.push(Errors.invalidAttribute(typevar, typevar.name.name, "native", span));
			}
			case IsNative(_, [
				{label: {name: "repr"}, expr: ELitsym(_, "ptr")},
				{label: {name: "elem"}, expr: EType(t)}
			], _): typevar.native = Some(NPtr(typevar.makeTypePath(t)));
			case IsNative(_, [
				{label: {name: "repr"}, expr: ELitsym(_, "dec")},
				{label: {name: "bits"}, expr: EInt(_, bits, _)}
			], _): switch bits {
				case 32: typevar.native = Some(NDec32);
				case 64: typevar.native = Some(NDec64);
				default: typevar.errors.push(Errors.invalidAttribute(typevar, typevar.name.name, "native", span));
			}
			case IsNative(_, [
				{label: {name: "repr"}, expr: ELitsym(_, "int")},
				{label: {name: "bits"}, expr: EInt(_, bits, _)},
				{label: {name: "signed"}, expr: EBool(_, signed)}
			], _): switch bits {
				case 8: typevar.native = Some(signed ? NInt8 : NUInt8);
				case 16: typevar.native = Some(signed ? NInt16 : NUInt16);
				case 32: typevar.native = Some(signed ? NInt32 : NUInt32);
				case 64: typevar.native = Some(signed ? NInt64 : NUInt64);
				default: typevar.errors.push(Errors.invalidAttribute(typevar, typevar.name.name, "native", span));
			}
			case IsNative(_, _, _): typevar.errors.push(Errors.invalidAttribute(typevar, typevar.name.name, "native", span));
			
			case IsFlags: typevar.isFlags = true;
			
			case IsStrong: typevar.isStrong = true;

			case IsUncounted: typevar.isUncounted = true;
		}

		if(ast.body.isSome()) {
			for(decl in ast.body.value().of) switch decl {
				case DMember(m) if(m.attrs.exists(IsStatic)): typevar.staticMembers.push(Member.fromAST(typevar, m));
				case DMember(m): typevar.members.push(Member.fromAST(typevar, m));

				case DCase(c = {kind: Tagged(_)}): typevar.taggedCases.push(TaggedCase.fromAST(typevar, c));
				case DCase(c = {kind: Scalar(_, _)}): typevar.valueCases.push(ValueCase.fromAST(typevar, c));

				case DCategory(c): typevar.categories.push(Category.fromAST(typevar, c));
	
				case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(typevar, m).forEach(x -> typevar.staticMethods.push(x));
				case DMethod(m): typevar.methods.push(Method.fromAST(typevar, m));
	
				case DInit(i): typevar.inits.push(Init.fromAST(typevar, i));
	
				case DOperator(o): Operator.fromAST(typevar, o).forEach(x -> typevar.operators.push(x));
	
				case DDefaultInit(i) if(typevar.staticInit.isSome()): typevar.staticInit = Some(StaticInit.fromAST(typevar, i));
				case DDefaultInit(i): typevar.defaultInit = Some(DefaultInit.fromAST(typevar, i));
				
				case DDeinit(d) if(typevar.staticDeinit.isSome()): typevar.staticDeinit = Some(StaticDeinit.fromAST(typevar, d));
				case DDeinit(d): typevar.deinit = Some(Deinit.fromAST(typevar, d));
				
				default: typevar.errors.push(Errors.unexpectedDecl(typevar, ast.name.name, decl));
			}
		}

		return typevar;
	}

	inline function declName() {
		return "type variable";
	}

	function fullName() {
		var res = switch params {
			case []: name.name;
			default: name.name + "[" + params.joinMap(", ", p -> p.fullName()) + "]";
		};

		switch parents {
			case []:
			case [p]: res = '($res of ${p.fullName()})';
			case _: res = '($res of #[${parents.joinMap(", ", p -> p.fullName())}])';
		}

		return res;
	}


	function isNative(kind: NativeKind) {
		if(native.exists(nat -> nat.matches(kind))) return true;

		if(parents.some(p -> p.isNative(kind))) return true;

		rule._match(
			at(Some(cond)) => {
				if(cond.isNative(kind, this)) {
					return true;
				}
			},
			_ => {}
		);

		return false;
	}


	function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		//if(cache.contains(thisType)) return None;
		//cache = cache.prepend(thisType);

		return path._match(
			at([[span, "This", args]], when(search != Inside && depth == 0)) => {
				if(args.length == 0) {
					Some({t: TThis(this), span: span});
				} else {
					// errors prob shouldn't be attatched to *this* type decl, but eh
					if(params.length == 0) {
						errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
						None;
					} else if(args.length > params.length) {
						errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
						None;
					} else if(args.length < params.length) {
						errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
						None;
					} else {
						Some({t: TApplied({t: TThis(this), span: span}, args), span: span});
					}
				}
			},
			at([[span, typeName, args]], when(typeName == this.name.name)) => {
				lookup.findType(path, Start, null, 0, cache==Nil?Nil:cache.tail());
			},
			_ => if(search == Inside) None else lookup.findType(path, search, from, depth, cache)
		);
	}

	/*function findTypeOld(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(cache.contains(this)) {
			return None;
		}

		return path._match(
			at([[span, "This", []]], when(absolute)) => Some({t: TThis(this), span: span}),
			at([[span, "This", args]], when(absolute)) => {
				// errors prob shouldn't be attatched to *this* type var, but eh
				if(params.length == 0) {
					errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
					None;
				} else if(args.length > params.length) {
					errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
					None;
				} else if(args.length < params.length) {
					errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
					None;
				} else {
					Some({t: TApplied({t: TThis(this), span: span}, args), span: span});
				}
			},
			at([[span, typeName, args]], when(typeName == this.name.name && !cache.contains(this))) => {
				return lookup.findTypeOld(path, false, cache);
			},
			_ => if(absolute) lookup.findTypeOld(path, true, cache.prepend(this)) else None
		);
	}*/

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}

	function hasErrors() {
		return errors.length != 0
			|| staticMembers.some(m -> m.hasErrors())
			|| staticMethods.some(m -> m.hasErrors())
			|| members.some(m -> m.hasErrors())
			|| methods.some(m -> m.hasErrors())
			|| inits.some(i -> i.hasErrors())
			|| operators.some(o -> o.hasErrors())
			|| valueCases.some(c -> c.hasErrors())
			|| taggedCases.some(c -> c.hasErrors())
			|| categories.some(c -> c.hasErrors());
	}

	function allErrors() {
		var result = errors;
		
		for(member in staticMembers) result = result.concat(member.allErrors());
		for(member in members) result = result.concat(member.allErrors());
		for(method in staticMethods) result = result.concat(method.allErrors());
		for(method in methods) result = result.concat(method.allErrors());
		for(init in inits) result = result.concat(init.allErrors());
		for(op in operators) result = result.concat(op.allErrors());
		for(taggedCase in taggedCases) result = result.concat(taggedCase.allErrors());
		for(valueCase in valueCases) result = result.concat(valueCase.allErrors());
		for(category in categories) result = result.concat(category.allErrors());

		return result;
	}


	function hasParentDecl(decl: TypeDecl): Bool {
		if(params.length != 0) throw "todo";

		for(parent in parents) {
			if(!decl.hasChildType(parent)) return false;
		}

		rule._match(
			at(Some(cond)) => {
				if(!cond.hasParentDecl(decl, this)) {
					return false;
				}
			},
			_ => {}
		);

		if(defaultInit != None
		|| deinit != None
		|| inits.length != 0
		|| members.length != 0
		|| methods.length != 0
		|| operators.length != 0
		|| staticDeinit != None
		|| staticInit != None
		|| staticMembers.length != 0
		|| staticMethods.length != 0
		|| taggedCases.length != 0
		|| valueCases.length != 0
		|| categories.length != 0
		//|| native != None
		|| isFlags
		|| isStrong
		|| isUncounted
		) {
			throw "todo";
		}

		native._match(
			at(Some(nat)) => {
				if(!decl.isNative(nat)) {
					return false;
				}
			},
			_ => {}
		);

		return true;
	}

	function hasChildDecl(decl: TypeDecl): Bool {
		throw "todo";
	}


	function hasParentTypevar(tvar: TypeVar): Bool {
		throw "todo";
	}

	// TODO: This needs to account for  existential types, HOW ON EARTH DO I DO THAT????????
	function hasChildTypevar(tvar: TypeVar): Bool {
		if(params.length != 0) throw "todo";

		for(parent in parents) {
			if(!tvar.hasParentType(parent)) return false;
		}

		if(rule != None) throw "todo";

		if(defaultInit != None
		|| deinit != None
		|| inits.length != 0
		|| members.length != 0
		|| methods.length != 0
		|| operators.length != 0
		|| staticDeinit != None
		|| staticInit != None
		|| staticMembers.length != 0
		|| staticMethods.length != 0
		|| taggedCases.length != 0
		|| valueCases.length != 0
		|| categories.length != 0
		//|| native != None
		|| isFlags
		|| isStrong
		|| isUncounted
		) {
			throw "todo "+this.name.span.display();
		}

		native._match(
			at(Some(nat)) => {
				if(!tvar.isNative(nat)) {
					return false;
				}
			},
			_ => {}
		);

		return true;
	}


	function hasParentType(type: Type): Bool {
		return switch type.t {
			case TPath(_, _, _): throw "bad";
			case TLookup(_, _, _): throw "todo";
			case TConcrete(decl): this.hasParentDecl(decl);
			case TThis(source): throw "todo";
			case TBlank: true;
			case TMulti(types): types.some(t -> this.hasParentType(t)); // should this mutate to help with type inference?
			case TApplied(type2, params): this.hasParentType(type2);//throw "todo";
			case TTypeVar(typevar): typevar.hasChildTypevar(this);
			case TModular(type2, _): this.hasParentType(type2);
		}
	}

	function hasChildType(type: Type): Bool {
		type.t._match(
			at(TTypeVar(tv)) => return this.hasChildTypevar(tv),
			_ => {}
		);

		if(!parents.every(p -> type.hasParentType(p))) {
			return false;
		}

		if(defaultInit == None
		&& deinit == None
		&& inits.length == 0
		&& members.length == 0
		&& methods.length == 0
		&& operators.length == 0
		&& staticDeinit == None
		&& staticInit == None
		&& staticMembers.length == 0
		&& staticMethods.length == 0
		&& taggedCases.length == 0
		&& valueCases.length == 0
		&& categories.length == 0
		//&& native == None
		&& !isFlags
		&& !isStrong
		&& !isUncounted) {
			//trace(parents.map(p -> type.hasParentType(p)&&p.hasChildType(type)),this.fullName(),type.fullName());
			return true;
		}

		native._match(
			at(Some(nat)) => {
				if(!type.isNative(nat)) {
					return false;
				}
			},
			_ => {}
		);

		throw "todo "+name.span.display();
	}


	function canSeeMember(member: Member) {
		return true; // TODO
	}

	function canSeeMethod(method: AnyMethod) {
		return true; // TODO
	}


	function findSingleInst(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleInstKind> {
		for(mem in members) {
			if(mem.matchesGetter(name) && from.canSeeMember(mem)) {
				return SIFromTypevar(this, name, getter, SIMember(mem));
			}
		}

		for(mth in methods) mth._match(
			at(sm is SingleMethod) => {
				if(sm.name.name == name && (!getter || sm.isGetter) && from.canSeeMethod(sm)) {
					return SIFromTypevar(this, name, getter, SIMethod(sm));
				}
			},
			_ => {}
		);

		for(parent in parents) {
			parent.findSingleInst(name, from, getter)._match(
				at(si!) => return SIFromTypevar(this, name, getter, si),
				_ => {}
			);
		}

		if(rule != None) throw "todo";

		return null;
	}


	function findMultiInst(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return [];
		
		final candidates: Array<MultiInstKind> = [];

		names._match(at([name]) => for(mem in members) {
			if(mem.matchesSetter(name) && from.canSeeMember(mem)) {
				candidates.push(MIMember(mem));
			}
		}, _ => {});

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
					at(mm is MultiMethod) => if(mm.isSetter) {
						if(mm.params.every2Strict(names, (l, n) -> n == "=" || l.label.name == n) && from.canSeeMethod(mm)) {
							candidates.push(MIMethod(mm));
						}
					},
					_ => {}
				);
			}
		} else {
			for(mth in methods) mth._match(
				at(mm is MultiMethod) => {
					if(mm.params.every2Strict(names, (l, n) -> l.label.name == n) && from.canSeeMethod(mm)) {
						candidates.push(MIMethod(mm));
					}
				},
				_ => {}
			);
		}

		for(parent in parents) {
			candidates.pushAll(parent.findMultiInst(names, from, setter, cache));
		}

		return candidates;
	}


	function findCast(target: Type, from: ITypeDecl, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return [];

		final candidates = [];

		for(mth in methods) mth._match(
			at(cm is CastMethod) => {
				if(cm.type.hasChildType(target)) {
					candidates.push(cm);
				}
			},
			_ => {}
		);

		for(parent in parents) {
			candidates.pushAll(parent.findCast(target, from, cache));
		}

		return candidates;
	}

	
	function findCategory(cat: Type, forType: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		return if(forType == thisType) {
			findThisCategory(cat, from, cache);
		} else {
			lookup.findCategory(cat, forType, from, cache);
		}
	}

	function findThisCategory(cat: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		return switch parents {
			case []: lookup.findCategory(cat, thisType, from, cache);
			case [p]: p.findCategory(cat, p, from, cache).concat(lookup.findCategory(cat, thisType, from, cache));
			default:
				throw "todo";
				//lookup.findCategory(cat, forType, from, cache);
		}
	}
}