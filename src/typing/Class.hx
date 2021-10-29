package typing;

import typing.Traits;
import parsing.ast.Ident;
import reporting.Diagnostic;

class Class extends ClassLike {
	final inits: Array<Init> = [];
	var defaultInit: Option<DefaultInit> = None;
	var deinit: Option<Deinit> = None;
	var native: Option<NativeKind> = None;
	var isStrong: Bool = false;
	var isUncounted: Bool = false;

	static function fromAST(lookup, ast: parsing.ast.decls.Class) {
		final cls = new Class({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: []
		});

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(cls, a))) {
			cls.typevars.add(typevar.name.name, typevar);
		}

		if(ast.params.isSome()) {
			cls.params = ast.params.value().of.map(param -> cls.makeTypePath(param));
		}

		if(ast.parents.isSome()) {
			for(parent in ast.parents.value().parents) {
				cls.parents.push(cls.makeTypePath(parent));
			}
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(cls.hidden.isSome()): cls.errors.push(Errors.duplicateAttribute(cls, ast.name.name, "hidden", span));
			case IsHidden(None): cls.hidden = Some(None);
			case IsHidden(Some(outsideOf)): cls.hidden = Some(Some(cls.makeTypePath(outsideOf)));

			case IsFriend(_) if(cls.friends.length != 0): cls.errors.push(Errors.duplicateAttribute(cls, ast.name.name, "friend", span));
			case IsFriend(One(friend)): cls.friends.push(cls.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) cls.friends.push(cls.makeTypePath(friend));

			case IsSealed(_) if(cls.sealed.isSome()): cls.errors.push(Errors.duplicateAttribute(cls, ast.name.name, "sealed", span));
			case IsSealed(None): cls.sealed = Some(None);
			case IsSealed(Some(outsideOf)): cls.sealed = Some(Some(cls.makeTypePath(outsideOf)));

			case IsNative(_, _, _) if(cls.native.isSome()): cls.errors.push(Errors.duplicateAttribute(cls, ast.name.name, "native", span));
			case IsNative(_, [{label: {name: "repr"}, expr: ELitsym(_, repr)}], _): switch repr {
				case "void": cls.native = Some(NVoid);
				case "bool": cls.native = Some(NBool);
				case "voidptr": cls.native = Some(NVoidPtr);
				default: cls.errors.push(Errors.invalidAttribute(cls, cls.name.name, "native", span));
			}
			case IsNative(_, [
				{label: {name: "repr"}, expr: ELitsym(_, "ptr")},
				{label: {name: "elem"}, expr: EType(t)}
			], _): cls.native = Some(NPtr(cls.makeTypePath(t)));
			case IsNative(_, [
				{label: {name: "repr"}, expr: ELitsym(_, "dec")},
				{label: {name: "bits"}, expr: EInt(_, bits, _)}
			], _): switch bits {
				case 32: cls.native = Some(NDec32);
				case 64: cls.native = Some(NDec64);
				default: cls.errors.push(Errors.invalidAttribute(cls, cls.name.name, "native", span));
			}
			case IsNative(_, [
				{label: {name: "repr"}, expr: ELitsym(_, "int")},
				{label: {name: "bits"}, expr: EInt(_, bits, _)},
				{label: {name: "signed"}, expr: EBool(_, signed)}
			], _): switch bits {
				case 8: cls.native = Some(signed ? NInt8 : NUInt8);
				case 16: cls.native = Some(signed ? NInt16 : NUInt16);
				case 32: cls.native = Some(signed ? NInt32 : NUInt32);
				case 64: cls.native = Some(signed ? NInt64 : NUInt64);
				default: cls.errors.push(Errors.invalidAttribute(cls, cls.name.name, "native", span));
			}
			case IsNative(_, _, _): cls.errors.push(Errors.invalidAttribute(cls, cls.name.name, "native", span));

			case IsStrong: cls.isStrong = true;

			case IsUncounted: cls.isUncounted = true;
		}

		for(decl in ast.body.of) switch decl {
			case DMember(m) if(m.attrs.exists(IsStatic)): cls.staticMembers.push(Member.fromAST(cls, m));
			case DMember(m): cls.members.push(Member.fromAST(cls, m));

			case DModule(m): cls.addTypeDecl(Module.fromAST(cls, m));

			case DClass(c): cls.addTypeDecl(Class.fromAST(cls, c));

			case DProtocol(p): cls.addTypeDecl(Protocol.fromAST(cls, p));

			case DKind(k): cls.addTypeDecl(Kind.fromAST(cls, k));
			
			case DAlias(a): cls.addTypeDecl(Alias.fromAST(cls, a));

			case DCategory(c): cls.categories.push(Category.fromAST(cls, c));

			case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(cls, m).forEach(x -> cls.staticMethods.push(x));
			case DMethod(m): cls.methods.push(Method.fromAST(cls, m));

			case DInit(i): cls.inits.push(Init.fromAST(cls, i));

			case DOperator(o): Operator.fromAST(cls, o).forEach(x -> cls.operators.push(x));

			case DDefaultInit(i) if(cls.staticInit.isSome()): cls.staticInit = Some(StaticInit.fromAST(cls, i));
			case DDefaultInit(i): cls.defaultInit = Some(DefaultInit.fromAST(cls, i));
			
			case DDeinit(d) if(cls.staticDeinit.isSome()): cls.staticDeinit = Some(StaticDeinit.fromAST(cls, d));
			case DDeinit(d): cls.deinit = Some(Deinit.fromAST(cls, d));
			
			default: cls.errors.push(Errors.unexpectedDecl(cls, ast.name.name, decl));
		}

		return cls;
	}

	override function hasErrors() {
		return super.hasErrors()
			|| members.some(m -> m.hasErrors())
			|| methods.some(m -> m.hasErrors())
			|| inits.some(i -> i.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(member in members) result = result.concat(member.allErrors());
		for(method in methods) result = result.concat(method.allErrors());
		for(init in inits) result = result.concat(init.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}

	inline function declName() {
		return "class";
	}


	override function isNative(kind: NativeKind) {
		return native.exists(nat -> nat.matches(kind));
	}


	override function hasParentDecl(decl: TypeDecl) {
		return this == decl
			|| parents.some(p -> p.hasParentDecl(decl))
			|| (!native.match(Some(NVoid)) && decl == Pass2.STD_Value)
			|| super.hasParentDecl(decl);
	}


	override function defaultSingleStatic(name: String, from: ITypeDecl, getter = false) {
		if(!native.match(Some(NVoid | NBool))) {
			return Pass2.STD_Value.findSingleStatic(name, from, getter);
		} else {
			return null;
		}
	}

	override function findSingleStatic(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleStaticKind> {
		if(cache.contains(thisType)) return null;
		
		if(!getter) {
			for(init in inits) init._match(
				at(si is SingleInit) => {
					if(si.name.name == name && from.canSeeMethod(si)) {
						return SSInit(si);
					}
				},
				at(mi is MultiInit) => {
					if(mi.params[0].label.name == name && mi.params.every(p -> p.value != null)) {
						return SSMultiInit(mi);
					}
				},
				_ => {}
			);
		}

		return super.findSingleStatic(name, from, getter, cache);
	}


	override function findMultiStatic(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return [];
		
		final candidates: Array<MultiStaticKind> = [];

		names._match(at([name]) => for(mem in staticMembers) {
			if(mem.matchesSetter(name) && from.canSeeMember(mem)) {
				candidates.push(MSMember(mem));
			}
		}, _ => {});

		if(setter) {
			throw "todo";
		} else {
			for(mth in staticMethods) mth._match(
				at(mm is MultiStaticMethod) => if(from.canSeeMethod(mm))
					mm.params.matchesNames(names)._match(
						at(Yes) => candidates.push(MSMethod(mm)),
						at(Partial) => candidates.push(MSMethod(mm, true)),
						at(No) => {}
					),
				_ => {}
			);

			for(init in inits) init._match(
				at(mi is MultiInit) => if(from.canSeeMethod(mi))
					mi.params.matchesNames(names)._match(
						at(Yes) => candidates.push(MSInit(mi)),
						at(Partial) => candidates.push(MSInit(mi, true)),
						at(No) => {}
					),
				_ => {}
			);

			// BAD
			if(candidates.length == 0 && !names.contains("_") && names.isUnique()) {
				final mems = instMembers(this);
				final found = [];
				var bad = false;

				for(name in names) {
					  mems.find(mem -> mem.name.name == name)._match(
						at(mem!) => if(from.canSeeMember(mem)) {
							found.push(mem);
						} else {
							bad = true;
							break;
						},

						// ???
						_ => {
							bad = true;
							break;
						}
					);
				}

				if(!bad) {
					candidates.push(MSMemberwiseInit(found));
				}
			}
		}

		for(parent in parents) {
			candidates.pushAll(parent.findMultiStatic(names, from, setter, cache));
		}

		for(refinee in refinees) {
			candidates.pushAll(refinee.findMultiStatic(names, from, setter, cache));
		}

		return candidates;
	}


	override function defaultSingleInst(name: String, from: ITypeDecl, getter = false) {
		if(!native.match(Some(NVoid)) && !getter) {
			return Pass2.STD_Value.findSingleInst(name, from, getter);
		} else {
			return null;
		}
	}

	override function findCast(target: Type, from: ITypeDecl, cache: List<Type> = Nil): Array<CastKind> {
		final res = super.findCast(target, from, cache);

		native._match(
			at(Some(nat)) => {
				if(!nat.match(NPtr(_)) && target.isNative(nat)) {
					res.push(CNative(target));
				}
			},
			_ => {}
		);

		return res;
	}


	override function defaultUnaryOp(op: UnaryOp, from: ITypeDecl): Null<UnaryOpKind> {
		if(!native.match(Some(NVoid))) {
			return Pass2.STD_Value.findUnaryOp(op, from);
		} else {
			return null;
		}
	}


	override function defaultBinaryOp(op: BinaryOp, from: ITypeDecl) {
		if(!native.match(Some(NVoid))) {
			return Pass2.STD_Value.findBinaryOp(op, from);
		} else {
			return [];
		}
	}
}