package typing;

import parsing.ast.Ident;
import reporting.Diagnostic;

enum NativeClass {
	NVoid;
	NBool;
	NInt8;
	NUInt8;
	NInt16;
	NUInt16;
	NInt32;
	NUInt32;
	NInt64;
	NUInt64;
	NDec32;
	NDec64;
	NVoidPtr;
	NPtr(t: Type);
}

class Class extends Namespace {
	final parents: Array<Type> = [];
	final members: Array<Member> = [];
	final methods: Array<Method> = [];
	final inits: Array<Init> = [];
	final operators: Array<Operator> = [];
	var defaultInit: Option<DefaultInit> = None;
	var deinit: Option<Deinit> = None;
	var native: Option<NativeClass> = None;
	var isStrong: Bool = false;
	var isUncounted: Bool = false;
	var sealed: Option<Option<Type>> = None;

	static function fromAST(lookup, ast: parsing.ast.decls.Class) {
		final cls = new Class({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: None
		});

		for(generic in ast.generics.mapArray(Generic.fromAST.bind(lookup, _))) {
			cls.generics.add(generic.name.name, generic);
		}

		if(ast.params.isSome()) {
			cls.params = Some(ast.params.value().of.map(param -> cls.makeTypePath(param)));
		}

		if(ast.parents.isSome()) {
			for(parent in ast.parents.value().parents) {
				cls.parents.push(lookup.makeTypePath(parent));
			}
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(cls.hidden.isSome()): cls.errors.push(Errors.duplicateAttribute(cls, ast.name.name, "hidden", span));
			case IsHidden(None): cls.hidden = Some(None);
			case IsHidden(Some(outsideOf)): cls.hidden = Some(Some(lookup.makeTypePath(outsideOf)));

			case IsFriend(_) if(cls.friends.length != 0): cls.errors.push(Errors.duplicateAttribute(cls, ast.name.name, "friend", span));
			case IsFriend(One(friend)): cls.friends.push(lookup.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) cls.friends.push(lookup.makeTypePath(friend));

			case IsSealed(_) if(cls.sealed.isSome()): cls.errors.push(Errors.duplicateAttribute(cls, ast.name.name, "sealed", span));
			case IsSealed(None): cls.sealed = Some(None);
			case IsSealed(Some(outsideOf)): cls.sealed = Some(Some(lookup.makeTypePath(outsideOf)));

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

			case DModule(m): cls.decls.push(Module.fromAST(cls, m));

			case DClass(c): cls.decls.push(Class.fromAST(cls, c));

			case DProtocol(p): cls.decls.push(Protocol.fromAST(cls, p));

			case DKind(k): cls.decls.push(Kind.fromAST(cls, k));
			
			case DAlias(a): cls.decls.push(Alias.fromAST(cls, a));

			case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(cls, m).forEach(cls.staticMethods.push);
			case DMethod(m): cls.methods.push(Method.fromAST(cls, m));

			case DInit(i): cls.inits.push(Init.fromAST(cls, i));

			case DOperator(o): Operator.fromAST(cls, o).forEach(cls.operators.push);

			case DDefaultInit(i) if(cls.staticInit.isSome()): cls.staticInit = Some(StaticInit.fromAST(cls, i));
			case DDefaultInit(i): cls.defaultInit = Some(DefaultInit.fromAST(cls, i));
			
			case DDeinit(d) if(cls.staticDeinit.isSome()): cls.staticDeinit = Some(StaticDeinit.fromAST(cls, d));
			case DDeinit(d): cls.deinit = Some(Deinit.fromAST(cls, d));
			
			default: cls.errors.push(Errors.unexpectedDecl(cls, ast.name.name, decl));
		}

		return cls;
	}

	override function hasErrors() {
		return super.hasErrors() || members.some(m -> m.hasErrors()) || methods.some(m -> m.hasErrors())
			|| inits.some(i -> i.hasErrors()) || operators.some(o -> o.hasErrors());
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
}