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

	function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(cache.contains(this)) {
			return None;
		}

		return path._match(
			at([[span, "This", []]], when(absolute)) => Some({t: TThis(this), span: span}),
			at([[span, "This", _]], when(absolute)) => {
				// prob shouldn't be attatched to *this* type var, but eh
				errors.push(Errors.notYetImplemented(span));
				None;
			},
			at([[span, typeName, args]], when(typeName == this.name.name && !cache.contains(this))) => {
				return lookup.findType(path, false, cache);
			},
			_ => if(absolute) lookup.findType(path, true, cache.prepend(this)) else None
		);
	}

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
}