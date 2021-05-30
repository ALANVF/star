package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;

using typing.GenericRule.Tools;

// Dear god what have I gotten myself into

@:build(util.Auto.build({keepInit: true}))
class Generic {
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType;
	final span: Span;
	final name: Ident;
	var params: Option<Array<Type>>;
	var parents: Option<Array<Type>>;
	var rule: Option<GenericRule>;
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
	var native: Option<NativeKind> = None;
	var isFlags: Bool = false;
	var isStrong: Bool = false;
	var isUncounted: Bool = false;
	@:ignore var thisType: Type;

	function new() {
		thisType = new Type(TGeneric(this));
	}

	static function fromAST(lookup, ast: parsing.ast.decls.GenericParam): Generic {
		final generic = new Generic({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: ast.params.map(p -> p.of.map(lookup.makeTypePath)),
			parents: ast.parents.map(p -> p.parents.map(lookup.makeTypePath)),
			rule: ast.rule.map(r -> GenericRule.fromAST(lookup, r.rule))
		});
		
		for(attr => span in ast.attrs) switch attr {
			case IsNative(_, _, _) if(generic.native.isSome()): generic.errors.push(Errors.duplicateAttribute(generic, ast.name.name, "native", span));
			case IsNative(_, [{label: {name: "repr"}, expr: ELitsym(_, repr)}], _): switch repr {
				case "void": generic.native = Some(NVoid);
				case "bool": generic.native = Some(NBool);
				case "voidptr": generic.native = Some(NVoidPtr);
				default: generic.errors.push(Errors.invalidAttribute(generic, generic.name.name, "native", span));
			}
			case IsNative(_, [
				{label: {name: "repr"}, expr: ELitsym(_, "ptr")},
				{label: {name: "elem"}, expr: EType(t)}
			], _): generic.native = Some(NPtr(generic.makeTypePath(t)));
			case IsNative(_, [
				{label: {name: "repr"}, expr: ELitsym(_, "dec")},
				{label: {name: "bits"}, expr: EInt(_, bits, _)}
			], _): switch bits {
				case 32: generic.native = Some(NDec32);
				case 64: generic.native = Some(NDec64);
				default: generic.errors.push(Errors.invalidAttribute(generic, generic.name.name, "native", span));
			}
			case IsNative(_, [
				{label: {name: "repr"}, expr: ELitsym(_, "int")},
				{label: {name: "bits"}, expr: EInt(_, bits, _)},
				{label: {name: "signed"}, expr: EBool(_, signed)}
			], _): switch bits {
				case 8: generic.native = Some(signed ? NInt8 : NUInt8);
				case 16: generic.native = Some(signed ? NInt16 : NUInt16);
				case 32: generic.native = Some(signed ? NInt32 : NUInt32);
				case 64: generic.native = Some(signed ? NInt64 : NUInt64);
				default: generic.errors.push(Errors.invalidAttribute(generic, generic.name.name, "native", span));
			}
			case IsNative(_, _, _): generic.errors.push(Errors.invalidAttribute(generic, generic.name.name, "native", span));
			
			case IsFlags: generic.isFlags = true;
			
			case IsStrong: generic.isStrong = true;

			case IsUncounted: generic.isUncounted = true;
		}

		if(ast.body.isSome()) {
			for(decl in ast.body.value().of) switch decl {
				case DMember(m) if(m.attrs.exists(IsStatic)): generic.staticMembers.push(Member.fromAST(generic, m));
				case DMember(m): generic.members.push(Member.fromAST(generic, m));

				case DCase(c = {kind: Tagged(_)}): generic.taggedCases.push(TaggedCase.fromAST(generic, c));
				case DCase(c = {kind: Scalar(_, _)}): generic.valueCases.push(ValueCase.fromAST(generic, c));
	
				case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(generic, m).forEach(generic.staticMethods.push);
				case DMethod(m): generic.methods.push(Method.fromAST(generic, m));
	
				case DInit(i): generic.inits.push(Init.fromAST(generic, i));
	
				case DOperator(o): Operator.fromAST(generic, o).forEach(generic.operators.push);
	
				case DDefaultInit(i) if(generic.staticInit.isSome()): generic.staticInit = Some(StaticInit.fromAST(generic, i));
				case DDefaultInit(i): generic.defaultInit = Some(DefaultInit.fromAST(generic, i));
				
				case DDeinit(d) if(generic.staticDeinit.isSome()): generic.staticDeinit = Some(StaticDeinit.fromAST(generic, d));
				case DDeinit(d): generic.deinit = Some(Deinit.fromAST(generic, d));
				
				default: generic.errors.push(Errors.unexpectedDecl(generic, ast.name.name, decl));
			}
		}

		return generic;
	}

	inline function declName() {
		return "generic type";
	}

	function findType(path: List<String>, absolute = false, cache: List<{}> = Nil) {
		if(cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}

		return if(absolute) {
			lookup.findType(path, true, cache);
		} else {
			None;
		}
	}

	function makeTypePath(path) {
		return new Type(TPath(path, this));
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
			|| taggedCases.some(c -> c.hasErrors());
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

		return result;
	}
}