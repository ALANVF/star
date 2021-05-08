package compiler;

import reporting.Severity;
import reporting.Diagnostic;

inline function invalidType(span) {
	return new Diagnostic({
		severity: Severity.ERROR,
		message: "Invalid type",
		info: [
			Spanned({
				span: span,
				message: "Invalid type",
				isPrimary: true
			})
		]
	});
}

enum Type {
	TVoid;
	TBool;
	TInt;
	TInt8;
	TUInt8;
	TInt16;
	TUInt16;
	TInt32;
	TUInt32;
	TInt64;
	TUInt64;
	TSize_t;
	TFloat;
	TDouble;
	TLongDouble;
	
	TChar;
	TChar8_t;
	
	TUnsigned(t: Type);
	TSigned(t: Type);
	
	TConst(t: Type);
	TVolatile(t: Type);
	
	TPtr(t: Type);
	TLVal(t: Type);
	TRVal(t: Type);
	TArray(t: Type);
	TArrayN(t: Type, size: Expr);
	
	TFunc(ret: Type, params: Array<Type>);
	
	TStruct(body: DeclBody);
	TExplicitStruct(path: TypePath);
	TNamedStruct(path: TypePath, body: DeclBody);
	
	TUnion(body: DeclBody);
	TExplicitUnion(path: TypePath);
	TNamedUnion(path: TypePath, body: DeclBody);
	
	TEnum(cases: EnumCases);
	TExplicitEnum(path: TypePath);
	TNamedEnum(path: TypePath, cases: EnumCases);
	
	TTypename(t: Type);
	TPath(path: TypePath);
	TLookup(t: Type, path: TypePath);
	
	TAuto(constraint: Option<Type>);
	TDecltype(expr: Expr);
	TDecltypeAuto(constraint: Option<Type>);
	
	TPack(type: Type);
	
	TExpr(expr: Expr);
}

@:publicFields
class TypeTools {
	static inline function formTypes(types: Array<Type>) {
		return types.map(t -> t.form()).join(", ");
	}
	
	static function fromType(_: std.Enum<Type>, cmp: Compiler, type: typing.Type) {
		return switch type.t {
			case TPath(path, _): Type.fromTypePath(cmp, path);
			case TConcrete(decl): TPath(TypePathTools.getFullPath(cmp, decl));
			case TThis(_, _): TPath([{name: "$This", args: None}]);
			case TErased(span): throw "todo";
			case TMulti(types): throw "todo";
			case TApplied({t: TThis(_, _)}, params): TPath([{name: "$This", args: Some(params.map(p -> Type.fromType(cmp, p)))}]);
			case TApplied(type, params): throw "todo";
			case TGeneric(generic): throw "todo";
			case TModular(type, _): Type.fromType(cmp, type);
		}
	}
	
	static function fromTypePath(_: std.Enum<Type>, cmp: Compiler, path: typing.TypePath) {
		return TPath(path.mapArray(s -> switch s {
			case Named(_, name, args): {name: name, args: args.map(a -> a.of.map(arg -> Type.fromTypePath(cmp, arg)))};
			case Blank(span, args):
				cmp.addError(invalidType(span));			
				{name: "$INVALID$", args: args.map(a -> a.of.map(arg -> Type.fromTypePath(cmp, arg)))};
		}));
	}
	
	static function form(type: Type) {
		return switch type {
			case TVoid: "void";
			case TBool: "bool";
			case TInt: "int";
			case TInt8: "int8_t";
			case TUInt8: "uint8_t";
			case TInt16: "int16_t";
			case TUInt16: "uint16_t";
			case TInt32: "int32_t";
			case TUInt32: "uint32_t";
			case TInt64: "int64_t";
			case TUInt64: "uint64_t";
			case TSize_t: "size_t";
			case TFloat: "float";
			case TDouble: "double";
			case TLongDouble: "long double";
			
			case TChar: "char";
			case TChar8_t: "char8_t";
			
			case TUnsigned(t): "unsigned " + t.form();
			case TSigned(t): "signed " + t.form();
			
			case TConst(t): t.form() + " const";
			case TVolatile(t): t.form() + " volatile";
			
			case TPtr(t): t.form() + "*";
			case TLVal(t): t.form() + "&";
			case TRVal(t): t.form() + "&&";
			case TArray(t): t.form() + "[]";
			case TArrayN(t, s): t.form() + "[" + s.form() + "]";
			
			case TFunc(ret, params): ret.form() + "(" + params.map(p -> p.form()).join(", ") + ")";
			
			case TStruct(body): "struct " + body.form();
			case TExplicitStruct(path): "struct " + path.form();
			case TNamedStruct(path, body): "struct " + path.form() + " " + body.form();
			
			case TUnion(body): "union " + body.form();
			case TExplicitUnion(path): "union " + path.form();
			case TNamedUnion(path, body): "union " + path.form() + " " + body.form();
			
			case TEnum(cases): "enum " + cases.form();
			case TExplicitEnum(path): "enum " + path.form();
			case TNamedEnum(path, cases): "enum " + path.form() + " " + cases.form();
			
			case TTypename(t): "typename " + t.form();
			case TPath(path): path.form();
			case TLookup(t, path): t.form() + "::" + path.form();
			
			case TAuto(None): "auto";
			case TAuto(Some(c)): c.form() + " auto";
			case TDecltype(expr): "decltype(" + expr.form() + ")";
			case TDecltypeAuto(None): "decltype(auto)";
			case TDecltypeAuto(Some(c)): c.form() + " decltype(auto)";
			
			case TPack(t): t.form() + "...";
			
			case TExpr(expr): expr.form();
		}
	}
}