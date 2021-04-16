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
	
	TStruct(members: Array<Member>);
	TExplicitStruct(path: TypePath);
	TNamedStruct(name: String, members: Array<Member>);
	
	TUnion(members: Array<Member>);
	TExplicitUnion(path: TypePath);
	TNamedUnion(path: TypePath, members: Array<Member>);
	
	TEnum(cases: EnumCases);
	TExplicitEnum(path: TypePath);
	TNamedEnum(path: TypePath, cases: EnumCases);
	
	TTypename(t: Type);
	TPath(path: TypePath);
	
	TAuto;
	TDecltype(expr: Expr);
	
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
			case TArray(t): "$Array<" + t.form() + ">";
			case TArrayN(t, s): throw "todo!";
			
			case TFunc(ret, []): "$Func<" + ret.form() + ">";
			case TFunc(ret, params): "$Func<" + ret.form() + ", " + params.map(p -> p.form()).join(", ") + ">";
			
			case TStruct(_): throw "todo!";
			case TExplicitStruct(path): "struct " + path.form();
			case TNamedStruct(_, _): throw "todo!";
			
			case TUnion(_): throw "todo!";
			case TExplicitUnion(path): "union " + path.form();
			case TNamedUnion(_, _): throw "todo!";
			
			case TEnum(_): throw "todo!";
			case TExplicitEnum(path): "enum " + path.form();
			case TNamedEnum(_, _): throw "todo!";
			
			case TTypename(t): "typename " + t.form();
			case TPath(path): path.form();
			
			case TAuto: "auto";
			case TDecltype(expr): throw "todo!";
			
			case TPack(t): t.form() + "...";
			
			case TExpr(expr): throw "todo!";
		}
	}
}