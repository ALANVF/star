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
	TInt8;
	TUInt8;
	TChar8_t;
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
	static inline function fromType(_: std.Enum<Type>, cmp: Compiler, type: typing.Type) {
		return switch type.t {
			case TPath(path, _): Type.fromTypePath(cmp, path);
			case TConcrete(decl): throw "todo";
			case TThis(span, source): throw "todo";
			case TErased(span): throw "todo";
			case TMulti(types): throw "todo";
			case TApplied(type, params): throw "todo";
			case TGeneric(generic): throw "todo";
			case TModular(type, _): Type.fromType(cmp, type);
		}
	}
	
	static inline function fromTypePath(_: std.Enum<Type>, cmp: Compiler, path: typing.TypePath) {
		return TPath(path.mapArray(s -> switch s {
			case Named(_, name, args): {name: name, args: args.map(a -> a.of.map(arg -> Type.fromTypePath(cmp, arg)))};
			case Blank(span, args):
				cmp.addError(invalidType(span));			
				{name: "$INVALID$", args: args.map(a -> a.of.map(arg -> Type.fromTypePath(cmp, arg)))};
		}));
	}
}