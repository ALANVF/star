package compiler.nim;

import compiler.nim.Type._Type;
import compiler.nim.TypeDecl._TypeDecl;
import compiler.nim.Expr;
import compiler.nim.Wrap.*;

@:using(compiler.nim.AnyType.AnyTypeTools)
enum AnyType<T> {
	// Primitives:
	TInt;
	TInt8;
	TInt16;
	TInt32;
	TInt64;
	TUInt;
	TUInt8;
	TUInt16;
	TUInt32;
	TUInt64;
	TFloat;
	TFloat32;
	TFloat64;
	TBool;
	TChar;
	TString;
	TCString;
	TOrdinal;
	
	// Refs:
	TPointer;
	TPtr(type: AnyType<T>);
	TRef(type: AnyType<T>);
	//TVTPtr(type: AnyType<T>);
	//TVTRef(type: AnyType<T>);
	
	// Access:
	TVar(type: Type);
	TConst(type: Type);
	TShared(type: Type);
	
	// Compound:
	TRange(type: Type);
	TSet(type: Type);
	TSeq(type: Type);
	TArray(index: Type, type: Type);
	TOpenArray(type: Type);
	TUncheckedArray(type: Type);
	TTuple(types: Types);
	TNamedTuple(fields: Fields);
	
	// Logical:
	TAnd(types: Types);
	TOr(types: Types);
	TNot(type: Type);
	
	// Procs:
	TProc(?params: Fields, ?ret: Type, ?pragmas: Pragmas);
	TIter(?params: Fields, ?ret: Type, ?pragmas: Pragmas);
	
	// Special:
	TRootObj;
	//Exception;
	//TCatchableError;
	TSelf;
	TVarargs(type: Type, ?transform: Expr): AnyType<_Type>;
	TTypeof(expr: Expr, ?typeOfProc: Bool);
	TTypeDesc(type: Type);
	TStatic(type: Type);
	
	// Kinds:
	TPtrKind;
	TRefKind;
	TRangeKind;
	TSetKind;
	TSeqKind;
	TArrayKind;
	TOpenArrayKind;
	TTupleKind;
	TTypeKind;
	TTypeDescKind;
	TEnumKind;
	TObjectKind;
	TProcKind;
	TIterKind;
	TDistinctKind;
	TAny;
	TAuto;
	
	// Macros:
	TTyped: AnyType<_Type>;
	TUntyped: AnyType<_Type>;
	
	// Named:
	TName(name: String, ?args: TypeArgs);
	TSym(sym: String, ?args: TypeArgs);
	TPath(types: Array<Type>);
	
	// Decls:
	TEnum(entries: Array<{n: Name, ?v: Expr}>): AnyType<_TypeDecl>;
	//TTupleDecl(fields: Fields): AnyType<_TypeDecl>;
	TObject(?parent: Type, ?pragmas: Pragmas, fields: ObjFields): AnyType<_TypeDecl>;
	TConcept(vars: Types, ?self: Name, ?of: Types, body: Stmts): AnyType<_TypeDecl>;
	
	// Experimental:
	TVoid: AnyType<_Type>;
	TNotNil(type: Type);
	
	// Misc:
	TDistinct(type: Type);
	TType(type: Type);
	TParen(type: Type);
	TGeneric(type: Type, args: TypeArgs);
	TExpr(expr: Expr);
}

final TAB = "    ";

function proc(kw: String, tabs: String, ?params: Fields, ?ret: Type, ?pragmas: Pragmas) {
	var res = kw;
	
	params._and(p => {
		res += p.paramsToNim(tabs);
	});
	
	ret._and(r => {
		res += ": ";
		res += r.toNim(tabs);
	});
	
	 pragmas._and(p => {
		res += " ";
		res += p.toNim();
	});
	
	return res;
}

@:publicFields
class AnyTypeTools {
	static function toNim<T>(self: AnyType<T>, tabs = "") return self._match(
		at(TInt) => "int",
		at(TInt8) => "int8",
		at(TInt16) => "int16",
		at(TInt32) => "int32",
		at(TInt64) => "int64",
		at(TUInt) => "uint",
		at(TUInt8) => "uint8",
		at(TUInt16) => "uint16",
		at(TUInt32) => "uint32",
		at(TUInt64) => "uint64",
		at(TFloat) => "float",
		at(TFloat32) => "float32",
		at(TFloat64) => "float64",
		at(TBool) => "bool",
		at(TChar) => "char",
		at(TString) => "string",
		at(TCString) => "cstring",
		at(TOrdinal) => "Ordinal",
		
		at(TPointer) => "pointer",
		at(TPtr(t)) => "ptr " + t.toNim(tabs),
		at(TRef(t)) => "ref " + t.toNim(tabs),
		
		at(TVar(t)) => "var " + t.toNim(tabs),
		at(TConst(t)) => "const " + t.toNim(tabs),
		at(TShared(t)) => "shared " + t.toNim(tabs),
		
		at(TRange(t)) => 'range[${t.toNim(tabs)}]',
		at(TSet(t)) => 'set[${t.toNim(tabs)}]',
		at(TSeq(t)) => 'seq[${t.toNim(tabs)}]',
		at(TArray(i, t)) => 'array[${i.toNim(tabs)}, ${t.toNim(tabs)}]',
		at(TOpenArray(t)) => 'openarray[${t.toNim(tabs)}]',
		at(TUncheckedArray(t)) => 'UncheckedArray[${t.toNim(tabs)}]',
		at(TTuple(types)) => '(${types.toNim(tabs)})',
		at(TNamedTuple(fields)) => 'tuple[${fields.inlineToNim(tabs)}]',
		
		at(TAnd(types)) => types.joinMap(" and ", t -> wrapInfixType(t, true).toNim(tabs)),
		at(TOr(types)) => types.joinMap(" or ", t -> wrapInfixType(t, true).toNim(tabs)),
		at(TNot(t)) => 'not(${t.toNim(tabs)})',
		
		at(TProc(params, ret, pragmas)) => proc("proc", tabs, params, ret, pragmas),
		at(TIter(params, ret, pragmas)) => proc("iterator", tabs, params, ret, pragmas),
		
		at(TRootObj) => "RootObj",
		at(TSelf) => "Self",
		at(TVarargs(t, null)) => 'varargs[${t.toNim(tabs)}]',
		at(TVarargs(t, tr!!)) => 'varargs[${t.toNim(tabs)}, ${tr.toNim(tabs)}]',
		at(TTypeof(expr, true)) => 'typeof(${expr.toNim(tabs)}, typeOfProc)',
		at(TTypeof(expr)) => 'typeof(${expr.toNim(tabs)})',
		at(TTypeDesc(t)) => 'typedesc[${t.toNim(tabs)}]',
		at(TStatic(t)) => 'static[${t.toNim(tabs)}]',
		
		at(TPtrKind) => "ptr",
		at(TRefKind) => "ref",
		at(TRangeKind) => "range",
		at(TSetKind) => "set",
		at(TSeqKind) => "seq",
		at(TArrayKind) => "array",
		at(TOpenArrayKind) => "openarray",
		at(TTupleKind) => "tuple",
		at(TTypeKind) => "type",
		at(TTypeDescKind) => "typedesc",
		at(TEnumKind) => "enum",
		at(TObjectKind) => "object",
		at(TProcKind) => "proc",
		at(TIterKind) => "iterator",
		at(TDistinctKind) => "distinct",
		at(TAny) => "any",
		at(TAuto) => "auto",
		
		at(TTyped) => "typed",
		at(TUntyped) => "untyped",
		
		at(TName(n, null)) => n,
		at(TName(n, a!!)) => n + a.toNim(tabs),
		at(TSym(s, null)) => '`$s`',
		at(TSym(s, a!!)) => '`$s`${a.toNim(tabs)}',
		at(TPath([])) => throw "error!",
		at(TPath([t])) => t.toNim(tabs),
		at(TPath(tp)) => tp.joinMap(".", t -> wrapImmediateType(t).toNim(tabs)),
		
		at(TEnum(entries)) => {
			var res = "enum";
			
			tabs += TAB;
			
			final ws = '\n$tabs';
			
			for(e in entries) {
				res += ws;
				res += e.n.toNim();
				e.v._and(v => {
					res += " = ";
					res += v.toNim(tabs);
				});
			}
			
			res;
		},
		at(TObject(parent, pragmas, fields)) => {
			var res = parent._andOr(
				p => "object of " + p.toNim(tabs),
				"object"
			);
			
			pragmas._and(p => {
				res += " ";
				res += p.toNim();
			});
			
			if(fields.length > 0) {
				res += fields.toNim(tabs);
			}
			
			res;
		},
		at(TConcept(vars, self_, of, body)) => {
			var res = "concept ";
			
			res += vars.toNim(tabs);
			
			self_._and(s => {
				res += ", type ";
				res += s.toNim();
			});
			
			of._and(o => {
				res += " of ";
				res += o.toNim(tabs);
			});
			
			res += body.toNim(tabs);
			
			res;
		},
		
		at(TVoid) => "void",
		at(TNotNil(t)) => wrapImmediateType(t).toNim(tabs) + " not nil",
		
		at(TDistinct(t)) => "distinct " + wrapImmediateType(t).toNim(tabs),
		at(TType(t)) => "type " + wrapImmediateType(t).toNim(tabs),
		at(TParen(t)) => "(" + t.toNim(tabs) + ")",
		at(TGeneric(t, a)) => wrapImmediateType(t).toNim(tabs) + "[" + a.toNim(tabs) + "]",
		at(TExpr(expr)) => expr.toNim(tabs)
	);
}