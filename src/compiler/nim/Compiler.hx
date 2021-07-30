package compiler.nim;

import compiler.nim.Name;
import compiler.nim.Expr;
import compiler.nim.Stmt;
import compiler.nim.AnyType;
import compiler.nim.ObjField;

@:publicFields
class Compiler {
	static function test() {
		for(i => t in [
			SVar([
				{
					n: [NName("a")],
					t: TInt,
					v: EInt(1)
				}
			]),
			
			SVar([
				{
					n: [NName("a"), NSym("b")],
					t: TInt,
					v: EInt(1)
				}
			]),
			
			SVar([
				{
					n: [NName("a"), NName("b")],
					t: TInt
				},
				{
					n: [NName("c")],
					t: TFloat
				}
			]),
			
			SVar([
				{
					n: [NName("a")],
					t: TInt,
					v: EBlock([
						SExpr(EInfix(EInt(1), IAdd, EInt(2)))
					])
				}
			]),
			
			SVar([
				{
					n: [NName("a"), NName("b"), NName("c")],
					unpack: true,
					v: ETuple([EInt(1), EInt(2), EInt(3)])
				}
			]),
			
			SProc({
				name: NName("add"),
				params: [
					{
						n: [NName("a"), NName("b")],
						t: TInt
					}
				],
				ret: TInt,
				body: [
					SReturn(EInfix(EName("a"), IAdd, EName("b")))
				]
			}),
			
			SType([
				{
					name: NName("Add"),
					params: [
						{
							n: [NName("Rhs"), NName("Res")]
						}
					],
					decl: TConcept([
						TName("lhs")
					], [
						SExpr(EIs(EInfix(EName("lhs"), IAdd, EType(TName("Rhs"))), TName("Res")))
					])
				}
			]),
			
			SProc({
				name: NName("add"),
				typeParams: [
					{
						n: [NName("Rhs"), NName("Res")]
					},
					{
						n: [NName("Lhs")],
						t: TName("Add", [{t: TName("Rhs")}, {t: TName("Res")}])
					}
				],
				params: [
					{
						n: [NName("a")],
						t: TName("Lhs")
					},
					{
						n: [NName("b")],
						t: TName("Rhs")
					}
				],
				ret: TName("Res"),
				body: [
					SReturn(EInfix(EName("a"), IAdd, EName("b")))
				]
			}),
			
			SProc({
				name: NName("add"),
				typeParams: [
					{
						n: [NName("Rhs"), NName("Res")]
					}
				],
				params: [
					{
						n: [NName("a")],
						t: TName("Add", [{t: TName("Rhs")}, {t: TName("Res")}])
					},
					{
						n: [NName("b")],
						t: TName("Rhs")
					}
				],
				ret: TName("Res"),
				body: [
					SReturn(EInfix(EName("a"), IAdd, EName("b")))
				]
			}),
			
			SType([
				{
					name: NSym("_Option"),
					params: [
						{
							n: [NName("T")]
						}
					],
					decl: TRef(TObject([
						FCase(NName("status"), TBool, [
							COf(ETrue, [
								FFields([
									{
										n: [NName("value")],
										t: TName("T")
									}
								])
							]),
							COf(EFalse, [
								FNil
							])
						])
					]))
				},
				{
					name: NName("Option"),
					params: [
						{
							n: [NName("T")],
							t: TAuto
						}
					],
					decl: TNotNil(TSym("_Option", [{t: TName("T")}]))
				}
			])
		]) {
			Sys.println('\n\n=== ${i + 1} ===\n');
			Sys.println(t.toNim());
		}
		
		for(i => t in [
			ECase(
				EString("b"),
				[
					COf(EString("a"), [
						SExpr(ECall(EName("echo"), [], [EInt(1)]))
					]),
					COfAny([EString("b"), EString("c")], [
						SExpr(ECall(EName("echo"), [], [EInt(2)]))
					])
				],
				[
					SExpr(ECall(EName("echo"), [], [EInt(3)]))
				]
			),
			
			EAssign(EName("a"), ECase(
				EString("b"),
				[
					COf(EString("a"), [
						SExpr(ECall(EName("echo"), [], [EInt(1)]))
					]),
					COfAny([EString("b"), EString("c")], [
						SExpr(ECall(EName("echo"), [], [EInt(2)]))
					])
				],
				[
					SExpr(ECall(EName("echo"), [], [EInt(3)]))
				]
			))
		]) {
			Sys.println('\n\n=== ${i + 1} ===\n');
			Sys.println(t.toNim());
		}
	}
}