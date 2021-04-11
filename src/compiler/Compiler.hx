package compiler;

import reporting.Diagnostic;

@:publicFields
class Compiler {
	final errors = new Array<Diagnostic>();
	final stmts: Array<DeclStmt>;
	
	function new() {
		stmts = [
			DIncludeLib("cstdint"),
			DTypeDecl(
				new Alias({
					template: Some(new Template({
						types: [
							new TypeParam({
								name: Some("Ret")
							}),
							new TypeParam({
								name: Some("Args"),
								isVariadic: true
							})
						]
					})),
					path: [{name: "$Func", args: None}],
					type: TFunc(
						TPath([{name: "Ret", args: None}]),
						[
							TPack(
								TPath([{name: "Args", args: None}])
							)
						]
					)
				})
			),
			DTypeDecl(
				new Alias({
					template: Some(new Template({
						types: [
							new TypeParam({
								name: Some("Elem")
							})
						],
					})),
					path: [{name: "$Array", args: None}],
					type: TArray(
						TPath([{name: "Elem", args: None}])
					)
				})
			),
			DTypeDecl(
				new Alias({
					template: Some(new Template({
						types: [
							new TypeParam({
								name: Some("Elem")
							}),
							new TypeParam({
								type: Some(TInt),
								name: Some("Size")
							})
						],
					})),
					path: [{name: "$ArrayN", args: None}],
					type: TArrayN(
						TPath([{name: "Elem", args: None}]),
						EName("Size")
					)
				})
			)
		];
	}
	
	inline function addError(error: Diagnostic) errors.push(error);
	
	inline function addStmt(stmt: DeclStmt) stmts.push(stmt);
}