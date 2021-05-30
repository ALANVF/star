package compiler;

import reporting.Diagnostic;

@:publicFields
class Compiler {
	final errors = new Array<Diagnostic>();
	final stmts: Array<DeclStmt>;
	
	function new() {
		stmts = [
			DIncludeLib("cstdint"),
			DIncludeLib("type_traits"),
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
			),
			DTypeDecl(
				new Struct({
					template: Some(new Template({
						types: [
							new TypeParam({
								type: Some(TPath([{name: "$Array", args: Some([TConst(TChar)])}])),
								name: Some("Name")
							})
						]
					})),
					path: [{name: "$L", args: None}]
				})
			),
			DTypeDecl(
				new Struct({
					template: Some(new Template({
						types: [
							new TypeParam({
								name: Some("T")
							})
						]
					})),
					path: [{name: "$T", args: None}],
					body: {
						normal: [
							DTypeDecl(
								new Alias({
									path: [{name: "Type", args: None}],
									type: TPath([{name: "T", args: None}])
								})
							)
						]
					}
				})
			)
		];
	}
	
	inline function addError(error: Diagnostic) errors.push(error);
	
	inline function addStmt(stmt: DeclStmt) stmts.push(stmt);
}