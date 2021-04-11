package compiler;

import reporting.Diagnostic;

@:publicFields
class Compiler {
	final errors = new Array<Diagnostic>();
	final stmts: Array<DeclStmt>;
	
	function new() {
		stmts = [
			DIncludeLib("cstdint")
		];
	}
	
	inline function addError(error: Diagnostic) errors.push(error);
	
	inline function addStmt(stmt: DeclStmt) stmts.push(stmt);
}