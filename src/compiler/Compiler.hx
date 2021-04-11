package compiler;

import reporting.Diagnostic;

@:publicFields
class Compiler {
	final errors = new Array<Diagnostic>();
	final stmts = new Array<DeclStmt>();
	
	function new() {}
	
	inline function addError(error: Diagnostic) errors.push(error);
	
	inline function addStmt(stmt: DeclStmt) stmts.push(stmt);
}