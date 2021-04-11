package compiler;

import reporting.Diagnostic;

@:publicFields
class Compiler {
	final errors = new Array<Diagnostic>();
	final includes = new Array<String>();
	final types = new Array<TypeDecl>();
	
	function new() {}
	
	inline function addError(error: Diagnostic) errors.push(error);
	
	inline function addType(type: TypeDecl) types.push(type);
}