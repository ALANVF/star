package compiler.nim;

@:using(compiler.nim.Stmt.StmtTools)
enum Stmt {
	SExpr(expr: Expr);
	
	SVar(section: VarSection);
	SLet(section: VarSection);
	SConst(section: VarSection);
	SUsing(section: VarSection);
	SType(section: TypeSection);
	
	SProc(routine: Routine);
	SFunc(routine: Routine);
	SMethod(routine: Routine);
	SIterator(routine: Routine);
	SConverter(routine: Routine);
	STemplate(routine: Routine);
	SMacro(routine: Routine);
	
	SWhile(cond: Expr, body: Stmts);
	
	SReturn(?expr: Expr);
	SRaise(?expr: Expr);
	SYield(?expr: Expr);
	SDiscard(?expr: Expr);
	SBreak(?expr: Expr);
	SContinue(?expr: Expr);
	
	SStatic(stmts: Stmts);
	SDefer(stmts: Stmts);
	
	SFromImport(from: Expr, imports: Exprs, ?as: Exprs);
	SImportExcept(import_: Expr, except: Exprs);
	SImport(imports: Exprs, ?as: Expr);
	SInclude(import_: Expr);
	SBind(names: Exprs);
	SMixin(names: Exprs);
	
	SPragma(pragmas: Pragmas);
	
	//asm
}

final TAB = "    ";

@:publicFields
class StmtTools {
	static function toNim(self: Stmt, tabs = "") return self._match(
		at(SExpr(expr)) => expr.toNim(tabs),
		
		at(SVar(section)) => "var" + section.toNim(tabs),
		at(SLet(section)) => "let" + section.toNim(tabs),
		at(SConst(section)) => "const" + section.toNim(tabs),
		at(SUsing(section)) => "using" + section.toNim(tabs),
		at(SType(section)) => "type" + section.toNim(tabs),
		
		at(SProc(routine)) => "proc " + routine.toNim(tabs),
		at(SFunc(routine)) => "func " + routine.toNim(tabs),
		at(SMethod(routine)) => "method " + routine.toNim(tabs),
		at(SIterator(routine)) => "iterator " + routine.toNim(tabs),
		at(SConverter(routine)) => "converter " + routine.toNim(tabs),
		at(STemplate(routine)) => "template " + routine.toNim(tabs),
		at(SMacro(routine)) => "macro " + routine.toNim(tabs),
		
		at(SWhile(cond, body)) => "while " + cond.toNim(tabs) + ":" + body.toNim(tabs),
		
		at(SReturn(null)) => "return",
		at(SRaise(null)) => "raise",
		at(SYield(null)) => "yield",
		at(SDiscard(null)) => "discard",
		at(SBreak(null)) => "break",
		at(SContinue(null)) => "continue",
		
		at(SReturn(expr!!)) => "return " + expr.toNim(tabs),
		at(SRaise(expr!!)) => "raise " + expr.toNim(tabs),
		at(SYield(expr!!)) => "yield " + expr.toNim(tabs),
		at(SDiscard(expr!!)) => "discard " + expr.toNim(tabs),
		at(SBreak(expr!!)) => "break " + expr.toNim(tabs),
		at(SContinue(expr!!)) => "continue " + expr.toNim(tabs),
		
		at(SStatic(stmts)) => "static:" + stmts.toNim(tabs),
		at(SDefer(stmts)) => "defer:" + stmts.toNim(tabs),
		
		at(SFromImport(from, imports, as)) => {
			final res = "from " + from.toNim(tabs) + " import " + imports.toNim(tabs);
			as._andOr(
				a => '$res as ' + a.toNim(tabs),
				res
			);
		},
		at(SImportExcept(imp, except)) => "import " + imp.toNim(tabs) + " except " + except.toNim(tabs),
		at(SImport(imp, null)) => "import " + imp.toNim(tabs),
		at(SImport(imports, as!!)) => "import " + imports.toNim(tabs) + " as " + as.toNim(tabs),
		at(SInclude(imp)) => "include " + imp.toNim(tabs),
		at(SBind(names)) => "bind " + names.toNim(tabs),
		at(SMixin(names)) => "mixin " + names.toNim(tabs),
		
		at(SPragma(pragmas)) => pragmas.toNim()
	);
}