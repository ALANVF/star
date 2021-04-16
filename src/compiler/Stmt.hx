package compiler;

import util.Buffer;
using hx.strings.Strings;

@:using(compiler.Stmt.CondTools)
enum Cond {
	CExpr(e: Expr);
	CVarDecl(v: VarDecl);
}

@:using(compiler.Stmt.StmtTools)
enum Stmt {
	SExpr(expr: Expr);
	SVarDecl(v: VarDecl);
	SBlock(b: Block);
	SNone;
	SLabel(name: String, stmt: Stmt);
	SCase(expr: Expr, stmt: Stmt);
	SDefault(stmt: Stmt);
	SIf(isConstexpr: Bool, init: Option<VarDecl>, cond: Cond, ifTrue: Stmt, ifFalse: Option<Stmt>);
	SSwitch(init: Option<VarDecl>, cond: Cond, stmt: Stmt);
	SWhile(cond: Cond, stmt: Stmt);
	SDoWhile(stmt: Stmt, cond: Expr);
	SFor(init: Stmt, cond: Option<Expr>, update: Option<Expr>, stmt: Stmt);
	SForRange(init: Option<VarDecl>, decl: VarDecl, expr: Expr, stmt: Stmt);
	SBreak;
	SContinue;
	SReturn(expr: Option<Expr>);
	SGoto(label: String);
	STry(tryBlock: Block, cases: Array<{param: Param, body: Block}>, catchAll: Option<Block>);
}


@:publicFields
class CondTools {
	static function form(cond: Cond) return switch cond {
		case CExpr(e): e.form();
		case CVarDecl(v): v.form();
	}
}


@:publicFields
class StmtTools {
	static function form(stmt: Stmt, indent = 1) return switch stmt {
		case SExpr(expr): expr.form() + ";";
		
		case SVarDecl(v): v.form() + ";";
		
		case SBlock(b): b.form(indent);
		
		case SNone: ";";
		
		case SLabel(name, stmt): ExprTools.fixName(name) + ":\n" + "\t".repeat(indent) + stmt.form(indent);
		
		case SCase(expr, stmt): "case " + expr.form() + ": " + stmt.form(indent + 1);
		
		case SDefault(stmt): "default: " + stmt.form(indent + 1);
		
		case SIf(ce, i, c, t, f):
			final buf = new Buffer();
			
			buf.addString(if(ce) "if constexpr(" else "if(");
			i.forEach(i -> {
				buf.addString(i.form());
				buf.addString("; ");
			});
			buf.addString(c.form());
			buf.addString(") ");
			buf.addString(t.form(indent + 1));
			f.forEach(f -> {
				buf.addString(" else ");
				buf.addString(f.form(indent + 1));
			});
			
			buf.toString();
		
		case SSwitch(None, cond, stmt): "switch(" + cond.form() + ") " + stmt.form(indent + 1);
		case SSwitch(Some(init), cond, stmt): "switch(" + init.form() + "; " + cond.form() + ") " + stmt.form(indent + 1);
		
		case SWhile(cond, stmt): "while(" + cond.form() + ") " + stmt.form(indent + 1);
		
		case SDoWhile(stmt, cond): "do " + stmt.form(indent + 1) + " while(" + cond.form() + ");";
		
		case SFor(init, None, None, stmt): "for(" + init.form() + ";) " + stmt.form(indent + 1);
		case SFor(init, Some(cond), None, stmt): "for(" + init.form() + " " + cond.form() + ";) " + stmt.form(indent + 1);
		case SFor(init, None, Some(update), stmt): "for(" + init.form() + "; " + update.form() + ") " + stmt.form(indent + 1);
		case SFor(init, Some(cond), Some(update), stmt): "for(" + init.form() + " " + cond.form() + "; " + update.form() + ") " + stmt.form(indent + 1);
		
		case SForRange(None, decl, expr, stmt): "for(" + decl.form() + ": " + expr.form() + ") " + stmt.form(indent + 1);
		case SForRange(Some(init), decl, expr, stmt): "for(" + init.form() + "; " + decl.form() + ": " + expr.form() + ") " + stmt.form(indent + 1);
		
		case SBreak: "break;";
		
		case SContinue: "continue;";
		
		case SReturn(None): "return;";
		case SReturn(Some(expr)): "return " + expr.form() + ";";
		
		case SGoto(label): 'goto $label;';
		
		case STry(tb, cs, ca):
			final buf = new Buffer();
			
			buf.addString("try ");
			buf.addString(tb.form(indent + 1));
			
			for(c in cs) {
				buf.addString(" catch(");
				buf.addString(c.param.form());
				buf.addString(") ");
				buf.addString(c.body.form(indent + 1));
			}
			
			ca.forEach(ca -> {
				buf.addString(" catch(...) ");
				buf.addString(ca.form(indent + 1));
			});
			
			buf.toString();
	}
}