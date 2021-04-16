package compiler;

@:using(compiler.DeclStmt.DeclStmtTools)
enum DeclStmt {
	DTypeDecl(decl: TypeDecl);
	DAnonEnum(cases: EnumCases);
	DAnonUnion(body: DeclBody);
	DNamespace(ns: Namespace);
	DUsing(kind: Option<String>, type: Type);
	DUsingExpr(expr: Expr);
	DIncludeLib(path: String);
	DIncludeFile(path: String);
	DMember(m: Member);
	DMemberList(m: MemberList);
	DMethod(m: AnyMethod);
}


@:publicFields
class DeclStmtTools {
	static function form(stmt: DeclStmt, indent = 0) return switch stmt {
		case DTypeDecl(decl): decl.form(indent);
		case DAnonEnum(cases): "enum " + cases.form(indent) + ";";
		case DAnonUnion(body): "union " + body.form(indent) + ";";
		case DNamespace(ns): ns.form(indent);
		case DUsing(None, type): "using " + type.form() + ";";
		case DUsing(Some(kind), type): 'using $kind ' + type.form() + ";";
		case DUsingExpr(expr): "using " + expr.form(indent) + ";";
		case DIncludeLib(path): "#include <" + ExprTools.escapeString(path) + ">";
		case DIncludeFile(path): "#include \"" + ExprTools.escapeString(path) + "\"";
		case DMember(m): m.form(indent);
		case DMemberList(m): m.form(indent);
		case DMethod(m): m.form(indent);
	}
}