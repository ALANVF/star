package parsing.ast.decls;

@:using(parsing.ast.decls.Decl.Tools)
enum Decl {
	DMember(m: Member);
	DCase(c: Case);

	DModule(m: Module);
	DClass(c: Class);
	DProtocol(p: Protocol);
	DCategory(c: Category);
	DKind(k: Kind);
	DAlias(a: Alias);

	DMethod(m: Method);
	DInit(i: Init);
	DDefaultInit(i: BaseMethod);
	DOperator(o: Operator);
	DDeinit(d: BaseMethod);

	DUse(u: Use);
}

@:noCompletion
@:publicFields
class Tools {
	static function span(decl: Decl) return switch decl {
		case DMember({span: span}): span;
		case DCase({span: span}): span;
		case DModule({span: span}): span;
		case DClass({span: span}): span;
		case DProtocol({span: span}): span;
		case DCategory({span: span}): span;
		case DKind({span: span}): span;
		case DAlias({span: span}): span;
		case DMethod({span: span}): span;
		case DInit({span: span}): span;
		case DDefaultInit({span: span}): span;
		case DOperator({span: span}): span;
		case DDeinit({span: span}): span;
		case DUse({span: span}): span;
	}

	static function name(decl: Decl) return switch decl {
		case DMember(_): "member";
		case DCase(_): "case";
		case DModule(_): "module";
		case DClass(_): "class";
		case DProtocol(_): "protocol";
		case DCategory(_): "category";
		case DKind(_): "kind";
		case DAlias(_): "alias";
		case DMethod(_): "method";
		case DInit(_): "initializer";
		case DDefaultInit(_): "default initializer";
		case DOperator(_): "operator";
		case DDeinit(_): "deinitializer";
		case DUse(_): "import";
	};
}