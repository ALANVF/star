package compiler;

@:using(compiler.InitCtor.InitCtorTools)
enum InitCtor {
	CCall(args: Array<Expr>);
	CInitList(l: InitList);
}


@:publicFields
class InitCtorTools {
	static function form(ctor: InitCtor) {
		return switch ctor {
			case CCall(args): "(" + args.map(arg -> arg.form()).join(", ") + ")";
			case CInitList(l): l.form();
		}
	}
}