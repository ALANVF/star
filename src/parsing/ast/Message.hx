package parsing.ast;

import text.Span;

@:using(parsing.ast.Message)
enum Label {
	Named(_: Span, name: String, expr: Expr);
	Punned(_: Span, name: String);
	Anon(expr: Expr);
}

function span(self: Label) return switch self {
	case Named(s, _, _) | Punned(s, _): s;
	case Anon(_): throw "Cannot get the span of an anon label!"; 
}


enum Message<T> {
	Single(category: Option<Type>, _: Span, name: String);
	Multi(category: Option<Type>, labels: Array<Label>);
	Cast(category: Option<Type>, type: Type): Message<Expr>;
}