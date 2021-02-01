package parsing.ast;

import text.Span;

enum Label {
	Named(_: Span, name: String, expr: Expr);
	Punned(_: Span, name: String);
	Anon(expr: Expr);
}

enum Message<T> {
	Single(category: Option<Type>, _: Span, name: String);
	Multi(category: Option<Type>, labels: Array<Label>);
	Cast(category: Option<Type>, type: Type): Message<Expr>;
}