package parsing.ast;

import text.Span;

enum Step {
	Incr;
	Decr;
}

enum CascadeKind<T> {
	Member(mem: Ident);
	Message(msg: Message<T>);
	AssignMember(mem: Ident, _: Span, op: Null<Infix.Assignable>, expr: Expr);
	AssignMessage(msg: Message<T>, _: Span, op: Null<Infix.Assignable>, expr: Expr);
	StepMember(mem: Ident, _: Span, step: Step);
	StepMessage(msg: Message<T>, _: Span, step: Step);
	Block(blk: Block);
}

@:structInit
@:publicFields
class Cascade<T> {
	final span: Span;
	final depth: Int;
	var kind: CascadeKind<T>;
	final nested: Array<Cascade<Expr>>;
}