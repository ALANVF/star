package typing;

typedef Step = parsing.ast.Cascade.Step;

enum CascadeKind<T> {
	Member(mem: String);
	Message(msg: Message<T>);
	AssignMember(mem: String, ?op: TExpr.AssignInfix, expr: TExpr);
	AssignMessage(msg: Message<T>, ?op: TExpr.AssignInfix, expr: TExpr);
	StepMember(mem: String, step: Step);
	StepMessage(msg: Message<T>, step: Step);
	Block(blk: TStmts);
}

@:structInit
@:publicFields
class Cascade<T> {
	final depth: Int;
	final kind: CascadeKind<T>;
	final nested: Array<Cascade<TExpr>>;
}