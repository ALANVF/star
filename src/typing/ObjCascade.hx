package typing;

import typing.ObjMessage;
import typing.Cascade.Step;

enum ObjCascadeKind {
	Lazy(kind: Cascade.CascadeKind<TExpr>);
	Member(mem: ObjMessage);
	Message(msg: ObjMessage);
	AssignMember(mem: ObjMessage, ?op: TExpr.AssignInfix, expr: TExpr);
	AssignMessage(msg: ObjMessage, ?op: TExpr.AssignInfix, expr: TExpr); // should this have getter/setter versions of the method?
	StepMember(mem: ObjMessage, step: Step);
	StepMessage(msg: ObjMessage, step: Step); // should this have getter/setter versions of the method?
	Block(ctx: Pass2.Ctx, blk: TStmts);
}

@:structInit
@:publicFields
class ObjCascade {
	var ctx: Pass2.Ctx;
	var t: Null<Type> = null;
	final depth: Int;
	final kind: ObjCascadeKind;
	final nested: Array<ObjCascade>;
}