package typing;

import typing.SingleInstKind;
import typing.ObjMessage;
import typing.Cascade.Step;

enum ObjCascadeKind {
	Lazy(kind: Cascade.CascadeKind<TExpr>);
	Member(mem: ObjMessage);
	Message(msg: ObjMessage);
	AssignMember(
		setMem: ObjMessage,
		?op: {
			getMem: ObjMessage,
			op: TExpr.AssignInfix,
			kinds: Array<BinaryOpKind>
		},
		expr: TExpr
	);
	AssignMessage(
		setMsg: ObjMessage,
		?op: {
			getMsg: ObjMessage,
			op: TExpr.AssignInfix,
			kinds: Array<BinaryOpKind> 
		},
		expr: TExpr
	);
	StepMember(setMem: MultiInstKind, getMem: SingleInstKind, step: UnaryOpKind);
	StepMessage(msg: ObjMessage, step: Step); // should this have getter/setter versions of the method?
	Block(ctx: Ctx, blk: TStmts);
}

@:structInit
@:publicFields
class ObjCascade {
	var ctx: Ctx;
	var t: Null<Type> = null;
	final depth: Int;
	final kind: ObjCascadeKind;
	final nested: Array<ObjCascade>;
}