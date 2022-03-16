package typing;

import typing.SingleStaticKind;
import typing.TypeMessage;
import typing.Cascade.Step;

enum TypeCascadeKind {
	Lazy(kind: Cascade.CascadeKind<Type>);
	Member(mem: TypeMessage);
	Message(msg: TypeMessage);
	AssignMember(
		setMem: TypeMessage,
		?op: {
			getMem: TypeMessage,
			op: TExpr.AssignInfix,
			kinds: Array<BinaryOpKind>
		},
		expr: TExpr
	);
	AssignMessage(
		setMsg: TypeMessage,
		?op: {
			getMsg: TypeMessage,
			op: TExpr.AssignInfix,
			kinds: Array<BinaryOpKind> 
		},
		expr: TExpr
	);
	StepMember(setMem: MultiStaticKind, getMem: SingleStaticKind, step: UnaryOpKind);
	StepMessage(setMsg: MultiStaticKind, getMsg: TypeMessage, step: UnaryOpKind);
	Block(ctx: Ctx, blk: TStmts);
}

@:structInit
@:publicFields
class TypeCascade {
	var ctx: Ctx;
	var t: Null<Type> = null;
	final depth: Int;
	final kind: TypeCascadeKind;
	final nested: Array<ObjCascade>;
}