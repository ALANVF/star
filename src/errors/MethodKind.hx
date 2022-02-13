package errors;

import typing.Type;
import typing.TExpr;
import typing.BinaryOp;
import typing.UnaryOp;

enum MethodKind {
	Single(access: Access, name: String);
	Multi(access: Access, names: Array<String>, ?args: Array<TExpr>);
	Unary(op: UnaryOp);
	Binary(op: BinaryOp, ?rtype: Type);
}