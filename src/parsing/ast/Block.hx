package parsing.ast;

import text.Span;

@:structInit
@:publicFields
class Block {
	final begin: Span;
	final stmts: Array<Stmt>;
	final end: Span;
}