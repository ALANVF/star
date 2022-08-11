package typing;

import parsing.ast.Ident;

@:structInit
class SingleTaggedCase extends TaggedCase {
	final name: Ident;
}