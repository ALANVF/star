package typing;

import parsing.ast.Ident;

class MultiTaggedCase extends TaggedCase {
	final params: Array<{label: Ident, name: Ident, type: Type}>;
}