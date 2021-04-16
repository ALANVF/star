package compiler;

enum abstract Attr(String) to String {
	final AFriend = "friend";
	final AInline = "inline";
	final AVirtual = "virtual";
	final AConstexpr = "constexpr";
	final AConsteval = "consteval";
	final AStatic = "static";
	final AImplicit = "implicit";
	final AExplicit = "explicit";
	final AMutable = "mutable";
	final AExtern = "extern";
}