package compiler;

@:using(compiler.TypePathTools)
typedef TypePath = Array<{name: String, args: Option<Array<Type>>}>;