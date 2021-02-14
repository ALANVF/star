package parsing.ast;

import parsing.ast.TypeParams;
import text.Span;

enum TypeSeg {
	Named(_: Span, name: String, args: TypeParams);
	Blank(_: Span, args: TypeParams);
}

@:using(parsing.ast.Type.Tools)
typedef Type = List<TypeSeg>;

@:noCompletion
@:publicFields
class Tools {
	static function span(typePath: Type) return Util.match(typePath,
		at([]) => throw "Error!",
		at([Named(s, _, None) | Blank(s, None)]) => s,
		at([Named(s, _, Some({end: end})) | Blank(s, Some({end: end}))]) => Span.range(s, end),
		at([Named(s, _, _) | Blank(s, _), ...rest]) => switch rest.last() {
			case Named(s2, _, None) | Blank(s2, None): Span.range(s, s2);
			case Named(s, _, Some({end: end})) | Blank(s, Some({end: end})): Span.range(s, end);
		}
	);

	static function simpleName(typePath: Type) {
		return typePath.map(s -> switch s {
			case Named(_, name, None): name;
			case Named(_, name, Some({of: args})): '$name[${args.map(_ -> "...").join(", ")}]';
			case Blank(_, None): "_";
			case Blank(_, Some({of: args})): '_[${args.map(_ -> "...").join(", ")}]';
		}).join(".");
	}
}