package parsing.ast;

import parsing.ast.TypeParams;
import text.Span;

@:using(parsing.ast.Type.TypeSegTools)
enum TypeSeg {
	Name(_: Span, name: String);
	NameParams(_: Span, name: String, params: TypeParams);
}

@:publicFields
class TypeSegTools {
	static inline function name(seg: TypeSeg) return switch seg {
		case Name(_, name): name;
		case NameParams(_, name, _): name;
	}
	
	static inline function span(seg: TypeSeg) return switch seg {
		case Name(span, _): span;
		case NameParams(begin, _, {end: end}): Span.range(begin, end);
	}
	
	static inline function simpleName(seg: TypeSeg) return switch seg {
		case Name(_, name): name;
		case NameParams(_, name, params): '$name[' + "..., ".repeat(params.of.length - 1) + "...]";
	}
}


@:using(parsing.ast.Type.TypeTools)
enum Type {
	TSegs(leading: List<Span>, segs: List<TypeSeg>);
	TBlank(_: Span);
	TBlankParams(_: Span, params: TypeParams);
}

@:publicFields
class TypeTools {
	static function span(type: Type) return switch type {
		case TSegs(Nil, segs): Span.range(segs.head().span(), segs.last().span());
		case TSegs(leading, segs): Span.range(leading.head(), segs.last().span());
		case TBlank(span): span;
		case TBlankParams(begin, {end: end}): Span.range(begin, end);
	}

	static function simpleName(type: Type) return switch type {
		case TSegs(leading, segs): "_.".repeat(leading.length()) + segs.map(s -> s.simpleName()).join(".");
		case TBlank(_): "_";
		case TBlankParams(_, params): "_[" + "..., ".repeat(params.of.length - 1) + "...]";
	}
}