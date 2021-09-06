package typing;

import text.Span;
import typing.Traits;

typedef TypeSeg = parsing.ast.Type.TypeSeg;

@:using(typing.TypePath.TypePathTools)
typedef TypePath = parsing.ast.Type;

function rec(segs: List<TypeSeg>, lookup: ILookupType) return segs._match(
	at([]) => Nil3,
	at([Name(span, name), ...rest]) => Cons3(span, name, [], rec(rest, lookup)),
	at([NameParams(span, name, params), ...rest]) => Cons3(
		Span.range(span, params.end),
		name,
		params.of.map(p -> lookup.makeTypePath(p)),
		rec(rest, lookup)
	)
);

@:publicFields
class TypePathTools {
	static function toLookupPath(self: TypePath, lookup: ILookupType): LookupPath return switch self {
		case TSegs(_, Nil) | TBlank(_) | TBlankParams(_, _): throw "error!";
		case TSegs(_, segs): rec(segs, lookup);
	}

	static function toType(self: TypePath, lookup: ILookupType): Type {
		return switch self {
			case TBlank(span): {t: TBlank, span: span};
			case TBlankParams(span, {of: params}):
				{
					t: TApplied({t: TBlank, span: span}, params.map(p -> toType(p, lookup))),
					span: self.span()
				};
			case TSegs(_, Nil): throw "error!";
			case TSegs(leading, segs):
				{
					t: TPath(leading.length(), rec(segs, lookup), lookup),
					span: self.span()
				};
		};
	}

	static function leading(self: TypePath) return switch self {
		case TSegs(_, Nil) | TBlank(_) | TBlankParams(_, _): throw "error!";
		case TSegs(leading, _): leading;
	}

	static inline function leadingCount(self: TypePath) {
		return leading(self).length();
	}

	static inline function simpleName(self: TypePath) {
		return (self : parsing.ast.Type).simpleName();
	}

	static inline function span(self: TypePath) {
		return (self : parsing.ast.Type).span();
	}
}