package typing;

import parsing.ast.Ident;
import parsing.ast.Expr;
import text.Span;

@:publicFields @:structInit class MultiParam {
	var label: Ident;
	var name: Ident;
	var type: Type;
	var value: Null<Expr>;
	var tvalue: Null<TExpr> = null;

	static function fromUntyped(decl: ITypeLookup, param: parsing.ast.decls.MultiParam) {
		final type = decl.makeTypePath(param.type);
		final res: MultiParam = Util._match([param.label, param.name],
			at([l!, n!]) => {label: l, name: n, type: type, value: param.value},
			at([l!, null]) => {label: l, name: l, type: type, value: param.value},
			at([null, n!]) => {label: new Ident(n.span, "_"), name: n, type: type, value: param.value},
			at([_, _]) => {
				final span = {
					final s = param.type.span();
					Span.at(s.start, s.source);
				};
				final ident = new Ident(span, "_");
				{label: ident, name: ident, type: type, value: param.value};
			}
		);
		return res;
	}
}