package typing;

import text.Span;
import typing.Traits;

typedef BaseMethod = VErrors & {
	final decl: ITypeDecl;
	final span: Span;
	
	var typedBody: Null<Array<TStmt>>;
};

typedef BaseGenericMethod = BaseMethod & {
	final typevars: MultiMap<String, TypeVar>;
};

@:generic
inline function _findType<T: ILookupType & BaseGenericMethod>(method: T, path: LookupPath, depth: Int): Option<Type> {
	return path._match(
		at([[span, typeName, args], ...rest]) => {
			var finished = true;
			final res: Option<Type> = method.typevars.find(typeName).map(found -> found.filter(tvar ->
				tvar.params.length == 0 || tvar.params.length == args.length
			))._match(
				at(None | Some([])) => method.decl.findType(path, Start, null, depth, Nil),
				at(Some(_), when(depth != 0)) => method.decl.findType(path, Start, null, depth - 1, Nil),
				at(Some([tvar])) => switch [args, tvar.params] {
					case [[], _]:
						finished = false;
						Some({t: tvar.thisType.t, span: span}); // should probably curry parametrics but eh
					case [_, []]:
						// error...?
						None;
					case [_, params]:
						if(args.length > params.length) {
							method.errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
							None;
						} else if(args.length < params.length) {
							method.errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
							None;
						} else {
							finished = false;
							Some({t: TApplied(tvar.thisType, args), span: span});
						}
				},
				at(Some(found)) => {
					if(args.length == 0) {
						finished = false;
						Some({t: TMulti(found.map(t -> t.thisType)), span: span});
					} else switch found.filter(t -> t.params.length == args.length).map(t -> t.thisType) {
						case []:
							method.errors.push(Errors.invalidTypeApply(span, "No candidate matches the type arguments"));
							None;
						case [tvar]:
							finished = false;
							Some({t: TApplied(tvar, args), span: span});
						case tvars:
							finished = false;
							Some({t: TMulti(tvars), span: span});
					}
				}
			);

			switch [rest, res] {
				case [_, None]: None;
				case [_, _] if(finished): res;
				case [Nil3, _]: res;
				case [_, Some(type)]: Some({t: TLookup(type, rest, method), span: span});
			}
		},
		_ => throw "bad"
	);
}