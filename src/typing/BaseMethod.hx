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
inline function _findType<T: ILookupType & BaseGenericMethod>(method: T, path: LookupPath, from: Null<ITypeDecl>, depth: Int): Option<Type> {
	return path._match(
		at([[span, typeName, args], ...rest]) => {
			var finished = true;
			final res: Option<Type> = method.typevars.find(typeName).map(found -> found.filter(tvar ->
				tvar.params.length == 0 || tvar.params.length == args.length
			))._match(
				at(None | Some([])) => method.decl.findType(path, Start, from, depth, Nil),
				at(Some(_), when(depth != 0)) => method.decl.findType(path, Start, from, depth - 1, Nil),
				at(Some([tvar])) => switch [args, tvar.params] {
					case [[], []]:
						finished = false;
						Some({t: tvar.thisType.t, span: span});
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
							Some({t: TApplied(tvar.thisType, args.map(arg -> arg.t._match(
								at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
									at(Some(type)) => type,
									at(None) => {
										method.errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
										arg;
									}
								),
								_ => arg
							))), span: span});
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
							Some({t: TApplied(tvar, args.map(arg -> arg.t._match(
								at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
									at(Some(type)) => type,
									at(None) => {
										method.errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
										arg;
									}
								),
								_ => arg
							))), span: span});
						case tvars:
							finished = false;
							Some({t: TMulti(tvars), span: span});
					}
				}
			);

			switch [rest, res] {
				case [_, None]: method.decl.findType(path, Outside, from, depth);
				case [_, _] if(finished): res;
				case [Nil3, _]: res;
				case [_, Some({t: TConcrete(decl)})]: decl.findType(rest, Inside, from, 0);
				case [_, Some(type)]: Some({t: TLookup(type, rest, method), span: span});
			}
		},
		_ => throw "bad"
	);
}