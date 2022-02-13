package typing;

import text.Span;
import typing.Traits;

typedef BaseMethod = VErrors & {
	final decl: AnyTypeDecl;
	final span: Span;
	
	var typedBody: Null<Array<TStmt>>;
};

typedef BaseGenericMethod = BaseMethod & {
	final typevars: MultiMap<String, TypeVar>;
};

@:generic
inline function _findType<T: ITypeLookup & BaseGenericMethod>(method: T, path: LookupPath, from: Null<AnyTypeDecl>, depth: Int): Null<Type> {
	return path._match(
		at([[span, typeName, args], ...rest]) => {
			var finished = true;
			final res: Null<Type> = method.typevars.find(typeName).map(found -> found.filter(tvar ->
				tvar.params.length == 0 || tvar.params.length == args.length
			))._match(
				at(None | Some([])) => method.decl.findType(path, Start, from, depth, Nil),
				at(Some(_), when(depth != 0)) => method.decl.findType(path, Start, from, depth - 1, Nil),
				at(Some([tvar])) => switch [args, tvar.params] {
					case [[], []]:
						finished = false;
						{t: tvar.thisType.t, span: span};
					case [[], _]:
						finished = false;
						{t: tvar.thisType.t, span: span}; // should probably curry parametrics but eh
					case [_, []]:
						// error...?
						null;
					case [_, params]:
						if(args.length > params.length) {
							method.errors.push(Type_InvalidTypeApply(span, "Too many arguments"));
							null;
						} else if(args.length < params.length) {
							method.errors.push(Type_InvalidTypeApply(span, "Not enough arguments"));
							null;
						} else {
							finished = false;
							{t: TApplied(tvar.thisType, args.map(arg -> arg.t._match(
								at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
									at(type!) => type,
									_ => {
										method.errors.push(Type_InvalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
										arg;
									}
								),
								_ => arg
							))), span: span};
						}
				},
				at(Some(found)) => {
					if(args.length == 0) {
						finished = false;
						{t: TMulti(found.map(t -> t.thisType)), span: span};
					} else switch found.filter(t -> t.params.length == args.length).map(t -> t.thisType) {
						case []:
							method.errors.push(Type_InvalidTypeApply(span, "No candidate matches the type arguments"));
							null;
						case [tvar]:
							finished = false;
							{t: TApplied(tvar, args.map(arg -> arg.t._match(
								at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
									at(type!) => type,
									_ => {
										method.errors.push(Type_InvalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
										arg;
									}
								),
								_ => arg
							))), span: span};
						case tvars:
							finished = false;
							{t: TMulti(tvars), span: span};
					}
				}
			);

			Util._match([rest, res],
				at([_, null]) => method.decl.findType(path, Outside, from, depth),
				at([_, _], when(finished)) => res,
				at([Nil3, _]) => res,
				at([_, {t: TConcrete(decl)}]) => decl.findType(rest, Inside, from, 0),
				at([_, type!!]) => {t: TLookup(type, rest, method), span: span}
			);
		},
		_ => throw "bad"
	);
}