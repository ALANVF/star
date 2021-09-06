package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;
import typing.Traits;

@:build(util.Auto.build({keepInit: true}))
@:autoBuild(util.Auto.build())
abstract class TypeDecl implements IErrors {
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType;
	@ignore final typevars = new MultiMap<String, TypeVar>();
	final span: Span;
	final name: Ident;
	var params: Array<Type>;
	var hidden: Option<Option<Type>> = None;
	final friends: Array<Type> = [];
	@ignore var thisType: Type;
	@ignore final refinements = new Array<TypeDecl>();

	function new() {
		thisType = new Type(TConcrete(this));
	}
	
	abstract function declName(): String;

	function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(absolute) {
			if(cache.contains(this)) {
				return None;
			} else {
				cache = cache.prepend(this);
			}
		}

		return path._match(
			at([[span, "This", []]], when(absolute)) => Some({t: TThis(this), span: span}),
			at([[span, "This", _]], when(absolute)) => {
				// prob shouldn't be attatched to *this* type decl, but eh
				errors.push(Errors.notYetImplemented(span));
				None;
			},
			at([[span, typeName, args], ...rest]) => {
				final res: Option<Type> = switch typevars.find(typeName) {
					case None: return if(absolute) lookup.findType(path, true, cache) else None;
					case Some([type]): switch [args, type.params] {
						case [[], _]: Some({t: type.thisType.t, span: span}); // should probably curry parametrics but eh
						case [_, []]:
							// should this check for type aliases?
							errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
							None;
						case [_, params]:
							if(args.length > params.length) {
								errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
								None;
							} else if(args.length < params.length) {
								errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
								None;
							} else {
								Some({t: TApplied(type.thisType, args), span: span});
							}
					}
					case Some(found):
						if(args.length == 0) {
							Some({t: TMulti(found.map(t -> t.thisType)), span: span});
						} else switch found.filter(t -> t.params.length == args.length).map(g -> g.thisType) {
							case []:
								errors.push(Errors.invalidTypeApply(span, "No candidate matches the type arguments"));
								None;
							case [type]: Some({t: TApplied(type, args), span: span});
							case types: Some({t: TMulti(types), span: span});
						}
				};

				switch [rest, res] {
					case [_, None]: if(absolute) lookup.findType(path, true, cache) else None;
					case [Nil3, _]: res;
					case [_, Some({t: TConcrete(decl)})]:
						decl.findType(rest, false, Nil);
					/*case [_, Some({t: TModular(t, u)})]:
						switch t.findType(rest, false, Nil) {
							case r = Some(_): r;
							case None: u.findType(rest, false, List.of(t));
						}*/
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
				}
			},
			_ => if(absolute) lookup.findType(path, true, cache) else None
		);
	}

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}

	function hasErrors() {
		return errors.length != 0
			|| typevars.allValues().some(g -> g.hasErrors());
	}

	function allErrors() {
		var result = errors;

		for(typevar in typevars) result = result.concat(typevar.allErrors());

		return result;
	}
}