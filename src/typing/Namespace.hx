package typing;

import typing.Traits;

abstract class Namespace extends TypeDecl {
	final parents: Array<Type> = [];
	@ignore final decls = new MultiMap<String, TypeDecl>();
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	var staticInit: Option<StaticInit> = None;
	var staticDeinit: Option<StaticDeinit> = None;
	var sealed: Option<Option<Type>> = None;
	final categories: Array<Category> = [];

	inline function addTypeDecl(decl: TypeDecl) {
		decls.add(decl.name.name, decl);
	}

	override function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
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
				final res: Option<Type> = switch (absolute ? {
					final ts: Option<Array<IFullTypeDecl>> = cast decls.find(typeName);
					final tvs: Option<Array<IFullTypeDecl>> = cast typevars.find(typeName);
					tvs.doOrElse(
						_tvs => ts.doOrElse(
							_ts => Some(_ts.concat(_tvs)),
							tvs
						),
						ts
					);
				} : cast decls.find(typeName)) {
					case None: return if(absolute) lookup.findType(path, true, cache) else None;
					case Some([type]): switch [args, type.params] {
						case [[], _]: Some(type.thisType); // should probably curry parametrics but eh
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
								errors.push(Errors.invalidTypeApply(span, "No matching candidates were found"));
								None;
							case [type]: Some({t: TApplied(type, args), span: span});
							case types: Some({t: TMulti(types), span: span});
						}
				};

				switch [rest, res] {
					// is this really needed?
					case [_, None]: if(absolute && rest == Nil3) lookup.findType(path, true, cache) else lookup._match(
						at(file is File) => file.dir._match(
							at(unit is Unit) => if(unit.primary.contains(file)) {
								unit.findType(rest, false, cache.prepend(file));
							} else {
								throw "???";
							},
							_ => None
						),
						_ => None
					);
					case [Nil3, _]: res;
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
				}
			},
			_ => if(absolute) lookup.findType(path, true, cache) else None
		);
	}

	override function hasErrors() {
		return super.hasErrors()
			|| decls.allValues().some(d -> d.hasErrors())
			|| staticMembers.some(m -> m.hasErrors())
			|| staticMethods.some(m -> m.hasErrors())
			|| categories.some(c -> c.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(decl in decls) result = result.concat(decl.allErrors());
		for(member in staticMembers) result = result.concat(member.allErrors());
		for(method in staticMethods) result = result.concat(method.allErrors());
		for(category in categories) result = result.concat(category.allErrors());

		return result;
	}
}