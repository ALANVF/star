package typing;

class DirectAlias extends Alias {
	var type: Type;

	static function fromAST(lookup, ast: parsing.ast.decls.Alias) {
		final alias = new DirectAlias({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: [],
			type: null // Hack for partial initialization
		});

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(lookup, a))) {
			alias.typevars.add(typevar.name.name, typevar);
		}

		switch ast.kind {
			case Direct(_, type): alias.type = lookup.makeTypePath(type); // Fix
			default: throw "Error!";
		}

		if(ast.params.isSome()) {
			alias.params = ast.params.value().of.map(param -> alias.makeTypePath(param));
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(alias.hidden.isSome()): alias.errors.push(Errors.duplicateAttribute(alias, ast.name.name, "hidden", span));
			case IsHidden(None): alias.hidden = Some(None);
			case IsHidden(Some(outsideOf)): alias.hidden = Some(Some(lookup.makeTypePath(outsideOf)));

			case IsFriend(_) if(alias.friends.length != 0): alias.errors.push(Errors.duplicateAttribute(alias, ast.name.name, "friend", span));
			case IsFriend(One(friend)): alias.friends.push(lookup.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) alias.friends.push(lookup.makeTypePath(friend));
			
			case IsNoinherit: alias.errors.push(Errors.invalidAttribute(alias, ast.name.name, "noinherit", span));
		}

		return alias;
	}

	override function makeTypePath(path) {
		return type.makeTypePath(path);
	}
}