package typing;

import typing.Traits;

class DirectAlias extends Alias {
	var type: Type;

	static function fromAST(lookup: ILookupType, ast: parsing.ast.decls.Alias) {
		final alias = new DirectAlias({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: [],
			type: null // Hack for partial initialization
		});

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(alias, a))) {
			alias.typevars.add(typevar.name.name, typevar);
		}

		switch ast.kind {
			case Direct(_, type): alias.type = alias.makeTypePath(type);
			default: throw "Error!";
		}

		if(ast.params.isSome()) {
			alias.params = ast.params.value().of.map(param -> alias.makeTypePath(param));
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(alias.hidden.isSome()): alias.errors.push(Errors.duplicateAttribute(alias, ast.name.name, "hidden", span));
			case IsHidden(None): alias.hidden = Some(None);
			case IsHidden(Some(outsideOf)): alias.hidden = Some(Some(alias.makeTypePath(outsideOf)));

			case IsFriend(_) if(alias.friends.length != 0): alias.errors.push(Errors.duplicateAttribute(alias, ast.name.name, "friend", span));
			case IsFriend(One(friend)): alias.friends.push(alias.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) alias.friends.push(alias.makeTypePath(friend));
			
			case IsNoinherit: alias.errors.push(Errors.invalidAttribute(alias, ast.name.name, "noinherit", span));
		}

		return alias;
	}
}