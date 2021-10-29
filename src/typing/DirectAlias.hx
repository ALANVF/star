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


	override function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		if(search == Inside) {
			return type.findType(path, Inside, from, depth, cache.prepend(thisType));
		} else {
			return super.findType(path, search, from, depth, cache);
		}
	}


	/*override function unifyWithType(type2: Type) {
		return type.unifyWithType(type);
	}*/

	override function strictUnifyWithType(type2: Type) {
		return type.strictUnifyWithType(type);
	}


	override function isNative(kind: NativeKind) {
		return type.isNative(kind);
	}


	override function hasParentDecl(decl: TypeDecl) {
		return super.hasParentDecl(decl)
			|| type.hasParentDecl(decl);
	}
	
	override function hasChildDecl(decl: TypeDecl) {
		return super.hasChildDecl(decl)
			|| type.hasChildDecl(decl);
	}


	override function hasParentType(type2: Type) {
		return super.hasParentType(type2)
			|| type.hasParentType(type2);
	}
	
	override function hasChildType(type2: Type) {
		return super.hasChildType(type2)
			|| type.hasChildType(type2);
	}


	override function canSeeMember(member: Member) {
		return type.canSeeMember(member);
	}

	override function canSeeMethod(method: AnyMethod) {
		return type.canSeeMethod(method);
	}


	override function instMembers(from: ITypeDecl) {
		return type.instMembers(from);
	}


	override function findSingleStatic(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return null;
		
		return type.findSingleStatic(name, from, getter, cache.prepend(thisType));
	}


	override function findMultiStatic(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return [];
		
		return type.findMultiStatic(names, from, setter, cache.prepend(thisType));
	}


	override function findSingleInst(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return null;
		
		return type.findSingleInst(name, from, getter, cache.prepend(thisType));
	}


	override function findMultiInst(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return [];
		
		return type.findMultiInst(names, from, setter, cache.prepend(thisType));
	}


	override function findCast(target: Type, from: ITypeDecl, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return [];
		
		return type.findCast(target, from, cache.prepend(thisType));
	}


	override function findUnaryOp(op: UnaryOp, from: ITypeDecl, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return null;

		return type.findUnaryOp(op, from, cache.prepend(thisType));
	}


	override function findBinaryOp(op: BinaryOp, from: ITypeDecl, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return [];

		return type.findBinaryOp(op, from, cache.prepend(thisType));
	}
}