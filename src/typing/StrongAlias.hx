package typing;

import typing.Traits;

class StrongAlias extends Alias {
	var type: Type;
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	final members: Array<Member> = [];
	final methods: Array<Method> = [];
	final operators: Array<Operator> = [];
	var noInherit: Bool = false;

	static function fromAST(lookup, ast: parsing.ast.decls.Alias) {
		final alias = new StrongAlias({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: [],
			type: null // Hack for partial initialization
		});

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(alias, a))) {
			alias.typevars.add(typevar.name.name, typevar);
		}

		final body = switch ast.kind {
			case Strong(type, body):
				alias.type = alias.makeTypePath(type); // Fix
				body;
			default: throw "Error!";
		};

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
			
			case IsNoinherit: alias.noInherit = true;
		}

		if(body.isSome()) {
			for(decl in body.value().of) switch decl {
				case DMember(m) if(m.attrs.exists(IsStatic)): alias.staticMembers.push(Member.fromAST(alias, m));
				case DMember(m): alias.members.push(Member.fromAST(alias, m));

				case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(alias, m).forEach(x -> alias.staticMethods.push(x));
				case DMethod(m): alias.methods.push(Method.fromAST(alias, m));
	
				case DOperator(o): Operator.fromAST(alias, o).forEach(x -> alias.operators.push(x));
	
				default: alias.errors.push(Errors.unexpectedDecl(alias, ast.name.name, decl));
			}
		}

		return alias;
	}
	
	override function hasErrors() {
		return super.hasErrors()
			|| staticMembers.some(m -> m.hasErrors())
			|| staticMethods.some(m -> m.hasErrors())
			|| members.some(m -> m.hasErrors())
			|| methods.some(m -> m.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();

		for(member in staticMembers) result = result.concat(member.allErrors());
		for(method in staticMethods) result = result.concat(method.allErrors());
		for(member in members) result = result.concat(member.allErrors());
		for(method in methods) result = result.concat(method.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}
	
	override function declName() {
		return "strong alias";
	}


	override function hasParentDecl(decl: TypeDecl) {
		return super.hasParentDecl(decl)
			|| type.hasParentDecl(decl);
	}


	override function canSeeMethod(method: AnyMethod) {
		return super.canSeeMethod(method)
			|| type.canSeeMethod(method);
	}


	override function findSingleStatic(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleStaticKind> {
		if(cache.contains(thisType)) return null;
		
		for(mem in staticMembers) {
			if(mem.matchesGetter(name) && from.canSeeMember(mem)) {
				return SSMember(mem);
			}
		}

		for(mth in staticMethods) mth._match(
			at(sm is SingleStaticMethod) => {
				if(sm.name.name == name && (!getter || sm.isGetter) && from.canSeeMethod(sm)) {
					return SSMethod(sm);
				}
			},
			_ => {}
		);
		
		if(!noInherit) type.findSingleStatic(name, from, getter, cache.prepend(thisType))._match(
			at(ss!) => return ss,
			_ => {}
		);

		return null;
	}


	override function findMultiStatic(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return [];
		
		final candidates: Array<MultiStaticKind> = [];

		names._match(at([name]) => for(mem in staticMembers) {
			if(mem.matchesSetter(name) && from.canSeeMember(mem)) {
				candidates.push(MSMember(mem));
			}
		}, _ => {});

		if(setter) {
			throw "todo";
		} else {
			for(mth in staticMethods) mth._match(
				at(mm is MultiStaticMethod) => {
					if(mm.params.every2Strict(names, (l, n) -> l.label.name == n) && from.canSeeMethod(mm)) {
						candidates.push(MSMethod(mm));
					}
				},
				_ => {}
			);
		}

		if(!noInherit) {
			candidates.pushAll(type.findMultiStatic(names, from, setter, cache.prepend(thisType)));
		}

		return candidates;
	}
}