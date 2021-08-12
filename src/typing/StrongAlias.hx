package typing;

class StrongAlias extends Alias {
	var type: Type;
	final staticMethods: Array<StaticMethod> = [];
	final methods: Array<Method> = [];
	final operators: Array<Operator> = [];
	var noInherit: Bool = false;

	static function fromAST(lookup, ast: parsing.ast.decls.Alias) {
		final alias = new StrongAlias({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: None,
			type: null // Hack for partial initialization
		});

		for(generic in ast.generics.mapArray(Generic.fromAST.bind(lookup, _))) {
			alias.generics.add(generic.name.name, generic);
		}

		final body = switch ast.kind {
			case Strong(type, body):
				alias.type = lookup.makeTypePath(type); // Fix
				body;
			default: throw "Error!";
		};

		if(ast.params.isSome()) {
			alias.params = Some(ast.params.value().of.map(param -> alias.makeTypePath(param)));
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(alias.hidden.isSome()): alias.errors.push(Errors.duplicateAttribute(alias, ast.name.name, "hidden", span));
			case IsHidden(None): alias.hidden = Some(None);
			case IsHidden(Some(outsideOf)): alias.hidden = Some(Some(lookup.makeTypePath(outsideOf)));

			case IsFriend(_) if(alias.friends.length != 0): alias.errors.push(Errors.duplicateAttribute(alias, ast.name.name, "friend", span));
			case IsFriend(One(friend)): alias.friends.push(lookup.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) alias.friends.push(lookup.makeTypePath(friend));
			
			case IsNoinherit: alias.noInherit = true;
		}

		if(body.isSome()) {
			for(decl in body.value().of) switch decl {
				case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(alias, m).forEach(x -> alias.staticMethods.push(x));
				case DMethod(m): alias.methods.push(Method.fromAST(alias, m));
	
				case DOperator(o): Operator.fromAST(alias, o).forEach(x -> alias.operators.push(x));
	
				default: alias.errors.push(Errors.unexpectedDecl(alias, ast.name.name, decl));
			}
		}

		return alias;
	}

	override function makeTypePath(path) {
		return type.makeTypePath(path);
	}

	override function hasErrors() {
		return super.hasErrors()
			|| staticMethods.some(m -> m.hasErrors())
			|| methods.some(m -> m.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();

		for(method in methods) result = result.concat(method.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}
	
	override function declName() {
		return "strong alias";
	}
}