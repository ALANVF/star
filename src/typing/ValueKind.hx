package typing;

class ValueKind extends Kind implements IValueCases {
	var repr: Option<Type> = None;
	final valueCases: Array<ValueCase> = [];

	static function fromAST(lookup, ast: parsing.ast.decls.Kind) {
		if(ast.generics != Nil) throw "NYI!";

		final kind = new ValueKind({
			lookup: lookup,
			generics: [],
			span: ast.span,
			name: ast.name,
			params: None
		});

		if(ast.params.isSome()) {
			kind.params = Some(ast.params.value().of.map(param -> kind.makeTypePath(param)));
		}

		if(ast.repr.isSome()) {
			kind.repr = Some(lookup.makeTypePath(ast.repr.value()));
		}

		if(ast.parents.isSome()) {
			for(parent in ast.parents.value().parents) {
				kind.parents.push(lookup.makeTypePath(parent));
			}
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(kind.hidden.isSome()): kind.errors.push(Errors.duplicateAttribute(kind, ast.name.name, "hidden", span));
			case IsHidden(None): kind.hidden = Some(None);
			case IsHidden(Some(outsideOf)): kind.hidden = Some(Some(lookup.makeTypePath(outsideOf)));

			case IsFriend(_) if(kind.friends.length != 0): kind.errors.push(Errors.duplicateAttribute(kind, ast.name.name, "friend", span));
			case IsFriend(One(friend)): kind.friends.push(lookup.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) kind.friends.push(lookup.makeTypePath(friend));

			case IsFlags: kind.isFlags = true;

			case IsStrong: kind.isStrong = true;

			case IsUncounted: kind.isUncounted = true;
		}

		for(decl in ast.body.of) switch decl {
			case DMember(m) if(m.attrs.exists(IsStatic)): kind.staticMembers.push(Member.fromAST(kind, m));

			case DCase(c = {kind: Scalar(_, _)}): kind.valueCases.push(ValueCase.fromAST(kind, c));

			case DModule(m): kind.decls.push(Module.fromAST(kind, m));

			case DClass(c): kind.decls.push(Class.fromAST(kind, c));

			case DProtocol(p): kind.decls.push(Protocol.fromAST(kind, p));

			case DKind(k): kind.decls.push(Kind.fromAST(kind, k));
			
			// ...

			case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(kind, m).forEach(kind.staticMethods.push);
			case DMethod(m): kind.methods.push(Method.fromAST(kind, m));

			case DOperator(o): Operator.fromAST(kind, o).forEach(kind.operators.push);

			case DDefaultInit(_) if(kind.staticInit.isSome()): kind.errors.push(Errors.duplicateDecl(kind, ast.name.name, decl));
			case DDefaultInit(i): kind.staticInit = Some(StaticInit.fromAST(kind, i));
			
			case DDeinit(d) if(kind.staticDeinit.isSome()): kind.staticDeinit = Some(StaticDeinit.fromAST(kind, d));
			case DDeinit(d): kind.deinit = Some(Deinit.fromAST(kind, d));
			
			default: kind.errors.push(Errors.unexpectedDecl(kind, ast.name.name, decl));
		}

		return kind;
	}

	override function hasErrors() {
		return super.hasErrors() || valueCases.some(c -> c.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(valueCase in valueCases) result = result.concat(valueCase.allErrors());
		
		return result;
	}
}