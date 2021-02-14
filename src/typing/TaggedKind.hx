package typing;

import reporting.Severity;
import reporting.Diagnostic;

class TaggedKind extends Kind
	implements ITaggedCases
	implements IMembers
	implements IDefaultInit
{
	final taggedCases: Array<TaggedCase> = [];
	final members: Array<Member> = [];
	var defaultInit: Option<DefaultInit> = None;

	static function fromAST(lookup, ast: parsing.ast.decls.Kind) {
		final kind = new TaggedKind({
			lookup: lookup,
			generics: ast.generics.mapArray(Generic.fromAST.bind(lookup, _)),
			span: ast.span,
			name: ast.name,
			params: None
		});

		if(ast.params.isSome()) {
			kind.params = Some(ast.params.value().of.map(param -> kind.makeTypePath(param)));
		}

		if(ast.repr.isSome()) {
			kind.errors.push(new Diagnostic({
				severity: Severity.ERROR,
				message: "Invalid declaration",
				info: [
					Spanned({
						span: ast.repr.value().span(),
						message: "Tagged kinds may not have an underlaying type",
						isPrimary: true
					}),
					Spanned({
						span: kind.span,
						message: 'For kind `${kind.name.name}`',
						isSecondary: true
					})
				]
			}));
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
			case DMember(m): kind.members.push(Member.fromAST(kind, m));

			case DCase(c = {kind: Tagged(_)}): kind.taggedCases.push(TaggedCase.fromAST(kind, c));

			case DModule(m): kind.decls.push(Module.fromAST(kind, m));

			case DClass(c): kind.decls.push(Class.fromAST(kind, c));

			case DProtocol(p): kind.decls.push(Protocol.fromAST(kind, p));
			
			case DKind(k): kind.decls.push(Kind.fromAST(kind, k));

			// ...

			case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(kind, m).forEach(kind.staticMethods.push);
			case DMethod(m): kind.methods.push(Method.fromAST(kind, m));

			case DOperator(o): Operator.fromAST(kind, o).forEach(kind.operators.push);

			case DDefaultInit(i) if(kind.staticInit.isSome()): kind.staticInit = Some(StaticInit.fromAST(kind, i));
			case DDefaultInit(i): kind.defaultInit = Some(DefaultInit.fromAST(kind, i));
			
			case DDeinit(d) if(kind.staticDeinit.isSome()): kind.staticDeinit = Some(StaticDeinit.fromAST(kind, d));
			case DDeinit(d): kind.deinit = Some(Deinit.fromAST(kind, d));
			
			default: kind.errors.push(Errors.unexpectedDecl(kind, ast.name.name, decl));
		}

		return kind;
	}

	override function hasErrors() {
		return super.hasErrors() || taggedCases.some(c -> c.hasErrors()) || members.some(m -> m.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(taggedCase in taggedCases) result = result.concat(taggedCase.allErrors());
		for(member in members) result = result.concat(member.allErrors());
		
		return result;
	}
}