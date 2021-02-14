package typing;

class Protocol extends Namespace
	implements IParents
	implements IMembers
	implements IMethods
	implements IInits
	implements IOperators
	implements IDefaultInit
	implements IDeinit
{
	final parents: Array<Type> = [];
	final members: Array<Member> = [];
	final methods: Array<Method> = [];
	final inits: Array<Init> = [];
	final operators: Array<Operator> = [];
	var defaultInit: Option<DefaultInit> = None;
	var deinit: Option<Deinit> = None;

	static function fromAST(lookup, ast: parsing.ast.decls.Protocol) {
		if(ast.generics != Nil) throw "NYI!";

		final protocol = new Protocol({
			lookup: lookup,
			generics: [],
			span: ast.span,
			name: ast.name,
			params: None
		});

		if(ast.params.isSome()) {
			protocol.params = Some(ast.params.value().of.map(param -> protocol.makeTypePath(param)));
		}

		if(ast.parents.isSome()) {
			for(parent in ast.parents.value().parents) {
				protocol.parents.push(lookup.makeTypePath(parent));
			}
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(protocol.hidden.isSome()): protocol.errors.push(Errors.duplicateAttribute(protocol, ast.name.name, "hidden", span));
			case IsHidden(None): protocol.hidden = Some(None);
			case IsHidden(Some(outsideOf)): protocol.hidden = Some(Some(lookup.makeTypePath(outsideOf)));

			case IsFriend(_) if(protocol.friends.length != 0): protocol.errors.push(Errors.duplicateAttribute(protocol, ast.name.name, "friend", span));
			case IsFriend(One(friend)): protocol.friends.push(lookup.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) protocol.friends.push(lookup.makeTypePath(friend));
		}

		for(decl in ast.body.of) switch decl {
			case DMember(m) if(m.attrs.exists(IsStatic)): protocol.staticMembers.push(Member.fromAST(protocol, m));
			case DMember(m): protocol.members.push(Member.fromAST(protocol, m));

			case DModule(m): protocol.decls.push(Module.fromAST(protocol, m));

			case DClass(c): protocol.decls.push(Class.fromAST(protocol, c));

			case DProtocol(p): protocol.decls.push(Protocol.fromAST(protocol, p));
			
			case DKind(k): protocol.decls.push(Kind.fromAST(protocol, k));

			// ...

			case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(protocol, m).forEach(protocol.staticMethods.push);
			case DMethod(m): Method.fromAST(protocol, m).forEach(protocol.methods.push);

			case DInit(i): Init.fromAST(protocol, i).forEach(protocol.inits.push);

			case DOperator(o): Operator.fromAST(protocol, o).forEach(protocol.operators.push);

			case DDefaultInit(i) if(protocol.staticInit.isSome()): protocol.staticInit = Some(StaticInit.fromAST(protocol, i));
			case DDefaultInit(i): protocol.defaultInit = Some(DefaultInit.fromAST(protocol, i));
			
			case DDeinit(d) if(protocol.staticDeinit.isSome()): protocol.staticDeinit = Some(StaticDeinit.fromAST(protocol, d));
			case DDeinit(d): protocol.deinit = Some(Deinit.fromAST(protocol, d));
			
			default: protocol.errors.push(Errors.unexpectedDecl(protocol, ast.name.name, decl));
		}

		return protocol;
	}

	override function hasErrors() {
		return super.hasErrors() || members.some(m -> m.hasErrors()) || methods.some(m -> m.hasErrors())
			|| inits.some(i -> i.hasErrors()) || operators.some(o -> o.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(member in members) result = result.concat(member.allErrors());
		for(method in methods) result = result.concat(method.allErrors());
		for(init in inits) result = result.concat(init.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}

	inline function declName() {
		return "class";
	}
}