package typing;

import parsing.ast.Ident;
import reporting.Diagnostic;

class Module extends Namespace implements IParents {
	final parents: Array<Type> = [];
	var isMain: Bool = false;
	var native: Option<Ident> = None;

	static function fromAST(lookup, ast: parsing.ast.decls.Module) {
		if(ast.generics != Nil) throw "NYI!";

		final module = new Module({
			lookup: lookup,
			generics: [],
			span: ast.span,
			name: ast.name,
			params: None
		});

		if(ast.params.isSome()) {
			module.params = Some(ast.params.value().of.map(param -> module.makeTypePath(param)));
		}

		if(ast.parents.isSome()) {
			for(parent in ast.parents.value().parents) {
				module.parents.push(lookup.makeTypePath(parent));
			}
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(module.hidden.isSome()): module.errors.push(Errors.duplicateAttribute(module, ast.name.name, "hidden", span));
			case IsHidden(None): module.hidden = Some(None);
			case IsHidden(Some(outsideOf)): module.hidden = Some(Some(lookup.makeTypePath(outsideOf)));

			case IsMain: module.isMain = true;

			// Logical error: `is friend #[] is friend #[] ...` is technically valid.
			// Solution: nothing because I'm lazy.
			case IsFriend(_) if(module.friends.length != 0): module.errors.push(Errors.duplicateAttribute(module, ast.name.name, "friend", span));
			case IsFriend(One(friend)): module.friends.push(lookup.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) module.friends.push(lookup.makeTypePath(friend));

			case IsNative(_, _) if(module.native.isSome()): module.errors.push(Errors.duplicateAttribute(module, ast.name.name, "native", span));
			case IsNative(span2, libName): module.native = Some({span: span2, name: libName});
		}

		for(decl in ast.body.of) switch decl {
			case DMember(m): module.staticMembers.push(Member.fromAST(module, m));

			case DModule(m): module.decls.push(Module.fromAST(module, m));

			case DClass(c): module.decls.push(Class.fromAST(module, c));

			case DProtocol(p): module.decls.push(Protocol.fromAST(module, p));

			case DKind(k): module.decls.push(Kind.fromAST(module, k));
			
			// ...

			case DMethod(m): StaticMethod.fromAST(module, m).forEach(module.staticMethods.push);

			case DDefaultInit(_) if(module.staticInit.isSome()): module.errors.push(Errors.duplicateDecl(module, ast.name.name, decl));
			case DDefaultInit(i): module.staticInit = Some(StaticInit.fromAST(module, i));
			
			case DDeinit(_) if(module.staticDeinit.isSome()): module.errors.push(Errors.duplicateDecl(module, ast.name.name, decl));
			case DDeinit(d): module.staticDeinit = Some(StaticDeinit.fromAST(module, d));
			
			default: module.errors.push(Errors.unexpectedDecl(module, ast.name.name, decl));
		}

		return module;
	}

	inline function declName() {
		return "module";
	}
}