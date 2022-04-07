package typing;

import parsing.ast.Ident;
import errors.Error;

class Module extends Namespace {
	var isMain: Bool = false;
	var native: Option<Ident> = None;

	static function fromAST(lookup, ast: parsing.ast.decls.Module) {
		final module = new Module({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: []
		});

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(module, a))) {
			module.typevars.add(typevar.name.name, typevar);
		}

		ast.params._and(params => {
			module.params = params.of.map(param -> module.makeTypePath(param));
		});

		ast.parents._and(parents => {
			for(parent in parents.parents) {
				module.parents.push(module.makeTypePath(parent));
			}
		});

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(module.hidden != null): module.errors.push(Type_DuplicateAttribute(module, ast.name.name, "hidden", span));
			case IsHidden(None): module.hidden = None;
			case IsHidden(Some(outsideOf)): module.hidden = Some(module.makeTypePath(outsideOf));

			case IsSealed(_) if(module.sealed.isSome()): module.errors.push(Type_DuplicateAttribute(module, ast.name.name, "sealed", span));
			case IsSealed(None): module.sealed = Some(None);
			case IsSealed(Some(outsideOf)): module.sealed = Some(Some(module.makeTypePath(outsideOf)));

			case IsMain: module.isMain = true;

			// Logical error: `is friend #[] is friend #[] ...` is technically valid.
			// Solution: nothing because I'm lazy.
			case IsFriend(_) if(module.friends.length != 0): module.errors.push(Type_DuplicateAttribute(module, ast.name.name, "friend", span));
			case IsFriend(One(friend)): module.friends.push(module.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) module.friends.push(module.makeTypePath(friend));

			case IsNative(_, _) if(module.native.isSome()): module.errors.push(Type_DuplicateAttribute(module, ast.name.name, "native", span));
			case IsNative(span2, libName): module.native = Some({span: span2, name: libName});
		}

		for(decl in ast.body.of) switch decl {
			case DMember(m): module.staticMembers.push({
								final mem = Member.fromAST(module, m);
								mem.isStatic = true;
								mem;
							});

			case DModule(m): module.addTypeDecl(Module.fromAST(module, m));

			case DClass(c): module.addTypeDecl(Class.fromAST(module, c));

			case DProtocol(p): module.addTypeDecl(Protocol.fromAST(module, p));

			case DKind(k): module.addTypeDecl(Kind.fromAST(module, k));
			
			case DAlias(a): module.addTypeDecl(Alias.fromAST(module, a));

			case DCategory(c): module.categories.push(Category.fromAST(module, c));

			case DMethod(m): StaticMethod.fromAST(module, m).forEach(x -> module.staticMethods.push(x));

			case DDefaultInit(_) if(module.staticInit.isSome()): module.errors.push(Type_DuplicateDecl(module, decl));
			case DDefaultInit(i): module.staticInit = Some(StaticInit.fromAST(module, i));
			
			case DDeinit(_) if(module.staticDeinit.isSome()): module.errors.push(Type_DuplicateDecl(module, decl));
			case DDeinit(d): module.staticDeinit = Some(StaticDeinit.fromAST(module, d));
			
			default: module.errors.push(Type_UnexpectedDecl(module, decl));
		}

		return module;
	}

	function declName() {
		return "module";
	}
}