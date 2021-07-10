package typing;

import text.Span;
import parsing.ast.Ident;
import reporting.Diagnostic;
import typing.Traits;

@:build(util.Auto.build())
class Category {
	final errors: Array<Diagnostic> = [];
	@:ignore final generics = new MultiMap<String, Generic>();
	final lookup: ILookupType;
	final span: Span;
	final name: Ident;
	var params: Option<Array<Type>>;
	var type: Option<Type>;
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	final methods: Array<Method> = [];
	final inits: Array<Init> = [];
	final operators: Array<Operator> = [];
	var hidden: Option<Option<Type>> = None;
	final friends: Array<Type> = [];

	static function fromAST(lookup, ast: parsing.ast.decls.Category) {
		switch ast.path {
			case TSegs(_, Cons(seg, Nil)): {
				var s, n, params;
				switch seg {
					case Name(span, name): s = span; n = name; params = None;
					case NameParams(span, name, p): s = span; n = name; params = Some(p);
				}
				
				final category = new Category({
					lookup: lookup,
					span: ast.span,
					name: new Ident(s, n),
					params: params.map(p -> p.of.map(x -> lookup.makeTypePath(x))),
					type: ast.type.map(x -> lookup.makeTypePath(x)),
				});

				for(generic in ast.generics.mapArray(Generic.fromAST.bind(lookup, _))) {
					category.generics.add(generic.name.name, generic);
				}

				for(attr => span in ast.attrs) switch attr {
					case IsHidden(_) if(category.hidden.isSome()): category.errors.push(Errors.duplicateAttribute(category, category.name.name, "hidden", span));
					case IsHidden(None): category.hidden = Some(None);
					case IsHidden(Some(outsideOf)): category.hidden = Some(Some(lookup.makeTypePath(outsideOf)));
		
					case IsFriend(_) if(category.friends.length != 0): category.errors.push(Errors.duplicateAttribute(category, category.name.name, "friend", span));
					case IsFriend(One(friend)): category.friends.push(lookup.makeTypePath(friend));
					case IsFriend(Many(_, friends, _)): for(friend in friends) category.friends.push(lookup.makeTypePath(friend));
				}
		
				for(decl in ast.body.of) switch decl {
					case DMember(m) if(m.attrs.exists(IsStatic)): category.staticMembers.push(Member.fromAST(category, m));
					
					case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(category, m).forEach(x -> category.staticMethods.push(x));
					case DMethod(m): category.methods.push(Method.fromAST(category, m));
		
					case DInit(i): category.inits.push(Init.fromAST(category, i));
		
					case DOperator(o): Operator.fromAST(category, o).forEach(x -> category.operators.push(x));
		
					default: category.errors.push(Errors.unexpectedDecl(category, category.name.name, decl));
				}

				return category;
			}
			case TSegs(_, Nil) | TBlank(_) | TBlankParams(_, _): throw "Error!";
			default: throw "NYI!";
		}
	}

	function hasErrors() {
		return errors.length != 0
			|| generics.allValues().some(g -> g.hasErrors())
			|| staticMembers.some(m -> m.hasErrors())
			|| staticMethods.some(m -> m.hasErrors())
			|| methods.some(m -> m.hasErrors())
			|| inits.some(i -> i.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	function allErrors() {
		var result = errors;
		
		for(generic in generics) result = result.concat(generic.allErrors());
		for(member in staticMembers) result = result.concat(member.allErrors());
		for(method in staticMethods) result = result.concat(method.allErrors());
		for(method in methods) result = result.concat(method.allErrors());
		for(init in inits) result = result.concat(init.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}

	inline function declName() {
		return "category";
	}

	function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil) {
		if(cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}

		return if(absolute) {
			path._match(
				at([[typeName, _]]) => switch generics.find(typeName) {
					case None: lookup.findType(path, true, cache);
					case Some([type]): Some(type.thisType);
					case Some(found): Some(new Type(TMulti(found.map(g -> g.thisType))));
				},
				_ => lookup.findType(path, true, cache)
			);
		} else {
			None;
		}
	}

	function makeTypePath(path) {
		return lookup.makeTypePath(path);
	}
}