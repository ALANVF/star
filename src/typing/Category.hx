package typing;

import text.Span;
import parsing.ast.Ident;
import reporting.Diagnostic;
import Util.match;

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
		Util.match(ast.path,
			at([Named(s, n, params)]) => {
				final category = new Category({
					lookup: lookup,
					span: ast.span,
					name: new Ident(s, n),
					params: params.map(p -> p.of.map(lookup.makeTypePath)),
					type: ast.type.map(lookup.makeTypePath),
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
					
					case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(category, m).forEach(category.staticMethods.push);
					case DMethod(m): category.methods.push(Method.fromAST(category, m));
		
					case DInit(i): category.inits.push(Init.fromAST(category, i));
		
					case DOperator(o): Operator.fromAST(category, o).forEach(category.operators.push);
		
					default: category.errors.push(Errors.unexpectedDecl(category, category.name.name, decl));
				}

				return category;
			},
			at([] | [Blank(_, _)]) => throw "Error!",
			_ => throw "NYI!"
		);
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

	function findType(path: List<String>, absolute = false, cache: List<{}> = Nil) {
		if(cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}

		return if(absolute) {
			match(path,
				at([typeName]) => switch generics.find(typeName) {
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