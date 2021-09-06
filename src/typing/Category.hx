package typing;

import text.Span;
import parsing.ast.Ident;
import reporting.Diagnostic;
import typing.Traits;

@:build(util.Auto.build())
class Category implements IErrors {
	final errors: Array<Diagnostic> = [];
	@ignore final typevars = new MultiMap<String, TypeVar>();
	final lookup: ILookupType;
	final span: Span;
	final name: Ident;
	var path: Type;
	var type: Option<Type>;
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	final methods: Array<Method> = [];
	final inits: Array<Init> = [];
	final operators: Array<Operator> = [];
	var hidden: Option<Option<Type>> = None;
	final friends: Array<Type> = [];

	static function fromAST(lookup, ast: parsing.ast.decls.Category) {
		final category = new Category({
			lookup: lookup,
			span: ast.span,
			name: new Ident(ast.path.span(), ast.path.simpleName()),
			path: null, // hack for partial initialization
			type: null  // hack for partial initialization
		});

		var path = (ast.path : TypePath).toType(category);

		category.path = path;
		category.type = ast.type.map(x -> category.makeTypePath(x));

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(category, a))) {
			category.typevars.add(typevar.name.name, typevar);
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(category.hidden.isSome()): category.errors.push(Errors.duplicateAttribute(category, category.name.name, "hidden", span));
			case IsHidden(None): category.hidden = Some(None);
			case IsHidden(Some(outsideOf)): category.hidden = Some(Some(category.makeTypePath(outsideOf)));

			case IsFriend(_) if(category.friends.length != 0): category.errors.push(Errors.duplicateAttribute(category, category.name.name, "friend", span));
			case IsFriend(One(friend)): category.friends.push(category.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) category.friends.push(category.makeTypePath(friend));
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

	function hasErrors() {
		return errors.length != 0
			|| typevars.allValues().some(g -> g.hasErrors())
			|| staticMembers.some(m -> m.hasErrors())
			|| staticMethods.some(m -> m.hasErrors())
			|| methods.some(m -> m.hasErrors())
			|| inits.some(i -> i.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	function allErrors() {
		var result = errors;
		
		for(typevar in typevars) result = result.concat(typevar.allErrors());
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

	function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}

		return path._match(
			at([[_, "This", []]], when(absolute)) => type.doOrElse(
				t => Some(t),
				lookup.findType(path, true, cache)
			),
			at([[span, "This", _]], when(absolute)) => {
				// prob shouldn't be attatched to *this* category decl, but eh
				errors.push(Errors.notYetImplemented(span));
				None;
			},
			at([[span, typeName, args], ...rest]) => {
				final res: Option<Type> = switch typevars.find(typeName) {
					case None: return if(absolute) lookup.findType(path, true, cache) else None;
					case Some([type]):
						switch [args, type.params] {
							case [[], _]: Some({t: type.thisType.t, span: span}); // should probably curry parametrics but eh
							case [_, []]:
								// should this check for type aliases?
								errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
								None;
							case [_, params]:
								if(args.length > params.length) {
									errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
									None;
								} else if(args.length < params.length) {
									errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
									None;
								} else {
									Some({t: TApplied(type.thisType, args), span: span});
								}
							}
					case Some(found):
						if(args.length == 0) {
							Some({t: TMulti(found.map(t -> t.thisType)), span: span});
						} else switch found.filter(t -> t.params.length == args.length).map(g -> g.thisType) {
							case []:
								errors.push(Errors.invalidTypeApply(span, "No candidate matches the type arguments"));
								None;
							case [type]: Some({t: TApplied(type, args), span: span});
							case types: Some({t: TMulti(types), span: span});
						}
				};

				switch [rest, res] {
					case [Nil3, _]: res;
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
					case [_, None]: res;
				}
			},
			_ => if(absolute) lookup.findType(path, true, cache) else None
		);
	}

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}
}