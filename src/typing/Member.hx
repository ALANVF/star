package typing;

import parsing.ast.Expr;
import parsing.ast.Ident;
import text.Span;

@:build(util.Auto.build())
class Member {
	final lookup: ILookupType;
	final name: Ident;
	var type: Option<Type>;
	var isStatic: Bool = false;
	var hidden: Option<Option<Type>> = None;
	var isReadonly: Bool = false;
	var getter: Option<Option<Ident>> = None;
	var setter: Option<Option<Ident>> = None;
	var noInherit: Bool = false;
	final value: Option<Expr>;

	static function fromAST(lookup, ast: parsing.ast.decls.Member) {
		final declSpan = Span.range(ast.span, ast.name.span);

		final member = new Member({
			lookup: lookup,
			name: ast.name,
			type: ast.type.map(t -> switch lookup.lookupTypePath(t, true) {
				case Some(t2): t2;
				case None: throw Errors.invalidDeclType("member", declSpan, ast.name.name, t.span());
			}),
			value: ast.value
		});

		for(attr => span in ast.attrs) switch attr {
			case IsStatic: member.isStatic = true;

			case IsHidden(_) if(member.hidden.isSome()): throw Errors.duplicateAttribute("member", declSpan, ast.name.name, "hidden", span);
			case IsHidden(None): member.hidden = Some(None);
			case IsHidden(Some(outsideOf)): member.hidden = switch lookup.lookupTypePath(outsideOf, true) {
				case Some(t): Some(Some(t));
				case None: throw Errors.invalidDeclType("member", declSpan, ast.name.name, outsideOf.span());
			}

			case IsReadonly: member.isReadonly = true;

			case IsGetter(_) if(member.getter.isSome()): throw Errors.duplicateAttribute("member", declSpan, ast.name.name, "getter", span);
			case IsGetter(name): member.getter = Some(name);

			case IsSetter(_) if(member.setter.isSome()): throw Errors.duplicateAttribute("member", declSpan, ast.name.name, "setter", span);
			case IsSetter(name): member.setter = Some(name);

			case IsNoinherit: member.noInherit = true;
		}

		return member;
	}
}