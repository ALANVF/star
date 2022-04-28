package typing;

import errors.Error;
import parsing.ast.Expr;
import parsing.ast.Ident;
import text.Span;

@:build(util.Auto.build())
class Member implements IDecl {
	final errors: Array<Error> = [];
	final decl: AnyTypeDecl;
	final span: Span;
	final name: Ident;
	var type: Null<Type>;
	var isStatic: Bool = false;
	var hidden: Null<Option<Type>> = null;
	var isReadonly: Bool = false;
	var getter: Null<Option<Ident>> = null;
	var setter: Null<Option<Ident>> = null;
	var noInherit: Bool = false;
	final value: Null<Expr>;
	@ignore var typedValue: Null<TExpr> = null;
	var refinee: Null<Member> = null;

	static function fromAST(decl: AnyTypeDecl, ast: parsing.ast.decls.Member) {
		final declSpan = Span.range(ast.span, ast.name.span);

		final member = new Member({
			decl: decl,
			span: ast.span,
			name: ast.name,
			type: ast.type._and(t => decl.makeTypePath(t)),
			value: ast.value
		});

		var getterSpan = null;
		var setterSpan = null;

		for(attr => span in ast.attrs) switch attr {
			case IsStatic: member.isStatic = true;

			case IsHidden(_) if(member.hidden != null): member.errors.push(Type_DuplicateAttribute(member, ast.name.name, "hidden", span));
			case IsHidden(None): member.hidden = None;
			case IsHidden(Some(outsideOf)): member.hidden = Some(decl.makeTypePath(outsideOf));

			case IsReadonly: member.isReadonly = true;

			case IsGetter(_) if(member.getter != null): member.errors.push(Type_DuplicateAttribute(member, ast.name.name, "getter", span));
			case IsGetter(name):
				member.getter = name;
				getterSpan = span;

			case IsSetter(_) if(member.setter != null): member.errors.push(Type_DuplicateAttribute(member, ast.name.name, "setter", span));
			case IsSetter(name):
				member.setter = name;
				setterSpan = span;

			case IsNoinherit: member.noInherit = true;
		}

		switch member.getter {
			case Some({span: s, name: n}) if(n == ast.name.name):
				member.errors.push(Type_RedundantGetter(
					member.name.name,
					declSpan,
					n,
					Span.range(getterSpan.nonNull(), s)
				));
			
			default:
		}

		switch member.setter {
			case Some({span: s, name: n}) if(n == ast.name.name):
				member.errors.push(Type_RedundantSetter(
					member.name.name,
					declSpan,
					n,
					Span.range(setterSpan.nonNull(), s)
				));
			
			default:
		}

		switch member {
			case {getter: None, setter: None}:
				member.errors.push(Type_RedundantGetterSetter(
					member.name.name,
					declSpan,
					getterSpan.nonNull(),
					setterSpan.nonNull()
				));

			case {getter: Some({name: n1}), setter: Some({name: n2})} if(n1 == ast.name.name && n1 == n2):
				member.errors.push(Type_RedundantGetterSetter(
					member.name.name,
					declSpan,
					getterSpan.nonNull(),
					setterSpan.nonNull()
				));
				
			default:
		}

		return member;
	}

	function declName() {
		return "member";
	}

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}

	function matchesGetter(name: String) {
		return switch getter {
			case Some(n): n.name == name;
			default: this.name.name == name;
		}
	}

	function matchesSetter(name: String) {
		return !isReadonly && switch setter {
			case Some(n): n.name == name;
			default: this.name.name == name;
		}
	}
}