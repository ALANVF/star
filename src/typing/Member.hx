package typing;

import reporting.Severity;
import reporting.Diagnostic;
import parsing.ast.Expr;
import parsing.ast.Ident;
import text.Span;
import typing.Traits;

@:build(util.Auto.build())
class Member implements IErrors {
	final errors: Array<Diagnostic> = [];
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

	static function fromAST(decl: AnyTypeDecl, ast: parsing.ast.decls.Member) {
		final declSpan = Span.range(ast.span, ast.name.span);

		final member = new Member({
			decl: decl,
			span: ast.span,
			name: ast.name,
			type: ast.type.toNull()._and(t => decl.makeTypePath(t)),
			value: ast.value.toNull()
		});

		var getterSpan = null;
		var setterSpan = null;

		for(attr => span in ast.attrs) switch attr {
			case IsStatic: member.isStatic = true;

			case IsHidden(_) if(member.hidden != null): member.errors.push(Errors.duplicateAttribute(member, ast.name.name, "hidden", span));
			case IsHidden(None): member.hidden = None;
			case IsHidden(Some(outsideOf)): member.hidden = Some(decl.makeTypePath(outsideOf));

			case IsReadonly: member.isReadonly = true;

			case IsGetter(_) if(member.getter != null): member.errors.push(Errors.duplicateAttribute(member, ast.name.name, "getter", span));
			case IsGetter(name):
				member.getter = name;
				getterSpan = span;

			case IsSetter(_) if(member.setter != null): member.errors.push(Errors.duplicateAttribute(member, ast.name.name, "setter", span));
			case IsSetter(name):
				member.setter = name;
				setterSpan = span;

			case IsNoinherit: member.noInherit = true;
		}

		switch member.getter {
			case Some({span: s, name: n}) if(n == ast.name.name):
				member.errors.push(new Diagnostic({
					severity: Severity.WARNING,
					message: "Redundant code",
					info: [
						Spanned({
							span: Span.range(getterSpan.nonNull(), s),
							message: 'Unnecessary use of "is getter `$n`". Doing "is getter" is just fine',
							isPrimary: true
						}),
						Spanned({
							span: declSpan,
							message: 'For member `${member.name.name}`',
							isSecondary: true
						})
					]
				}));
			
			default:
		}

		switch member.setter {
			case Some({span: s, name: n}) if(n == ast.name.name):
				member.errors.push(new Diagnostic({
					severity: Severity.WARNING,
					message: "Redundant code",
					info: [
						Spanned({
							span: Span.range(setterSpan.nonNull(), s),
							message: 'Unnecessary use of "is setter `$n`". Doing "is setter" is just fine',
							isPrimary: true
						}),
						Spanned({
							span: declSpan,
							message: 'For member `${member.name.name}`',
							isSecondary: true
						})
					]
				}));
			
			default:
		}

		switch member {
			case {getter: None, setter: None}:
				member.errors.push(new Diagnostic({
					severity: Severity.WARNING,
					message: "Redundant code",
					info: [
						Spanned({
							span: getterSpan.nonNull(),
							message: 'Unnecessary use of "is getter" along with "is setter"',
							isPrimary: true
						}),
						Spanned({
							span: setterSpan.nonNull(),
							isPrimary: true
						}),
						Spanned({
							span: declSpan,
							message: 'For member `${member.name.name}`',
							isSecondary: true
						})
					]
				}));

			case {getter: Some({name: n1}), setter: Some({name: n2})} if(n1 == ast.name.name && n1 == n2):
				member.errors.push(new Diagnostic({
					severity: Severity.WARNING,
					message: "Redundant code",
					info: [
						Spanned({
							span: getterSpan.nonNull(),
							message: 'Unnecessary use of "is getter" along with "is setter"',
							isPrimary: true
						}),
						Spanned({
							span: setterSpan.nonNull(),
							isPrimary: true
						}),
						Spanned({
							span: declSpan,
							message: 'For member `${member.name.name}`',
							isSecondary: true
						})
					]
				}));
				
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