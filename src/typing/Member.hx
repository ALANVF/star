package typing;

import reporting.Severity;
import reporting.Diagnostic;
import parsing.ast.Expr;
import parsing.ast.Ident;
import text.Span;

@:build(util.Auto.build())
class Member implements IDecl {
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType;
	final span: Span;
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
			span: ast.span,
			name: ast.name,
			type: ast.type.map(t -> lookup.makeTypePath(t)),
			value: ast.value
		});

		var getterSpan = None;
		var setterSpan = None;

		for(attr => span in ast.attrs) switch attr {
			case IsStatic: member.isStatic = true;

			case IsHidden(_) if(member.hidden.isSome()): member.errors.push(Errors.duplicateAttribute(member, ast.name.name, "hidden", span));
			case IsHidden(None): member.hidden = Some(None);
			case IsHidden(Some(outsideOf)): member.hidden = Some(Some(lookup.makeTypePath(outsideOf)));

			case IsReadonly: member.isReadonly = true;

			case IsGetter(_) if(member.getter.isSome()): member.errors.push(Errors.duplicateAttribute(member, ast.name.name, "getter", span));
			case IsGetter(name):
				member.getter = Some(name);
				getterSpan = Some(span);

			case IsSetter(_) if(member.setter.isSome()): member.errors.push(Errors.duplicateAttribute(member, ast.name.name, "setter", span));
			case IsSetter(name):
				member.setter = Some(name);
				setterSpan = Some(span);

			case IsNoinherit: member.noInherit = true;
		}

		switch member.getter {
			case Some(Some({span: s, name: n})) if(n == ast.name.name):
				member.errors.push(new Diagnostic({
					severity: Severity.WARNING,
					message: "Redundant code",
					info: [
						Spanned({
							span: Span.range(getterSpan.value(), s),
							message: 'Unnecessary use of "is getter `$n`". Doing "is getter" is just fine',
							isPrimary: true
						}),
						Spanned({
							span: declSpan,
							isSecondary: true
						})
					]
				}));
			
			default:
		}

		switch member.setter {
			case Some(Some({span: s, name: n})) if(n == ast.name.name):
				member.errors.push(new Diagnostic({
					severity: Severity.WARNING,
					message: "Redundant code",
					info: [
						Spanned({
							span: Span.range(setterSpan.value(), s),
							message: 'Unnecessary use of "is setter `$n`". Doing "is setter" is just fine',
							isPrimary: true
						}),
						Spanned({
							span: declSpan,
							isSecondary: true
						})
					]
				}));
			
			default:
		}

		switch member {
			case {getter: Some(None), setter: Some(None)}:
				member.errors.push(new Diagnostic({
					severity: Severity.WARNING,
					message: "Redundant code",
					info: [
						Spanned({
							span: declSpan,
							message: 'Unnecessary use of "is getter" along with "is setter"',
							isSecondary: true
						}),
						Spanned({
							span: getterSpan.value(),
							isPrimary: true
						}),
						Spanned({
							span: setterSpan.value(),
							isPrimary: true
						})
					]
				}));

			case {getter: Some(Some({name: n1})), setter: Some(Some({name: n2}))} if(n1 == ast.name.name && n1 == n2):
				member.errors.push(new Diagnostic({
					severity: Severity.WARNING,
					message: "Redundant code",
					info: [
						Spanned({
							span: declSpan,
							message: 'Unnecessary use of "is getter" along with "is setter"',
							isSecondary: true
						}),
						Spanned({
							span: getterSpan.value(),
							isPrimary: true
						}),
						Spanned({
							span: setterSpan.value(),
							isPrimary: true
						})
					]
				}));
				
			default:
		}

		return member;
	}

	inline function declName() {
		return "member";
	}

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}
}