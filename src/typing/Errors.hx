package typing;

import reporting.Severity;
import reporting.Diagnostic;

@:publicFields
class Errors {
	/*static inline function invalidDeclType(decl, declSpan, name, typeSpan) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Invalid type",
			info: [
				Spanned({
					span: typeSpan,
					message: "Invalid type",
					isPrimary: true
				}),
				Spanned({
					span: declSpan,
					message: 'For $decl `$name`',
					isSecondary: true
				})
			]
		});
	}*/
	
	static inline function duplicateAttribute(decl, declSpan, name, attr, attrSpan) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Duplicate attribute",
			info: [
				Spanned({
					span: attrSpan,
					message: 'Duplicate attribute `is $attr`',
					isPrimary: true
				}),
				Spanned({
					span: declSpan,
					message: 'For $decl `$name`',
					isSecondary: true
				})
			]
		});
	}

	static inline function duplicateDecl(decl, declSpan, name, decl2: parsing.ast.decls.Decl) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Duplicate declaration",
			info: [
				Spanned({
					span: decl2.span(),
					message: 'Duplicate ${decl2.name()}',
					isPrimary: true
				}),
				Spanned({
					span: declSpan,
					message: 'For $decl `$name`',
					isSecondary: true
				})
			]
		});
	}

	static inline function unexpectedDecl(decl, declSpan, name, decl2: parsing.ast.decls.Decl) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Unexpected declaration",
			info: [
				Spanned({
					span: decl2.span(),
					message: 'Unexpected ${decl2.name()}',
					isPrimary: true
				}),
				Spanned({
					span: declSpan,
					message: 'For $decl `$name`',
					isSecondary: true
				})
			]
		});
	}
}