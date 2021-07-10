package typing;

import text.Span;
import reporting.Severity;
import reporting.Diagnostic;
import typing.Traits;

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
	
	static inline function duplicateAttribute<T: IDecl>(decl: T, name, attr, attrSpan) {
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
					span: decl.span,
					message: 'For ${decl.declName()} `$name`',
					isSecondary: true
				})
			]
		});
	}

	static inline function invalidAttribute<T: IDecl>(decl: T, name, attr, attrSpan) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Invalid attribute",
			info: [
				Spanned({
					span: attrSpan,
					message: 'Invalid attribute `is $attr`',
					isPrimary: true
				}),
				Spanned({
					span: decl.span,
					message: 'For ${decl.declName()} `$name`',
					isSecondary: true
				})
			]
		});
	}


	static overload extern inline function duplicateDecl<T: IDecl>(decl: T, name, decl2) return duplicateDecl_IDecl(decl, name, decl2);
	private static inline function duplicateDecl_IDecl<T: IDecl>(decl: T, name, decl2: parsing.ast.decls.Decl) {
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
					span: decl.span,
					message: 'For ${decl.declName()} `$name`',
					isSecondary: true
				})
			]
		});
	}

	static overload extern inline function duplicateDecl(decl: File, decl2) return duplicateDecl_File(decl2);
	private static inline function duplicateDecl_File(decl: parsing.ast.decls.Decl) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Duplicate declaration",
			info: [
				Spanned({
					span: decl.span(),
					message: 'Duplicate ${decl.name()}',
					isPrimary: true
				})
			]
		});
	}

	static overload extern inline function unexpectedDecl<T: IDecl>(decl: T, name, decl2) return unexpectedDecl_IDecl(decl, name, decl2);
	private static inline function unexpectedDecl_IDecl<T: IDecl>(decl: T, name, decl2: parsing.ast.decls.Decl) {
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
					span: decl.span,
					message: 'For ${decl.declName()} `$name`',
					isSecondary: true
				})
			]
		});
	}

	static overload extern inline function unexpectedDecl(decl: File, decl2) return unexpectedDecl_File(decl2);
	private static inline function unexpectedDecl_File(decl2: parsing.ast.decls.Decl) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Unexpected declaration",
			info: [
				Spanned({
					span: decl2.span(),
					message: 'Unexpected ${decl2.name()}',
					isPrimary: true
				})
			]
		});
	}
	
	static overload extern inline function invalidDecl<T: IDecl>(decl: T, name, decl2) return invalidDecl_IDecl(decl, name, decl2);
	private static inline function invalidDecl_IDecl<T: IDecl>(decl: T, name, decl2: parsing.ast.decls.Decl) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Invalid declaration",
			info: [
				Spanned({
					span: decl2.span(),
					message: 'Invalid ${decl2.name()}',
					isPrimary: true
				}),
				Spanned({
					span: decl.span,
					message: 'For ${decl.declName()} `$name`',
					isSecondary: true
				})
			]
		});
	}

	static overload extern inline function invalidDecl(decl: File, decl2) return invalidDecl_File(decl2);
	private static inline function invalidDecl_File(decl2: parsing.ast.decls.Decl) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Invalid declaration",
			info: [
				Spanned({
					span: decl2.span(),
					message: 'Invalid ${decl2.name()}',
					isPrimary: true
				})
			]
		});
	}

	static inline function invalidTypeLookup(
		span: Span,
		why = "Invalid type lookup"
	) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Invalid type lookup",
			info: [
				Spanned({
					span: span,
					message: why,
					isPrimary: true
				})
			]
		});
	}
	
	static inline function invalidTypeApply(
		span: Span,
		why = "Invalid type application"
	) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Invalid type application",
			info: [
				Spanned({
					span: span,
					message: why,
					isPrimary: true
				})
			]
		});
	}
	
	static inline function notYetImplemented(
		span: Span,
		why = "This feature has not been implemented yet"
	) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Not yet implemented",
			info: [
				Spanned({
					span: span,
					message: why,
					isPrimary: true
				})
			]
		});
	}
}