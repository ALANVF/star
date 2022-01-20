package typing;

import text.Span;
import reporting.Severity;
import reporting.Diagnostic;
import typing.Traits;

@:publicFields
@:build(util.Overload.build())
class Errors {
	/*static function invalidDeclType(decl, declSpan, name, typeSpan) {
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
	
	static function duplicateAttribute<T: VDecl>(decl: T, name, attr, attrSpan) {
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

	static function invalidAttribute<T: VDecl>(decl: T, name, attr, attrSpan) {
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


	static overload extern inline function duplicateDecl<T: VDecl>(decl: T, name, decl2) return duplicateDecl_IDecl(decl, name, decl2);
	private static function duplicateDecl_IDecl<T: VDecl>(decl: T, name, decl2: parsing.ast.decls.Decl) {
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
	private static function duplicateDecl_File(decl: parsing.ast.decls.Decl) {
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

	static overload extern inline function unexpectedDecl<T: VDecl>(decl: T, name, decl2) return unexpectedDecl_IDecl(decl, name, decl2);
	private static function unexpectedDecl_IDecl<T: VDecl>(decl: T, name, decl2: parsing.ast.decls.Decl) {
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
	private static function unexpectedDecl_File(decl2: parsing.ast.decls.Decl) {
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
	
	static overload extern inline function invalidDecl<T: VDecl>(decl: T, name, decl2) return invalidDecl_IDecl(decl, name, decl2);
	private static function invalidDecl_IDecl<T: VDecl>(decl: T, name, decl2: parsing.ast.decls.Decl) {
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
	private static function invalidDecl_File(decl2: parsing.ast.decls.Decl) {
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

	static function invalidTypeLookup(
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
	
	static function invalidTypeApply(
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
	
	static function notYetImplemented(
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


	overload static function duplicateParam<T: AnyMethod>(method: T, name, origSpan, dupSpan) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Duplicate parameter",
			info: [
				Spanned({
					span: dupSpan,
					message: 'Duplicate parameter `$name`',
					isPrimary: true
				}),
				Spanned({
					span: origSpan,
					message: 'First defined here',
					isSecondary: true
				}),
				Spanned({
					span: method.span,
					message: 'For ${method.declName()} `${method.methodName()}`',
					isSecondary: true
				})
			]
		});
	}
	overload static function duplicateParam(tcase: MultiTaggedCase, name, origSpan, dupSpan) {
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Duplicate parameter",
			info: [
				Spanned({
					span: dupSpan,
					message: 'Duplicate parameter `$name`',
					isPrimary: true
				}),
				Spanned({
					span: origSpan,
					message: 'First defined here',
					isSecondary: true
				}),
				Spanned({
					span: tcase.span,
					message: 'For ${tcase.declName()} `${tcase.params.joinMap("", p -> p.label.name+":")}`',
					isSecondary: true
				})
			]
		});
	}


	static function unknownFieldOrVar(ctx: Ctx, name, span) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Unknown name",
			info: [
				Spanned({
					span: span,
					message: 'Unknown field or variable `$name`',
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}

	static function shadowedLocalVar(ctx: Ctx, name, origSpan, dupSpan) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.WARNING,
			message: "Shadowed variable",
			info: [
				Spanned({
					span: dupSpan,
					message: 'This shadows an existing local variable `$name`',
					isPrimary: true
				}),
				Spanned({
					span: origSpan,
					message: 'First defined here',
					isSecondary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}

	static function localVarTypeMismatch(ctx: Ctx, name, wantedType: Type, gotType: Type, declSpan, hereSpan) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Type mismatch",
			info: [
				Spanned({
					span: hereSpan,
					message: 'local variable `$name` declared to be of type `${wantedType.fullName()}`, but got `${gotType.fullName()}` instead',
					isPrimary: true
				}),
				Spanned({
					span: declSpan,
					message: 'First defined here',
					isSecondary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}
	

	static inline overload function unknownMethod(ctx: Ctx, isStatic: Bool, type: Type, name: String, span, ?categories: Array<Category>)
		return _unknownMethod(ctx, isStatic, type, '[$name]', span, categories);

	static inline overload function unknownMethod(ctx: Ctx, isStatic: Bool, type: Type, names: Array<String>, args: Array<TExpr>, span, ?categories: Array<Category>)
		return _unknownMethod(ctx, isStatic, type, "["+names.zip(args, (n, a) -> '$n: (${a.t._andOr(t => t.fullName(), "???")})').join(" ")+"]", span, categories);
	
	static inline overload function unknownMethod(ctx: Ctx, isStatic: Bool, type: Type, names: Array<String>, span, ?categories: Array<Category>)
		return _unknownMethod(ctx, isStatic, type, "["+names.joinMap(" ", n -> '$n:')+"]", span, categories);

	static inline overload function unknownMethod(ctx: Ctx, type: Type, op: UnaryOp, span)
		return _unknownMethod(ctx, false, type, '[`${op.symbol()}`]', span);

	static inline overload function unknownMethod(ctx: Ctx, type: Type, op: BinaryOp, span)
		return _unknownMethod(ctx, false, type, '[`${op.symbol()}`:]', span);
	
	static inline overload function unknownMethod(ctx: Ctx, type: Type, rtype: Type, op: BinaryOp, span)
		return _unknownMethod(ctx, false, type, '[`${op.symbol()}`: (${rtype.fullName()})]', span);

	private static function _unknownMethod(ctx: Ctx, isStatic: Bool, type: Type, methodName, span, ?categories: Array<Category>) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Unknown method",
			info: [
				Spanned({
					span: span,
					message: {
						var msg = '${isStatic ? "Type" : "Value of type"} `${type.fullName()}` does not respond to method $methodName';
						
						categories._and(cats => {
							msg += " in any categories of:";
							for(cat in cats) {
								msg += '\n    ${cat.fullName()}';
							}
						});

						msg;
					},
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}

	static function unknownCast(ctx: Ctx, type: Type, target: Type, span) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Unknown cast",
			info: [
				Spanned({
					span: span,
					message: 'Value of type `${type.fullName()}` cannot be cast to type `${target.fullName()}`',
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}


	static function unknownGetter(ctx: Ctx, isStatic: Bool, type: Type, name, span) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Unknown name access",
			info: [
				Spanned({
					span: span,
					message: '${isStatic ? "Type" : "Value of type"} `${type.fullName()}` does not have member/getter `$name`',
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}

	overload static function unknownSetter(ctx: Ctx, isStatic: Bool, type: Type, name, span) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Unknown name access",
			info: [
				Spanned({
					span: span,
					message: '${isStatic ? "Type" : "Value of type"} `${type.fullName()}` does not have member/setter `$name`',
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}
	overload static function unknownSetter(ctx: Ctx, isStatic: Bool, type: Type, name, expr: TExpr, span) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Unknown name access",
			info: [
				Spanned({
					span: span,
					message: '${isStatic ? "Type" : "Value of type"} `${type.fullName()}` does not have member/setter `$name` of type ${expr.t._andOr(t=>t.fullName(), "???")}',
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}


	static function unknownCategory(ctx: Ctx, isStatic: Bool, type: Type, cat: Type, span) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Unknown cast",
			info: [
				Spanned({
					span: span,
					message: '${isStatic ? "Type" : "Value of type"} `${type.fullName()}` does not have the category `${cat.fullName()}`',
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}


	static function thisNotAllowed(ctx: Ctx, span) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Invalid usage",
			info: [
				Spanned({
					span: span,
					message: '`this` is not allowed in a static context',
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}


	static function expectedLogicalValue(ctx: Ctx, gotType: Type, span) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Invalid type",
			info: [
				Spanned({
					span: span,
					message: 'Expected a logical value, but got value of type `${gotType.fullName()}` instead',
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}


	static function possiblyUnintendedArrowBlock(ctx: Ctx, span) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.WARNING,
			message: "Possibly unintentional arrow shorthand",
			info: [
				Spanned({
					span: span,
					message: 'Using a block in an arrow shorthand does not act the same as a plain block!',
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}

	
	static function arrayPatternNotAllowed(ctx: Ctx, span) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.ERROR,
			message: "Invalid pattern",
			info: [
				Spanned({
					span: span,
					message: 'This pattern is only allowed in array patterns',
					isPrimary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}


	static function duplicateBinding(ctx: Ctx, name, origSpan, dupSpan) {
		final lookup = ctx.thisLookup;
		return new Diagnostic({
			severity: Severity.WARNING,
			message: "Duplicate binding",
			info: [
				Spanned({
					span: dupSpan,
					message: 'This shadows a previous binding `$name`',
					isPrimary: true
				}),
				Spanned({
					span: origSpan,
					message: 'First defined here',
					isSecondary: true
				}),
				Spanned({
					span: lookup.span,
					message: 'In ${ctx.description()}',
					isSecondary: true
				})
			]
		});
	}
}