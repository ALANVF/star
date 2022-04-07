;use Expr from: Parser as: UExpr
alias UExpr is hidden = Parser.Expr

module Pass2 {
	my std_Value (TypeDecl) is getter


	;== Type refinement

	on [buildRefines: dir (Dir)] {
		for my file in: dir.files => This[buildRefines: file]
		for my unit in: dir.units => This[buildRefines: unit]
	}

	on [buildRefines: project (Project)] {
		This[buildRefines: project[Dir]]
		match project.main at Maybe[the: my main] => This[buildRefines: main]
	}

	on [buildRefines: unit (Unit)] {
		match unit.primary at Maybe[the: my file] => This[buildRefines: file]
		This[buildRefines: unit[Dir]]
	}

	on [buildRefines: file (File)] {
		for my decl in: file.sortedDecls => decl[buildRefinements]
	}


	;== Early member resolution

	on [resolveMembers: dir (Dir)] {
		for my file in: dir.files => This[resolveMembers: file]
		for my unit in: dir.units => This[resolveMembers: unit]
	}

	on [resolveMembers: project (Project)] {
		This[resolve: project[Dir]]
		match project.main at Maybe[the: my main] => This[resolveMembers: main]
	}

	on [resolveMembers: unit (Unit)] {
		match unit.primary at Maybe[the: my file] => This[resolveMembers: file]
		This[resolveMembers: unit[Dir]]
	}

	on [resolveMembers: file (File)] {
		;@@ TODO: make this better
		for my decl in: file.sortedDecls {
			This[ctx: Ctx[:decl] resolveMembers: decl]
		}
	}

	on [ctx: (Ctx) resolveMembers: decl (TypeDecl)] {
		match decl {
			at my ns (Namespace) {
				for my decl' in: ns.sortedDecls {
					This[ctx: ctx[innerDecl: decl'] resolveMembers: decl']
				}
				;for my mem in: ns.staticMembers => This[:ctx resolve: mem]

				match ns at my classLike (ClassLike) {
					for my mem in: classLike.members => This[:ctx resolve: mem]
				}
			}

			at my alias (StrongAlias) {
				;for my mem in: alias.staticMembers => This[:ctx resolve: mem]
				for my mem in: alias.members => This[:ctx resolve: mem]
			}

			else {}
		}
	}


	;== Project resolution (MAIN)

	on [resolve: dir (Dir)] {
		for my file in: dir.files => This[resolve: file]
		for my unit in: dir.units => This[resolve: unit]
	}

	on [resolve: project (Project)] {
		This
		-> [buildRefines: project]
		-> [resolveMembers: project]
		-> [resolve: project[Dir]]
		match project.main at Maybe[the: my main] => This[resolve: main]
	}

	on [resolve: unit (Unit)] {
		match unit.primary at Maybe[the: my file] => This[resolve: file]
		This[resolve: unit[Dir]]
	}

	on [resolve: file (File)] {
		;@@ TODO: make this better
		for my decl in: file.sortedDecls => This[ctx: Ctx[:decl] resolve: decl]
		for my category in: file.categories => This[ctx: Ctx[:category] resolve: category]
	}

	
	;== Type declaration resolution

	on [ctx: (Ctx) resolve: decl (TypeDecl)]

	on [ctx: (Ctx) resolve: cat (Category)]


	;== Method declaration resolution

	on [ctx: (Ctx) resolve: method (EmptyMethod)]

	on [ctx: (Ctx) resolve: method (Method)]

	on [ctx: (Ctx) resolve: method (StaticMethod)]

	on [ctx: (Ctx) resolve: init (Init)]

	on [ctx: (Ctx) resolve: operator (Operator)]


	;== Case declaration resolution

	on [ctx: (Ctx) resolve: vcase (ValueCase)]

	on [ctx: (Ctx) resolve: tcase (TaggedCase)]


	;== Member declaration resolution

	on [ctx: (Ctx) resolve: member (Member)]


	;== Exprs

	on [ctx: (Ctx) typeExpr: expr (UExpr)] (Expr)
}