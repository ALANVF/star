;use Expr from: Parser as: UExpr
alias UExpr is hidden = Parser.Expr

module Pass2 {
	my std_Value (TypeDecl) is getter


	;== Exprs

	on [ctx: (Ctx) typeExpr: expr (UExpr)] (Expr)
}