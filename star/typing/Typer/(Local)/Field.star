class Field of Local {
	my member (Member)

	init [ctx: (Ctx) member: (Member)] {
		_.ctx = ctx
		_.member = member
		span = member.span
		name = member.name
		type = member.type[collect: Type$0[in: ctx]]
		expr = member.value[collect: Pass2[:ctx typeExpr: Parser.Expr$.0]]
	}
}