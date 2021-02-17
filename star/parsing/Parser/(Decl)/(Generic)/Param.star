class Param of Parents {
	my span (Span)
	my name (Ident)
	my params (Maybe[Type.Params])
	my rule (Maybe[Tuple[Span, Rule]])
	my body (Maybe[Body])
}