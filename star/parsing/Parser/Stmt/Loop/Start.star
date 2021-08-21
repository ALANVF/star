kind Start {
	my span (Span)
	my expr (Expr)

	has [from: span (Span), expr (Expr)]  => [from]  {this->[:span]->[:expr]}
	has [after: span (Span), expr (Expr)] => [after] {this->[:span]->[:expr]}
}