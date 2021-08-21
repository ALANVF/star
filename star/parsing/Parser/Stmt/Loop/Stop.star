kind Stop {
	my span (Span)
	my expr (Expr)

	has [to: span (Span), expr (Expr)]     => [to]     {this->[:span]->[:expr]}
	has [upto: span (Span), expr (Expr)]   => [upto]   {this->[:span]->[:expr]}
	has [downto: span (Span), expr (Expr)] => [downto] {this->[:span]->[:expr]}
	has [times: span (Span), expr (Expr)]  => [times]  {this->[:span]->[:expr]}
}