kind Body {
	has [arrow: (Span) stmt: (Stmt)]
	has [block: (Block)]
	
	on [stmts] (Array[Stmt]) is getter {
		match this {
			at Body[block: my block] => return block.stmts
			at Body[arrow: _ stmt: my stmt] => return #[stmt]
		}
	}
}