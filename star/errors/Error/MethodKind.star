;[
Single(access: Access, name: String);
Multi(access: Access, names: Array<String>, ?args: Array<TExpr>);
Unary(op: UnaryOp);
Binary(op: BinaryOp, ?rtype: Type);
]

use Typer

kind MethodKind {
	has [single: name (Str) access: (Access)]
	has [multi: names (Array[Str]) access: (Access)]
	has [multi: names (Array[Str]) args: (Array[Expr]) access: (Access)]
	has [unary: op (UnaryOperator.Op)]
	has [binary: op (BinaryOperator.Op)]
	has [binary: op (BinaryOperator.Op) arg: (Type)]

	on [accessAndName] (Tuple[Access, Str]) {
		match this {
			at This[single: my name access: my access] {
				return #{
					access
					"[\(name)]"
				}
			}

			at This[multi: my names access: my access] {
				return #{
					access
					"[" + names[collect: Str$0+":"][joinWith: " "] + "]"
				}
			}

			at This[multi: my names args: my args access: my access] {
				return #{
					access
					"[" + names[zip: args collect: {|n (Str), a (Expr)|
						return "\(n): (\({
							match a.t at Maybe[the: my t] {
								return t.fullName
							} else {
								return "???"
							}
						}))"
					}][joinWith: " "] + "]"
				}
			}

			at This[unary: my op] {
				return #{
					Access.instance
					"[`\(op[Str])`]"
				}
			}

			at This[binary: my op] {
				return #{
					Access.instance
					"[`\(op[Str])`:]"
				}
			}

			at This[binary: my op arg: my arg] {
				return #{
					Access.instance
					"[`\(op[Str])`: (\(arg.fullName))]"
				}
			}
		}
	}
}