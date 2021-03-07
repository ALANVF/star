kind Param {
	has [label: (Ident) name: (Ident) type: (Type) value: (Maybe[Expr])]
	has [label: (Ident)               type: (Type) value: (Maybe[Expr])]
	has [               name: (Ident) type: (Type) value: (Maybe[Expr])]
	has [                             type: (Type)                     ]

	on [label] (Maybe[Ident]) is getter {
		match this at Param[label: my label name: _ type: _ value: _] || Param[label: my label type: _ value: _] {
			return Maybe[the: label]
		} else {
			return Maybe[none]
		}
	}

	on [name] (Maybe[Ident]) is getter {
		match this at Param[label: _ name: my name type: _ value: _] || Param[name: my name type: _ value: _] {
			return Maybe[the: name]
		} else {
			return Maybe[none]
		}
	}

	on [type] (Type) is getter {
		match this {
			at Param[label: _ name: _ type: my type value: _] => return type
			at Param[label: _         type: my type value: _] => return type
			at Param[         name: _ type: my type value: _] => return type
			at Param[                 type: my type         ] => return type
		}
	}

	on [value] (Maybe[Expr]) is getter {
		match this {
			at Param[label: _ name: _ type: _ value: my value] => return value
			at Param[label: _         type: _ value: my value] => return value
			at Param[         name: _ type: _ value: my value] => return value
			at Param[                 type: _                ] => return Maybe[none]
		}
	}
}