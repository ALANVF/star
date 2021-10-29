package typing;

import parsing.ast.Ident;
import parsing.ast.Expr;

@:using(typing.MultiParams)
typedef MultiParams = Array<{label: Ident, name: Ident, type: Type, ?value: Expr}>;


enum MatchParams {
	Yes;
	Partial;
	No;
}

function matchesNames(self: MultiParams, names: Array<String>, isSetter = false) {
	if(self.every2Strict(names, (l, n) -> (n == "=" && isSetter) || l.label.name == n)) {
		return Yes;
	} else if(names.length < self.length) {
		var n = 0;
		var p = 0;
		var matchedOnce = false;
		while(n < names.length && p < self.length) {
			self[p]._match(
				at({label: {name: label}, value: _}, when(label == names[n])) => {
					n++;
					p++;
					if(!matchedOnce) matchedOnce = true;
				},
				
				at({label: {name: _}, value: _!}) => {
					p++;
				},

				_ => {
					matchedOnce = false;
					break;
				}
			);
		}

		if(matchedOnce) {
			return Partial;
		} else {
			return No;
		}
	} else {
		return No;
	}
}