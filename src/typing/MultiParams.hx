package typing;

import parsing.ast.Ident;
import parsing.ast.Expr;

typedef MultiParam = {label: Ident, name: Ident, type: Type, ?value: Expr, ?tvalue: TExpr};

@:using(typing.MultiParams)
typedef MultiParams = Array<MultiParam>;


enum MatchParams {
	Yes;
	Partial(indexes: Array<Int>);
	No;
}

function matchesNames(self: MultiParams, names: Array<String>, isSetter = false) {
	if(self.every2Strict(names, (l, n) -> (n == "=" && isSetter) || l.label.name == n)) {
		return Yes;
	} else if(names.length < self.length && self.some(p -> p.value != null)) {
		final indexes = [];
		var n = 0;
		var p = 0;
		var matchedOnce = false;
		while(n < names.length && p < self.length) {
			self[p]._match(
				at({label: {name: label}, value: _}, when(label == names[n])) => {
					indexes.push(p);
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
			return Partial(indexes);
		} else {
			return No;
		}
	} else {
		return No;
	}
}