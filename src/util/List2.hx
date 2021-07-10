package util;

import haxe.macro.Expr;

enum List2<T, U> {
	Nil2;
	Cons2(head1: T, head2: U, tail: List2<T, U>);
}

@:noCompletion
@:publicFields
class List2Helper {
	static inline macro function of<T, U>(_: ExprOf<Enum<List2<T, U>>>, values: Array<Expr>): ExprOf<List2<T, U>> {
		if(values.length == 0) {
			return macro Nil2;
		} else {
			return switch values.last() {
				case macro ...$rest:
					values.pop();
					macro ${values.foldRight(rest, (acc, v) -> macro Cons2($a{@:privateAccess Util.getPair(v).concat([acc])}))};
				
				default:
					macro ${values.foldRight(macro Nil2, (acc, v) -> macro Cons2($a{@:privateAccess Util.getPair(v).concat([acc])}))};
			}
		}
	}
}