package util;

import haxe.macro.Expr;

enum List3<T, U, V> {
	Nil3;
	Cons3(head1: T, head2: U, head3: V, tail: List3<T, U, V>);
}

@:noCompletion
@:publicFields
class List3Helper {
	static inline macro function of<T, U, V>(_: ExprOf<Enum<List3<T, U, V>>>, values: Array<Expr>): ExprOf<List3<T, U, V>> {
		if(values.length == 0) {
			return macro Nil3;
		} else {
			return switch values.last() {
				case macro ...$rest:
					values.pop();
					macro ${values.foldRight(rest, (acc, v) -> macro Cons3($a{@:privateAccess Util.getTriple(v).concat([acc])}))};
				
				default:
					macro ${values.foldRight(macro Nil3, (acc, v) -> macro Cons3($a{@:privateAccess Util.getTriple(v).concat([acc])}))};
			}
		}
	}
}