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

	static function mapArray<T, U, V, W>(list: List3<T, U, V>, func: (T, U, V) -> W) {
		final array = [];
		
		while(true) switch list {
			case Nil3: break;
			case Cons3(h1, h2, h3, tl):
				array.push(func(h1, h2, h3));
				list = tl;
		}

		return array;
	}
	
	static inline function mapList<T, U, V, W>(self: List3<T, U, V>, func: (T, U, V) -> W) return switch self {
		case Nil3: Nil;
		case Cons3(h1, h2, h3, tl): Cons(func(h1, h2, h3), tl.mapList(func));
	}
	
	static inline function fold<T, U, V, Acc>(self: List3<T, U, V>, acc: Acc, func: (Acc, T, U, V) -> Acc): Acc return switch self {
		case Nil3: acc;
		case Cons3(h1, h2, h3, tl): tl.fold(func(acc, h1, h2, h3), func);
	}

	static inline function forEach<T, U, V>(self: List3<T, U, V>, func: (T, U, V) -> Void) switch self {
		case Nil3:
		case Cons3(h1, h2, h3, tl):
			func(h1, h2, h3);
			tl.forEach(func);
	}
}