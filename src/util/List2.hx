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
	
	static function mapArray<T, U, V>(list: List2<T, U>, func: (T, U) -> V) {
		final array = [];
		
		while(true) switch list {
			case Nil2: break;
			case Cons2(h1, h2, tl):
				array.push(func(h1, h2));
				list = tl;
		}

		return array;
	}
	
	static inline function mapList<T, U, V>(self: List2<T, U>, func: (T, U) -> V) return switch self {
		case Nil2: Nil;
		case Cons2(h1, h2, tl): Cons(func(h1, h2), tl.mapList(func));
	}
	
	static inline function fold<T, U, Acc>(self: List2<T, U>, acc: Acc, func: (Acc, T, U) -> Acc): Acc return switch self {
		case Nil2: acc;
		case Cons2(h1, h2, tl): tl.fold(func(acc, h1, h2), func);
	}

	static inline function forEach<T, U>(self: List2<T, U>, func: (T, U) -> Void) switch self {
		case Nil2:
		case Cons2(h1, h2, tl):
			func(h1, h2);
			tl.forEach(func);
	}
}