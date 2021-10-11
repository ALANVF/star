package util;

import haxe.macro.Expr;

enum List<T> {
	Nil;
	Cons(head: T, tail: List<T>);
}

@:noCompletion
@:publicFields
class ListHelper {
	// Why can't I use this in pattern matching ._.
	static inline macro function of<T>(_: ExprOf<Enum<List<T>>>, values: Array<Expr>): ExprOf<List<T>> {
		if(values.length == 0) {
			return macro Nil;
		} else {
			return switch values.last() {
				case macro ...$rest:
					values.pop();
					macro ${values.foldRight(rest, (acc, v) -> macro Cons($v, $acc))};
				
				default:
					macro ${values.foldRight(macro Nil, (acc, v) -> macro Cons($v, $acc))};
			}
		}
	}

	static function length<T>(list: List<T>) {
		var len = 0;
		
		while(true) switch list {
			case Nil: return len;
			case Cons(_, tail):
				len++;
				list = tail;
		}
	}

	static function toArray<T>(list: List<T>) {
		final array = [];
		
		list.forEach(v -> array.push(v));

		return array;
	}

	/*function iterator<T>(list: List<T>) {

	}*/

	/*static function forEach<T>(list: List<T>, func: (T) -> Void) switch list {
		case Nil:
		case Cons(head, tail):
			func(head);
			tail.forEach(func);
	}*/
	static inline function forEach<T>(list: List<T>, func: (T) -> Void) {
		while(true) switch list {
			case Nil: break;
			case Cons(value, rest):
				func(value);
				list = rest;
		}
	}

	static function forEach2<T, U>(list1: List<T>, list2: List<U>, func: (T, U) -> Void) switch [list1, list2] {
		case [Nil, Nil]:
		case [Cons(head1, tail1), Cons(head2, tail2)]:
			func(head1, head2);
			tail1.forEach2(tail2, func);
		case [_, _]: throw "Length error!";
	}

	static function map<T, U>(list: List<T>, func: (T) -> U) return switch list {
		case Nil: Nil;
		case Cons(head, tail): Cons(func(head), tail.map(func));
	}

	static function mapArray<T, U>(list: List<T>, func: (T) -> U) {
		final array = [];
		
		while(true) switch list {
			case Nil: break;
			case Cons(head, tail):
				array.push(func(head));
				list = tail;
		}

		return array;
	}

	static function some<T>(list: List<T>, func: (T) -> Bool) return switch list {
		case Nil: false;
		case Cons(head, tail): func(head) || tail.some(func);
	}

	static function every<T>(list: List<T>, func: (T) -> Bool) return switch list {
		case Nil: true;
		case Cons(head, tail): func(head) && tail.some(func);
	}

	static function every2<T, U>(list1: List<T>, list2: List<U>, func: (T, U) -> Bool) return switch [list1, list2] {
		case [Nil, Nil]: true;
		case [Cons(head1, tail1), Cons(head2, tail2)]:
			func(head1, head2) && tail1.every2(tail2, func);
		case [_, _]: throw "Length error!";
	}

	static inline function prepend<T>(list: List<T>, value: T) {
		return Cons(value, list);
	}

	static function append<T>(list1: List<T>, list2: List<T>) return switch list1 {
		case Nil: list2;
		case Cons(head, tail): Cons(head, tail.append(list2));
	}
	
	static inline function rev<T>(list: List<T>) {
		return list.revAppend(Nil);
	}
	
	static function revAppend<T>(list1: List<T>, list2: List<T>) {
		while(true) switch list1 {
			case Nil: break;
			case Cons(head, tail):
				list1 = tail;
				list2 = list2.prepend(head);
		}

		return list2;
	}

	static function join<T>(list: List<T>, sep: String) return switch list {
		case Nil: "";
		case Cons(head, Nil): Std.string(head);
		case Cons(head, tail): Std.string(head) + sep + tail.join(sep);
	}

	/*static inline function contains<T>(list: List<T>, value: T) return switch list {
		case Nil: false;
		case Cons(head, tail): value == head || tail.contains(value);
	}*/
	static function contains<T>(list: List<T>, value: T) {
		while(true) switch list {
			case Nil: return false;
			case Cons(v, rest):
				if(value == v) {
					return true;
				} else {
					list = rest;
				}
		}
	}

	static function nth<T>(list: List<T>, i: Int) return switch [i, list] {
		case [_, Nil]: throw new IndexError();
		case [0, Cons(head, _)]: head;
		case [_, Cons(_, tail)]: tail.nth(i - 1);
	}

	static inline function head<T>(list: List<T>) return switch list {
		case Nil: throw "Empty list!";
		case Cons(head, _): head;
	}

	static inline function setHead<T>(list: List<T>, value: T) {
		if(list == Nil) {
			throw "Empty list!";
		} else {
			util.EnumValues.setParameter(list, 0, value);
		}
	}
	static inline function unsafeSetHead<T>(list: List<T>, value: T) {
		util.EnumValues.setParameter(list, 0, value);
	}

	static inline function tail<T>(list: List<T>) return switch list {
		case Nil: throw "Empty list!";
		case Cons(_, tail): tail;
	}

	static inline function setTail<T>(list: List<T>, value: List<T>) {
		if(list == Nil) {
			throw "Empty list!";
		} else {
			util.EnumValues.setParameter(list, 1, value);
		}
	}
	static inline function unsafeSetTail<T>(list: List<T>, value: List<T>) {
		util.EnumValues.setParameter(list, 1, value);
	}

	static function last<T>(list: List<T>) return switch list {
		case Nil: throw "Empty list!";
		case Cons(v, Nil): v;
		case Cons(_, rest): rest.last();
	}
}